"""
Benchmark GUI server — FastAPI backend.

Start with:  make gui   (or: python tools/gui/server.py)

Architecture
------------
Two execution modes:

1. Interactive (PTY) — for sudo-auth, profiler, model-selector:
   SubprocessManager runs the process attached to a PTY so the user can type
   interactively in the browser terminal. Output streamed via WebSocket.

2. Detached job — for all make targets (run, build, clean, etc.):
   Subprocess is fully detached (new process group, stdin=DEVNULL,
   stdout/stderr→log file). Survives server restarts / SSH disconnects.
   Server tails log and streams new lines to WebSocket clients.
"""

import asyncio
import configparser
import csv
import fcntl
import json
import os
import pty
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import time
from datetime import datetime, timedelta
from pathlib import Path
from typing import Optional

import uvicorn
from fastapi import FastAPI, HTTPException, Query, WebSocket, WebSocketDisconnect
from fastapi.responses import FileResponse
from fastapi.staticfiles import StaticFiles

ROOT = Path(__file__).resolve().parent.parent.parent
STATIC_DIR = Path(__file__).resolve().parent / "static"
TOOLS_DIR = ROOT / "tools"
DEFAULT_CONFIG = ROOT / "bench.config"

sys.path.insert(0, str(ROOT))
from tools.bench_profile import load_config

app = FastAPI(title="Benchmark GUI")
app.mount("/static", StaticFiles(directory=str(STATIC_DIR)), name="static")


# ---------------------------------------------------------------------------
# Interactive process manager — PTY or pipe
# ---------------------------------------------------------------------------

class SubprocessManager:
    def __init__(self):
        self._proc: Optional[asyncio.subprocess.Process] = None
        self._label: str = ""
        self._lock = asyncio.Lock()
        self._clients: list[WebSocket] = []
        self._master_fd: Optional[int] = None

    def is_running(self) -> bool:
        return self._proc is not None and self._proc.returncode is None

    async def start(self, cmd: list[str], label: str, use_pty: bool = False,
                    env: Optional[dict] = None) -> bool:
        async with self._lock:
            if self.is_running():
                return False
            self._label = label
            self._master_fd = None
            proc_env = {**os.environ, **(env or {})}

            if use_pty:
                master_fd, slave_fd = pty.openpty()
                fcntl.ioctl(slave_fd, termios.TIOCSWINSZ,
                            struct.pack("HHHH", 24, 220, 0, 0))
                self._master_fd = master_fd

                def _child_setup(sfd=slave_fd):
                    os.setsid()
                    fcntl.ioctl(sfd, termios.TIOCSCTTY, 0)

                self._proc = await asyncio.create_subprocess_exec(
                    *cmd,
                    stdin=slave_fd, stdout=slave_fd, stderr=slave_fd,
                    cwd=str(ROOT), env=proc_env,
                    close_fds=True, preexec_fn=_child_setup,
                )
                os.close(slave_fd)
                asyncio.ensure_future(self._pump_pty())
            else:
                self._proc = await asyncio.create_subprocess_exec(
                    *cmd,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE,
                    cwd=str(ROOT), env=proc_env,
                )
                asyncio.ensure_future(self._pump())
            return True

    async def stop(self) -> bool:
        async with self._lock:
            if not self.is_running():
                return False
            try:
                self._proc.send_signal(signal.SIGTERM)
            except ProcessLookupError:
                pass
            return True

    def resize_pty(self, rows: int, cols: int):
        if self._master_fd is not None:
            try:
                fcntl.ioctl(self._master_fd, termios.TIOCSWINSZ,
                            struct.pack("HHHH", rows, cols, 0, 0))
            except OSError:
                pass

    def write_stdin(self, data: str):
        if self._master_fd is not None:
            try:
                os.write(self._master_fd, data.encode())
            except OSError:
                pass

    async def _pump_pty(self):
        loop = asyncio.get_running_loop()
        done = loop.create_future()
        mfd = self._master_fd

        def _read_ready():
            try:
                data = os.read(mfd, 4096)
                if data:
                    asyncio.ensure_future(self._broadcast({
                        "stream": "stdout",
                        "data": data.decode(errors="replace"),
                    }))
            except OSError:
                loop.remove_reader(mfd)
                if not done.done():
                    done.set_result(None)

        loop.add_reader(mfd, _read_ready)
        await done
        try:
            os.close(mfd)
        except OSError:
            pass
        self._master_fd = None
        await self._proc.wait()
        await self._broadcast({"exit": self._proc.returncode, "label": self._label})

    async def _pump(self):
        async def _read(stream, name):
            while True:
                chunk = await stream.read(4096)
                if not chunk:
                    break
                await self._broadcast({"stream": name,
                                       "data": chunk.decode(errors="replace")})
        await asyncio.gather(
            _read(self._proc.stdout, "stdout"),
            _read(self._proc.stderr, "stderr"),
        )
        await self._proc.wait()
        await self._broadcast({"exit": self._proc.returncode, "label": self._label})

    async def _broadcast(self, msg: dict):
        dead = []
        for ws in list(self._clients):
            try:
                await ws.send_text(json.dumps(msg))
            except Exception:
                dead.append(ws)
        for ws in dead:
            try:
                self._clients.remove(ws)
            except ValueError:
                pass

    def add_client(self, ws: WebSocket):
        self._clients.append(ws)

    def remove_client(self, ws: WebSocket):
        try:
            self._clients.remove(ws)
        except ValueError:
            pass

    def info(self) -> dict:
        return {
            "running": self.is_running(),
            "label": self._label if self.is_running() else None,
            "pid": self._proc.pid if self.is_running() else None,
            "pty": self._master_fd is not None,
            "detached": False,
        }


# ---------------------------------------------------------------------------
# Detached job runner — benchmark processes with log tailing
# ---------------------------------------------------------------------------

class JobRunner:
    def __init__(self):
        self._proc: Optional[subprocess.Popen] = None
        self._label: str = ""
        self._log_file: Optional[Path] = None
        self._started: float = 0.0
        self._clients: list[WebSocket] = []
        self._lock = asyncio.Lock()

    def is_running(self) -> bool:
        return self._proc is not None and self._proc.poll() is None

    async def submit(self, cmd: list[str], label: str,
                     env: Optional[dict] = None) -> bool:
        async with self._lock:
            if self.is_running():
                return False
            self._label = label
            self._started = time.time()
            logs_dir = ROOT / "logs"
            logs_dir.mkdir(parents=True, exist_ok=True)
            ts = datetime.now().strftime("%Y-%m-%d_%H%M%S")
            self._log_file = logs_dir / f"gui_{ts}.log"
            proc_env = {**os.environ, **(env or {})}
            self._log_file.write_text(
                f"[GUI] {' '.join(cmd)}\n"
                f"[GUI] Started: {datetime.now().isoformat()}\n"
                f"[GUI] Log: {self._log_file}\n\n"
            )
            with open(self._log_file, "a") as lf:
                self._proc = subprocess.Popen(
                    cmd,
                    stdin=subprocess.DEVNULL,
                    stdout=lf,
                    stderr=lf,
                    cwd=str(ROOT),
                    env=proc_env,
                    preexec_fn=os.setsid,
                    close_fds=True,
                )
            asyncio.ensure_future(self._monitor())
            return True

    async def stop(self) -> bool:
        async with self._lock:
            if not self.is_running():
                return False
            try:
                pgid = os.getpgid(self._proc.pid)
                os.killpg(pgid, signal.SIGTERM)
                asyncio.ensure_future(self._force_kill(pgid))
            except (ProcessLookupError, OSError):
                try:
                    self._proc.terminate()
                except Exception:
                    pass
            return True

    async def _force_kill(self, pgid: int):
        await asyncio.sleep(6)
        if self.is_running():
            try:
                os.killpg(pgid, signal.SIGKILL)
            except (ProcessLookupError, OSError):
                pass

    async def _monitor(self):
        pos = 0
        log_path = self._log_file
        while True:
            rc = self._proc.poll()
            try:
                if log_path and log_path.exists():
                    with open(log_path, "rb") as f:
                        f.seek(pos)
                        chunk = f.read(65536)
                    if chunk:
                        pos += len(chunk)
                        await self._broadcast({
                            "stream": "stdout",
                            "data": chunk.decode(errors="replace"),
                        })
            except Exception:
                pass
            if rc is not None:
                await asyncio.sleep(0.3)
                try:
                    if log_path and log_path.exists():
                        with open(log_path, "rb") as f:
                            f.seek(pos)
                            chunk = f.read()
                        if chunk:
                            await self._broadcast({
                                "stream": "stdout",
                                "data": chunk.decode(errors="replace"),
                            })
                except Exception:
                    pass
                await self._broadcast({
                    "exit": rc, "label": self._label,
                    "log": str(log_path),
                })
                break
            await asyncio.sleep(0.4)

    async def _broadcast(self, msg: dict):
        dead = []
        for ws in list(self._clients):
            try:
                await ws.send_text(json.dumps(msg))
            except Exception:
                dead.append(ws)
        for ws in dead:
            try:
                self._clients.remove(ws)
            except ValueError:
                pass

    def add_client(self, ws: WebSocket):
        self._clients.append(ws)

    def remove_client(self, ws: WebSocket):
        try:
            self._clients.remove(ws)
        except ValueError:
            pass

    def info(self) -> dict:
        return {
            "running": self.is_running(),
            "label": self._label if self.is_running() else None,
            "pid": self._proc.pid if self._proc else None,
            "log_file": str(self._log_file) if self._log_file else None,
            "started": self._started if self.is_running() else None,
            "detached": True,
        }


mgr = SubprocessManager()
job_mgr = JobRunner()
_ms_results_path: Optional[str] = None


# ---------------------------------------------------------------------------
# Routes
# ---------------------------------------------------------------------------

@app.get("/", include_in_schema=False)
async def index():
    return FileResponse(str(STATIC_DIR / "index.html"))


@app.get("/api/status")
async def api_status():
    if job_mgr.is_running():
        return {"ok": True, **job_mgr.info()}
    return {"ok": True, **mgr.info()}


@app.get("/api/sysinfo")
async def api_sysinfo():
    from tools.bench_profile import (detect_cpu_model, detect_cpu_cores,
                                     detect_memory_gb, detect_os, detect_kernel)
    from tools.env_control import (detect_governor, detect_turbo, detect_thp,
                                   detect_swap_active, detect_temperatures,
                                   detect_rapl, detect_scaphandre_version)
    temps = detect_temperatures()
    physical, logical = detect_cpu_cores()
    docker_ok = subprocess.run(
        ["docker", "info"], capture_output=True
    ).returncode == 0

    cfg = load_config(str(DEFAULT_CONFIG))

    def _cfg_get(section, key, fb="--"):
        return cfg.get(section, key, fallback=fb) if cfg.has_section(section) else fb

    venv_ok = (ROOT / "srv" / "bin" / "python3").exists()

    return {
        "cpu_model": detect_cpu_model(),
        "cpu_physical": physical,
        "cpu_logical": logical,
        "memory_gb": round(detect_memory_gb(), 1),
        "os": detect_os(),
        "kernel": detect_kernel(),
        "hostname": os.uname().nodename,
        "governor": detect_governor(),
        "turbo": detect_turbo(),
        "thp": detect_thp(),
        "swap_active": detect_swap_active(),
        "temp_current": round(max(temps), 1) if temps else None,
        "rapl": detect_rapl(),
        "scaphandre": detect_scaphandre_version(),
        "docker_ok": docker_ok,
        "venv_ok": venv_ok,
        "config_exists": DEFAULT_CONFIG.exists(),
        "filter": _cfg_get("filter", "model", "none"),
        "runs": _cfg_get("measurement", "runs", "1"),
        "isolation": _cfg_get("isolation", "level", "none"),
    }


# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

@app.get("/api/config")
async def get_config():
    cfg = load_config(str(DEFAULT_CONFIG))
    return {s: dict(cfg[s]) for s in cfg.sections()}


@app.post("/api/config")
async def post_config(body: dict):
    cfg = configparser.ConfigParser()
    if DEFAULT_CONFIG.exists():
        cfg.read(str(DEFAULT_CONFIG))
    for section, keys in body.items():
        if not cfg.has_section(section):
            cfg.add_section(section)
        for key, value in keys.items():
            cfg.set(section, key, str(value))
    with open(str(DEFAULT_CONFIG), "w") as f:
        cfg.write(f)
    return {"ok": True}


# ---------------------------------------------------------------------------
# Filesystem browser
# ---------------------------------------------------------------------------

@app.get("/api/browse")
async def api_browse(path: str = Query(default=".")):
    base = Path(path) if Path(path).is_absolute() else ROOT / path
    try:
        base = base.resolve()
    except Exception:
        raise HTTPException(400, "Invalid path")
    home = Path.home()
    if not (str(base).startswith(str(ROOT)) or str(base).startswith(str(home))):
        raise HTTPException(403, "Access outside home not allowed")
    if not base.exists():
        raise HTTPException(404, f"Not found: {base}")
    if not base.is_dir():
        raise HTTPException(400, "Not a directory")
    entries = []
    try:
        for entry in sorted(base.iterdir(), key=lambda e: (e.is_file(), e.name.lower())):
            item = {"name": entry.name, "path": str(entry),
                    "type": "dir" if entry.is_dir() else "file"}
            if entry.is_file():
                try:
                    st = entry.stat()
                    item["size"] = st.st_size
                    item["mtime"] = st.st_mtime
                except OSError:
                    pass
            entries.append(item)
    except PermissionError:
        raise HTTPException(403, "Permission denied")
    parent = str(base.parent) if base != base.parent else None
    return {"path": str(base), "parent": parent, "entries": entries}


# ---------------------------------------------------------------------------
# Containers — recursive discovery under benchmarks/
# ---------------------------------------------------------------------------

def _discover_containers(bench_root: Path) -> list[dict]:
    """Find all directories containing a Dockerfile under bench_root.
    Handles both flat (type/container/) and nested (type/lang/fw/container/) layouts."""
    containers = []
    if not bench_root.exists():
        return containers
    for df in sorted(bench_root.rglob("Dockerfile")):
        container_dir = df.parent
        name = container_dir.name
        try:
            rel = container_dir.relative_to(bench_root)
        except ValueError:
            continue
        parts = rel.parts
        bench_type = parts[0] if parts else "unknown"
        try:
            path = str(container_dir.relative_to(ROOT))
        except ValueError:
            # Outside this repo — show relative to bench_root for cleaner display
            path = str(rel)
        containers.append({
            "name": name,
            "type": bench_type,
            "path": path,
        })
    return containers


@app.get("/api/containers")
async def get_containers(bench_dir: str = Query(default="")):
    root_bench = (Path(bench_dir) if bench_dir else ROOT / "benchmarks")
    if not root_bench.is_absolute():
        root_bench = ROOT / root_bench
    return _discover_containers(root_bench)


# ---------------------------------------------------------------------------
# Results
# ---------------------------------------------------------------------------

@app.get("/api/results/sessions")
async def list_sessions(root: str = Query(default="")):
    if root and Path(root).is_absolute():
        results_root = Path(root)
    elif root:
        results_root = ROOT / root
    else:
        results_root = ROOT / "results"
    sessions = []
    if not results_root.exists():
        return sessions
    for entry in sorted(results_root.iterdir(), reverse=True):
        if not entry.is_dir():
            continue
        types = [d.name for d in sorted(entry.iterdir()) if d.is_dir()]
        csv_count = len(list(entry.rglob("*.csv")))
        try:
            mtime = entry.stat().st_mtime
        except OSError:
            mtime = 0
        sessions.append({
            "name": entry.name, "path": str(entry),
            "mtime": mtime, "types": types, "csv_count": csv_count,
        })
    return sessions


@app.get("/api/results/files")
async def list_result_files(path: str = Query(default="")):
    search = Path(path) if path and Path(path).is_absolute() else ROOT / (path or "results")
    results = []
    if not search.exists():
        return results
    for p in sorted(search.rglob("*.csv")):
        try:
            st = p.stat()
            rel_parts = p.relative_to(search).parts
            bench_type = rel_parts[0] if len(rel_parts) > 1 else "unknown"
            results.append({
                "name": p.name, "stem": p.stem,
                "path": str(p), "type": bench_type,
                "size": st.st_size, "mtime": st.st_mtime,
            })
        except (OSError, ValueError):
            pass
    return results


@app.get("/api/results/csv")
async def get_csv_data(path: str = Query(...)):
    p = Path(path) if Path(path).is_absolute() else ROOT / path
    if not p.exists() or p.suffix != ".csv":
        raise HTTPException(404, f"CSV not found: {path}")
    rows = []
    headers = []
    with open(p, newline="") as f:
        reader = csv.DictReader(f)
        headers = reader.fieldnames or []
        for row in reader:
            rows.append(dict(row))
    return {"headers": headers, "rows": rows, "path": str(p), "name": p.name}


# ---------------------------------------------------------------------------
# Logs
# ---------------------------------------------------------------------------

@app.get("/api/logs")
async def list_logs():
    logs_dir = ROOT / "logs"
    if not logs_dir.exists():
        return []
    files = []
    for p in sorted(logs_dir.glob("*.log"), reverse=True):
        try:
            st = p.stat()
            files.append({"name": p.name, "path": str(p),
                          "size": st.st_size, "mtime": st.st_mtime})
        except OSError:
            pass
    return files


@app.get("/api/logs/content")
async def get_log_content(path: str = Query(...), tail: int = Query(default=200)):
    p = Path(path) if Path(path).is_absolute() else ROOT / path
    if not p.exists() or p.suffix != ".log":
        raise HTTPException(404, "Log not found")
    with open(p) as f:
        lines = f.readlines()
    return {"lines": lines[-tail:], "total": len(lines), "name": p.name}


# ---------------------------------------------------------------------------
# Subprocess launchers
# ---------------------------------------------------------------------------

VALID_TARGETS = {
    "run", "run-all", "run-quick", "run-super-quick",
    "run-single", "run-single-super-quick",
    "run-static", "run-dynamic", "run-websocket",
    "check-health", "test", "validate", "check-tools",
    "build", "build-test-run", "setup", "init", "install",
    "clean-results", "clean-build", "clean-env", "clean-all",
    "clean-build-run", "clean-all-build-run",
    "clean-port", "clean-benchmarks", "clean-nuclear", "clean-repo",
    "graph",
}


@app.post("/api/sudo-auth")
async def api_sudo_auth():
    if mgr.is_running():
        raise HTTPException(409, "A process is already running")
    ok = await mgr.start(["sudo", "-v"], label="sudo-auth", use_pty=True)
    return {"ok": ok}


@app.post("/api/profile")
async def api_profile():
    if mgr.is_running():
        raise HTTPException(409, "A process is already running")
    await mgr.start([sys.executable, str(TOOLS_DIR / "bench_profile.py")],
                    label="profiler")
    return {"ok": True}


@app.post("/api/model-selector")
async def api_model_selector(body: dict):
    global _ms_results_path
    if mgr.is_running():
        raise HTTPException(409, "A process is already running")
    container = body.get("container")
    metric = body.get("metric", "cv")
    apply_flag = body.get("apply", False)
    input_glob = body.get("input", "output/*.json")
    tmp = tempfile.NamedTemporaryFile(suffix=".json", delete=False)
    tmp.close()
    _ms_results_path = tmp.name
    cmd = [sys.executable, str(TOOLS_DIR / "model_selector.py"),
           "--input", input_glob, "--metric", metric,
           "--json-output", _ms_results_path]
    if container:
        cmd += ["--container", container]
    if apply_flag:
        cmd.append("--apply")
    await mgr.start(cmd, label="model-selector")
    return {"ok": True}


@app.post("/api/run")
async def api_run(body: dict):
    target = body.get("target", "run")
    env_overrides = body.get("env", {})
    server = body.get("server", "").strip()
    if target not in VALID_TARGETS:
        raise HTTPException(400, f"Unknown target: {target}")
    cmd = ["make", "-C", str(ROOT), target]
    if server:
        cmd.append(f"SERVER={server}")
    for k, v in env_overrides.items():
        cmd.append(f"{k}={v}")
    label = target + (f":{server}" if server else "")
    ok = await job_mgr.submit(cmd, label=label, env=env_overrides or None)
    if not ok:
        raise HTTPException(409, "A job is already running")
    return {"ok": True, "label": label, "log": str(job_mgr._log_file)}


@app.post("/api/stop")
async def api_stop():
    ok = await job_mgr.stop() or await mgr.stop()
    return {"ok": ok}


@app.post("/api/resize-pty")
async def api_resize_pty(body: dict):
    rows = int(body.get("rows", 24))
    cols = int(body.get("cols", 220))
    mgr.resize_pty(rows, cols)
    return {"ok": True}


# ---------------------------------------------------------------------------
# WebSocket terminal
# ---------------------------------------------------------------------------

@app.websocket("/ws/terminal")
async def ws_terminal(ws: WebSocket):
    await ws.accept()
    mgr.add_client(ws)
    job_mgr.add_client(ws)
    try:
        while True:
            try:
                raw = await asyncio.wait_for(ws.receive_text(), timeout=30)
                try:
                    msg = json.loads(raw)
                    if msg.get("type") == "input":
                        mgr.write_stdin(msg["data"])
                    elif msg.get("type") == "resize":
                        mgr.resize_pty(int(msg.get("rows", 24)),
                                       int(msg.get("cols", 220)))
                except (json.JSONDecodeError, KeyError):
                    mgr.write_stdin(raw)
            except asyncio.TimeoutError:
                await ws.send_text(json.dumps({"ping": True}))
    except (WebSocketDisconnect, Exception):
        pass
    finally:
        mgr.remove_client(ws)
        job_mgr.remove_client(ws)


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main():
    import argparse
    parser = argparse.ArgumentParser(description="Benchmark GUI server")
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8080)
    parser.add_argument("--reload", action="store_true")
    args = parser.parse_args()
    cfg = load_config(str(DEFAULT_CONFIG))
    if cfg.has_option("gui", "port"):
        try:
            args.port = int(cfg.get("gui", "port"))
        except ValueError:
            pass
    print(f"Benchmark GUI: http://{args.host}:{args.port}")
    uvicorn.run("tools.gui.server:app", host=args.host, port=args.port,
                reload=args.reload)


if __name__ == "__main__":
    main()
