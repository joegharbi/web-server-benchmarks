"""
Benchmark GUI server — FastAPI backend.

Start with:
    python tools/gui/server.py   (or: make gui)

Architecture
------------
Two execution modes:

1. Interactive (PTY) — used for sudo-auth, profiler, model-selector:
   SubprocessManager runs the process attached to a PTY so the user can type
   interactively in the browser terminal. Output streamed via WebSocket.

2. Detached job — used for all `make run*` benchmark targets:
   The benchmark subprocess is fully detached (new process group, stdin=DEVNULL,
   stdout/stderr→log file). It survives GUI server restarts and SSH disconnects.
   The server tails the log file and streams new lines to WebSocket clients.
   An email is sent when the job finishes (configured in bench.config [notifications]).

Endpoints
---------
GET  /                          → index.html
GET  /api/status                → server alive + active job/process info
GET  /api/sysinfo               → live system readings (CPU, temp, governor, …)
GET  /api/config                → read bench.config as JSON
POST /api/config                → write bench.config from JSON body
GET  /api/browse?path=DIR       → list files/dirs under DIR (filesystem browser)
GET  /api/containers            → list discovered Docker containers (from benchmarks/)
GET  /api/results               → list result CSVs (recursive under results/)
GET  /api/results/sessions      → list benchmark sessions (results/<timestamp>/)
GET  /api/results/file?path=P   → return CSV rows as JSON for any absolute path
GET  /api/output-files          → scan output/*.json and return list of files
GET  /api/json-containers       → scan Scaphandre JSON for unique container names
GET  /api/json-processes        → scan Scaphandre JSON for unique process exe names
GET  /api/json-for-session      → find output JSON files belonging to a specific session
GET  /api/session-containers    → find container names from a session's CSV files
POST /api/sudo-auth             → run sudo -v via PTY (cache credentials)
POST /api/profile               → run bench_profile.py
POST /api/model-selector        → run model_selector.py
POST /api/run                   → submit detached benchmark job
POST /api/stop                  → kill active job or interactive process
POST /api/resize-pty            → resize PTY window
WS   /ws/terminal               → live output (JSON lines); stdin forwarded to PTY
"""

import asyncio
import configparser
import csv
import fcntl
import glob
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
from typing import List, Optional

import uvicorn
from fastapi import FastAPI, HTTPException, Query, WebSocket, WebSocketDisconnect
from fastapi.responses import FileResponse, JSONResponse
from fastapi.staticfiles import StaticFiles

ROOT       = Path(__file__).resolve().parent.parent.parent
STATIC_DIR = Path(__file__).resolve().parent / "static"
TOOLS_DIR  = ROOT / "tools"
DEFAULT_CONFIG = ROOT / "bench.config"
RESULTS_DIR    = ROOT / "results"
LOGS_DIR       = ROOT / "logs"

sys.path.insert(0, str(ROOT))
from tools.bench_profile import load_config

app = FastAPI(title="Benchmark GUI")
app.mount("/static", StaticFiles(directory=str(STATIC_DIR)), name="static")


# ---------------------------------------------------------------------------
# Subprocess manager — one active process at a time
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Email helper
# ---------------------------------------------------------------------------

def _send_email_sync(cfg: configparser.ConfigParser, subject: str, body: str) -> bool:
    """Send a notification email via the system sendmail command."""
    try:
        email_to   = cfg.get("notifications", "email_to",   fallback="").strip()
        email_from = cfg.get("notifications", "email_from", fallback="benchmark@localhost").strip()
        if not email_to:
            return False
        message = f"From: {email_from}\nTo: {email_to}\nSubject: {subject}\n\n{body}"
        result = subprocess.run(
            ["sendmail", "-t"],
            input=message.encode(),
            capture_output=True,
            timeout=30,
        )
        return result.returncode == 0
    except Exception as exc:
        print(f"[EMAIL] Failed to send: {exc}", flush=True)
        return False


# ---------------------------------------------------------------------------
# Interactive process manager — PTY or pipe, for sudo-auth / profiler / MS
# ---------------------------------------------------------------------------

class SubprocessManager:
    """Runs short interactive processes (sudo-auth, profiler, model-selector).
    Uses a PTY when requested so the user can type in the browser terminal."""

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
                fcntl.ioctl(slave_fd, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 220, 0, 0))
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
        mfd  = self._master_fd

        def _read_ready():
            try:
                data = os.read(mfd, 4096)
                if data:
                    asyncio.ensure_future(
                        self._broadcast({"stream": "stdout",
                                         "data": data.decode(errors="replace")})
                    )
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
            "label":   self._label if self.is_running() else None,
            "pid":     self._proc.pid if self.is_running() else None,
            "pty":     self._master_fd is not None,
            "detached": False,
        }


# ---------------------------------------------------------------------------
# Job runner — fully detached benchmark processes with log-file tailing
# ---------------------------------------------------------------------------

class JobRunner:
    """Runs make benchmark targets as detached OS processes.

    The subprocess is in its own process group (os.setsid) with stdin=DEVNULL
    and stdout/stderr piped to a log file under logs/.  It survives GUI server
    restarts and SSH session disconnects.  The runner tails the log file and
    broadcasts new content to connected WebSocket clients.  An email is sent
    on completion if bench.config [notifications] is configured.
    """

    def __init__(self):
        self._proc:     Optional[subprocess.Popen] = None
        self._label:    str  = ""
        self._log_file: Optional[Path] = None
        self._started:  float = 0.0
        self._clients:  list[WebSocket] = []
        self._lock = asyncio.Lock()

    def is_running(self) -> bool:
        return self._proc is not None and self._proc.poll() is None

    async def submit(self, cmd: list[str], label: str,
                     env: Optional[dict] = None) -> bool:
        async with self._lock:
            if self.is_running():
                return False
            self._label   = label
            self._started = time.time()
            LOGS_DIR.mkdir(parents=True, exist_ok=True)
            ts = datetime.now().strftime("%Y-%m-%d_%H%M%S")
            self._log_file = LOGS_DIR / f"gui_{ts}.log"
            proc_env = {**os.environ, **(env or {})}
            # Write a header so the log file exists before tailing starts
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
                    preexec_fn=os.setsid,   # detach from server's process group
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
        """Send SIGKILL to the process group if still alive after 6 seconds."""
        await asyncio.sleep(6)
        if self.is_running():
            try:
                os.killpg(pgid, signal.SIGKILL)
            except (ProcessLookupError, OSError):
                pass

    async def _monitor(self):
        """Tail log file → broadcast → email on completion."""
        pos = 0
        log_path = self._log_file

        while True:
            rc = self._proc.poll()
            # Drain new log content
            try:
                if log_path and log_path.exists():
                    with open(log_path, "rb") as f:
                        f.seek(pos)
                        chunk = f.read(65536)
                    if chunk:
                        pos += len(chunk)
                        await self._broadcast({
                            "stream": "stdout",
                            "data":   chunk.decode(errors="replace"),
                        })
            except Exception:
                pass

            if rc is not None:
                await asyncio.sleep(0.3)   # final drain
                try:
                    if log_path and log_path.exists():
                        with open(log_path, "rb") as f:
                            f.seek(pos)
                            chunk = f.read()
                        if chunk:
                            await self._broadcast({
                                "stream": "stdout",
                                "data":   chunk.decode(errors="replace"),
                            })
                except Exception:
                    pass
                duration = time.time() - self._started
                await self._broadcast({"exit": rc, "label": self._label,
                                        "log": str(log_path)})
                asyncio.ensure_future(self._notify(rc, duration, log_path))
                break

            await asyncio.sleep(0.4)

    async def _notify(self, rc: int, duration: float, log_path: Path):
        cfg = load_config(str(DEFAULT_CONFIG))
        if not cfg.has_section("notifications"):
            return
        notify_on = cfg.get("notifications", "notify_on", fallback="always")
        if notify_on == "success" and rc != 0:
            return
        if notify_on == "failure" and rc == 0:
            return
        mins, secs = divmod(int(duration), 60)
        hours, mins = divmod(mins, 60)
        dur_str = (f"{hours}h " if hours else "") + f"{mins}m {secs}s"
        status  = "COMPLETED ✓" if rc == 0 else f"FAILED (exit {rc})"
        subject = f"[Benchmark] {self._label} — {status}"
        body    = (
            f"Benchmark job: {self._label}\n"
            f"Status:        {status}\n"
            f"Duration:      {dur_str}\n"
            f"Log file:      {log_path}\n"
            f"Results dir:   {ROOT / 'results'}\n\n"
            f"Started:  {datetime.fromtimestamp(self._started).strftime('%Y-%m-%d %H:%M:%S')}\n"
            f"Finished: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n"
        )
        loop = asyncio.get_running_loop()
        await loop.run_in_executor(None, _send_email_sync, cfg, subject, body)

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
            "running":  self.is_running(),
            "label":    self._label    if self.is_running() else None,
            "pid":      self._proc.pid if self._proc else None,
            "log_file": str(self._log_file) if self._log_file else None,
            "started":  self._started  if self.is_running() else None,
            "detached": True,
        }


mgr     = SubprocessManager()   # interactive: sudo-auth, profiler, model-selector
job_mgr = JobRunner()            # detached:    all make benchmark targets

# Path to the last model-selector JSON results (written by --json-output)
_ms_results_path: Optional[str] = None


# ---------------------------------------------------------------------------
# Static / index
# ---------------------------------------------------------------------------

@app.get("/", include_in_schema=False)
async def index():
    return FileResponse(str(STATIC_DIR / "index.html"))


# ---------------------------------------------------------------------------
# Status + sysinfo
# ---------------------------------------------------------------------------

@app.get("/api/status")
async def api_status():
    # Job runner takes priority in status (benchmark runs are the primary use-case)
    if job_mgr.is_running():
        return {"ok": True, **job_mgr.info()}
    return {"ok": True, **mgr.info()}


@app.get("/api/sysinfo")
async def api_sysinfo():
    from tools.bench_profile import detect_cpu_model, detect_cpu_cores, detect_memory_gb, detect_os, detect_kernel
    from tools.env_control import (detect_governor, detect_turbo, detect_thp,
                                    detect_swap_active, detect_temperatures,
                                    detect_rapl, detect_scaphandre_version)
    import subprocess as _sp
    temps = detect_temperatures()
    physical, logical = detect_cpu_cores()
    docker_ok = _sp.run(["docker", "info"], capture_output=True).returncode == 0

    # Pull current filter/runs/isolation from bench.config for the status grid
    cfg = load_config(str(DEFAULT_CONFIG))
    filter_model = cfg.get("filter", "model", fallback="none") if cfg.has_section("filter") else "none"
    runs         = cfg.get("measurement", "runs", fallback="—") if cfg.has_section("measurement") else "—"
    isolation    = cfg.get("isolation", "level", fallback="—") if cfg.has_section("isolation") else "—"

    return {
        "cpu_model":    detect_cpu_model(),
        "cpu_physical": physical,
        "cpu_logical":  logical,
        "memory_gb":    round(detect_memory_gb(), 1),
        "os":           detect_os(),
        "kernel":       detect_kernel(),
        "governor":     detect_governor(),
        "turbo":        detect_turbo(),
        "thp":          detect_thp(),
        "swap_active":  detect_swap_active(),
        "temp_current": round(max(temps), 1) if temps else None,
        "rapl":         detect_rapl(),
        "scaphandre":   detect_scaphandre_version(),
        "docker_ok":    docker_ok,
        "filter":       filter_model,
        "runs":         runs,
        "isolation":    isolation,
    }


# ---------------------------------------------------------------------------
# bench.config
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
    return {"ok": True, "path": str(DEFAULT_CONFIG)}


# ---------------------------------------------------------------------------
# Filesystem browser
# ---------------------------------------------------------------------------

@app.get("/api/browse")
async def api_browse(path: str = Query(default=".")):
    base = Path(path) if Path(path).is_absolute() else ROOT / path
    try:
        base = base.resolve()
    except Exception:
        raise HTTPException(status_code=400, detail="Invalid path")

    home = Path.home()
    if not (str(base).startswith(str(ROOT)) or str(base).startswith(str(home))):
        raise HTTPException(status_code=403, detail="Access outside project root not allowed")

    if not base.exists():
        raise HTTPException(status_code=404, detail=f"Path not found: {base}")
    if not base.is_dir():
        raise HTTPException(status_code=400, detail="Path is not a directory")

    entries = []
    try:
        for entry in sorted(base.iterdir(), key=lambda e: (e.is_file(), e.name.lower())):
            item: dict = {"name": entry.name, "path": str(entry), "type": "dir" if entry.is_dir() else "file"}
            if entry.is_file():
                try:
                    stat = entry.stat()
                    item["size"] = stat.st_size
                    item["mtime"] = stat.st_mtime
                except OSError:
                    pass
            entries.append(item)
    except PermissionError:
        raise HTTPException(status_code=403, detail="Permission denied")

    parent = str(base.parent) if base != base.parent else None
    return {"path": str(base), "parent": parent, "entries": entries}


# ---------------------------------------------------------------------------
# Containers — scanned from benchmarks/<type>/<name>/Dockerfile
# ---------------------------------------------------------------------------

@app.get("/api/containers")
async def get_containers(bench_dir: str = Query(default="")):
    """Discover containers from benchmarks/<type>/<name>/Dockerfile.
    bench_dir overrides the default 'benchmarks' directory (matches BENCH_DIR Makefile var)."""
    root_bench = (Path(bench_dir) if bench_dir else ROOT / "benchmarks")
    if not root_bench.is_absolute():
        root_bench = ROOT / root_bench
    containers = []
    if root_bench.exists():
        for type_dir in sorted(root_bench.iterdir()):
            if not type_dir.is_dir():
                continue
            type_name = type_dir.name
            for entry in sorted(type_dir.iterdir()):
                if not entry.is_dir():
                    continue
                if (entry / "Dockerfile").exists() or (entry / "docker-compose.yml").exists():
                    try:
                        rel = str(entry.relative_to(ROOT))
                    except ValueError:
                        rel = str(entry)
                    containers.append({
                        "name":  entry.name,
                        "type":  type_name,
                        "path":  rel,
                    })
    return containers


# ---------------------------------------------------------------------------
# Results — recursive search
# ---------------------------------------------------------------------------

def _find_csvs(root_dir: Path) -> list[dict]:
    results = []
    if not root_dir.exists():
        return results
    for p in sorted(root_dir.rglob("*.csv")):
        try:
            stat = p.stat()
            results.append({
                "name":  p.name,
                "stem":  p.stem,
                "path":  str(p),
                "rel":   str(p.relative_to(ROOT)),
                "size":  stat.st_size,
                "mtime": stat.st_mtime,
            })
        except OSError:
            pass
    return results


@app.get("/api/results/sessions")
async def list_sessions():
    """List benchmark sessions as {name, path, mtime, types:[...]}."""
    results_root = ROOT / "results"
    sessions = []
    if not results_root.exists():
        return sessions
    for entry in sorted(results_root.iterdir(), reverse=True):
        if not entry.is_dir():
            continue
        types = [d.name for d in sorted(entry.iterdir()) if d.is_dir()]
        try:
            mtime = entry.stat().st_mtime
        except OSError:
            mtime = 0
        sessions.append({"name": entry.name, "path": str(entry), "mtime": mtime, "types": types})
    return sessions


@app.get("/api/results")
async def list_results(path: Optional[str] = None):
    if path:
        search_root = Path(path) if Path(path).is_absolute() else ROOT / path
        return _find_csvs(search_root)
    return _find_csvs(RESULTS_DIR)


@app.get("/api/results/file")
async def get_result_file(path: str = Query(...)):
    p = Path(path) if Path(path).is_absolute() else ROOT / path
    if not p.exists() or p.suffix != ".csv":
        raise HTTPException(status_code=404, detail=f"CSV not found: {path}")
    rows = []
    with open(p, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            rows.append(dict(row))
    return rows


# ---------------------------------------------------------------------------
# Output files (for Model Selector tab)
# ---------------------------------------------------------------------------

@app.get("/api/output-files")
async def get_output_files(path: str = Query(default="output")):
    """Scan output/*.json and return list of {name, path, mtime, size}."""
    search_root = Path(path) if Path(path).is_absolute() else ROOT / path
    files = []
    if not search_root.exists():
        return files
    for p in sorted(search_root.rglob("*.json"), key=lambda x: x.stat().st_mtime, reverse=True):
        try:
            stat = p.stat()
            files.append({
                "name":  p.name,
                "path":  str(p),
                "mtime": stat.st_mtime,
                "size":  stat.st_size,
            })
        except OSError:
            pass
    return files


# ---------------------------------------------------------------------------
# JSON container scanner (for Model Selector tab)
# ---------------------------------------------------------------------------

@app.get("/api/json-containers")
async def get_json_containers(path: str = Query(default="output")):
    """Scan Scaphandre JSON files and return unique container names found in consumers."""
    search_root = Path(path) if Path(path).is_absolute() else ROOT / path
    containers: set[str] = set()
    if not search_root.exists():
        return []
    for p in sorted(search_root.rglob("*.json")):
        try:
            with open(p) as f:
                data = json.load(f)
            if not isinstance(data, list):
                continue
            for entry in data:
                for consumer in entry.get("consumers", []):
                    cname = (consumer.get("container") or {}).get("name", "").strip()
                    if cname:
                        containers.add(cname)
        except Exception:
            continue
    return sorted(containers)


@app.get("/api/json-processes")
async def get_json_processes(path: str = Query(default="output")):
    """Scan Scaphandre JSON files and return unique process exe basenames.
    Used as a fallback when Scaphandre was not run with --containers."""
    import os as _os
    search_root = Path(path) if Path(path).is_absolute() else ROOT / path
    processes: set[str] = set()
    if not search_root.exists():
        return []
    for p in sorted(search_root.rglob("*.json")):
        try:
            with open(p) as f:
                data = json.load(f)
            if not isinstance(data, list):
                continue
            for entry in data:
                for consumer in entry.get("consumers", []):
                    power = consumer.get("consumption", 0.0)
                    if not power or power <= 0:
                        continue
                    exe = consumer.get("exe", "") or consumer.get("cmdline", "") or ""
                    if exe:
                        name = _os.path.basename(exe.split()[0])
                        if name:
                            processes.add(name)
        except Exception:
            continue
    return sorted(processes)


# ---------------------------------------------------------------------------
# Subprocess launchers
# ---------------------------------------------------------------------------

@app.post("/api/sudo-auth")
async def api_sudo_auth():
    """Run sudo -v through a PTY so the user can type their password in the terminal."""
    if mgr.is_running():
        raise HTTPException(status_code=409, detail="A process is already running")
    ok = await mgr.start(["sudo", "-v"], label="sudo-auth", use_pty=True)
    return {"ok": ok}


@app.post("/api/profile")
async def api_profile():
    if mgr.is_running():
        raise HTTPException(status_code=409, detail="A process is already running")
    cmd = [sys.executable, str(TOOLS_DIR / "bench_profile.py")]
    await mgr.start(cmd, label="profiler")
    return {"ok": True}


@app.get("/api/json-for-session")
async def get_json_for_session(
    session_path: str = Query(...),
    folder: str = Query(default="output"),
):
    """
    Return output JSON file paths that belong to a specific benchmark session.

    A session directory is named YYYY-MM-DD_HHMMSS (when make run started).
    Output JSON files are named YYYY-MM-DD-HHMMSS[_runXofY].json (per measurement).
    We find JSON files whose filename timestamp falls between the session start
    and the last CSV modification time in the session (+ a small buffer).
    """
    session_dir = Path(session_path) if Path(session_path).is_absolute() else ROOT / session_path
    if not session_dir.exists():
        raise HTTPException(status_code=404, detail=f"Session not found: {session_path}")

    try:
        session_start = datetime.strptime(session_dir.name, "%Y-%m-%d_%H%M%S")
    except ValueError:
        raise HTTPException(status_code=400, detail="Session directory name must be YYYY-MM-DD_HHMMSS")

    # Find session end time from latest CSV mtime
    csv_files = list(session_dir.rglob("*.csv"))
    if csv_files:
        last_mtime = max(f.stat().st_mtime for f in csv_files)
        session_end = datetime.fromtimestamp(last_mtime) + timedelta(minutes=10)
    else:
        session_end = session_start + timedelta(hours=24)

    # Search output folder for matching JSON files
    output_dir = Path(folder) if Path(folder).is_absolute() else ROOT / folder
    matching = []
    if not output_dir.exists():
        return matching

    for json_file in sorted(output_dir.rglob("*.json")):
        stem = json_file.stem  # e.g. 2026-05-28-142135 or 2026-05-28-142135_run1of10
        ts_part = stem.split("_")[0] if "_" in stem else stem
        try:
            file_dt = datetime.strptime(ts_part, "%Y-%m-%d-%H%M%S")
        except ValueError:
            continue
        if session_start <= file_dt <= session_end:
            matching.append(str(json_file))

    return matching


@app.get("/api/session-containers")
async def get_session_containers(session_path: str = Query(...)):
    """
    Return unique container names found in a session's CSV files.
    The 'Container Name' column is present in all benchmark result CSVs.
    This is more reliable than scanning JSON files (which may lack container metadata).
    """
    session_dir = Path(session_path) if Path(session_path).is_absolute() else ROOT / session_path
    if not session_dir.exists():
        return []
    containers: set[str] = set()
    for csv_path in sorted(session_dir.rglob("*.csv")):
        try:
            with open(csv_path, newline="") as f:
                reader = csv.DictReader(f)
                for row in reader:
                    name = (row.get("Container Name") or "").strip()
                    if name:
                        containers.add(name)
        except Exception:
            continue
    return sorted(containers)


@app.get("/api/model-selector-results")
async def api_model_selector_results():
    """Return the last model-selector JSON results (written by --json-output)."""
    global _ms_results_path
    if not _ms_results_path or not Path(_ms_results_path).exists():
        raise HTTPException(status_code=404, detail="No results available yet")
    try:
        with open(_ms_results_path) as f:
            return json.load(f)
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/api/model-selector")
async def api_model_selector(body: dict):
    global _ms_results_path
    if mgr.is_running():
        raise HTTPException(status_code=409, detail="A process is already running")

    container    = body.get("container")
    process      = body.get("process")
    process_mode = body.get("process_mode", False)
    metric       = body.get("metric", "cv")
    apply_flag   = body.get("apply", False)
    json_files   = body.get("json_files", [])

    if json_files:
        first = Path(json_files[0])
        input_glob = str(first.parent / "*.json")
    else:
        input_glob = body.get("input", "output/*.json")

    # Temp file for structured JSON results (displayed in-page)
    tmp = tempfile.NamedTemporaryFile(suffix=".json", delete=False)
    tmp.close()
    _ms_results_path = tmp.name

    cmd = [sys.executable, str(TOOLS_DIR / "model_selector.py"),
           "--input", input_glob, "--metric", metric,
           "--json-output", _ms_results_path]

    if process or process_mode:
        if process:
            cmd += ["--process", process]
        else:
            cmd.append("--process-mode")
    elif container:
        cmd += ["--container", container]

    if apply_flag:
        cmd.append("--apply")

    await mgr.start(cmd, label="model-selector")
    return {"ok": True, "cmd": " ".join(cmd)}


@app.post("/api/run")
async def api_run(body: dict):
    """
    Invoke a make target.

    Body fields
    -----------
    target  : make target (run, run-quick, run-super-quick, run-single, etc.)
    env     : dict of env var overrides (e.g. {"HTTP_MAX_WORKERS": "50"})
    server  : container image name for run-single / run-single-super-quick
    """
    if mgr.is_running():
        raise HTTPException(status_code=409, detail="A process is already running")

    target = body.get("target", "run")
    env_overrides = body.get("env", {})
    server = body.get("server", "").strip()

    # Valid targets
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
    if target not in VALID_TARGETS:
        raise HTTPException(status_code=400, detail=f"Unknown target: {target}")

    cmd = ["make", "-C", str(ROOT), target]
    if server:
        cmd.append(f"SERVER={server}")
    for k, v in env_overrides.items():
        cmd.append(f"{k}={v}")

    label = target + (f":{server}" if server else "")
    ok = await job_mgr.submit(cmd, label=label, env=env_overrides if env_overrides else None)
    if not ok:
        raise HTTPException(status_code=409, detail="A benchmark job is already running")
    return {"ok": True, "cmd": " ".join(cmd), "log": str(job_mgr._log_file)}


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
    # Register with both managers so output from either reaches the browser
    mgr.add_client(ws)
    job_mgr.add_client(ws)
    try:
        while True:
            try:
                raw = await asyncio.wait_for(ws.receive_text(), timeout=30)
                try:
                    msg = json.loads(raw)
                    if msg.get("type") == "input":
                        mgr.write_stdin(msg["data"])   # only PTY mgr accepts stdin
                    elif msg.get("type") == "resize":
                        mgr.resize_pty(int(msg.get("rows", 24)), int(msg.get("cols", 220)))
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

    print(f"GUI server: http://{args.host}:{args.port}")
    uvicorn.run("tools.gui.server:app", host=args.host, port=args.port, reload=args.reload)


if __name__ == "__main__":
    main()
