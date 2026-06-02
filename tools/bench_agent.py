"""
Minimal benchmark agent.

Listens on stdin for a single JSON command, runs it as a subprocess,
streams stdout/stderr back over stdout, and exits.

Designed to be invoked over SSH:
    ssh user@host 'python3 tools/bench_agent.py' <<< '{"cmd": ["make", "run"]}'

The GUI (Phase 2) will call this via paramiko or subprocess+SSH to trigger
remote benchmark runs while the GUI itself stays on the local machine.

Protocol
--------
Input  (one line of JSON on stdin):
    {"cmd": ["python3", "tools/measure_docker.py", "--server_image", "nginx"]}

Output (line-delimited, each line is JSON):
    {"stream": "stdout", "data": "...\n"}
    {"stream": "stderr", "data": "...\n"}
    {"exit": 0}

Errors before the subprocess starts:
    {"error": "description"}
"""

import json
import os
import select
import signal
import subprocess
import sys
import threading


_CHUNK = 4096


def _stream_output(proc: subprocess.Popen) -> None:
    """Read stdout and stderr from proc concurrently, emit JSON lines."""
    fds = {proc.stdout.fileno(): ("stdout", proc.stdout),
           proc.stderr.fileno(): ("stderr", proc.stderr)}
    open_fds = set(fds)

    while open_fds:
        try:
            readable, _, _ = select.select(list(open_fds), [], [], 0.5)
        except (ValueError, OSError):
            break

        for fd in readable:
            stream_name, fobj = fds[fd]
            chunk = fobj.read(_CHUNK)
            if chunk:
                line = json.dumps({"stream": stream_name, "data": chunk})
                sys.stdout.write(line + "\n")
                sys.stdout.flush()
            else:
                open_fds.discard(fd)

        if proc.poll() is not None and not readable:
            break


def run_command(cmd: list[str]) -> int:
    """Run cmd, stream output as JSON lines, return exit code."""
    try:
        proc = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
        )
    except (FileNotFoundError, PermissionError) as exc:
        sys.stdout.write(json.dumps({"error": str(exc)}) + "\n")
        sys.stdout.flush()
        return 127

    # Forward SIGTERM to child
    def _on_sigterm(signum, frame):
        proc.terminate()

    signal.signal(signal.SIGTERM, _on_sigterm)

    _stream_output(proc)
    proc.wait()
    return proc.returncode


def main() -> None:
    raw = sys.stdin.readline()
    if not raw.strip():
        sys.stdout.write(json.dumps({"error": "empty input"}) + "\n")
        sys.stdout.flush()
        sys.exit(1)

    try:
        request = json.loads(raw)
    except json.JSONDecodeError as exc:
        sys.stdout.write(json.dumps({"error": f"invalid JSON: {exc}"}) + "\n")
        sys.stdout.flush()
        sys.exit(1)

    cmd = request.get("cmd")
    if not cmd or not isinstance(cmd, list):
        sys.stdout.write(json.dumps({"error": "'cmd' must be a non-empty list"}) + "\n")
        sys.stdout.flush()
        sys.exit(1)

    exit_code = run_command(cmd)
    sys.stdout.write(json.dumps({"exit": exit_code}) + "\n")
    sys.stdout.flush()
    sys.exit(exit_code)


if __name__ == "__main__":
    main()
