"""
Tests for tools/bench_agent.py

Covers:
  - run_command() streams stdout and stderr as JSON lines
  - run_command() returns correct exit code
  - run_command() handles missing binary gracefully (exit 127)
  - main() rejects empty input
  - main() rejects invalid JSON
  - main() rejects missing 'cmd' field
  - main() emits {"exit": N} as the final line
  - Full round-trip via subprocess (invoke bench_agent.py as a child process)
"""

import json
import os
import subprocess
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

AGENT = os.path.join(os.path.dirname(__file__), "..", "tools", "bench_agent.py")
PYTHON = sys.executable


def _run_agent(stdin_text: str) -> tuple[list[dict], int]:
    """Invoke bench_agent.py with stdin_text. Returns (parsed_lines, process_exit_code)."""
    result = subprocess.run(
        [PYTHON, AGENT],
        input=stdin_text,
        capture_output=True,
        text=True,
    )
    lines = []
    for line in result.stdout.splitlines():
        line = line.strip()
        if line:
            try:
                lines.append(json.loads(line))
            except json.JSONDecodeError:
                pass
    return lines, result.returncode


# ---------------------------------------------------------------------------
# run_command() unit tests
# ---------------------------------------------------------------------------

class TestRunCommand:
    def test_exit_code_zero_on_success(self):
        from tools.bench_agent import run_command
        import io, contextlib
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            code = run_command([PYTHON, "-c", "print('hello')"])
        assert code == 0

    def test_exit_code_nonzero_on_failure(self):
        from tools.bench_agent import run_command
        import io, contextlib
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            code = run_command([PYTHON, "-c", "import sys; sys.exit(42)"])
        assert code == 42

    def test_stdout_captured_as_json(self):
        from tools.bench_agent import run_command
        import io, contextlib
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            run_command([PYTHON, "-c", "print('hello world')"])
        output = buf.getvalue()
        lines = [json.loads(l) for l in output.splitlines() if l.strip()]
        stdout_data = "".join(l["data"] for l in lines if l.get("stream") == "stdout")
        assert "hello world" in stdout_data

    def test_stderr_captured_as_json(self):
        from tools.bench_agent import run_command
        import io, contextlib
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            run_command([PYTHON, "-c", "import sys; sys.stderr.write('err msg\n')"])
        output = buf.getvalue()
        lines = [json.loads(l) for l in output.splitlines() if l.strip()]
        stderr_data = "".join(l["data"] for l in lines if l.get("stream") == "stderr")
        assert "err msg" in stderr_data

    def test_missing_binary_returns_127(self):
        from tools.bench_agent import run_command
        import io, contextlib
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            code = run_command(["/nonexistent/binary/that/does/not/exist"])
        assert code == 127

    def test_missing_binary_emits_error_json(self):
        from tools.bench_agent import run_command
        import io, contextlib
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            run_command(["/nonexistent/binary"])
        output = buf.getvalue()
        lines = [json.loads(l) for l in output.splitlines() if l.strip()]
        assert any("error" in l for l in lines)


# ---------------------------------------------------------------------------
# Full round-trip via subprocess
# ---------------------------------------------------------------------------

class TestAgentRoundTrip:
    def test_successful_command_exit_zero(self):
        cmd = {"cmd": [PYTHON, "-c", "print('ok')"]}
        lines, proc_exit = _run_agent(json.dumps(cmd))
        exit_line = next((l for l in lines if "exit" in l), None)
        assert exit_line is not None
        assert exit_line["exit"] == 0
        assert proc_exit == 0

    def test_failed_command_exit_nonzero(self):
        cmd = {"cmd": [PYTHON, "-c", "import sys; sys.exit(3)"]}
        lines, proc_exit = _run_agent(json.dumps(cmd))
        exit_line = next((l for l in lines if "exit" in l), None)
        assert exit_line is not None
        assert exit_line["exit"] == 3
        assert proc_exit == 3

    def test_stdout_in_output(self):
        cmd = {"cmd": [PYTHON, "-c", "print('benchmark result')"]}
        lines, _ = _run_agent(json.dumps(cmd))
        stdout_data = "".join(l["data"] for l in lines if l.get("stream") == "stdout")
        assert "benchmark result" in stdout_data

    def test_stderr_in_output(self):
        cmd = {"cmd": [PYTHON, "-c", "import sys; sys.stderr.write('warn\n')"]}
        lines, _ = _run_agent(json.dumps(cmd))
        stderr_data = "".join(l["data"] for l in lines if l.get("stream") == "stderr")
        assert "warn" in stderr_data

    def test_exit_line_is_last(self):
        cmd = {"cmd": [PYTHON, "-c", "print('data')"]}
        lines, _ = _run_agent(json.dumps(cmd))
        assert lines, "no output lines"
        assert "exit" in lines[-1], f"last line is not exit: {lines[-1]}"

    def test_empty_input_returns_error(self):
        lines, proc_exit = _run_agent("")
        assert any("error" in l for l in lines)
        assert proc_exit != 0

    def test_invalid_json_returns_error(self):
        lines, proc_exit = _run_agent("not json {{")
        assert any("error" in l for l in lines)
        assert proc_exit != 0

    def test_missing_cmd_field_returns_error(self):
        lines, proc_exit = _run_agent(json.dumps({"action": "run"}))
        assert any("error" in l for l in lines)
        assert proc_exit != 0

    def test_cmd_not_list_returns_error(self):
        lines, proc_exit = _run_agent(json.dumps({"cmd": "echo hello"}))
        assert any("error" in l for l in lines)
        assert proc_exit != 0

    def test_multiline_output_all_captured(self):
        cmd = {"cmd": [PYTHON, "-c",
                        "for i in range(5): print(f'line {i}')"]}
        lines, _ = _run_agent(json.dumps(cmd))
        stdout_data = "".join(l["data"] for l in lines if l.get("stream") == "stdout")
        for i in range(5):
            assert f"line {i}" in stdout_data
