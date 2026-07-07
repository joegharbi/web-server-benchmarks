# Framework Improvement Plan

A living document. We work one step at a time. Each step has a plain-English
explanation, why it matters, and a status. We update the progress log at the
bottom as we finish things.

Status keys: TODO (not started), DOING (in progress), DONE (finished).

---

## The goal in one sentence

Make each measurement trustworthy by measuring a thing several times and
reporting an average with a give-or-take, then build the smaller helpers that
keep those repeated measurements clean.

## Why we are doing this

Right now the tool measures each thing once. One measurement can be lucky or
unlucky, the same way weighing yourself a single time can be slightly off. If we
measure a few times instead, we learn the typical value and how steady it is.
That is what makes results believable, and it is the main thing the SQAMIA paper
is missing (it reports one run per point).

## Plain-English glossary

- Repeat: run the same measurement a few times (for example five or ten) instead
  of once, so we get several numbers.
- Average: the typical value of those numbers. More trustworthy than any single
  run.
- Give-or-take (confidence interval): how much the runs agree. We write a result
  as "80 joules, give or take 2". Small give-or-take means we are confident, a
  large one means less so.
- Cooldown: a short wait between runs so the laptop cools down and each run
  starts from a similar state.
- Outlier: one repeated run that came out weird, usually because something else
  on the machine interfered. A filter spots it and drops it so it does not pull
  the average off. This only works once we have several runs to compare.
- Environment control: quieting the machine before measuring, for example closing
  other programs and stopping the processor from randomly speeding up. Less noise
  means tighter, steadier numbers.
- Machine profile / config: a small file that records which machine and which
  settings were used, so a run can be repeated the same way later.

## The plan, in order

Each step builds on the one before it. We do not start a step until the previous
one works.

### Step 1 — Repeat each measurement and report average plus give-or-take  [DONE]
Run each measurement several times instead of once, keep every single run, and
write a summary with the average and the give-or-take.
Why first: every later step needs more than one measurement to make sense.

- Part A — the summary tool  [DONE]
  `tools/aggregate_repeats.py` reads a results CSV (one row per run), groups the
  rows that share a configuration, and writes the average and the 95%
  give-or-take for every number. With one run it shows the value and n=1.
  Checked on a 5-run demo: 78, 80, 81, 79, 82 J came out as 80.0 +/- 2.0 (n=5).
- Part B — make the tool repeat by itself  [DONE]
  Add `--repeat N` and `--cooldown S` so one command runs the load several times
  with a rest between, instead of running the tool by hand each time. Each repeat
  is a fully fresh run (its own boot and load), which gives the most honest
  give-or-take. Output: one row per run, then the summary from Part A.
  - HTTP tool (`tools/measure_docker.py`)  [DONE]  flags added, compiles, shown in --help.
  - WebSocket tool (`tools/measure_websocket.py`)  [DONE]  same flags, compiles,
    shown in --help (run under the project venv, which has the websockets module).

### Step 2 — Outlier rejection  [DOING]
On the repeated runs, drop the occasional weird one. Each result is computed per
configuration (per server, per workload type, per load), never across loads.

- Part A — IQR and Hampel  [DONE]
  `tools/aggregate_repeats.py` now reports energy three ways for every group: raw,
  after IQR, and after Hampel, with how many runs each rule dropped. Verified on a
  7-run group with one bad run (120 J among ~80 J): raw 86.1 +/- 13.8, both rules
  dropped 1 and gave 80.5 +/- 1.1. A clean 3-run group was left untouched.
  Guards: if there is no spread (IQR or wobble is zero) nothing is dropped.
- Part B — heavier rules and a selector  [PARKED]
  Documented but not implemented, see `docs/advanced-outlier-methods.md`. The
  heavier rules (Isolation Forest, LOF, DBSCAN, Elliptic) and an automatic selector
  are not needed for our case and would add a scikit-learn dependency. We stick with
  IQR and Hampel and revisit only if the simple rules prove insufficient.

### Step 3 — Environment control  [DONE]
Make the machine quiet and steady before measuring, which shrinks the give-or-take.

- Part A — read-only environment check  [DONE]
  `tools/check_environment.py` reports the machine state without changing anything:
  CPU governor, turbo/boost, swap, current load, other running containers, and time
  sync. Each is marked OK or WARN with a short note. No sudo needed. First run on the
  laptop flagged powersave governor, turbo on, swap on, and NTP on as things to fix.
- Part B — apply performance settings  [DONE]
  `tools/prepare_environment.py apply` (sudo) sets the CPU governor to performance,
  turns turbo off, and stops other running containers, saving the previous state to
  a small file. `... restore` puts all of it back: governor, turbo, and the stopped
  containers. It refuses to run without root and changes nothing in that case. It
  leaves swap and time sync alone (riskier to flip); the check tool flags those.
  Use `--keep name1,name2` to keep some containers running.

### Step 4 — Machine profile and config file  [TODO]
A profiler that records the hardware and software, and a config file that holds the
whole campaign in one place, so a run is reproducible and you do not pass long
command lines. This is where the pieces we built come together.

The config file would hold:
- Which images or containers to measure.
- Which containers to keep alive (passed to `prepare_environment --keep`).
- The workloads and their inputs: HTTP request counts (for example 100, 1000, 5000,
  ... 80000) and WebSocket patterns and client counts (for example burst/stream at
  5, 50, 100).
- How many repeats and the cooldown (default `repeat = 5`, `cooldown = 30` or 60).
- The environment settings (governor, turbo).

A small runner would then, for a campaign: run `check_environment.py`, run
`prepare_environment.py apply`, do every workload and input with the chosen repeats
and cooldown, summarise with `aggregate_repeats.py`, and finally
`prepare_environment.py restore`. The low-level tools stay usable on their own with
`repeat = 1` for quick single runs.

### Step 5 — Graphs with error bars  [TODO]
Update the graph tool so plots show the give-or-take as error bars.

### Small fix — WebSocket burst interval  [DONE]
The burst pattern slept 1 second after every burst (the `--interval` default was
1.0), so 2000 bursts took about 2000 seconds and the server was idle most of the
time. The tool's own help even called burst "as fast as possible", so the default
was effectively a bug. Fixed: `--interval` now defaults to 0, a back-to-back
saturation burst, which matches the intent and matches GMT's burst. The flag is
still there for anyone who wants a paced burst.

The other WebSocket pattern, stream, needed no code fix. It sends at a controlled
rate for a fixed duration, which is correct. The only paper mismatch there was the
duration (20 s for WSEB versus 25 s for GMT), which is just a setting to align in
the config when comparing the two tools, not a bug.

---

## Progress log

- 2026-06-17 — Plan created. Agreed to work step by step. Starting Step 1
  (repeat each measurement and report average plus give-or-take).
- 2026-06-17 — Step 1, Part A done. Added `tools/aggregate_repeats.py`, the tool
  that turns several runs into an average with a 95% give-or-take. Verified on a
  5-run demo (80.0 +/- 2.0, n=5). Next: Part B, the `--repeat`/`--cooldown` flag
  so the tool runs the load several times on its own.
- 2026-06-17 — Step 1, Part B done for the HTTP tool. Added `--repeat` and
  `--cooldown` to `tools/measure_docker.py`. One command now runs the load N times
  (each a fresh run), rests between, and prints the average with a give-or-take.
  Compiles and shows in --help. Next: the same flags for the WebSocket tool.
- 2026-06-17 — Decided defaults. The tool keeps `--repeat 1` (safe and fast, does
  not break existing single-run callers). Changed `--cooldown` default to 30, which
  only matters once you repeat. The "5 repeats" default will live in the
  config/campaign layer (Step 4), not the tool.
- 2026-06-17 — Step 1 DONE. Added `--repeat`/`--cooldown` to the WebSocket tool too
  (`tools/measure_websocket.py`), mirroring the HTTP tool. Both compile and show the
  flags under the venv. Step 1 (repeat plus average plus give-or-take) is complete
  for HTTP and WebSocket. Next up: Step 2 (outlier rejection) when ready.
- 2026-06-17 — Step 2, Part A done. `aggregate_repeats.py` now reports energy raw,
  after IQR, and after Hampel, per configuration, with the dropped count for each.
  Both rules are shown side by side so they can be compared. Verified on a demo
  with one bad run (raw 86.1 +/- 13.8 -> 80.5 +/- 1.1, dropped 1) and on a clean
  group (dropped 0). Part B (heavier rules plus a selector) is left for later.
- 2026-06-17 — Parked Step 2 Part B. Wrote `docs/advanced-outlier-methods.md` with
  plain descriptions of the heavier rules (Isolation Forest, LOF, DBSCAN, Elliptic)
  and the automatic selector. Not implemented, not needed now, IQR and Hampel stay.
- 2026-06-17 — Step 3, Part A done. Added `tools/check_environment.py`, a read-only
  readiness check. First run on the laptop: governor powersave, turbo on, swap on,
  NTP on (all WARN); load OK. Next: Part B, the sudo step to set performance and
  turbo off with save/restore.
- 2026-06-17 — Step 3 DONE. Added `tools/prepare_environment.py` with apply/restore:
  sets governor=performance, turbo off, stops other containers, and restores all of
  it (governor, turbo, containers) from a saved state file. Refuses to run without
  root. Step 3 complete. Expanded Step 4 to spell out the config file contents
  (images to measure, keep-alive list, workloads and inputs, repeats, cooldown,
  environment), which answers "will these go in a config later" -> yes, in Step 4.
- 2026-06-17 — Small fix DONE. Changed the burst `--interval` default from 1.0 to 0
  in `tools/measure_websocket.py`, so a burst is now back-to-back (a real saturation
  burst) instead of 1 burst per second. Stream needed no change; its only mismatch
  with GMT was the duration (20 vs 25 s), a config setting, not a bug.
- 2026-06-29 — Bug fixes in repeat mode (both measure tools). `run_repeats()` now
  returns a real exit code and `main()` exits with it. It returns non-zero when no
  runs complete, when a run fails, or when the summary step (aggregate_repeats.py)
  fails, instead of silently exiting 0. Verified with a mocked-subprocess unit test:
  all-fail, aggregator-fail, and all-ok cases give the right codes for both tools.
  (repeat each measurement and report average plus give-or-take).
