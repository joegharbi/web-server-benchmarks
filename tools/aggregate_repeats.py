#!/usr/bin/env python3
"""Average repeated measurement runs and report a give-or-take, with outlier checks.

Reads a results CSV produced by measure_docker.py or measure_websocket.py, where
the same configuration may appear on several rows (one row per repeat). It groups
the rows by configuration (per server, per workload type, per load) and reports,
for every numeric column, the average and the 95% give-or-take across the repeats.

For energy it also applies two simple outlier rules and reports the average three
ways, so you can see what each rule changed:
  * raw     - nothing dropped
  * IQR     - drop runs outside the middle-half range (the box-plot rule)
  * Hampel  - drop runs far from the median, measured by the typical wobble (MAD)

The give-or-take answers "how steady are the runs". A small give-or-take means
the repeats agree; a large one means they vary. With one run it is 0 and n is 1.
Outlier rules need at least a few runs and stay conservative with very few.

Standard library only. Usage:
  python3 tools/aggregate_repeats.py results_docker/fair-erlang-index.csv
  python3 tools/aggregate_repeats.py in.csv --output summary.csv
"""
import argparse
import csv
import math
import os
import statistics
import sys

# t value for a 95% two-sided interval, by degrees of freedom (number of runs - 1).
# For more runs than listed we fall back to the normal value 1.96.
_T95 = {1: 12.706, 2: 4.303, 3: 3.182, 4: 2.776, 5: 2.571, 6: 2.447, 7: 2.365,
        8: 2.306, 9: 2.262, 10: 2.228, 11: 2.201, 12: 2.179, 13: 2.160,
        14: 2.145, 15: 2.131, 16: 2.120, 17: 2.110, 18: 2.101, 19: 2.093,
        20: 2.086, 21: 2.080, 22: 2.074, 23: 2.069, 24: 2.064, 25: 2.060,
        26: 2.056, 27: 2.052, 28: 2.048, 29: 2.045, 30: 2.042}


def t95(df):
    return _T95.get(df, 1.96) if df > 0 else 0.0


# Columns that name a configuration. Rows sharing these are repeats of one thing.
# Everything else that looks numeric is averaged.
KEY_COLS = ["Container Name", "Type", "Total Requests", "HTTP Max Workers",
            "Pattern", "Num Clients", "Message Size (KB)", "Rate (msg/s)",
            "Bursts", "Duration (s)"]


def is_number(s):
    try:
        float(s)
        return True
    except (TypeError, ValueError):
        return False


def mean_and_giveortake(values):
    """Return (n, mean, give-or-take). Give-or-take is 0 when there is one value."""
    mean = statistics.mean(values)
    if len(values) >= 2:
        sd = statistics.stdev(values)
        ci = t95(len(values) - 1) * sd / math.sqrt(len(values))
    else:
        ci = 0.0
    return len(values), mean, ci


def _quantile(sorted_vals, q):
    """Value at fraction q (0..1) of a sorted list, with linear interpolation."""
    if len(sorted_vals) == 1:
        return sorted_vals[0]
    pos = q * (len(sorted_vals) - 1)
    lo = math.floor(pos)
    hi = math.ceil(pos)
    if lo == hi:
        return sorted_vals[lo]
    return sorted_vals[lo] + (pos - lo) * (sorted_vals[hi] - sorted_vals[lo])


def iqr_keep(values):
    """Return a True/False list: True to keep, False if flagged as an outlier (IQR)."""
    s = sorted(values)
    q1, q3 = _quantile(s, 0.25), _quantile(s, 0.75)
    iqr = q3 - q1
    if iqr == 0:                       # no spread to judge against; keep all
        return [True] * len(values)
    lo, hi = q1 - 1.5 * iqr, q3 + 1.5 * iqr
    return [lo <= v <= hi for v in values]


def hampel_keep(values):
    """Return a True/False list: True to keep, False if flagged as an outlier (Hampel)."""
    med = statistics.median(values)
    scaled_mad = 1.4826 * statistics.median([abs(v - med) for v in values])
    if scaled_mad == 0:               # typical wobble is zero; keep all
        return [True] * len(values)
    return [abs(v - med) <= 3 * scaled_mad for v in values]


def filtered(values, keep):
    return [v for v, k in zip(values, keep) if k]


def main():
    ap = argparse.ArgumentParser(description="Average repeated runs and report a 95% give-or-take.")
    ap.add_argument("input_csv", help="CSV from measure_docker.py or measure_websocket.py")
    ap.add_argument("--output", default=None, help="Summary CSV path (default: <input>_summary.csv)")
    args = ap.parse_args()

    with open(args.input_csv, newline="", encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    if not rows:
        sys.exit("No rows in the input file.")
    headers = list(rows[0].keys())

    key_cols = [c for c in KEY_COLS if c in headers]
    value_cols = [c for c in headers
                  if c not in key_cols and any(is_number(r.get(c, "")) for r in rows)]
    energy_col = next((c for c in value_cols if c.lower().startswith("total energy")), None)

    groups = {}
    for r in rows:
        groups.setdefault(tuple(r.get(c, "") for c in key_cols), []).append(r)

    out = args.output or os.path.splitext(args.input_csv)[0] + "_summary.csv"
    summary_headers = key_cols + ["Repeats"]
    for c in value_cols:
        summary_headers += [f"{c} mean", f"{c} +/-95%"]
    if energy_col:
        summary_headers += ["Energy IQR mean", "Energy IQR +/-95%", "Energy IQR dropped",
                            "Energy Hampel mean", "Energy Hampel +/-95%", "Energy Hampel dropped"]

    with open(out, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(summary_headers)
        for key, grp in groups.items():
            row = list(key) + [len(grp)]
            for c in value_cols:
                vals = [float(r[c]) for r in grp if is_number(r.get(c, ""))]
                if vals:
                    _, mean, ci = mean_and_giveortake(vals)
                    row += [round(mean, 4), round(ci, 4)]
                else:
                    row += ["", ""]
            if energy_col:
                ev = [float(r[energy_col]) for r in grp if is_number(r.get(energy_col, ""))]
                for keep_fn in (iqr_keep, hampel_keep):
                    if len(ev) >= 2:
                        kept = filtered(ev, keep_fn(ev))
                        _, m, ci = mean_and_giveortake(kept)
                        row += [round(m, 4), round(ci, 4), len(ev) - len(kept)]
                    else:
                        row += ["", "", 0]
            w.writerow(row)

    print(f"Wrote {out}  ({len(groups)} configuration(s))\n")
    label_cols = [c for c in ("Container Name", "Total Requests", "Pattern", "Num Clients")
                  if c in key_cols]
    if energy_col:
        print("Energy (J), average +/- give-or-take, with the two outlier rules:")
        for key, grp in groups.items():
            d = dict(zip(key_cols, key))
            ev = [float(r[energy_col]) for r in grp if is_number(r.get(energy_col, ""))]
            if not ev:
                continue
            label = " ".join(str(d.get(c, "")) for c in label_cols)
            n, m, ci = mean_and_giveortake(ev)
            line = f"  {label:<30} raw {m:8.3f} +/- {ci:6.3f} (n={n})"
            if n >= 2:
                for name, keep_fn in (("IQR", iqr_keep), ("Hampel", hampel_keep)):
                    kept = filtered(ev, keep_fn(ev))
                    _, mf, cif = mean_and_giveortake(kept)
                    dropped = n - len(kept)
                    line += f" | {name} {mf:8.3f} +/- {cif:6.3f} (dropped {dropped})"
            else:
                line += "   (only 1 run)"
            print(line)


if __name__ == "__main__":
    main()
