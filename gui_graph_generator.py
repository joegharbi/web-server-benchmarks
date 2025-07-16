import os
import csv
import tkinter as tk
from tkinter import filedialog, messagebox, ttk
from collections import defaultdict
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

# --- Constants for CSV Types ---
WEBSOCKET_HEADERS = [
    "Container Name", "Test Type", "Num CPUs", "Total Messages", "Successful Messages", "Failed Messages", "Execution Time (s)", "Messages/s", "Throughput (MB/s)",
    "Avg Latency (ms)", "Min Latency (ms)", "Max Latency (ms)", "Total Energy (J)", "Avg Power (W)", "Samples", "Avg CPU (%)", "Peak CPU (%)", "Total CPU (%)",
    "Avg Mem (MB)", "Peak Mem (MB)", "Total Mem (MB)", "Pattern", "Num Clients", "Message Size (KB)", "Rate (msg/s)", "Bursts", "Interval (s)", "Duration (s)"
]
HTTP_HEADERS = [
    "Container Name", "Type", "Num CPUs", "Total Requests", "Successful Requests", "Failed Requests", "Execution Time (s)", "Requests/s", "Total Energy (J)", "Avg Power (W)", "Samples", "Avg CPU (%)", "Peak CPU (%)", "Total CPU (%)", "Avg Mem (MB)", "Peak Mem (MB)", "Total Mem (MB)"
]

# --- Helper Functions ---
def detect_csv_type(header):
    if "Test Type" in header or "Total Messages" in header:
        return "websocket"
    if "Type" in header and "Total Requests" in header:
        return "http"
    return "unknown"

def read_csv(filepath):
    with open(filepath, newline='') as f:
        reader = csv.DictReader(f)
        header = reader.fieldnames
        rows = list(reader)
    return header, rows

def summarize_column(rows, col):
    vals = [float(r[col]) for r in rows if r.get(col) not in (None, '', 'NaN')]
    if not vals:
        return {'min': '-', 'max': '-', 'avg': '-'}
    return {
        'min': min(vals),
        'max': max(vals),
        'avg': sum(vals)/len(vals)
    }

def get_numeric_columns(header):
    # Heuristic: columns with numbers in sample data
    numeric = []
    for h in header:
        if any(x in h.lower() for x in ["cpu", "mem", "latency", "throughput", "energy", "power", "requests", "messages", "samples", "rate", "size", "duration", "interval", "bursts"]):
            numeric.append(h)
    return numeric

# --- Main GUI Class ---
class BenchmarkGrapher(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Web Server Benchmark Graph Generator")
        self.geometry("1100x700")
        self.configure(bg="#f7f7f7")
        self.files = []
        self.file_types = {}
        self.headers = {}
        self.rows = {}
        self.selected_metric = tk.StringVar()
        self.selected_files = []
        self.init_ui()

    def init_ui(self):
        # --- File Selection ---
        file_frame = tk.Frame(self, bg="#f7f7f7")
        file_frame.pack(fill=tk.X, padx=10, pady=5)
        tk.Label(file_frame, text="Select CSV files:", bg="#f7f7f7", font=("Arial", 12, "bold")).pack(side=tk.LEFT)
        tk.Button(file_frame, text="Browse...", command=self.browse_files, font=("Arial", 11)).pack(side=tk.LEFT, padx=5)
        tk.Button(file_frame, text="Clear", command=self.clear_files, font=("Arial", 11)).pack(side=tk.LEFT, padx=5)
        self.file_listbox = tk.Listbox(file_frame, selectmode=tk.MULTIPLE, width=80, height=3, font=("Arial", 10))
        self.file_listbox.pack(side=tk.LEFT, padx=10)
        self.file_listbox.bind('<Double-1>', lambda e: self.plot_selected())
        # Drag-and-drop support can be added with tkinterDnD2 if desired in the future.

        # --- Metric Selection ---
        metric_frame = tk.Frame(self, bg="#f7f7f7")
        metric_frame.pack(fill=tk.X, padx=10, pady=5)
        tk.Label(metric_frame, text="Metric to plot:", bg="#f7f7f7", font=("Arial", 12, "bold")).pack(side=tk.LEFT)
        self.metric_combo = ttk.Combobox(metric_frame, textvariable=self.selected_metric, state="readonly", width=40, font=("Arial", 11))
        self.metric_combo.pack(side=tk.LEFT, padx=5)
        self.metric_combo.bind('<<ComboboxSelected>>', lambda e: self.plot_selected())
        tk.Button(metric_frame, text="Plot", command=self.plot_selected, font=("Arial", 11, "bold")).pack(side=tk.LEFT, padx=5)
        tk.Button(metric_frame, text="Export Graph as PNG", command=self.export_graph, font=("Arial", 11)).pack(side=tk.LEFT, padx=5)

        # --- Graph Area ---
        self.fig, self.ax = plt.subplots(figsize=(8, 5))
        self.canvas = FigureCanvasTkAgg(self.fig, master=self)
        self.canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True, padx=10, pady=10)

        # --- Summary Area ---
        summary_frame = tk.Frame(self, bg="#f7f7f7")
        summary_frame.pack(fill=tk.X, padx=10, pady=5)
        tk.Label(summary_frame, text="Summary:", bg="#f7f7f7", font=("Arial", 12, "bold")).pack(side=tk.LEFT)
        self.summary_text = tk.Text(summary_frame, height=4, width=120, font=("Arial", 10))
        self.summary_text.pack(side=tk.LEFT, padx=10)
        self.summary_text.config(state=tk.DISABLED)

        # --- Help Button ---
        tk.Button(self, text="Help", command=self.show_help, font=("Arial", 11)).pack(side=tk.RIGHT, padx=10, pady=5)

    def browse_files(self):
        files = filedialog.askopenfilenames(title="Select CSV files", filetypes=[("CSV Files", "*.csv")])
        if files:
            self.add_files(files)

    # Drag-and-drop support removed for compatibility with standard Tkinter.
    # def drop_files(self, event):
    #     files = self.tk.splitlist(event.data)
    #     self.add_files(files)

    def add_files(self, files):
        for f in files:
            if f not in self.files:
                try:
                    header, rows = read_csv(f)
                except Exception as e:
                    messagebox.showerror("Error", f"Failed to read {f}: {e}")
                    continue
                typ = detect_csv_type(header)
                self.files.append(f)
                self.file_types[f] = typ
                self.headers[f] = header
                self.rows[f] = rows
                self.file_listbox.insert(tk.END, os.path.basename(f) + f"  [{typ}]")
        self.update_metric_options()

    def clear_files(self):
        self.files.clear()
        self.file_types.clear()
        self.headers.clear()
        self.rows.clear()
        self.file_listbox.delete(0, tk.END)
        self.metric_combo.set("")
        self.ax.clear()
        self.canvas.draw()
        self.summary_text.config(state=tk.NORMAL)
        self.summary_text.delete(1.0, tk.END)
        self.summary_text.config(state=tk.DISABLED)

    def update_metric_options(self):
        # Show only metrics common to all selected files
        if not self.files:
            self.metric_combo['values'] = []
            return
        metrics = set(get_numeric_columns(self.headers[self.files[0]]))
        for f in self.files[1:]:
            metrics &= set(get_numeric_columns(self.headers[f]))
        metrics = sorted(metrics)
        self.metric_combo['values'] = metrics
        if metrics:
            self.metric_combo.set(metrics[0])
        else:
            self.metric_combo.set("")

    def plot_selected(self):
        selected_indices = self.file_listbox.curselection()
        if not selected_indices:
            selected_indices = range(len(self.files))  # Default: all
        selected_files = [self.files[i] for i in selected_indices]
        metric = self.selected_metric.get()
        if not metric:
            messagebox.showwarning("No metric selected", "Please select a metric to plot.")
            return
        self.ax.clear()
        summary_lines = []
        for f in selected_files:
            header = self.headers[f]
            rows = self.rows[f]
            typ = self.file_types[f]
            x, y, label = self.get_plot_data(header, rows, typ, metric, os.path.basename(f))
            if x and y:
                self.ax.plot(x, y, marker='o', label=label)
                stats = summarize_column(rows, metric)
                summary_lines.append(f"{label}: min={stats['min']}, max={stats['max']}, avg={stats['avg']}")
        self.ax.set_title(f"{metric} vs. Test Parameter")
        self.ax.set_xlabel("Test Parameter (Requests, Clients, etc.)")
        self.ax.set_ylabel(metric)
        self.ax.legend()
        self.ax.grid(True)
        self.canvas.draw()
        self.summary_text.config(state=tk.NORMAL)
        self.summary_text.delete(1.0, tk.END)
        self.summary_text.insert(tk.END, "\n".join(summary_lines))
        self.summary_text.config(state=tk.DISABLED)

    def get_plot_data(self, header, rows, typ, metric, label):
        # Heuristics for x-axis:
        if typ == "websocket":
            # Prefer Num Clients, Message Size, Rate, Bursts, Duration, etc.
            for xkey in ["Num Clients", "Message Size (KB)", "Rate (msg/s)", "Bursts", "Duration (s)", "Interval (s)"]:
                if xkey in header:
                    x = [float(r[xkey]) if r.get(xkey) not in (None, '', 'NaN') else 0 for r in rows]
                    y = [float(r[metric]) if r.get(metric) not in (None, '', 'NaN') else 0 for r in rows]
                    return x, y, label
        else:
            # HTTP: Prefer Total Requests
            if "Total Requests" in header:
                x = [float(r["Total Requests"]) if r.get("Total Requests") not in (None, '', 'NaN') else 0 for r in rows]
                y = [float(r[metric]) if r.get(metric) not in (None, '', 'NaN') else 0 for r in rows]
                return x, y, label
        # Fallback: row index
        x = list(range(1, len(rows)+1))
        y = [float(r[metric]) if r.get(metric) not in (None, '', 'NaN') else 0 for r in rows]
        return x, y, label

    def export_graph(self):
        if not self.ax.has_data():
            messagebox.showwarning("No graph", "No graph to export. Please plot something first.")
            return
        file = filedialog.asksaveasfilename(defaultextension=".png", filetypes=[("PNG Image", "*.png")])
        if file:
            self.fig.savefig(file)
            messagebox.showinfo("Exported", f"Graph exported to {file}")

    def show_help(self):
        msg = (
            "Web Server Benchmark Graph Generator\n\n"
            "- Select one or more CSV files (from results directories).\n"
            "- The tool auto-detects file type (HTTP, WebSocket, etc).\n"
            "- Choose a metric to plot (latency, throughput, CPU, etc).\n"
            "- Overlay/combine results from multiple files.\n"
            "- Export graphs as PNG.\n"
            "- Summary stats (min, max, avg) shown below the graph.\n"
            "- Drag-and-drop files into the list.\n"
            "- Double-click a file to plot.\n"
            "- Handles all result types (static, dynamic, local, websocket).\n"
            "- If a metric is missing in a file, it is skipped.\n"
        )
        messagebox.showinfo("Help", msg)

if __name__ == "__main__":
    app = BenchmarkGrapher()
    app.mainloop()
