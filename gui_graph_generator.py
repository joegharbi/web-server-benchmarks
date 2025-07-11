import os
import tkinter as tk
from tkinter import filedialog, messagebox, ttk
import pandas as pd
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend to avoid display issues
import matplotlib.pyplot as plt
import threading
import json
from datetime import datetime
import numpy as np

class GraphGeneratorApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Advanced Graph Generator")
        self.root.geometry("1200x800")  # Larger window for better usability
        
        # Load configuration
        self.config_file = "gui_config.json"
        self.load_config()
        
        # Initialize variables
        self.file_list = []
        self.selected_columns = []
        self.column_checkboxes = {}
        self.csv_data = {}
        self.base_folder = None
        self.select_all_var = tk.BooleanVar()
        self.progress_var = tk.DoubleVar()
        self.status_var = tk.StringVar(value="Ready")
        
        # Create main notebook for tabbed interface
        self.notebook = ttk.Notebook(root)
        self.notebook.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Create tabs
        self.create_file_selection_tab()
        self.create_column_selection_tab()
        self.create_visualization_tab()
        self.create_export_tab()
        
        # Create bottom status bar
        self.create_status_bar()
        
        # Apply theme
        self.apply_theme()

    def load_config(self):
        """Load saved configuration"""
        self.config = {
            'recent_folders': [],
            'recent_files': [],
            'chart_type': 'auto',
            'color_palette': 'default',
            'chart_size': 'medium',
            'theme': 'light',
            'export_format': 'png',
            'export_dpi': 300,
            'auto_select_common_columns': True,
            'show_grid': True,
            'merge_csv': True,
            'merge_columns': False
        }
        
        try:
            if os.path.exists(self.config_file):
                with open(self.config_file, 'r') as f:
                    saved_config = json.load(f)
                    self.config.update(saved_config)
        except Exception as e:
            print(f"Error loading config: {e}")

    def save_config(self):
        """Save current configuration"""
        try:
            with open(self.config_file, 'w') as f:
                json.dump(self.config, f, indent=2)
        except Exception as e:
            print(f"Error saving config: {e}")

    def create_file_selection_tab(self):
        """Create the file selection tab"""
        file_frame = ttk.Frame(self.notebook)
        self.notebook.add(file_frame, text="📁 File Selection")
        
        # File selection methods
        methods_frame = ttk.LabelFrame(file_frame, text="Select Data Source", padding=10)
        methods_frame.pack(fill=tk.X, padx=10, pady=5)
        
        # Buttons frame
        buttons_frame = ttk.Frame(methods_frame)
        buttons_frame.pack(fill=tk.X, pady=5)
        
        # Open folder button
        self.open_folder_button = ttk.Button(
            buttons_frame, 
            text="📂 Open Folder", 
            command=self.select_folder,
            style='Accent.TButton'
        )
        self.open_folder_button.pack(side=tk.LEFT, padx=(0, 10))
        
        # Open files button
        self.open_files_button = ttk.Button(
            buttons_frame, 
            text="📄 Open Files", 
            command=self.select_files
        )
        self.open_files_button.pack(side=tk.LEFT, padx=(0, 10))
        
        # Drag & drop label
        drop_label = ttk.Label(
            buttons_frame, 
            text="💡 Tip: You can also drag and drop CSV files here",
            foreground='gray'
        )
        drop_label.pack(side=tk.LEFT, padx=(20, 0))
        
        # Recent folders
        if self.config['recent_folders']:
            recent_frame = ttk.LabelFrame(file_frame, text="Recent Folders", padding=10)
            recent_frame.pack(fill=tk.X, padx=10, pady=5)
            
            for folder in self.config['recent_folders'][:5]:  # Show last 5
                folder_button = ttk.Button(
                    recent_frame,
                    text=f"📁 {os.path.basename(folder)}",
                    command=lambda f=folder: self.load_recent_folder(f)
                )
                folder_button.pack(anchor=tk.W, pady=2)
        
        # File information panel
        self.info_frame = ttk.LabelFrame(file_frame, text="File Information", padding=10)
        self.info_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=5)
        
        # File count and status
        self.file_info_label = ttk.Label(self.info_frame, text="No files loaded")
        self.file_info_label.pack(anchor=tk.W)
        
        # File list with scrollbar
        list_frame = ttk.Frame(self.info_frame)
        list_frame.pack(fill=tk.BOTH, expand=True, pady=(10, 0))
        
        self.file_listbox = tk.Listbox(list_frame, height=8)
        file_scrollbar = ttk.Scrollbar(list_frame, orient=tk.VERTICAL, command=self.file_listbox.yview)
        self.file_listbox.configure(yscrollcommand=file_scrollbar.set)
        
        self.file_listbox.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        file_scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        
        # Data preview frame
        self.preview_frame = ttk.LabelFrame(file_frame, text="Data Preview", padding=10)
        self.preview_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=5)
        
        # Preview text widget
        self.preview_text = tk.Text(self.preview_frame, height=6, wrap=tk.NONE)
        preview_scrollbar_y = ttk.Scrollbar(self.preview_frame, orient=tk.VERTICAL, command=self.preview_text.yview)
        preview_scrollbar_x = ttk.Scrollbar(self.preview_frame, orient=tk.HORIZONTAL, command=self.preview_text.xview)
        self.preview_text.configure(yscrollcommand=preview_scrollbar_y.set, xscrollcommand=preview_scrollbar_x.set)
        
        self.preview_text.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        preview_scrollbar_y.pack(side=tk.RIGHT, fill=tk.Y)
        preview_scrollbar_x.pack(side=tk.BOTTOM, fill=tk.X)

    def create_column_selection_tab(self):
        """Create the column selection tab"""
        column_frame = ttk.Frame(self.notebook)
        self.notebook.add(column_frame, text="📊 Column Selection")
        
        # Search and filter frame
        filter_frame = ttk.LabelFrame(column_frame, text="Search & Filter", padding=10)
        filter_frame.pack(fill=tk.X, padx=10, pady=5)
        
        # Search entry
        search_frame = ttk.Frame(filter_frame)
        search_frame.pack(fill=tk.X)
        
        ttk.Label(search_frame, text="Search columns:").pack(side=tk.LEFT)
        self.search_var = tk.StringVar()
        self.search_var.trace('w', self.filter_columns)
        self.search_entry = ttk.Entry(search_frame, textvariable=self.search_var, width=30)
        self.search_entry.pack(side=tk.LEFT, padx=(10, 0))
        
        # Category filter
        ttk.Label(search_frame, text="Category:").pack(side=tk.LEFT, padx=(20, 0))
        self.category_var = tk.StringVar(value="All")
        self.category_combo = ttk.Combobox(
            search_frame, 
            textvariable=self.category_var,
            values=["All", "Performance", "Energy", "Memory", "CPU", "Other"],
            state="readonly",
            width=15
        )
        self.category_combo.pack(side=tk.LEFT, padx=(10, 0))
        self.category_combo.bind('<<ComboboxSelected>>', self.filter_columns)
        
        # Quick selection buttons
        quick_frame = ttk.Frame(filter_frame)
        quick_frame.pack(fill=tk.X, pady=(10, 0))
        
        ttk.Button(quick_frame, text="Select Performance", command=lambda: self.select_category("Performance")).pack(side=tk.LEFT, padx=(0, 5))
        ttk.Button(quick_frame, text="Select Energy", command=lambda: self.select_category("Energy")).pack(side=tk.LEFT, padx=(0, 5))
        ttk.Button(quick_frame, text="Select Memory", command=lambda: self.select_category("Memory")).pack(side=tk.LEFT, padx=(0, 5))
        ttk.Button(quick_frame, text="Clear All", command=self.clear_all_columns).pack(side=tk.LEFT, padx=(20, 0))
        
        # Column selection frame
        selection_frame = ttk.LabelFrame(column_frame, text="Available Columns", padding=10)
        selection_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=5)
        
        # Select all checkbox
        self.select_all_checkbox = ttk.Checkbutton(
            selection_frame, 
            text="Select All", 
            variable=self.select_all_var, 
            command=self.toggle_select_all
        )
        self.select_all_checkbox.pack(anchor=tk.W, pady=(0, 10))
        
        # Column list with scrollbar
        list_frame = ttk.Frame(selection_frame)
        list_frame.pack(fill=tk.BOTH, expand=True)
        
        self.column_canvas = tk.Canvas(list_frame)
        self.column_scrollbar = ttk.Scrollbar(list_frame, orient="vertical", command=self.column_canvas.yview)
        self.scrollable_column_frame = ttk.Frame(self.column_canvas)
        
        self.scrollable_column_frame.bind(
            "<Configure>",
            lambda e: self.column_canvas.configure(scrollregion=self.column_canvas.bbox("all"))
        )
        
        self.column_canvas.create_window((0, 0), window=self.scrollable_column_frame, anchor="nw")
        self.column_canvas.configure(yscrollcommand=self.column_scrollbar.set)
        
        self.column_canvas.pack(side="left", fill="both", expand=True)
        self.column_scrollbar.pack(side="right", fill="y")
        
        # Bind mouse wheel
        self.column_canvas.bind("<Enter>", self._bind_mousewheel)
        self.column_canvas.bind("<Leave>", self._unbind_mousewheel)

    def create_visualization_tab(self):
        """Create the visualization options tab"""
        viz_frame = ttk.Frame(self.notebook)
        self.notebook.add(viz_frame, text="🎨 Visualization")
        
        # Chart type selection
        chart_frame = ttk.LabelFrame(viz_frame, text="Chart Type", padding=10)
        chart_frame.pack(fill=tk.X, padx=10, pady=5)
        
        self.chart_type_var = tk.StringVar(value=self.config['chart_type'])
        
        ttk.Radiobutton(chart_frame, text="Auto (Recommended)", variable=self.chart_type_var, value="auto").pack(anchor=tk.W)
        ttk.Radiobutton(chart_frame, text="Bar Chart", variable=self.chart_type_var, value="bar").pack(anchor=tk.W)
        ttk.Radiobutton(chart_frame, text="Line Plot", variable=self.chart_type_var, value="line").pack(anchor=tk.W)
        
        # Chart customization
        custom_frame = ttk.LabelFrame(viz_frame, text="Chart Customization", padding=10)
        custom_frame.pack(fill=tk.X, padx=10, pady=5)
        
        # Color palette
        color_frame = ttk.Frame(custom_frame)
        color_frame.pack(fill=tk.X, pady=5)
        ttk.Label(color_frame, text="Color Palette:").pack(side=tk.LEFT)
        self.color_palette_var = tk.StringVar(value=self.config['color_palette'])
        color_combo = ttk.Combobox(
            color_frame, 
            textvariable=self.color_palette_var,
            values=["default", "viridis", "plasma", "inferno", "magma", "coolwarm", "rainbow"],
            state="readonly",
            width=15
        )
        color_combo.pack(side=tk.LEFT, padx=(10, 0))
        
        # Chart size
        size_frame = ttk.Frame(custom_frame)
        size_frame.pack(fill=tk.X, pady=5)
        ttk.Label(size_frame, text="Chart Size:").pack(side=tk.LEFT)
        self.chart_size_var = tk.StringVar(value=self.config['chart_size'])
        size_combo = ttk.Combobox(
            size_frame, 
            textvariable=self.chart_size_var,
            values=["small", "medium", "large"],
            state="readonly",
            width=15
        )
        size_combo.pack(side=tk.LEFT, padx=(10, 0))
        
        # Theme
        theme_frame = ttk.Frame(custom_frame)
        theme_frame.pack(fill=tk.X, pady=5)
        ttk.Label(theme_frame, text="Theme:").pack(side=tk.LEFT)
        self.theme_var = tk.StringVar(value=self.config['theme'])
        theme_combo = ttk.Combobox(
            theme_frame, 
            textvariable=self.theme_var,
            values=["light", "dark"],
            state="readonly",
            width=15
        )
        theme_combo.pack(side=tk.LEFT, padx=(10, 0))
        
        # Grid option
        self.show_grid_var = tk.BooleanVar(value=self.config['show_grid'])
        ttk.Checkbutton(custom_frame, text="Show Grid Lines", variable=self.show_grid_var).pack(anchor=tk.W, pady=5)
        
        # Processing options
        process_frame = ttk.LabelFrame(viz_frame, text="Processing Options", padding=10)
        process_frame.pack(fill=tk.X, padx=10, pady=5)
        
        self.merge_csv_var = tk.BooleanVar(value=self.config['merge_csv'])
        ttk.Checkbutton(process_frame, text="Merge CSV files", variable=self.merge_csv_var).pack(anchor=tk.W)
        
        self.merge_columns_var = tk.BooleanVar(value=self.config['merge_columns'])
        ttk.Checkbutton(process_frame, text="Merge columns", variable=self.merge_columns_var).pack(anchor=tk.W)

    def create_export_tab(self):
        """Create the export options tab"""
        export_frame = ttk.Frame(self.notebook)
        self.notebook.add(export_frame, text="💾 Export")
        
        # Export format
        format_frame = ttk.LabelFrame(export_frame, text="Export Format", padding=10)
        format_frame.pack(fill=tk.X, padx=10, pady=5)
        
        self.export_format_var = tk.StringVar(value=self.config['export_format'])
        formats = [("PNG", "png"), ("PDF", "pdf"), ("SVG", "svg"), ("JPG", "jpg")]
        
        for text, value in formats:
            ttk.Radiobutton(format_frame, text=text, variable=self.export_format_var, value=value).pack(anchor=tk.W)
        
        # Quality settings
        quality_frame = ttk.LabelFrame(export_frame, text="Quality Settings", padding=10)
        quality_frame.pack(fill=tk.X, padx=10, pady=5)
        
        # DPI setting
        dpi_frame = ttk.Frame(quality_frame)
        dpi_frame.pack(fill=tk.X, pady=5)
        ttk.Label(dpi_frame, text="DPI:").pack(side=tk.LEFT)
        self.dpi_var = tk.IntVar(value=self.config['export_dpi'])
        dpi_spinbox = ttk.Spinbox(dpi_frame, from_=72, to=600, textvariable=self.dpi_var, width=10)
        dpi_spinbox.pack(side=tk.LEFT, padx=(10, 0))
        
        # Batch export options
        batch_frame = ttk.LabelFrame(export_frame, text="Batch Export", padding=10)
        batch_frame.pack(fill=tk.X, padx=10, pady=5)
        
        self.batch_export_var = tk.BooleanVar(value=True)
        ttk.Checkbutton(batch_frame, text="Export all formats at once", variable=self.batch_export_var).pack(anchor=tk.W)
        
        # Generate button
        button_frame = ttk.Frame(export_frame)
        button_frame.pack(fill=tk.X, padx=10, pady=20)
        
        self.generate_button = ttk.Button(
            button_frame, 
            text="🚀 Generate Graphs", 
            command=self.generate_graphs,
            style='Accent.TButton'
        )
        self.generate_button.pack(pady=10)

    def create_status_bar(self):
        """Create the status bar"""
        status_frame = ttk.Frame(self.root)
        status_frame.pack(fill=tk.X, side=tk.BOTTOM)
        
        # Progress bar
        self.progress_bar = ttk.Progressbar(
            status_frame, 
            variable=self.progress_var, 
            maximum=100
        )
        self.progress_bar.pack(fill=tk.X, padx=10, pady=(5, 0))
        
        # Status label
        self.status_label = ttk.Label(status_frame, textvariable=self.status_var)
        self.status_label.pack(anchor=tk.W, padx=10, pady=(2, 5))

    def apply_theme(self):
        """Apply the selected theme"""
        theme = self.theme_var.get()
        if theme == "dark":
            self.root.configure(bg='#2b2b2b')
            style = ttk.Style()
            style.theme_use('clam')
            style.configure('.', background='#2b2b2b', foreground='white')
        else:
            self.root.configure(bg='#f0f0f0')
            style = ttk.Style()
            style.theme_use('clam')

    def load_recent_folder(self, folder_path):
        """Load a recently used folder"""
        self.base_folder = folder_path
        self.process_folder()
        self.update_recent_folders(folder_path)

    def update_recent_folders(self, folder_path):
        """Update the list of recent folders"""
        if folder_path in self.config['recent_folders']:
            self.config['recent_folders'].remove(folder_path)
        self.config['recent_folders'].insert(0, folder_path)
        self.config['recent_folders'] = self.config['recent_folders'][:10]  # Keep only 10
        self.save_config()

    def select_folder(self):
        """Enhanced folder selection with recent folders support"""
        folder_path = filedialog.askdirectory(title="Select Folder")
        if folder_path:
            self.base_folder = os.path.abspath(folder_path)
            self.update_recent_folders(self.base_folder)
            self.process_folder()
        else:
            return

    def select_files(self):
        """Enhanced file selection with recent files support"""
        file_paths = filedialog.askopenfilenames(
            title="Select CSV Files",
            filetypes=[("CSV Files", "*.csv"), ("All Files", "*.*")]
        )
        if file_paths:
            self.file_list = [os.path.abspath(file) for file in file_paths]
            self.base_folder = os.path.dirname(self.file_list[0])
            self.update_recent_files(file_paths)
            self.load_csv_data()
        else:
            return

    def update_recent_files(self, file_paths):
        """Update the list of recent files"""
        for file_path in file_paths:
            if file_path in self.config['recent_files']:
                self.config['recent_files'].remove(file_path)
            self.config['recent_files'].insert(0, file_path)
        self.config['recent_files'] = self.config['recent_files'][:20]  # Keep only 20
        self.save_config()

    def process_folder(self):
        """Enhanced folder processing with better feedback"""
        self.status_var.set("Scanning folder...")
        self.progress_var.set(0)
        
        self.file_list.clear()
        
        if self.base_folder is not None:
            total_files = 0
            for root, _, files in os.walk(self.base_folder):
                for file in files:
                    if file.endswith(".csv"):
                        total_files += 1
            
            processed = 0
            for root, _, files in os.walk(self.base_folder):
                for file in files:
                    if file.endswith(".csv"):
                        file_path = os.path.abspath(os.path.join(root, file))
                        self.file_list.append(file_path)
                        processed += 1
                        self.progress_var.set((processed / total_files) * 100)
                        self.root.update_idletasks()

        if not self.file_list:
            messagebox.showwarning("Warning", "No valid CSV files found in the selected folder!")
            self.status_var.set("No CSV files found")
        else:
            self.load_csv_data()

    def load_csv_data(self):
        """Enhanced CSV loading with progress feedback"""
        self.status_var.set("Loading CSV files...")
        self.progress_var.set(0)
        
        self.csv_data.clear()
        skipped_files = []
        
        total_files = len(self.file_list)
        for i, file in enumerate(self.file_list):
            try:
                data = pd.read_csv(file)
                self.csv_data[file] = data
                self.progress_var.set(((i + 1) / total_files) * 100)
                self.root.update_idletasks()
            except (pd.errors.ParserError, Exception) as e:
                skipped_files.append(file)
                print(f"Error loading {file}: {e}")

        if skipped_files:
            messagebox.showwarning("Warning", f"Skipped {len(skipped_files)} invalid CSV files")

        self.update_file_info()
        self.update_column_checkboxes()
        self.update_data_preview()
        self.status_var.set(f"Loaded {len(self.csv_data)} CSV files")

    def update_file_info(self):
        """Update file information display"""
        if self.csv_data:
            total_rows = sum(len(data) for data in self.csv_data.values())
            self.file_info_label.config(text=f"Loaded {len(self.csv_data)} CSV files with {total_rows} total rows")
            
            # Update file listbox
            self.file_listbox.delete(0, tk.END)
            for file_path in self.file_list:
                filename = os.path.basename(file_path)
                rows = len(self.csv_data[file_path])
                self.file_listbox.insert(tk.END, f"{filename} ({rows} rows)")
        else:
            self.file_info_label.config(text="No files loaded")
            self.file_listbox.delete(0, tk.END)

    def update_data_preview(self):
        """Update data preview"""
        self.preview_text.delete(1.0, tk.END)
        
        if self.csv_data:
            # Show preview of first file
            first_file = list(self.csv_data.keys())[0]
            data = self.csv_data[first_file]
            
            # Show first 5 rows and first 10 columns
            preview_data = data.head(5).iloc[:, :10]
            self.preview_text.insert(tk.END, f"Preview of {os.path.basename(first_file)}:\n\n")
            self.preview_text.insert(tk.END, preview_data.to_string())

    def categorize_column(self, column_name):
        """Categorize a column based on its name"""
        column_lower = column_name.lower()
        
        if any(word in column_lower for word in ['request', 'throughput', 'latency', 'response']):
            return "Performance"
        elif any(word in column_lower for word in ['energy', 'power', 'joule', 'watt']):
            return "Energy"
        elif any(word in column_lower for word in ['memory', 'mem', 'ram']):
            return "Memory"
        elif any(word in column_lower for word in ['cpu', 'processor']):
            return "CPU"
        else:
            return "Other"

    def filter_columns(self, *args):
        """Filter columns based on search and category"""
        search_term = self.search_var.get().lower()
        category_filter = self.category_var.get()
        
        # Clear existing checkboxes
        for widget in self.scrollable_column_frame.winfo_children():
            widget.destroy()
        
        self.column_checkboxes.clear()
        
        if not self.csv_data:
            return
        
        all_columns = set()
        for data in self.csv_data.values():
            all_columns.update(data.columns)
        
        # Auto-select common columns if enabled
        common_columns = ['Requests/s', 'Total Energy (J)', 'Avg Power (W)', 'Avg CPU (%)', 'Avg Mem (MB)']
        
        for column in sorted(all_columns):
            # Apply filters
            if search_term and search_term not in column.lower():
                continue
                
            category = self.categorize_column(column)
            if category_filter != "All" and category != category_filter:
                continue
            
            # Create checkbox with category color
            var = tk.BooleanVar()
            if self.config['auto_select_common_columns'] and column in common_columns:
                var.set(True)
            
            checkbox = ttk.Checkbutton(
                self.scrollable_column_frame, 
                text=f"{column} ({category})", 
                variable=var
            )
            checkbox.pack(anchor=tk.W)
            self.column_checkboxes[column] = var

    def select_category(self, category):
        """Select all columns in a specific category"""
        for column, var in self.column_checkboxes.items():
            if self.categorize_column(column) == category:
                var.set(True)

    def clear_all_columns(self):
        """Clear all column selections"""
        for var in self.column_checkboxes.values():
            var.set(False)

    def update_column_checkboxes(self):
        """Update column checkboxes with categorization"""
        self.filter_columns()

    def toggle_select_all(self):
        """Toggle select all columns"""
        for var in self.column_checkboxes.values():
            var.set(self.select_all_var.get())

    def get_color_palette(self):
        """Get the selected color palette"""
        palette_name = self.color_palette_var.get()
        
        if palette_name == "default":
            return ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f']
        else:
            # Try to get colormap, fallback to default if not available
            try:
                cmap = plt.get_cmap(palette_name)
                return cmap(np.linspace(0, 1, 8))
            except (ValueError, AttributeError):
                # Fallback to default if colormap not available
                return ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f']

    def get_chart_size(self):
        """Get chart size dimensions"""
        size = self.chart_size_var.get()
        if size == "small":
            return (8, 6)
        elif size == "large":
            return (16, 12)
        else:  # medium
            return (12, 8)

    def generate_graphs(self):
        """Enhanced graph generation with progress feedback"""
        # Validate inputs
        selected_columns = [col for col, var in self.column_checkboxes.items() if var.get()]
        if not selected_columns:
            messagebox.showwarning("Warning", "No columns selected!")
            return
        
        if not self.csv_data:
            messagebox.showwarning("Warning", "No CSV data loaded!")
            return
        
        # Start generation in background thread
        self.generate_button.config(state='disabled')
        self.status_var.set("Generating graphs...")
        self.progress_var.set(0)
        
        thread = threading.Thread(target=self._generate_graphs_thread, args=(selected_columns,))
        thread.daemon = True
        thread.start()

    def _generate_graphs_thread(self, selected_columns):
        """Generate graphs in background thread"""
        try:
            # Save current configuration
            self.config.update({
                'chart_type': self.chart_type_var.get(),
                'color_palette': self.color_palette_var.get(),
                'chart_size': self.chart_size_var.get(),
                'theme': self.theme_var.get(),
                'export_format': self.export_format_var.get(),
                'export_dpi': self.dpi_var.get(),
                'show_grid': self.show_grid_var.get(),
                'merge_csv': self.merge_csv_var.get(),
                'merge_columns': self.merge_columns_var.get()
            })
            self.save_config()
            
            # Apply theme
            self.apply_theme()
            
            # Get color palette and chart size
            colors = self.get_color_palette()
            chart_size = self.get_chart_size()
            
            # Set matplotlib style
            if self.theme_var.get() == "dark":
                plt.style.use('dark_background')
            else:
                plt.style.use('default')
            
            # Generate graphs (this is the existing logic, but with enhanced progress feedback)
            graphs_root = os.path.join(os.getcwd(), "graphs")
            os.makedirs(graphs_root, exist_ok=True)
            
            # Update progress
            self.progress_var.set(10)
            self.status_var.set("Setting up graph generation...")
            
            # Modern, visually appealing color palette
            color_marker_combinations = [(color, '') for color in colors]
            
            def ask_measurement_type():
                import tkinter.simpledialog
                return tkinter.simpledialog.askstring(
                    "Measurement Type Required",
                    "Could not infer measurement type from CSV path. Please enter one of: local, dynamic, static, websocket"
                ) or "other"
            
            # Determine chart type
            chart_type = self.chart_type_var.get()
            
            if self.merge_csv_var.get():
                self.status_var.set("Processing merged CSV files...")
                self.progress_var.set(20)
                
                # Extract metadata from CSVs
                server_names = set()
                types_found = set()
                modes_found = set()
                for data in self.csv_data.values():
                    # Look for specific container/server name columns
                    container_cols = [col for col in data.columns if col.lower() in ['container name', 'server name', 'container_name', 'server_name', 'name']]
                    if container_cols:
                        server_names.add(str(data[container_cols[0]].iloc[0]))
                    if 'Type' in data.columns:
                        types_found.add(str(data['Type'].iloc[0]))
                    if 'mode' in data.columns:
                        modes_found.add(str(data['mode'].iloc[0]))
                
                # Determine merged type
                if len(types_found) == 1:
                    merged_type = types_found.pop()
                else:
                    merged_type = ask_measurement_type()
                
                # Determine mode (for websocket)
                merged_mode = None
                if merged_type == 'websocket' and len(modes_found) == 1:
                    merged_mode = modes_found.pop()
                
                # Build output folder
                merged_folder = os.path.join(graphs_root, merged_type)
                if merged_mode:
                    merged_folder = os.path.join(merged_folder, merged_mode)
                os.makedirs(merged_folder, exist_ok=True)
                
                # Create descriptive filename with plot type
                server_names_sorted = sorted(server_names)
                n_servers = len(server_names_sorted)
                
                # Create short, readable server names
                def create_short_name(server_name):
                    """Create a short, readable name from server name"""
                    name = server_name
                    prefixes = ['ws-', 'st-', 'dy-', 'local-']
                    for prefix in prefixes:
                        if name.startswith(prefix):
                            name = name[len(prefix):]
                            break
                    if '-' in name:
                        name = name.split('-')[0]
                    return name
                
                # Create plot type description from selected columns
                def create_plot_type_name(columns):
                    """Create a short, readable name for the plot type"""
                    if len(columns) == 1:
                        return columns[0].replace(' ', '_').replace('(', '').replace(')', '').replace('%', 'pct').replace('/', '_')
                    elif len(columns) <= 3:
                        short_names = []
                        for col in columns:
                            short = col.replace(' ', '_').replace('(', '').replace(')', '').replace('%', 'pct').replace('/', '_')
                            if len(short) > 15:
                                short = short[:15]
                            short_names.append(short)
                        return "-".join(short_names)
                    else:
                        return f"{len(columns)}metrics"
                
                plot_type = create_plot_type_name(selected_columns)
                
                # Determine chart type for filename
                if chart_type == "auto":
                    chart_type_name = "bar" if merged_type.lower() == 'websocket' else "line"
                elif chart_type == "bar":
                    chart_type_name = "bar"
                elif chart_type == "line":
                    chart_type_name = "line"
                else:
                    chart_type_name = "bar" if merged_type.lower() == 'websocket' else "line"
                
                if n_servers == 0:
                    merged_filename = f"unknown_{merged_type}_{plot_type}_{chart_type_name}.png"
                elif n_servers <= 3:
                    short_names = [create_short_name(name) for name in server_names_sorted]
                    server_part = "-".join(short_names)
                    merged_filename = f"{server_part}_{merged_type}_{plot_type}_{chart_type_name}.png"
                else:
                    first_server = create_short_name(server_names_sorted[0])
                    merged_filename = f"{first_server}+{n_servers-1}more_{merged_type}_{plot_type}_{chart_type_name}.png"
                
                # Determine chart type based on user selection or measurement type
                if chart_type == "auto":
                    use_bar_chart = merged_type.lower() == 'websocket'
                elif chart_type == "bar":
                    use_bar_chart = True
                elif chart_type == "line":
                    use_bar_chart = False
                else:
                    use_bar_chart = merged_type.lower() == 'websocket'
                
                self.status_var.set("Collecting data for plotting...")
                self.progress_var.set(40)
                
                data_columns_lower = {}
                for data in self.csv_data.values():
                    data_columns_lower.update({col.lower(): col for col in data.columns})
                
                # Collect data for plotting
                server_names_list = []
                column_data = {}
                
                for file_idx, (file_path, data) in enumerate(self.csv_data.items()):
                    # Get server name for this file
                    container_cols = [col for col in data.columns if col.lower() in ['container name', 'server name', 'container_name', 'server_name', 'name']]
                    if container_cols:
                        server_name = str(data[container_cols[0]].iloc[0])
                    else:
                        server_name = os.path.basename(os.path.dirname(file_path))
                    
                    server_names_list.append(server_name)
                    
                    # Collect data for each selected column
                    for i, column in enumerate(selected_columns):
                        col_key = column.lower()
                        if col_key in data_columns_lower:
                            actual_col = data_columns_lower[col_key]
                            if actual_col in data.columns:
                                if use_bar_chart:
                                    value = data[actual_col].iloc[0]
                                else:
                                    values = data[actual_col].tolist()
                                    value = values
                                
                                if actual_col not in column_data:
                                    column_data[actual_col] = []
                                column_data[actual_col].append(value)
                
                self.status_var.set("Creating merged graph...")
                self.progress_var.set(60)
                
                # Create subplots for each column
                n_columns = len(column_data)
                if n_columns == 0:
                    plt.figure(figsize=chart_size)
                    plt.text(0.5, 0.5, 'No data to plot', ha='center', va='center', transform=plt.gca().transAxes)
                else:
                    if use_bar_chart:
                        # Create bar chart
                        plt.figure(figsize=chart_size)
                        
                        x_positions = list(range(len(server_names_list)))
                        width = 0.8 / n_columns
                        
                        for idx, (col_name, values) in enumerate(column_data.items()):
                            if len(values) == len(server_names_list):
                                offset = (idx - n_columns/2 + 0.5) * width
                                color_idx = idx % len(color_marker_combinations)
                                selected_color = color_marker_combinations[color_idx][0]
                                
                                bars = plt.bar([x + offset for x in x_positions], values, 
                                             width=width, 
                                             label=col_name,
                                             color=selected_color,
                                             alpha=0.8,
                                             edgecolor='black',
                                             linewidth=0.5)
                                
                                # Add value labels on bars
                                for bar, value in zip(bars, values):
                                    height = bar.get_height()
                                    plt.text(bar.get_x() + bar.get_width()/2., height + height*0.01,
                                           f'{value:.2f}', ha='center', va='bottom', fontsize=9, 
                                           fontweight='bold', color='black')
                        
                        plt.xlabel('Servers', fontsize=12, fontweight='bold')
                        plt.ylabel('Values', fontsize=12, fontweight='bold')
                        plt.title(f"Server Comparison - {merged_type}", fontsize=14, fontweight='bold', pad=20)
                        plt.xticks(x_positions, server_names_list, rotation=45, ha='right', fontsize=10)
                        plt.yticks(fontsize=10)
                        plt.legend(fontsize=10, framealpha=0.9, loc='best')
                        if self.show_grid_var.get():
                            plt.grid(axis='y', alpha=0.3, linestyle='--')
                        plt.tight_layout()
                    else:
                        # Create line plot
                        plt.figure(figsize=chart_size)
                        
                        # Define markers for better distinction
                        markers = ['o', 's', '^', 'D', 'v', '<', '>', 'p', '*', 'h', 'H', '+', 'x', '|', '_']
                        
                        # Create a mapping of server names to consistent colors and markers
                        server_color_map = {}
                        server_marker_map = {}
                        for server_idx, server_name in enumerate(server_names_list):
                            color_idx = server_idx % len(color_marker_combinations)
                            marker_idx = server_idx % len(markers)
                            server_color_map[server_name] = color_marker_combinations[color_idx][0]
                            server_marker_map[server_name] = markers[marker_idx]
                        
                        # Plot each metric with consistent server colors
                        for idx, (col_name, values) in enumerate(column_data.items()):
                            # Plot each server's data as a separate line with unique color and marker
                            for server_idx, server_data in enumerate(values):
                                if isinstance(server_data, list) and len(server_data) > 0:
                                    server_name = server_names_list[server_idx]
                                    selected_color = server_color_map[server_name]
                                    selected_marker = server_marker_map[server_name]
                                    
                                    x_points = list(range(len(server_data)))
                                    plt.plot(x_points, server_data, 
                                           label=f"{server_name} - {col_name}",
                                           color=selected_color,
                                           marker=selected_marker,
                                           markersize=6,
                                           linewidth=2,
                                           alpha=0.8)
                        
                        plt.xlabel('Data Points', fontsize=12, fontweight='bold')
                        plt.ylabel('Values', fontsize=12, fontweight='bold')
                        plt.title(f"Time Series Comparison - {merged_type}", fontsize=14, fontweight='bold', pad=20)
                        plt.xticks(fontsize=10)
                        plt.yticks(fontsize=10)
                        plt.legend(fontsize=10, framealpha=0.9, loc='best')
                        if self.show_grid_var.get():
                            plt.grid(axis='y', alpha=0.3, linestyle='--')
                        plt.tight_layout()
                
                # Save merged graph
                merged_path = os.path.join(merged_folder, merged_filename)
                plt.savefig(merged_path, dpi=self.dpi_var.get(), bbox_inches='tight')
                plt.close()
                
                self.status_var.set("Merged graph saved successfully!")
                self.progress_var.set(80)
                
            else:
                # Individual file processing
                self.status_var.set("Processing individual files...")
                self.progress_var.set(30)
                
                total_files = len(self.csv_data)
                for file_idx, (file_path, data) in enumerate(self.csv_data.items()):
                    self.status_var.set(f"Processing file {file_idx + 1} of {total_files}...")
                    self.progress_var.set(30 + (file_idx / total_files) * 50)
                    
                    # Extract metadata from CSV
                    server_cols = [col for col in data.columns if col.lower() in ['container name', 'server name', 'container_name', 'server_name', 'name']]
                    if server_cols:
                        server_name = str(data[server_cols[0]].iloc[0])
                    else:
                        server_name = os.path.basename(os.path.dirname(file_path))
                    
                    measurement_type = data['type'].iloc[0] if 'type' in data.columns else None
                    mode = data['mode'].iloc[0] if 'mode' in data.columns else None
                    if not measurement_type:
                        measurement_type = ask_measurement_type()
                    
                    # Build output folder
                    file_graphs_folder = os.path.join(graphs_root, measurement_type)
                    if measurement_type == 'websocket' and mode:
                        file_graphs_folder = os.path.join(file_graphs_folder, mode)
                    file_graphs_folder = os.path.join(file_graphs_folder, server_name)
                    os.makedirs(file_graphs_folder, exist_ok=True)
                    
                    # Determine chart type for individual files
                    if chart_type == "auto":
                        use_bar_chart = measurement_type.lower() == 'websocket'
                    elif chart_type == "bar":
                        use_bar_chart = True
                    elif chart_type == "line":
                        use_bar_chart = False
                    else:
                        use_bar_chart = measurement_type.lower() == 'websocket'
                    
                    if self.merge_columns_var.get():
                        # Create merged columns graph
                        plt.figure(figsize=chart_size)
                        data_columns_lower = {col.lower(): col for col in data.columns}
                        columns_to_plot = []
                        values_to_plot = []
                        
                        for i, column in enumerate(selected_columns):
                            col_key = column.lower()
                            if col_key in data_columns_lower:
                                actual_col = data_columns_lower[col_key]
                                if actual_col in data.columns:
                                    columns_to_plot.append(actual_col)
                                    values_to_plot.append(data[actual_col].iloc[0])
                        
                        if columns_to_plot:
                            bars = plt.bar(columns_to_plot, values_to_plot, 
                                         color=[color_marker_combinations[i % len(color_marker_combinations)][0] for i in range(len(values_to_plot))],
                                         alpha=0.8, edgecolor='black', linewidth=0.5)
                            plt.title(f"{server_name} - All Metrics", fontsize=14, fontweight='bold', pad=20)
                            plt.ylabel("Values", fontsize=12, fontweight='bold')
                            plt.xticks(rotation=45, ha='right', fontsize=10)
                            plt.yticks(fontsize=10)
                            
                            # Add value labels on bars
                            for bar, value in zip(bars, values_to_plot):
                                height = bar.get_height()
                                plt.text(bar.get_x() + bar.get_width()/2., height + height*0.01,
                                       f'{value:.2f}', ha='center', va='bottom', fontsize=9, 
                                       fontweight='bold', color='black')
                            
                            if self.show_grid_var.get():
                                plt.grid(axis='y', alpha=0.3, linestyle='--')
                        
                        # Determine chart type name for filename
                        if chart_type == "auto":
                            chart_type_name = "bar" if measurement_type.lower() == 'websocket' else "line"
                        elif chart_type == "bar":
                            chart_type_name = "bar"
                        elif chart_type == "line":
                            chart_type_name = "line"
                        else:
                            chart_type_name = "bar" if measurement_type.lower() == 'websocket' else "line"
                        
                        save_path = os.path.join(file_graphs_folder, f"merged_columns_{chart_type_name}.png")
                        plt.savefig(save_path, dpi=self.dpi_var.get(), bbox_inches='tight')
                        plt.close()
                    else:
                        # Individual column graphs
                        for i, column in enumerate(selected_columns):
                            data_columns_lower = {col.lower(): col for col in data.columns}
                            col_key = column.lower()
                            if col_key in data_columns_lower:
                                actual_col = data_columns_lower[col_key]
                                if actual_col in data.columns:
                                    plt.figure(figsize=chart_size)
                                    value = data[actual_col].iloc[0]
                                    
                                    if use_bar_chart:
                                        bars = plt.bar([actual_col], [value], 
                                                     color=color_marker_combinations[i % len(color_marker_combinations)][0],
                                                     alpha=0.8, edgecolor='black', linewidth=0.5)
                                    else:
                                        plt.plot([actual_col], [value], 
                                               color=color_marker_combinations[i % len(color_marker_combinations)][0],
                                               marker='o', markersize=8, linewidth=2, alpha=0.8)
                                        bars = None
                                    
                                    plt.title(f"{server_name} - {actual_col}", fontsize=14, fontweight='bold', pad=20)
                                    plt.ylabel(actual_col, fontsize=12, fontweight='bold')
                                    plt.xticks(rotation=45, ha='right', fontsize=10)
                                    plt.yticks(fontsize=10)
                                    
                                    # Add value label
                                    if use_bar_chart and bars:
                                        bar = bars[0]
                                        height = bar.get_height()
                                        plt.text(bar.get_x() + bar.get_width()/2., height + height*0.01,
                                               f'{value:.2f}', ha='center', va='bottom', fontsize=10, 
                                               fontweight='bold', color='black')
                                    else:
                                        plt.text(0.5, 0.95, f'{value:.2f}', ha='center', va='top', 
                                               transform=plt.gca().transAxes, fontsize=12, 
                                               fontweight='bold', color='black',
                                               bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.8))
                                    
                                    if self.show_grid_var.get():
                                        plt.grid(axis='y', alpha=0.3, linestyle='--')
                                    
                                    # Save individual graph
                                    safe_column = actual_col.replace(' ', '_').replace('(', '').replace(')', '').replace('%', 'pct').replace('/', '_')
                                    
                                    if chart_type == "auto":
                                        chart_type_name = "bar" if measurement_type.lower() == 'websocket' else "line"
                                    elif chart_type == "bar":
                                        chart_type_name = "bar"
                                    elif chart_type == "line":
                                        chart_type_name = "line"
                                    else:
                                        chart_type_name = "bar" if measurement_type.lower() == 'websocket' else "line"
                                    
                                    save_path = os.path.join(file_graphs_folder, f"{server_name}_{safe_column}_{chart_type_name}.png")
                                    plt.savefig(save_path, dpi=self.dpi_var.get(), bbox_inches='tight')
                                    plt.close()
            
            self.progress_var.set(100)
            self.status_var.set("Graphs generated successfully!")
            messagebox.showinfo("Success", f"Graphs saved in {graphs_root}")
            
        except Exception as e:
            self.status_var.set(f"Error: {str(e)}")
            messagebox.showerror("Error", f"An error occurred: {e}")
        finally:
            self.generate_button.config(state='normal')

    def _bind_mousewheel(self, event):
        """Bind mouse wheel events when the mouse enters the canvas."""
        if os.name == "nt":  # Windows
            self.column_canvas.bind_all("<MouseWheel>", self._on_mousewheel)
        else:  # Linux
            self.column_canvas.bind_all("<Button-4>", self._on_mousewheel_linux)
            self.column_canvas.bind_all("<Button-5>", self._on_mousewheel_linux)

    def _unbind_mousewheel(self, event):
        """Unbind mouse wheel events when the mouse leaves the canvas."""
        if os.name == "nt":  # Windows
            self.column_canvas.unbind_all("<MouseWheel>")
        else:
            self.column_canvas.unbind_all("<Button-4>")
            self.column_canvas.unbind_all("<Button-5>")

    def _on_mousewheel(self, event):
        """Scroll the canvas when the mouse wheel is used (Windows)."""
        self.column_canvas.yview_scroll(-1 * (event.delta // 120), "units")

    def _on_mousewheel_linux(self, event):
        """Scroll the canvas when the mouse wheel is used (Linux)."""
        if event.num == 4:  # Scroll up
            self.column_canvas.yview_scroll(-1, "units")
        elif event.num == 5:  # Scroll down
            self.column_canvas.yview_scroll(1, "units")

if __name__ == "__main__":
    root = tk.Tk()
    app = GraphGeneratorApp(root)
    root.mainloop()
