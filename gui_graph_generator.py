import os
import tkinter as tk
from tkinter import filedialog, messagebox
import pandas as pd
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend to avoid display issues
import matplotlib.pyplot as plt
import tkinter.ttk as ttk  # Import ttk for the scrollbar widget

class GraphGeneratorApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Graph Generator")
        self.root.geometry("1000x700")  # Enlarged GUI for better usability

        # File selection
        self.file_list = []
        self.selected_columns = []
        self.column_checkboxes = {}
        self.csv_data = {}
        self.base_folder = None
        self.select_all_var = tk.BooleanVar()

        # Separate buttons for folder and file selection
        self.open_folder_button = tk.Button(root, text="Open Folder", command=self.select_folder, font=("Arial", 12))
        self.open_folder_button.pack(pady=5)

        self.open_files_button = tk.Button(root, text="Open Files", command=self.select_files, font=("Arial", 12))
        self.open_files_button.pack(pady=5)

        # Chart type selection frame
        self.chart_type_frame = tk.Frame(root)
        self.chart_type_frame.pack(pady=5)
        
        tk.Label(self.chart_type_frame, text="Chart Type:", font=("Arial", 10, "bold")).pack(side=tk.LEFT, padx=(0, 10))
        
        self.chart_type_var = tk.StringVar(value="auto")
        self.auto_radio = tk.Radiobutton(self.chart_type_frame, text="Auto (Recommended)", 
                                       variable=self.chart_type_var, value="auto", font=("Arial", 10))
        self.auto_radio.pack(side=tk.LEFT, padx=(0, 10))
        
        self.bar_radio = tk.Radiobutton(self.chart_type_frame, text="Bar Chart", 
                                      variable=self.chart_type_var, value="bar", font=("Arial", 10))
        self.bar_radio.pack(side=tk.LEFT, padx=(0, 10))
        
        self.line_radio = tk.Radiobutton(self.chart_type_frame, text="Line Plot", 
                                       variable=self.chart_type_var, value="line", font=("Arial", 10))
        self.line_radio.pack(side=tk.LEFT)

        # Column selection with scrollable frame
        self.column_frame_container = tk.Frame(root)
        self.column_frame_container.pack(fill=tk.BOTH, expand=True, pady=10)

        self.canvas = tk.Canvas(self.column_frame_container)
        self.scrollbar = ttk.Scrollbar(self.column_frame_container, orient="vertical", command=self.canvas.yview)
        self.scrollable_frame = tk.Frame(self.canvas)

        self.scrollable_frame.bind(
            "<Configure>",
            lambda e: self.canvas.configure(scrollregion=self.canvas.bbox("all"))
        )

        self.canvas.create_window((0, 0), window=self.scrollable_frame, anchor="nw")
        self.canvas.configure(yscrollcommand=self.scrollbar.set)

        self.canvas.pack(side="left", fill="both", expand=True)
        self.scrollbar.pack(side="right", fill="y")

        # Bind mouse wheel scrolling to the canvas for Windows and Linux
        self.canvas.bind("<Enter>", self._bind_mousewheel)
        self.canvas.bind("<Leave>", self._unbind_mousewheel)

        self.select_all_var = tk.BooleanVar()
        self.select_all_checkbox = tk.Checkbutton(self.scrollable_frame, text="Select All", variable=self.select_all_var, command=self.toggle_select_all, font=("Arial", 10))
        self.select_all_checkbox.pack(anchor="w")

        # Options frame
        self.options_frame = tk.Frame(root)
        self.options_frame.pack(pady=5)

        # Merge CSV files option
        self.merge_csv_var = tk.BooleanVar(value=True)
        self.merge_csv_checkbox = tk.Checkbutton(self.options_frame, text="Merge CSV files", 
                                               variable=self.merge_csv_var, font=("Arial", 10))
        self.merge_csv_checkbox.pack(side=tk.LEFT, padx=(0, 20))

        # Merge columns option
        self.merge_columns_var = tk.BooleanVar(value=False)
        self.merge_columns_checkbox = tk.Checkbutton(self.options_frame, text="Merge columns", 
                                                   variable=self.merge_columns_var, font=("Arial", 10))
        self.merge_columns_checkbox.pack(side=tk.LEFT)

        # Generate button
        self.generate_button = tk.Button(root, text="Generate Graphs", command=self.generate_graphs, 
                                       font=("Arial", 12, "bold"), bg="#4CAF50", fg="white", 
                                       relief=tk.RAISED, bd=3)
        self.generate_button.pack(pady=10)

        # Status label
        self.status_label = tk.Label(root, text="Ready", font=("Arial", 10), fg="gray")
        self.status_label.pack(pady=5)

    def select_folder(self):
        # Allow the user to select a folder
        folder_path = filedialog.askdirectory(title="Select Folder")
        if folder_path:
            self.base_folder = os.path.abspath(folder_path)
            self.file_list.clear()
            # Add all CSV files from the selected folder and its subfolders
            for root, _, files in os.walk(self.base_folder):
                for file in files:
                    if file.endswith(".csv"):
                        file_path = os.path.abspath(os.path.join(root, file))
                        self.file_list.append(file_path)

            if not self.file_list:
                messagebox.showinfo("No CSV Files Found", "No valid CSV files found in the selected folder.")
            else:
                self.load_csv_data()
        else:
            # Gracefully handle cancel action without showing additional dialogs
            return

    def select_files(self):
        # Allow the user to select multiple CSV files
        file_paths = filedialog.askopenfilenames(
            title="Select CSV Files",
            filetypes=[("CSV Files", "*.csv")],
        )
        if file_paths:
            self.file_list = [os.path.abspath(file) for file in file_paths]
            self.base_folder = os.path.dirname(self.file_list[0])  # Set base folder to the directory of the first file
            self.load_csv_data()
        else:
            # Gracefully handle cancel action without showing additional dialogs
            return

    def process_folder(self):
        # Clear the file list to avoid duplicates
        self.file_list.clear()

        # Process CSV files in the selected folder
        if self.base_folder is not None:
            for root, _, files in os.walk(self.base_folder):
                for file in files:
                    if file.endswith(".csv"):
                        file_path = os.path.abspath(os.path.join(root, file))
                        self.file_list.append(file_path)

        if not self.file_list:
            messagebox.showwarning("Warning", "No valid CSV files found in the selected folder!")
        else:
            self.load_csv_data()

    def load_csv_data(self):
        # Clear previous data
        self.csv_data.clear()
        skipped_files = []

        for file in self.file_list:
            try:
                data = pd.read_csv(file)
                self.csv_data[file] = data
            except (pd.errors.ParserError, Exception):
                skipped_files.append(file)

        if skipped_files:
            print(f"Skipped invalid CSV files: {', '.join(skipped_files)}")

        # Update column checkboxes
        self.update_column_checkboxes()

    def update_column_checkboxes(self):
        for widget in self.scrollable_frame.winfo_children():
            widget.destroy()

        self.select_all_checkbox = tk.Checkbutton(self.scrollable_frame, text="Select All", variable=self.select_all_var, command=self.toggle_select_all, font=("Arial", 10))
        self.select_all_checkbox.pack(anchor="w")

        self.column_checkboxes.clear()
        all_columns = set()
        for data in self.csv_data.values():
            all_columns.update(data.columns)

        for column in sorted(all_columns):
            var = tk.BooleanVar()
            checkbox = tk.Checkbutton(self.scrollable_frame, text=column, variable=var, font=("Arial", 10))
            checkbox.pack(anchor="w")
            self.column_checkboxes[column] = var

    def toggle_select_all(self):
        for var in self.column_checkboxes.values():
            var.set(self.select_all_var.get())

    def generate_graphs(self):
        try:
            selected_columns = [col for col, var in self.column_checkboxes.items() if var.get()]
            if not selected_columns:
                messagebox.showwarning("Warning", "No columns selected!")
                return

            graphs_root = os.path.join(os.getcwd(), "graphs")
            os.makedirs(graphs_root, exist_ok=True)

            # Modern, visually appealing color palette
            colors = [
                '#1f77b4',  # Blue
                '#ff7f0e',  # Orange
                '#2ca02c',  # Green
                '#d62728',  # Red
                '#9467bd',  # Purple
                '#8c564b',  # Brown
                '#e377c2',  # Pink
                '#7f7f7f',  # Gray
                '#bcbd22',  # Olive
                '#17becf',  # Cyan
                '#ff9896',  # Light Red
                '#98df8a',  # Light Green
                '#ffbb78',  # Light Orange
                '#c5b0d5',  # Light Purple
                '#c49c94',  # Light Brown
            ]
            
            # Use only colors, no markers for cleaner bar charts
            color_marker_combinations = [(color, '') for color in colors]

            def ask_measurement_type():
                import tkinter.simpledialog
                return tkinter.simpledialog.askstring(
                    "Measurement Type Required",
                    "Could not infer measurement type from CSV path. Please enter one of: local, dynamic, static, websocket"
                ) or "other"

            if self.merge_csv_var.get():
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
                    # Remove common prefixes
                    name = server_name
                    prefixes = ['ws-', 'st-', 'dy-', 'local-']
                    for prefix in prefixes:
                        if name.startswith(prefix):
                            name = name[len(prefix):]
                            break
                    
                    # Take first part if it contains hyphens
                    if '-' in name:
                        name = name.split('-')[0]
                    
                    return name
                
                # Create plot type description from selected columns
                def create_plot_type_name(columns):
                    """Create a short, readable name for the plot type"""
                    if len(columns) == 1:
                        return columns[0].replace(' ', '_').replace('(', '').replace(')', '').replace('%', 'pct').replace('/', '_')
                    elif len(columns) <= 3:
                        # For 2-3 columns, use abbreviated names
                        short_names = []
                        for col in columns:
                            # Create short version of column name
                            short = col.replace(' ', '_').replace('(', '').replace(')', '').replace('%', 'pct').replace('/', '_')
                            if len(short) > 15:  # Truncate long names
                                short = short[:15]
                            short_names.append(short)
                        return "-".join(short_names)
                    else:
                        # For many columns, use count
                        return f"{len(columns)}metrics"
                
                plot_type = create_plot_type_name(selected_columns)
                
                # Debug information
                print(f"Debug - server_names: {server_names}")
                print(f"Debug - server_names_sorted: {server_names_sorted}")
                print(f"Debug - n_servers: {n_servers}")
                print(f"Debug - merged_type: {merged_type}")
                print(f"Debug - plot_type: {plot_type}")
                
                # Determine chart type for filename
                chart_type = self.chart_type_var.get()
                if chart_type == "auto":
                    # Auto mode: bar charts for websocket, line plots for others
                    chart_type_name = "bar" if merged_type.lower() == 'websocket' else "line"
                elif chart_type == "bar":
                    chart_type_name = "bar"
                elif chart_type == "line":
                    chart_type_name = "line"
                else:
                    chart_type_name = "bar" if merged_type.lower() == 'websocket' else "line"  # fallback
                
                if n_servers == 0:
                    # Fallback naming when no server names found
                    merged_filename = f"unknown_{merged_type}_{plot_type}_{chart_type_name}.png"
                elif n_servers <= 3:
                    # For 3 or fewer servers, list them all
                    short_names = [create_short_name(name) for name in server_names_sorted]
                    server_part = "-".join(short_names)
                    merged_filename = f"{server_part}_{merged_type}_{plot_type}_{chart_type_name}.png"
                else:
                    # For more than 3 servers, use count and first server
                    first_server = create_short_name(server_names_sorted[0])
                    merged_filename = f"{first_server}+{n_servers-1}more_{merged_type}_{plot_type}_{chart_type_name}.png"
                
                # Determine chart type based on user selection or measurement type
                chart_type = self.chart_type_var.get()
                if chart_type == "auto":
                    # Auto mode: bar charts for websocket, line plots for others
                    use_bar_chart = merged_type.lower() == 'websocket'
                elif chart_type == "bar":
                    use_bar_chart = True
                elif chart_type == "line":
                    use_bar_chart = False
                else:
                    use_bar_chart = merged_type.lower() == 'websocket'  # fallback
                
                print(f"Debug - Chart type: {'Bar Chart' if use_bar_chart else 'Line Plot'} for {merged_type} (user selection: {chart_type})")
                
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
                        print(f"Debug - Looking for column '{column}' (key: '{col_key}')")
                        print(f"Debug - Available columns: {list(data_columns_lower.keys())}")
                        
                        if col_key in data_columns_lower:
                            actual_col = data_columns_lower[col_key]
                            print(f"Debug - Found actual column: '{actual_col}'")
                            
                            if actual_col in data.columns:  # Make sure column exists in this specific file
                                if use_bar_chart:
                                    # For bar charts: use first row (single value per server)
                                    value = data[actual_col].iloc[0]
                                    print(f"Debug - Column '{actual_col}' exists in data, value: {value}")
                                else:
                                    # For line plots: use all rows (time series)
                                    values = data[actual_col].tolist()
                                    print(f"Debug - Column '{actual_col}' exists in data, values: {values}")
                                    value = values  # Store all values for line plot
                                
                                if actual_col not in column_data:
                                    column_data[actual_col] = []
                                column_data[actual_col].append(value)
                            else:
                                print(f"Debug - Column '{actual_col}' NOT found in data.columns: {list(data.columns)}")
                        else:
                            print(f"Debug - Column key '{col_key}' NOT found in data_columns_lower")
                
                # Create subplots for each column
                print(f"Debug - column_data: {column_data}")
                print(f"Debug - server_names_list: {server_names_list}")
                
                n_columns = len(column_data)
                if n_columns == 0:
                    plt.figure(figsize=(8, 6))
                    plt.text(0.5, 0.5, 'No data to plot', ha='center', va='center', transform=plt.gca().transAxes)
                else:
                    if use_bar_chart:
                        # Create bar chart for websocket data
                        plt.figure(figsize=(12, 8))
                        
                        # Plot all data in one chart with different colors
                        x_positions = list(range(len(server_names_list)))
                        width = 0.8 / n_columns  # Adjust bar width based on number of columns
                        
                        # Use original order for color assignment within this graph
                        for idx, (col_name, values) in enumerate(column_data.items()):
                            if len(values) == len(server_names_list):  # Make sure we have matching data
                                offset = (idx - n_columns/2 + 0.5) * width
                                color_idx = idx % len(color_marker_combinations)
                                selected_color = color_marker_combinations[color_idx][0]
                                print(f"Debug - Metric '{col_name}' gets color index {color_idx}: {selected_color}")
                                
                                bars = plt.bar([x + offset for x in x_positions], values, 
                                             width=width, 
                                             label=col_name,
                                             color=selected_color,
                                             alpha=0.8,  # Slight transparency
                                             edgecolor='black',  # Black edges
                                             linewidth=0.5)  # Thin edge lines
                                
                                # Add value labels on bars with better styling
                                for bar, value in zip(bars, values):
                                    height = bar.get_height()
                                    print(f"Debug - Adding label for {col_name}: {value} (formatted: {value:.2f})")
                                    plt.text(bar.get_x() + bar.get_width()/2., height + height*0.01,
                                           f'{value:.2f}', ha='center', va='bottom', fontsize=9, 
                                           fontweight='bold', color='black')
                        
                        plt.xlabel('Servers', fontsize=12, fontweight='bold')
                        plt.ylabel('Values', fontsize=12, fontweight='bold')
                        plt.title(f"Server Comparison - {merged_type}", fontsize=14, fontweight='bold', pad=20)
                        plt.xticks(x_positions, server_names_list, rotation=45, ha='right', fontsize=10)
                        plt.yticks(fontsize=10)
                        plt.legend(fontsize=10, framealpha=0.9, loc='best')
                        plt.grid(axis='y', alpha=0.3, linestyle='--')  # Subtle grid
                        plt.tight_layout()
                    else:
                        # Create line plot for time-series data (static, dynamic, local)
                        plt.figure(figsize=(12, 8))
                        
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
                            print(f"Debug - Server {server_name} assigned color: {server_color_map[server_name]}, marker: {server_marker_map[server_name]}")
                        
                        # Plot each metric with consistent server colors
                        for idx, (col_name, values) in enumerate(column_data.items()):
                            print(f"Debug - Processing metric '{col_name}' with {len(values)} server values")
                            
                            # Plot each server's data as a separate line with unique color and marker
                            for server_idx, server_data in enumerate(values):
                                if isinstance(server_data, list) and len(server_data) > 0:
                                    server_name = server_names_list[server_idx]
                                    selected_color = server_color_map[server_name]
                                    selected_marker = server_marker_map[server_name]
                                    
                                    print(f"Debug - Server {server_name} using color: {selected_color}, marker: {selected_marker}")
                                    
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
                        plt.grid(axis='y', alpha=0.3, linestyle='--')  # Subtle grid
                        plt.tight_layout()
                
                merged_path = os.path.join(merged_folder, merged_filename)
                plt.savefig(merged_path, dpi=300, bbox_inches='tight')
                print(f"Saved merged graph: {merged_path}")
                plt.close()
            else:
                for file_idx, (file_path, data) in enumerate(self.csv_data.items()):
                    # Extract metadata from CSV
                    # Look for specific server/container name columns
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
                    chart_type = self.chart_type_var.get()
                    if chart_type == "auto":
                        # Auto mode: bar charts for websocket, line plots for others
                        use_bar_chart = measurement_type.lower() == 'websocket'
                    elif chart_type == "bar":
                        use_bar_chart = True
                    elif chart_type == "line":
                        use_bar_chart = False
                    else:
                        use_bar_chart = measurement_type.lower() == 'websocket'  # fallback
                    
                    if self.merge_columns_var.get():
                        # For single data point, create a simple bar chart
                        plt.figure(figsize=(10, 6))
                        data_columns_lower = {col.lower(): col for col in data.columns}
                        columns_to_plot = []
                        values_to_plot = []
                        
                        # Use original selection order for color assignment within this graph
                        for i, column in enumerate(selected_columns):
                            col_key = column.lower()
                            if col_key in data_columns_lower:
                                actual_col = data_columns_lower[col_key]
                                if actual_col in data.columns:
                                    columns_to_plot.append(actual_col)
                                    values_to_plot.append(data[actual_col].iloc[0])
                        
                        if columns_to_plot:
                            try:
                                bars = plt.bar(columns_to_plot, values_to_plot, 
                                             color=[color_marker_combinations[i % len(color_marker_combinations)][0] for i in range(len(values_to_plot))],
                                             alpha=0.8, edgecolor='black', linewidth=0.5)
                                plt.title(f"{server_name} - All Metrics", fontsize=14, fontweight='bold', pad=20)
                                plt.ylabel("Values", fontsize=12, fontweight='bold')
                                plt.xticks(rotation=45, ha='right', fontsize=10)
                                plt.yticks(fontsize=10)
                                
                                # Add value labels on bars with better styling
                                for bar, value in zip(bars, values_to_plot):
                                    height = bar.get_height()
                                    plt.text(bar.get_x() + bar.get_width()/2., height + height*0.01,
                                           f'{value:.2f}', ha='center', va='bottom', fontsize=9, 
                                           fontweight='bold', color='black')
                                
                                plt.grid(axis='y', alpha=0.3, linestyle='--')
                            except Exception as plot_error:
                                print(f"Error plotting merged columns for {server_name}: {plot_error}")
                                plt.text(0.5, 0.5, f'Plotting error: {plot_error}', ha='center', va='center', transform=plt.gca().transAxes)
                        
                        # Determine chart type name for filename
                        chart_type = self.chart_type_var.get()
                        if chart_type == "auto":
                            chart_type_name = "bar" if measurement_type.lower() == 'websocket' else "line"
                        elif chart_type == "bar":
                            chart_type_name = "bar"
                        elif chart_type == "line":
                            chart_type_name = "line"
                        else:
                            chart_type_name = "bar" if measurement_type.lower() == 'websocket' else "line"  # fallback
                        
                        save_path = os.path.join(file_graphs_folder, f"merged_columns_{chart_type_name}.png")
                        plt.savefig(save_path, dpi=300, bbox_inches='tight')
                        print(f"Saved graph: {save_path}")
                        plt.close()
                    else:
                        # Use original selection order for color assignment within this graph
                        for i, column in enumerate(selected_columns):
                            # For plotting, match columns case-insensitively
                            data_columns_lower = {col.lower(): col for col in data.columns}
                            col_key = column.lower()
                            if col_key in data_columns_lower:
                                actual_col = data_columns_lower[col_key]
                                if actual_col in data.columns:
                                    plt.figure(figsize=(10, 6))
                                    value = data[actual_col].iloc[0]
                                    
                                    if use_bar_chart:
                                        # Create a bar chart for single value
                                        bars = plt.bar([actual_col], [value], 
                                                     color=color_marker_combinations[i % len(color_marker_combinations)][0],
                                                     alpha=0.8, edgecolor='black', linewidth=0.5)
                                    else:
                                        # Create a line plot for single value (point plot)
                                        plt.plot([actual_col], [value], 
                                               color=color_marker_combinations[i % len(color_marker_combinations)][0],
                                               marker='o', markersize=8, linewidth=2, alpha=0.8)
                                        bars = None  # No bars for line plot
                                    plt.title(f"{server_name} - {actual_col}", fontsize=14, fontweight='bold', pad=20)
                                    plt.ylabel(actual_col, fontsize=12, fontweight='bold')
                                    plt.xticks(rotation=45, ha='right', fontsize=10)
                                    plt.yticks(fontsize=10)
                                    
                                    # Add value label with better styling
                                    if use_bar_chart and bars:
                                        bar = bars[0]
                                        height = bar.get_height()
                                        plt.text(bar.get_x() + bar.get_width()/2., height + height*0.01,
                                               f'{value:.2f}', ha='center', va='bottom', fontsize=10, 
                                               fontweight='bold', color='black')
                                    else:
                                        # For line plot, add text annotation
                                        plt.text(0.5, 0.95, f'{value:.2f}', ha='center', va='top', 
                                               transform=plt.gca().transAxes, fontsize=12, 
                                               fontweight='bold', color='black',
                                               bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.8))
                                    
                                    plt.grid(axis='y', alpha=0.3, linestyle='--')
                                    
                                    # Updated naming convention for non-merged graphs: {server}_{column}_{chart_type}.png
                                    safe_column = actual_col.replace(' ', '_').replace('(', '').replace(')', '').replace('%', 'pct').replace('/', '_')
                                    # Determine chart type name for filename
                                    chart_type = self.chart_type_var.get()
                                    if chart_type == "auto":
                                        chart_type_name = "bar" if measurement_type.lower() == 'websocket' else "line"
                                    elif chart_type == "bar":
                                        chart_type_name = "bar"
                                    elif chart_type == "line":
                                        chart_type_name = "line"
                                    else:
                                        chart_type_name = "bar" if measurement_type.lower() == 'websocket' else "line"  # fallback
                                    
                                    save_path = os.path.join(file_graphs_folder, f"{server_name}_{safe_column}_{chart_type_name}.png")
                                    plt.savefig(save_path, dpi=300, bbox_inches='tight')
                                    print(f"Saved graph: {save_path}")
                                    plt.close()

            messagebox.showinfo("Success", f"Graphs saved in {graphs_root}")
        except Exception as e:
            print(f"An error occurred: {e}")
            messagebox.showerror("Error", f"An error occurred: {e}")

    def _bind_mousewheel(self, event):
        """Bind mouse wheel events when the mouse enters the canvas."""
        if os.name == "nt":  # Windows
            self.canvas.bind_all("<MouseWheel>", self._on_mousewheel)
        else:  # Linux
            self.canvas.bind_all("<Button-4>", self._on_mousewheel_linux)
            self.canvas.bind_all("<Button-5>", self._on_mousewheel_linux)

    def _unbind_mousewheel(self, event):
        """Unbind mouse wheel events when the mouse leaves the canvas."""
        if os.name == "nt":  # Windows
            self.canvas.unbind_all("<MouseWheel>")
        else:
            self.canvas.unbind_all("<Button-4>")
            self.canvas.unbind_all("<Button-5>")

    def _on_mousewheel(self, event):
        """Scroll the canvas when the mouse wheel is used (Windows)."""
        self.canvas.yview_scroll(-1 * (event.delta // 120), "units")

    def _on_mousewheel_linux(self, event):
        """Scroll the canvas when the mouse wheel is used (Linux)."""
        if event.num == 4:  # Scroll up
            self.canvas.yview_scroll(-1, "units")
        elif event.num == 5:  # Scroll down
            self.canvas.yview_scroll(1, "units")

if __name__ == "__main__":
    root = tk.Tk()
    app = GraphGeneratorApp(root)
    root.mainloop()
