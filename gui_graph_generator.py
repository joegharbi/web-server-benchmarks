import os
import tkinter as tk
from tkinter import filedialog, messagebox
import pandas as pd
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

        # Separate buttons for folder and file selection
        self.open_folder_button = tk.Button(root, text="Open Folder", command=self.select_folder, font=("Arial", 12))
        self.open_folder_button.pack(pady=5)

        self.open_files_button = tk.Button(root, text="Open Files", command=self.select_files, font=("Arial", 12))
        self.open_files_button.pack(pady=5)

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

        # Merge options
        self.merge_csv_var = tk.BooleanVar()
        self.merge_csv_checkbox = tk.Checkbutton(root, text="Merge CSV Files into One Graph", variable=self.merge_csv_var, font=("Arial", 10))
        self.merge_csv_checkbox.pack(anchor="w", pady=5)

        self.merge_columns_var = tk.BooleanVar()
        self.merge_columns_checkbox = tk.Checkbutton(root, text="Merge Columns into One Graph", variable=self.merge_columns_var, font=("Arial", 10))
        self.merge_columns_checkbox.pack(anchor="w", pady=5)

        # Generate graphs button
        self.generate_button = tk.Button(root, text="Generate Graphs", command=self.generate_graphs, font=("Arial", 12))
        self.generate_button.pack(pady=10)

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

            colors = ['b', 'g', 'r', 'c', 'm', 'y', 'k']
            markers = ['o', 's', 'D', '^', 'v', '<', '>', 'p', '*', 'h', 'H', '+', 'x', '|', '_']
            color_marker_combinations = [(color, marker) for color in colors for marker in markers]
            import random
            random.shuffle(color_marker_combinations)

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
                    if 'container_name' in data.columns:
                        server_names.add(str(data['container_name'].iloc[0]))
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
                # Updated naming convention for merged graphs: preview-first-2-servers_Nservers_type_hash.png
                import hashlib
                server_names_sorted = sorted(server_names)
                servers_concat = ",".join(server_names_sorted)
                hash_digest = hashlib.sha1(servers_concat.encode()).hexdigest()[:6]
                n_servers = len(server_names_sorted)
                n_types = len(types_found) if types_found else 1
                preview = "-".join(server_names_sorted[:2])
                if n_types == 1:
                    merged_filename = f"{preview}_{n_servers}servers_{merged_type}_{hash_digest}.png"
                else:
                    merged_filename = f"{preview}_{n_servers}servers_{n_types}types_{hash_digest}.png"
                merged_path = os.path.join(merged_folder, merged_filename)
                plt.savefig(merged_path, dpi=300)
                print(f"Saved merged graph: {merged_path}")
                plt.close()
            else:
                for file_idx, (file_path, data) in enumerate(self.csv_data.items()):
                    # Extract metadata from CSV
                    if 'Server Name' in data.columns:
                        server_name = str(data['Server Name'].iloc[0])
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

                    if self.merge_columns_var.get():
                        plt.figure(figsize=(10, 6))
                        # For plotting, match columns case-insensitively
                        data_columns_lower = {col.lower(): col for col in data.columns}
                        for i, column in enumerate(selected_columns):
                            col_key = column.lower()
                            if col_key in data_columns_lower:
                                actual_col = data_columns_lower[col_key]
                                color, marker = color_marker_combinations[(file_idx * len(selected_columns) + i) % len(color_marker_combinations)]
                                plt.plot(data.index, data[actual_col], label=f"{server_name}/{os.path.splitext(os.path.basename(file_path))[0]} - {actual_col}", color=color, marker=marker, linestyle='-')
                        plt.title(f"{server_name}")
                        plt.xlabel("Number of Requests")
                        plt.ylabel("Values")
                        plt.legend(framealpha=0.5)
                        plt.grid(True)
                        save_path = os.path.join(file_graphs_folder, f"merged_columns.png")
                        plt.savefig(save_path, dpi=300)
                        print(f"Saved graph: {save_path}")
                        plt.close()
                    else:
                        for i, column in enumerate(selected_columns):
                            # For plotting, match columns case-insensitively
                            data_columns_lower = {col.lower(): col for col in data.columns}
                            col_key = column.lower()
                            if col_key in data_columns_lower:
                                actual_col = data_columns_lower[col_key]
                                plt.figure(figsize=(10, 6))
                                color, marker = color_marker_combinations[(file_idx * len(selected_columns) + i) % len(color_marker_combinations)]
                                plt.plot(data.index, data[actual_col], label=f"{server_name}/{os.path.splitext(os.path.basename(file_path))[0]} - {actual_col}", color=color, marker=marker, linestyle='-')
                                plt.title(f"{server_name} - {actual_col}")
                                plt.xlabel("Number of Requests")
                                plt.ylabel(actual_col)
                                plt.legend(framealpha=0.5)
                                plt.grid(True)
                                # Updated naming convention for non-merged graphs: {server}_{column}_{hash}.png
                                import hashlib
                                hash_input = f"{server_name},{file_path}".encode()
                                hash_digest = hashlib.sha1(hash_input).hexdigest()[:6]
                                safe_column = actual_col.replace(' ', '_')
                                save_path = os.path.join(file_graphs_folder, f"{server_name}_{safe_column}_{hash_digest}.png")
                                plt.savefig(save_path, dpi=300)
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
