# Web Server Benchmarks Makefile

# Configuration
VENV_NAME ?= srv
VENV_PATH = $(VENV_NAME)/bin/activate

.PHONY: help install test clean build run-all run-static run-dynamic run-websocket run-local setup quick-test graph validate

help: ## Show this help message
	@echo "Web Server Benchmarks - Available Commands:"
	@echo ""
	@echo "Environment: $(VENV_NAME) (change with VENV_NAME=name)"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | grep -vE 'check-env|ensure-env' | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
	@echo ""
	@echo "Setup:"
	@echo "  1. Create virtual environment: python -m venv $(VENV_NAME)"
	@echo "  2. Activate environment: source $(VENV_PATH)"
	@echo "  3. Install dependencies: make install"
	@echo ""
	@echo "Auto-Discovery:"
	@echo "  - Add new servers: create folder + Dockerfile in containers/static, containers/dynamic, or web-socket/"

# Environment management
check-env: ## Check if virtual environment is active
	@if [ -z "$$VIRTUAL_ENV" ]; then \
		echo "ERROR: Virtual environment not active"; \
		echo "Please run: source $(VENV_PATH)"; \
		echo "Or use: make ensure-env"; \
		exit 1; \
	fi
	@echo "Virtual environment active: $$VIRTUAL_ENV"

ensure-env: ## Ensure virtual environment is active
	@if [ -z "$$VIRTUAL_ENV" ]; then \
		echo "Activating virtual environment: $(VENV_NAME)"; \
		. $(VENV_PATH); \
	fi

install: check-env ## Install Python dependencies
	@echo "Installing dependencies in virtual environment..."
	@pip install -r requirements.txt

test: check-env ## Run unit tests
	@echo "Running tests in virtual environment..."
	@python -m unittest discover tests/ -v

build: ## Build all Docker images
	./install_benchmarks.sh

clean: ## Clean up Docker containers and images
	./install_benchmarks.sh clean

run-all: ## Run all benchmarks
	./run_benchmarks.sh

run-static: ## Run static server benchmarks
	./run_benchmarks.sh static

run-dynamic: ## Run dynamic server benchmarks
	./run_benchmarks.sh dynamic

run-websocket: ## Run WebSocket benchmarks
	./run_benchmarks.sh websocket

run-local: ## Run local server benchmarks
	./run_benchmarks.sh local

graph: check-env ## Generate graphs from results
	@python3 gui_graph_generator.py

validate: check-env ## Check and validate all required dependencies
	@echo "Checking dependencies..."
	@python3 --version || echo "ERROR: Python 3 not found"
	@docker --version || echo "ERROR: Docker not found"
	@scaphandre --version || echo "ERROR: Scaphandre not found"
	@echo "Validation complete"

setup: ## Create the virtual environment and set up local servers
	@echo "Creating virtual environment: $(VENV_NAME)"
	@python -m venv $(VENV_NAME)
	@echo "Virtual environment created!"
	@echo "Setting up local servers (nginx, yaws)..."
	sudo ./local/setup_nginx.sh install
	sudo ./local/setup_yaws.sh install
	@echo "Next steps:"
	@echo "  1. Activate: source $(VENV_PATH)"
	@echo "  2. Install dependencies: make install"
	@echo "  3. Validate: make validate"

quick-test: ## Quick test with minimal requests
	./run_benchmarks.sh static st-nginx-deb
	./run_benchmarks.sh dynamic dy-nginx-deb
	./run_benchmarks.sh websocket ws-nginx 