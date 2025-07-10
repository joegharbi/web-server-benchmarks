# Web Server Benchmarks Makefile

# Configuration
VENV_NAME ?= srv
VENV_PATH = $(VENV_NAME)/bin/activate

.PHONY: help install test clean build run-all run-static run-dynamic run-websocket run-local check-env ensure-env

help: ## Show this help message
	@echo "Web Server Benchmarks - Available Commands:"
	@echo ""
	@echo "Environment: $(VENV_NAME) (change with VENV_NAME=name)"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
	@echo ""
	@echo "Setup:"
	@echo "  1. Create virtual environment: python -m venv $(VENV_NAME)"
	@echo "  2. Activate environment: source $(VENV_PATH)"
	@echo "  3. Install dependencies: make install"
	@echo ""
	@echo "Auto-Discovery:"
	@echo "  - Add new servers: create folder + Dockerfile in containers/static, containers/dynamic, or web-socket/"
	@echo "  - No configuration files needed!"

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

test-config: check-env ## Test configuration file
	@echo "No config file needed - using auto-discovery system"

build: ## Build all Docker images
	./install_benchmarks.sh

clean: ## Clean up Docker containers and images
	sudo docker rm -f $$(sudo docker ps -aq) 2>/dev/null || true
	sudo docker rmi -f $$(sudo docker images -aq) 2>/dev/null || true
	sudo docker system prune -af

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

setup-local: ## Setup local servers (nginx, yaws)
	sudo ./local/setup_nginx.sh install
	sudo ./local/setup_yaws.sh install

graph: check-env ## Generate graphs from results
	@python3 gui_graph_generator.py

lint: ## Run code linting (if you have flake8 installed)
	flake8 containers/ local/ web-socket/ --max-line-length=120 --ignore=E501,W503

format: ## Format code with black (if you have black installed)
	black containers/ local/ web-socket/ --line-length=120

check-deps: check-env ## Check if all required dependencies are installed
	@echo "Checking dependencies..."
	@python3 --version || echo "ERROR: Python 3 not found"
	@docker --version || echo "ERROR: Docker not found"
	@scaphandre --version || echo "ERROR: Scaphandre not found"
	@echo "Dependency check complete"

validate: check-deps ## Validate dependencies
	@echo "Validation complete"

# Development helpers
setup-env: ## Create and setup virtual environment
	@echo "Creating virtual environment: $(VENV_NAME)"
	@python -m venv $(VENV_NAME)
	@echo "Virtual environment created!"
	@echo "Next steps:"
	@echo "  1. Activate: source $(VENV_PATH)"
	@echo "  2. Install: make install"

dev-setup: setup-env install setup-local validate ## Complete development setup
	@echo "Development environment ready!"

quick-test: ## Quick test with minimal requests
	./run_benchmarks.sh static st-nginx-deb
	./run_benchmarks.sh dynamic dy-nginx-deb
	./run_benchmarks.sh websocket ws-nginx

# Documentation
docs: ## Generate documentation (placeholder)
	@echo "Documentation generation not yet implemented"
	@echo "See README.md for current documentation" 