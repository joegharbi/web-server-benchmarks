# Web Server Benchmarks Makefile

# Configuration
VENV_NAME ?= srv
VENV_PATH = $(VENV_NAME)/bin/activate

.PHONY: help install clean-build clean-repo build run-all run-static run-dynamic run-websocket run-local setup quick-test graph validate check-health build-test-run test-container

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
	@echo "Quick Start:"
	@echo "  make build-test-run    # Build, check health, and run all benchmarks"
	@echo "  make test-container    # Test a specific container (usage: make test-container CONTAINER=st-nginx-deb)"
	@echo ""
	@echo "Auto-Discovery:"
	@echo "  - Add new servers: create folder + Dockerfile in containers/static, containers/dynamic, or web-socket/"
	@echo "  - Framework automatically detects, builds, tests, and benchmarks all containers"
	@echo "  - No naming conventions required - any directory name works"
	@echo "  - Port assignment based on Dockerfile EXPOSE directive"

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

build: ## Build all Docker images
	./install_benchmarks.sh

clean-build: ## Clean up Docker containers and images
	./install_benchmarks.sh clean

clean-repo: ## Clean repository to bare minimum (fresh clone state)
	./run_benchmarks.sh clean

run-all: ## Run all benchmarks
	./run_benchmarks.sh

run: run-all ## Alias for run-all

run-static: ## Run static server benchmarks
	./run_benchmarks.sh static

run-dynamic: ## Run dynamic server benchmarks
	./run_benchmarks.sh dynamic

run-websocket: ## Run WebSocket benchmarks
	./run_benchmarks.sh websocket

run-local: ## Run local server benchmarks
	./run_benchmarks.sh local

check-health: ## Check health of all built containers (startup, HTTP response, stability)
	@echo "Checking health of all built containers..."
	@./check_health.sh

health: check-health ## Alias for check-health

check: check-health ## Alias for check-health

build-test-run: ## Build all containers, check health, and run all benchmarks
	@echo "=== Building all containers ==="
	@$(MAKE) build
	@echo ""
	@echo "=== Checking container health ==="
	@$(MAKE) check-health
	@echo ""
	@echo "=== Running all benchmarks ==="
	@$(MAKE) run-all

test-container: ## Test a specific container (usage: make test-container CONTAINER=st-nginx-deb)
	@if [ -z "$(CONTAINER)" ]; then \
		echo "ERROR: CONTAINER variable not set"; \
		echo "Usage: make test-container CONTAINER=<container-name>"; \
		echo "Example: make test-container CONTAINER=st-nginx-deb"; \
		exit 1; \
	fi
	@echo "Testing container: $(CONTAINER)"
	@echo "=== Building container ==="
	@docker build -t $(CONTAINER) ./containers/*/$(CONTAINER)/ 2>/dev/null || docker build -t $(CONTAINER) ./web-socket/$(CONTAINER)/ 2>/dev/null || (echo "ERROR: Container $(CONTAINER) not found" && exit 1)
	@echo "=== Checking container health ==="
	@./check_health.sh | grep -A 5 -B 5 "$(CONTAINER)" || echo "Container $(CONTAINER) health check completed"
	@echo "=== Running container benchmark ==="
	@if [[ "$(CONTAINER)" == ws-* ]]; then \
		./run_benchmarks.sh websocket $(CONTAINER); \
	elif [[ "$(CONTAINER)" == dy-* ]]; then \
		./run_benchmarks.sh dynamic $(CONTAINER); \
	elif [[ "$(CONTAINER)" == st-* ]]; then \
		./run_benchmarks.sh static $(CONTAINER); \
	else \
		echo "ERROR: Unknown container type. Container name should start with ws-, dy-, or st-"; \
		exit 1; \
	fi

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

run-quick: ## Quick test with minimal requests (all discovered containers)
	./run_benchmarks.sh --quick 