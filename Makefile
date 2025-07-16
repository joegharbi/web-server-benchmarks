# Web Server Benchmarks Makefile

.SHELL := /bin/bash

# Configuration
VENV_NAME ?= srv
VENV_PATH = $(VENV_NAME)/bin/activate

.PHONY: help install clean-build clean-repo build run run-static run-dynamic run-websocket run-local setup graph validate check-health build-test-run run-single setup-local clean-local clean-all

# Color codes
GREEN=\033[0;32m
RED=\033[0;31m
YELLOW=\033[1;33m
NC=\033[0m

check-tools: ## Check for required tools (Python, pip, Docker, scaphandre)
	@command -v python3 >/dev/null 2>&1 || { printf "${RED}ERROR:${NC} python3 not found\n"; exit 1; }
	@command -v pip >/dev/null 2>&1 || { printf "${RED}ERROR:${NC} pip not found\n"; exit 1; }
	@command -v docker >/dev/null 2>&1 || { printf "${RED}ERROR:${NC} docker not found\n"; exit 1; }
	@command -v scaphandre >/dev/null 2>&1 || { printf "${YELLOW}WARNING:${NC} scaphandre not found (energy measurements will be skipped)\n"; }
	@printf "${GREEN}All required tools found.${NC}\n"

check-env:  ## Check if Python virtual environment exists
	@if [ ! -d srv ]; then \
		echo "ERROR: Python virtual environment not found"; \
		echo "Please run: make setup"; \
		exit 1; \
	fi

install: check-env ## Install Python dependencies in the active virtual environment
	@printf "${YELLOW}Installing dependencies in virtual environment...${NC}\n"
	@pip install -r requirements.txt
	@printf "${GREEN}Dependencies installed!${NC}\n"

build: check-tools ## Build all Docker images for all discovered containers
	@printf "${YELLOW}Building all Docker images...${NC}\n"
	@bash install_benchmarks.sh
	@printf "${GREEN}Docker images built!${NC}\n"

clean-build: ## Clean up Docker containers and images (use 'make clean-all' to also clean local servers)
	@bash install_benchmarks.sh clean

clean-local: ## Uninstall all local servers with uninstall support
	@for script in ./local/setup_*.sh; do \
		if grep -q 'uninstall_server()' $$script && grep -q 'uninstall") uninstall_server' $$script; then \
			printf "Uninstalling with $$script\n"; \
			sudo $$script uninstall; \
		fi \
	done

clean-all: ## Clean up Docker containers/images and uninstall local servers
	@$(MAKE) clean-build
	@$(MAKE) clean-local

clean-repo: ## Clean repository to bare minimum (fresh clone state)
	@bash run_benchmarks.sh clean

run:  ## Run all benchmarks (static, dynamic, websocket, local)
	@for v in ./*/bin/activate; do \
		if [ -f "$$v" ]; then . "$$v"; break; fi; \
	done; \
	bash run_benchmarks.sh

run-quick:  ## Quick test with single request per container type
	@for v in ./*/bin/activate; do \
		if [ -f "$$v" ]; then . "$$v"; break; fi; \
	done; \
	bash run_benchmarks.sh --super-quick

run-single:  ## Run a single server benchmark (usage: make run-single SERVER=dy-nginx-deb-self)
	@for v in ./*/bin/activate; do \
		if [ -f "$$v" ]; then . "$$v"; break; fi; \
	done; \
	bash run_benchmarks.sh --single $(SERVER)

run-static:  ## Run static server benchmarks only
	@for v in ./*/bin/activate; do \
		if [ -f "$$v" ]; then . "$$v"; break; fi; \
	done; \
	bash run_benchmarks.sh --static

run-dynamic:  ## Run dynamic server benchmarks only
	@for v in ./*/bin/activate; do \
		if [ -f "$$v" ]; then . "$$v"; break; fi; \
	done; \
	bash run_benchmarks.sh --dynamic

run-websocket:  ## Run WebSocket server benchmarks only
	@for v in ./*/bin/activate; do \
		if [ -f "$$v" ]; then . "$$v"; break; fi; \
	done; \
	bash run_benchmarks.sh --websocket

run-local:  ## Run local server benchmarks only
	@for v in ./*/bin/activate; do \
		if [ -f "$$v" ]; then . "$$v"; break; fi; \
	done; \
	bash run_benchmarks.sh --local

check-health:  ## Check health of all built containers (startup, HTTP response, stability)
	@for v in ./*/bin/activate; do \
		if [ -f "$$v" ]; then . "$$v"; break; fi; \
	done; \
	bash check_health.sh

build-test-run: check-env ## Build all containers, check health, and run all benchmarks
	@printf "${YELLOW}=== Building all containers ===${NC}\n"
	@$(MAKE) build
	@printf "\n"
	@printf "${YELLOW}=== Checking container health ===${NC}\n"
	@$(MAKE) check-health
	@printf "\n"
	@printf "${YELLOW}=== Running all benchmarks ===${NC}\n"
	@$(MAKE) run

graph:  ## Launch the GUI graph generator
	@python3 gui_graph_generator.py

validate: check-tools check-env check-health ## Validate all prerequisites and health
	@printf "${GREEN}Validation complete!${NC}\n"

run-all: run  ## Run the full benchmark suite (alias)

setup:  ## Set up Python virtual environment and install dependencies
	@if [ ! -d srv ]; then \
		python3 -m venv srv; \
		echo "[INFO] Created Python virtual environment in ./srv"; \
	else \
		echo "[INFO] Python virtual environment already exists in ./srv"; \
	fi
	@for v in ./*/bin/activate; do \
		if [ -f "$$v" ]; then . "$$v"; break; fi; \
	done; \
	pip install --upgrade pip && pip install -r requirements.txt
	@echo "[INFO] Python environment is ready."

setup-local: check-env ## Install/setup all local servers (nginx, yaws, etc.)
	sudo ./local/setup_nginx.sh install
	sudo ./local/setup_yaws.sh install
	@printf "${GREEN}Local servers installed.${NC}\n"

# Aliases for backward compatibility (not shown in help)
# Removed: setup-docker

# Set help as the default goal
.DEFAULT_GOAL := help

# Color variables
YELLOW=\033[1;33m
CYAN=\033[1;36m
GREEN=\033[1;32m
NC=\033[0m

# Removed: concise-help

help:  ## Show this help message
	@printf "${YELLOW}Web Server Benchmarks - Available Commands:${NC}\n\n"
	@printf "${CYAN}Environment:${NC} srv (change with VENV_NAME=name)\n\n"
	@printf "${YELLOW}Setup:${NC}\n"
	@printf "  %-22s %s\n" "init" "One-step setup: venv, install, build, local servers, validate (recommended for new users)"
	@printf "  %-22s %s\n" "setup" "Set up Python virtual environment and install dependencies"
	@printf "  %-22s %s\n" "setup-local" "Install/setup all local servers (nginx, yaws, etc.)"
	@printf "  %-22s %s\n" "build" "Build all Docker images for all discovered containers"
	@printf "\n"
	@printf "${YELLOW}Run Benchmarks:${NC}\n"
	@printf "  %-22s %s\n" "run" "Run the full benchmark suite"
	@printf "  %-22s %s\n" "run-all" "Alias for run (full benchmark suite)"
	@printf "  %-22s %s\n" "run-quick" "Super quick test (fastest validation)"
	@printf "  %-22s %s\n" "run-single" "Run a single server benchmark (SERVER=dy-nginx-deb-self)"
	@printf "  %-22s %s\n" "run-static" "Run static server benchmarks only"
	@printf "  %-22s %s\n" "run-dynamic" "Run dynamic server benchmarks only"
	@printf "  %-22s %s\n" "run-websocket" "Run WebSocket server benchmarks only"
	@printf "  %-22s %s\n" "run-local" "Run local server benchmarks only"
	@printf "\n"
	@printf "${YELLOW}Validation & Health:${NC}\n"
	@printf "  %-22s %s\n" "check-tools" "Check for required tools (Python, pip, Docker, scaphandre)"
	@printf "  %-22s %s\n" "check-env" "Check if virtual environment is active"
	@printf "  %-22s %s\n" "check-health" "Check health of all built containers (startup, HTTP response, stability)"
	@printf "  %-22s %s\n" "validate" "Validate all prerequisites and health"
	@printf "\n"
	@printf "${YELLOW}Build & Clean:${NC}\n"
	@printf "  %-22s %s\n" "build-test-run" "Build all containers, check health, and run all benchmarks"
	@printf "  %-22s %s\n" "clean-build" "Clean up Docker containers and images (use 'make clean-all' to also clean local servers)"
	@printf "  %-22s %s\n" "clean-local" "Uninstall all local servers with uninstall support"
	@printf "  %-22s %s\n" "clean-all" "Clean up Docker containers/images and uninstall local servers"
	@printf "  %-22s %s\n" "clean-repo" "Clean repository to bare minimum (fresh clone state)"
	@printf "\n"
	@printf "${YELLOW}Other:${NC}\n"
	@printf "  %-22s %s\n" "graph" "Launch the GUI graph generator"
	@printf "  %-22s %s\n" "help" "Show this help message"
	@printf "\n"
	@printf "${GREEN}Quick Start:${NC}\n"
	@printf "  make init    # One-step setup for new users (recommended)\n"
	@printf "  make run     # Run all benchmarks\n"
	@printf "  make run-quick # Super quick test\n"
	@printf "\n"
	@printf "${CYAN}Auto-Discovery:${NC}\n"
	@printf "  - Add new servers: create folder + Dockerfile in containers/static, containers/dynamic, or web-socket/\n"
	@printf "  - Framework automatically detects, builds, tests, and benchmarks all containers\n"
	@printf "  - No naming conventions required - any directory name works\n"
	@printf "  - Port assignment based on Dockerfile EXPOSE directive\n"
	@printf "\n"
	@printf "${CYAN}Advanced:${NC}\n"
	@printf "  %-22s %s\n" "install" "(Advanced) Install Python dependencies in the active virtual environment only" 

init:  ## One-step setup: venv, install, build, local servers, validate
	@if [ ! -d srv ]; then \
		python3 -m venv srv; \
		echo "[INFO] Created Python virtual environment in ./srv"; \
	else \
		echo "[INFO] Python virtual environment already exists in ./srv"; \
	fi
	@for v in ./*/bin/activate; do \
		if [ -f "$$v" ]; then . "$$v"; break; fi; \
	done; \
	pip install --upgrade pip && pip install -r requirements.txt
	@$(MAKE) build
	@$(MAKE) setup-local
	@$(MAKE) validate
	@echo "[INFO] All setup complete! Your environment is ready to run benchmarks." 