#!/bin/bash

# Script to install, run, stop, and uninstall Nginx server on a local Debian-based system

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

NEW_PACKAGES_FILE="/tmp/newly_installed_nginx_packages.txt"
PACKAGES="git make build-essential autoconf automake libtool gcc autotools-dev libpam0g-dev openssl wget curl sudo libpcre3-dev zlib1g-dev"
NGINX_BIN="/usr/local/nginx/sbin/nginx"
NGINX_CONF="/usr/local/nginx/conf/nginx.conf"
NGINX_PID="/usr/local/nginx/logs/nginx.pid"

check_existing_packages() {
    echo -e "${GREEN}Checking pre-existing packages...${NC}"
    > "$NEW_PACKAGES_FILE"
    for pkg in $PACKAGES; do
        if dpkg -l "$pkg" &>/dev/null; then
            echo "$pkg is already installed, will not remove it later."
        else
            echo "$pkg" >> "$NEW_PACKAGES_FILE"
        fi
    done
}

cleanup_nginx() {
    echo -e "${GREEN}Performing full cleanup of Nginx...${NC}"
    # Stop any running Nginx processes
    if ps aux | grep -v grep | grep '[n]ginx' > /dev/null; then
        sudo "$NGINX_BIN" -s stop 2>/dev/null || true
        sleep 2
        sudo killall -9 nginx 2>/dev/null || true
    fi
    # Remove all Nginx-related files
    sudo rm -rf /usr/local/nginx /var/www/html /usr/local/bin/nginx /var/log/nginx "$NGINX_PID" 2>/dev/null
    # Remove any local build directory
    [ -d "nginx-1.24.0" ] && rm -rf nginx-1.24.0
    echo -e "${GREEN}Full cleanup completed${NC}"
}

install_server() {
    echo -e "${GREEN}Starting Nginx installation with full cleanup...${NC}"
    cleanup_nginx

    check_existing_packages

    echo -e "${GREEN}Installing dependencies...${NC}"
    sudo apt-get update
    for pkg in $PACKAGES; do
        if ! dpkg -l "$pkg" &>/dev/null; then
            sudo apt-get install -y "$pkg" || { echo -e "${RED}Failed to install $pkg${NC}"; exit 1; }
        fi
    done
    sudo apt-get clean && rm -rf /var/lib/apt/lists/*

    echo -e "${GREEN}Building Nginx from source...${NC}"
    wget http://nginx.org/download/nginx-1.24.0.tar.gz || { echo -e "${RED}Failed to download Nginx${NC}"; exit 1; }
    tar -xzf nginx-1.24.0.tar.gz
    cd nginx-1.24.0
    ./configure --without-http_rewrite_module --without-http_gzip_module --sbin-path="$NGINX_BIN" --conf-path="$NGINX_CONF" --pid-path="$NGINX_PID" || { echo -e "${RED}Nginx configure failed${NC}"; exit 1; }
    make && sudo make install || { echo -e "${RED}Nginx build or install failed${NC}"; exit 1; }
    cd ..
    rm -rf nginx-1.24.0.tar.gz nginx-1.24.0

    echo -e "${GREEN}Setting up document root and logs directory...${NC}"
    sudo mkdir -p /var/www/html /usr/local/nginx/logs
    sudo chown -R www-data:www-data /var/www/html /usr/local/nginx/logs

    echo -e "${GREEN}Creating index.html...${NC}"
    sudo bash -c 'cat <<EOF > /var/www/html/index.html
<!DOCTYPE html>
<html>
  <head>
    <title>Energy Test</title>
  </head>
  <body>
    <h1>Hello, Energy Test!</h1>
  </body>
</html>
EOF' || { echo -e "${RED}Failed to create index.html${NC}"; exit 1; }

    echo -e "${GREEN}Configuring Nginx...${NC}"
    sudo bash -c "cat <<EOF > $NGINX_CONF
pid $NGINX_PID;
worker_processes 8;
events {
    worker_connections 1024;
}
http {
    server {
        listen 8001;
        root /var/www/html;
        index index.html;
        access_log off;
    }
}
EOF" || { echo -e "${RED}Failed to configure Nginx${NC}"; exit 1; }

    echo -e "${GREEN}Installation complete! Nginx will run on port 8001.${NC}"
}

run_server() {
    echo -e "${GREEN}Starting Nginx server on port 8001...${NC}"
    if [ ! -f "$NGINX_BIN" ]; then
        echo -e "${RED}Nginx is not installed. Run './setup_nginx.sh install' first.${NC}"
        exit 1
    fi
    sudo bash -c 'ulimit -n 100000; "$NGINX_BIN" -c "$NGINX_CONF"'
    sleep 2
    if ps aux | grep -v grep | grep '[n]ginx' > /dev/null; then
        echo -e "${GREEN}Server is running. Access it at http://localhost:8001${NC}"
    else
        echo -e "${RED}Failed to start server. Check configuration or port availability.${NC}"
        exit 1
    fi
}

stop_server() {
    echo -e "${GREEN}Stopping Nginx server...${NC}"
    if ps aux | grep -v grep | grep '[n]ginx' > /dev/null; then
        set +e
        sudo "$NGINX_BIN" -s stop
        sleep 5
        if ps aux | grep -v grep | grep '[n]ginx' > /dev/null; then
            echo -e "${RED}Failed to stop server gracefully. Forcing termination...${NC}"
            sudo kill -9 $(pgrep nginx) >/dev/null 2>&1
            sleep 2
            if ps aux | grep -v grep | grep '[n]ginx' > /dev/null; then
                echo -e "${RED}Failed to stop server even with force. Check processes manually.${NC}"
                exit 1
            else
                echo -e "${GREEN}Server stopped successfully (forced).${NC}"
            fi
        else
            echo -e "${GREEN}Server stopped successfully.${NC}"
        fi
        set -e
    else
        echo -e "${GREEN}No running Nginx server found.${NC}"
    fi
    [ -f "$NGINX_PID" ] && sudo rm -f "$NGINX_PID"
}

uninstall_server() {
    echo -e "${GREEN}Uninstalling Nginx with full cleanup...${NC}"
    cleanup_nginx

    if [ -f "$NEW_PACKAGES_FILE" ]; then
        echo -e "${GREEN}Removing only newly installed packages...${NC}"
        NEW_PACKAGES=$(cat "$NEW_PACKAGES_FILE")
        if [ -n "$NEW_PACKAGES" ]; then
            sudo apt-get remove -y $NEW_PACKAGES
            sudo apt-get autoremove -y
            sudo apt-get clean
        else
            echo "No new packages were installed by this script."
        fi
        rm -f "$NEW_PACKAGES_FILE"
    else
        echo "No record of newly installed packages found."
    fi

    echo -e "${GREEN}Uninstallation complete!${NC}"
}

usage() {
    echo "Usage: $0 {install|run|stop|uninstall}"
    exit 1
}

case "$1" in
    "install") install_server ;;
    "run") run_server ;;
    "stop") stop_server ;;
    "uninstall") uninstall_server ;;
    *) usage ;;
esac