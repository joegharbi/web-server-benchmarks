#!/bin/bash

# Script to install, run, stop, and uninstall Yaws server with prebuilt Erlang from Erlang Solutions

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

NEW_PACKAGES_FILE="/tmp/newly_installed_yaws_packages.txt"
BASE_PACKAGES="git make build-essential autoconf automake libtool gcc autotools-dev libpam0g-dev openssl wget curl sudo"
ERLANG_URL="https://binaries2.erlang-solutions.com/debian/pool/contrib/e/esl-erlang/esl-erlang_27.3-1~debian~bookworm_amd64.deb"
ERLANG_DEB="/tmp/esl-erlang_27.3-1.deb"
YAWS_CONF="/usr/local/etc/yaws/yaws.conf"
YAWS_PID="/usr/local/var/run/yaws/yaws.pid"

check_existing_packages() {
    echo -e "${GREEN}Checking pre-existing packages...${NC}"
    > "$NEW_PACKAGES_FILE"
    for pkg in $BASE_PACKAGES; do
        if dpkg -l "$pkg" &>/dev/null; then
            echo "$pkg is already installed, will not remove it later."
        else
            echo "$pkg" >> "$NEW_PACKAGES_FILE"
        fi
    done
    # Always track esl-erlang as a new package to ensure cleanup
    echo "esl-erlang" >> "$NEW_PACKAGES_FILE"
}

cleanup_yaws() {
    echo -e "${GREEN}Performing full cleanup of Yaws and Erlang...${NC}"
    # Stop any running Yaws processes
    if ps aux | grep -v grep | grep '[y]aws' > /dev/null; then
        sudo yaws --stop 2>/dev/null || true
        sleep 2
        sudo killall -9 yaws 2>/dev/null || true
        sudo killall -9 beam.smp 2>/dev/null || true
    fi
    # Remove all Yaws-related files
    sudo rm -rf /usr/local/bin/yaws /usr/local/lib/yaws /usr/local/etc/yaws /usr/local/var/run/yaws /usr/local/var/log/yaws /var/www/html "$YAWS_PID" 2>/dev/null
    # Remove any local build directory
    [ -d "yaws" ] && rm -rf yaws
    # Remove existing Erlang installations
    if dpkg -l | grep -q erlang; then
        echo -e "${GREEN}Removing existing Erlang installations...${NC}"
        sudo apt-get remove -y --purge 'erlang-*' esl-erlang 2>/dev/null || true
        sudo apt-get autoremove -y 2>/dev/null || true
        sudo rm -rf /usr/lib/erlang /usr/bin/erl /usr/bin/erlc 2>/dev/null
    fi
    echo -e "${GREEN}Full cleanup completed${NC}"
}

install_erlang() {
    echo -e "${GREEN}Installing Erlang 27.3-1 from Erlang Solutions...${NC}"
    wget -O "$ERLANG_DEB" "$ERLANG_URL" || { echo -e "${RED}Failed to download Erlang .deb${NC}"; exit 1; }
    sudo dpkg -i "$ERLANG_DEB" || { echo -e "${RED}Failed to install Erlang .deb - may need dependencies${NC}"; sudo apt-get install -f -y; sudo dpkg -i "$ERLANG_DEB" || exit 1; }
    rm -f "$ERLANG_DEB"

    ERL_PATH="/usr/lib/erlang/bin/erl"
    if [ ! -f "$ERL_PATH" ]; then
        echo -e "${RED}Could not find 'erl' binary at $ERL_PATH after installation${NC}"
        exit 1
    fi
    echo -e "${GREEN}Found erl at: $ERL_PATH${NC}"

    ERL_OTP_VERSION=$("$ERL_PATH" -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>/dev/null | tr -d '"')
    echo -e "${GREEN}Erlang OTP version: $ERL_OTP_VERSION${NC}"

    if echo "$ERL_OTP_VERSION" | grep -q "^27"; then
        echo -e "${GREEN}Erlang 27.3-1 installed successfully${NC}"
    else
        echo -e "${RED}Erlang 27 not correctly installed or version mismatch (got $ERL_OTP_VERSION)${NC}"
        exit 1
    fi
}

install_server() {
    echo -e "${GREEN}Starting Yaws installation with full cleanup...${NC}"
    cleanup_yaws

    check_existing_packages

    echo -e "${GREEN}Installing dependencies...${NC}"
    sudo apt-get update
    for pkg in $BASE_PACKAGES; do
        if ! dpkg -l "$pkg" &>/dev/null; then
            sudo apt-get install -y "$pkg" || { echo -e "${RED}Failed to install $pkg${NC}"; exit 1; }
        fi
    done
    sudo apt-get clean && rm -rf /var/lib/apt/lists/*

    install_erlang

    if ! dpkg -l libpam0g-dev &>/dev/null; then
        echo -e "${RED}libpam0g-dev not installed. Forcing installation...${NC}"
        sudo apt-get update
        sudo apt-get install -y libpam0g-dev || { echo -e "${RED}Failed to install libpam0g-dev${NC}"; exit 1; }
    fi
    if ! find /usr -name pam_appl.h 2>/dev/null | grep -q .; then
        echo -e "${RED}pam_appl.h not found despite libpam0g-dev installation. Reinstalling...${NC}"
        sudo apt-get install --reinstall libpam0g-dev || { echo -e "${RED}Failed to reinstall libpam0g-dev${NC}"; exit 1; }
        if ! find /usr -name pam_appl.h 2>/dev/null | grep -q .; then
            echo -e "${RED}PAM headers still missing, configuring Yaws without PAM${NC}"
        fi
    else
        echo -e "${GREEN}pam_appl.h found at $(find /usr -name pam_appl.h 2>/dev/null | head -1)${NC}"
    fi

    echo -e "${GREEN}Building Yaws from source...${NC}"
    git clone https://github.com/erlyaws/yaws.git || { echo -e "${RED}Failed to clone Yaws${NC}"; exit 1; }
    cd yaws
    autoreconf -fi || { echo -e "${RED}autoreconf failed - check libtool and autoconf setup${NC}"; exit 1; }
    if find /usr -name pam_appl.h 2>/dev/null | grep -q .; then
        ./configure || { echo -e "${RED}Yaws configure failed${NC}"; exit 1; }
    else
        ./configure --disable-pam || { echo -e "${RED}Yaws configure failed even with --disable-pam${NC}"; exit 1; }
    fi
    make all && sudo make install || { echo -e "${RED}Yaws build or install failed${NC}"; exit 1; }
    cd ..
    rm -rf yaws

    echo -e "${GREEN}Setting up document root...${NC}"
    sudo mkdir -p /var/www/html || { echo -e "${RED}Failed to create document root${NC}"; exit 1; }
    sudo chown -R www-data:www-data /var/www/html

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

    echo -e "${GREEN}Configuring Yaws...${NC}"
    if [ ! -f "yaws.conf" ]; then
        echo -e "${RED}yaws.conf not found in current directory. Creating a default one with port 8001.${NC}"
        sudo bash -c "cat <<EOF > $YAWS_CONF
acceptor_pool_size = 8
max_connections = nolimit
<server localhost>
    port = 8001
    listen = 0.0.0.0
    docroot = /var/www/html
    access_log = false
</server>
EOF" || { echo -e "${RED}Failed to create yaws.conf${NC}"; exit 1; }
    else
        echo -e "${GREEN}Using existing yaws.conf from current directory. Ensure it uses port 8001.${NC}"
        sudo cp yaws.conf "$YAWS_CONF" || { echo -e "${RED}Failed to copy yaws.conf${NC}"; exit 1; }
    fi

    if ! command -v yaws >/dev/null 2>&1; then
        echo -e "${RED}Yaws installation failed - 'yaws' command not found${NC}"
        exit 1
    fi

    echo -e "${GREEN}Installation complete! Yaws will run on port 8001.${NC}"
}

run_server() {
    echo -e "${GREEN}Starting Yaws server on port 8001...${NC}"
    if ! command -v yaws >/dev/null 2>&1; then
        echo -e "${RED}Yaws is not installed. Run './setup_yaws.sh install' first.${NC}"
        exit 1
    fi
    sudo bash -c 'ulimit -n 100000; yaws --daemon --conf "$YAWS_CONF"'
    sleep 2
    if ps aux | grep -v grep | grep '[y]aws' > /dev/null; then
        echo -e "${GREEN}Server is running. Access it at http://localhost:8001${NC}"
    else
        echo -e "${RED}Failed to start server. Check configuration or port availability.${NC}"
        exit 1
    fi
}

stop_server() {
    echo -e "${GREEN}Stopping Yaws server...${NC}"
    if ps aux | grep -v grep | grep '[y]aws' > /dev/null; then
        set +e
        sudo yaws --stop
        sleep 5
        if ps aux | grep -v grep | grep '[y]aws' > /dev/null; then
            echo -e "${RED}Failed to stop server gracefully. Forcing termination...${NC}"
            sudo kill -9 $(pgrep yaws) >/dev/null 2>&1
            sudo kill -9 $(pgrep beam.smp) >/dev/null 2>&1
            sleep 2
            if ps aux | grep -v grep | grep '[y]aws' > /dev/null; then
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
        echo -e "${GREEN}No running Yaws server found.${NC}"
    fi
    [ -f "$YAWS_PID" ] && sudo rm -f "$YAWS_PID"
}

uninstall_server() {
    echo -e "${GREEN}Uninstalling Yaws with full cleanup...${NC}"
    cleanup_yaws

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