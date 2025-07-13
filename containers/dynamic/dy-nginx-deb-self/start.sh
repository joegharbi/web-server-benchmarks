#!/bin/bash
python3 app.py &
sleep 2
nginx -g "daemon off;" &
sleep infinity