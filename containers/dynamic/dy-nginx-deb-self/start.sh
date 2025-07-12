#!/bin/bash
python3 -m gunicorn --bind 127.0.0.1:8000 app:app &
sleep 2
/usr/local/nginx/sbin/nginx -c /usr/local/nginx/conf/nginx.conf
sleep infinity