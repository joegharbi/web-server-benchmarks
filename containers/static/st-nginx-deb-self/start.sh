#!/bin/sh
ulimit -n 100000
exec nginx -g 'daemon off;' 