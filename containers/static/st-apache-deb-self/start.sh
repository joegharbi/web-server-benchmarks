#!/bin/sh
ulimit -n 100000
exec apachectl -D FOREGROUND -f /etc/apache2/apache2.conf 