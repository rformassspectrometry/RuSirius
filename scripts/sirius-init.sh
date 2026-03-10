#!/usr/bin/with-contenv bash
## Start Sirius REST API on port 9999 at container startup.
## RuSirius vignettes connect with: Sirius(port = 9999)

echo "[sirius-init] Starting Sirius REST API on port 9999..."

s6-setuidgid rstudio \
    /home/rstudio/sirius/bin/sirius -p 9999 service \
    > /var/log/sirius.log 2>&1 &

echo "[sirius-init] Sirius service launched (PID: $!)"
