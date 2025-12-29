#!/bin/sh

# cache ip and host
status_host=$([ -f $HOME/.name ] && cat $HOME/.name || hostname || cat /etc/hostname)
status_ip=$(ip -o -4 addr | awk -F 'inet |/' '!/127.0.0.1/ {print $2}' | sort -n | head -n 1)
tmux set-option -g status-right "#[fg=green]#(cut -d ' ' -f 1-3 /proc/loadavg) #[fg=#E6E4D9]${status_host}  ${status_ip}  %A %H:%M:%S "
tmux set -g status-interval 1
