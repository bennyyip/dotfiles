#!/bin/sh

# cache ip and host

status_host=$([ -f $HOME/.name ] && cat $HOME/.name || hostname || cat /etc/hostname)
status_ip=$(ip -o -4 addr | awk -F 'inet |/' '!/127.0.0.1/ {print $2}' | sort -n | head -n 1)

tmux set -g status-justify left
tmux set -g status-position bottom
tmux set -g status-left '#[bold](#S)#[nobold] '
tmux set -g status-style "none,bg=#374231,fg=#E6E4D9"
tmux set -g status-right-length 50
tmux set -g status-left-length 20
tmux set-option -g status-right "${status_host}  ${status_ip}  %A #[bold] %H:%M:%S "

tmux set -g window-status-style "none"
tmux set -g window-status-format "#{?window_activity_flag,#[fg=#BC5215],#[none]} #I #W "
tmux set -g window-status-current-format "#[reverse,bold] #F #W "
tmux set -g window-status-separator ""
tmux set -g window-status-activity-style 'none'
