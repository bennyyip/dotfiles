#!/bin/sh

# cache ip and host

status_host=$([ -f $HOME/.name ] && cat $HOME/.name || hostname || cat /etc/hostname)
status_ip=$(ip -o -4 addr | awk -F 'inet |/' '!/127.0.0.1/ {print $2}' | sort -n | head -n 1)

tmux set -g status-justify left
tmux set -g status-position bottom
tmux set -g status-left '#[bold](#S)#[nobold] '
tmux set -g status-style "none,bg=#32302f"
tmux set -g status-right-length 50
tmux set -g status-left-length 20
tmux set-option -g status-right "#[fg=#32302f,bg=#32302f]#[fg=#ddc7a1,bg=#32302f] ${status_host} #[fg=#32302f,bg=#5a524c]#[fg=#ddc7a1,bg=#5a524c] ${status_ip} #[fg=#5a524c,bg=#a89984]#[fg=#282828,bg=#a89984,bold] %H:%M:%S %A"

tmux set -g window-status-style "none,fg=#ddc7a1,bg=#32302f"
tmux set -g window-status-format "#[fg=#32302f]#{?window_activity_flag,#[bg=#76946a],#[bg=#a89984]} #I #W #{?window_activity_flag,#[fg=#76946a],#[fg=#a89984]}#[bg=#32302f]"
tmux set -g window-status-current-format "#[fg=#32302f,bg=#dcd7ba]#[fg=#1c1c1c,bg=#dcd7ba bold] #F #W #[fg=#dcd7ba,bg=#32302f]"
tmux set -g window-status-separator ""
tmux set -g window-status-activity-style 'none'
