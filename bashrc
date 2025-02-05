# Functions
# shellcheck disable=SC1090
source ~/.shell/functions.sh

# Allow local customizations in the ~/.shell_local_before file
if [ -f ~/.shell_local_before ]; then
  source ~/.shell_local_before
fi

# Allow local customizations in the ~/.bashrc_local_before file
if [ -f ~/.bashrc_local_before ]; then
  source ~/.bashrc_local_before
fi

# Settings
source ~/.bash/settings.bash

# External settings
source ~/.shell/external.sh

# Bootstrap
source ~/.shell/bootstrap.sh

# If not running interactively, don't do anything
case $- in
*i*) ;;
*) return ;;
esac

# Aliases
source ~/.shell/alias.sh
alias which='(alias; declare -f) | /usr/bin/which --tty-only --read-alias --read-functions --show-tilde --show-dot'

# Bash Functions
source ~/.bash/functions.bash

# Custom prompt
source ~/.bash/prompt.bash

# Plugins
source ~/.bash/plugins.bash

# Allow local customizations in the ~/.shell_local_after file
if [ -f ~/.shell_local_after ]; then
  source ~/.shell_local_after
fi

# Allow local customizations in the ~/.bashrc_local_after file
if [ -f ~/.bashrc_local_after ]; then
  source ~/.bashrc_local_after
fi

# Allow private customizations (not checked in to version control)
if [ -f ~/.shell_private ]; then
  source ~/.shell_private
fi
