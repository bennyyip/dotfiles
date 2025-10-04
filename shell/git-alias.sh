g() {
  if [ $# = 0 ]; then
    git status -sb .
  else
    # shellcheck disable=all
    git $*
  fi
}

alias gst='git status -sb'
alias gss='git stash'

alias gci='git commit'
alias gcm='git commit -m'
alias gca='git commit --amend'
alias gcf='git commit --fixup'
alias gcam='git commit -a -m'
alias gcan!='git commit --verbose --all --no-edit --amend'

alias gp='git push'
alias gpl='git pull --rebase --autostash'
alias grv='git remote --verbose'
alias gop='git open'

alias gl='git log --graph --pretty="format:%C(red)%h%Creset %C(yellow)%G?%Creset%C(auto)%d%Creset %s %Cgreen(%cd) %C(bold blue)<%aN>%Creset" --date=relative'
alias gll='git log --pretty="format:%C(red)%h%Creset %C(yellow)%G?%Creset%C(auto)%d%Creset %s %Cgreen(%cd) %C(bold blue)<%aN>%Creset" --date=relative'
alias gL='gl --stat'
alias glg=gl

gfk() {
  local fork_url
  fork_url=$(git remote get-url origin | awk -F '/' '{printf "git@github.com:bennyyip/%s", $NF}')
  git remote add fork "$fork_url"
}
