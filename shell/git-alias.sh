g() { [[ $# = 0 ]] && git status -sb . || git $*; }

alias gst='git status -sb'
alias gss='git stash'

alias gci='git commit'
alias gcm='git commit -m'
alias gca='git commit --amend'
alias gcan!='git commit --verbose --all --no-edit --amend'

alias gp='git push'
alias gpl='git pull --rebase --autostash'
alias grv='git remote --verbose'
alias gop='git open'

alias gl='git log --graph --pretty="format:%C(red)%h%Creset %C(yellow)%G?%Creset%C(auto)%d%Creset %s %Cgreen(%cd) %C(bold blue)<%aN>%Creset" --date=relative'
alias gll='git log --pretty="format:%C(red)%h%Creset %C(yellow)%G?%Creset%C(auto)%d%Creset %s %Cgreen(%cd) %C(bold blue)<%aN>%Creset" --date=relative'
alias gL='gl --stat'
alias glg=gl
