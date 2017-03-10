#
# ~/.bashrc
#

export GOPATH=/home/ben/go

[[ $- != *i* ]] && return


alias ls='ls --color=auto'

alias ll='ls -l'
alias la='ls -la'
alias lh='ls -lh'
alias grep='grep --color'

alias start="sudo systemctl start"
alias stop="sudo systemctl stop"
alias restart="sudo systemctl restart"
alias .="source"
alias cp="cp -i --reflink=auto"
alias ssh="TERM=xterm-256color ssh"
alias bc="bc -l"

alias gtar="tar -Ipigz czfv"
alias btar="tar -Ilbzip3 cjfv"
alias 7tar="7z a -mmt" 
alias xcp="rsync -aviHAXKhP --delete --exclude='*~' --exclude=__pycache__"
alias tmux="tmux -2"

# pacman aliases and functions
function Syu(){
    sudo pacman -Sy && sudo powerpill -Suw $@ && sudo pacman -Su $@ 
    pacman -Qtdq | ifne sudo pacaur -Rcs -
    vim +PlugUpgrade +PlugUpdate 
}

alias Rcs="sudo pacman -Rcs"
alias Ss="pacman -Ss"
alias Si="pacman -Si"
alias Qs="pacman -Qs"
alias Qi="pacman -Qi"
alias Qo="pacman -Qo"
alias Ql="pacman -Ql"
alias Fo="pacman -Fo"
alias Fy="sudo pacman -Fy" 
alias Ssa="pacaur -Ssa"


alias urldecode='python2 -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])"'
alias urlencode='python2 -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])"'

alias pvim="curl -F 'vimcn=<-' https://cfp.vim-cn.com/"
imgvim(){
    curl -F "name=@$1" https://img.vim-cn.com/
}


dsf(){
    # depends on diff-so-fancy
    git diff --color=always $@ | diff-so-fancy | less
}

man() {
	env \
		LESS_TERMCAP_mb=$(printf "\e[1;37m") \
		LESS_TERMCAP_md=$(printf "\e[1;37m") \
		LESS_TERMCAP_me=$(printf "\e[0m") \
		LESS_TERMCAP_se=$(printf "\e[0m") \
		LESS_TERMCAP_so=$(printf "\e[1;47;30m") \
		LESS_TERMCAP_ue=$(printf "\e[0m") \
		LESS_TERMCAP_us=$(printf "\e[0;36m") \
			man "$@"
}

alias which='(alias; declare -f) | /usr/bin/which --tty-only --read-alias --read-functions --show-tilde --show-dot'
alias vi=vim
export VISUAL=vim
export EDITOR=vim

alias clip="xsel --clipboard"
alias py=python
alias ipy=ipython

[[ -f ~/.extend.bashrc ]] && . ~/.extend.bashrc
