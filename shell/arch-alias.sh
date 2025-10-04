# pacman aliases
Syu() {
  sudo pacman -Syu
  pacman -Qtdq | ifne sudo pacman -Rcs -
}

alias Rcs="sudo pacman -Rcs"
alias Ss="pacman -Ss"
alias Si="pacman -Si"
alias Qs="pacman -Qs"
alias Qi="pacman -Qi"
alias Qo="pacman -Qo"
alias Ql="pacman -Ql"
alias Fo="pacfiles -F"
alias Fy="sudo pacman -Fy"
alias Ssa="pacaur -Ssa"
alias pain='sudo pacman -S --needed'
alias painn='sudo pacman -S'
alias cower='cower --domain aur.tuna.tsinghua.edu.cn'

paclist() {
  # Source: https://bbs.archlinux.org/viewtopic.php?id=93683
  LC_ALL=C pacman -Qei $(pacman -Qu | cut -d " " -f 1) | \
    awk 'BEGIN {FS=":"} /^Name/{printf("\033[1;36m%s\033[1;37m", $2)} /^Description/{print $2}'
}

# use paru -G for aur packages
G() {
  git clone "https://gitlab.archlinux.org/archlinux/packaging/packages/$1.git"
}
