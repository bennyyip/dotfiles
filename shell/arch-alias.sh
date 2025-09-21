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

G() {
  git clone https://gitlab.archlinux.org/archlinux/packaging/packages/$1.git
}

# alias Ge="G packages core/extra"
# alias Gc="G community community"
#
# function Ga() { # 獲取PKGBUILD {{{2
#   [ -z "$1" ] && echo "usage: Ga <aur package name>: get AUR package PKGBUILD" && return 1
#   git clone aur@aur.archlinux.org:"$1".git
#   rm -rf "$1"/.git
# }
#
# function G() {
#   [ -z "$3" ] && echo "usage: $0 <$2 package name>: get $2 package PKGBUILD" && return 1
#   git clone https://git.archlinux.org/svntogit/$1.git/ -b packages/$3 --single-branch $3
#   mv "$3"/trunk/* "$3"
#   rm -rf "$3"/{repos,trunk,.git}
# }



