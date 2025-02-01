alias Ss="apt search"
alias Ql="dpkg-query -L"
alias Qi="dpkg-query -s"
alias pain="sudo apt install"
# sudo update-alternatives --install /usr/bin/fd fd /usr/bin/fdfind  1
if exists fdfind; then
  alias fd="fdfind"
fi
