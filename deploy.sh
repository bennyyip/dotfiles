#!/bin/bash

set -e

# Native symlink on Windows
export MSYS=winsymlinks:nativestrict

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

gitClone() {
url="$1"
destination="$HOME/$2"
  if [ -e "$destination" ]; then
    echo "[WARNING] $destination exists."
    return
  fi
  git clone --depth=1 "$url" "$destination"
}

symlinkFile() {
  filename="$SCRIPT_DIR/$1"
  destination="$HOME/$2"

  if [[ ! -e $filename ]]; then
    echo "[ERROR] $filename doesn't exists."
    exit 1
  fi

  if [ -L "$destination" ]; then
    actual=$(realpath "$destination")
    if [[ $actual != "$filename" ]]; then
      echo "[WARNING] $destination already symlinked to $actual"
    fi
    return
  fi

  if [ -e "$destination" ]; then
    echo "[ERROR] $destination exists but it's not a symlink. Please fix that manually."
    exit 1
  fi

  mkdir -p "$(dirname "$destination")"
  ln -s "$filename" "$destination"
  echo "[OK] $filename -> $destination"
}

deployManifest() {
  while read -r row; do
    if [[ $row =~ ^#.* ]] || [[ -z $row ]]; then
      continue
    fi

    filename=$(echo "$row" | cut -d \| -f 1)
    operation=$(echo "$row" | cut -d \| -f 2)
    destination=$(echo "$row" | cut -d \| -f 3)

    if [[ -z $destination ]]; then
      destination=".${filename}"
    fi

    case $operation in
      symlink)
        symlinkFile "$filename" "$destination"
        ;;
      cp)
        mkdir -p "$(dirname "$destination")"
        cp "$filename" "$destination"
        ;;
      git)
        gitClone "$filename" "$destination"
        ;;

      *)
        echo "[WARNING] Unknown operation $operation. Skipping..."
        ;;
    esac
  done < "$SCRIPT_DIR/$1"
}

if [[ $# -eq 0 ]]; then
  echo "Usage: $0 <MANIFEST>"
  echo "ERROR: no MANIFEST file is provided"
  exit 1
fi

deployManifest "$1"
