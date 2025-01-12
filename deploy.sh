#!/bin/bash

set -e

export MSYS=winsymlinks:nativestrict

SCRIPT_DIR="$(cd "$(dirname "$BASH_SOURCE[0]")" && pwd)"

symlinkFile() {
  filename="$SCRIPT_DIR/$1"
  destination="$HOME/$2"

  mkdir -p $(dirname "$destination")

  if [ -L "$destination" ]; then
    echo "[WARNING] $filename already symlinked to $destination"
    return
  fi

  if [ -e "$destination" ]; then
    echo "[ERROR] $destination exists but it's not a symlink. Please fix that manually"
    exit 1
  fi

  ln -s "$filename" "$destination"
  echo "[OK] $filename -> $destination"
}

deployManifest() {
  while read row; do
    if [[ $row =~ ^#.*  ]] || [[ -z $row ]]; then
      continue
    fi

    filename=$(echo $row | cut -d \| -f 1)
    operation=$(echo $row | cut -d \| -f 2)
    destination=$(echo $row | cut -d \| -f 3)

    if [[ -z $destination ]]; then
      destination=".${filename}"
    fi

    case $operation in
      symlink)
        symlinkFile $filename $destination
        ;;

      *)
        echo "[WARNING] Unknown operation $operation. Skipping..."
        ;;
    esac
  done < "$SCRIPT_DIR/$1"
}

if [ -z "$@" ]; then
  echo "Usage: $0 <MANIFEST>"
  echo "ERROR: no MANIFEST file is provided"
  exit 1
fi

deployManifest $1
