#!/bin/bash

# updater for Windows

set -e

# REPO="shinchiro/mpv-winbuild-cmake"
REPO="zhongfly/mpv-winbuild"
INSTALL_PATH="$HOME/mpv"
_7Z_EXE='C:/Program Files/7-Zip/7z.exe'

if tasklist | grep -q -i mpv.exe; then
  echo "Please exit mpv first."
  exit
fi

DOWNLOAD_DIR=$(mktemp -d)
# trap "rm -rf $DOWNLOAD_DIR" EXIT  # Clean up on exit

echo "Fetching latest release information for $REPO..."

ARCHIVE_URL=$(curl -s "https://api.github.com/repos/$REPO/releases/latest" | \
    jq -r '.assets[] | select(.name | startswith("mpv-x86_64-v3")) | .browser_download_url' | head -1)

if [ -z "$ARCHIVE_URL" ]; then
    echo "Error: Could not find .exe file in latest release"
    exit 1
fi

ARCHIVE_FILENAME=$(basename "$ARCHIVE_URL")

echo "Found: $ARCHIVE_FILENAME"
echo "Downloading to: $DOWNLOAD_DIR/"

echo "$ARCHIVE_URL"

curl -L -o "$DOWNLOAD_DIR/$ARCHIVE_FILENAME" "$ARCHIVE_URL"

echo "✓ Download complete: $DOWNLOAD_DIR/$ARCHIVE_FILENAME"

"$_7Z_EXE" x -y "$DOWNLOAD_DIR/$ARCHIVE_FILENAME" "-o$INSTALL_PATH"
