#!/usr/bin/env bash

if ! ls vec0* 1>/dev/null 2>&1; then
  echo "Extension sqlite-vec not found. Downloading..."
  curl -L -o install.sh https://github.com/muazzam0x48/sqlite-vec/releases/download/latest/install.sh
  chmod +x install.sh
  ./install.sh
  rm install.sh
fi
