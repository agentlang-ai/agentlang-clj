#!/bin/bash
set -e  # Exit on error

# Get the current working directory
CURRENT_DIR="$(pwd)"
LIB_DIR="$CURRENT_DIR/lib"  # Use absolute path for lib directory
BUILD_DIR="$(mktemp -d)"

echo "Current directory: $CURRENT_DIR"
echo "Creating lib directory: $LIB_DIR"

# Create lib directory with proper permissions
mkdir -p "$LIB_DIR"
chmod 755 "$LIB_DIR"

VERSION="0.1.6"
DOWNLOAD_URL="https://github.com/asg017/sqlite-vec/releases/download/v${VERSION}/sqlite-vec-${VERSION}-amalgamation.tar.gz"

echo "Downloading vec extension from $DOWNLOAD_URL"

# Download with curl and check HTTP status
HTTP_RESPONSE=$(curl -L -w "%{http_code}" -o "$BUILD_DIR/vec.tar.gz" "$DOWNLOAD_URL")
if [ "$HTTP_RESPONSE" != "200" ]; then
    echo "Failed to download: HTTP status $HTTP_RESPONSE"
    rm -rf "$BUILD_DIR"
    exit 1
fi

# Extract and compile
cd "$BUILD_DIR"
tar xf vec.tar.gz  # This handles both gzip and xz formats 【1】

# Find the extracted directory
EXTRACTED_DIR=$(find . -type d -name "sqlite-vec*" -maxdepth 1)
if [ -z "$EXTRACTED_DIR" ]; then
    echo "Could not find extracted directory, using current directory"
    EXTRACTED_DIR="."
fi

cd "$EXTRACTED_DIR"

# Compile the extension
echo "Compiling vec extension..."
gcc -fPIC -shared sqlite-vec.c -o vec0.so

# Copy the compiled extension to lib directory using absolute paths
if [ -f "vec0.so" ]; then
    echo "Copying vec0.so to $LIB_DIR/"
    cp vec0.so "$LIB_DIR/"
    chmod 755 "$LIB_DIR/vec0.so"
    echo "Extension compiled and installed to: $LIB_DIR/vec0.so"
    ls -l "$LIB_DIR/vec0.so"
else
    echo "Compilation failed - vec0.so not found"
    exit 1
fi

# Cleanup
cd "$CURRENT_DIR"
rm -rf "$BUILD_DIR"
