#!/bin/bash
#
# Build script for fpGUI framework and critical applications
# This script builds:
#   1. fpGUI framework (AggCanvas backend)
#   2. uidesigner
#   3. docview
#   4. maximus (example IDE)
#

set -e  # Exit on error (but we'll trap errors to continue)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BRIGHT_YELLOW='\033[0;93m'
BRIGHT_BLUE='\033[0;94m'
NC='\033[0m' # No Color

# Track build results
declare -a BUILD_SUCCESS
declare -a BUILD_FAILED

# Function to print colored messages
print_header() {
    echo -e "${BRIGHT_BLUE}========================================${NC}"
    echo -e "${BRIGHT_BLUE}$1${NC}"
    echo -e "${BRIGHT_BLUE}========================================${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_info() {
    echo -e "${BRIGHT_YELLOW}→ $1${NC}"
}

# Function to build a project
build_project() {
    local name=$1
    local dir=$2
    local cmd=$3
    local logname=$(echo "$name" | tr ' ' '_' | tr '[:upper:]' '[:lower:]' | tr -d '()')

    print_header "Building $name"
    print_info "Directory: $dir"
    print_info "Command: $cmd"

    if cd "$dir" 2>/dev/null; then
        if eval "$cmd" > "/tmp/fpgui_build_${logname}.log" 2>&1; then
            print_success "$name built successfully"
            BUILD_SUCCESS+=("$name")
            cd - > /dev/null
            return 0
        else
            print_error "$name build FAILED (see /tmp/fpgui_build_${logname}.log)"
            BUILD_FAILED+=("$name")
            cd - > /dev/null
            return 1
        fi
    else
        print_error "Directory $dir not found"
        BUILD_FAILED+=("$name")
        return 1
    fi
}

# Save current directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

print_header "fpGUI Build All - Starting"
echo "Build started at: $(date)"
echo ""

# Build 1: fpGUI Framework (AggCanvas)
build_project "fpGUI Framework (AggCanvas)" \
              "$SCRIPT_DIR/src" \
              "./build.sh 1"

echo ""

# Build 2: uidesigner
build_project "uidesigner" \
              "$SCRIPT_DIR/uidesigner" \
              "fpc @extrafpc.cfg uidesigner.lpr"

echo ""

# Build 3: docview
build_project "docview" \
              "$SCRIPT_DIR/docview/src" \
              "fpc @extrafpc.cfg docview.lpr"

echo ""

# Build 4: maximus (example IDE)
build_project "maximus" \
              "$SCRIPT_DIR/examples/apps/ide/src" \
              "fpc @extrafpc.cfg maximus.lpr"

echo ""

# Print summary
print_header "Build Summary"
echo ""

if [ ${#BUILD_SUCCESS[@]} -gt 0 ]; then
    echo -e "${GREEN}Successful builds (${#BUILD_SUCCESS[@]}):${NC}"
    for project in "${BUILD_SUCCESS[@]}"; do
        print_success "$project"
    done
    echo ""
fi

if [ ${#BUILD_FAILED[@]} -gt 0 ]; then
    echo -e "${RED}Failed builds (${#BUILD_FAILED[@]}):${NC}"
    for project in "${BUILD_FAILED[@]}"; do
        print_error "$project"
    done
    echo ""
    echo -e "${YELLOW}Check log files in /tmp/fpgui_build_*.log for details${NC}"
    echo ""
fi

# Final status
echo "Build completed at: $(date)"
echo ""

if [ ${#BUILD_FAILED[@]} -eq 0 ]; then
    print_success "All projects built successfully!"
    exit 0
else
    print_error "Some projects failed to build"
    exit 1
fi
