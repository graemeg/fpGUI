#!/bin/bash
#
# Build script for fpGUI framework and critical applications (PARALLEL VERSION)
# This script builds:
#   1. fpGUI framework (AggCanvas backend) - sequential
#   2. uidesigner, docview, maximus - in parallel
#

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BRIGHT_YELLOW='\033[0;93m'
BRIGHT_BLUE='\033[0;94m'
NC='\033[0m' # No Color

# Track build results
declare -a BUILD_SUCCESS
declare -a BUILD_FAILED
declare -A BUILD_PIDS

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

# Function to build a project (returns exit code via return, name via global arrays)
build_project() {
    local name=$1
    local dir=$2
    local cmd=$3
    local logname=$(echo "$name" | tr ' ' '_' | tr '[:upper:]' '[:lower:]' | tr -d '()')

    if cd "$dir" 2>/dev/null; then
        if eval "$cmd" > "/tmp/fpgui_build_${logname}.log" 2>&1; then
            cd - > /dev/null
            return 0
        else
            cd - > /dev/null
            return 1
        fi
    else
        return 1
    fi
}

# Function to build a project in background
build_project_async() {
    local name=$1
    local dir=$2
    local cmd=$3

    print_header "Starting build: $name"
    print_info "Directory: $dir"
    print_info "Command: $cmd"

    # Build in background and store PID
    (build_project "$name" "$dir" "$cmd"; exit $?) &
    BUILD_PIDS["$name"]=$!
}

# Save current directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

print_header "fpGUI Parallel Build - Starting"
echo "Build started at: $(date)"
echo ""

# Build 1: fpGUI Framework (AggCanvas) - MUST be sequential
print_header "Building fpGUI Framework (AggCanvas)"
print_info "Directory: $SCRIPT_DIR/src"
print_info "Command: ./build.sh 1"

if build_project "fpGUI Framework (AggCanvas)" "$SCRIPT_DIR/src" "./build.sh 1"; then
    print_success "fpGUI Framework (AggCanvas) built successfully"
    BUILD_SUCCESS+=("fpGUI Framework (AggCanvas)")
else
    print_error "fpGUI Framework (AggCanvas) build FAILED (see /tmp/fpgui_build_fpgui_framework_aggcanvas.log)"
    BUILD_FAILED+=("fpGUI Framework (AggCanvas)")

    # If framework fails, don't bother building apps
    print_header "Build Summary"
    echo ""
    echo -e "${RED}Framework build failed - aborting remaining builds${NC}"
    echo -e "${YELLOW}Check log file: /tmp/fpgui_build_fpgui_framework_aggcanvas.log${NC}"
    exit 1
fi

echo ""
print_header "Building applications in parallel"
echo ""

# Build applications in parallel
build_project_async "uidesigner" \
                    "$SCRIPT_DIR/uidesigner" \
                    "fpc @extrafpc.cfg uidesigner.lpr"

build_project_async "docview" \
                    "$SCRIPT_DIR/docview/src" \
                    "fpc @extrafpc.cfg docview.lpr"

build_project_async "maximus" \
                    "$SCRIPT_DIR/examples/apps/ide/src" \
                    "fpc @extrafpc.cfg maximus.lpr"

echo ""
print_info "Waiting for parallel builds to complete..."
echo ""

# Wait for all background jobs and collect results
for name in "${!BUILD_PIDS[@]}"; do
    pid=${BUILD_PIDS[$name]}
    if wait $pid; then
        print_success "$name built successfully"
        BUILD_SUCCESS+=("$name")
    else
        logname=$(echo "$name" | tr ' ' '_' | tr '[:upper:]' '[:lower:]' | tr -d '()')
        print_error "$name build FAILED (see /tmp/fpgui_build_${logname}.log)"
        BUILD_FAILED+=("$name")
    fi
done

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
