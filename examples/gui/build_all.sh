#!/bin/bash
#
# Build script for all GUI examples (PARALLEL VERSION)
# Finds and compiles all .lpr projects in the examples/gui directory
#

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Track build results
declare -a BUILD_SUCCESS
declare -a BUILD_FAILED
declare -A BUILD_PIDS
declare -a PROJECT_NAMES

# Function to print colored messages
print_header() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_info() {
    echo -e "${YELLOW}→ $1${NC}"
}

# Function to build a project (returns exit code)
build_project() {
    local dir=$1
    local project=$2
    local logfile=$3

    if cd "$dir" 2>/dev/null; then
        # Ensure units directory exists
        if [ ! -d units ]; then
            mkdir units
        fi

        # Build the project
        if fpc @extrafpc.cfg "$project" > "$logfile" 2>&1; then
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
    local project=$3
    local logfile=$4

    # Build in background and store PID
    (build_project "$dir" "$project" "$logfile"; exit $?) &
    BUILD_PIDS["$name"]=$!
}

# Save starting directory
START_DIR=$(pwd)

print_header "GUI Examples Parallel Build - Starting"
echo "Build started at: $(date)"
echo ""

print_info "Discovering projects..."
echo ""

# Find all .lpr files and start parallel builds
for f in $(find ./ -name '*.lpr' | sort); do
    DIR=$(dirname "$f")
    PROJECT=$(basename "$f")
    LPRNAME=$(basename "$PROJECT" .lpr)
    LOGNAME=$(echo "$LPRNAME" | tr '[:upper:]' '[:lower:]')
    LOGFILE="/tmp/fpgui_example_${LOGNAME}.log"

    PROJECT_NAMES+=("$LPRNAME")

    print_info "Queuing: $LPRNAME (in $DIR)"

    # Start build in background
    build_project_async "$LPRNAME" "$DIR" "$PROJECT" "$LOGFILE"
done

echo ""
print_info "Waiting for parallel builds to complete..."
echo ""

# Wait for all background jobs and collect results
for name in "${PROJECT_NAMES[@]}"; do
    pid=${BUILD_PIDS[$name]}
    logname=$(echo "$name" | tr '[:upper:]' '[:lower:]')
    logfile="/tmp/fpgui_example_${logname}.log"

    if wait $pid; then
        print_success "$name"
        BUILD_SUCCESS+=("$name")
    else
        print_error "$name (see $logfile)"
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
        logname=$(echo "$project" | tr '[:upper:]' '[:lower:]')
        print_error "$project - /tmp/fpgui_example_${logname}.log"
    done
    echo ""
    echo -e "${YELLOW}Check log files in /tmp/fpgui_example_*.log for details${NC}"
    echo ""
fi

# Final status
echo "Build completed at: $(date)"
echo ""

if [ ${#BUILD_FAILED[@]} -eq 0 ]; then
    print_success "All examples built successfully!"
    exit 0
else
    print_error "Some examples failed to build"
    exit 1
fi
