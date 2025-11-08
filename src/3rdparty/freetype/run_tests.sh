#!/bin/bash
#
# Run all FreeType 1 Pascal tests
# This script runs the unit tests and both rendering tests
#

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default font file
FONT_FILE="${1:-/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf}"
FONT_SIZE="${2:-24}"

# Function to print status
print_status() {
    echo -e "${GREEN}==>${NC} $1"
}

print_error() {
    echo -e "${RED}ERROR:${NC} $1"
}

print_section() {
    echo ""
    echo -e "${BLUE}=========================================="
    echo -e "$1"
    echo -e "==========================================${NC}"
    echo ""
}

# Check if executables exist
if [ ! -f "test_adapter" ] || [ ! -f "test_ft1" ] || [ ! -f "test_ft2" ]; then
    print_error "Test programs not found. Please run ./rebuild_tests.sh first"
    exit 1
fi

# Check if font file exists
if [ ! -f "$FONT_FILE" ]; then
    print_error "Font file not found: $FONT_FILE"
    echo ""
    echo "Usage: $0 [font_file.ttf] [font_size]"
    echo ""
    echo "Examples:"
    echo "  $0"
    echo "  $0 /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf 24"
    echo "  $0 /usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf 32"
    exit 1
fi

echo ""
echo "=========================================="
echo "FreeType 1 Pascal Test Suite"
echo "=========================================="
echo ""
echo "Font file: $FONT_FILE"
echo "Font size: $FONT_SIZE pt"
echo ""

# Test 1: Run adapter unit tests
print_section "Test 1: Adapter Unit Tests"
./test_adapter "$FONT_FILE" || true  # Don't exit on test failures, just log them
print_status "Unit tests completed (some tests may have failed - check output above)"

# Test 2: Run FreeType 1 rendering test
print_section "Test 2: FreeType 1 Pascal Rendering"
if ./test_ft1 "$FONT_FILE" "$FONT_SIZE"; then
    print_status "FreeType 1 test completed"
    if [ -f "output_ft1.png" ]; then
        FILE_SIZE=$(du -h output_ft1.png | cut -f1)
        echo "   Output: output_ft1.png ($FILE_SIZE)"
    fi
else
    print_error "FreeType 1 test failed!"
    exit 1
fi

# Test 3: Run FreeType 2 rendering test
print_section "Test 3: FreeType 2 Library Rendering"
if ./test_ft2 "$FONT_FILE" "$FONT_SIZE"; then
    print_status "FreeType 2 test completed"
    if [ -f "output_ft2.png" ]; then
        FILE_SIZE=$(du -h output_ft2.png | cut -f1)
        echo "   Output: output_ft2.png ($FILE_SIZE)"
    fi
else
    print_error "FreeType 2 test failed!"
    exit 1
fi

# Compare results
print_section "Test Results Summary"

if [ -f "output_ft1.png" ] && [ -f "output_ft2.png" ]; then
    FT1_SIZE=$(wc -c < output_ft1.png)
    FT2_SIZE=$(wc -c < output_ft2.png)

    echo "Output files generated:"
    echo "  output_ft1.png: $(du -h output_ft1.png | cut -f1)"
    echo "  output_ft2.png: $(du -h output_ft2.png | cut -f1)"
    echo ""

    # Check if FT1 output is suspiciously small (likely blank)
    if [ "$FT1_SIZE" -lt 5000 ]; then
        print_error "FT1 output appears to be blank (file size too small)"
        echo "   This suggests the rendering failed silently."
        echo "   Expected: ~20-50 KB, Got: $FT1_SIZE bytes"
        echo ""
    fi

    # Check if outputs are similar in size (within 50%)
    SIZE_DIFF=$((FT1_SIZE > FT2_SIZE ? FT1_SIZE - FT2_SIZE : FT2_SIZE - FT1_SIZE))
    SIZE_AVG=$(((FT1_SIZE + FT2_SIZE) / 2))
    PERCENT_DIFF=$((SIZE_DIFF * 100 / SIZE_AVG))

    if [ "$PERCENT_DIFF" -lt 50 ]; then
        echo -e "${GREEN}✓${NC} Output file sizes are similar (within 50%)"
        echo "  This suggests both implementations rendered text successfully."
    else
        print_warning "Output file sizes differ significantly ($PERCENT_DIFF% difference)"
        echo "  This may indicate different rendering results."
    fi
    echo ""

    echo "To compare visually:"
    echo "  feh output_ft1.png output_ft2.png"
    echo "  # or"
    echo "  display output_ft1.png &"
    echo "  display output_ft2.png &"
    echo ""
fi

print_status "All tests completed!"
echo ""
