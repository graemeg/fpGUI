#!/bin/bash
#
# Clean rebuild script for FreeType 1 Pascal test programs
# This ensures all units are recompiled from scratch
#

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "=========================================="
echo "FreeType 1 Pascal Test Programs - Rebuild"
echo "=========================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print status
print_status() {
    echo -e "${GREEN}==>${NC} $1"
}

print_error() {
    echo -e "${RED}ERROR:${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}WARNING:${NC} $1"
}

# Step 1: Clean old compiled units
print_status "Cleaning old compiled units..."
if [ -d "units" ]; then
    rm -rf units
    echo "   Removed units/ directory"
fi

# Recreate units directory
mkdir -p units
echo "   Created fresh units/ directory"
echo ""

# Step 2: Compile test_adapter
print_status "Compiling test_adapter.pas..."
if fpc -FUunits \
    -Fu../../corelib/render/software/ \
    -Fi../../corelib/render/software/ \
    test_adapter.pas 2>&1 | grep -v "^Free Pascal" | grep -v "^Copyright" | grep -v "^Target OS" | grep -v "^Compiling" | grep -v "^Linking"; then
    echo -e "   ${GREEN}✓${NC} test_adapter compiled successfully"
else
    print_error "Failed to compile test_adapter.pas"
    exit 1
fi
echo ""

# Step 3: Compile test_ft1
print_status "Compiling test_ft1.pas (FreeType 1 Pascal)..."
if fpc -FUunits \
    -Fu../../corelib/render/software/ \
    -Fi../../corelib/render/software/ \
    test_ft1.pas 2>&1 | grep -v "^Free Pascal" | grep -v "^Copyright" | grep -v "^Target OS" | grep -v "^Compiling" | grep -v "^Linking"; then
    echo -e "   ${GREEN}✓${NC} test_ft1 compiled successfully"
else
    print_error "Failed to compile test_ft1.pas"
    exit 1
fi
echo ""

# Step 4: Compile test_ft2
print_status "Compiling test_ft2.pas (FreeType 2 Library)..."
if fpc -FUunits \
    -Fu../../corelib/render/software/ \
    -Fi../../corelib/render/software/ \
    test_ft2.pas 2>&1 | grep -v "^Free Pascal" | grep -v "^Copyright" | grep -v "^Target OS" | grep -v "^Compiling" | grep -v "^Linking"; then
    echo -e "   ${GREEN}✓${NC} test_ft2 compiled successfully"
else
    print_error "Failed to compile test_ft2.pas"
    exit 1
fi
echo ""

# Step 5: Show what was built
print_status "Build complete! Generated executables:"
if [ -f "test_adapter" ]; then
    echo -e "   ${GREEN}✓${NC} test_adapter  - Unit tests for FT2-to-FT1 adapter"
fi
if [ -f "test_ft1" ]; then
    echo -e "   ${GREEN}✓${NC} test_ft1      - FreeType 1 Pascal rendering test"
fi
if [ -f "test_ft2" ]; then
    echo -e "   ${GREEN}✓${NC} test_ft2      - FreeType 2 library rendering test"
fi
echo ""

# Step 6: Show usage
print_status "Usage examples:"
echo ""
echo "  Run adapter unit tests:"
echo "    ./test_adapter /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
echo ""
echo "  Run FreeType 1 rendering test:"
echo "    ./test_ft1 /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf 24"
echo ""
echo "  Run FreeType 2 rendering test:"
echo "    ./test_ft2 /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf 24"
echo ""
echo "  Compare outputs:"
echo "    ./test_ft1 /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf 24"
echo "    ./test_ft2 /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf 24"
echo "    # Then view output_ft1.png and output_ft2.png"
echo ""

print_status "All tests compiled successfully!"
echo ""
