#!/bin/bash
#
# Verify that critical bug fixes are in place
#

echo "=========================================="
echo "FreeType 1 Pascal Adapter - Code Verification"
echo "=========================================="
echo ""

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

ADAPTER_FILE="../../corelib/render/software/agg_font_freetype_pas.pas"
PASS_COUNT=0
FAIL_COUNT=0

check_fix() {
    local description="$1"
    local pattern="$2"

    if grep -q "$pattern" "$ADAPTER_FILE"; then
        echo -e "${GREEN}✓${NC} $description"
        ((PASS_COUNT++))
        return 0
    else
        echo -e "${RED}✗${NC} $description"
        ((FAIL_COUNT++))
        return 1
    fi
}

echo "Checking for critical bug fixes:"
echo ""

# Check 1: FT_Set_Char_Size uses CharSizes (plural)
check_fix "FT_Set_Char_Size uses TT_Set_Instance_CharSizes" "TT_Set_Instance_CharSizes(face\^.tt_instance, char_width, char_height)"

# Check 2: FT_Set_Pixel_Sizes passes width correctly
check_fix "FT_Set_Pixel_Sizes passes width (not height twice)" "TT_Set_Instance_PixelSizes(face\^.tt_instance, pixel_width,"

# Check 3: Outline copying uses field-by-field copy
check_fix "Outline points copied field-by-field" "face\^.glyph\^.outline.points\[i\].x := outline.points\^"

echo ""
echo "=========================================="
echo "Results: $PASS_COUNT passed, $FAIL_COUNT failed"
echo "=========================================="
echo ""

if [ $FAIL_COUNT -gt 0 ]; then
    echo -e "${RED}ERROR: Some critical fixes are missing!${NC}"
    echo ""
    echo "You need to pull the latest code:"
    echo "  git pull"
    echo ""
    echo "Current git status:"
    git log --oneline -5
    exit 1
else
    echo -e "${GREEN}✓ All critical fixes are in place!${NC}"
    echo ""
    echo "You can now run:"
    echo "  ./rebuild_tests.sh"
    echo "  ./run_tests.sh"
    exit 0
fi
