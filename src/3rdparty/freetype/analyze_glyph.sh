#!/bin/bash
# Extract which instructions are used by problematic glyphs
echo "Analyzing problematic glyphs..."
grep -A 3 "glyph_index=90\|glyph_index=91\|glyph_index=86\|glyph_index=89\|glyph_index=92\|glyph_index=23" /tmp/test_coords.txt | grep "codeSize" | sort | uniq
