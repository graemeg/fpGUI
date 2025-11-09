# TrueType Hinting Bug - Debug Assistance Request

## Context

I'm working on a FreeType 1 Pascal port for the fpGUI project. The code is a Pascal implementation of FreeType 1 with an FT2 API adapter layer on top. The port is based on the Lazarus FreeType1 Pascal implementation.

**Location**: `/home/user/fpGUI`

**You have access to**:
- The full codebase on my local filesystem
- Free Pascal Compiler (fpc) - you can compile and run test programs
- The ability to view generated PNG images

## The Bug

When TrueType hinting is enabled, certain glyphs have **coordinate corruption** that creates rendering artifacts:

### Specific Example: Letter 'y'
When rendering the text "The quick brown fox jumps over the lazy dog", the letter 'y' in "lazy" has a horizontal line artifact from the left edge of the image to between 'z' and 'y'.

**Debug Evidence**:
- **Point 7 coordinates**:
  - Raw from glyf table: `(346, -272)` ✓ VALID
  - After scaling to 26.6 format: `(260, -204)` ✓ VALID
  - After org_to_cur transformation: `(260, -204)` ✓ VALID
  - **After hinting: `(-602082, -192)` ✗ CORRUPTED**

The X coordinate becomes -602082 which is completely invalid. This is not an off-by-one error or scaling issue - it's massive coordinate corruption.

### Other Affected Glyphs
Letters 'x', 'v', 'w' also show artifacts when hinting is enabled (diagonal lines show gradient from black to grey).

### What Works
- **Unhinted rendering**: Works perfectly when hinting is disabled
- **FreeType 2 (external C library)**: Works perfectly with hinting enabled
- All other FT1 Pascal functionality works correctly (font loading, glyph caching, outline decomposition, etc.)

## Relevant Files

### Main Hinting Pipeline

1. **`src/3rdparty/freetype/ttgload.pas`** - Glyph loading and hinting orchestration
   - `Load_TrueType_Glyph()` function (main entry point)
   - Calls hinting at line ~460: `TT_Hint_Glyph(exec, glyph, i = n_ins - 1)`
   - Copies hinted coordinates back to outline points

2. **`src/3rdparty/freetype/ttinterp.pas`** - TrueType bytecode interpreter (~3000 lines)
   - `TT_Hint_Glyph()` function - main hinting entry point
   - `RunIns()` function - bytecode instruction execution loop
   - This is where the bug likely exists

3. **`src/3rdparty/freetype/ttobjs.pas`** - Object management
   - `Instance_Init()` and `Instance_Reset()` - graphics state initialization

4. **`src/3rdparty/freetype/tttypes.pas`** - Type definitions
   - **Critical fix already applied**: `TT_FWord = SmallInt` (was Integer)

### Test Programs

Located in: `/home/user/fpGUI/src/3rdparty/freetype/`

- **`test_ft1.pas`** - Tests FreeType 1 Pascal implementation (shows bug)
- **`test_ft2.pas`** - Tests FreeType 2 C library (works correctly)
- **`rebuild_tests.sh`** - Script to rebuild both test programs

**To compile and run**:
```bash
cd /home/user/fpGUI/src/3rdparty/freetype
./rebuild_tests.sh
./test_ft1 /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf 24
./test_ft2 /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf 24
# View output_ft1.png and output_ft2.png
```

Both tests render: "The quick brown fox jumps over the lazy dog 0123456789"

### Enabling/Disabling Hinting

**File**: `src/corelib/render/software/agg_font_freetype_pas.pas`
**Lines**: ~983-993

```pascal
// Map FT2 load flags to FT1 load flags
load_flag := 0;
if (load_flags and FT_LOAD_NO_SCALE) = 0 then
  load_flag := load_flag or TT_Load_Scale_Glyph;
// NOTE: TrueType hinting disabled due to unfixed bug in ttinterp.pas
// Uncomment the next 2 lines to enable hinting and trigger the bug:
// if (load_flags and FT_LOAD_NO_HINTING) = 0 then
//   load_flag := load_flag or TT_Load_Hint_Glyph;
```

**To enable hinting for debugging**: Uncomment those 2 lines, recompile, and run test_ft1.

## What We've Already Tried

### Fixes Applied from Lazarus FreeType1
These were successful patches that fixed other issues but didn't fix the hinting bug:

1. ✅ **Type correctness**: `TT_FWord = SmallInt` (was Integer) - CRITICAL fix
2. ✅ **Removed redundant graphics state copy** in `ttobjs.pas` line 1725
3. ✅ **Added signed type casts** for composite glyph offsets (SmallInt/ShortInt)
4. ✅ **Fixed phantom points** in bounding box calculation

### What Didn't Help
- The hinting bug persists even with all Lazarus patches applied
- The Lazarus version also has this same bug (confirmed by comparison)
- This suggests the bug is deep in the bytecode interpreter

## The Bug Location

**Most likely location**: `src/3rdparty/freetype/ttinterp.pas`

This file contains:
- ~3000 lines of TrueType bytecode interpreter
- Complex instruction execution (`RunIns()` function)
- Graphics state management
- Point coordinate manipulation during hinting

The corruption happens **during** the hinting process in the bytecode interpreter. The raw coordinates, scaling, and org_to_cur transformations are all correct - only hinting corrupts them.

## What I Need Help With

**Primary Goal**: Find and fix the bug causing coordinate corruption during TrueType hinting.

### Specific Questions

1. **Where in `ttinterp.pas` is the coordinate corruption happening?**
   - Which bytecode instruction(s) might be causing this?
   - Are there pointer arithmetic errors?
   - Array bounds issues?
   - Integer overflow/underflow?
   - Type casting problems?

2. **Why does it only affect certain glyphs?**
   - 'y' is corrupted, but 'g' is not
   - What's different about the hinting instructions for these glyphs?

3. **Can you trace the execution for the 'y' glyph specifically?**
   - Add strategic debug output to track Point 7's X coordinate
   - Which instruction changes it from 260 → -602082?

4. **Are there known issues with Pascal TrueType interpreters?**
   - Integer size assumptions (16-bit vs 32-bit vs 64-bit)?
   - Signed vs unsigned arithmetic?
   - Pointer arithmetic differences from C?

### Debugging Approach

Since you have access to fpc and can compile/run code:

1. **Enable hinting** (as shown above)
2. **Add debug output** to `ttinterp.pas` to track Point 7's coordinates
3. **Compile and run** test_ft1 with DejaVuSans.ttf
4. **Trace the execution** to find where X goes from 260 → -602082
5. **Analyze the bytecode instruction** that corrupts the coordinate

### Font Information

**Test font**: DejaVuSans.ttf
- Path: `/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf`
- This is a standard TrueType font that works perfectly with FreeType 2
- Glyph 'y' (lowercase) is the easiest to debug

## Additional Context

### Coordinate Format
- TrueType uses **26.6 fixed-point format**: `value / 64.0 = pixels`
- Example: 260 in 26.6 format = 260/64 = 4.06 pixels ✓
- Example: -602082 in 26.6 format = -9407.53 pixels ✗ (way off screen)

### How Hinting Works
1. Load raw outline points from glyf table
2. Scale to requested size (26.6 format)
3. Copy to `exec.pts.org` (original points)
4. Copy to `exec.pts.cur` (current points - will be modified)
5. **Run TrueType bytecode interpreter** to adjust `exec.pts.cur`
6. Copy `exec.pts.cur` back to glyph outline
7. Render the adjusted outline

The bug is in step 5.

### Architecture Differences
- **FreeType 1**: Separates Face/Instance/Glyph as distinct objects
- **FreeType 2**: Combines them in a single Face object
- Our adapter layer wraps FT1 to provide FT2 API compatibility

## Expected Output

Please provide:
1. **Analysis** of where the bug likely is in `ttinterp.pas`
2. **A fix** (if you can find it) with explanation
3. **Debug strategy** if you need more information
4. **Any insights** about Pascal vs C implementation differences that could cause this

## Thank You!

This bug has been challenging because:
- The interpreter is complex (~3000 lines)
- It only affects certain glyphs
- The Lazarus version also has the bug
- All other functionality works perfectly

A fresh perspective would be incredibly valuable. Feel free to compile, run tests, view images, and add debug output as needed!
