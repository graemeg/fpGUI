#!/usr/bin/env bash
#
# Usage: sh localize.sh
#
# This script should be executed after adding new resource strings and after
# updating the translated .po files.
#
# This script
# - builds the tools if required
# - converts all compiled .rst files to .po files
# - updates all translated xx.po files
#

# enable for debugging
#set -x
set -e

if [ ! -x updatepofiles.exe ]; then
  ./build_tools.bat
fi

if [ "@"$FPCTARGET == "@" ]; then
  FPCTARGET=`fpc -iTP`-`fpc -iTO`
  if [ $FPCTARGET == "-" ]; then
    FPCTARGET=""
  fi
fi

RSTFILES=(
  ".. fpg_constants fpgui"
#  "ideintf objinspstrconsts"
#  "components/codetools codetoolsstrconsts"
#  "lcl lclstrconsts"
)  

for idx in ${!RSTFILES[@]}; do
  LINE=(${RSTFILES[idx]})
  RSTDIR=${LINE[0]}  
  RSTFILE=${LINE[1]}  
  POFILE=${LINE[2]:-$RSTFILE}
   
#  RST=`find $RSTDIR/{units,lib}/$FPCTARGET -name $RSTFILE.rst | xargs ls -1t | head -1`;
  RST=`find $RSTDIR/{units,lib} -name $RSTFILE.rst | xargs ls -1t | head -1`;

  if [ "@"$RST != "@" ]; then
    echo $RSTDIR/languages/$POFILE.po
#    rstconv -c UTF-8 -i $RST -o $RSTDIR/languages/$POFILE.po
    rstconv -i $RST -o $RSTDIR/languages/$POFILE.po
    ./updatepofiles.exe $RSTDIR/languages/$POFILE.po
  fi
done

# generate new include files from the updated .po files.
./generateincfiles.exe ..

exit 0

