#!/bin/sh
FPC=fpc 
for f in `find -name '*.lpr'`
do
  DIR=`dirname $f`
  PROJECT=`basename $f`
  LPRNAME=`basename $PROJECT .lpr`
  echo Doing $LPRNAME in $DIR
  pushd $DIR > /dev/null
  if [ ! -d units ]; then
    echo 'Missing units dir'
    mkdir units
  fi
  $FPC @extrafpc.cfg $PROJECT;
  popd > /dev/null
done
