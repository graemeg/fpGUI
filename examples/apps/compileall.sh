#!/bin/sh
FPC=fpc 
for f in `find -name '*.lpr'`
do
  DIR=`dirname $f`
  PROJECT=`basename $f`
  LPRNAME=`basename $PROJECT .lpr`
  echo Doing $LPRNAME in $DIR
  cd $DIR
  if [ ! -d units ]; then
    echo 'Missing units dir'
    mkdir units
  fi
  $FPC @extrafpc.cfg $PROJECT;
  cd ..
done
