#!/bin/bash

POTFILE="uidesigner.pot"

A=`which xgettext`
if [ -z "$A" ]
then
  echo "Error: missing xgettext"
  exit 1
fi

cp -f ../vfd_constants.pas 1.tmp
rm -f $POTFILE
cat 1.tmp |sed '1,/resourcestring/d'|sed "s|'*;|zzzzz|g" > 2.tmp
cat 2.tmp |sed "s|=*'|_('|g" > 3.tmp
cat 3.tmp |sed "s|zzzzz|');|g" > 4.tmp
xgettext --language=Python --keyword=_ 4.tmp -o $POTFILE
sed -i "s|4.tmp|vfd_constants.pas|g" $POTFILE
sed -i '/python-format/d' $POTFILE
rm -f *.tmp
