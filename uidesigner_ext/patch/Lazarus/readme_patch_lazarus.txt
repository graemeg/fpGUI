This patch enable complete integration of fpGUI into Lazarus.

1)- Compile uidesinger_ext.pas
  - Close uidesigner_ext

2)- Copy-replace ~/uidesigner_ext/patch/Lazarus/main.pp into ../Lazarus/ide and rebuild Lazarus.
  - Close Lazarus

3)- Load uidesigner_ext and, in "Settings", enable "Integration into IDE Lazarus".
  - Close uidesigner_ext

Now, you may run Lazarus with fpGUI-uidesigner integeration...

Fred van Stappen
fiens@hotmail.com  
