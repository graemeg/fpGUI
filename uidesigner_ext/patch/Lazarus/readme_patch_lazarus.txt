This patch enable complete integration of fpGUI into Lazarus.

1)- Compile uidesinger_ext.pas
  - Close uidesigner_ext

2)- Depending of your Lazarus version, unzip main.pp_laz_xxx in ~/uidesigner_ext/patch/Lazarus/

3)- Copy-replace ~/uidesigner_ext/patch/Lazarus/main.pp into ../Lazarus/ide and rebuild Lazarus.
  - Close Lazarus

4)- Load uidesigner_ext and, in "Settings", enable "Integration into IDE Lazarus".
  - Close uidesigner_ext

Now, you may run Lazarus with fpGUI-uidesigner integeration...

Fred van Stappen
fiens@hotmail.com  
