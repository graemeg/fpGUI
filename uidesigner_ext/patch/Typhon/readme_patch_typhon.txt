This patch enable complete integration of fpGUI into Typhon.

1)- Compile uidesinger_ext.pas
  - Close uidesinger_ext

2)- Copy-replace ~/uidesigner_ext/patch/Typhon/sourcefilemanager.pas into ../codetyphon/typhon/ide and rebuild Typhon.
  - Close Typhon

3)- Load uidesigner_ext and, in "Settings", enable "Integration into IDE Typhon".
  - Close uidesigner_ext

Now, you may run Typhon with fpGUI-uidesigner integeration...

Fred van Stappen
fiens@hotmail.com  

