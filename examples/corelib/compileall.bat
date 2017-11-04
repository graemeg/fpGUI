@ECHO OFF

SET FPC=fpc

FOR /D %%D IN (*) DO (
   Pushd %%D
      ECHO doing %%D in %%D.lpr
      If NOT EXIST bin MKDIR bin
      %FPC% "@extrafpc.cfg" %%D.lpr
      ECHO ###############################################################################
   Popd
)