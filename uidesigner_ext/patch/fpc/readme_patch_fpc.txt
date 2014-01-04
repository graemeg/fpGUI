This is a patch that define fpGUI path into fpc.cfg.
For Windows, it adds the fpc path in Windows PATH Environment Variable.

Copy this files into fpGUI root-directory (can be fpGUI-develop too):
- For Windows : fpc_fpg_patch.pas and fpc_fpg_patch.bat
- For Linux   : fpc_fpg_patch.pas and fpc_fpg_patch.sh

Then:
--------------------------------------------------------------------------------------
- For Windows:
In Terminal (cmd.exe) run:

> cd c:\root_dir_of_fpGUI  (go in root-directory of fpGUI, for example: > cd c:\fpGUI)

> fpc_fpg_patch.bat (apply the patch)
----------------------------------------------------------------------------------------
- For Linux:
In Terminal run:

> cd /root_dir_of_fpGUI  (go in root-directory of fpGUI, for example: > cd /home/me_user/fpGUI)

> sudo chmod 777 fpc_fpg_patch.sh  (change permission of script)

> sh fpc_fpg_patch.sh  (apply the patch)
-----------------------------------------------------------------------------------------

That's all ;-)

Fred van Stappen
fiens@hotmail.com  

