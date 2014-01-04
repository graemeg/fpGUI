{ This is a patch that define fpGUI path into fpc.cfg free-pascal-congig-file.
Fred van Stappen
fiens@hotmail.com
}
program fpc_fpg_patch;

{$mode objfpc}{$H+}
  uses
  Classes,
  SysUtils,
  CustApp;

type

  { Tfpc_fpg_patch }

  Tfpc_fpg_patch = class(TCustomApplication)
  private
    procedure DoPatch;
  protected
    procedure doRun; override;
  public
    procedure Consoleclose;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  procedure Tfpc_fpg_patch.DoPatch;
  var
  ordir, tempstr, tempstr2: string;
  f : textfile ;
  x, y : integer;
  begin
      writeln('') ;

    writeln('This patch will define the fpGUI location...');
    writeln('');
    writeln('Location of fpc_fpg_patch : ' + ParamStr(0) );
   tempstr := copy(ParamStr(0),1, pos(directoryseparator + 'fpc_fpg_patch',ParamStr(0))-1) ;
    writeln('Location of fpGUI : ' + tempstr );

      y := 0 ;
      {$IFDEF linux}
     while y < 6 do
      begin
    case y of
    0 : ordir := '/etc/fpc.cfg' ;
    1 : ordir := '/etc/fpc-2.6.2.cfg' ;
    2 : ordir := '/etc/fpc-2.7.1.cfg' ;
    3 : ordir := '/usr/lib/codetyphon/fpc/bin/x86_64-linux/fpc.cfg' ;
    4 : ordir := '/usr/lib/codetyphon/fpc/bin/i386-linux/fpc.cfg' ;
    5 : ordir := '~/.fpc.cfg' ;
    end;
     {$ENDIF}

   {$IFDEF windows}
     while y < 17 do
      begin
    case y of
    0 : ordir := 'c:\codetyphon\fpc\bin\x86_64-win64\fpc.cfg' ;
    1 : ordir := 'c:\codetyphon\fpc\bin\i386-win32\fpc.cfg' ;
    2 : ordir := 'c:\lazarus\fpc\2.6.2\bin\x86_64-win64\fpc.cfg' ;
    3 : ordir := 'c:\lazarus\fpc\2.6.2\bin\i386-win32\fpc.cfg' ;
    4 : ordir := 'c:\lazarus\fpc\2.7.1\bin\x86_64-win64\fpc.cfg' ;
    5 : ordir := 'c:\lazarus\fpc\2.7.1\bin\i386-win32\fpc.cfg' ;
    6 : ordir := 'c:\fpc\2.6.2\bin\x86_64-win64\fpc.cfg' ;
    7 : ordir := 'c:\fpc\2.6.2\bin\i386-win32\fpc.cfg' ;
    8 : ordir := 'c:\fpc\2.7.1\bin\x86_64-win64\fpc.cfg' ;
    9 : ordir := 'c:\fpc\2.7.1\bin\i386-win32\fpc.cfg' ;
    10: ordir := 'c:\fpc\bin\x86_64-win64\fpc.cfg' ;
    11: ordir := 'c:\fpc\bin\i386-win32\fpc.cfg' ;
    12: ordir := 'c:\fpc\bin\fpc.cfg' ;
    13: ordir := 'c:\fpc\2.6.2\bin\i386-win32\fpc.cfg' ;
    14: ordir := 'c:\fpc\2.6.2\bin\x86_64-win64\fpc.cfg' ;
    15: ordir := 'c:\fpc\2.7.1\bin\i386-win32\fpc.cfg' ;
    16: ordir := 'c:\fpc\2.7.1\bin\x86_64-win64\fpc.cfg' ;
    end;
     {$ENDIF}

     inc(y);

   {$IFDEF windows}
    tempstr2 := '#include c:\fpc_ext\fpc-fpg.cfg' ;
   {$ENDIF}

    {$IFDEF linux}
    tempstr2 := '#include /etc/fpc-fpg.cfg' ;
   {$ENDIF}

 if fileexists(ordir) then
 begin
   x := 0;
  AssignFile(f,pchar(ordir)) ;
 Reset(F);
       while not eof(f) do
   begin
   readln(f,tempstr) ;
   if pos(tempstr2,tempstr) > 0
   then x := 1 ;
   end;
       CloseFile(f);
     if  x = 0 then begin
    AssignFile(f,pchar(ordir));
    append(f);
   writeln(f,tempstr2) ;
    Flush(f);
      CloseFile(f);
   writeln('Patch added in ' + ordir) ;
   end else  writeln('Patch already added in ' + ordir + ', nothing to do...') ;
  end;

  end;

     /////////////Writing new fpc-fpg.cfg//////////
      {$IFDEF linux}
     ordir := '/etc/fpc-fpg.cfg' ;
      {$ENDIF}

      {$IFDEF windows}
     if DirectoryExists('c:\fpc_ext') then
  else
    ForceDirectories('c:\fpc_ext');
    ordir := 'c:\fpc_ext\fpc-fpg.cfg' ;
   {$ENDIF}
   AssignFile(f,pchar(ordir));
   rewrite(f);
       append(f);
    writeln(f,'# Begin fpGUI-block') ;
      append(f);
    writeln(f,'# search-path for fpGUI units and includes') ;
    tempstr := copy(ParamStr(0),1, pos(directoryseparator + 'fpc_fpg_patch',ParamStr(0))-1) + directoryseparator + 'src' ;
    append(f);
    writeln(f,'-Fu' + tempstr) ;
        append(f);
    writeln(f,'-Fi' + tempstr) ;
        append(f);
    writeln(f,'-Fu' + tempstr + directoryseparator + 'corelib') ;
     {$IFDEF linux}
     append(f);
    writeln(f,'-Fu' + tempstr + directoryseparator + 'corelib'+ directoryseparator + 'x11') ;
      append(f);
    writeln(f,'-Fi' + tempstr + directoryseparator + 'corelib' + directoryseparator + 'x11') ;
      {$ENDIF}
      {$IFDEF windows}
     append(f);
    writeln(f,'-Fu' + tempstr + directoryseparator + 'corelib'+ directoryseparator + 'gdi') ;
      append(f);
    writeln(f,'-Fi' + tempstr + directoryseparator + 'corelib'+ directoryseparator + 'gdi');
      {$ENDIF}
     append(f);
    writeln(f,'-Fu' + tempstr + directoryseparator + 'corelib'+ directoryseparator + 'render' + directoryseparator + 'software') ;
       append(f);
    writeln(f,'-Fu' + tempstr + directoryseparator + 'gui') ;
       append(f);
    writeln(f,'-Fu' + tempstr + directoryseparator + 'gui'+ directoryseparator + 'db') ;
    append(f);
    writeln(f,'-Fu' + tempstr  + directoryseparator + 'reportengine') ;
    append(f);
     writeln(f,'# object pascal dialect and compil options');
     append(f);
     writeln(f,'-MObjFPC');
     append(f);
     writeln(f,'-Schi');
     {$IFDEF windows}
     append(f);
     writeln(f,'-WG');
     {$ENDIF}
     append(f);
     writeln(f,'-O1');
     append(f);
     writeln(f,'-vewnhi');
     append(f);
     writeln(f,'-Xs');
     append(f);
     writeln(f,'-XX');
      // append(f);
    // writeln(f,'# define output in /units');
     //append(f);
     //writeln(f,'-FUunits');
     append(f);
     writeln(f,'# End fpGUI-block') ;
       Flush(f);
    CloseFile(f);
     writeln('');
    writeln('=> fpc-fpg.cfg created in ' +ordir) ;
     writeln('') ;
    writeln('Everything done and OK... ;-)') ;

    end;
        
   procedure Tfpc_fpg_patch.doRun;
   var
   quid :char ;
   tempstr : string;
  begin
    if  (ParamStr(1) = '1') then DoPatch else
    begin
   writeln('This is a patch for fpc.cfg to define fpGUI path.');
   writeln('');
   writeln('WARNING: Be sure that the patch is in fpGUI root-directory.');

        tempstr := copy(ParamStr(0),1, pos(directoryseparator + 'fpc_fpg_patch',ParamStr(0))-1) ;

   writeln( '(Now in ' + tempstr + ')') ;
   writeln('Do you want to patch fpc.cfg (y/n) ?');
   readln(quid) ;

     case lowercase(quid) of
     'y' : DoPatch;
          end;
     end;
     Terminate;     
  end;

  procedure Tfpc_fpg_patch.ConsoleClose;

  begin
    Terminate;
  end;

  constructor Tfpc_fpg_patch.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor Tfpc_fpg_patch.Destroy;
  begin
    inherited Destroy;
  end;

var
  Application: Tfpc_fpg_patch;
begin
  Application := Tfpc_fpg_patch.Create(nil);
  Application.Run;
  Application.Free;
end.
