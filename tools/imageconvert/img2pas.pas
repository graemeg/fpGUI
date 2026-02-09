{**********************************************************************
 * Image to Pascal                                                    *
 * Written by Jonathan A. Foster <jon@jfpossibilities.com>            *
 * Started August 30th, 2017                                          *
 * Copyright JF Possibilities, Inc.                                   *
 * License: fpGUI's modified LGPL.                                    *
 **********************************************************************}
program img2pas; {$mode objfpc}{$H+}
uses SysUtils, bin2pas;


procedure help;
begin
	WriteLn(ExtractFileName(ParamStr(0))+' {file} [{file} ...]');
end;



procedure exout(e: exception);
begin
	ExitCode:=2;
	WriteLn(ErrOutput, e.message);
end;



var
	x: integer;
begin
	try
		if ParamCount<1 then begin help; ExitCode:=1; exit; end;
		for x:=1 to ParamCount do Write(ConvertImage(ParamStr(x)));
	except
		on e: exception do exout(e);
	end;
end.
