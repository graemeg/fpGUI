unit dvHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  OWN_HELP_MARKER = '[DOCVIEWHELP]';


function GetOwnHelpFileName: String;
function FindHelpFile(const AFilename: string): string;


implementation


function GetOwnHelpFileName: String;
//var
//  tmpLanguage : String;
begin
  //tmpLanguage := getLanguage;
  //if tmpLanguage = '' then
  //begin
  //  tmpLanguage := GetEnv(LanguageEnvironmentVar)
  //end;
  //
  //result := FindDefaultLanguageHelpFile('NewView', tmpLanguage);

  { TODO -oGraeme -cown help : Improve own help file location }
  result := ExtractFilePath(ParamStr(0)) + 'docview.inf';
end;

// Given a "filename" which may include a path, find it in various paths and extensions
function FindHelpFile(const AFilename: string): string;
begin
  { TODO -ograemeg -csearch files : Implement searching know locations }
  Result := AFileName;
end;


end.

