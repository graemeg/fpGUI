unit dvHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base;


function GetOwnHelpFileName: String;
// Given a filename, which may or may not contain a path or extension,
// finds the actual file. This can involve searching
// the help and bookshelf paths.
function FindHelpFile(const AFilename: TfpgString): TfpgString;

function SearchHelpPaths( const Filename: TfpgString; var ResultFilename: TfpgString; const IncludeAppDir: boolean ): boolean;

function GetApplicationDir: TfpgString;
function SearchPath( PathEnvVar: TfpgString; Filename: TfpgString; var FilenameFound: string ): boolean;

implementation

uses
  fpg_utils
  ,dvConstants
  ,nvUtilities
  ;


function GetOwnHelpFileName: String;
begin
  { TODO -oGraeme -cown help : Maybe later we will have different language versions }
  result := fpgExtractFilePath(ParamStr(0)) + cDocViewHelpFile;
  if not fpgFileExists(Result) then
  begin
     Result := FindHelpFile(cDocViewHelpFile);
  end;
end;

// Given a "filename" which may include a path, find it in various paths and extensions
function FindHelpFile(const AFilename: TfpgString): TfpgString;
var
  AlternativeFileName: TfpgString;
  lFilename: TfpgString;
begin
  lFilename := AFilename;
  if lFileName = OWN_HELP_MARKER then
  begin
    Result := GetOwnHelpFileName;
    exit;
  end;

  Result := '';

  AlternativeFileName := '';
  if fpgExtractFileExt( lFilename ) = '' then
  begin
    lFilename := fpgChangeFileExt(lFilename, '.inf');
    AlternativeFileName := fpgChangeFileExt(lFilename, '.hlp');
  end;

  if fpgExtractFilePath( lFileName ) <> '' then
  begin
    // Path specified; just see if it exists

    // expand out relative paths
    lFilename := fpgExpandFileName( lFileName );
    AlternativeFilename := fpgExpandFileName( AlternativeFilename );

    if fpgFileExists( lFilename ) then
      Result := lFilename
    else if fpgFileExists( AlternativeFilename ) then
      Result := AlternativeFilename;

  end
  else
  begin
    // Path not specified; search current
    if fpgFileExists( fpgExpandFileName( lFileName ) ) then
    begin
      Result := fpgExpandFileName( lFileName );
      exit;
    end;

    if (AlternativeFilename <> '') and fpgFileExists(fpgExpandFileName(AlternativeFilename)) then
    begin
      Result := fpgExpandFileName( AlternativeFilename );
      exit;
    end;

    // Search help paths
    if not SearchHelpPaths( lFileName,
                            Result,
                            false // don't search our app dir
                             ) then
    begin
      // Didn't find as specified or as .inf, try .hlp
      if AlternativeFilename <> '' then
      begin
        if not SearchHelpPaths( AlternativeFileName,
                                Result,
                                false // don't search our app dir
                                ) then
        begin
          Result := '';
        end;
      end;
    end;
  end;
//  Result := AFileName;
end;

Function SearchHelpPaths( const Filename: TfpgString;
                          var ResultFilename: TfpgString;
                          const IncludeAppDir: boolean ): boolean;
begin
  Result := SearchPath( HelpPathEnvironmentVar,
                        FileName,
                        ResultFilename );
  if not Result then
    Result := SearchPath( BookshelfEnvironmentVar,
                          FileName,
                          ResultFilename );
  if ( not Result ) and IncludeAppDir then
  begin
    ResultFilename := fpgAppendPathDelim(GetApplicationDir)
                      + Filename;
    Result := fpgFileExists( ResultFilename );
    if not Result then
      ResultFilename := '';
  end;

end;

function GetApplicationDir: TfpgString;
begin
  Result := fpgExtractFilePath(ParamStr(0));
end;

function SearchPath( PathEnvVar: TfpgString; Filename: TfpgString; var FilenameFound: string ): boolean;
var
  lFilename: string;
  lDir: TfpgString;
  fl: TStrings;
  i: integer;
begin
  Result := False;
  FilenameFound := '';

  lDir := GetEnvironmentVariable(PathEnvVar);

  fl := TStringList.Create;
  ListFilesInDirectory(lDir, AllFilesMask, True, fl);
  TStringList(fl).Sort;
  for i := 0 to fl.Count-1 do
  begin
    lFilename := fpgExtractFileName(fl[i]);
    if SameText(lFilename, Filename) then
    begin
      FilenameFound := fl[i];
      Result := True;
      Exit;
    end;
  end;
  fl.Free;
end;


end.

