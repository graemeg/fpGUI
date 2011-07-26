unit nvUtilities;

{$mode objfpc}{$H+}

// disable to remove debugging output
{.$Define DEBUG}

interface

uses
  Classes, SysUtils, fpg_base;

const
 { TODO -oGraeme : Should this change to LineEnding (platfrom dependent) }
  EndLine= chr(13)+chr(10);
  TwoEndLines= chr(13)+chr(10)+chr(13)+chr(10);
  Quote = '''';
  DoubleQuote = '"';

  // -- Logging --
type
  LogAspect = (       LogStartup,
                      LogShutdown,
                      LogSettings,
                      LogI18n,
                      LogParse,
                      LogDisplay,
                      LogSearch,
                      LogNHM,
                      LogViewStub,
                      LogObjConstDest,
                      LogDebug
  );
  LogAspects = SET OF LogAspect;

procedure LogEvent(const aLogAspect: LogAspect; const anEventDescription: String);

// Removes and returns the first value in a separated
// value list (removes quotes if found)
Function ExtractNextValue(var S: string; const Separator: string ): string;

Function ExtractNextValueNoTrim(var S: string; const Separator: string ): string;

function AllocateMemory( const Size: ValUInt ): pointer;
procedure DeallocateMemory( Var P: pointer );

// Alias method which is the same as Move() but with less confusing name
procedure MemCopy(const src; var dest; size: SizeInt);
procedure FillMem( Dest: pointer; Size: longint; Data: Byte );
// Allows for debug output and quite disable of output
procedure ProfileEvent(const AString: string);
// Return AFilename's size in bytes
function GetFileSize(const AFilename: string): integer;

function IsDigit(const AChar: TfpgChar): boolean;
function IsAlpha(const AChar: TfpgChar): boolean;
function Between( const Value: longint; const Limit1: longint; const Limit2: longint ): boolean;


operator = (ARect: TRect; BRect: TRect): boolean;

// Destroy the objects stored in List and clear the list.
procedure ClearListAndObjects( List: TList );
// Destroy the objects stored in the list and then destroy the list itself
// And set the reference to nil
procedure DestroyListAndObjects( Var List: TList );
// Destroy the objects stored in the list.
// You probably want to use one of the two functions above.
procedure DestroyListObjects( List: TList );

procedure AddList( Source, Dest: TList );
procedure AssignList( Source, Dest: TList );

procedure ListFilesInDirectory(const aDirectory: String; const aFilter: String; const aWithDirectoryFlag: boolean; var aList: TStrings);

// add all file name parts of aFileNameString to the aResult
// check for containing environment vars
// and include all help files if the environment var points
// to a directory
procedure ParseAndExpandFileNames(const aFileNameString: String; aResult: TStrings);



var
  startTime : Cardinal;
  lastTime : Cardinal;
  activeLogAspects : LogAspects;
  infoMessage1 : String;
  infoMessage2 : String;


implementation

uses
  fpg_utils
  ,fpg_main
  ,ACLStringUtility
  ,dvconstants
  ;

  Function GetAspectPrefix(const aLogAspect: LogAspect): String;
  Begin
    Case aLogAspect of
      LogStartup      : result := 'Startup';
      LogShutdown     : result := 'Start';
      LogSettings     : result := 'Settings';
      LogI18n         : result := 'I18n';
      LogParse        : result := 'Parse';
      LogDisplay      : result := 'Display';
      LogSearch       : result := 'Search';
      LogNHM          : result := 'NewHelpManager';
      LogViewStub     : result := 'ViewStub';
      LogObjConstDest : result := 'ObjConstDest';
      LogDebug        : result := 'Debug';
      else              result := 'Unknown';
      end;
  End;


  Procedure SetLogAspects(const aCommaSeparatedListOfAspectNames : String);
  Var
    tmpAspects : TStrings;
    i : Integer;
  Begin
    tmpAspects := TStringList.Create;
    StrExtractStrings(tmpAspects, aCommaSeparatedListOfAspectNames, [','], #0);

    for i:=0 to tmpAspects.count-1 do
    begin
      if tmpAspects[i] = 'LogStartup'      then activeLogAspects := activeLogAspects + [ LogStartup ];
      if tmpAspects[i] = 'LogShutdown'     then activeLogAspects := activeLogAspects + [ LogShutdown ];
      if tmpAspects[i] = 'LogSettings'     then activeLogAspects := activeLogAspects + [ LogSettings ];
      if tmpAspects[i] = 'LogI18n'         then activeLogAspects := activeLogAspects + [ LogI18n ];
      if tmpAspects[i] = 'LogParse'        then activeLogAspects := activeLogAspects + [ LogParse ];
      if tmpAspects[i] = 'LogDisplay'      then activeLogAspects := activeLogAspects + [ LogDisplay ];
      if tmpAspects[i] = 'LogSearch'       then activeLogAspects := activeLogAspects + [ LogSearch ];
      if tmpAspects[i] = 'LogNHM'          then activeLogAspects := activeLogAspects + [ LogNHM ];
      if tmpAspects[i] = 'LogViewStub'     then activeLogAspects := activeLogAspects + [ LogViewStub ];
      if tmpAspects[i] = 'LogObjConstDest' then activeLogAspects := activeLogAspects + [ LogObjConstDest ];
      if tmpAspects[i] = 'LogDebug'        then activeLogAspects := activeLogAspects + [ LogDebug ];
    end;

    tmpAspects.Destroy;
  End;

procedure LogEvent(const aLogAspect: LogAspect; const anEventDescription: String);
var
  tmpMessage: String;
begin
  if (aLogAspect IN activeLogAspects) then
  begin
    tmpMessage := 'Log[' + GetAspectPrefix(aLogAspect) + ']  ' + anEventDescription;
    debugln(tmpMessage);
  end;
end;

Function ExtractNextValue( var S: string;
                           const Separator: string ): string;
begin
  Result := ExtractNextValueNoTrim( S, Separator );
  Result := trim( Result );

  // Remove quotes if present
  if Result <> '' then
    if Result[ 1 ] = DoubleQuote then
      Delete( Result, 1, 1 );

  if Result <> '' then
    if Result[ length( Result ) ] = DoubleQuote then
      Delete( Result, length( Result ), 1 );
end;

Function ExtractNextValueNoTrim( var S: string;
                                 const Separator: string ): string;
Var
  SeparatorPos: integer;
Begin
  SeparatorPos := Pos( Separator, S );
  if SeparatorPos > 0 then
  begin
    Result := Copy( S, 1, SeparatorPos-1 );
    Delete( S, 1, SeparatorPos + length( Separator ) - 1 );
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

function AllocateMemory( const Size: ValUInt ): pointer;
begin
  GetMem( Result, size + sizeof( Size ) );
  PtrUInt(Result^) := Size;
  inc( Result, sizeof( Size ) );
end;

procedure DeallocateMemory( Var P: pointer );
var
  Size: ValUInt;
begin
  if P = nil then
    exit;

  dec( P, sizeof( size ) );
  Size := ValUInt(P^);
  FreeMem( P, Size + sizeof( Size ) );
  P := nil;
end;

procedure MemCopy(const src; var dest; size: SizeInt);
begin
  Move(src, dest, size);
end;

procedure FillMem( Dest: pointer; Size: longint; Data: Byte );
begin
  FillChar( Dest^, Size, Data );
end;

procedure ProfileEvent(const AString: string);
begin
  {$IFDEF DEBUG}
  writeln('DEBUG:  ', AString);
  {$ENDIF}
end;

function GetFileSize(const AFilename: string): integer;
begin
  Result := fpgFileSize(AFilename);
end;

function IsDigit(const AChar: TfpgChar): boolean;
begin
  { TODO -oGraeme -cunicode : Not utf-8 compliant. }
  Result := ( AChar>='0' ) and ( AChar<='9' );
  //Result := TCharacter.IsDigit(AChar);
end;

function IsAlpha(const AChar: TfpgChar): boolean;
var
  UppercaseC: TfpgChar;
Begin
  { TODO -oGraeme -cunicode : Not utf-8 compliant. }
  UppercaseC := UpperCase( AChar );
  Result := ( UppercaseC >= 'A' ) and ( UppercaseC <= 'Z' );
  //Result := TCharacter.IsLetter(AChar);
end;

function Between( const Value: longint; const Limit1: longint; const Limit2: longint ): boolean;
begin
  if Limit1 < Limit2 then
    Result:= ( Value >= Limit1 ) and ( Value <= Limit2 )
  else
    Result:= ( Value >= Limit2 ) and ( Value <= Limit1 );
end;

operator = (ARect: TRect; BRect: TRect): boolean;
begin
  result :=  (ARect.Top   = BRect.Top) and
             (ARect.Left  = BRect.Left) and
             (ARect.Bottom = BRect.Bottom) and
             (ARect.Right = BRect.Right);
end;

// Destroy the objects stored in List
// and clear the list.
Procedure ClearListAndObjects( List: TList );
begin
  DestroyListObjects( List );
  List.Clear;
end;

// Destroy the objects stored in the list
// and then destroy the list itself.
Procedure DestroyListAndObjects( Var List: TList );
begin
  if not Assigned( List ) then
    exit;

  DestroyListObjects( List );
  List.Free;
  List := nil;
end;

Procedure DestroyListObjects( List: TList );
var
  Index: longint;
begin
  for Index := 0 to List.Count - 1 do
  begin
    if List[ Index ] <> nil then
    begin
      TObject( List[ Index ] ).Free;
      List[ Index ] := nil;
    end;
  end;
end;

Procedure AddList( Source, Dest: TList );
var
  i: longint;
begin
  // expand the destination list to what's required
  Dest.Capacity := Dest.Capacity + Source.Capacity;
  for i:= 0 to Source.Count - 1 do
    Dest.Add( Source[ i ] );
end;

Procedure AssignList( Source, Dest: TList );
begin
  Dest.Clear;
  AddList( Source, Dest );
end;

procedure ListFilesInDirectory(const aDirectory: String; const aFilter: String;
    const aWithDirectoryFlag: boolean; var aList: TStrings);
var
  tmpRC: longint;
  tmpSearchResults: TSearchRec;
  tmpMask: String;
  tmpFilterParts : TStrings;
  tmpDirectory: String;
  i: integer;
begin
  tmpFilterParts := TStringList.Create;

  StrExtractStrings(tmpFilterParts, aFilter, [PathSeparator], #0);

  for i:=0 to tmpFilterParts.count-1 do
  begin
    tmpMask := tmpFilterParts[i];
    tmpDirectory := IncludeTrailingPathDelimiter(aDirectory);
    tmpRC := fpgFindFirst(tmpDirectory + tmpMask, faAnyFile, tmpSearchResults);

    while tmpRC = 0 do
    begin
      if (tmpSearchResults.Attr and faDirectory) = 0 then
      begin
        if (aWithDirectoryFlag) then
          aList.Add(tmpDirectory + tmpSearchResults.Name)
        else
          aList.Add(tmpSearchResults.Name);
      end;

      tmpRC := fpgFindNext(tmpSearchResults);
    end;

    FindClose(tmpSearchResults);
  end;
  tmpFilterParts.Destroy;
end;

procedure ParseAndExpandFileNames(const aFileNameString: String; aResult: TStrings);
var
  i: longint;
  tmpFileNamesList: TStrings;
  tmpItem: String;
  tmpEnvironmentVarValue: string;
begin
  LogEvent(LogDebug, 'ParseAndExpandFileNames "' + aFileNameString + '"');
  tmpFileNamesList := TStringList.Create;

  StrExtractStrings(tmpFileNamesList, aFileNameString, [HELP_FILE_DELIMITER, PathSeparator], #0);
  for i := 0 to tmpFileNamesList.Count - 1 do
  begin
    tmpItem := tmpFileNamesList[i];

    // is this a environment var
    tmpEnvironmentVarValue := GetEnvironmentVariable(tmpItem);
    if tmpEnvironmentVarValue <> '' then      // environment var exists
    begin
      LogEvent(LogStartup, '    Environment var found; translated to: "' + tmpEnvironmentVarValue + '"');
      ParseAndExpandFileNames(tmpEnvironmentVarValue, aResult);
    end
    else if fpgDirectoryExists(tmpItem) then
    begin
      ListFilesInDirectory(tmpItem, '*' + INF_FILE_EXTENSION, true, aResult);
    end
    else
    begin
      aResult.Add(tmpItem);
    end;
  end;

  tmpFileNamesList.Free;
end;

{$IFDEF DEBUG}
initialization
  SetLogAspects('LogDebug,LogStartup');
{$ENDIF}

end.

