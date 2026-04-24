{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Session storage for IDE state. Saves/loads per-project session
      data (open files, caret positions, scroll offsets, active tab)
      to a .ide/session.json file alongside the project file.
}

unit ide.session;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base;

type
  TOpenFileInfo = record
    Path: TfpgString;        { relative to project dir }
    CaretLine: Integer;
    CaretCol: Integer;
    ScrollTop: Integer;
    ScrollLeft: Integer;
    TabOrder: Integer;
  end;

  TIDESession = class(TObject)
  private
    FProjectDir: TfpgString;
    FSessionFile: TfpgString;
    FOpenFiles: array of TOpenFileInfo;
    FActiveTab: Integer;
    FActiveProfiles: TStringList;
    FToolPanelWidth: Integer;
    FBottomPanelHeight: Integer;
    FVarShowType: Boolean;
    FVarShowScope: Boolean;
    FVarShowGlobals: Boolean;
    FWatches: TStringList;
    function GetOpenFileCount: Integer;
    function GetOpenFile(AIndex: Integer): TOpenFileInfo;
  public
    constructor Create(const AProjectDir: TfpgString);
    destructor Destroy; override;
    procedure Clear;
    procedure AddOpenFile(const AAbsPath: TfpgString;
      ACaretLine, ACaretCol, AScrollTop, AScrollLeft, ATabOrder: Integer);
    procedure Load;
    procedure Save;
    function SessionFileExists: Boolean;
    property OpenFileCount: Integer read GetOpenFileCount;
    property OpenFiles[AIndex: Integer]: TOpenFileInfo read GetOpenFile;
    property ActiveTab: Integer read FActiveTab write FActiveTab;
    property ActiveProfiles: TStringList read FActiveProfiles;
    property ToolPanelWidth: Integer read FToolPanelWidth write FToolPanelWidth;
    property BottomPanelHeight: Integer read FBottomPanelHeight write FBottomPanelHeight;
    property VarShowType: Boolean read FVarShowType write FVarShowType;
    property VarShowScope: Boolean read FVarShowScope write FVarShowScope;
    property VarShowGlobals: Boolean read FVarShowGlobals write FVarShowGlobals;
    { Persisted watch expressions. Caller populates before Save; reads after Load. }
    property Watches: TStringList read FWatches;
    property SessionFile: TfpgString read FSessionFile;
  end;


implementation

uses
  fpjson, jsonparser, fpg_utils;


{ TIDESession }

constructor TIDESession.Create(const AProjectDir: TfpgString);
begin
  inherited Create;
  FProjectDir := IncludeTrailingPathDelimiter(AProjectDir);
  FSessionFile := FProjectDir + '.ide' + PathDelim + 'session.json';
  FActiveTab := -1;
  FToolPanelWidth := -1;
  FBottomPanelHeight := -1;
  FVarShowType    := True;
  FVarShowScope   := True;
  FVarShowGlobals := True;
  FActiveProfiles := TStringList.Create;
  FActiveProfiles.Delimiter := ',';
  FActiveProfiles.StrictDelimiter := True;
  FWatches := TStringList.Create;
  SetLength(FOpenFiles, 0);
end;

destructor TIDESession.Destroy;
begin
  FWatches.Free;
  FActiveProfiles.Free;
  inherited Destroy;
end;

function TIDESession.GetOpenFileCount: Integer;
begin
  Result := Length(FOpenFiles);
end;

function TIDESession.GetOpenFile(AIndex: Integer): TOpenFileInfo;
begin
  Result := FOpenFiles[AIndex];
end;

procedure TIDESession.Clear;
begin
  SetLength(FOpenFiles, 0);
  FActiveTab := -1;
  FActiveProfiles.Clear;
  FWatches.Clear;
  FToolPanelWidth := -1;
  FBottomPanelHeight := -1;
  FVarShowType    := True;
  FVarShowScope   := True;
  FVarShowGlobals := True;
end;

procedure TIDESession.AddOpenFile(const AAbsPath: TfpgString;
  ACaretLine, ACaretCol, AScrollTop, AScrollLeft, ATabOrder: Integer);
var
  Idx: Integer;
begin
  Idx := Length(FOpenFiles);
  SetLength(FOpenFiles, Idx + 1);
  FOpenFiles[Idx].Path := ExtractRelativePath(FProjectDir, AAbsPath);
  FOpenFiles[Idx].CaretLine := ACaretLine;
  FOpenFiles[Idx].CaretCol := ACaretCol;
  FOpenFiles[Idx].ScrollTop := AScrollTop;
  FOpenFiles[Idx].ScrollLeft := AScrollLeft;
  FOpenFiles[Idx].TabOrder := ATabOrder;
end;

function TIDESession.SessionFileExists: Boolean;
begin
  Result := fpgFileExists(FSessionFile);
end;

procedure TIDESession.Load;
var
  FS: TFileStream;
  JSONData: TJSONData;
  RootObj: TJSONObject;
  SessionObj: TJSONObject;
  FilesArr: TJSONArray;
  FileObj: TJSONObject;
  ProfilesArr: TJSONArray;
  I: Integer;
  Info: TOpenFileInfo;
begin
  Clear;
  if not SessionFileExists then
    Exit;

  FS := TFileStream.Create(FSessionFile, fmOpenRead or fmShareDenyNone);
  try
    JSONData := GetJSON(FS);
  finally
    FS.Free;
  end;

  try
    RootObj := TJSONObject(JSONData);

    { Session section }
    if RootObj.IndexOfName('session') >= 0 then
    begin
      SessionObj := RootObj.Objects['session'];
      if SessionObj.IndexOfName('activeTab') >= 0 then
        FActiveTab := SessionObj.Integers['activeTab'];
      if SessionObj.IndexOfName('activeProfiles') >= 0 then
      begin
        ProfilesArr := SessionObj.Arrays['activeProfiles'];
        for I := 0 to ProfilesArr.Count - 1 do
          FActiveProfiles.Add(ProfilesArr.Strings[I]);
      end;
      if SessionObj.IndexOfName('toolPanelWidth') >= 0 then
        FToolPanelWidth := SessionObj.Integers['toolPanelWidth'];
      if SessionObj.IndexOfName('bottomPanelHeight') >= 0 then
        FBottomPanelHeight := SessionObj.Integers['bottomPanelHeight'];
      if SessionObj.IndexOfName('varShowType') >= 0 then
        FVarShowType := SessionObj.Booleans['varShowType'];
      if SessionObj.IndexOfName('varShowScope') >= 0 then
        FVarShowScope := SessionObj.Booleans['varShowScope'];
      if SessionObj.IndexOfName('varShowGlobals') >= 0 then
        FVarShowGlobals := SessionObj.Booleans['varShowGlobals'];
    end;

    { Watch expressions }
    if RootObj.IndexOfName('watches') >= 0 then
    begin
      FilesArr := RootObj.Arrays['watches'];
      for I := 0 to FilesArr.Count - 1 do
        if FilesArr.Strings[I] <> '' then
          FWatches.Add(FilesArr.Strings[I]);
    end;

    { Open files }
    if RootObj.IndexOfName('openFiles') >= 0 then
    begin
      FilesArr := RootObj.Arrays['openFiles'];
      SetLength(FOpenFiles, FilesArr.Count);
      for I := 0 to FilesArr.Count - 1 do
      begin
        FileObj := FilesArr.Objects[I];
        Info.Path := '';
        Info.CaretLine := 0;
        Info.CaretCol := 0;
        Info.ScrollTop := 0;
        Info.ScrollLeft := 0;
        Info.TabOrder := I;
        if FileObj.IndexOfName('path') >= 0 then
          Info.Path := FileObj.Strings['path'];
        if FileObj.IndexOfName('caretLine') >= 0 then
          Info.CaretLine := FileObj.Integers['caretLine'];
        if FileObj.IndexOfName('caretCol') >= 0 then
          Info.CaretCol := FileObj.Integers['caretCol'];
        if FileObj.IndexOfName('scrollTop') >= 0 then
          Info.ScrollTop := FileObj.Integers['scrollTop'];
        if FileObj.IndexOfName('scrollLeft') >= 0 then
          Info.ScrollLeft := FileObj.Integers['scrollLeft'];
        if FileObj.IndexOfName('tabOrder') >= 0 then
          Info.TabOrder := FileObj.Integers['tabOrder'];
        FOpenFiles[I] := Info;
      end;
    end;
  finally
    JSONData.Free;
  end;
end;

procedure TIDESession.Save;
var
  RootObj, SessionObj, FileObj: TJSONObject;
  FilesArr, ProfilesArr: TJSONArray;
  I: Integer;
  IdeDir: TfpgString;
  FS: TFileStream;
  JSONStr: TfpgString;
begin
  { Ensure .ide directory exists }
  IdeDir := FProjectDir + '.ide';
  if not fpgDirectoryExists(IdeDir) then
    fpgForceDirectories(IdeDir);

  RootObj := TJSONObject.Create;
  try
    { Session section }
    SessionObj := TJSONObject.Create;
    SessionObj.Add('activeTab', FActiveTab);
    if FActiveProfiles.Count > 0 then
    begin
      ProfilesArr := TJSONArray.Create;
      for I := 0 to FActiveProfiles.Count - 1 do
        ProfilesArr.Add(FActiveProfiles[I]);
      SessionObj.Add('activeProfiles', ProfilesArr);
    end;
    if FToolPanelWidth > 0 then
      SessionObj.Add('toolPanelWidth', FToolPanelWidth);
    if FBottomPanelHeight > 0 then
      SessionObj.Add('bottomPanelHeight', FBottomPanelHeight);
    SessionObj.Add('varShowType',    FVarShowType);
    SessionObj.Add('varShowScope',   FVarShowScope);
    SessionObj.Add('varShowGlobals', FVarShowGlobals);
    RootObj.Add('session', SessionObj);

    { Watch expressions }
    if FWatches.Count > 0 then
    begin
      ProfilesArr := TJSONArray.Create;
      for I := 0 to FWatches.Count - 1 do
        ProfilesArr.Add(FWatches[I]);
      RootObj.Add('watches', ProfilesArr);
    end;

    { Open files }
    FilesArr := TJSONArray.Create;
    for I := 0 to High(FOpenFiles) do
    begin
      FileObj := TJSONObject.Create;
      FileObj.Add('path', FOpenFiles[I].Path);
      FileObj.Add('caretLine', FOpenFiles[I].CaretLine);
      FileObj.Add('caretCol', FOpenFiles[I].CaretCol);
      FileObj.Add('scrollTop', FOpenFiles[I].ScrollTop);
      FileObj.Add('scrollLeft', FOpenFiles[I].ScrollLeft);
      FileObj.Add('tabOrder', FOpenFiles[I].TabOrder);
      FilesArr.Add(FileObj);
    end;
    RootObj.Add('openFiles', FilesArr);

    { Write to file }
    JSONStr := RootObj.FormatJSON;
    FS := TFileStream.Create(FSessionFile, fmCreate);
    try
      if Length(JSONStr) > 0 then
        FS.Write(JSONStr[1], Length(JSONStr));
    finally
      FS.Free;
    end;
  finally
    RootObj.Free;
  end;
end;

end.
