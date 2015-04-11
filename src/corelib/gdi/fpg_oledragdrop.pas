{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit implements the OLE Drag-n-Drop functionality of Windows.
      This unit is implemented based on the articles posted on
        http://www.catch22.net/tuts/dragdrop/
}
unit fpg_OLEDragDrop;

{$mode delphi}{$H+}

interface

uses
  Windows, Classes, ActiveX, ShellAPI, fpg_base;

const
  CFSTR_FILENAMEMAPA          = 'FileNameMap';            { CF_FILENAMEMAPA }
  CFSTR_FILENAMEMAP           = CFSTR_FILENAMEMAPA;
  CFSTR_FILEDESCRIPTORA       = 'FileGroupDescriptor';    { CF_FILEGROUPDESCRIPTORA }
  CFSTR_FILEDESCRIPTOR        = CFSTR_FILEDESCRIPTORA;
  CFSTR_FILECONTENTS          = 'FileContents';           { CF_FILECONTENTS }

type
  TfpgOLEFormatEtcList = class(TList)
  private
    function    GetFormatEtc(Index: Integer): PFormatEtc;
  protected
    procedure   Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor CreateCopy(AFormatEtcList: TfpgOLEFormatEtcList);
    property    FormatEtc[Index: Integer]: PFormatEtc read GetFormatEtc; default;
  end;


  TfpgOLEStgMediumList = class(TList)
  private
    function    GetStgMedium(Index: Integer): PStgMedium;
  protected
    procedure   Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property    StgMedium[Index: Integer]: PStgMedium read GetStgMedium; default;
  end;


  TfpgOLEDropSource = class(TInterfacedObject, IDropSource)
  private
    { IDropSource }
    {$IF FPC_FULLVERSION>=20601}
    function    QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD):HResult; StdCall;
    function    GiveFeedback(dwEffect: DWORD): HResult; StdCall;
    {$ELSE}
    function    QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult; stdcall;
    function    GiveFeedback(dwEffect: longint): HResult; stdcall;
    {$ENDIF}
  end;


  TfpgOLEDragDropEffect = (deNone, deCopy, deMove, deLink);
  TfpgOLEDragEnterEvent = procedure(Sender: TObject; DataObj: IDataObject; KeyState: Longint; PT: TPoint; var Effect: DWORD) of object;
  TfpgOLEDragOverEvent = procedure(Sender: TObject; KeyState: Longint; PT: TPoint; var Effect: TfpgOLEDragDropEffect) of object;
  TfpgOLEDragDropEvent = procedure(Sender: TObject; DataObj: IDataObject; KeyState: Longint; PT: TPoint; Effect: TfpgOLEDragDropEffect) of object;


  TfpgOLEDropTarget = class(TObject, IInterface, IDropTarget)
  private
    FDropTarget: TfpgWindowBase;
    FRegistered: Boolean;
    FOnDragEnter: TfpgOLEDragEnterEvent;
    FOnDragOver: TfpgOLEDragOverEvent;
    FOnDragLeave: TNotifyEvent;
    FOnDragDrop: TfpgOLEDragDropEvent;
  private
    { IInterface }
    function    QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out obj): LongInt; stdcall;
    function    _AddRef: longint; stdcall;
    function    _Release: longint; stdcall;
    { IDropTarget }
    function    DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;StdCall;
    function    DragOver(grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
    function    DragLeave: HResult; stdcall;
    function    Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
  protected
    procedure   DoDragEnter(DataObj: IDataObject; KeyState: Longint; PT: TPoint; var Effect: DWORD); virtual;
    procedure   DoDragOver(KeyState: Longint; PT: TPoint; var Effect: TfpgOLEDragDropEffect); virtual;
    procedure   DoDragLeave;
    procedure   DoDragDrop(DataObj: IDataObject; KeyState: Longint; PT: TPoint; Effect: TfpgOLEDragDropEffect); virtual;
  public
    property    OnDragEnter: TfpgOLEDragEnterEvent read FOnDragEnter write FOnDragEnter;
    property    OnDragOver: TfpgOLEDragOverEvent read FOnDragOver write FOnDragOver;
    property    OnDragLeave: TNotifyEvent read FOnDragLeave write FOnDragLeave;
    property    OnDragDrop: TfpgOLEDragDropEvent read FOnDragDrop write FOnDragDrop;
  public
    constructor Create(ADropTargetWidget: TfpgWindowBase); reintroduce; { Actually a TfpgWidget }
    destructor  Destroy; override;
    procedure   RegisterDragDrop;
    procedure   RevokeDragDrop;
    property    DropTarget: TfpgWindowBase read FDropTarget;
  end;


  TfpgOLEDataObject = class(TInterfacedObject, IDataObject)
  private
    FFormatEtcList: TfpgOLEFormatEtcList;
    FStgMediumList: TfpgOLEStgMediumList;
    function    LookupFormatEtc(AFormat: TFormatEtc): Integer;
  protected
    { IDataObject }
    function    GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function    GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function    QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function    GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult; stdcall;
    function    SetData(const formatetc: TFormatEtc; const medium: TStgMedium; fRelease: BOOL): HResult; stdcall;
    function    EnumFormatEtc(dwDirection: DWORD; out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function    DAdvise(const formatetc: TFormatEtc; advf: DWORD; const advSink: IAdviseSink; out dwConnection: DWORD): HResult; stdcall;
    function    DUnadvise(dwConnection: DWORD): HResult; stdcall;
    function    EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  public
    constructor Create; overload;
    constructor Create(AFormatEtcList: TfpgOLEFormatEtcList); overload;
    destructor  Destroy; override;
    property    FormatEtcList: TfpgOLEFormatEtcList read FFormatEtcList;
    property    StgMediumList: TfpgOLEStgMediumList read FStgMediumList;
  end;


  TfpgOLEEnumFormatEtc = class(TInterfacedObject, IEnumFORMATETC)
  private
    FFormatEtcList: TfpgOLEFormatEtcList;
    FIndex: Integer;
  protected
    { IEnumFORMATETC }
    function    Next(celt: ULong; out elt:FormatEtc; pceltFetched: pULong=nil): HResult; stdcall;
    function    Skip(celt: ULong): HResult; stdcall;
    function    Reset: HResult; stdcall;
    function    Clone(out Enum: IEnumFormatEtc): HResult; stdcall;
  public
    constructor Create(AFormatEtcList: TfpgOLEFormatEtcList);
    destructor  Destroy; override;
  end;


  TDragFilesSource = class(TObject)
  private
    FFileNames: TStrings;
    FAliasFileNames: TStrings;
    function    GetAliasFileNames: TStrings;
    function    GetSourceFileNames: TStrings;
    procedure   SetAliasFileNames(const Value: TStrings);
    procedure   SetSourceFileNames(const Value: TStrings);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Execute;
    property    SourceFileNames: TStrings read GetSourceFileNames write SetSourceFileNames;
    property    AliasFileNames: TStrings read GetAliasFileNames write SetAliasFileNames;
  end;


  TDragAcceptFilesEvent = function(Sender: TObject; FileNames: TStrings): Boolean of object;
  TDragAcceptPositionEvent = function(Sender: TObject; PT: TPoint): Boolean of object;
  TDropFilesEvent = procedure(Sender: TObject; PT: TPoint; FileNames: TStrings) of object;


  TDragFilesTarget = class(TfpgOLEDropTarget)
  private
    FDragAcceptFiles: Boolean;
    FOnDragAcceptFiles: TDragAcceptFilesEvent;
    FOnDragAcceptPosition: TDragAcceptPositionEvent;
    FOnDropFiles: TDropFilesEvent;
    procedure   GetFileNamesFromDropHandle(DropHandle: HDROP; SL: TStrings);
    procedure   StreamToFile(Stream: IStream; const FileName: string);
  protected
    function    DoDragAcceptFiles(DataObj: IDataObject): Boolean;
    function    DoDragAcceptPosition(PT: TPoint): Boolean;
    procedure   DoDropFiles(DataObj: IDataObject; PT: TPoint);
    procedure   DoDragEnter(DataObj: IDataObject; KeyState: Longint; PT: TPoint; var Effect: DWORD); override;
    procedure   DoDragOver(KeyState: Longint; PT: TPoint; var Effect: TfpgOLEDragDropEffect); override;
    procedure   DoDragDrop(DataObj: IDataObject; KeyState: Longint; PT: TPoint; Effect: TfpgOLEDragDropEffect); override;
  public
    property    OnDragAcceptFiles: TDragAcceptFilesEvent read FOnDragAcceptFiles write FOnDragAcceptFiles;
    property    OnDragAcceptPosition: TDragAcceptPositionEvent read FOnDragAcceptPosition write FOnDragAcceptPosition;
    property    OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
  end;


function WindowsMimeLookup(const CFFormat: string): string;
function WindowsClipboardLookup(const AMime: string; var IsTranslated: Boolean): DWORD;
function EnumDataToStringList(DataObj: IDataObject): TStringList;
function GetFormatEtc(const CFFormat: DWORD): FORMATETC;

implementation

uses
  SysUtils, ShlObj, fpg_widget;

var
  CF_FILENAMEMAP: Cardinal;
  CF_FILEDESCRIPTOR: Cardinal;
  CF_FILECONTENTS: Cardinal;


function WindowsMimeLookup(const CFFormat: string): string;
begin
  { replace known clipboard formats with mime types }
  if CFFormat = 'CF_TEXT' then
    Result := 'text/plain'
  else if CFFormat = 'CF_UNICODETEXT' then
    Result := 'text/plain'
  else if CFFormat = 'CF_OEMTEXT' then
    Result := 'text/plain'
  else if CFFormat = 'CF_HDROP' then
    Result := 'text/uri-list'
  else if CFFormat = 'CF_RICHTEXT' then
    Result := 'text/html'
  else
    Result := CFFormat;
end;

function WindowsClipboardLookup(const AMime: string; var IsTranslated: Boolean): DWORD;
begin
  { TODO: We need to improve this implementation }
  if AMime = 'text/html' then
  begin
    { We don't want duplicate CF_TEXT in DataObject, so register some of our
      known convenience types (from TfpgMimeData) as-is }
    IsTranslated := False;
    Result := RegisterClipboardFormat('text/html');
  end
  else if Pos('text/', AMime) = 1 then
  begin
    IsTranslated := True;
    Result := CF_TEXT;  // fallback result
  end
  else
  begin
    IsTranslated := False;
    Result := RegisterClipboardFormat(PChar(AMime));
  end;
end;

function WindowsClipboardFormatToString(const CFFormat: integer): string;
begin
  { replace know clipboard formats with mime types }
  case CFFormat of
    CF_DIF          : result := 'CF_DIF';
    CF_DIB          : result := 'CF_DIB';
    CF_TEXT         : result := 'CF_TEXT';
    CF_SYLK         : result := 'CF_SYLK';
    CF_TIFF         : result := 'CF_TIFF';
    CF_RIFF         : result := 'CF_RIFF';
    CF_WAVE         : result := 'CF_WAVE';
    CF_HDROP        : result := 'CF_HDROP';
    CF_BITMAP       : result := 'CF_BITMAP';
    CF_LOCALE       : result := 'CF_LOCALE';
    CF_OEMTEXT      : result := 'CF_OEMTEXT';
    CF_PALETTE      : result := 'CF_PALETTE';
    CF_PENDATA      : result := 'CF_PENDATA';
    CF_UNICODETEXT  : result := 'CF_UNICODETEXT';
    CF_ENHMETAFILE  : result := 'CF_ENHMETAFILE';
    CF_METAFILEPICT : result := 'CF_METAFILEPICT';
    else
      Result := Format('unknown (%d)', [CFFormat]);
  end;
end;

function EnumDataToStringList(DataObj: IDataObject): TStringList;
var
	FE: FORMATETC;
	EnumFormats: IEnumFORMATETC;
	num: integer;
  lname: string;
  lMimeName: string;
  FormatName: array[0..MAX_PATH] of Char;
  i: integer;
begin
  if DataObj.EnumFormatEtc(DATADIR_GET, EnumFormats) <> S_OK then
    raise Exception.Create('EnumDataToStringList: Failed to get EnumFormatEtc interface');

  Result := TStringList.Create;
  EnumFormats.Reset;
  while EnumFormats.Next(1, FE, @num) = S_OK do
  begin
    lName := '';
    i := GetClipboardFormatName(FE.cfFormat,FormatName,MAX_PATH);
    if i <> 0 then
    begin
      lName := FormatName;
    end
    else
    begin
      lName := WindowsClipboardFormatToString(FE.cfFormat);
    end;
    Result.Add(lName);
  end;
end;

function GetFormatEtc(const CFFormat: DWORD): FORMATETC;
begin
  Result.CfFormat := CFFormat;
  Result.Ptd := nil;
  Result.dwAspect := DVASPECT_CONTENT;
  Result.lindex := -1;
  Result.tymed := TYMED_HGLOBAL;
end;

procedure DeepCopyFormatEtc(P1, P2: PFormatEtc);
begin
  P2^ := P1^;
  if P1^.ptd <> nil then begin
    P2^.ptd := CoTaskMemAlloc(SizeOf(tagDVTARGETDEVICE));
    P2^.ptd^ := P1^.ptd^;
  end;
end;

function DupGlobalMem(hMem: HGLOBAL): HGLOBAL;
var
  len: DWORD;
  Source: Pointer;
  Dest: HGLOBAL;
begin
  len := GlobalSize(hMem);
  Source := GlobalLock(hMem);
  Dest := GlobalAlloc(GMEM_FIXED, len);
  Move(Source^, Pointer(Dest)^, len);
  GlobalUnlock(hMem);
  Result := Dest;
end;

{ TDragFilesSource }

constructor TDragFilesSource.Create;
begin
  inherited Create;
  FFileNames := TStringList.Create;
  FAliasFileNames := TStringList.Create;
end;

destructor TDragFilesSource.Destroy;
begin
  FreeAndNil(FFileNames);
  FreeAndNil(FAliasFileNames);
  inherited Destroy;
end;

procedure TDragFilesSource.Execute;
var
  DataObject: TfpgOLEDataObject;
  DropSource: TfpgOLEDropSource;
  dwEffect: DWORD;
  dwResult: HRESULT;
  I: Integer;
  F: PFormatEtc;
  S: string;
  M: PStgMedium;
begin
  DataObject := TfpgOLEDataObject.Create;

  { append filenames as one long string delimited by #0. ie: something like a PChar }
  S := '';
  for I := 0 to FFileNames.Count - 1 do
  begin
    SetLength(S, Length(S)+Length(FFileNames[I])+1);
    Move(FFileNames[I][1], S[Length(S)-Length(FFileNames[I])], Length(FFileNames[I]));
    S[Length(S)] := #0;
  end;
  SetLength(S, Length(S)+1);
  S[Length(S)] := #0;

  { description of data we are sending }
  New(F);
  F^.cfFormat := CF_HDROP;
  F^.ptd := nil;
  F^.dwAspect := DVASPECT_CONTENT;
  F^.lindex := -1;
  F^.tymed := TYMED_HGLOBAL;
  DataObject.FormatEtcList.Add(F);

  { storage for data we are sending }
  New(M);
  M^.tymed := TYMED_HGLOBAL;
  M^.hGlobal := Cardinal(GlobalAlloc(GMEM_FIXED, SizeOf(TDropFiles)+Length(S)));
  PDropFiles(M^.hGlobal)^.pFiles := SizeOf(TDropFiles);
  PDropFiles(M^.hGlobal)^.pt := Point(0,0);
  PDropFiles(M^.hGlobal)^.fNC := FALSE;
  PDropFiles(M^.hGlobal)^.fWide := FALSE;
  Move(S[1], PChar(M^.hGlobal+SizeOf(TDropFiles))^, Length(S));
  DataObject.StgMediumList.Add(M);

  if (FAliasFileNames.Count > 0) and (FAliasFileNames.Count = FFileNames.Count) then
  begin
    { append filename aliases as one long string delimited by #0. ie: something like a PChar }
    S := '';
    for I := 0 to FAliasFileNames.Count - 1 do
    begin
      SetLength(S, Length(S)+Length(FAliasFileNames[I])+1);
      Move(FAliasFileNames[I][1], S[Length(S)-Length(FAliasFileNames[I])], Length(FAliasFileNames[I]));
      S[Length(S)] := #0;
    end;
    SetLength(S, Length(S)+1);
    S[Length(S)] := #0;

    { description of data we are sending }
    New(F);
    F^.cfFormat := CF_FILENAMEMAP;
    F^.ptd := nil;
    F^.dwAspect := DVASPECT_CONTENT;
    F^.lindex := -1;
    F^.tymed := TYMED_HGLOBAL;
    DataObject.FormatEtcList.Add(F);

    { storage for data we are sending }
    New(M);
    M^.tymed := TYMED_HGLOBAL;
    M^.hGlobal := Cardinal(GlobalAlloc(GMEM_FIXED, Length(S)));
    Move(S[1], PChar(M^.hGlobal)^, Length(S));
    DataObject.StgMediumList.Add(M);
  end;

  DropSource := TfpgOLEDropSource.Create;
  dwResult := ActiveX.DoDragDrop(DataObject as IDataObject, DropSource as IDropSource, DROPEFFECT_COPY, @dwEffect);

  if dwResult = DRAGDROP_S_DROP then
  begin
    if dwEffect = DROPEFFECT_COPY then
    begin
      // nothing to do. If this whas xxx_MOVE, we would remove data from source
    end;
  end;
end;

function TDragFilesSource.GetAliasFileNames: TStrings;
begin
  Result := FAliasFileNames;
end;

function TDragFilesSource.GetSourceFileNames: TStrings;
begin
  Result := FFileNames;
end;

procedure TDragFilesSource.SetAliasFileNames(const Value: TStrings);
begin
  FAliasFileNames.Assign(Value);
end;

procedure TDragFilesSource.SetSourceFileNames(const Value: TStrings);
begin
  FFileNames.Assign(Value);
end;

{ TfpgOLEDropSource }

{$IF FPC_FULLVERSION>=20601}
function TfpgOLEDropSource.GiveFeedback(dwEffect: DWORD): HResult;
{$ELSE}
function TfpgOLEDropSource.GiveFeedback(dwEffect: longint): HResult;
{$ENDIF}
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

{$IF FPC_FULLVERSION>=20601}
function TfpgOLEDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD):HResult;
{$ELSE}
function TfpgOLEDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: LongInt): HResult;
{$ENDIF}
begin
  if FEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else if (grfKeyState and (MK_LBUTTON or MK_RBUTTON) = 0) then
    Result := DRAGDROP_S_DROP
  else
    Result := S_OK;
  {$IFDEF DND_DEBUG}
  writeln('TfpgOLEDropSource.QueryContinueDrag  Result = ', Result);
  {$ENDIF}
end;

{ TfpgOLEDataObject }

constructor TfpgOLEDataObject.Create(AFormatEtcList: TfpgOLEFormatEtcList);
begin
  inherited Create;
  FFormatEtcList := TfpgOLEFormatEtcList.CreateCopy(AFormatEtcList);
  FStgMediumList := TfpgOLEStgMediumList.Create;
end;

constructor TfpgOLEDataObject.Create;
begin
  inherited Create;
  FFormatEtcList := TfpgOLEFormatEtcList.Create;
  FStgMediumList := TfpgOLEStgMediumList.Create;
end;

function TfpgOLEDataObject.DAdvise(const formatetc: TFormatEtc; advf: DWORD;
  const advSink: IAdviseSink; out dwConnection: DWORD): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

destructor TfpgOLEDataObject.Destroy;
begin
  FreeAndNil(FFormatEtcList);
  FreeAndNil(FStgMediumList);
  inherited Destroy;
end;

function TfpgOLEDataObject.DUnadvise(dwConnection: DWORD): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TfpgOLEDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TfpgOLEDataObject.EnumFormatEtc(dwDirection: DWORD;
  out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  if dwDirection = DATADIR_GET then
  begin
    enumFormatEtc := TfpgOLEEnumFormatEtc.Create(FFormatEtcList) as IEnumFormatEtc;
    Result := S_OK;
  end
  else begin
    Result := E_NOTIMPL;
  end;
end;

function TfpgOLEDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HResult;
begin
  // Apparently we have to set this field to NULL even though we don't do anything else
  formatetcOut.ptd := nil;
  Result := E_NOTIMPL;
end;

function TfpgOLEDataObject.GetData(const formatetcIn: TFormatEtc;
  out medium: TStgMedium): HResult;
var
  Idx: Integer;
begin
  Idx := LookupFormatEtc(formatetcIn);
  if Idx = -1 then
    Result := DV_E_FORMATETC
  else
  begin
    medium.tymed := FFormatEtcList[Idx]^.tymed;
    medium.PUnkForRelease := nil;
    if medium.tymed = TYMED_HGLOBAL then
    begin
      medium.hGlobal := DupGlobalMem(FStgMediumList[Idx]^.hGlobal);
      Result := S_OK;
    end
    else
      Result := DV_E_FORMATETC;
  end;
end;

function TfpgOLEDataObject.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HResult;
begin
  Result := DV_E_FORMATETC;
end;

function TfpgOLEDataObject.LookupFormatEtc(AFormat: TFormatEtc): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FFormatEtcList.Count - 1 do begin
    if (FFormatEtcList[I]^.cfFormat = AFormat.cfFormat) and
       (FFormatEtcList[I]^.dwAspect = AFormat.dwAspect) and
       (FFormatEtcList[I]^.tymed = AFormat.tymed) then begin
      Result := I;
      Break;
    end;
  end;
end;

function TfpgOLEDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  if LookupFormatEtc(formatetc) >= 0 then begin
    Result := S_OK;
  end
  else begin
    Result := DV_E_FORMATETC;
  end;
end;

function TfpgOLEDataObject.SetData(const formatetc: TFormatEtc;
  const medium: TStgMedium; fRelease: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

{ TfpgOLEEnumFormatEtc }

function TfpgOLEEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HResult;
var
  C: TfpgOLEEnumFormatEtc;
begin
  // make a duplicate enumerator
  C := TfpgOLEEnumFormatEtc.Create(FFormatEtcList);
  // manually set the index state
  C.FIndex := FIndex;
  Enum := C as IEnumFormatEtc;
  Result := S_OK;
end;

constructor TfpgOLEEnumFormatEtc.Create(AFormatEtcList: TfpgOLEFormatEtcList);
begin
  FFormatEtcList := TfpgOLEFormatEtcList.CreateCopy(AFormatEtcList);
  FIndex := 0;
end;

destructor TfpgOLEEnumFormatEtc.Destroy;
begin
  FreeAndNil(FFormatEtcList);
  inherited;
end;

function TfpgOLEEnumFormatEtc.Next(celt: ULong; out elt:FormatEtc;
  pceltFetched: pULong): HResult;
var
  Copied: Integer;
  OutBuf: PFormatEtc;
begin
  // copy the FORMATETC structures into the caller's buffer
  OutBuf := PFormatEtc(@elt);
  Copied := 0;
  while(FIndex < FFormatEtcList.Count) and (Copied < celt) do begin
    DeepCopyFormatEtc(FFormatEtcList[FIndex], OutBuf);
    OutBuf := PFormatEtc(Cardinal(OutBuf) + SizeOf(TFormatEtc));
    Inc(Copied);
    FIndex := FIndex + 1;
  end;

  // store result
  if (pceltFetched <> nil) then
    pceltFetched^ := Copied;

  // did we copy all that was requested?
  if (Copied = celt) then Result := S_OK
  else Result := S_FALSE;
end;

function TfpgOLEEnumFormatEtc.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TfpgOLEEnumFormatEtc.Skip(celt: ULong): HResult;
begin
  FIndex := FIndex + celt;
  if FIndex <= FFormatEtcList.Count then Result := S_OK
  else Result := S_FALSE;
end;

{ TfpgOLEFormatEtcList }

constructor TfpgOLEFormatEtcList.CreateCopy(AFormatEtcList: TfpgOLEFormatEtcList);
var
  I: Integer;
  P: PFormatEtc;
begin
  Create;
  for I := 0 to AFormatEtcList.Count - 1 do begin
    New(P);
    DeepCopyFormatEtc(AFormatEtcList[I], P);
    Add(P);
  end;
end;

function TfpgOLEFormatEtcList.GetFormatEtc(Index: Integer): PFormatEtc;
begin
  Result := PFormatEtc(Get(Index));
end;

procedure TfpgOLEFormatEtcList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if PFormatEtc(Ptr)^.ptd <> nil then begin
      CoTaskMemFree(PFormatEtc(Ptr)^.ptd);
    end;
    Dispose(PFormatEtc(Ptr));
  end;
  inherited;
end;

{ TfpgOLEStgMediumList }

function TfpgOLEStgMediumList.GetStgMedium(Index: Integer): PStgMedium;
begin
  Result := PStgMedium(Get(Index));
end;

procedure TfpgOLEStgMediumList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    if PStgMedium(Ptr)^.hGlobal <> 0 then begin
      GlobalFree(PStgMedium(Ptr)^.hGlobal);
    end;
    Dispose(Ptr);
  end;
  inherited;
end;

{ TfpgOLEDropTarget }

function TfpgOLEDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
//var
//  Effect: TfpgOLEDragDropEffect;
begin
  //dwEffect := DROPEFFECT_NONE;
  //Effect := deNone;
  DoDragEnter(dataObj, grfKeyState, pt, dwEffect);
  //case Effect of
  //  deNone: dwEffect := DROPEFFECT_NONE;
  //  deCopy: dwEffect := DROPEFFECT_COPY;
  //  deMove: dwEffect := DROPEFFECT_MOVE;
  //  deLink: dwEffect := DROPEFFECT_LINK;
  //end;
  Result := S_OK;
end;

function TfpgOLEDropTarget.DragLeave: HResult;
begin
  Result := S_OK;
  DoDragLeave;
end;

function TfpgOLEDropTarget.DragOver(grfKeyState: DWORD; pt: TPoint;
  var dwEffect: DWORD): HResult;
var
  Effect: TfpgOLEDragDropEffect;
begin
  if ((MK_SHIFT and grfKeyState) = MK_SHIFT) and
     ((dwEffect and DROPEFFECT_MOVE) = DROPEFFECT_MOVE) then begin
    Effect := deMove;
  end;
  if ((MK_CONTROL and grfKeyState) = MK_CONTROL) and
     ((dwEffect and DROPEFFECT_COPY) = DROPEFFECT_COPY) then begin
    Effect := deCopy;
  end;
  if dwEffect and DROPEFFECT_COPY > 0 then Effect := deCopy
  else if dwEffect and DROPEFFECT_MOVE  > 0 then Effect := deMove
  else if dwEffect and DROPEFFECT_LINK > 0 then Effect := deLink
  else Effect := deNone;
  DoDragOver(grfKeyState, pt, Effect);
  case Effect of
    deNone: dwEffect := DROPEFFECT_NONE;
    deCopy: dwEffect := DROPEFFECT_COPY;
    deMove: dwEffect := DROPEFFECT_MOVE;
    deLink: dwEffect := DROPEFFECT_LINK;
  end;
  Result := S_OK;
end;

function TfpgOLEDropTarget.Drop(const dataObj: IDataObject;
  grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
var
  Effect: TfpgOLEDragDropEffect;
begin
  if dwEffect and DROPEFFECT_COPY > 0 then
    Effect := deCopy
  else if dwEffect and DROPEFFECT_MOVE  > 0 then
    Effect := deMove
  else if dwEffect and DROPEFFECT_LINK > 0 then
    Effect := deLink
  else
    Effect := deNone;
  DoDragDrop(dataObj, grfKeyState, pt, Effect);
  Result := S_OK;
end;

function TfpgOLEDropTarget._AddRef: longint;
begin
  Result := 1;
end;

function TfpgOLEDropTarget._Release: longint;
begin
  Result := 1;
end;

function TfpgOLEDropTarget.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out obj): longint; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

constructor TfpgOLEDropTarget.Create(ADropTargetWidget: TfpgWindowBase);
begin
  inherited Create;
  FDropTarget := ADropTargetWidget;
  FRegistered := False;
end;

procedure TfpgOLEDropTarget.RegisterDragDrop;
begin
  ActiveX.RegisterDragDrop(TfpgWidget(FDropTarget).WinHandle, Self as IDropTarget);
  FRegistered := True;
end;

procedure TfpgOLEDropTarget.RevokeDragDrop;
begin
  FRegistered := False;
  ActiveX.RevokeDragDrop(TfpgWidget(FDropTarget).WinHandle);
end;

destructor TfpgOLEDropTarget.Destroy;
begin
  if FRegistered then RevokeDragDrop;
  inherited;
end;

procedure TfpgOLEDropTarget.DoDragEnter(DataObj: IDataObject;
  KeyState: LongInt; PT: TPoint; var Effect: DWORD);
begin
  if Assigned(FOnDragEnter) then begin
    FOnDragEnter(Self, DataObj, KeyState, PT, Effect);
  end;
end;

procedure TfpgOLEDropTarget.DoDragOver(KeyState: LongInt; PT: TPoint;
  var Effect: TfpgOLEDragDropEffect);
begin
  if Assigned(FOnDragOver) then begin
    FOnDragOver(Self, KeyState, PT, Effect);
  end;
end;

procedure TfpgOLEDropTarget.DoDragLeave;
begin
  if Assigned(FOnDragLeave) then
    FOnDragLeave(self);
end;

procedure TfpgOLEDropTarget.DoDragDrop(DataObj: IDataObject; KeyState: LongInt;
  PT: TPoint; Effect: TfpgOLEDragDropEffect);
begin
  if Assigned(FOnDragDrop) then begin
    FOnDragDrop(Self, DataObj, KeyState, PT, Effect);
  end;
end;

{ TDragFilesTarget }

function TDragFilesTarget.DoDragAcceptFiles(DataObj: IDataObject): Boolean;
const
  FormatEtcHDrop:           TFormatEtc = (cfFormat:CF_HDROP;ptd:nil;dwAspect:DVASPECT_CONTENT;lindex:-1;tymed:TYMED_HGLOBAL);
  FormatEtcFileDescriptor:  TFormatEtc = (cfFormat:0;ptd:nil;dwAspect:DVASPECT_CONTENT;lindex:-1;tymed:TYMED_HGLOBAL);
  FormatEtcFileContents:    TFormatEtc = (cfFormat:0;ptd:nil;dwAspect:DVASPECT_CONTENT;lindex:-1;tymed:TYMED_ISTREAM);
var
  StgMedium: TStgMedium;
  DropHandle: HDROP;
  EnumFormatEtc: IEnumFORMATETC;
  FE: TFormatEtc;
  FetchedCount: Longint;
  FormatName: array[0..MAX_PATH] of Char;
  FileGroupDescriptor: PFileGroupDescriptorA;
  I, Count: Integer;
  FileDescriptor: TFileDescriptorA;
  FileNames: TStringList;
begin
  FileNames := TStringList.Create;
  try
    if Assigned(FOnDragAcceptFiles) then
    begin
      Result := False;
      FormatEtcFileDescriptor.cfFormat := CF_FILEDESCRIPTOR;
      FormatEtcFileContents.cfFormat := CF_FILECONTENTS;

      if (DataObj.QueryGetData(FormatEtcHDrop) = S_OK) and
         (DataObj.GetData(FormatEtcHDrop,StgMedium) = S_OK) then
      begin
        DropHandle := StgMedium.hGlobal;
        GetFileNamesFromDropHandle(DropHandle, FileNames);
        Result := FOnDragAcceptFiles(Self, FileNames);
        ReleaseStgMedium(StgMedium);
      end
      else
      if (DataObj.QueryGetData(FormatEtcFileDescriptor) = S_OK) and
         (DataObj.QueryGetData(FormatEtcFileContents) = S_OK) and
         (DataObj.GetData(FormatEtcFileDescriptor,StgMedium) = S_OK) then
      begin
        FileGroupDescriptor := GlobalLock(StgMedium.hGlobal);
        if FileGroupDescriptor <> nil then
        begin
          Count := FileGroupDescriptor^.cItems;
          I := 0;
          while I < Count do
          begin
            FileDescriptor := FileGroupDescriptor^.fgd[I];
            FileNames.Add(FileDescriptor.cFileName);
            Inc(I);
          end;
          GlobalUnlock(StgMedium.hGlobal);
        end;
        Result := FOnDragAcceptFiles(Self, FileNames);
        ReleaseStgMedium(StgMedium);
      end
      else
      begin
//        DataObj.EnumFormatEtc(DATADIR_GET, EnumFormatEtc);
//        EnumFormatEtc.Reset;
//        while EnumFormatEtc.Next(1, FE, @FetchedCount) = S_OK do begin
//          GetClipboardFormatName(FE.cfFormat,FormatName,MAX_PATH);
//          ShowMessage(FormatName);
//        end;
      end;
    end
    else
    begin
      Result := True;
    end;
  finally
    FileNames.Free;
  end;
end;

procedure TDragFilesTarget.DoDragEnter(DataObj: IDataObject;
  KeyState: LongInt; PT: TPoint; var Effect: DWORD);
begin
  FDragAcceptFiles := DoDragAcceptFiles(DataObj);
  if FDragAcceptFiles and DoDragAcceptPosition(PT) then
    inherited DoDragEnter(DataObj, KeyState, PT, Effect)
  else
    Effect := DROPEFFECT_NONE;
end;

procedure TDragFilesTarget.DoDragOver(KeyState: LongInt; PT: TPoint; var Effect: TfpgOLEDragDropEffect);
begin
  if FDragAcceptFiles and DoDragAcceptPosition(PT) then
    inherited DoDragOver(KeyState, PT, Effect)
  else
    Effect := deNone;
end;

procedure TDragFilesTarget.DoDragDrop(DataObj: IDataObject;
  KeyState: LongInt; PT: TPoint; Effect: TfpgOLEDragDropEffect);
begin
  DoDropFiles(DataObj, PT);
  inherited;
end;

function TDragFilesTarget.DoDragAcceptPosition(PT: TPoint): Boolean;
begin
  if Assigned(FOnDragAcceptPosition) then begin
    Result := FOnDragAcceptPosition(Self, PT);
  end else begin
    Result := True;
  end;
end;

procedure TDragFilesTarget.DoDropFiles(DataObj: IDataObject; PT: TPoint);
const
  FormatEtcHDrop: TFormatEtc = (cfFormat:CF_HDROP;ptd:nil;dwAspect:DVASPECT_CONTENT;lindex:-1;tymed:TYMED_HGLOBAL);
  FormatEtcFileDescriptor: TFormatEtc =
    (cfFormat:0;ptd:nil;dwAspect:DVASPECT_CONTENT;lindex:-1;tymed:TYMED_HGLOBAL);
  FormatEtcFileContents: TFormatEtc =
    (cfFormat:0;ptd:nil;dwAspect:DVASPECT_CONTENT;lindex:-1;tymed:TYMED_ISTREAM);
var
  StgMedium, StgMediumContents: TStgMedium;
  DropHandle: HDROP;
  FileNames: TStringList;
  FileGroupDescriptor: PFileGroupDescriptorA;
  I, Count: Integer;
  FileDescriptor: TFileDescriptorA;
  Path: array[0..MAX_PATH] of Char;
  TempFileName: string;
begin
  if not Assigned(FOnDropFiles) then Exit;
  FileNames := TStringList.Create;
  try
    FormatEtcFileDescriptor.cfFormat := CF_FILEDESCRIPTOR;
    FormatEtcFileContents.cfFormat := CF_FILECONTENTS;
    if (DataObj.QueryGetData(FormatEtcHDrop) = S_OK) and
       (DataObj.GetData(FormatEtcHDrop,StgMedium) = S_OK) then begin
      DropHandle := StgMedium.hGlobal;
      GetFileNamesFromDropHandle(DropHandle, FileNames);
      FOnDropFiles(Self, PT, FileNames);
      ReleaseStgMedium(StgMedium);
    end else
    if (DataObj.QueryGetData(FormatEtcFileDescriptor) = S_OK) and
       (DataObj.QueryGetData(FormatEtcFileContents) = S_OK) and
       (DataObj.GetData(FormatEtcFileDescriptor,StgMedium) = S_OK) then begin
      GetTempPath(MAX_PATH, Path);
      GetTempFileName(Path, 'PXM', 0, Path);
      FileGroupDescriptor := GlobalLock(StgMedium.hGlobal);
      if FileGroupDescriptor <> nil then begin
        Count := FileGroupDescriptor^.cItems;
        I := 0;
        while I < Count do begin
          FileDescriptor := FileGroupDescriptor^.fgd[I];
          TempFileName := ChangeFileExt(Path, ExtractFileExt(FileDescriptor.cFileName));
          FormatEtcFileContents.lindex := I;
          if (DataObj.GetData(FormatEtcFileContents,StgMediumContents) = S_OK) then begin
            StreamToFile(IStream(StgMediumContents.pstm), TempFileName);
            FileNames.Clear;
            FileNames.Add(TempFileName);
            FOnDropFiles(Self, PT, FileNames);
            ReleaseStgMedium(StgMediumContents);
          end;
          Inc(I);
        end;
        GlobalUnlock(StgMedium.hGlobal);
      end;
      FOnDropFiles(Self, PT, FileNames);
      ReleaseStgMedium(StgMedium);
      ReleaseStgMedium(StgMediumContents);
    end;
  finally
    FileNames.Free;
  end;
end;

procedure TDragFilesTarget.GetFileNamesFromDropHandle(DropHandle: HDROP; SL: TStrings);
var
  I: Integer;
  Path: array[0..MAX_PATH] of Char;
begin
  for I := 0 to DragQueryFile(DropHandle, $FFFFFFFF, nil, 0) do begin
    DragQueryFile(DropHandle, I, Path, MAX_PATH);
    SL.Add(Path);
  end;
  DragFinish(DropHandle);
end;

procedure TDragFilesTarget.StreamToFile(Stream: IStream; const FileName: string);
const
  BLOCK_SIZE = 4096;
var
  BytesRead: Longint;
  FileStream: TFileStream;
  Buffer: array[0..BLOCK_SIZE] of Byte;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    while (Stream.Read(@Buffer[0], BLOCK_SIZE, @BytesRead) = S_OK) and (BytesRead > 0) do begin
      FileStream.Write(Buffer, BytesRead);
    end;
  finally
    FileStream.Free;
  end;
end;

initialization
  CF_FILENAMEMAP := RegisterClipboardFormat(CFSTR_FILENAMEMAP);
  CF_FILEDESCRIPTOR := RegisterClipboardFormat(CFSTR_FILEDESCRIPTOR);
  CF_FILECONTENTS := RegisterClipboardFormat(CFSTR_FILECONTENTS);

finalization

end.

