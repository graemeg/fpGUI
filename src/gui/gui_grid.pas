unit gui_grid;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gui_customgrid;
  
type
  TfpgGrid = class(TfpgCustomGrid)
  published
    property    Columns;
    property    DefaultColWidth;
    property    DefaultRowHeight;
    property    Font;
    property    HeaderFont;
    property    BackgroundColor;
    property    FocusCol;
    property    FocusRow;
    property    RowSelect;
    property    ColumnCount;
    property    RowCount;
    property    ShowHeader;
    property    ShowGrid;
    property    HeaderHeight;
    property    ColResizing;
    property    ColumnWidth;
    property    OnFocusChange;
    property    OnRowChange;
  end;


  //*****************  Move these to CoreLib  ********************
  TFileEntryType = (etFile, etDir);
  TFileListSortOrder = (soNone, soFileName, soCSFileName, soFileExt, soSize, soTime);

  // A simple data object
  TFileEntry = class(TObject)
  private
    FAttributes: longword;
    FEntryType: TFileEntryType;
    FExtention: string;
    FGroupID: integer;
    FIsLink: boolean;
    FLinkTarget: string;
    FMode: longword;
    FModTime: TDateTime;
    FName: string;
    FOwnerID: integer;
    FSize: int64;
  public
    constructor Create;
    property    Name: string read FName write FName;
    property    Extention: string read FExtention write FExtention;
    property    Size: int64 read FSize write FSize;
    property    EntryType: TFileEntryType read FEntryType write FEntryType;
    property    IsLink: boolean read FIsLink write FIsLink;
    property    Attributes: longword read FAttributes write FAttributes;
    property    Mode: longword read FMode write FMode;  // only used by unix OS's
    property    ModTime: TDateTime read FModTime write FModTime;
    property    OwnerID: integer read FOwnerID write FOwnerID;
    property    GroupID: integer read FGroupID write FGroupID;
    property    LinkTarget: string read FLinkTarget write FLinkTarget;
  end;


  TFileList = class(TObject)
  private
    FEntries: TList;
    FDirectoryName: string;
    function    GetEntry(i: integer): TFileEntry;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Count: integer;
    function    ReadDirectory(const AFilemask: string; AShowHidden: boolean): integer;
    procedure   Clear;
    procedure   Sort(AOrder: TFileListSortOrder);
    property    Entry[i: integer]: TFileEntry read GetEntry;
    property    DirectoryName: string read FDirectoryName;
  end;
  
  
  TfpgFileGrid = class(TfpgCustomGrid)
  private
    FFileList: TFileList;
    FFixedFont: TfpgFont;
  protected
    function    GetRowCount: integer; override;
    procedure   DrawCell(ARow, ACol: integer; ARect: TfpgRect; AFlags: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CurrentEntry: TFileEntry;
    property    FixedFont: TfpgFont read FFixedFont;
    property    FileList: TFileList read FFileList;
  end;



implementation

uses
  gfx_utils
  {$IFDEF MSWINDOWS}
  ,Windows   // Graeme: temporary, just to see how the grid looks under Windows.
  {$ENDIF}
  ;
  

// *****  These two functions will be moving out of this unit soon!
  
function StringMatches(const astr, apat: string): boolean;
var
  pati, si: longint;
begin
  result := True;
  pati := 1;
  si := 1;
  while result and (si <= length(astr)) and (pati <= length(apat)) do
  begin
    if (apat[pati] = '?') or (apat[pati] = astr[si]) then
    begin
      inc(si);
      inc(pati);
    end
    else if (apat[pati] = '*') then
    begin
      while (pati <= length(apat)) and (apat[pati] in ['?','*']) do
        inc(pati);
      if pati > length(apat) then
      begin
        si := length(astr)+1;
        Break;   // * at the end
      end;

      while (si <= length(astr)) and (astr[si] <> apat[pati]) do
        inc(si);
      if si > length(astr) then
        result := False;
    end
    else
    begin
      result := False;
    end;
  end;

  result := result and (si > length(astr));
end;

// multiple patterns separated with ;
function FileNameMatches(const astr, apats: string): boolean;
var
  cpat: string;
  p: integer;
  s: string;
  astrupper: string;
begin
  astrupper := UpperCase(astr);
  result := False;
  s := apats;
  repeat
    cpat := '';
    p := pos(';',s);
    if p > 0 then
    begin
      cpat := copy(s, 1, p-1);
      delete(s, 1, p);
    end
    else
    begin
      cpat := s;
      s := '';
    end;  { if/else }
    cpat := UpperCase(trim(cpat));
    if cpat <> '' then
      result := StringMatches(astrupper, cpat);
  until result or (cpat = '');
end;


{ TFileEntry }

constructor TFileEntry.Create;
begin
  FAttributes := 0;
  FMode := 0;
  FSize := 0;
  FIsLink := False;
  FEntryType := etFile;
end;

{ TFileList }

function TFileList.GetEntry(i: integer): TFileEntry;
begin
  if (i < 1) or (i > FEntries.Count) then
    Result := nil
  else
    Result := TFileEntry(FEntries[i-1]);
end;

constructor TFileList.Create;
begin
  FEntries := TList.Create;
  FDirectoryName := '';
end;

destructor TFileList.Destroy;
begin
  Clear;
  FEntries.Free;
  inherited Destroy;
end;

function TFileList.Count: integer;
begin
  Result := FEntries.Count;
end;

function TFileList.ReadDirectory(const AFilemask: string; AShowHidden: boolean): integer;

  { HasAttrib() tests whether or not a file (with attributes fileAttrib) has the
    testAttrib attribute bit set. }
  function HasAttrib(fileAttrib, testAttrib: Integer): Boolean;
  begin
    Result := (fileAttrib and testAttrib) <> 0;
  end;

  // locally visible proc
  procedure AddEntry(sr: TSearchRec);
  var
    e: TFileEntry;
    fullname: string;
  begin
//    if HasAttrib(sr.Attr, faDirectory) or (sr.Name = '.') or (sr.Name = '..') then
//      Exit; //==>

    e := TFileEntry.Create;
    e.Name := sr.Name;
    e.Extention := ExtractFileExt(e.Name);
    e.Size := sr.Size;
    e.Attributes := sr.Attr; // this is incorrect and needs to improve!
    e.EntryType := etFile;
    {$IFDEF UNIX}
    e.mode := sr.Mode;
    {$ENDIF}
    e.IsLink := HasAttrib(sr.Attr, faSymLink);
    fullname := FDirectoryName + e.Name;
    e.LinkTarget := ExtractTargetSymLinkPath(fullname);
    e.ModTime := FileDateToDateTime(sr.Time);

    if HasAttrib(sr.Attr, faDirectory) then
      e.EntryType := etDir
    else
      e.EntryType := etFile;
{
    if (e.mode and $F000) = $4000 then
      e.etype := etDir
    else
      e.etype := etFile;
}

    if (e.Name = '.') or
       ((e.Name = '..') and (FDirectoryName = '/')) or
       (not AShowHidden and (Copy(e.Name, 1, 1) = '.') and (Copy(e.Name, 2, 1) <> '.')) or
//       (not AShowHidden and HasAttrib(sr.Attr, faHidden)) or
       ((e.EntryType = etFile) and not FileNameMatches(e.Name, AFilemask)) then
    begin
      // do not add this entry
      e.Free;
    end
    else
      FEntries.Add(e)
  end;

var
  SearchRec: TSearchRec;
begin
  Clear;
  GetDir(0, FDirectoryName);

  // Add PathDelim to end if it doesn't yet exist
  if Copy(FDirectoryName, Length(FDirectoryName), 1) <> PathDelim then
    FDirectoryName := FDirectoryName + PathDelim;

  try
    // The extra 'or' includes Normal attribute files under Windows. faAnyFile doesn't return those.
    // Reported to FPC as bug 9440 in Mantis.
    if SysUtils.FindFirst(FDirectoryName + '*', faAnyFile or $00000080, SearchRec) = 0 then
    begin
      AddEntry(SearchRec);
      while SysUtils.FindNext(SearchRec) = 0 do
      begin
        AddEntry(SearchRec);
      end;
    end;
  finally
    SysUtils.FindClose(SearchRec);
  end;

  Result := FEntries.Count;
end;

procedure TFileList.Clear;
var
  n: integer;
begin
  for n := 0 to FEntries.Count-1 do
    TFileEntry(FEntries[n]).Free;
  FEntries.Clear;
end;

procedure TFileList.Sort(AOrder: TFileListSortOrder);
var
  newl: TList;
  n, i: integer;
  e: TFileEntry;

  function IsBefore(newitem, item: TFileEntry): boolean;
  begin
    //if newitem.etype = etDir then writeln('dir: ',newitem.name,' (',item.name,')');
    if (newitem.EntryType = etDir) and (item.EntryType <> etDir) then
    begin
      result := true;
    end
    else if (newitem.EntryType <> etDir) and (item.EntryType = etDir) then
    begin
      result := false;
    end
    else if (newitem.EntryType = etDir) and (newitem.Name = '..') then
    begin
      result := true;
    end
    else if (item.EntryType = etDir) and (item.Name = '..') then
    begin
      result := false;
    end
    else
      case AOrder of
        soFileName   : result := UpperCase(newitem.Name) < UpperCase(item.Name);
        soCSFileName : result := newitem.Name < item.Name;
        soFileExt    : result := UpperCase(newitem.Extention+' '+newitem.Name) < UpperCase(item.Extention+' '+item.Name);
        soSize       : result := newitem.size < item.size;
        soTime       : result := newitem.modtime < item.modtime;
      else
        result := False;
      end;
  end;

begin
  newl := TList.Create;
  for n := 0 to FEntries.Count-1 do
  begin
    e := TFileEntry(FEntries[n]);
    i := 0;
    while (i < newl.Count) and not IsBefore(e,TFileEntry(newl[i])) do inc(i);
    newl.Insert(i,e);
  end;
  FEntries.Free;
  FEntries := newl;
end;

{ TfpgFileGrid }

function TfpgFileGrid.GetRowCount: integer;
begin
  Result := FFileList.Count;
end;

procedure TfpgFileGrid.DrawCell(ARow, ACol: integer; ARect: TfpgRect; AFlags: integer);
const
  modestring: string[9] = 'rwxrwxrwx';
var
  e: TFileEntry;
  x: integer;
  y: integer;
  s: string;
  img: TfpgImage;
  b: integer;
  n: integer;
begin
  e := FFileList.Entry[ARow];
  if e = nil then
    Exit; //==>

  x := ARect.Left + 2;
  y := ARect.Top + 1;
  s := '';

  if (e.EntryType = etDir) and (ACol = 1) then
    Canvas.SetFont(HeaderFont)
  else
    Canvas.SetFont(Font);

  case ACol of
    1:  begin
          if e.EntryType = etDir then
            fpgImages.GetImage('stdimg.folder')
          else
          begin
            img := fpgImages.GetImage('stdimg.document');
            {$IFDEF UNIX}
            if (e.Mode and $40) <> 0 then
              img := fpgImages.GetImage('stdimg.yes'); // executable
            {$ENDIF}
          end;

//          if img <> nil then
//            Canvas.DrawImage(ARect.Left+1, y, img);
//          if e.IsLink then
//            Canvas.DrawImage(ARect.Left+1, y, fpgImages.GetImage('stdimg.link'));
          x := ARect.Left + 20;
          s := e.Name;
        end;
        
    2:  begin
          s := FormatFloat('###,###,###,##0',e.size);
          x := ARect.Right - Font.TextWidth(s) - 1;
          if x < (ARect.Left + 2) then
            x := ARect.Left + 2;
        end;

    3:  s := FormatDateTime('yyyy-mm-dd hh:nn', e.ModTime);

    4:  begin
          {$IFDEF MSWINDOWS}
          // File attributes
          s := '';
          //if (e.attributes and FILE_ATTRIBUTE_ARCHIVE) <> 0    then s := s + 'a' else s := s + ' ';
          if (e.attributes and FILE_ATTRIBUTE_HIDDEN) <> 0     then s := s + 'h';
          if (e.attributes and FILE_ATTRIBUTE_READONLY) <> 0   then s := s + 'r';
          if (e.attributes and FILE_ATTRIBUTE_SYSTEM) <> 0     then s := s + 's';
          if (e.attributes and FILE_ATTRIBUTE_TEMPORARY) <> 0  then s := s + 't';
          if (e.attributes and FILE_ATTRIBUTE_COMPRESSED) <> 0 then s := s + 'c';
          {$ENDIF}
          {$IFDEF UNIX}
          // rights
          //rwx rwx rwx
          b := 1;
          n := 1;
          s := '';
          while n <= 9 do
          begin
            if (e.mode and b) = 0 then
              s := '-'+s
            else
              s := modestring[n]+s;
            inc(n);
            b := b shl 1;
          end;
          {$ENDIF}

          Canvas.SetFont(FixedFont);
        end;

    {$IFDEF UNIX}
//    5:  s := GetUserName(e.ownerid);  // use getpwuid(); for the name of this user
    {$ENDIF}

    {$IFDEF UNIX}
//    6:  s := GetGroupName(e.groupid);  // use getgrgid(); for the name of this group
    {$ENDIF}
  end;
  canvas.DrawString(x, y, s);
end;

constructor TfpgFileGrid.Create(AOwner: TComponent);
begin
  FFileList := TFileList.Create;
  inherited Create(AOwner);
  ColumnCount := 0;
  FFixedFont := fpgGetFont('Courier New-9:antialias=true');

  {$Note Abstract this!  No IFDEF's allowed!!! }
{$ifdef MSWINDOWS}
  AddColumn('Name', 320);
{$else}
  AddColumn('Name', 220);
{$endif}

  AddColumn('Size', 80);
  AddColumn('Mod. Time', 108);
{$ifdef MSWINDOWS}
  AddColumn('Attributes', 78);
{$else}
  AddColumn('Rights', 78);
  AddColumn('Owner', 54);
  AddColumn('Group', 54);
{$endif}

  RowSelect := True;
end;

destructor TfpgFileGrid.Destroy;
begin
  FFixedFont.Free;
  FFileList.Free;
  inherited Destroy;
end;

function TfpgFileGrid.CurrentEntry: TFileEntry;
begin
  Result := FFileList.Entry[FocusRow];
end;

end.

