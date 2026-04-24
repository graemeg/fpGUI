{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2013 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      ---
}

unit ide.form.procedurelist;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_panel, fpg_label,
  fpg_edit, fpg_combobox, fpg_basegrid, fpg_grid, fpg_imagelist,
  fpg_textedit;

type

  TSourceLanguage = (ltPas, ltCpp);

  TProcInfo = class(TObject)
  private
    FLineNo: Integer;
    FName: string;
    FDisplayName: string;
    FProcedureType: string;
    FProcArgs: string;
    FProcClass: string;
    FProcReturnType: string;
    FProcName: string;
    FProcIndex: Integer;
  public
    property LineNo: Integer read FLineNo write FLineNo;
    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property ProcedureType: string read FProcedureType write FProcedureType;
    property ProcArgs: string read FProcArgs write FProcArgs;
    property ProcName: string read FProcName write FProcName;
    property ProcClass: string read FProcClass write FProcClass;
    property ProcReturnType: string read FProcReturnType write FProcReturnType;
    property ProcIndex: Integer read FProcIndex write FProcIndex;
  end;


  TProcedureListForm = class(TfpgForm)
  private
    procedure SearchTextChanged(Sender: TObject);
    procedure SearchEditKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure SetFilename(const AValue: string);
  private
    {@VFD_HEAD_BEGIN: ProcedureListForm}
    Bevel1: TfpgBevel;
    Bevel2: TfpgBevel;
    lblSearch: TfpgLabel;
    edtSearch: TfpgEdit;
    cbObjects: TfpgComboBox;
    lblObjects: TfpgLabel;
    grdProcedures: TfpgStringGrid;
    StatusBar: TfpgPanel;
    {@VFD_HEAD_END: ProcedureListForm}
    FFilename: TfpgString;
    FLanguage: TSourceLanguage;
    FSortOnColumn: Integer;
    FSearchAll: Boolean;
    FProcList: TStringList;
    FObjectStrings: TStringList;
    FEditor: TfpgTextEdit;
//    FImageList: TfpgImageList;
    procedure   FormShow(Sender: TObject);
    procedure   LoadProcs;
    procedure   AddProcedure(ProcedureInfo: TProcInfo);
    procedure   ClearObjectStrings;
    procedure   LoadObjectCombobox;
    procedure   QuickSort(L, R: Integer);
    procedure   InitializeForm;
    procedure   FillListBox;
    function    GetMethodName(const ProcName: string): string;
    function    GetImageIndex(const ProcName, ProcClass: string): Integer;
    procedure   GridDrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    property    Filename: string read FFilename write SetFilename;
    property    Editor: TfpgTextEdit read FEditor write FEditor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
    property    Language: TSourceLanguage read FLanguage write FLanguage default ltPas;
  end;

{@VFD_NEWFORM_DECL}

function DisplayProcedureList(const AFilename: TfpgString; var AEditor: TfpgTextEdit): boolean;


implementation

uses
  ide.consts
  ,ide.utils
  ,ide.pascal.tokeniser
  ,dbugintf
  ,fpg_utils
  ,fpg_imgfmt_bmp
  ;

const
  SAllString  = '<All>';
  SNoneString = '<None>';
  SUnknown = 'Unknown';
  SImplementationNotFound = 'Implementation section not found (parser error?)';
  SInvalidIndex = 'Invalid index number';
  SParseStatistics = 'Procedures processed in %.4g seconds';


{$I proclistimages.inc}


function DisplayProcedureList(const AFilename: TfpgString; var AEditor: TfpgTextEdit): boolean;
var
  frm: TProcedureListForm;
begin
  try
    frm := TProcedureListForm.Create(nil);
    frm.Filename := AFilename;
    frm.Editor := AEditor;
    frm.ShowModal;
  finally
    frm.Free;
    AEditor.SetFocus;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TProcedureListForm.SearchTextChanged(Sender: TObject);
begin
  FillListBox;
end;

procedure TProcedureListForm.SearchEditKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  case KeyCode of
    KeyUp:
        begin
          grdProcedures.FocusRow := grdProcedures.FocusRow-1;
          consumed := True;
        end;
    KeyDown:
        begin
          grdProcedures.FocusRow := grdProcedures.FocusRow+1;
          consumed := True;
        end;
    KeyEnter:
        begin
          Editor.GotoLine(StrToInt(grdProcedures.Cells[3, grdProcedures.FocusRow]));
          consumed := True;
          Close;
        end;
    KeyEscape:
        begin
          consumed := True;
          Close;
        end;
  end;
end;

procedure TProcedureListForm.SetFilename(const AValue: string);
var
  LoadTime: QWord;
begin
  if FFilename=AValue then
    exit;
  FFilename:=AValue;
  Language := ltPas;
  LoadTime := fpgGetTickCount;
  InitializeForm;
  LoadTime := fpgGetTickCount - LoadTime;
  StatusBar.Text := Format(SParseStatistics, [LoadTime / 1000]);
end;

procedure TProcedureListForm.FormShow(Sender: TObject);
begin
  // placeholder — loading is handled by SetFilename/InitializeForm
end;

procedure TProcedureListForm.InitializeForm;
begin
  FObjectStrings := TStringList.Create;
  FObjectStrings.Sorted := True;
  FObjectStrings.Duplicates := dupIgnore;
  ClearObjectStrings;
  FSortOnColumn := 1;

  FProcList := TStringList.Create;

//  LoadSettings;
  LoadProcs;

  FillListBox;
  edtSearch.SetFocus;
end;

procedure TProcedureListForm.FillListBox;
var
  i: Integer;
  ProcName: string;
  IsObject: Boolean;
  ProcInfo: TProcInfo;

  procedure AddListItem(ProcInfo: TProcInfo);
  var
    r: integer;
  begin
    r := grdProcedures.RowCount;
    grdProcedures.RowCount := grdProcedures.RowCount + 1;
    grdProcedures.Objects[0, r] := ProcInfo;
//    case Language of
//      ltPas: ListItem.ImageIndex := GetPasImageIndex(ProcInfo.Name);
//      ltCpp: ListItem.ImageIndex := ProcInfo.ProcIndex;
//    end;
    grdProcedures.Cells[1, r] := ProcInfo.DisplayName;
    grdProcedures.Cells[2, r] := ProcInfo.ProcedureType;
    grdProcedures.Cells[3, r] := IntToStr(ProcInfo.LineNo);
  end;

  procedure FocusAndSelectFirstItem;
  begin
    if grdProcedures.RowCount > 0 then
    begin
      grdProcedures.FocusRow := 0;
//      lvProcs.ItemFocused := lvProcs.Selected;
    end;
  end;

begin
  grdProcedures.BeginUpdate;
  try
    grdProcedures.RowCount := 0;
    if (Length(edtSearch.Text) = 0) and (cbObjects.Text = SAllString) then
    begin
      for i := 0 to FProcList.Count - 1 do
        AddListItem(TProcInfo(FProcList.Objects[i]));
      FocusAndSelectFirstItem;
      Exit;
    end;

    for i := 0 to FProcList.Count - 1 do
    begin
      ProcInfo := TProcInfo(FProcList.Objects[i]);
      case Language of
        ltPas: ProcName := ProcInfo.Name;
        ltCpp: ProcName := ProcInfo.ProcClass;
      end;
      IsObject := Length(ProcInfo.ProcClass) > 0;

      // Is it the object we want?
      if cbObjects.Text <> SAllString then
      begin
        if cbObjects.Text = SNoneString then
        begin
          if IsObject then // Does it have an object?
            Continue;
          if Length(edtSearch.Text) = 0 then // If no filter is active, add
          begin
            AddListItem(ProcInfo);
            Continue;
          end;
        end // if/then
        else if not SameText(cbObjects.Text, ProcInfo.ProcClass) then
          Continue;
      end;

      case Language of
        ltPas: ProcName := GetMethodName(ProcName);
        ltCpp: ProcName := ProcInfo.ProcName;
      end;

      if Length(edtSearch.Text) = 0 then
        AddListItem(ProcInfo)
      else if not FSearchAll and SameText(edtSearch.Text, Copy(ProcName, 1, Length(edtSearch.Text))) then
        AddListItem(ProcInfo)
      else if FSearchAll and StrContains(edtSearch.Text, ProcName, False) then
        AddListItem(ProcInfo);
    end;
    FocusAndSelectFirstItem;
  finally
    grdProcedures.EndUpdate;
  end;
//  ResizeCols;
end;

function TProcedureListForm.GetMethodName(const ProcName: string): string;
var
  CharPos: Integer;
begin
  Result := ProcName;
  Delete(Result, 1, 1);

  CharPos := Pos(#9, Result);
  if CharPos <> 0 then
    Delete(Result, CharPos, Length(Result));

  CharPos := Pos(' ', Result);
  Result := Copy(Result, CharPos + 1, Length(Result));

  CharPos := Pos('(', Result);
  if CharPos > 0 then
    Result := Copy(Result, 1, CharPos - 1);

  CharPos := Pos('.', Result);
  if CharPos > 0 then
    Result := Copy(Result, CharPos + 1, Length(Result));

  Result := Trim(Result);
end;

function TProcedureListForm.GetImageIndex(const ProcName, ProcClass: string): Integer;
const
  ImageIndexNew = 1;
  ImageIndexTrash = 2;
  ImageIndexGear = 3;
  ImageIndexFunction = 4;
begin
  if StrContains('constructor', ProcName, False) then     // Do not localize.
    Result := ImageIndexNew
  else if StrContains('destructor', ProcName, False) then // Do not localize.
    Result := ImageIndexTrash
  else if StrBeginsWith('class proc', ProcName, False)    // Do not localize.
      or StrContains('class func', ProcName, False)
      or (ProcClass <> '') then
    Result := ImageIndexGear
  else
    Result := ImageIndexFunction;
end;

procedure TProcedureListForm.GridDrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
var
  img: TfpgImage;
  i: integer;
  ProcInfo: TProcInfo;
begin
  ADefaultDrawing := True;
  if ACol = 0 then
  begin
    ProcInfo := grdProcedures.Objects[ACol, ARow] as TProcInfo;
    i := GetImageIndex(ProcInfo.ProcedureType, ProcInfo.ProcClass);
{
  ImageIndexNew = 1;
  ImageIndexTrash = 2;
  ImageIndexGear = 3;
  ImageIndexFunction = 4;
}
    case i of
      1:  img := fpgImages.GetImage('ide.grid.constr');
      2:  img := fpgImages.GetImage('ide.grid.destr');
      3:  img := fpgImages.GetImage('ide.grid.gears');
      4:  img := fpgImages.GetImage('ide.grid.func');
    end;
    if Assigned(img) then
      grdProcedures.Canvas.DrawImage((ARect.Width-16) div 2, ARect.Top, img);
  end;
end;

procedure TProcedureListForm.LoadProcs;
var
  Tokeniser: TFpgPascalTokeniser;
  Tok: TFpgPasToken;
  TokUpper: string;

  procedure FetchToken;
  begin
    repeat
      Tok := Tokeniser.NextToken;
    until not (Tok.Kind in [fptkWhitespace, fptkLineEnding,
                            fptkComment, fptkDirective]);
    TokUpper := Tokeniser.TokenTextUpper;
  end;

  // Keep whitespace for building display strings
  procedure FetchTokenKeepWS;
  begin
    repeat
      Tok := Tokeniser.NextToken;
    until not (Tok.Kind in [fptkComment, fptkDirective]);
    TokUpper := Tokeniser.TokenTextUpper;
  end;

  function IsKW(const AWord: string): Boolean; inline;
  begin
    Result := (Tok.Kind = fptkKeyword) and (TokUpper = AWord);
  end;

  function IsSym(const ACh: string): Boolean; inline;
  begin
    Result := (Tok.Kind = fptkSymbol) and (Tokeniser.TokenText = ACh);
  end;

  function MoveToImplementation: Boolean;
  begin
    if IsProgram(FFileName) or (IsInc(FFileName)) then
      Exit(True);
    Result := False;
    while Tok.Kind <> fptkEOF do
    begin
      if IsKW('IMPLEMENTATION') then
        Result := True;
      FetchToken;
      if Result then
        Break;
    end;
  end;

  function IsVisibilityIdent: Boolean;
  begin
    Result := (Tok.Kind = fptkIdentifier) and
      ((TokUpper = 'PRIVATE') or (TokUpper = 'PROTECTED') or
       (TokUpper = 'PUBLIC') or (TokUpper = 'PUBLISHED'));
  end;

  procedure FindProcs;
  var
    ProcLine: string;
    ProcKindStr: string;
    Line: Integer;
    ClassLast: Boolean;
    InParenthesis: Boolean;
    InTypeDeclaration: Boolean;
    FoundNonEmptyType: Boolean;
    IdentifierNeeded: Boolean;
    ProcedureInfo: TProcInfo;
    IsClassProc: Boolean;
  begin
    FProcList.Capacity := 200;
    FProcList.BeginUpdate;
    try
      case Language of
        ltPas:
          begin
            if not MoveToImplementation then
              raise Exception.Create(SImplementationNotFound);
            ClassLast := False;
            InParenthesis := False;
            InTypeDeclaration := False;
            FoundNonEmptyType := False;

            while Tok.Kind <> fptkEOF do
            begin
              if not InTypeDeclaration and (Tok.Kind = fptkKeyword) and
                 ((TokUpper = 'FUNCTION') or (TokUpper = 'PROCEDURE') or
                  (TokUpper = 'CONSTRUCTOR') or (TokUpper = 'DESTRUCTOR')) then
              begin
                IdentifierNeeded := True;
                IsClassProc := ClassLast;
                // Determine procedure type display
                if IsClassProc then
                begin
                  if TokUpper = 'FUNCTION' then ProcKindStr := 'Class Func'
                  else if TokUpper = 'PROCEDURE' then ProcKindStr := 'Class Proc'
                  else ProcKindStr := SUnknown;
                end
                else
                begin
                  if TokUpper = 'FUNCTION' then ProcKindStr := 'Function'
                  else if TokUpper = 'PROCEDURE' then ProcKindStr := 'Procedure'
                  else if TokUpper = 'CONSTRUCTOR' then ProcKindStr := 'Constructor'
                  else if TokUpper = 'DESTRUCTOR' then ProcKindStr := 'Destructor'
                  else ProcKindStr := SUnknown;
                end;
                Line := Tok.Line;
                ProcLine := '';

                // Build procedure signature string
                while Tok.Kind <> fptkEOF do
                begin
                  if Tok.Kind = fptkIdentifier then
                    IdentifierNeeded := False;

                  if IsSym('(') then
                  begin
                    // Prevent "AProcedure = procedure() of object" from matching
                    if IdentifierNeeded then
                      Break;
                    InParenthesis := True;
                  end
                  else if IsSym(')') then
                    InParenthesis := False;

                  if (not InParenthesis) and IsSym(';') then
                    Break;

                  if Tok.Kind = fptkWhitespace then
                  begin
                    // Preserve a single space between tokens
                    if (ProcLine <> '') and (ProcLine[Length(ProcLine)] <> ' ') then
                      ProcLine := ProcLine + ' ';
                  end
                  else if Tok.Kind <> fptkLineEnding then
                    ProcLine := ProcLine + Tokeniser.TokenText;

                  FetchTokenKeepWS;
                end;
                if IsSym(';') then
                  ProcLine := ProcLine + ';';
                if IsClassProc then
                  ProcLine := 'class ' + ProcLine;
                if not IdentifierNeeded then
                begin
                  ProcedureInfo := TProcInfo.Create;
                  ProcedureInfo.Name := ProcLine;
                  ProcedureInfo.ProcedureType := ProcKindStr;
                  ProcedureInfo.LineNo := Line;
                  AddProcedure(ProcedureInfo);
                end;
              end;
              // Track class type declarations to skip forward-declared methods
              if IsKW('CLASS') and not ClassLast then
              begin
                InTypeDeclaration := True;
                FoundNonEmptyType := False;
              end
              else if InTypeDeclaration and
                ((IsKW('PROCEDURE') or IsKW('FUNCTION') or IsKW('PROPERTY')) or
                 IsVisibilityIdent) then
              begin
                FoundNonEmptyType := True;
              end
              else if InTypeDeclaration and
                (IsKW('END') or (IsSym(';') and not FoundNonEmptyType)) then
              begin
                InTypeDeclaration := False;
              end;
              ClassLast := IsKW('CLASS');
              FetchToken;
            end;
          end; //ltPas
      end; //case Language
    finally
      FProcList.EndUpdate;
    end;
  end;

var
  SFile: TFileStream;
  SourceText: string;
  Size: Integer;
begin
  // Read source file
  SFile := TFileStream.Create(FFilename, fmOpenRead or fmShareDenyWrite);
  try
    Size := SFile.Size;
    SetLength(SourceText, Size);
    if Size > 0 then
      SFile.Read(SourceText[1], Size);
  finally
    SFile.Free;
  end;

  Tokeniser := TFpgPascalTokeniser.Create;
  try
    Tokeniser.SetSource(SourceText);
    FetchToken;

    WindowTitle := WindowTitle + ' - ' + fpgExtractFileName(FFileName);

    ClearObjectStrings;
    try
      FindProcs;
    finally
      LoadObjectCombobox;
    end;
    QuickSort(0, FProcList.Count - 1);
  finally
    Tokeniser.Free;
  end;
end;

procedure TProcedureListForm.AddProcedure(ProcedureInfo: TProcInfo);
var
  TempStr: string;
  i: Integer;
begin
  ProcedureInfo.Name := CompressWhiteSpace(ProcedureInfo.Name);
  case Language of
    ltPas:
      begin
        TempStr := ProcedureInfo.Name;
        // Remove the class reserved word
        if StrBeginsWith('CLASS ', TempStr, False) then // Do not localize.
          Delete(TempStr, 1, 6); // Do not localize.
        // Remove 'function' or 'procedure'
        i := Pos(' ', TempStr);
        if i > 0 then
          TempStr := Copy(TempStr, i + 1, Length(TempStr));
        // Remove the paramater list
        i := Pos('(', TempStr);
        if i > 0 then
          TempStr := Copy(TempStr, 1, i - 1);
        // Remove the function return type
        i := Pos(':', TempStr);
        if i > 0 then
          TempStr := Copy(TempStr, 1, i - 1);
        // Check for an implementation procedural type
        if Length(TempStr) = 0 then
        begin
          ProcedureInfo.Free;
          Exit;
        end;
        // Remove any trailing ';'
        if TempStr[Length(TempStr)] = ';' then
          Delete(TempStr, Length(TempStr), 1);
        TempStr := Trim(TempStr);
        ProcedureInfo.DisplayName := TempStr;
        // Add to the object combobox and set the object name in ProcedureInfo
        i := Pos('.', TempStr);
        if i = 0 then
          FObjectStrings.Add(SNoneString)
        else
        begin
          ProcedureInfo.ProcClass := Copy(TempStr, 1, i - 1);
          FObjectStrings.Add(ProcedureInfo.ProcClass);
        end;
        FProcList.AddObject(#9 + TempStr + #9 + ProcedureInfo.ProcedureType + #9 + IntToStr(ProcedureInfo.LineNo), ProcedureInfo);
      end; //ltPas

(*
    ltCpp:
      begin
        if Length(ProcedureInfo.ProcClass) > 0 then
          ProcedureInfo.DisplayName := ProcedureInfo.ProcClass + '::';
        // Should be the return type and args displayed, they are now in the status bar ?
        ProcedureInfo.DisplayName := ProcedureInfo.DisplayName + ProcedureInfo.ProcName;
        FProcList.AddObject(#9 + ProcedureInfo.DisplayName + #9 + ProcedureInfo.ProcedureType + #9 + IntToStr(ProcedureInfo.LineNo), ProcedureInfo);
        if Length(ProcedureInfo.ProcClass) = 0 then
          FObjectStrings.Add(SNoneString)
        else
          FObjectStrings.Add(ProcedureInfo.ProcClass);
      end; //ltCpp
*)
  end; //case Language
end;

procedure TProcedureListForm.ClearObjectStrings;
begin
  FObjectStrings.Clear;
  FObjectStrings.Add(SAllString);
end;

procedure TProcedureListForm.LoadObjectCombobox;
begin
  cbObjects.Items.Assign(FObjectStrings);
  cbObjects.FocusItem := cbObjects.Items.IndexOf(SAllString);
end;

procedure TProcedureListForm.QuickSort(L, R: Integer);

  function GetValue(idx: Integer): string;
  var
    i: Integer;
    TabPos: Integer;
  begin
    if idx >= FProcList.Count then
      raise Exception.Create(SInvalidIndex);
    Result := FProcList.Strings[idx];
    for i := 0 to FSortOnColumn - 1 do
    begin
      TabPos := Pos(#9, Result);
      if TabPos > 0 then
        Delete(Result, 1, TabPos)
      else
        Exit;
    end;
    if FSortOnColumn = 3 then
    begin
      for i := Length(Result) to 5 do
        Result := ' ' + Result;
    end;
  end;

var
  I, J: Integer;
  P: string;
begin
  if FProcList.Count = 0 then
    Exit;
  repeat
    I := L;
    J := R;
    P := GetValue((L + R) shr 1);
    repeat
      while AnsiCompareText(GetValue(I), P) < 0 do
        Inc(I);
      while AnsiCompareText(GetValue(J), P) > 0 do
        Dec(J);
      if I <= J then
      begin
        FProcList.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

constructor TProcedureListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow  := @FormShow;
  FLanguage := ltPas;
  FSearchAll := True; // search anywhere in a method name
//  FImageList := TfpgImageList.Create;

//  CreateImage_BMP(@grdimg_destructor_16, SizeOf(grdimg_destructor_16));

  fpgImages.AddMaskedBMP(  // 16x16 image
          'ide.grid.destr',
          @grdimg_destructor_16,
    sizeof(grdimg_destructor_16), 0, 0);

  fpgImages.AddMaskedBMP(  // 16x16 image
          'ide.grid.constr',
          @grdimg_constructor_16,
    sizeof(grdimg_constructor_16), 0, 0);

  fpgImages.AddMaskedBMP(  // 16x16 image
          'ide.grid.func',
          @grdimg_function_16,
    sizeof(grdimg_function_16), 0, 0);

  fpgImages.AddMaskedBMP(  // 16x16 image
          'ide.grid.gears',
          @grdimg_gears_16,
    sizeof(grdimg_gears_16), 0, 0);
end;

destructor TProcedureListForm.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FObjectStrings);

  if FProcList <> nil then
  begin
    for i := 0 to FProcList.Count - 1 do
      FProcList.Objects[i].Free;
    FreeAndNil(FProcList);
  end;
  inherited Destroy;
end;

procedure TProcedureListForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ProcedureListForm}
  Name := 'ProcedureListForm';
  SetPosition(332, 253, 564, 310);
  WindowTitle := 'Procedure List';
  Hint := '';
  ShowHint := True;
  WindowPosition := wpMainFormCenter;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(0, 0, 564, 32);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Shape := bsSpacer;
  end;

  Bevel2 := TfpgBevel.Create(self);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(0, 33, 564, 32);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Shape := bsSpacer;
  end;

  lblSearch := TfpgLabel.Create(Bevel2);
  with lblSearch do
  begin
    Name := 'lblSearch';
    SetPosition(4, 8, 47, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Search';
  end;

  edtSearch := TfpgEdit.Create(Bevel2);
  with edtSearch do
  begin
    Name := 'edtSearch';
    SetPosition(52, 4, 264, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Text := '';
    OnChange := @SearchTextChanged;
    OnKeyPress := @SearchEditKeyPressed;
  end;

  cbObjects := TfpgComboBox.Create(Bevel2);
  with cbObjects do
  begin
    Name := 'cbObjects';
    SetPosition(376, 4, 184, 22);
    Anchors := [anRight,anTop];
    FontDesc := '#List';
    Hint := '';
    TabOrder := 3;
    OnChange := @SearchTextChanged;
  end;

  lblObjects := TfpgLabel.Create(Bevel2);
  with lblObjects do
  begin
    Name := 'lblObjects';
    SetPosition(324, 8, 51, 16);
    Anchors := [anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Objects';
  end;

  grdProcedures := TfpgStringGrid.Create(self);
  with grdProcedures do
  begin
    Name := 'grdProcedures';
    SetPosition(4, 68, 556, 216);
    Anchors := [anLeft,anRight,anTop,anBottom];
    BackgroundColor := TfpgColor($80000002);
    AddColumn('', 30, taLeftJustify);
    AddColumn('Procedure', 300, taLeftJustify);
    AddColumn('Type', 130, taLeftJustify);
    AddColumn('Line', 70, taRightJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := True;
    TabOrder := 3;
    HeaderStyle := ghsFlat;
    OnDrawCell := @GridDrawCell;
  end;

  StatusBar := TfpgPanel.Create(self);
  with StatusBar do
  begin
    Name := 'StatusBar';
    SetPosition(0, 290, 564, 20);
    Align := alBottom;
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Style := bsLowered;
    Text := 'Panel';
  end;

  {@VFD_BODY_END: ProcedureListForm}
  {%endregion}
end;


end.
