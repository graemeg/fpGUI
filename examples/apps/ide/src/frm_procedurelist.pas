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

unit frm_procedurelist;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_panel, fpg_label,
  fpg_edit, fpg_combobox, fpg_basegrid, fpg_grid, fpg_imagelist,
  pparser, pastree, fpg_textedit;

type

  TSimpleEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement; override;
    function FindElement(const AName: String): TPasElement; override;
  end;

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
  ideconst
  ,mPasLex
  ,ideutils
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
  SParseStatistics = 'Procedures processed in %g seconds';


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
  LoadTime: DWord;
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
//  if IsCpp(FileName) or IsC(FileName) or IsH(FileName) then
//    Language := ltCpp
//  else
    Language := ltPas;

  LoadTime := fpgGetTickCount;
  InitializeForm;
  LoadTime := fpgGetTickCount - LoadTime;
  StatusBar.Text := Format(SParseStatistics, [LoadTime / 1000]);
end;

procedure TProcedureListForm.FormShow(Sender: TObject);
var
  M: TPasModule;
  E: TPasTreeContainer;
  I: Integer;
  Decls: TList;
  p: TPasElement;
begin
{
  E := TSimpleEngine.Create;
  try
//    writeln(Format('Parsing file <%s> for OS <%s> and CPU <%s>', [FFilename, OSTarget, CPUTarget]));
    M := ParseSource(E, FFilename, OSTarget, CPUTarget);

    { Cool, we successfully parsed the unit.
      Now output some info about it. }
    Decls := M.InterfaceSection.Declarations;
    for I := 0 to Decls.Count - 1 do
    begin
      p := TObject(Decls[I]) as TPasElement;
//      Writeln('Interface item ', I, ': ' + p.Name + ' [line ' + IntToStr(p.SourceLinenumber) + ']');
    end;
    FreeAndNil(M);
  finally
    FreeAndNil(E)
  end;
}
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
  Parser: TmwPasLex;
//  CParser: TBCBTokenList;
  BeginBracePosition: Longint;
  BraceCount, PreviousBraceCount: Integer;

  function MoveToImplementation: Boolean;
  begin
    if IsProgram(FFileName) or (IsInc(FFileName)) then
    begin
      Result := True;
      Exit;
    end;
    Result := False;
    while Parser.TokenID <> tkNull do
    begin
      if Parser.TokenID = tkImplementation then
        Result := True;
      Parser.Next;
      if Result then
        Break;
    end;
  end;

  procedure FindProcs;

    function GetProperProcName(ProcType: TTokenKind; IsClass: Boolean): string;
    begin
      Result := SUnknown;
      if IsClass then
      begin
        if ProcType = tkFunction then
          Result := 'Class Func' // Do not localize.
        else if ProcType = tkProcedure then
          Result := 'Class Proc'; // Do not localize.
      end
      else
      begin
        case ProcType of
          // Do not localize.
          tkFunction: Result := 'Function';
          tkProcedure: Result := 'Procedure';
          tkConstructor: Result := 'Constructor';
          tkDestructor: Result := 'Destructor';
        end;
      end;
    end;

(*
    procedure FindBeginningBrace;
    begin
      repeat
        CParser.NextNonJunk;
        case CParser.RunID of
          ctkbraceopen: Inc(BraceCount);
          ctkbraceclose: Dec(BraceCount);
          ctknull: Exit;
        end;
      until (CParser.RunID = ctkbraceopen) or
            (CParser.RunID = ctkbracepair) or
            (CParser.RunID = ctknull);
    end;

    // This procedure does two things.  It looks for procedures and it
    // looks for named scopes (like class/struct definitions & namespaces)
    // If it finds a named scope it returns the non-blank name.  If it finds
    // a procedure it returns a blank name.
    procedure FindBeginningProcedureBrace(var Name: string); // Used for CPP
    var
      InitialPosition: Integer;
      RestorePosition: Integer;
      FoundClass: Boolean;
    begin
      BeginBracePosition := 0;
      InitialPosition := CParser.RunPosition;
      // Skip these: enum {a, b, c};  or  int a[] = {0, 3, 5};  and find  foo () {
      FindBeginningBrace;
      if CParser.RunID = ctknull then
        Exit;
      CParser.PreviousNonJunk;
      // Check for a namespace or a class name
      if CParser.RunID = ctkidentifier then
      begin
        Name := CParser.RunToken;  // The name
        // This might be a derived class so search backward
        // no further than InitialPosition to see
        RestorePosition := CParser.RunPosition;
        FoundClass      := False;
        while CParser.RunPosition >= InitialPosition do begin
          if CParser.RunID in [ctkclass, ctkstruct, ctknamespace] then
          begin
            FoundClass := True;
            Break;
          end;
          if CParser.RunPosition = InitialPosition then
            Break;
          CParser.PreviousNonJunk;
        end;
        // The class name is the last token before a : or {
        if FoundClass then
        begin
          while not (CParser.RunID in [ctkcolon, ctkbraceopen, ctknull]) do begin
             Name := CParser.RunToken;
             CParser.NextNonJunk;
          end;
          // Back up a bit if we are on a brace open so empty enums don't get treated as namespaces
          if CParser.RunID = ctkbraceopen then
            CParser.PreviousNonJunk;
        end;
        // Now get back to where you belong
        while CParser.RunPosition < RestorePosition do
          CParser.NextNonJunk;
        CParser.NextNonJunk;
        BeginBracePosition := CParser.RunPosition;
      end
      else
      begin
        if CParser.RunID in [ctkroundclose, ctkroundpair, ctkconst, ctkvolatile, ctknull] then
        begin
          // Return an empty name to indicate that a procedure was found
          Name := '';
          CParser.NextNonJunk;
          BeginBracePosition := CParser.RunPosition;
        end
        else
        begin
          while not (CParser.RunID in [ctkroundclose, ctkroundpair, ctkconst, ctkvolatile, ctknull]) do
          begin
            CParser.NextNonJunk;
            if CParser.RunID = ctknull then
              Exit;
            // Recurse
            FindBeginningProcedureBrace(Name);
            CParser.PreviousNonJunk;
            if Name <> '' then Break;
          end;
          CParser.NextNonJunk;
        end;
      end;
    end;

    // This function searches backward from the current parser position
    // trying to find the procedure name - it returns all of the text
    // between the starting position and the position where it thinks a
    // procedure name has been found.
    function SearchForProcedureName: string;
    var
      ParenCount: Integer;
    begin
      ParenCount := 0;
      Result := '';
      repeat
        CParser.Previous;
        if CParser.RunID <> ctkcrlf then
          if (CParser.RunID = ctkspace) and (CParser.RunToken = #9) then
            Result := #32 + Result
          else
            Result := CParser.RunToken + Result;
        case CParser.RunID of
          ctkroundclose: Inc(ParenCount);
          ctkroundopen: Dec(ParenCount);
          ctknull: Exit;
        end;
      until ((ParenCount = 0) and ((CParser.RunID = ctkroundopen) or (CParser.RunID = ctkroundpair)));
      CParser.PreviousNonJunk; // This is the procedure name
    end;

    function SearchForTemplateArgs: string;
    var
      AngleCount: Integer;
    begin
      Result := '';
      if CParser.RunID <> ctkGreater then
        Exit; // only use if we are on a '>'
      AngleCount := 1;
      Result := CParser.RunToken;
      repeat
        CParser.Previous;
        if CParser.RunID <> ctkcrlf then
          if (CParser.RunID = ctkspace) and (CParser.RunToken = #9) then
            Result := #32 + Result
          else
            Result := CParser.RunToken + Result;
        case CParser.RunID of
          ctkgreater: Inc(AngleCount);
          ctklower: Dec(AngleCount);
          ctknull: Exit;
        end;
      until (((AngleCount = 0) and (CParser.RunID = ctklower)) or (CParser.RunIndex = 0));
      CParser.PreviousNonJunk; // This is the token before the template args
    end;
*)

  var
    ProcLine: string;
    ProcType: TTokenKind;
    Line: Integer;
    ClassLast: Boolean;
    InParenthesis: Boolean;
    InTypeDeclaration: Boolean;
    FoundNonEmptyType: Boolean;
    IdentifierNeeded: Boolean;
    ProcedureInfo: TProcInfo;
    BeginProcHeaderPosition: Longint;
    i, j: Integer;
    LineNo: Integer;
    ProcName, ProcReturnType: string;
    ProcedureType, ProcClass, ProcArgs: string;
    ProcIndex: Integer;
    NameList: TStringList;
    NewName, TmpName, ProcClassAdd, ClassName: string;
    BraceCountDelta: Integer;
    TemplateArgs: string;

    procedure EraseName(Index: Integer);
    var
      NameIndex: Integer;
    begin
      NameIndex := NameList.IndexOfName(IntToStr(Index));
      if NameIndex <> -1 then
        NameList.Delete(NameIndex);
    end;

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

            while Parser.TokenID <> tkNull do
            begin
              if not InTypeDeclaration and
                (Parser.TokenID in [tkFunction, tkProcedure, tkConstructor, tkDestructor]) then
              begin
                IdentifierNeeded := True;
                ProcType := Parser.TokenID;
                Line := Parser.LineNumber + 1;
                ProcLine := '';
                while not (Parser.TokenId in [tkNull]) do
                begin
                  //{$IFOPT D+} SendDebug('Found Inner Token: '+ Parser.Token+ ' '+BTS(ClassLast)); {$ENDIF}
                  case Parser.TokenID of
                    tkIdentifier, tkRegister:
                      IdentifierNeeded := False;

                    tkRoundOpen:
                      begin
                        // Did we run into an identifier already?
                        // This prevents
                        //    AProcedure = procedure() of object
                        // from being recognised as a procedure
                        if IdentifierNeeded then
                          Break;
                        InParenthesis := True;
                      end;

                    tkRoundClose:
                      InParenthesis := False;

                  else
                    // nothing
                  end; // case

                  if (not InParenthesis) and (Parser.TokenID = tkSemiColon) then
                    Break;

                  if not (Parser.TokenID in [tkCRLF, tkCRLFCo]) then
                    ProcLine := ProcLine + Parser.Token;
                  Parser.Next;
                end; // while
                if Parser.TokenID = tkSemicolon then
                  ProcLine := ProcLine + ';';
                if ClassLast then
                  ProcLine := 'class ' + ProcLine; // Do not localize.
                //{$IFOPT D+} SendDebug('FoundProc: ' + ProcLine); {$ENDIF}
                if not IdentifierNeeded then
                begin
                  ProcedureInfo := TProcInfo.Create;
                  ProcedureInfo.Name := ProcLine;
                  ProcedureInfo.ProcedureType := GetProperProcName(ProcType, ClassLast);
                  ProcedureInfo.LineNo := Line;
                  AddProcedure(ProcedureInfo);
                end;
              end;
              if (Parser.TokenID = tkClass) and Parser.IsClass then
              begin
                InTypeDeclaration := True;
                FoundNonEmptyType := False;
              end
              else if InTypeDeclaration and
                (Parser.TokenID in [tkProcedure, tkFunction, tkProperty,
                tkPrivate, tkProtected, tkPublic, tkPublished]) then
              begin
                FoundNonEmptyType := True;
              end
              else if InTypeDeclaration and
                ((Parser.TokenID = tkEnd) or
                ((Parser.TokenID = tkSemiColon) and not FoundNonEmptyType)) then
              begin
                InTypeDeclaration := False;
              end;
              //{$IFOPT D+} SendDebug('Found Token: '+ Parser.Token+ ' '+BTS(ClassLast)); {$ENDIF}
              ClassLast := (Parser.TokenID = tkClass);
              if ClassLast then
              begin
                Parser.NextNoJunk;
                //{$IFOPT D+} SendDebug('Found Class Token'+ ' '+BTS(ClassLast)); {$ENDIF}
              end
              else
                Parser.Next;
            end;
          end; //ltPas

(*
        ltCpp:
          begin
            NameList := TStringList.Create;
            try
              BraceCount := 0;
              NameList.Add('0='); // empty enclosure name
              j := CParser.TokenPositionsList[CParser.TokenPositionsList.Count - 1];
              PreviousBraceCount := BraceCount;
              FindBeginningProcedureBrace(NewName);

              while (CParser.RunPosition <= j - 1) or (CParser.RunID <> ctknull) do
              begin
                // If NewName = '' then we are looking at a real procedure - otherwise
                // we've just found a new enclosure name to add to our list
                if NewName = '' then
                begin
                  // If we found a brace pair then special handling is necessary
                  // for the bracecounting stuff (it is off by one)
                  if CParser.RunID = ctkbracepair then
                    BraceCountDelta := 0
                  else
                    BraceCountDelta := 1;
                  if (BraceCountDelta > 0) and (PreviousBraceCount >= BraceCount) then
                    EraseName(PreviousBraceCount);
                  // Back up a tiny bit so that we are "in front of" the
                  // ctkbraceopen or ctkbracepair we just found
                  CParser.Previous;

                  while not ((CParser.RunID in [ctksemicolon, ctkbraceclose, ctkbraceopen, ctkbracepair]) or
                             (CParser.RunID in IdentDirect) or
                             (CParser.RunIndex = 0)) do
                  begin
                    CParser.PreviousNonJunk;
                    // Handle the case where a colon is part of a valid procedure definition
                    if CParser.RunID = ctkcolon then
                    begin
                      // A colon is valid in a procedure definition only if it is immediately
                      // following a close parenthesis (possibly separated by "junk")
                      CParser.PreviousNonJunk;
                      if CParser.RunID in [ctkroundclose, ctkroundpair] then
                        CParser.NextNonJunk
                      else
                      begin
                        // Restore position and stop backtracking
                        CParser.NextNonJunk;
                        Break;
                      end;
                    end;
                  end;

                  if CParser.RunID in [ctkcolon, ctksemicolon, ctkbraceclose, ctkbraceopen, ctkbracepair] then
                    CParser.NextNonComment
                  else if CParser.RunIndex = 0 then
                  begin
                    if CParser.IsJunk then
                      CParser.NextNonJunk;
                  end
                  else // IdentDirect
                  begin
                    while CParser.RunID <> ctkcrlf do
                    begin
                      if (CParser.RunID = ctknull) then
                        Exit;
                      CParser.Next;
                    end;
                    CParser.NextNonJunk;
                  end;
                  // We are at the beginning of procedure header
                  BeginProcHeaderPosition := CParser.RunPosition;

                  ProcLine := '';
                  while (CParser.RunPosition < BeginBracePosition) and (CParser.RunID <> ctkcolon) do
                  begin
                    if (CParser.RunID = ctknull) then
                      Exit
                    else if (CParser.RunID <> ctkcrlf) then
                      if (CParser.RunID = ctkspace) and (CParser.RunToken = #9) then
                        ProcLine := ProcLine + #32
                      else
                        ProcLine := ProcLine + CParser.RunToken;
                    CParser.NextNonComment;
                  end;
                  // We are at the end of a procedure header
                  // Go back and skip parenthesis to find the procedure name
                  ProcName := '';
                  ProcClass := '';
                  ProcReturnType := '';
                  ProcArgs := SearchForProcedureName;
                  // We have to check for ctknull and exit since we moved the
                  // code to a nested procedure (if we exit SearchForProcedureName
                  // early due to RunID = ctknull we exit this procedure early as well)
                  if CParser.RunID = ctknull then
                    Exit;
                  if CParser.RunID = ctkthrow then
                  begin
                    ProcArgs := CParser.RunToken + ProcArgs;
                    ProcArgs := SearchForProcedureName + ProcArgs;
                  end;
                  // Since we've enabled nested procedures it is now possible
                  // that we think we've found a procedure but what we've really found
                  // is a standard C or C++ construct (like if or for, etc...)
                  // To guard against this we require that our procedures be of type
                  // ctkidentifier.  If not, then skip this step.
                  if (CParser.RunID = ctkidentifier) and not InProcedureBlacklist(CParser.RunToken) then
                  begin
                    ProcName := CParser.RunToken;
                    LineNo := CParser.PositionAtLine(CParser.RunPosition);
                    CParser.PreviousNonJunk;
                    if CParser.RunID = ctkcoloncolon then // The object/method delimiter
                    begin
                      // There may be multiple name::name::name:: sets here
                      // so loop until no more are found
                      ClassName := '';
                      while CParser.RunID = ctkcoloncolon do begin
                        CParser.PreviousNonJunk; // The object name?
                        // It is possible that we are looking at a templatized class and
                        // what we have in front of the :: is the end of a specialization:
                        // ClassName<x, y, z>::Function
                        if CParser.RunID = ctkgreater then
                          TemplateArgs := SearchForTemplateArgs;
                        ProcClass := CParser.RunToken + ProcClass;
                        if ClassName = '' then
                          ClassName := CParser.RunToken;
                        CParser.PreviousNonJunk; // look for another ::
                        if CParser.RunID = ctkcoloncolon then
                          ProcClass := CParser.RunToken + ProcClass;
                      end;
                      // We went back one step too far so go ahead one
                      CParser.NextNonJunk;
                      ProcIndex := ImageIndexFunction;
                      if ProcName = ClassName then // A constructor
                        ProcIndex := ImageIndexNew;
                      if ProcName = '~' + ClassName then // A destructor
                        ProcIndex := ImageIndexTrash;
                    end
                    else
                    begin
                      ProcIndex := ImageIndexFunction;
                      // If ProcIndex is 1 then we have backed up too far already
                      // so restore our previous position in order to correctly
                      // get the return type information for non-class methods
                      CParser.NextNonJunk;
                    end;

                    while CParser.RunPosition > BeginProcHeaderPosition do // Find the return type of the procedure
                    begin
                      CParser.PreviousNonComment;
                      // Handle the possibility of template specifications and
                      // do not include them in the return type
                      if CParser.RunID = ctkGreater then
                        TemplateArgs := SearchForTemplateArgs;
                      if CParser.RunID = ctktemplate then
                        Continue;
                      if CParser.RunID in [ctkcrlf, ctkspace] then
                        ProcReturnType := ' ' + ProcReturnType
                      else
                        ProcReturnType := CParser.RunToken + ProcReturnType
                    end;

                    // If the return type is an empty string then it must be a constructor
                    // or a destructor (depending on the presence of a ~ in the name
                    if (Trim(ProcReturnType) = '') or (Trim(ProcReturnType) = 'virtual') then
                    begin
                      if StrBeginsWith('~', ProcName) then
                        ProcIndex := ImageIndexTrash // a destructor
                      else
                        ProcIndex := ImageIndexNew; // a constructor
                    end;

                    ProcLine := Trim(ProcReturnType) + ' ';

                    // This code sticks enclosure names in front of
                    // methods (namespaces & classes with in-line definitions)
                    ProcClassAdd := '';
                    for i := 0 to BraceCount - BraceCountDelta do begin
                      if i < NameList.Count then
                      begin
                        TmpName := NameList.Values[IntToStr(i)];
                        if TmpName <> '' then
                        begin
                          if ProcClassAdd <> '' then
                            ProcClassAdd := ProcClassAdd + '::';
                          ProcClassAdd := ProcClassAdd + TmpName;
                        end;
                      end;
                    end;

                    if Length(ProcClassAdd) > 0 then
                    begin
                      if Length(ProcClass) > 0 then
                        ProcClassAdd := ProcClassAdd + '::';
                      ProcClass := ProcClassAdd + ProcClass;
                    end;
                    if Length(ProcClass) > 0 then
                      ProcLine := ProcLine + ' ' + ProcClass + '::';
                    ProcLine := ProcLine + ProcName + ' ' + ProcArgs;

                    // We need to double check the ProcIndex if it is = 0
                    // if it isn't a "static" method it should be 1
                    if (ProcIndex in [ImageIndexFunction, ImageIndexGear]) then
                      if StrBeginsWith('static ', Trim(ProcReturnType)) and
                         (Length(ProcClass) > 0) then
                        ProcIndex := ImageIndexGear
                      else
                        ProcIndex := ImageIndexFunction;

                    case ProcIndex of
                      ImageIndexFunction: if StrContains('void', ProcReturnType) then
                          ProcedureType := 'Procedure'
                        else
                          ProcedureType := 'Function';
                      ImageIndexGear: if StrContains('void', ProcReturnType) then
                          ProcedureType := 'Class Proc'
                        else
                          ProcedureType := 'Class Func';
                      ImageIndexNew: ProcedureType := 'Constructor';
                      ImageIndexTrash: ProcedureType := 'Destructor';
                    end;

                    ProcedureInfo := TProcInfo.Create;
                    ProcedureInfo.Name := ProcLine;
                    ProcedureInfo.ProcedureType := ProcedureType;
                    ProcedureInfo.LineNo := LineNo;
                    ProcedureInfo.ProcClass := ProcClass;
                    ProcedureInfo.ProcArgs := ProcArgs;
                    ProcedureInfo.ProcReturnType := ProcReturnType;
                    ProcedureInfo.ProcIndex := ProcIndex;
                    ProcedureInfo.ProcName := ProcName;
                    AddProcedure(ProcedureInfo);
                  end;
                  while (CParser.RunPosition < BeginBracePosition) do
                    CParser.Next;
                end
                else begin
                  // Insert enclosure name into our list (delete the old one if found)
                  EraseName(BraceCount);
                  NameList.Add(IntToStr(BraceCount) + '=' + NewName);
                end;
                PreviousBraceCount := BraceCount;
                FindBeginningProcedureBrace(NewName);
              end; //while (RunPosition <= j-1) ...
            finally
              NameList.Free;
            end;
          end; //Cpp
*)
      end; //case Language
    finally
      FProcList.EndUpdate;
    end;
  end;

var
  SFile: TFileStream;
  MemStream: TMemoryStream;
  Pos: Integer;
  Size: Integer;
const
  TheEnd: Char = #0; // Leave typed constant as is - needed for streaming code
begin
  case Language of
    ltPas: Parser := TmwPasLex.Create;
//    ltCpp: CParser := TBCBTokenList.Create;
  end;
  try
    MemStream := TMemoryStream.Create;
    try
      // Read from file on disk and store in a memory stream
      SFile := TFileStream.Create(FFilename, fmOpenRead or fmShareDenyWrite);
      try
        SFile.Position := 0;
        MemStream.CopyFrom(SFile, SFile.Size);
        MemStream.Write(TheEnd, 1);
      finally
        SFile.Free;
      end;
//      SendDebug('Procedure List: Starting Parse');
      case Language of
        ltPas: Parser.Origin := MemStream.Memory;
//        ltCpp: CParser.SetOrigin(MemStream.Memory, MemStream.Size);
      end;
      WindowTitle := WindowTitle + ' - ' + fpgExtractFileName(FFileName);

      ClearObjectStrings;
      try
        FindProcs;
      finally
        LoadObjectCombobox;
      end;
//      SendDebug('Procedure List: QuickSorting procedures');
      QuickSort(0, FProcList.Count - 1);
//      StatusBar.Panels[1].Text := Trim(IntToStr(lvProcs.Items.Count));
    finally
      MemStream.Free;
    end;
  finally
    case Language of
      ltPas: Parser.Free;
//      ltCpp: CParser.Free;
    end;
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
  WindowPosition := wpOneThirdDown;

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


{ TSimpleEngine }

function TSimpleEngine.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  { dummy implementation, see TFPDocEngine.FindElement for a real example }
  Result := nil;
end;

end.
