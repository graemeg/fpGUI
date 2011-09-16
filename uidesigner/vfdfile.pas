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
      This unit handles the load, save and merge functions. Doing
      marker searching.
}

unit vfdfile;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_utils;

type
  TVFDFileBlock = class
  public
    BlockID: string;
    FormName: string;
    Position: integer;
    Data: string;
  end;
  

  TVFDFile = class
  private
    FParsedData: string;
    FBlocks: TList;
    FFileData: string;
    NewFormsDecl: string;
    NewFormsImpl: string;
    procedure   FreeBlocks;
    procedure   AddBlock(aposition: integer; ablockid, aformname, ablockdata: string);
  public
    constructor Create;
    destructor  Destroy; override;
    function    LoadFile(fname: string): boolean;
    function    BlockCount: integer;
    function    Block(index: integer): TVFDFileBlock;
    function    GetBlocks: integer;   // parse file
    function    MergeBlocks: string;  // store file
    procedure   AddNewFormDecl(formname, formheadblock: string);
    procedure   AddNewFormImpl(formname, formbody: string);
    function    FindFormBlock(blockid, formname: string): TVFDFileBlock;
    procedure   SetFormData(formname, headblock, bodyblock: string);
    procedure   NewFileSkeleton(AUnitname: string);
  end;
  

implementation

uses
  fpg_iniutils,
  vfdutils;

const
  cBlockPrefix = '{@VFD_';

{ TVFDFile }

procedure TVFDFile.AddBlock(aposition: integer; ablockid, aformname, ablockdata: string);
var
  fb: TVFDFileBlock;
begin
  fb          := TVFDFileBlock.Create;
  fb.Position := aposition;
  fb.BlockID  := ablockid;
  fb.FormName := aformname;
  fb.Data     := ablockdata;
  FBlocks.Add(fb);
end;

procedure TVFDFile.AddNewFormDecl(formname, formheadblock: string);
var
  s: string;
begin
  s :=
      Ind(1) + 'T' + formname + ' = class(TfpgForm)' + LineEnding +
      Ind(1) + 'private' + LineEnding +
      Ind(2) + '{@VFD_HEAD_BEGIN: ' + formname + '}' + LineEnding
    + formheadblock +
      Ind(2) + '{@VFD_HEAD_END: ' + formname + '}' + LineEnding +
      Ind(1) + 'public' + LineEnding +
      Ind(2) + 'procedure AfterCreate; override;' + LineEnding
    + Ind(1) + 'end;' + LineEnding + LineEnding;
  NewFormsDecl := NewFormsDecl + s;
end;

procedure TVFDFile.AddNewFormImpl(formname, formbody: string);
var
  s: string;
  lUseRegions: boolean;
  lRegionTop, lRegionBottom: string;
begin
  { Does the developer code regions or not - a Lazarus IDE feature }
  lUseRegions := gINI.ReadBool('Options', 'UseCodeRegions', True);
  if lUseRegions then
  begin
    lRegionTop    := Ind(1) + '{%region ''Auto-generated GUI code'' -fold}' + LineEnding;
    lRegionBottom := Ind(1) + '{%endregion}' + LineEnding;
  end
  else
  begin
    lRegionTop    := '';
    lRegionBottom := '';
  end;

  s := LineEnding + LineEnding +
    'procedure T' + formname + '.AfterCreate;' + LineEnding +
    'begin' + LineEnding +
    lRegionTop +
    Ind(1) + '{@VFD_BODY_BEGIN: ' + formname + '}' + LineEnding +
    formbody +
    Ind(1) + '{@VFD_BODY_END: ' + formname + '}' + LineEnding +
    lRegionBottom +
    'end;' + LineEnding;
  NewFormsImpl := NewFormsImpl + s;
end;

function TVFDFile.Block(index: integer): TVFDFileBlock;
begin
  Result := nil;
  if (index < 0) or (index > FBlocks.Count-1) then
    Exit;
  Result := TVFDFileBlock(FBlocks[index]);
end;

function TVFDFile.BlockCount: integer;
begin
  Result := FBlocks.Count;
end;

constructor TVFDFile.Create;
begin
  FFileData    := '';
  FParsedData  := '';
  NewFormsDecl := '';
  NewFormsImpl := '';
  FBlocks      := TList.Create;
end;

destructor TVFDFile.Destroy;
begin
  FreeBlocks;
  FBlocks.Free;
  inherited;
end;

function TVFDFile.FindFormBlock(blockid, formname: string): TVFDFileBlock;
var
  n: integer;
  fb: TVFDFileBlock;
begin
  Result := nil;
  for n := 0 to BlockCount-1 do
  begin
    fb := Block(n);
    if (fb.BlockID = blockid) and (UpperCase(fb.FormName) = UpperCase(formname)) then
    begin
      Result := fb;
      Exit;
    end;
  end;
end;

procedure TVFDFile.FreeBlocks;
var
  n: integer;
begin
  for n := FBlocks.Count-1 downto 0 do
    TVFDFileBlock(FBlocks[n]).Free;
  FBlocks.Clear;
  NewFormsDecl := '';
  NewFormsImpl := '';
end;

function TVFDFile.GetBlocks: integer;
var
  n: integer;
  s: string;
  startp, endp: integer;
  formname: string;
  bname, startmarker, endmarker: string;
  datablock: string;
  deletelen: integer;
  dropmarker: boolean;
begin
  FreeBlocks;
  FParsedData := FFileData;

  // searching blocks:
  repeat
    bname     := '';
    formname  := '';
    datablock := '';
    s      := cBlockPrefix;
    startp := pos(s, FParsedData);
    if startp > 0 then
    begin
      // marker found
      n := startp + 2;
      while (n < length(FParsedData)) and (FParsedData[n] in ['_', 'A'..'Z']) do
      begin
        bname := bname + FParsedData[n];
        Inc(n);
      end;

      if FParsedData[n] = ':' then
        Inc(n, 2);

      while (n < length(FParsedData)) and (FParsedData[n] <> '}') do
      begin
        formname := formname + FParsedData[n];
        Inc(n);
      end;

      startmarker := copy(FParsedData, startp, n - startp + 1);
      deletelen   := length(startmarker);
      dropmarker  := False;

//      Writeln('marker: ', startmarker);

      // block marker ?
      endmarker := '';
      if bname = 'VFD_HEAD_BEGIN' then //or (bname = 'VFD_BODY_BEGIN') then
        endmarker := '{@VFD_HEAD_END: ' + formname + '}'
      else if bname = 'VFD_BODY_BEGIN' then
        endmarker := '{@VFD_BODY_END: ' + formname + '}';

      if endmarker <> '' then
      begin
        //Writeln('Block: ',bname,' form: ',formname);
        // find the end of the block
        endp := pos(endmarker, FParsedData);
        if endp > 0 then
        begin
          //Writeln('end marker found.');
          datablock  := copy(FParsedData, startp + length(startmarker), endp - startp - length(startmarker));
          //Writeln('data block:');
          //writeln(datablock);
          //writeln('.');
          deletelen  := endp - startp + length(endmarker);
        end
        else
        begin
          dropmarker := True; // error: end marker did not found
          //Writeln('file error: ',endmarker,' marker wasn''t found.');
          // block length = 0
        end;
      end;

      Delete(FParsedData, startp, deletelen);
      if not dropmarker then
        AddBlock(startp, bname, formname, datablock);
    end;
  until startp <= 0;

  //writeln(FParsedData);
  Result := BlockCount;
end;

function TVFDFile.LoadFile(fname: string): boolean;
var
  fs: TFileStream;
begin
  Result := False;
  fs := TFileStream.Create(fpgToOSEncoding(fname), fmOpenRead);
  try
    FFileData := ''; // make sure it is empty
    SetLength(FFileData, fs.Size);
    fs.Read(FFileData[1], fs.Size);
    //Writeln('data length: ',fs.Size);
    Result := True;
  finally
    fs.Free;
  end;
end;

function TVFDFile.MergeBlocks: string;
var
  rs: string;
  n: integer;
  iofs, startp: integer;
  fb: TVFDFileBlock;
  startmarker, endmarker: string;
  iblock: string;
  newsaved: boolean;
begin
  //  Writeln('merging blocks: ');
  newsaved := False;
  rs       := FParsedData;
  iofs     := 0;
  for n := 0 to FBlocks.Count - 1 do
  begin
    fb          := TVFDFileBlock(FBlocks[n]);
    startmarker := '{@' + fb.BlockID;
    if fb.formname <> '' then
      startmarker := startmarker + ': ' + fb.FormName;
    startmarker := startmarker + '}';
    if fb.BlockID = 'VFD_HEAD_BEGIN' then
      endmarker := Ind(2) + '{@VFD_HEAD_END: ' + fb.FormName + '}'
    else if fb.BlockID = 'VFD_BODY_BEGIN' then
      endmarker := Ind(1) + '{@VFD_BODY_END: ' + fb.FormName + '}'
    else
      endmarker := '';

    iblock := startmarker;
    if endmarker <> '' then
      iblock := iblock + LineEnding + fb.Data + endmarker;

    if fb.BlockID = 'VFD_NEWFORM_DECL' then
      iblock := NewFormsDecl + iblock
    else if fb.BlockID = 'VFD_NEWFORM_IMPL' then
    begin
      iblock   := iblock + NewFormsImpl;
      newsaved := True;
    end;

    startp := fb.Position + iofs;
    insert(iblock, rs, startp);
    Inc(iofs, length(iblock));
  end;

  if not newsaved and (NewFormsImpl <> '') then
    rs := rs + NewFormsImpl;  // do not loose new form data.

  Result := rs;
end;

procedure TVFDFile.NewFileSkeleton(AUnitname: string);
begin
  FFileData :=
    'unit ' + AUnitname + ';'+ LineEnding + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding + LineEnding +
    'interface' + LineEnding + LineEnding +
    'uses' + LineEnding +
    Ind(1) + 'SysUtils, Classes, fpg_base, fpg_main, fpg_form;' + LineEnding + LineEnding +
    'type' + LineEnding + LineEnding +
    '{@VFD_NEWFORM_DECL}' + LineEnding + LineEnding +
    'implementation' + LineEnding + LineEnding +
    '{@VFD_NEWFORM_IMPL}' + LineEnding + LineEnding +
    'end.' + LineEnding;

  GetBlocks;
end;

procedure TVFDFile.SetFormData(formname, headblock, bodyblock: string);
var
  fb: TVFDFileBlock;
begin
  fb := FindFormBlock('VFD_HEAD_BEGIN', formname);
  if fb <> nil then
    fb.Data := HeadBlock
  else
    AddNewFormDecl(formname, headblock);

  fb := FindFormBlock('VFD_BODY_BEGIN', formname);
  if fb <> nil then
    fb.Data := bodyblock
  else
    AddNewFormImpl(formname, bodyblock);
end;


end.

