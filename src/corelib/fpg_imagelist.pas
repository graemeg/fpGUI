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

      A basic image list component for use by the GUI components like the
      treeview etc.
}

unit fpg_imagelist;

{$mode objfpc}{$H+}

{.$define DEBUG}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main;
  
type

  EItemExists = class(Exception);

  TfpgImageList = class;  // forward declaration
  

  TfpgImageItem = class(TObject)
  private
    FImage: TfpgImage;
    FIndex: integer;
    FImageList: TfpgImageList;
    procedure   SetImageList(AImageList: TfpgImageList);
    procedure   SetIndex(AIndex: integer);
    procedure   SetImage(AImage: TfpgImage);
  public
    constructor Create; overload;
    constructor Create(AImageList: TfpgImageList; AIndex: integer; AImage: TfpgImage); overload;
    constructor Create(AFileName: TfpgString; AIndex: integer); overload;
    destructor  Destroy; override;
    property    Index: integer read FIndex write SetIndex;
    property    Image: TfpgImage read FImage write SetImage;
    property    ImageList: TfpgImageList read FImageList write SetImageList;
    procedure   LoadFromFile(AFileName: TfpgString);
  end;


  TfpgImageList = class(TObject)
  private
    FList: TList;
    function    GetFListIndex(AIndex: Integer): Integer;
    function    GetItem(AIndex: integer): TfpgImageItem;
    procedure   SetItem(AIndex: integer; AItem: TfpgImageItem);
    function    GetCount: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   AddItemFromFile(AFileName: TfpgString; AIndex: integer = -1);
    procedure   AddImage(AImage: TfpgImage; AIndex: integer = -1);
    procedure   RemoveIndex(AIndex: integer);
    function    GetMaxItem: integer;
    procedure   Clear;
    property    Items[AIndex: integer]: TfpgImageItem read GetItem write SetItem; default;
    property    Count: integer read GetCount;
  end;

  

implementation

uses
  fpg_imgfmt_bmp,
  fpg_utils;

{ TfpgImageList }

function TfpgImageList.GetFListIndex(AIndex: Integer): Integer;
var
  i: integer;
begin
  {$IFDEF DEBUG}
  writeln('TfpgImageList.GetFListIndex');
  {$ENDIF}
  result := -1;
  for i := 0 to FList.Count - 1 do
    if TfpgImageItem(FList[i]).Index = AIndex then
    begin
      result := i;
      Break;  //==>
    end;
end;

function TfpgImageList.GetItem(AIndex: integer): TfpgImageItem;
var
  AFindIndex: integer;
begin
  {$IFDEF DEBUG}
  writeln('TfpgImageList.GetItem');
  {$ENDIF}
  result := nil;
  AFindIndex := GetFListIndex(AIndex);
  if AFindIndex > -1 then
    result := TfpgImageItem(FList[AFindIndex]);
end;

procedure TfpgImageList.SetItem(AIndex: integer; AItem: TfpgImageItem);
begin
  if AItem = nil then
    Exit; //==>
    
  if GetItem(AIndex) = AItem then
    Exit; //==>
    
  RemoveIndex(AIndex);      // delete existing Item
  AItem.Index := AIndex;
  FList.Add(AItem);
end;

function TfpgImageList.GetCount: integer;
begin
  Result := FList.Count;
end;

constructor TfpgImageList.Create;
begin
  FList := TList.Create;
end;

destructor TfpgImageList.Destroy;
var
  i: integer;
begin
  for i := FList.Count-1 downto 0 do
    TfpgImageItem(FList[i]).Destroy;  // frees images
  FList.Destroy;
  inherited Destroy
end;

procedure TfpgImageList.AddItemFromFile(AFileName: TfpgString; AIndex: integer);
var
  AImageItem: TfpgImageItem;
begin
  {$IFDEF DEBUG}
  writeln('TfpgImageList.AddItemFromFile');
  {$ENDIF}
  
  if not fpgFileExists(AFileName) then
    Exit; //==>
  
  AImageItem := TfpgImageItem.Create;
  AImageItem.LoadFromFile(AFileName);
  if AIndex > -1 then
    Items[AIndex] := AImageItem
  else
  begin
    FList.Add(AImageItem);
    AImageItem.Index := GetMaxItem+1;
  end;
end;

procedure TfpgImageList.AddImage(AImage: TfpgImage; AIndex: integer);
var
  AImageItem: TfpgImageItem;
begin
  AImageItem := TfpgImageItem.Create;
  AImageItem.Image := AImage;
  if AIndex > -1 then
    Items[AIndex] := AImageItem
  else
  begin
    FList.Add(AImageItem);
    AImageItem.Index := GetMaxItem+1;
  end;
end;

procedure TfpgImageList.RemoveIndex(AIndex: integer);
begin
  {$IFDEF DEBUG}
  writeln('TfpgImageList.RemoveIndex');
  {$ENDIF}
  AIndex := GetFListIndex(AIndex);
  if AIndex <> -1 then
  begin
    TfpgImageItem(FList[AIndex]).Destroy;
    FList.Delete(AIndex);
  end;
end;

function TfpgImageList.GetMaxItem: integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to FList.Count - 1 do
    if TfpgImageItem(FList[i]).Index > result then
      result := TfpgImageItem(FList[i]).Index;
end;

procedure TfpgImageList.Clear;
begin
  FList.Clear;
end;

{ TfpgImageItem }

procedure TfpgImageItem.SetImageList(AImageList: TfpgImageList);
begin
  if AImageList = nil then
  begin
    FImageList := nil;
  end
  else
  begin
    if FImageList <> nil then
      FImageList.RemoveIndex(Index);
    FImageList := AImageList;
    FImageList.Items[Index] := self;
  end;
end;

procedure TfpgImageItem.SetIndex(AIndex: integer);
begin
  {$IFDEF DEBUG}
  writeln('TfpgImageItem.SetIndex');
  {$ENDIF}
  if AIndex <> FIndex then
  begin
    if ImageList <> nil then
      ImageList.RemoveIndex(AIndex);
    FIndex := AIndex;
  end;
end;

procedure TfpgImageItem.SetImage(AImage: TfpgImage);
begin
  {$IFDEF DEBUG}
  writeln('TfpgImageItem.SetImage');
  {$ENDIF}
  FImage := AImage;
end;

constructor TfpgImageItem.Create;
begin
  ImageList := nil;
  FIndex    := -1;
  FImage    := nil;
end;

constructor TfpgImageItem.Create(AImageList: TfpgImageList; AIndex: integer;
    AImage: TfpgImage);
begin
  if AImageList = nil then
    Exit; //==>
  FImage      := AImage;
  FIndex      := AIndex;
  FImageList  := nil;
  ImageList   := AImageList;
end;

constructor TfpgImageItem.Create(AFileName: TfpgString; AIndex: integer);
begin
  {$IFDEF DEBUG}
  writeln('TfpgImageItem.Create(', AFileName, ',', AIndex, ')');
  {$ENDIF}
  Index := AIndex;
  LoadFromFile(AFileName);
end;

destructor TfpgImageItem.Destroy;
begin
  if FImage <> nil then
     FImage.Free;
  inherited Destroy;
end;

procedure TfpgImageItem.LoadFromFile(AFileName: TfpgString);
begin
  {$IFDEF DEBUG}
  writeln('TfpgImageItem.LoadFromFile');
  {$ENDIF}
  if FImage <> nil then
    FImage.Destroy;
  FImage := LoadImage_BMP(AFileName);
end;

end.

