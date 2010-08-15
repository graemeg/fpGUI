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
      A component implementing a 'Most Recently Used' feature normally
      inserted in the File menu.
}

unit fpg_mru;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_menu;
  
type

  TMRUClickEvent = procedure(Sender: TObject; const FileName: String) of object;

  TfpgMRU = class(TComponent)
  private
    FItems: TStringList;
    FMaxItems: Integer;
    FShowFullPath: boolean;
    FParentMenuItem: TfpgPopupMenu;
//    FIniFilePath: string;
    FOnClick: TMRUClickEvent;
    procedure   SetMaxItems(const AValue: Integer);
//    procedure   SetIniFilePath(const AValue: string);
    procedure   SetParentMenuItem(const AValue: TfpgPopupMenu);
    procedure   SetShowFullPath(const AValue: boolean);
    procedure   SaveMRU;
    procedure   ItemsChange(Sender: TObject);
    procedure   ClearParentMenu;
  protected
    // this never gets called without a Form Streaming class, which fpGUI doesn't use
    procedure   Loaded; override;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure   DoClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AddItem(const FileName: string);
    function    RemoveItem(const FileName : string) : boolean;
    procedure   LoadMRU;
  published
    property    MaxItems: Integer read FMaxItems write SetMaxItems default 4;
//    property    IniFilePath: string read FIniFilePath write SetIniFilePath;
    property    ShowFullPath: boolean read FShowFullPath write SetShowFullPath default True;
    property    ParentMenuItem: TfpgPopupMenu read FParentMenuItem write SetParentMenuItem;
    property    OnClick: TMRUClickEvent read FOnClick write FOnClick;
  end;


implementation

uses
  fpg_iniutils;

type
  //to be able to recognize MRU menu item when deleting
  TMRUMenuItem = class(TfpgMenuItem);


{ TfpgMRU }

procedure TfpgMRU.SetMaxItems(const AValue: Integer);
begin
  if AValue <> FMaxItems then
  begin
    if AValue < 1 then
      FMaxItems := 1
    else
    begin
      if AValue > High(Word) then // 65535 should be enough
        FMaxItems := High(Word)
      else
      begin
        FMaxItems := AValue;
        FItems.BeginUpdate;
        try
          while FItems.Count > MaxItems do
            FItems.Delete(FItems.Count - 1);
        finally
          FItems.EndUpdate;
        end;
      end;
    end;  { if/else }
  end;
end;

{
procedure TfpgMRU.SetIniFilePath(const AValue: string);
begin
  if FIniFilePath=AValue then exit;
  FIniFilePath:=AValue;
end;
}

procedure TfpgMRU.SetParentMenuItem(const AValue: TfpgPopupMenu);
begin
  if AValue = FParentMenuItem then
    Exit;
  FParentMenuItem := AValue;
end;

procedure TfpgMRU.SetShowFullPath(const AValue: boolean);
begin
  if FShowFullPath <> AValue then
  begin
    FShowFullPath := AValue;
    ItemsChange(Self);
  end;
end;

procedure TfpgMRU.LoadMRU;
var
  i: cardinal;
begin
  FItems.BeginUpdate;
  FItems.Clear;
  try
    for i := 1 to FMaxItems do
      if gINI.ValueExists('MRU', 'MRU'+IntToStr(i)) then
        FItems.Add(gINI.ReadString('MRU', 'MRU'+IntToStr(i), ''));
  finally
    FItems.EndUpdate;
  end;
end;

procedure TfpgMRU.SaveMRU;
var
  i: integer;
begin
  if FItems.Count = 0 then
    Exit;
    
  //delete old mru
  i := 1;
  while gINI.ValueExists('MRU', 'MRU'+IntToStr(i)) do
  begin
    gINI.DeleteKey('MRU', 'MRU'+IntToStr(i));
    Inc(i);
  end;

  //write new mru
  for i := 0 to FItems.Count-1 do
    gINI.WriteString('MRU', 'MRU'+IntToStr(i+1), FItems[i]);
end;

procedure TfpgMRU.ItemsChange(Sender: TObject);
var
  i: Integer;
  NewMenuItem: TfpgMenuItem;
  FileName: String;
begin
//  writeln('TfpgMRU.ItemsChange');
  if ParentMenuItem <> nil then
  begin
    ClearParentMenu;
    if FItems.Count = 0 then
      ParentMenuItem.AddMenuItem('-', '', nil); // add something if we have no previous MRU's
    for i := 0 to -1 + FItems.Count do
    begin
      if ShowFullPath then
        FileName := StringReplace(FItems[I], '&', '&&', [rfReplaceAll, rfIgnoreCase])
      else
        FileName := StringReplace(ExtractFileName(FItems[i]), '&', '&&', [rfReplaceAll, rfIgnoreCase]);

//      NewMenuItem := ParentMenuItem.AddMenuItem(Format('%s', [FileName]), '', @DoClick);
//      NewMenuItem.Tag := i;
      NewMenuItem := TMRUMenuItem.Create(ParentMenuItem);
      NewMenuItem.Text    := Format('%s', [FileName]);
      NewMenuItem.Tag     := i;
      NewMenuItem.OnClick := @DoClick;
    end;
  end;
end;

procedure TfpgMRU.ClearParentMenu;
//var
//  i:integer;
begin
  if Assigned(ParentMenuItem) then
    ParentMenuItem.DestroyComponents;
{
    for i := ParentMenuItem.ComponentCount-1 downto 0 do
      if ParentMenuItem.Components[i] is TMRUMenuItem then
        ParentMenuItem.Delete(i);
}
end;

procedure TfpgMRU.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
//    if FIniFilePath <> '' then
      LoadMRU;
end;

procedure TfpgMRU.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FParentMenuItem) then
    FParentMenuItem := nil;
end;

procedure TfpgMRU.DoClick(Sender: TObject);
begin
  if Assigned(FOnClick) and (Sender is TMRUMenuItem) then
    FOnClick(Self, FItems[TMRUMenuItem(Sender).Tag]);
end;

constructor TfpgMRU.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentMenuItem := nil;
  FItems := TStringList.Create;
  FItems.OnChange := @ItemsChange;
  FMaxItems := 4;
  FShowFullPath := True;
//  Loaded;
end;

destructor TfpgMRU.Destroy;
begin
  if not (csDesigning in ComponentState) then
    SaveMRU;
  FItems.OnChange := nil;
  FItems.Free;
  inherited Destroy;
end;

procedure TfpgMRU.AddItem(const FileName: string);
begin
  if FileName <> '' then
  begin
    FItems.BeginUpdate;
    try
      if FItems.IndexOf(FileName) > -1 then
        FItems.Delete(FItems.IndexOf(FileName));
      FItems.Insert(0, FileName);

      while FItems.Count > MaxItems do
        FItems.Delete(MaxItems);
    finally
      FItems.EndUpdate;
    end;
  end;
end;

function TfpgMRU.RemoveItem(const FileName: string): boolean;
begin
  if FItems.IndexOf(FileName) > -1 then
  begin
    FItems.Delete(FItems.IndexOf(FileName));
    Result := True;
  end
  else
    Result := False;
end;

end.

