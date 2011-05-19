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
      This unit contains various "composite" components. Components that
      work together as a single component.
}

unit fpg_editbtn;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,fpg_base
  ,fpg_main
  ,fpg_widget
  ,fpg_edit
  ,fpg_button
  ,fpg_panel
  ;

type

  TfpgBaseEditButton = class(TfpgAbstractPanel)
  private
    FOnButtonClick: TNotifyEvent;
    FReadOnly: Boolean;
    procedure SetReadOnly(const AValue: Boolean);
    function GetExtraHint: TfpgString;
    procedure SetExtraHint(const AValue: TfpgString);
  protected
    FEdit: TfpgEdit;
    FButton: TfpgButton;
    function  GetOnShowHint: THintEvent; override;
    procedure SetOnShowHint(const AValue: THintEvent); override;
    procedure SetHint(const AValue: TfpgString); override;
    function  GetHint: TfpgString; override;
    procedure InternalButtonClick(Sender: TObject); virtual;
    procedure HandleResize(AWidth, AHeight: TfpgCoord); override;
    property  ExtraHint: TfpgString read GetExtraHint write SetExtraHint;
    property  ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property  OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TFilenameSetEvent = procedure(Sender: TObject; const AOldValue, ANewValue: TfpgString) of object;

  TfpgFileNameEdit = class(TfpgBaseEditButton)
  private
    FOnFilenameSet: TFilenameSetEvent;
    FFilter: TfpgString;
    FInitialDir: TfpgString;
    procedure SetFilter(const AValue: TfpgString);
    procedure SetFileName(const AValue: TfpgString);
    function GetFileName: TfpgString;
    procedure DoFilenameSet(const AOld, ANew: TfpgString);
  protected
    procedure HandlePaint; override;
    procedure InternalButtonClick(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Align;
    property    Enabled;
    property    ExtraHint;
    property    FileName: TfpgString read GetFileName write SetFileName;
    property    InitialDir: TfpgString read FInitialDir write FInitialDir;
    property    Filter: TfpgString read FFilter write SetFilter;
    property    ReadOnly;
    property    TabOrder;
    property    OnButtonClick;
    property    OnShowHint;
    property    OnFilenameSet: TFilenameSetEvent read FOnFilenameSet write FOnFilenameSet;
  end;


  TfpgDirectoryEdit = class(TfpgBaseEditButton)
  private
    FRootDirectory: TfpgString;
    function GetDirectory: TfpgString;
    procedure SetDirectory(const AValue: TfpgString);
  protected
    procedure HandlePaint; override;
    procedure InternalButtonClick(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Align;
    property    Directory: TfpgString read GetDirectory write SetDirectory;
    property    Enabled;
    property    ExtraHint;
    property    RootDirectory: TfpgString read FRootDirectory write FRootDirectory;
    property    ReadOnly;
    property    TabOrder;
    property    OnButtonClick;
    property    OnShowHint;
  end;


  TfpgFontEdit = class(TfpgBaseEditButton)
  protected
    function GetFontDesc: TfpgString; virtual;
    procedure SetFontDesc(const AValue: TfpgString); virtual;
    procedure HandlePaint; override;
    procedure InternalButtonClick(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Align;
    property    Enabled;
    property    ExtraHint;
    property    FontDesc: TfpgString read GetFontDesc write SetFontDesc;
    property    ReadOnly;
    property    TabOrder;
    property    OnButtonClick;
    property    OnShowHint;
  end;


  TfpgEditButton =  class(TfpgBaseEditButton)
  protected
    function    GetText: TfpgString;
    procedure   SetText(const AValue: TfpgString);
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Align;
    property    Enabled;
    property    ExtraHint;
    property    ReadOnly;
    property    TabOrder;
    property    Text: TfpgString read GetText write SetText;
    property    OnButtonClick;
    property    OnShowHint;
  end;


implementation

uses
  fpg_constants
  ,fpg_dialogs
  ,fpg_utils
  ;

{ TfpgEditButton }

function TfpgEditButton.GetText: TfpgString;
begin
  Result := FEdit.Text;
end;

procedure TfpgEditButton.SetText(const AValue: TfpgString);
begin
  FEdit.Text := AValue;
end;

procedure TfpgEditButton.HandlePaint;
var
  img: TfpgImage;
begin
  inherited HandlePaint;
  // only so that it looks pretty in the UI Designer
  if csDesigning in ComponentState then
  begin
    FEdit.Visible := False;
    FButton.Visible := False;
    Canvas.Clear(clBoxColor);
    fpgStyle.DrawControlFrame(Canvas, 0, 0, Width - Height, Height);
    fpgStyle.DrawButtonFace(Canvas, Width - Height, 0, Height, Height, [btfIsEmbedded]);
    Canvas.SetFont(fpgApplication.DefaultFont);
    if Text <> '' then
    begin
      Canvas.TextColor := clText3;
      Canvas.DrawText(4, 0, Width - Height, Height, Text, [txtLeft, txtVCenter]);
    end
    else
    begin
      Canvas.TextColor := clShadow1;
      Canvas.DrawText(0, 0, Width - Height, Height, ClassName, [txtHCenter, txtVCenter]);
    end;
    img := fpgImages.GetImage('stdimg.ellipse'); // don't free the img instance - we only got a reference
    if img <> nil then
      Canvas.DrawImage(Width-Height+((Height-img.Width) div 2), (Height-img.Height) div 2, img);
  end;
end;

constructor TfpgEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton.ImageName := 'stdimg.ellipse';
end;


{ TfpgBaseEditButton }

procedure TfpgBaseEditButton.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly = AValue then
    Exit;
  FReadOnly := AValue;
  FEdit.ReadOnly := FReadOnly;
  FButton.Enabled := not FReadOnly;   // Buttons don't have ReadOnly property.
end;

function TfpgBaseEditButton.GetExtraHint: TfpgString;
begin
  Result := FEdit.ExtraHint;
end;

procedure TfpgBaseEditButton.SetExtraHint(const AValue: TfpgString);
begin
  FEdit.ExtraHint := AValue;
end;

function TfpgBaseEditButton.GetOnShowHint: THintEvent;
begin
  // rewire the FEdit event to the parent (composite) component
  Result := FEdit.OnShowHint;
end;

procedure TfpgBaseEditButton.SetOnShowHint(const AValue: THintEvent);
begin
  // rewire the FEdit event to the parent (composite) component
  FEdit.OnShowHint := AValue;
end;

procedure TfpgBaseEditButton.SetHint(const AValue: TfpgString);
begin
  FEdit.Hint := AValue;
end;

function TfpgBaseEditButton.GetHint: TfpgString;
begin
  Result := FEdit.Hint;
end;

procedure TfpgBaseEditButton.InternalButtonClick(Sender: TObject);
begin
  // do nothing
  if Assigned(OnButtonClick) then
    OnButtonClick(self);
end;

procedure TfpgBaseEditButton.HandleResize(AWidth, AHeight: TfpgCoord);
begin
  inherited HandleResize(AWidth, AHeight);
  { resizing can now occur before the component is shown, so we need extra
    checks here, like are we still busy creating everything. }
  if not (csLoading in ComponentState) then
  begin
    if csDesigning in ComponentState then
    begin
      FEdit.Visible := False;
      FButton.Visible := False;
    end
    else
    begin
        FEdit.SetPosition(0, 0, AWidth - AHeight, AHeight);
        FButton.SetPosition(AWidth - AHeight, 0, AHeight, AHeight);
    end;
  end;
end;

constructor TfpgBaseEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width           := 140;
  Height          := 24;
  FReadOnly       := False;

  FEdit := TfpgEdit.Create(self);
  with FEdit do
  begin
    Name := 'FEdit';
    SetPosition(0, 0, self.Width - self.Height, self.Height);
    Text := '';
    FontDesc := '#Edit1';
    TabOrder := 0;
  end;

  FButton := TfpgButton.Create(self);
  with FButton do
  begin
    Name := 'FButton';
    SetPosition(self.Width - self.Height, 0, self.Height, self.Height);
    Text := '';
    FontDesc := '#Label1';
    ImageMargin := -1;
    ImageName := 'stdimg.elipses';
    ImageSpacing := 0;
    TabOrder := 1;
    OnClick := @InternalButtonClick;
  end;
end;



{ TfpgFileNameEdit }

constructor TfpgFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilter         := '';
  FButton.ImageName := 'stdimg.folderfile';
end;

procedure TfpgFileNameEdit.SetFilter(const AValue: TfpgString);
begin
  FFilter := AValue;
end;

procedure TfpgFileNameEdit.SetFileName(const AValue: TfpgString);
begin
  FEdit.Text := AValue;
end;

function TfpgFileNameEdit.GetFileName: TfpgString;
begin
  Result := FEdit.Text;
end;

procedure TfpgFileNameEdit.DoFilenameSet(const AOld, ANew: TfpgString);
begin
  if Assigned(FOnFilenameSet) then
    FOnFilenameSet(self, AOld, ANew);
end;

procedure TfpgFileNameEdit.HandlePaint;
var
  img: TfpgImage;
begin
  inherited HandlePaint;
  // only so that it looks pretty in the UI Designer
  if csDesigning in ComponentState then
  begin
    FEdit.Visible := False;
    FButton.Visible := False;
    Canvas.Clear(clBoxColor);
    fpgStyle.DrawControlFrame(Canvas, 0, 0, Width - Height, Height);
    fpgStyle.DrawButtonFace(Canvas, Width - Height, 0, Height, Height, [btfIsEmbedded]);
    Canvas.SetFont(fpgApplication.DefaultFont);
    if Filename <> '' then
    begin
      Canvas.TextColor := clText3;
      Canvas.DrawText(4, 0, Width - Height, Height, Filename, [txtLeft, txtVCenter]);
    end
    else
    begin
      Canvas.TextColor := clShadow1;
      Canvas.DrawText(0, 0, Width - Height, Height, ClassName, [txtHCenter, txtVCenter]);
    end;
    img := fpgImages.GetImage('stdimg.folderfile'); // don't free the img instance - we only got a reference
    if img <> nil then
      Canvas.DrawImage(Width-Height+((Height-img.Width) div 2), (Height-img.Height) div 2, img);
  end;
end;

procedure TfpgFileNameEdit.InternalButtonClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
  old: TfpgString;
begin
  old := FEdit.Text;
  dlg := TfpgFileDialog.Create(nil);
  try
    if FileName = '' then
    begin
      if FInitialDir <> '' then
        dlg.InitialDir := FInitialDir;
    end
    else
    begin
      // Use path of existing filename
      dlg.InitialDir := fpgExtractFilePath(FileName);
      if dlg.InitialDir = '' then    // FileName had no path
        dlg.InitialDir := FInitialDir;
    end;
    if FFilter = '' then
      dlg.Filter := rsAllFiles + ' (' + AllFilesMask + ')' + '|' + AllFilesMask
    else
      dlg.Filter :=  FFilter + '|' + rsAllFiles + ' (' + AllFilesMask + ')' + '|' + AllFilesMask;
    if dlg.RunOpenFile then
    begin
      FEdit.Text := dlg.FileName;
    end;
  finally
    dlg.Free;
  end;
  inherited InternalButtonClick(Sender);
  if old <> FEdit.Text then
    DoFilenameSet(old, FEdit.Text);
end;


{ TfpgDirectoryEdit}

constructor TfpgDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton.ImageName := 'stdimg.folder';
end;

function TfpgDirectoryEdit.GetDirectory: TfpgString;
begin
  Result := FEdit.Text;
end;

procedure TfpgDirectoryEdit.SetDirectory(const AValue: TfpgString);
begin
  FEdit.Text := AValue;
end;

procedure TfpgDirectoryEdit.HandlePaint;
var
  img: TfpgImage;
begin
  inherited HandlePaint;
  // only so that it looks pretty in the UI Designer
  if csDesigning in ComponentState then
  begin
    FEdit.Visible := False;
    FButton.Visible := False;
    Canvas.Clear(clBoxColor);
    fpgStyle.DrawControlFrame(Canvas, 0, 0, Width - Height, Height);
    fpgStyle.DrawButtonFace(Canvas, Width - Height, 0, Height, Height, [btfIsEmbedded]);
    Canvas.SetFont(fpgApplication.DefaultFont);
    if Directory <> '' then
    begin
      Canvas.TextColor := clText3;
      Canvas.DrawText(4, 0, Width - Height, Height, Directory, [txtLeft, txtVCenter]);
    end
    else
    begin
      Canvas.TextColor := clShadow1;
      Canvas.DrawText(0, 0, Width - Height, Height, ClassName, [txtHCenter, txtVCenter]);
    end;
    img := fpgImages.GetImage('stdimg.folder'); // don't free the img instance - we only got a reference
    if img <> nil then
      Canvas.DrawImage(Width-Height+((Height-img.Width) div 2), (Height-img.Height) div 2, img);
  end;
end;

procedure TfpgDirectoryEdit.InternalButtonClick(Sender: TObject);
var
  dlg: TfpgSelectDirDialog;
begin
  dlg := TfpgSelectDirDialog.Create(nil);
  try
    if FRootDirectory <> '' then
      dlg.RootDirectory := FRootDirectory;
    dlg.SelectedDir := Directory;
    if dlg.ShowModal = mrOK then
    begin
      FEdit.Text:= dlg.SelectedDir;
    end;
  finally
    dlg.Free;
  end;
  inherited InternalButtonClick(Sender);
end;


{ TfpgFontEdit }

function TfpgFontEdit.GetFontDesc: TfpgString;
begin
  Result := FEdit.Text;
end;

procedure TfpgFontEdit.SetFontDesc(const AValue: TfpgString);
begin
  FEdit.Text := AValue;
end;

procedure TfpgFontEdit.HandlePaint;
var
  img: TfpgImage;
begin
  inherited HandlePaint;
  // only so that it looks pretty in the UI Designer
  if csDesigning in ComponentState then
  begin
    FEdit.Visible := False;
    FButton.Visible := False;
    Canvas.Clear(clBoxColor);
    fpgStyle.DrawControlFrame(Canvas, 0, 0, Width - Height, Height);
    fpgStyle.DrawButtonFace(Canvas, Width - Height, 0, Height, Height, [btfIsEmbedded]);
    Canvas.TextColor := clShadow1;
    Canvas.SetFont(fpgApplication.DefaultFont);
    Canvas.DrawText(0, 0, Width - Height, Height, ClassName, [txtHCenter, txtVCenter]);
    img := fpgImages.GetImage('stdimg.font'); // don't free the img instance - we only got a reference
    if img <> nil then
      Canvas.DrawImage(Width-Height+((Height-img.Width) div 2), (Height-img.Height) div 2, img);
  end;
end;

procedure TfpgFontEdit.InternalButtonClick(Sender: TObject);
var
  f: TfpgString;
begin
  f := FontDesc;
  if SelectFontDialog(f) then
    FontDesc := f;
  inherited InternalButtonClick(Sender);
end;

constructor TfpgFontEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton.ImageName := 'stdimg.font';
end;


end.

