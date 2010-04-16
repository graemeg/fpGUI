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
  TfpgFileNameEdit = class(TfpgPanel)
  private
    FEdit: TfpgEdit;
    FButton: TfpgButton;
    FFilter: TfpgString;
    FOnButtonClick: TNotifyEvent;
    FInitialDir: TfpgString;
    function GetExtraHint: TfpgString;
    procedure SetExtraHint(const AValue: TfpgString);
    procedure SetFilter(const AValue: TfpgString);
    procedure btnClick(Sender: TObject);
    procedure SetFileName(const AValue: TfpgString);
    function GetFileName: TfpgString;
  protected
    procedure HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetPosition(aleft, atop, awidth, aheight: TfpgCoord); override;
  published
    property ExtraHint: TfpgString read GetExtraHint write SetExtraHint;
    property FileName: TfpgString read GetFileName write SetFileName;
    property InitialDir: TfpgString read FInitialDir write FInitialDir;
    property Filter: TfpgString read FFilter write SetFilter;
    property TabOrder;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  end;


  TfpgDirectoryEdit = class(TfpgPanel)
  private
    FEdit: TfpgEdit;
    FButton: TfpgButton;
    FOnButtonClick: TNotifyEvent;
    FRootDirectory: TfpgString;
    function GetDirectory: TfpgString;
    procedure btnClick(Sender: TObject);
    function GetExtraHint: TfpgString;
    procedure SetDirectory(const AValue: TfpgString);
    procedure SetExtraHint(const AValue: TfpgString);
  protected
    procedure HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetPosition(aleft, atop, awidth, aheight: TfpgCoord);override;
  published
    property Directory: TfpgString read GetDirectory write SetDirectory;
    property ExtraHint: TfpgString read GetExtraHint write SetExtraHint;
    property RootDirectory: TfpgString read FRootDirectory write FRootDirectory;
    property TabOrder;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  end;


  TfpgFontEdit = class(TfpgPanel)
  private
    FEdit: TfpgEdit;
    FButton: TfpgButton;
    FOnButtonClick: TNotifyEvent;
    procedure btnClick(Sender: TObject);
  protected
    function GetFontDesc: string; override;
    procedure SetFontDesc(const AValue: string); override;
    procedure HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetPosition(aleft, atop, awidth, aheight: TfpgCoord);override;
  published
    property FontDesc;
    property TabOrder;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  end;


implementation

uses
  fpg_constants
  ,fpg_dialogs
  ,fpg_utils
  ;


{ TfpgFileNameEdit }

constructor TfpgFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text            := '';
  FWidth          := 140;
  FHeight         := 24;
  FFilter         := '';

  FEdit := TfpgEdit.Create(self);
  with FEdit do
  begin
    Name := 'FEdit';
    Text := '';
    FontDesc := '#Edit1';
    TabOrder := 0;
  end;

  FButton := TfpgButton.Create(self);
  with FButton do
  begin
    Name := 'FButton';
    Text := '';
    FontDesc := '#Label1';
    ImageMargin := -1;
    ImageName := 'stdimg.folderfile';
    ImageSpacing := 0;
    TabOrder := 1;
    OnClick := @btnClick;
  end;
end;

procedure TfpgFileNameEdit.SetFilter(const AValue: TfpgString);
begin
  FFilter := AValue;
end;

function TfpgFileNameEdit.GetExtraHint: TfpgString;
begin
  Result := FEdit.ExtraHint;
end;

procedure TfpgFileNameEdit.SetExtraHint(const AValue: TfpgString);
begin
  FEdit.ExtraHint := AValue;
end;

procedure TfpgFileNameEdit.SetPosition(aleft, atop, awidth, aheight: TfpgCoord);
begin
  inherited SetPosition(aleft, atop, awidth, aheight);
  FEdit.SetPosition(0, 0, Width - Height, Height);
  FButton.SetPosition(Width - Height, 0, Height, Height);
end;

procedure TfpgFileNameEdit.btnClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
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
  if Assigned(OnButtonClick) then
    OnButtonClick(self);
end;

procedure TfpgFileNameEdit.SetFileName(const AValue: TfpgString);
begin
  FEdit.Text := AValue;
end;

function TfpgFileNameEdit.GetFileName: TfpgString;
begin
  Result := FEdit.Text;
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


{ TfpgDirectoryEdit}

constructor TfpgDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text            := '';
  FWidth          := 140;
  FHeight         := 24;

  FEdit := TfpgEdit.Create(self);
  with FEdit do
  begin
    Name := 'FEdit';
    Text := '';
    FontDesc := '#Edit1';
    TabOrder := 0;
  end;

  FButton := TfpgButton.Create(self);
  with FButton do
  begin
    Name := 'FButton';
    Text := '';
    FontDesc := '#Label1';
    ImageMargin := -1;
    ImageName := 'stdimg.folder';
    ImageSpacing := 0;
    TabOrder := 1;
    OnClick := @btnClick;
  end;
end;

procedure TfpgDirectoryEdit.SetPosition(aleft, atop, awidth, aheight: TfpgCoord);
begin
  inherited SetPosition(aleft, atop, awidth, aheight);
  FEdit.SetPosition(0, 0, Width - Height, Height);
  FButton.SetPosition(Width - Height, 0, Height, Height);
end;

function TfpgDirectoryEdit.GetDirectory: TfpgString;
begin
  Result := FEdit.Text;
end;

procedure TfpgDirectoryEdit.btnClick(Sender: TObject);
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
  if Assigned(OnButtonClick) then
	  OnButtonClick(self);
end;

function TfpgDirectoryEdit.GetExtraHint: TfpgString;
begin
  Result := FEdit.ExtraHint;
end;

procedure TfpgDirectoryEdit.SetDirectory(const AValue: TfpgString);
begin
  FEdit.Text := AValue;
end;

procedure TfpgDirectoryEdit.SetExtraHint(const AValue: TfpgString);
begin
  FEdit.ExtraHint := AValue;
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


{ TfpgFontEdit }

procedure TfpgFontEdit.btnClick(Sender: TObject);
var
  f: TfpgString;
begin
  f := FontDesc;
  if SelectFontDialog(f) then
    FontDesc := f;
  if Assigned(OnButtonClick) then
	  OnButtonClick(self);
end;

function TfpgFontEdit.GetFontDesc: string;
begin
  Result := inherited GetFontDesc;
  if Result <> FEdit.Text then  // user must have entered fontdesc directly
  begin
    Result := FEdit.Text;
    SetFontDesc(Result); // update internal fontdesc to sync with FEdit value
  end;
end;

procedure TfpgFontEdit.SetFontDesc(const AValue: string);
begin
  inherited SetFontDesc(AValue);
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

constructor TfpgFontEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text            := '';
  FWidth          := 140;
  FHeight         := 24;

  FEdit := TfpgEdit.Create(self);
  with FEdit do
  begin
    Name := 'FEdit';
    Text := '#Edit1';
    FontDesc := '#Edit1';
    TabOrder := 0;
  end;

  FButton := TfpgButton.Create(self);
  with FButton do
  begin
    Name := 'FButton';
    Text := '';
    FontDesc := '#Label1';
    ImageMargin := -1;
    ImageName := 'stdimg.font';
    ImageSpacing := 0;
    TabOrder := 1;
    OnClick := @btnClick;
  end;
end;

procedure TfpgFontEdit.SetPosition(aleft, atop, awidth, aheight: TfpgCoord);
begin
  inherited SetPosition(aleft, atop, awidth, aheight);
  FEdit.SetPosition(0, 0, Width - Height, Height);
  FButton.SetPosition(Width - Height, 0, Height, Height);
end;

end.

