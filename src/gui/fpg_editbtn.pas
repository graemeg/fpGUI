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
    //property OnEnter;
    //property OnExit;
    //property OnKeyPress;
    //property OnMouseEnter;
    //property OnMouseExit;
    //property OnPaint;
  end;


implementation

uses
  fpg_constants
  ,fpg_dialogs
  ,fpg_utils
  ;

constructor TfpgFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth              := 140;
  FHeight             := 24;
  FFilter             := '';

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
    ImageName := 'stdimg.folderopen';
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
  fnt: TfpgFont;
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
    Canvas.DrawText(0, 0, Width - Height, Height, ClassName, [txtHCenter, txtVCenter]);
    Canvas.TextColor := clText1;
    fnt := fpgGetFont('DeJaVu-9:bold');
    Canvas.SetFont(fnt);
    Canvas.DrawText(Width - Height, 0, Height, Height, '···', [txtHCenter, txtVCenter]);
    fnt.Free;
  end;
end;


end.

