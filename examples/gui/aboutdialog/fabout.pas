{
    Demo fpGUI "about box" using app meta
    Written by Jonathan A. Foster <jon@jfpossibilities.com>
    Started October 10th, 2017

    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2019 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit fAbout;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, classes,
  fpg_constants, fpg_base, fpg_main, fpg_widget, fpg_button, fpg_dialogs,
  fpg_label;



{ These are the /keys/ we don't care about when comparing shift states for
  working with hot-keys. In other words I don't usually care about mouse
  actions or the key-lock states when determining if a hot-key has been hit.
  [CAPS]+[ESC] == [LEFT]+[ESC] == [ESC] }
const
    ssKeyOnly: TShiftState = [
        ssCaps, ssNum, ssScroll,   { lock keys }
        ssLeft, ssMiddle, ssRight, { mouse buttons }
        ssDouble, ssTriple, ssQuad { multi-mouse-clicks }
    ];



{*********************************************************************
 * About dialog box                                                  *
 *********************************************************************}
{ TODO: load the app icon in the upper left corner }
type
  TfpgAbout = class(TfpgBaseDialog)
  protected
    {@VFD_HEAD_BEGIN: fpgAbout}
    lTitle: TfpgLabel;
    lAuthor: TfpgLabel;
    lCopyright: TfpgLabel;
    lSite: TfpgLabel;
    lUsing: TfpgLabel;
    {@VFD_HEAD_END: fpgAbout}
    url: string;
    CopyrightID: integer; // INF ID of copyright page

    procedure   DoKeyShortcut(const AOrigin: TfpgWidget; const key: word; const shift: TShiftState; var consumed: boolean; const IsChildOfOrigin: boolean = False); override;
    procedure   FormShow(sender: TObject); virtual;
    procedure   lSiteClick(sender: TObject); virtual;
    procedure   lCopyrightClick(sender: TObject); virtual;
    function    TitleGet: string;
    procedure   TitleSet(const s: string);
    function    AuthorGet: string;
    procedure   AuthorSet(const s: string);
    function    CopyrightGet: String;
    procedure   CopyrightSet(const s: string);
    function    SiteGet: string;
    procedure   SiteSet(const s: string);
  public
    constructor create(AOwner: TComponent); override;
    property    title: string read TitleGet write TitleSet;
    property    author: string read AuthorGet write AuthorSet;
    property    copyright: string read CopyrightGet write CopyrightSet;
    property    site: string read SiteGet write SiteSet;
  end;



function AboutDlg: TfpgModalResult; // Not sure why. But why not?



implementation

uses
 fpg_utils
 //,fpg_imgfmt_bmp
 ;

{*********************************************************************
 * TfpgAbout                                                         *
 *********************************************************************}

constructor TfpgAbout.create(AOwner: TComponent);
const
  w = 300; // form width;
  h = 160; // form height
var
  s: string;
begin
  inherited create(AOwner);
  {@VFD_BODY_BEGIN: fpgAbout}
  Name := 'fpgAbout';
  MinWidth := w; // these two override parent class
  MinHeight := h;
  SetPosition(0, 0, w, h);
  WindowTitle := 'About';
  Hint := '';
  OnShow:=@FormShow;

  lTitle := TfpgLabel.Create(self);
  with lTitle do
  begin
    Name := 'lTitle';
    SetPosition(0, 8, w, 22);
    anchors:=[anLeft, anTop, anRight];
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  lAuthor := TfpgLabel.Create(self);
  with lAuthor do
  begin
    Name := 'lAuthor';
    SetPosition(0, 32, w, 22);
    anchors:=[anLeft, anTop, anRight];
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := fpgApplication.AppAuthor;
  end;

  lCopyright := TfpgLabel.Create(self);
  with lCopyright do
  begin
    Name := 'lCopyright';
    anchors:=[anLeft, anTop, anRight];
    SetPosition(0, 56, w, 22);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := fpgApplication.AppCopyright;
    OnClick := @lCopyrightClick;
  end;

  lSite := TfpgLabel.Create(self);
  with lSite do
  begin
    Name := 'lSite';
    SetPosition(0, 80, w, 22);
    anchors:=[anLeft, anTop, anRight];
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := fpgApplication.AppSiteName;
    OnClick:=@lSiteClick;
  end;

  lUsing := TfpgLabel.create(self);
  with lUsing do begin
    name:='lUsing';
    SetPosition(0, 104, w, 22);
    anchors:=[anLeft, anTop, anRight];
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Built with fpGUI v'+FPGUI_VERSION;;
  end;
  {@VFD_BODY_END: fpgAbout}

  { fix up buttons }

  btnCancel.visible:=false;
  btnOK.left:=(w-btnOK.width) shr 1;
  btnOK.height:=24;
  btnOK.top:=h-24-8;

  { additional meta handling }

  s := fpgApplication.AppVersion;
  if s<>'' then
    lTitle.text:=fpgApplication.AppTitle+' v'+s
  else
    lTitle.text := fpgApplication.AppTitle;
  url := fpgApplication.AppSiteURL;
  CopyrightID := fpgApplication.HelpContext;
end;



procedure TfpgAbout.FormSHow(sender: TObject);
begin
  if url='' then
    lSite.TextColor:=TfpgColor(lTitle.TextColor)
  else
    lSite.TextColor:=TfpgColor($0721FF);
  if CopyrightID=0 then
    lCopyright.TextColor:=TfpgColor(lTitle.TextColor)
  else
    lCopyright.TextColor:=TfpgColor($0721FF);
end;



procedure TfpgAbout.DoKeyShortcut(const AOrigin: TfpgWidget; const key: word;
  const shift: TShiftState; var consumed: boolean;
  const IsChildOfOrigin: boolean = False);
var
  ssht: TShiftState;
begin
  inherited DoKeyShortcut(aorigin, key, shift, consumed, IsChildOfOrigin);
  if consumed then exit;
  ssht:=shift-ssKeyOnly;
  if ssht=[] then
  begin
    if (key = keyEscape) or (key = keyEnter) or (key = keyPEnter) then
    begin
      consumed:=true;
      if key = keyEscape then
        ModalResult:=mrCancel
      else
        ModalResult:=mrOK;
     end;
  end;
end;



procedure TfpgAbout.lCopyrightClick(sender: TObject);
begin
  if (lCopyright.text='') or (CopyrightID=0) then exit;
  fpgApplication.ContextHelp(CopyrightID);
  ModalResult:=mrOK;
end;



procedure TfpgAbout.lSiteClick(sender: TObject);
begin
  if (lSite.text='') or (url='') then exit;
  fpgOpenURL(url);
  ModalResult:=mrOK;
end;



function TfpgAbout.TitleGet: string;
begin
  result:=lTitle.text;
end;



procedure TfpgAbout.TitleSet(const s: string);
begin
  lTitle.text:=s;
end;



function TfpgAbout.AuthorGet: string;
begin
  result:=lAuthor.text;
end;



procedure TfpgAbout.AuthorSet(const s: string);
begin
  lAuthor.text:=s;
end;



function TfpgAbout.CopyrightGet: String;
begin
  result:=lCopyright.text;
end;



procedure TfpgAbout.CopyrightSet(const s: string);
begin
  lCopyright.text:=s;
end;



function TfpgAbout.SiteGet: string;
begin
  result:=lSite.text;
end;



procedure TfpgAbout.SiteSet(const s: string);
begin
  lSite.text:=s;
end;



function AboutDlg: TfpgModalResult;
begin
  with TfpgAbout.create(nil) do try
    result := ShowModal;
  finally
    free;
  end;
end;



end.
