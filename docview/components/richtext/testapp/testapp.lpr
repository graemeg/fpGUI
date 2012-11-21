{
  This is a quick and simple test app so I coud test the TRichTextView component
  as a standard-alone widget.
}
program testapp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, fpg_base, fpg_main, fpg_form, fpg_button, fpg_memo, fpg_dialogs,
  fpg_checkbox, fpg_imagelist, RichTextView;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    Button1: TfpgButton;
    RichTextView1: TRichTextView;
    Memo1: TfpgMemo;
    CheckBox1: TfpgCheckBox;
    btnLoad: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    FImageList: TfpgImageList;
    procedure   Button1Click(Sender: TObject);
    procedure   btnLoadClicked(Sender: TObject);
    procedure   LinkClicked(Sender: TRichTextView; Link: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}



{@VFD_NEWFORM_IMPL}

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    RichTextView1.AddParagraph(PChar(Memo1.Text))
  else
    RichTextView1.AddText(PChar(Memo1.Text));
end;

procedure TMainForm.btnLoadClicked(Sender: TObject);
var
  sl: TStringList;
  lFile: TfpgString;
begin
  lFile := SelectFileDialog(sfdOpen, 'Text Files (*.txt)|*.txt');
  if lFile = '' then
    Exit;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(lFile);
    RichTextView1.Clear;
    RichTextView1.AddText(PChar(sl.Text));
  finally
    sl.Free;
  end;
end;

procedure TMainForm.LinkClicked(Sender: TRichTextView; Link: string);
begin
  ShowMessage(Link);
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  img_ref: TfpgImage;
  img: TfpgImage;
begin
  inherited Create(AOwner);
  FImageList := TfpgImageList.Create;

  { make copies of standard fpGUI images, and add them to our imagelist }
  img_ref := fpgImages.GetImage('stdimg.folderhome');
  img := img_ref.ImageFromSource;
  FImageList.AddImage(img);

  img_ref := fpgImages.GetImage('stdimg.ok');
  img := img_ref.ImageFromSource;
  FImageList.AddImage(img);

  img_ref := fpgImages.GetImage('stdimg.dlg.warning');
  img := img_ref.ImageFromSource;
  FImageList.AddImage(img);

  img_ref := fpgImages.GetImage('stdimg.dlg.info');
  img := img_ref.ImageFromSource;
  FImageList.AddImage(img);

end;

destructor TMainForm.Destroy;
begin
  FImageList.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(397, 276, 507, 403);
  WindowTitle := 'RichTextView test application';
  Hint := '';

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(412, 12, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Open';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick  := @Button1Click;
  end;

  RichTextView1 := TRichTextView.Create(self);
  with RichTextView1 do
  begin
    Name := 'RichTextView1';
    SetPosition(20, 100, 476, 288);
    Anchors := [anLeft,anRight,anTop,anBottom];
    ScrollDistance := 80;
    Images := FImageList;
    RichTextSettings.Heading1Font := fpgGetFont('Arial-18:bold');
    RichTextSettings.Heading2Font := fpgGetFont('Arial-14:bold');
    RichTextSettings.Heading3Font := fpgGetFont('Arial-12:bold');
    RichTextSettings.NormalFont := fpgGetFont(FPG_DEFAULT_FONT_DESC);
    RichTextSettings.FixedFont := fpgGetFont('Courier New-10:antialiased=true');
    OnClickLink :=@LinkClicked;
  end;

  Memo1 := TfpgMemo.Create(self);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(20, 8, 384, 80);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Edit1';
    Hint := '';
    Lines.Add('<red>hello<black> <u>world</u>. <b>Graeme</b> says hi.');
    TabOrder := 2;
  end;

  CheckBox1 := TfpgCheckBox.Create(self);
  with CheckBox1 do
  begin
    Name := 'CheckBox1';
    SetPosition(408, 40, 92, 20);
    Anchors := [anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 3;
    Text := 'Paragraph';
  end;

  btnLoad := TfpgButton.Create(self);
  with btnLoad do
  begin
    Name := 'btnLoad';
    SetPosition(412, 64, 80, 23);
    Text := 'Load...';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
    OnClick := @btnLoadClicked;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


