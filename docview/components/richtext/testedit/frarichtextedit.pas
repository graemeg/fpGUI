unit frarichtextedit;

{$mode objfpc}{$H+}

interface

uses
  fpg_base,
  fpg_tab,
  fpg_button,
  fpg_panel,
  fpg_main,
  fpg_memo,
  fpg_form,
  fpg_dialogs,
  fpg_stdimages,
  RichTextView,
  fpg_imagelist,
  fpg_imgfmt_bmp,
  fpg_imgfmt_png,
  fpg_imgfmt_jpg,
  Classes, SysUtils;

Type

  { TRichTextEditFrame }

  TRichTextEditFrame = class(TfpgFrame)
  private
    FImageNames : TStringList;
    FImageList : tfpgImageList;
    procedure CheckTags;
    procedure EncloseSelection(Const StartTag, EndTag: String);
    function GetImageNames: TStrings;
    function GetRichText: String;
    procedure InsertColor(Const Background : Boolean);
    procedure InsertFont;
    procedure InsertImage;
    procedure InsertLink;
    procedure InsertMargin;
    procedure DoPageChange(Sender: TObject; NewActiveSheet: TfpgTabSheet);
    procedure OnToolButton(Sender: TObject);
    procedure SetImageNames(AValue: TStrings);
    procedure SetRichText(AValue: String);
  protected
    {@VFD_HEAD_BEGIN: MainForm}
    PCedit: TfpgPageControl;
    TSedit: TfpgTabSheet;
    BBar: TfpgBevel;
    BBold: TfpgButton;
    BItalic: TfpgButton;
    Bunderline: TfpgButton;
    BHead: TfpgBevel;
    BH1: TfpgButton;
    BH2: TfpgButton;
    BH3: TfpgButton;
    BAligns: TfpgBevel;
    BAleft: TfpgButton;
    BACenter: TfpgButton;
    BAJustified: TfpgButton;
    BAright: TfpgButton;
    BAUnaligned: TfpgButton;
    BANowrap: TfpgButton;
    BevMargin: TfpgBevel;
    BMargin: TfpgButton;
    BFont: TfpgButton;
    BColor: TfpgButton;
    BBGColor: TfpgButton;
    BSpaceImage: TfpgBevel;
    BImage: TfpgButton;
    BLink: TfpgButton;
    BCheck: TfpgButton;
    MText: TfpgMemo;
    TSPreview: TfpgTabSheet;
    RTVPreview: TRichTextView;
    {@VFD_HEAD_END: MainForm}
    procedure AfterCreate; override;
  Public
    Property ImageNames : TStrings Read GetImageNames Write SetImageNames;
    Property RichText : String Read GetRichText Write SetRichText;
  end;

Procedure RegisterRichTextImages(ADir : String);
Procedure RegisterStdRichTextImages;

implementation

procedure TRichTextEditFrame.DoPageChange(Sender: TObject; NewActiveSheet: TfpgTabSheet);

Var
  F : String;

begin
  If NewActiveSheet=TSPreview then
    begin
    RTVPreview.Clear;
    F:=MText.Lines.Text;
    RTVPreview.AddText(Pchar(F));
    end;
end;

Const
  TBBold         = 1;
  TBItalic       = 2;
  TBUnderLine    = 3;
  TBH1           = 4;
  TBH2           = 5;
  TBH3           = 6;
  TBAlignLeft    = 7;
  TBAlignCenter  = 8;
  TBAlignRight   = 9;
  TBAlignNone    = 10;
  TBAlignJustify = 11;
  TBMargin       = 12;
  TBFont         = 13;
  TBColor        = 14;
  TBBGColor      = 15;
  TBImage        = 16;
  TBLink         = 17;
  TBNowrap       = 18;
  TBCheck        = 19;

  BSize          = 30;

  // Image names
  BIBold         = 'richtextedit.bold';
  BIItalic       = 'richtextedit.italic';
  BIunderline    = 'richtextedit.underline';
  BIAlignLeft    = 'richtextedit.left';
  BIAlignCenter  = 'richtextedit.center';
  BIAlignRight   = 'richtextedit.right';
//  BIAlignNone    = 10;
  BIAlignJustify = 'richtextedit.justify';
  BIMargin       = 'richtextedit.margin';
  BIFont         = 'richtextedit.font';
  BIColor        = 'richtextedit.color';
  BIBGColor      = 'richtextedit.backgroundcolor';
  BIImage        = 'richtextedit.image';
  BILink         = 'richtextedit.link';
  BINowrap       = 'richtextedit.nowrap';
  BICheck        = 'richtextedit.check';

  BFNBold         = 'bold';
  BFNItalic       = 'italic';
  BFNunderline    = 'underlined';
  BFNAlignLeft    = 'left';
  BFNAlignCenter  = 'center';
  BFNAlignRight   = 'right';
//  BFNAlignNone    = 10;
  BFNAlignJustify = 'justify';
  BFNMargin       = 'margin';
  BFNFont         = 'font';
  BFNColor        = 'color';
  BFNBGColor      = 'color_background';
  BFNImage        = 'image';
  BFNLink         = 'hyperlink';
  BFNNoWrap       = 'nowrap';
  BFNCheck        = 'check';

procedure CheckSelection(Const S, TextName : String);

Var
  I,P,L,TS,TC : Integer;
  T : TStrings;
  TT,TN : String;

begin
  T:=TStringList.Create;
  try
    I:=0;
    P:=0;
    L:=Length(S);
    While (P=0) and (I<L) do
      begin
      Inc(I);
      If (S[i]='<') then
        if (I=L) then
          P:=I
        else
          begin
          Inc(I);
          if (S[i]<>'<') then
             begin
             TS:=I;
             While (P=0) and (S[i]<>'>') do
               if (I=L) then
                 P:=I
               else
                 Inc(I);
             if (P=0) then
               begin
               TN:=LowerCase(Copy(S,TS,I-TS));
               TC:=Pos(' ',TN);
               if TC<>0 then
                 TN:=Copy(TN,1,TC-1);
               if (TN<>'') then
                 begin
                 if (TN[1]<>'/') then
                   begin
                   if Pos('/'+TN+'/','/align/rightmargin/leftmargin/image/')=0 then
                     T.Add(TN)
                   end
                 else
                   begin
                   Delete(TN,1,1);
                   TC:=T.Count-1;
                   if (TC<0) then
                     P:=TS
                   else
                     if (T[TC]<>TN) then
                       P:=TS
                     else
                       T.Delete(TC);
                    end;
                 end;
               end;
             end;
          end;
      end;
    if (P<>0) then
      Raise Exception.CreateFmt('The %s contains a not-opened closing tag at position %d: %s',[TextName,P,TN])
    else if (T.Count>0) then
      Raise Exception.CreateFmt('The %s contains a not-closed tag: %s',[TextName,T[T.Count-1]])
  finally
    T.Free;
  end;
end;

procedure TRichTextEditFrame.EncloseSelection(Const StartTag, EndTag : String);


Var
  S: String;

begin
  S:=MText.SelectionText;
  if (EndTag<>'') then
    CheckSelection(S,'selection');
  If (StartTag<>'') then
    S:='<'+StartTag+'>'+S;
  if (EndTag<>'') then
    S:=S+'</'+EndTag+'>';
  MText.SelectionText:=S;
//  MText.SelectionText:=S;
end;

function TRichTextEditFrame.GetImageNames: TStrings;
begin
  Result:=FImageNames;
end;

function TRichTextEditFrame.GetRichText: String;
begin
  Result:=MText.Lines.Text;
end;

procedure TRichTextEditFrame.InsertMargin;

Var
  S: tfpgstring;

begin
  S:='5';
  if fpgInputQuery('Insert margin','Enter the margin to be used:',S) then
    begin
    if StrToIntDef(S,-1)=-1 then
      ShowMessage('Not a numerical value. The margin must be a number','Error')
    else
      EncloseSelection('leftmargin '+s,'');
    end;
end;

procedure TRichTextEditFrame.InsertLink;

Var
  S: tfpgstring;

begin
  S:='';
  if fpgInputQuery('Insert link','Enter the link target text:',S) then
    EncloseSelection('link "'+s+'"','link');
end;

procedure TRichTextEditFrame.CheckTags;

begin
  CheckSelection(MText.Lines.Text,'text');
  ShowMessage('All tags are correctly balanced.','Check ok');
end;


procedure TRichTextEditFrame.InsertFont;

Var
  S : String;

begin
  if SelectFontDialog(S) then
    EncloseSelection('font "'+S+'"','font')
end;

procedure TRichTextEditFrame.InsertColor(Const Background : Boolean);

Var
  t : TRGBTriple;
  s : string;
begin
  With TfpgColorSelectDialog.Create(nil) do
    try
      SelectedColor:=clBlack;
      if ShowModal = mrOK then
        begin
        t:=fpgColorToRGBTriple(SelectedColor);
        S:='color';
        if Background then
          S:='back'+s;
        EncloseSelection(s+' #'+format('%.2x%.2x%.2x',[t.red,t.green,t.blue]),s);
        end;
    finally
      Free;
    end;
end;

procedure TRichTextEditFrame.InsertImage;

Var
  FN,E : String;
  I : integer;

begin
  FN:=SelectFileDialog(sfdOpen,'Supported image files|*.png;*.jpg;*.bmp','');
  if (FN<>'') then
    begin
    E:=LowerCase(ExtractFIleExt(FN));
    i:=FImageList.Count;
    if e='.png' then
      FImageList.AddImage(loadimage_png(FN),i)
    else if e='.bmp' then
      FImageList.AddImage(loadimage_bmp(FN),i)
    else if e='.jpg' then
      FImageList.AddImage(loadimage_jpg(FN),i);
    EncloseSelection('image '+intToStr(i),'');
    FImageNames.Add(IntTostr(i)+'='+FN);
    end;
end;

procedure TRichTextEditFrame.OnToolButton(Sender: TObject);

Var
  T : Ptruint;
begin
  T:=(Sender as TComponent).Tag;
  Case T of
    TBBold :
      EncloseSelection('b','b');
    TBItalic :
      EncloseSelection('i','i');
    TBUnderLine:
      EncloseSelection('u','u');
    TBH1:
      EncloseSelection('h1','h1');
    TBH2:
      EncloseSelection('h2','h2');
    TBH3:
      EncloseSelection('h3','h3');
    TBAlignLeft:
      EncloseSelection('align left','');
    TBAlignCenter:
      EncloseSelection('align center','');
    TBAlignRight:
      EncloseSelection('align right','');
    TBAlignNone:
      EncloseSelection('unalign','');
    TBNoWrap:
      EncloseSelection('nowrap','nowrap');
    TBAlignJustify:
      ; // Not functional yet
    TBMargin:
      InsertMargin;
    TBFont:
      InsertFont;
    TBColor,
    TBBGColor:
      InsertColor(TBBGColor=T);
    tbLink:
      InsertLink;
    TBCheck:
      CheckTags;
    TBImage:
      InsertImage;
  end;
end;

procedure TRichTextEditFrame.SetImageNames(AValue: TStrings);
begin
  if FImageNames=AValue then Exit;
  FImageNames.Assign(AValue);
end;

procedure TRichTextEditFrame.SetRichText(AValue: String);
begin
  MText.Lines.Text:=AValue;
end;

procedure TRichTextEditFrame.AfterCreate;

  Function TBSpace(Var ALeft : Integer; AName : String) : TfpgBevel;

  begin
    Result := TfpgBevel.Create(BBar);
    with Result do
      begin
      Name := AName;
      SetPosition(ALeft, 2, 10, 24);
      ALeft:=ALeft+10;
      Align := alLeft;
      Hint := '';
      Shape := bsSpacer;
      end;

  end;

  Function TBButton(Var ALeft,ATab : Integer; ATag : Integer; Const AName,AImage,AText : String) : tfpgButton;

  begin
    Result := TfpgButton.Create(BBar);
    with Result do
      begin
      Name := AName;
      SetPosition(ALeft, 2, BSize, BSize);
      ALeft:=ALeft+BSize;
      Align := alLeft;
      Text := AText;
      ImageName:=AImage;
      Tag:=ATag;
      FontDesc := '#Label1';
      Hint := '';
      ImageMargin := -1;
      Embedded:=True;
      Flat:=True;
      TabOrder := ATab;
      ATab:=ATab+1;
      OnClick:=@OnToolButton;
      end;
  end;

var
  I, J, L, T: integer;
  img: tfpgimage;
  S: string;


begin
  {%region 'Auto-generated GUI code' }

  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(496, 295, 739, 502);
  WindowTitle := 'MainForm';
  Hint := '';

  PCedit := TfpgPageControl.Create(self);
  with PCedit do
  begin
    Name := 'PCedit';
    SetPosition(0, 0, 739, 502);
    Align := alClient;
    Hint := '';
    TabOrder := 1;
    OnChange:=@DoPageChange;
  end;

  TSedit := TfpgTabSheet.Create(PCedit);
  with TSedit do
  begin
    Name := 'TSedit';
    SetPosition(3, 24, 733, 475);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Edit text';
  end;

  BBar := TfpgBevel.Create(TSedit);
  with BBar do
  begin
    Name := 'BBar';
    SetPosition(0, 0, 733, BSize+4);
    Align := alTop;
    Hint := '';
    Shape := bsBottomLine;
    Shape := bsBottomLine;
  end;


  MText := TfpgMemo.Create(TSedit);
  with MText do
  begin
    Name := 'MText';
    SetPosition(0, 28, 733, 447);
    Align := alClient;
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
  end;

  TSPreview := TfpgTabSheet.Create(PCedit);
  with TSPreview do
  begin
    Name := 'TSPreview';
    SetPosition(3, 24, 733, 475);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Preview';
  end;

  RTVPreview := TRichTextView.Create(TSPreview);
  with RTVPreview do
  begin
    Name := 'RTVPreview';
    SetPosition(0, 0, 735, 478);
    Anchors := [anLeft,anRight,anTop,anBottom];
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
  // Create toolbar
  L:=2;
  T:=0;
  BBold:=TBButton(L,T,TBBold,'BBold',BIBold,'');
  BItalic:=TBButton(L,T,TBItalic,'BItalic',BIItalic,'');
  Bunderline:=TBButton(L,T,TBunderline,'BUnderline',BIUnderline,'');

  BHead := TBSpace(L,'BHead');

  BH1 := TBButton(L,T,TBH1,'BH1','','1');
  BH2 := TBButton(L,T,TBH2,'BH2','','2');
  BH3 := TBButton(L,T,TBH3,'BH3','','3');

  BAligns := TBSpace(L,'BAligns');

  BAleft := TBButton(L,T,TBAlignLeft,'BALeft',BIAlignLeft,'');
  BACenter := TBButton(L,T,TBAlignCenter,'BACenter',BIAlignCenter,'');
  BAJustified := TBButton(L,T,TBAlignJustify,'BAJustified',BIAlignJustify,'');
  BARight := TBButton(L,T,TBAlignRight,'BARight',BIAlignRight,'');
  BAUnaligned := TBButton(L,T,TBAlignNone,'BAUnalign',BIAlignLeft,'');

  BANowrap := TBButton(L,T,TBNowrap,'BNowrap',BINoWrap,'');

  BevMargin := TBSpace(L,'BevMargin');
  BMargin := TBButton(L,T,TBMargin,'BMargin',BIMargin,'');

  BFont := TBButton(L,T,TBFont,'BFont',BIFont,'');
  BColor := TBButton(L,T,TBColor,'BColor',BIColor,'');
  BBGColor := TBButton(L,T,TBBGColor,'BBGColor',BIBGColor,'');

  BSpaceImage := TBSpace(L,'BSpaceImage');

  BImage := TBButton(L,T,TBImage,'BImage',BIImage,'');
  BLink := TBButton(L,T,TBLink,'BLink',BILink,'');
  BCheck := TBButton(L,T,TBCheck,'BCheck',BICheck,'');

  FImageList:=TfpgImageList.Create;
  FImageNames:=TStringList.Create;
  RTVPreview.Images:=FImageList;
  RTVPreview.RichTextSettings.Heading1Font := fpgGetFont('Arial-18:bold');
  RTVPreview.RichTextSettings.Heading2Font := fpgGetFont('Arial-14:bold');
  RTVPreview.RichTextSettings.Heading3Font := fpgGetFont('Arial-12:bold');
  RTVPreview.RichTextSettings.NormalFont := fpgGetFont(FPG_DEFAULT_FONT_DESC);
  RTVPreview.RichTextSettings.FixedFont := fpgGetFont('Courier New-10:antialiased=true');

end;

Procedure RegisterStdRichTextImages;

  Procedure LoadImage(const iname : string; ImageLoc : Pointer; ImageSize : Integer);

  begin
    if (fpgImages.GetImage(iname)<>Nil) then
      fpgImages.DeleteImage(iname,true);
    fpgImages.AddMaskedBMP(iname,ImageLoc,ImageSize,0,0)
  end;


{$i img_richedit.inc}

begin
  LoadImage(BIBold,@img_richedit_Bold,sizeof(img_richedit_Bold));
  LoadImage(BIItalic,@img_richedit_italic,sizeof(img_richedit_italic));
  LoadImage(BIunderline,@img_richedit_underlined,sizeof(img_richedit_underlined));
  LoadImage(BIAlignLeft,@img_richedit_left,sizeof(img_richedit_left));
  LoadImage(BIAlignCenter,@img_richedit_center,sizeof(img_richedit_center));
  LoadImage(BIAlignRight,@img_richedit_right,sizeof(img_richedit_right));
//  LoadImage(BIAlignCenter,@img_richedit_center,sizeof(img_richedit_center));
  LoadImage(BIAlignJustify,@img_richedit_justify,sizeof(img_richedit_justify));
  LoadImage(BIMargin,@img_richedit_margin,sizeof(img_richedit_margin));
  LoadImage(BIFont,@img_richedit_font,sizeof(img_richedit_font));
  LoadImage(BIColor,@img_richedit_color,sizeof(img_richedit_color));
  LoadImage(BIBGColor,@img_richedit_color_background,sizeof(img_richedit_color_background));
  LoadImage(BIImage,@img_richedit_image,sizeof(img_richedit_image));
  LoadImage(BINowrap,@img_richedit_nowrap,sizeof(img_richedit_nowrap));
  LoadImage(BILink,@img_richedit_hyperlink,sizeof(img_richedit_hyperlink));
  LoadImage(BICheck,@img_richedit_check,sizeof(img_richedit_check));
end;

Procedure RegisterRichTextImages(ADir : String);

  Procedure LoadImage(const iname,ifile : string);

  Var
    fn : string;
    img : TfpgImage;

  begin
    fn:=ADir+ifile+'.bmp';
    if FileExists(fn) then
      begin
      if (fpgImages.GetImage(iname)<>Nil) then
        fpgImages.DeleteImage(iname,true);
      img:=LoadImage_BMP(fn);
      img.CreateMaskFromSample(0,0);
      img.UpdateImage;
      fpgImages.AddImage(iname,img);
      end
    else
       ShowMessage(iname+' : file does not exist : '+ifile);
  end;

begin
  ADir:=IncludeTrailingPathDelimiter(ADir);
  LoadImage(BIBold,BFNBold);
  LoadImage(BIItalic,BFNItalic);
  LoadImage(BIunderline,BFNUnderLine);
//    BH1.ImageName:=BIH1;
//    BH2.ImageName:=BIH2;
//    BH3.ImageName:=BIH3;
  LoadImage(BIAlignLeft,BFNAlignLeft);
  LoadImage(BIAlignCenter,BFNAlignCenter);
  LoadImage(BIAlignRight,BFNAlignRight);
  LoadImage(BIAlignCenter,BFNAlignCenter);
  LoadImage(BIAlignJustify,BFNAlignJustify);
  // BIAlignJustify = 11;
  LoadImage(BIMargin,BFNMargin);
  LoadImage(BIFont,BFNFont);
  LoadImage(BIColor,BFNColor);
  LoadImage(BIBGColor,BFNBGColor);
  LoadImage(BIImage,BFNImage);
  LoadImage(BINowrap,BFNNowrap);
  LoadImage(BILink,BFNLink);
  LoadImage(BICheck,BFNCheck);

end;

end.

