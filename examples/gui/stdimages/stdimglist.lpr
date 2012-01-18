program stdimglist;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_imgfmt_bmp, fpg_button, u_report;

type

  TMainForm = class(TfpgForm)
  private
    FReport: T_Report;
    btnPrint: TfpgButton;
    btnClose: TfpgButton;
    procedure   btnPrintClick(Sender: TObject);
    procedure   btnCloseClick(Sender: TObject);
  protected
    procedure   HandlePaint; override;
  public
    procedure   AfterCreate; override;
  end;

{ TMainForm }

procedure TMainForm.AfterCreate;
begin
  SetPosition(100,100,700,500);
  WindowTitle := 'fpGUI Standard Image Listing';
  WindowPosition := wpOneThirdDown;
  MinWidth := 200;
  MinHeight := 100;

  btnPrint := CreateButton(Self, Width-90, Height-70, 75, 'Print', @btnPrintClick);
  btnPrint.ImageName := 'stdimg.print';
  btnPrint.Anchors := [anRight, anBottom];

  btnClose := CreateButton(self, Width-90, Height-35, 75, 'Quit', @btnCloseClick);
  btnClose.ImageName := 'stdimg.quit';
  btnClose.Anchors := [anRight, anBottom];

  FReport:= T_Report.Create;
end;

procedure TMainForm.btnPrintClick(Sender: TObject);
var
  FtTitle,FtText: integer;
  ColName1,ColImg1,ColName2,ColImg2: integer;
  n,i: integer;
  sl: TStringList;
  img: TfpgImage;
begin
  with FReport do
  begin
    BeginWrite(oPortrait,A4,msMM,'F',True);
    Section(10,10,10,10);
    FtTitle:= Font('helvetica-12:bold',clBlack);
    FtText:= Font('helvetica-8',clBlack);
    ColName1:= Column(20,45,0);
    ColImg1:= Column(65,45,0);
    ColName2:= Column(110,45,0);
    ColImg2:= Column(155,45,0);
    WriteHeader(cnCenter,lnEnd,'FpGUI standard images',ColDefaut,FtTitle);
    NumPageFooter(cnCenter,lnEnd,'Page','of',True,ColDefaut,FtText);

    sl  := TStringList.Create;
    fpgImages.ListImages(sl);
    i := 0;

    for n := 0 to sl.Count-1 do
    begin
      if n mod 2 = 0 then
      begin
        WritePage(cnLeft,(n div 2)*7+30+i,sl[n],ColName1,FtText);
        ImagePage(cnLeft,(n div 2)*7+13+i,sl[n],ColImg1);
        if TfpgImage(sl.Objects[n]).Height> 16 then
          inc(i,5);
      end
      else
      begin
        WritePage(cnLeft,(n div 2)*7+30+i,sl[n],ColName2,FtText);
        ImagePage(cnLeft,(n div 2)*7+13+i,sl[n],ColImg2);
        if (TfpgImage(sl.Objects[n]).Height> 16) and not (TfpgImage(sl.Objects[n-1]).Height> 16) then
          inc(i,5);
      end;
    end;

    EndWrite;
    sl.Free;
  end;
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  FReport.Free;
  Close;
end;

procedure TMainForm.HandlePaint;
var
  n: integer;
  x: TfpgCoord;
  y: TfpgCoord;
  sl: TStringList;
  img: TfpgImage;
begin
  Canvas.BeginDraw; // begin double buffering
  inherited HandlePaint;

  sl  := TStringList.Create;
  x   := 8;
  y   := 8;
  fpgImages.ListImages(sl);
  
  for n := 0 to sl.Count-1 do
  begin
    Canvas.DrawString(x, y, sl[n]+':');
    
    img := TfpgImage(sl.Objects[n]);
    if img <> nil then
      Canvas.DrawImage(x+130, y, img);

    inc(y, img.Height+8);
    if y > Height-32 then // largest images are 32 in height
    begin
      inc(x, 200);
      y := 8;
    end;
  end;

  Canvas.EndDraw;
  sl.Free;
end;

procedure MainProc;
var
  frm : TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.
