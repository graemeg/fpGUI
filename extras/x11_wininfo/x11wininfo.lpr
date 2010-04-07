program x11wininfo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, fpg_base, fpg_main, fpg_form, fpg_label, fpg_panel,
  fpg_button, fpg_listbox, fpg_impl, fpg_tab, fpg_edit, fpg_x11,
  xlib, x;

type
  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    Button1: TfpgButton;
    PageControl1: TfpgPageControl;
    TabSheet1: TfpgTabSheet;
    Edit1: TfpgEdit;
    Edit2: TfpgEdit;
    Label1: TfpgLabel;
    lblPos: TfpgLabel;
    Label3: TfpgLabel;
    lblSize: TfpgLabel;
    Label2: TfpgLabel;
    lblHandle: TfpgLabel;
    Bevel1: TfpgBevel;
    Label4: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    FTimer: TfpgTimer;
    FOutlineGC: TfpgGContext;
    FOutlineDrawn: Boolean;
    newrect: TfpgRect;
    lastRect: TfpgRect;
    last_child: TfpgWinHandle;
    procedure btnWinInfoClicked(Sender: TObject);
    procedure TimerFired(Sender: TObject);
    procedure InitOutline;
    procedure DrawOutline;
    procedure ClearOutine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterCreate; override;
  end;

{ TMainForm }

procedure TMainForm.btnWinInfoClicked(Sender: TObject);
begin
  FTimer.Enabled := not FTimer.Enabled;
  ClearOutine;
end;

procedure TMainForm.TimerFired(Sender: TObject);
type
   AChildren = array[0..0] of TfpgWinHandle;
   PChildren = ^AChildren;
var
  rootw: TfpgWinHandle;
  parentw: TfpgWinHandle;
  children: PChildren;
  cnum: longword;
  ret_root: TfpgWinHandle;
  ret_child: TfpgWinHandle;
  root_x, root_y: integer;
  child_x, child_y: integer;
  mask: longword;
  x: integer;
  winrect: TfpgRect;
  wa: TXWindowAttributes;
begin
  rootw := DefaultRootWindow(fpgApplication.Display);

  x := 1;
  XQueryPointer(fpgApplication.Display, rootw, @ret_root, @ret_child,
      @root_x, @root_y, @child_x, @child_y, @mask);

  while ret_child <> 0 do
  begin
    last_child := ret_child;
    if XQueryPointer(fpgApplication.Display, ret_child, @ret_root, @ret_child,
        @root_x, @root_y, @child_x, @child_y, @mask) then
    begin
//      writeln('X=', x);
      Inc(x);
//      writeln('Button.WinHandle: ', IntToHex(Button1.WinHandle, 6));
//      writeln('WinHandle under pointer: ', IntToHex(ret_child, 6));
    end;
  end;
  XGetWindowAttributes(fpgApplication.Display, last_child, @wa);
  winrect.SetRect(wa.x, wa.y, wa.width, wa.height);
//  writeln('----start----');
//  PrintRect(winrect);

  XTranslateCoordinates(fpgApplication.Display,
      last_child, DefaultRootWindow(fpgApplication.Display),
      wa.X, wa.Y, @root_x, @root_y, @ret_child);

  newrect.SetRect(root_x-wa.X, root_y-wa.Y, wa.width, wa.height);
//  PrintRect(newrect);
//  writeln('----done----');
  ClearOutine;
  DrawOutline;


  exit; //==>
  children := nil;
  if XQueryTree(
      fpgApplication.Display,
      DefaultRootWindow(fpgApplication.Display),
      @rootw, @parentw, @children, @cnum) <> 0 then
  begin

  end;
  if children <> nil then
    XFree(children);
end;

procedure TMainForm.InitOutline;
var
  gcValues: TXGCValues;
begin
   gcValues._function := GXxor; //GXinvert;
   gcValues.subwindow_mode := IncludeInferiors;
   gcValues.line_width := 2;
   FOutlineGC := XCreateGC(
      fpgApplication.Display,
      DefaultRootWindow(fpgApplication.Display),
      GCFunction or GCSubwindowMode or GCLineWidth, @gcValues);
   XSetForeGround(fpgApplication.display, FOutlineGC, fpgColorToX(clRed));
  FOutlineDrawn := False;
end;

procedure TMainForm.DrawOutline;
begin
  if not FOutlineDrawn then
  begin
    XSync(fpgApplication.Display, False);
    XDrawRectangle(
        fpgApplication.Display,
        DefaultRootWindow(fpgApplication.Display),
        FOutlineGC,
        newrect.Left, newrect.Top, newrect.Width, newrect.Height);
    lastrect := newrect;
    FOutlineDrawn := True;
    lblPos.Text := Format('(%d,%d)', [newrect.Left, newRect.Top]);
    lblSize.Text := Format('(%d,%d)', [newrect.Width, newRect.Height]);
    lblHandle.Text := IntToHex(last_child, 6);
  end;
end;

procedure TMainForm.ClearOutine;
begin
  if FOutlineDrawn then
  begin
    XDrawRectangle(
        fpgApplication.Display,
        DefaultRootWindow(fpgApplication.Display),
        FOutlineGC,
        lastrect.Left, lastrect.Top, lastrect.Width, lastrect.Height);
    FOutlineDrawn := False;
    XSync(fpgApplication.Display, False);
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TfpgTimer.Create(80);
  FTimer.OnTimer := @TimerFired;
  InitOutline;
end;

destructor TMainForm.Destroy;
begin
  ClearOutine;
  XFreeGC(fpgApplication.Display, FOutlineGC);
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(334, 226, 300, 309);
  WindowTitle := 'fpGUI Window Information';
  WindowPosition := wpUser;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(208, 8, 80, 24);
    Text := 'wininfo';
    AllowAllUp := False;
    Embedded := False;
    Flat := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    ImageLayout := ilImageLeft;
    ImageMargin := 3;
    ImageName := '';
    ImageSpacing := -1;
    ParentShowHint := True;
    ShowImage := True;
    TabOrder := 0;
    OnClick :=@btnWinInfoClicked;
  end;

  PageControl1 := TfpgPageControl.Create(self);
  with PageControl1 do
  begin
    Name := 'PageControl1';
    SetPosition(16, 172, 274, 116);
    ActivePageIndex := 0;
    ParentShowHint := True;
    TabOrder := 3;
  end;

  TabSheet1 := TfpgTabSheet.Create(PageControl1);
  with TabSheet1 do
  begin
    Name := 'TabSheet1';
    SetPosition(3, 24, 268, 89);
    Text := 'TabSheet1';
  end;

  Edit1 := TfpgEdit.Create(TabSheet1);
  with Edit1 do
  begin
    Name := 'Edit1';
    SetPosition(140, 12, 120, 22);
    TabOrder := 0;
    Text := '';
    FontDesc := '#Edit1';
    ParentShowHint := True;
  end;

  Edit2 := TfpgEdit.Create(TabSheet1);
  with Edit2 do
  begin
    Name := 'Edit2';
    SetPosition(24, 52, 22, 22);
    TabOrder := 1;
    Text := '';
    FontDesc := '#Edit1';
    ParentShowHint := True;
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(12, 44, 68, 16);
    Alignment := taLeftJustify;
    FontDesc := '#Label2';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := 'Left, Top:';
    WrapText := False;
  end;

  lblPos := TfpgLabel.Create(self);
  with lblPos do
  begin
    Name := 'lblPos';
    SetPosition(120, 44, 80, 16);
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := '-';
    WrapText := False;
  end;

  Label3 := TfpgLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(12, 68, 100, 16);
    Alignment := taLeftJustify;
    FontDesc := '#Label2';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := 'Width, Height:';
    WrapText := False;
  end;

  lblSize := TfpgLabel.Create(self);
  with lblSize do
  begin
    Name := 'lblSize';
    SetPosition(120, 68, 80, 16);
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := '-';
    WrapText := False;
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(12, 92, 80, 16);
    Alignment := taLeftJustify;
    FontDesc := '#Label2';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := 'Handle:';
    WrapText := False;
  end;

  lblHandle := TfpgLabel.Create(self);
  with lblHandle do
  begin
    Name := 'lblHandle';
    SetPosition(120, 92, 80, 16);
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := '-';
    WrapText := False;
  end;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(24, 132, 248, 28);
    BorderStyle := bsSingle;
    ParentShowHint := True;
    Style := bsRaised;
  end;

  Label4 := TfpgLabel.Create(self);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(42, 137, 223, 16);
    Alignment := taLeftJustify;
    FontDesc := '#Label2';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := '===== Below is a test area =====';
    WrapText := False;
  end;

  {@VFD_BODY_END: MainForm}
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

