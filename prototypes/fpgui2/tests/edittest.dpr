program edittest;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_imgfmt_bmp,
  gui_form,
  gui_label,
  gui_button,
  gui_edit,
  gui_combobox,
  gui_scrollbar,
  uhelpers,
  gui_memo,
  gui_dialogs,
  gui_listbox;

type

  { TXPButton }

  TXPButton = class(TfpgButton)
  private
    State: integer;
      // 0-normal
      // 1-hover
      // 2-mouse down
      // 3-disabled
      // 4-got focus
    image: TfpgImage;
  protected
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(X, Y: integer; ShiftState: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseExit; override;
    procedure   HandleMouseEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    procedure btnCloseClick(Sender: TObject);
    procedure btnDisplayBMP(Sender: TObject);
    procedure btn3Click(Sender: TObject);
  public
    label1: TfpgLabel;
    label2: TfpgLabel;
    edit1: TfpgEdit;
    edit2: TfpgEdit;
    btn: TfpgButton;
    btn2: TfpgButton;
    btn3: TfpgButton;
    memo: TfpgMemo;
    listbox: TfpgListBox;
    combo1: TfpgComboBox;
    sbar: TfpgScrollBar;
    xp: TXPButton;
    procedure AfterCreate; override;
  end;

{ TXPButton }

procedure TXPButton.HandlePaint;
var
  x, i: integer;
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.ClearClipRect;
  Canvas.Clear(clGray);

  image := nil;
  image := LoadImage_BMP('button.bmp');
  image.CreateMaskFromSample(0, 0);
  image.UpdateImage;
  if not Assigned(image) then
     writeln('Image is nil');

  { left }
  Writeln(Left, ' ' , Top, ' ', State, ' ', image.Width);
  x := 0;
  Canvas.DrawImagePart(x, 0, image, state*32, 0, 3, 21);
  { body }
  for i := (x+3) to (x+3+69) do
    Canvas.DrawImagePart(i, 0, image, (state*32)+3, 0, 1, 21);
  { right }
  Canvas.DrawImagePart(i, 0, image, (state*32)+29, 0, 3, 21);


  image.Free;
//    Canvas.DrawString(16, 242, 'OK');

  Canvas.EndDraw;
end;

procedure TXPButton.HandleLMouseDown(X, Y: integer; ShiftState: TShiftState);
begin
  State := 2;
  Repaint;
  inherited HandleLMouseDown(X, Y, ShiftState);
end;

procedure TXPButton.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  State := 1;
  Repaint;
  inherited HandleLMouseUp(x, y, shiftstate);
end;

procedure TXPButton.HandleMouseExit;
begin
  writeln('exit');
  inherited HandleMouseExit;
  State := 0;
  Repaint;
end;

procedure TXPButton.HandleMouseEnter;
begin
  writeln('enter');
  inherited HandleMouseEnter;
  State := 1;
  Repaint;
end;

constructor TXPButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 75;
  Height := 21;
  State := 0;
  
end;

destructor TXPButton.Destroy;
begin
  image.Free;
  inherited Destroy;
end;


  { TMainForm }

  procedure TMainForm.btnCloseClick(Sender: TObject);
  begin
    Close;
  end;

  procedure TMainForm.btnDisplayBMP(Sender: TObject);
  var
    bmp: TfpgImage;
    i: integer;
  begin
    bmp := LoadImage_BMP('button.bmp');
    //    bmp := LoadImage_BMP('..\images\close.bmp');
    bmp.CreateMaskFromSample(0, 0);
    bmp.UpdateImage;

    Canvas.BeginDraw;
    Canvas.ClearClipRect;
    // For some reason, under Windows if I don't call this
    // the forms background goes black.
    Canvas.Clear(clWindowBackground);

    Canvas.DrawImage(10, 200, bmp);
    Canvas.DrawImagePart(10, 240, bmp, 0, 0, 32, 21);
    Canvas.DrawImagePart(50, 240, bmp, 32, 0, 32, 21);
    Canvas.DrawString(16, 242, 'OK');

    // Lets draw a normal XP Button 75x21
    {top left corner}
//    Canvas.DrawImagePart(10, 280, bmp, 32, 0, 3, 3);
    { left }
    Canvas.DrawImagePart(10, 280, bmp, 32, 0, 3, 21);
    { body }
    for i := 13 to 69 do
      Canvas.DrawImagePart(i, 280, bmp, 35, 0, 1, 21);
    { right }
    Canvas.DrawImagePart(i, 280, bmp, 32+29, 0, 3, 21);
//    Canvas.DrawString(16, 242, 'OK');

    
    Canvas.EndDraw;

    bmp.Free;
    
  end;

  procedure TMainForm.btn3Click(Sender: TObject);
  begin
    ShowMessage('Do you really want to quit this application?' + #10 +
        'We can always keep playing and quite at a later date.' +
        #10#10 +
        'This is a very long line that has to must be split automatically ' +
        'and it should have done so. If not there is a bug in the code. It ' +
        'has also been optimized to wordwrap and not split words in half.'
        , 'My cool message title');
  end;


  procedure TMainForm.AfterCreate;
  var
    i: integer;
  begin
    SetPosition(200, 200, 500, 350);
    WindowTitle := 'fpGUI Widget Test';

    label1 := CreateLabel(self, 5, 5, 'Hello world!');
    label2 := CreateLabel(self, 5, 20, 'Hello world in Bold!');
    label2.FontDesc := 'Sans-12:bold:underline';
    label2.Width := 200;

    edit1      := CreateEdit(self, 10, 40, 120, 22);
    edit1.Text := 'Hello world. Hello world. Hello world.';
    edit2      := CreateEdit(self, 10, 70, 200, 22);
    edit2.Text := 'Test Russian text -> Òåñò';

    btn2          := CreateButton(self, 10, 100, 75, 'Normal', nil);
    btn2.OnClick  := @btnDisplayBMP;
    btn3          := CreateButton(self, 100, 100, 75, 'Embedded', nil);
    btn3.Embedded := True;
    btn3.OnClick  := @btn3Click;


    btn           := CreateButton(self, 10, 130, 75, 'Close', @btnCloseClick);
    btn.ImageName := 'stdimg.close';
    btn.ShowImage := True;

    combo1 := CreateComboBox(self, 10, 160, 120, nil);
    //    combo1.Height := 25;//22;

    memo        := TfpgMemo.Create(self);
    memo.Top    := 10;
    memo.Left   := 250;
    memo.Width  := 200;
    memo.Height := 80;

    listbox         := TfpgListBox.Create(self);
    listbox.Top     := 100;
    listbox.Left    := 250;
    listbox.Width   := 200;
    listbox.Height  := 80;
    for i := 1 to 20 do
      listbox.Items.Add(Format('Items %.2d', [i]));
    listbox.FocusItem := 3;


    sbar        := TfpgScrollBar.Create(self);
    sbar.Top    := 160;
    sbar.Left   := 150;
    sbar.Height := 100;
    sbar.Max    := 15;
    
    xp := TXPButton.Create(self);
    xp.Left := 250;
    xp.Top := 200;
    xp.Width := 75;

  end;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
end;


begin
  MainProc;
end.

