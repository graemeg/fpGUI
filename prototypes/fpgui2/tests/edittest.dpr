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
  gui_dialogs;

type

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
    combo1: TfpgComboBox;
    sbar: TfpgScrollBar;
    procedure AfterCreate; override;
  end;


  { TMainForm }

  procedure TMainForm.btnCloseClick(Sender: TObject);
  begin
    Close;
  end;

  procedure TMainForm.btnDisplayBMP(Sender: TObject);
  var
    bmp: TfpgImage;
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

    Canvas.EndDraw;

    bmp.Free;
  end;

  procedure TMainForm.btn3Click(Sender: TObject);
  begin
    ShowMessage('Do you really want to quit this application?' + #10 +
        'We can always keep playing and quite at a later date.' +
        #10#10 +
        'This is a very long line that has to must be split automatically ' +
        'and it should have done so. If not there is a bug in the code. We ' +
        'must still optimize where it cuts the lines.'
        , 'My cool message title');
  end;


  procedure TMainForm.AfterCreate;
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

    sbar        := TfpgScrollBar.Create(self);
    sbar.Top    := 160;
    sbar.Left   := 150;
    sbar.Height := 100;
    sbar.Max    := 15;
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

