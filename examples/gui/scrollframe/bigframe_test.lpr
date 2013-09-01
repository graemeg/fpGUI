program bigframe_test;

{$mode objfpc}{$H+}

uses
  Classes,
  sysutils,
  fpg_base,
  fpg_main,
  fpg_button,
  fpg_label,
  fpg_form,
  fpg_panel,
  fpg_scrollframe
  ;

procedure create_buttons (f : TfpgFrame);
var
  i, j, ij : integer;
  b : TfpgButton;
const
  num_button_cols = 4;
  num_button_rows = 5;
begin
  with f do begin
    for i := 0 to num_button_cols-1 do
      begin
        for j := 0 to num_button_rows-1 do
          begin
            if (j>4) and (j<16) then continue;
            ij := j + num_button_rows*i;
            b := TfpgButton.Create(f);
            with b do begin
              if (i=2) and (j=2)
                then SetPosition(6000, 6000, 100, 25)
                else SetPosition(20+i*105, 50+j*30, 100, 25);
              name := 'button' + inttostr(ij);
              Text := 'Button ' + inttostr(ij+1);
              FontDesc := '#Label1';
            end;
          end;
      end;
  end;
end;

type
  
  { t_sample_frame }

  t_sample_frame = class (TfpgAutoSizingFrame)
  protected
    my_color : TfpgColor;
    embed_button : TfpgButton;
    procedure click_embed_button (Sender: TObject);
    procedure paint_my_stuff (Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

procedure t_sample_frame.click_embed_button(Sender: TObject);
var
  inner_bevel : TfpgBevel;
  inner_frame : TfpgScrollFrame;
begin
  embed_button.Visible:=false;
  inner_bevel := TfpgBevel.Create(self);
  with inner_bevel do begin;
    SetPosition(90, 210, 300, 300);
    BorderStyle := bsDouble;
    Shape := bsFrame;
    UpdateWindowPosition;
  end;
  RecalcFrameSize;
  
  inner_frame := TfpgScrollFrame.Create(inner_bevel, t_sample_frame);
  inner_frame.Align:=alClient;
end;

procedure t_sample_frame.paint_my_stuff (Sender: TObject);
begin
  canvas.Color := my_color;
  canvas.FillRectangle (30, 30, 200, 400);
end;

procedure t_sample_frame.AfterCreate;
begin
  inherited AfterCreate;
  MarginBR:=7;
  my_color:=TfpgColor(random(high(longint)));
  embed_button := CreateButton (self, 20, 240, 270,
      'Click to embed another Scroll-Frame here', @click_embed_button);
  OnPaint:=@paint_my_stuff;
  create_buttons(self);
end;


var
  form: TfpgForm;
  outer_frame: TfpgScrollFrame;

begin
  fpgApplication.Initialize;
  form := TfpgForm.Create(nil);
  form.SetPosition(0,0,380,360);
  outer_frame := TfpgScrollFrame.Create(form, t_sample_frame);
  outer_frame.Align:=alClient;
  try
    form.Show;
    fpgApplication.Run;
  finally
    form.Free;
  end;
end.

