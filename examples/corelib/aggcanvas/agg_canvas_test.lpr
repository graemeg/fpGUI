{
  fpGUI  -  Free Pascal GUI Toolkit

  Copyright (C) 2006 - 2013 See the file AUTHORS.txt, included in this
  distribution, for details of the copyright.

  See the file COPYING.modifiedLGPL, included in this distribution,
  for details about redistributing fpGUI.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Description:
    This demo was simply to test the Agg2D unit and the TAgg2D canvas
    class. In this demo the AggPas TAgg2D canvas does all the painting.

    *** IMPORTANT ***
      For this demo to work, the fpGUI Toolkit MUST be compiled with
      the AggCanvas compiler define enabled.
}

program agg_canvas_test;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_imgfmt_bmp, 
  fpg_widget,
  Agg2D,
  fpg_dialogs;


type
  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    {@VFD_HEAD_END: MainForm}
    procedure   FormKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   FormPaint(Sender: TObject);
    procedure   CustomPaintJob;
  public
    procedure   AfterCreate; override;
  end;



{ TMainForm }

procedure TMainForm.FormKeyPressed(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if KeyCode = keyF1 then
  begin
    ShowMessage('This is a simple Canvas primitives painting test.');
  end;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  CustomPaintJob;
end;

// We can now call all the paint methods from HandlePaint or
// the OnPaint event.fpgcanvas
procedure TMainForm.CustomPaintJob;
var
  r: TfpgRect;
  fnt: TfpgFont;
  y: integer;
  ac: TAgg2D;
  c1, c2: TAggColor;
  d: double;
  i: integer;

  procedure DrawMacLionButton(const x, y: double; const astate: byte; const atext: ansistring);
  { AState values are interpreted as follows:
      0:  normal state
      1:  hover state
      2:  clicked state }
  var
    x1, y1, x2, y2, h, w: double;
    c1, c2: TAggColor;
  begin
    h := 21;
    w := 100;
    x1 := x;
    y1 := y;
    x2 := x1 + w;
    y2 := y1 + h;

    ac.NoLine;
  
    // Top vertical gradient
    case astate of
      0:  begin
            c1.Construct(255, 255, 255, 255);
            c2.Construct(243, 243, 243, 255);
          end;
      1:  begin
            c1.Construct(204, 229, 252, 255);
            c2.Construct(161, 209, 249, 255);
          end;
      2:  begin
            c1.Construct(144, 195, 241, 255);
            c2.Construct(113, 180, 239, 255);
          end;
    end;
    ac.FillLinearGradient(x1, y1, x1, y1+(h/2), c1, c2);
    ac.RoundedRect(x1+1, y1+1, x2+0.5, y2-(h/2)+0.5, 0.3, 3/2, 0.3, 3/2);
  
    // Bottom vertical gradient
    case astate of
      0:  begin
            c1.Construct(236, 236, 236, 255);
            c2.Construct(242, 242, 242, 255);
          end;
      1:  begin
            c1.Construct(143, 202, 251, 255);
            c2.Construct(207, 245, 253, 255);
          end;
      2:  begin
            c1.Construct(97, 173, 240, 255);
            c2.Construct(147, 206, 241, 255);
          end;
    end;
    ac.FillLinearGradient(x1, y1+(h/2)+1, x1, y2, c1, c2);
    ac.RoundedRect(x1+1, y1+(h/2)+0.5, x2+0.5, y2+0.5, 0.3, 3/2, 0.3, 3/2);
  
    // outer rounded rectangle
    ac.NoFill;
    case astate of 
      0:  begin
            ac.LineColor($9a, $9a, $9a);
          end;
      1,
      2:  begin
            ac.LineColor(86, 87, 143);
          end;
    end;
    ac.LineWidth(1.0);
    ac.RoundedRect(x1+0.5, y1+0.5, x2+0.5, y2+0.5, 3, 3);
    
    // Slight shadow under the button
    ac.LineColor($df, $df, $df, $ff);
    ac.Line(x1+0.5, y2+1.5, x2-0.5, y2+1.5);
  
    // button text
    c1.Construct(0,0,0);
    ac.FillColor(c1);
    ac.NoLine;
    ac.Font('arialbd.ttf', 13);
    ac.TextAlignment(AGG_AlignCenter ,AGG_AlignCenter );
    ac.Text(
      (x1 + x2 ) / 2.0 ,
      (y1 + y2 ) / 2.0 ,
      atext,
      true ,0.0 ,0.0 );
  
  end;
begin
  // casting so we have full access to the Agg2D canvas functions
  ac := TAgg2D(Canvas);

  ac.LineWidth(1.0);
//  ac.ClearAll($ed, $ed, $ed); // mac os x window background color
  ac.ClearAll(255, 255, 255);

  // Testing Rectangles
  Canvas.SetColor(clBlack);
  r.SetRect(0, 0, 1, 1);     // 1x1  (this is really a dot)
  Canvas.DrawRectangle(r);
  Canvas.SetColor(clRed);
  r.SetRect(0, 1, 1, 5);    // 1x5  (this is really a vertical line)
  Canvas.DrawRectangle(r);
  Canvas.SetColor(clMagenta);
  r.SetRect(1, 0, 5, 1);    // 5x1  (this is really a horizontal line)
  Canvas.DrawRectangle(r);

  Canvas.SetColor(clBlack);
  r.Top       := 5;
  r.Left      := 60;
  r.Width     := 50;
  r.Height    := 50;
  Canvas.DrawRectangle(r);

  r.Left      := 120;
  Canvas.SetLineStyle(2, lsDash);
  Canvas.DrawRectangle(r);

  r.Left      := 180;
  Canvas.SetColor(clGreen);
  Canvas.SetLineStyle(1, lsDot);
  Canvas.DrawRectangle(r);

  r.Left      := 240;
  Canvas.SetColor(clBlue);
  Canvas.SetLineStyle(1, lsSolid);
  Canvas.FillRectangle(r);

  // Testing line drawing
  ac.NoFill;
  Canvas.SetColor(clBlue);
  Canvas.DrawLine(5, 5, 54, 54);
  Canvas.DrawLine(54, 5, 5, 54);
  Canvas.SetColor(clRed);
  { Diagonal line }
  r.SetRect(60, 5, 50, 50);
  Canvas.DrawLine(r.Left, r.Top, r.Right, r.Bottom);
  { Horizontal line }
  Canvas.DrawLine(r.Left, r.Top-2, r.Right, r.Top-2);
  { Vertical line }
  Canvas.DrawLine(r.Left-2, r.Top, r.Left-2, r.Bottom);


  // Testing Text and Fonts
  y := 60;
  Canvas.SetTextColor(clBlack);
  Canvas.DrawString(5, y, 'This text must be black and default font');

  // red dot indicates top/left corner of where previous text was started
  Canvas.Pixels[5,y] := clRed;
//  Canvas.DrawLine(5,y-4, 5,y+5);
//  Canvas.DrawLine(1,y, 10, y);

  Canvas.SetTextColor(clRed);
  y := y + Canvas.Font.Height;  // fonts are different sizes on differet OS's
  Canvas.DrawString(5, y, 'This text must be red.');
  Canvas.SetTextColor(clBlack);
  y := y + Canvas.Font.Height;
  Canvas.DrawString(5, y, 'Russian (UTF-8) text: Невозможно создать директорию');
  y := y + Canvas.Font.Height;
  fnt := fpgApplication.GetFont('Times-14:bold');
  Canvas.Font := fnt;
  Canvas.DrawString(5, y, 'Font used is ' + Canvas.Font.FontDesc);
  y := y + Canvas.Font.Height;


  // Testing basic style drawings
  Canvas.Font := fpgApplication.DefaultFont;
  Canvas.DrawString(320, 3, 'DrawButtonFace():');

  r.SetRect(300, 20, 75, 25);
  Canvas.DrawButtonFace(r, []);
  Canvas.DrawString(385, 20, '= []');
  r.Top := 50;
  Canvas.DrawButtonFace(r, [btfIsDefault]);
  Canvas.DrawString(385, 50, '= [btnIsDefault]');
  r.Top := 80;
  Canvas.DrawButtonFace(r, [btfIsPressed]);
  Canvas.DrawString(385, 80, '= [btnIsPressed]');
  r.Top := 110;
  Canvas.DrawButtonFace(r, [btfIsEmbedded, btfIsPressed]);
  Canvas.DrawString(385, 110, '= [embed & press]');
  r.Top := 140;
  Canvas.DrawButtonFace(r, [btfIsEmbedded]);
  Canvas.DrawString(385, 140, '= [btnIsEmbedded]');

  Canvas.DrawString(45, y, 'DrawControlFrame():');
  y := y + Canvas.Font.Height;
  Canvas.DrawControlFrame(5, y, 150, 23);

  // A Vector Text example
  //----------------------
  ac.LineWidth(1 );
  ac.TextAlignment(AGG_AlignLeft ,AGG_AlignBottom );
  // Normal Text
  ac.LineColor($00 ,$00 ,$8B );
  ac.FillColor($1E ,$90 ,$FF );
  ac.Font('times.ttf' ,45);
  ac.Text(20 ,250 , 'Vectors are cool !' );
  // Upside-down Text
  ac.FlipText(true );
  ac.LineColor($C0 ,$C0 ,$C0 );
  ac.FillColor($C0 ,$C0 ,$C0 );
  ac.Text(20 ,255 ,'Vectors are cool !' );

  // reset the text flip effect
  ac.FlipText(False);

  // Cool new button styles are now possible
  DrawMacLionButton(65, 300, 0, 'Normal');
  DrawMacLionButton(65, 330, 1, 'Hover');
  DrawMacLionButton(65, 360, 2, 'Clicked');
  // nice display of virtical text
  c1.Construct(0,0,0);
  ac.FillColor(c1);
  ac.TextHints(true);
  ac.NoLine;
  ac.Font('arial.ttf', 13, false,
     false, AGG_VectorFontCache, 45.0);
  ac.Text(190, 310, 'Mac OS X Lion buttons', true ,0.0 ,0.0 );
  // reset text alignment to normal behaviour
  ac.TextAlignment(AGG_AlignLeft ,AGG_AlignBottom );

  
  // Star shape
  ac.LineCap(AGG_CapRound);
  ac.LineWidth(5);
  ac.LineColor($32 ,$cd ,$32 );
  c1.Construct(0, 0 , 255, 200);
  c2.Construct(0, 0, 255, 50);
  ac.FillLinearGradient(100, 100, 150, 150, c1, c2);
  ac.Star(100 ,150 ,30 ,70 ,55 ,5 );

  // Draw Arc from 45 degrees to 270 degrees 
  ac.LineColor($FF ,$00 ,$00 );
  ac.LineWidth(5 );
  ac.Arc(300 ,320 ,80 ,50 ,Deg2Rad(45 ) ,Deg2Rad(270 ) );


  // Lines at various thicknesses & coordinate translation
  ac.LineColor(0, 0, 0);
  ac.LineCap(AGG_CapRound);
  d := 0.1;
  for i := 1 to 10 do
  begin
    ac.LineWidth(d);
    ac.Line(350, 240, 410, 180);
    ac.Translate(8, 0);
    d := d + 0.3;
  end;
  ac.ResetTransformations;

  
  
  ac.LineWidth(20 );
  ac.ResetPath;
  ac.MoveTo(360 ,310 );
  ac.LineTo(400 ,270 );
  ac.LineTo(440 ,310 );

  // Default AGG_JoinRound
  ac.Translate(10 ,-10 );
  ac.DrawPath(AGG_StrokeOnly );

  // Change to AGG_JoinMiter
  ac.LineJoin (AGG_JoinMiter );
  ac.Translate(0 ,50 );
  ac.DrawPath (AGG_StrokeOnly );

  // Change to AGG_JoinBevel
  ac.LineJoin (AGG_JoinBevel );
  ac.Translate(0 ,50 );
  ac.DrawPath (AGG_StrokeOnly );

  ac.ResetTransformations;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(357, 214, 500, 400);
  WindowTitle := 'fpGUI with AGG-powered Canvas test';
  Hint := '';
  WindowPosition := wpOneThirdDown;
  OnKeyPress := @FormKeyPressed;
  OnPaint := @FormPaint;

  {@VFD_BODY_END: MainForm}
  {%endregion}

  fpgApplication.HelpKey := keyNul;
end;


procedure MainProc;
var
  frm: TMainForm;
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

