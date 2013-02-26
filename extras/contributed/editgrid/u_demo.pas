unit u_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils,
  fpg_main, fpg_base,
  fpg_form, fpg_panel, fpg_button, fpg_basegrid, fpg_editcombo, fpg_checkbox, fpg_radiobutton,
  u_editgrid;

type
  TF_Demo = class(TfpgForm)
    private
      EG_Grid: TfpgEditGrid;
      Bt_AddOne: TfpgButton;
      Ckb_Limits: TfpgCheckBox;
      Ckb_FloatDec: TfpgCheckBox;
      Ckb_FloatFixDec: TfpgCheckBox;
      Ckb_Column: TfpgCheckBox;
      Ckb_Row: TfpgCheckBox;
      Rb_Point: TfpgRadioButton;
      Rb_Comma: TfpgRadioButton;
      Ckb_Space: TfpgCheckBox;
      Ckb_Thousand: TfpgCheckBox;
      Ckb_NegativeColor: TfpgCheckBox;
      P_EditCombo: TfpgPanel;
      Ckb_AutoComplete: TfpgCheckBox;
      Rb_No: TfpgRadioButton;
      Rb_Yes: TfpgRadioButton;
      Rb_Ask: TfpgRadioButton;
      Bt_Fermer: TfpgButton;
      procedure EG_GridKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
                var Consumed: Boolean);
      procedure Bt_AddOneClick(Sender: TObject);
      procedure Ckb_LimitsChange(Sender: TObject);
      procedure Ckb_ThousandChange(Sender: TObject);
      procedure Ckb_NegativeColorChange(Sender: TObject);
      procedure Rb_Change(Sender: TObject);
      procedure Ckb_SpaceChange(Sender: TObject);
      procedure Ckb_FloatDecChange(Sender: TObject);
      procedure Ckb_FloatFixDecChange(Sender: TObject);
      procedure Ckb_ColumnChange(Sender: TObject);
      procedure Ckb_RowChange(Sender: TObject);
      procedure Ckb_AutoCompleteChange(Sender: TObject);
      procedure Rb_EditComboChange(Sender: TObject);
      procedure Bt_FermerClick(Sender: TObject);
    public
      constructor Create(AOwner: TComponent); override;
      procedure   AfterCreate; override;
    end;

var
  F_Demo: TF_Demo;

implementation

var
  ComboBoxListe: TStringList;

procedure PopulateListe;
begin
ComboBoxListe:= TStringList.Create;
ComboBoxListe.Add('one');
ComboBoxListe.Add('two');
ComboBoxListe.Add('three');
ComboBoxListe.Add('four');
ComboBoxListe.Add('five');
ComboBoxListe.Add('six');
ComboBoxListe.Add('seven');
ComboBoxListe.Add('eight');
ComboBoxListe.Add('nine');
ComboBoxListe.Add('ten');
end;

procedure TF_Demo.EG_GridKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
          var Consumed: Boolean);
begin
case KeyCode of
  KeyInsert:
    with EG_Grid do
      begin
      RowCount:= RowCount+1;
      FocusCol:= 0;
      FocusRow:= Pred(RowCount);
      Consumed:= True;
      end;
  end;
end;

procedure TF_Demo.Bt_AddOneClick(Sender: TObject);
var
  ADate: TDateTime;
begin
with EG_Grid do
  begin
  RowCount:= RowCount+1;
  Cells[0,Pred(RowCount)]:= 'No edit';
  Cells[1,Pred(RowCount)]:= 'Row '+IntToStr(RowCount);
  Cells[2,Pred(RowCount)]:= IntToStr(RowCount*RowCount*100);
  Cells[3,Pred(RowCount)]:= FloatToStr(1000/RowCount);
  Cells[4,Pred(RowCount)]:= FormatCurr('0.00',FloatToCurr(1000/RowCount));
  Cells[5,Pred(RowCount)]:= ComboBoxListe[Pred(RowCount) mod ComboBoxListe.Count];
  Cells[6,Pred(RowCount)]:= '';
  if Odd(RowCount) then
    Cells[7,Pred(RowCount)]:= 'True'
  else
    Cells[7,Pred(RowCount)]:= 'False';
  ADate:= IncDay(Now,RowCount);
  Dates[8]:= ADate;
  Cells[8,Pred(RowCount)]:= FormatDateTime(EG_Grid.GridDateFormat[8], Adate);
  end;
end;

procedure TF_Demo.Ckb_LimitsChange(Sender: TObject);
var
  Cpt: Integer;
begin
  with EG_Grid do
    for Cpt:= 0 to Pred(ColumnCount) do
      if Ckb_Limits.Checked then
        case ColumnEditType[Cpt] of
          etInteger:
            begin
              MaxIntValue[Cpt]:= 5000;
              MinIntValue[Cpt]:= -1000;
            end;
          etFloat:
            begin
              MaxFloatValue[Cpt]:= 5000;
              MinFloatValue[Cpt]:= -1000;
            end;
          etCurrency:
            begin
              MaxCurrValue[Cpt]:= 5000;
              MinCurrValue[Cpt]:= -1000;
            end;
        end
      else
        case ColumnEditType[Cpt] of
          etInteger, etFloat, etCurrency:
            begin
              NumericMaxLimit[Cpt]:= False;
              NumericMinLimit[Cpt]:= False;
            end;
        end;
end;

procedure TF_Demo.Ckb_ThousandChange(Sender: TObject);
var
  Cpt: Integer;
begin
  with EG_Grid do
  begin
    for Cpt:= 0 to Pred(ColumnCount) do
      if Ckb_Thousand.Checked then
        case ColumnEditType[Cpt] of
          etInteger, etFloat, etCurrency:
            begin
            NumericShowThousand[Cpt]:= True;
            Ckb_Space.Enabled:= False;
            end;
        end
      else
        case ColumnEditType[Cpt] of
          etInteger, etFloat, etCurrency:
            begin
            NumericShowThousand[Cpt]:= False;
            Ckb_Space.Enabled:= True;
            end;
        end;
    Invalidate;
  end;
end;

procedure TF_Demo.Ckb_NegativeColorChange(Sender: TObject);
var
  Cpt: Integer;
begin
  with EG_Grid do
  begin
    for Cpt:= 0 to Pred(ColumnCount) do
      if Ckb_NegativeColor.Checked then
        case ColumnEditType[Cpt] of
          etInteger, etFloat, etCurrency:
            NumericNegativeColor[Cpt]:= clRed;

        end
      else
        case ColumnEditType[Cpt] of
          etInteger, etFloat, etCurrency:
            NumericNegativeColor[Cpt]:= clBlack;
        end;
    Invalidate;
  end;
end;

procedure TF_Demo.Rb_Change(Sender: TObject);
var
  Cpt: Integer;
begin
  with EG_Grid do
  begin
    for Cpt:= 0 to Pred(ColumnCount) do
      if Sender is TfpgRadioButton then
        case (Sender as TfpgRadioButton).tag of
          0:
            case ColumnEditType[Cpt] of
              etInteger:
                if Ckb_Space.Checked then
                  NumericThousandSeparator[Cpt] := ' '
                else
                  NumericThousandSeparator[Cpt] := ',';
              etFloat, etCurrency:
                begin
                  NumericDecimalSeparator[Cpt] := '.';
                  if Ckb_Space.Checked then
                    NumericThousandSeparator[Cpt] := ' '
                  else
                    NumericThousandSeparator[Cpt] := ',';
                end;
            end;
          1:
            case ColumnEditType[Cpt] of
              etInteger, etFloat, etCurrency:
                begin
                  NumericDecimalSeparator[Cpt] := ',';
                  if Ckb_Space.Checked then
                    NumericThousandSeparator[Cpt] := ' '
                  else
                    NumericThousandSeparator[Cpt] := '.';
                end;
            end;
        end;
    Invalidate;
  end;
end;

procedure TF_Demo.Ckb_SpaceChange(Sender: TObject);
var
  Cpt: Integer;
begin
  with EG_Grid do
    for Cpt:= 0 to Pred(ColumnCount) do
      case ColumnEditType[Cpt] of
        etInteger, etFloat, etCurrency:
          begin
          if Ckb_Space.Checked then
            NumericThousandSeparator[Cpt] := ' '
          else
            if Rb_Point.Checked then
              NumericThousandSeparator[Cpt] := ','
            else
              NumericThousandSeparator[Cpt] := '.';
          end;
      end;
end;

procedure TF_Demo.Ckb_FloatDecChange(Sender: TObject);
var
  Cpt: Integer;
begin
  with EG_Grid do
    for Cpt:= 0 to Pred(ColumnCount) do
      case ColumnEditType[Cpt] of
        etFloat:
          if Ckb_FloatDec.Checked then
          begin
            Ckb_FloatFixDec.Checked := False;
            NumericDecimals[Cpt] := 3;
          end
          else
            FloatFixedDecimals[Cpt] := -1;
      end;
end;

procedure TF_Demo.Ckb_FloatFixDecChange(Sender: TObject);
var
  Cpt: Integer;
begin
  with EG_Grid do
    for Cpt:= 0 to Pred(ColumnCount) do
      case ColumnEditType[Cpt] of
        etFloat:
          if Ckb_FloatFixDec.Checked then
          begin
            Ckb_FloatDec.Checked := False;
            FloatFixedDecimals[Cpt] := 3;
          end
          else
            NumericDecimals[Cpt] := -1;
      end;
end;

procedure TF_Demo.Ckb_ColumnChange(Sender: TObject);
begin
  if Ckb_Column.Checked then
  begin
    EG_Grid.EditWay:= edColumn;
    Ckb_Row.Checked:= False;
  end
  else
  if not Ckb_Row.Checked then
    Eg_Grid.EditWay:= edNone;
end;

procedure TF_Demo.Ckb_RowChange(Sender: TObject);
begin
  if Ckb_Row.Checked then
  begin
    EG_Grid.EditWay:= edRow;
    Ckb_Column.Checked:= False;
  end
  else
  if not Ckb_Column.Checked then
    Eg_Grid.EditWay:= edNone;
end;

procedure TF_Demo.Ckb_AutoCompleteChange(Sender: TObject);
var
  Cpt: Integer;
begin
  with EG_Grid do
    for Cpt:= 0 to Pred(ColumnCount) do
      case ColumnEditType[Cpt] of
        etEditCombo:
          AutoComplete[Cpt] := Ckb_AutoComplete.Checked;
      end;
end;

procedure TF_Demo.Rb_EditComboChange(Sender: TObject);
var
  Cpt: Integer;
begin
  with EG_Grid do
    for Cpt:= 0 to Pred(ColumnCount) do
      case ColumnEditType[Cpt] of
        etEditCombo:
          begin
            if RB_No.Checked then
              AllowNew[Cpt] := anNo;
            if RB_Yes.Checked then
              AllowNew[Cpt] := anYes;
            if RB_Ask.Checked then
              AllowNew[Cpt] := anAsk;
          end;
      end;
end;

procedure TF_Demo.Bt_FermerClick(Sender: TObject);
begin
ComboBoxListe.Free;
Close;
end;

constructor TF_Demo.Create(AOwner: TComponent);
var
  Cpt: Integer;
begin
inherited Create(AOwner);
Name:= 'F_Demo';
WindowTitle:= 'EditGrid demo';
SetPosition(0,0,1000,400);
WindowPosition:= wpScreencenter;
Sizeable:= False;
PopulateListe;
EG_Grid:= CreateEditGrid(Self,10,10,Width-20,Height-120);
with EG_Grid do
  begin
  AddColumn('None',50,taCenter);
  AddColumn('Text',100,etText);
  TextColor[Pred(ColumnCount)] := clBlue;
  AddColumn('Integer',90,etInteger,taRightJustify);
  AddColumn('Float',90,etFloat,taRightJustify);
  AddColumn('Currency',90,etCurrency,taRightJustify);
  AddColumn('ComboBox',120,etComboBox);
  for Cpt:= 0 to Pred(ComboBoxListe.Count) do
    AddComboItem(Pred(ColumnCount),ComboBoxListe[Cpt]);
  ComboBoxDropDownCount[Pred(ColumnCount)] := 6;
  AddColumn('EditCombo',120,etEditCombo);
  AutoComplete[Pred(ColumnCount)] := True;
  AllowNew[Pred(ColumnCount)] := anAsk;
  EditComboDropDownCount[Pred(ColumnCount)] := 4;
  AddColumn('CheckBox',100,etCheckBox,taCenter);
  BoxCheckedText[Pred(ColumnCount)] := 'True';
  BoxUncheckedText[Pred(ColumnCount)] := 'False';
  BoxDisplayText[Pred(ColumnCount)] := 'CheckBox';
  AddColumn('Calendar',120,etCalendar,taCenter);
  GridDateFormat[Pred(ColumnCount)] := LongDateFormat;
  CalendarDateFormat[Pred(ColumnCount)] := ShortDateFormat;
  DateValue[Pred(ColumnCount)] := Now;
  WeekStartDay[Pred(ColumnCount)] := 1;
  WeeklyHoliday[Pred(ColumnCount)] := 7;
  DayColor[Pred(ColumnCount)] := clBlue;
  HoliDayColor[Pred(ColumnCount)] := clRed;
  SingleClickSelect[Pred(ColumnCount)] := True;
  DefaultRowHeight:= 20;
  HeaderFontDesc:= 'bitstream vera sans-10:bold';
//  Options:= [go_HideFocusRect];
  Options:= [go_AlternativeColor];
  OnKeyPress:= @EG_GridKeyPress;
  end;
Bt_AddOne:= CreateButton(Self,20,Height-100,100,'Add 1 line',@Bt_AddOneClick,'');
Ckb_Limits:= CreateCheckBox(Self,150,Height-100,'Limit min and max numeric values');
Ckb_Limits.OnChange:= @Ckb_LimitsChange;
Ckb_FloatDec:= CreateCheckBox(Self,150,Height-80,'Limit EditFloat to 3 decimals');
Ckb_FloatDec.OnChange:= @Ckb_FloatDecChange;
Ckb_FloatFixDec:= CreateCheckBox(Self,150,Height-60,'Set EditFloat to 3 decimals');
Ckb_FloatFixDec.OnChange:= @Ckb_FloatFixDecChange;
Ckb_Column:= CreateCheckBox(Self,150,Height-40,'Edit changing to next column');
Ckb_Column.OnChange:= @Ckb_ColumnChange;;
Ckb_Row:= CreateCheckBox(Self,150,Height-20,'Edit changing to next row');
Ckb_Row.OnChange:= @Ckb_RowChange;;
Rb_Point:= CreateRadioButton(Self,400,Height-100,'Point as decimal separator');
Rb_Point.Tag:= 0;
Rb_Point.OnChange:= @Rb_Change;
Rb_Comma:= CreateRadioButton(Self,400,Height-80,'Comma as decimal separator');
Rb_Comma.Tag:= 1;
Rb_Comma.OnChange:= @Rb_Change;
Ckb_Space:= CreateCheckBox(Self,400,Height-60,'Space as thousand separator');
Ckb_Space.OnChange:= @Ckb_SpaceChange;
Ckb_Thousand:= CreateCheckBox(Self,400,Height-40,'Show thousand separator');
Ckb_Thousand.OnChange:= @Ckb_ThousandChange;
Ckb_Thousand.Checked:= True;
Ckb_NegativeColor:= CreateCheckBox(Self,400,Height-20,'Negative values in red');
Ckb_NegativeColor.OnChange:= @Ckb_NegativeColorChange;
Ckb_NegativeColor.Checked:= True;
P_EditCombo:= CreatePanel(Self,650,Height-110,170,100,'EditCombo',bsFlat,taCenter,tlTop);
Ckb_AutoComplete:= CreateCheckBox(P_EditCombo,10,20,'Auto Completion');
Ckb_AutoComplete.OnChange:= @Ckb_AutoCompleteChange;
Rb_No:= CreateRadioButton(P_EditCombo,10,40,'No new item');
Rb_No.OnChange:= @Rb_EditComboChange;
Rb_Yes:= CreateRadioButton(P_EditCombo,10,60,'Auto new item');
Rb_Yes.OnChange:= @Rb_EditComboChange;
Rb_Ask:= CreateRadioButton(P_EditCombo,10,80,'Confirm new item');
Rb_Ask.OnChange:= @Rb_EditComboChange;
Rb_Ask.Checked:= True;
Bt_Fermer:= CreateButton(Self,Width-130,Height-40,100,'Close',@Bt_FermerClick,'stdimg.exit');
end;

procedure TF_Demo.AfterCreate;
begin
Rb_Point.Checked:= True;
end;

end.

