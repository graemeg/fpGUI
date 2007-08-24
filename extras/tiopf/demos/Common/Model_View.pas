unit Model_View;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes
  ,tiGenericEditMediators
//  ,tiGenericListMediators
//  ,tiCompositeMediators
  ;

type
  { TEdit - Name }
  TPerson_Name_TextEdit_View = class(TMediatorEditView)
  protected
    procedure SetupGUIandObject; override;
  end;


  { TSpinEdit - Age }
{
  TPerson_Age_SpinEdit_View = class(TMediatorSpinEditView)
  protected
    procedure SetupGUIandObject; override;
  end;
}

  { TTrackBar - Age }
  TPerson_Age_TrackBar_Mediator = class(TMediatorTrackBarView)
  protected
    procedure SetupGUIandObject; override;
  end;
  
  
  { TMemo - Name }
  TPerson_Name_Memo_Mediator = class(TMediatorMemoView)
  protected
    procedure SetupGUIandObject; override;
  end;
  
  
  { TCombobox - Gender }
  TPerson_Gender_ComboBox_Mediator = class(TMediatorComboBoxView)
  protected
    procedure SetupGUIandObject; override;
  end;
  

  { TPersonList_ComboBox_Mediator }
{
  TPersonList_ComboBox_Mediator = class(TComboBoxMediator)
  protected
    procedure SetupGUIandObject; override;
  end;
  
  
  TPersonList_ListView_CompositeMediator = class(TCompositeListViewMediator)
  protected
    procedure   SetupGUIandObject; override;
  end;
}

implementation


{ TPersonNameView }

procedure TPerson_Name_TextEdit_View.SetupGUIandObject;
begin
  inherited;
  { The Name field my only contain 25 characters max. }
  EditControl.MaxLength := 25;
end;


{ TPerson_Name_Memo_Mediator }

procedure TPerson_Name_Memo_Mediator.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  { The Name field my only contain 25 characters max. }
//  EditControl.ReadOnly := True;
  EditControl.Enabled := False;
end;


{ TPerson_Gender_ComboBox_Mediator }

procedure TPerson_Gender_ComboBox_Mediator.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
//  EditControl.Style := csDropDownList;
//  TComboBox(EditControl).ReadOnly := True;
end;


{ TPersonList_ComboBox_Mediator }
(*
procedure TPersonList_ComboBox_Mediator.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  View.Style := csDropDownList;
//  View.ReadOnly := True;
end;


{ TPersonList_ListView_CompositeMediator }

procedure TPersonList_ListView_CompositeMediator.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
//  View.OnCustomDrawItem := ListViewCustomDrawItem;
end;
*)

{ TPerson_Age_SpinEdit_View }
{
procedure TPerson_Age_SpinEdit_View.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  EditControl.MaxValue := 100;
end;
}
{ TPerson_Age_TrackBar_Mediator }

procedure TPerson_Age_TrackBar_Mediator.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  EditControl.Max := 100;
end;

initialization
  {-----------------------------------------------------------------------------
    Register all your Mediator Views here
      Params: ClassName and Property name of the business object as a string
              Mediator View class
  -----------------------------------------------------------------------------}
  
  // This is not used anymore and needs to be removed
{
  gMediatorFactory.RegisterMediatorClass('TPerson.Name'  ,TPerson_Name_TextEdit_View);
  gMediatorFactory.RegisterMediatorClass('TPerson.Age'   ,TPerson_Age_SpinEdit_View);
  gMediatorFactory.RegisterMediatorClass('TPerson.Age'   ,TPerson_Age_TrackBar_Mediator);
  gMediatorFactory.RegisterMediatorClass('TPerson.Name'  ,TPerson_Name_Memo_Mediator);
  gMediatorFactory.RegisterMediatorClass('TPerson.GenderGUI',TPerson_Gender_ComboBox_Mediator);
}
end.
