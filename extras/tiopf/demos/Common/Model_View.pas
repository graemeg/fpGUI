unit Model_View;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes
  ,tiMediators
  ;

type
  { TMemo - Name }
  TPerson_Name_Memo_Mediator = class(TMediatorMemoView)
  protected
    procedure SetupGUIandObject; override;
  end;
  

implementation

uses
  Model, tiBaseMediator;
  
  
procedure RegisterMediators;
begin
  // Fallbacks (generic)
  RegisterFallBackMediators;

  // Specific
  gMediatorManager.RegisterMediator(TPerson_Name_Memo_Mediator, TPerson, 'Name');
end;


{ TPerson_Name_Memo_Mediator }

procedure TPerson_Name_Memo_Mediator.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  EditControl.Enabled := False; // fpGUI doesn't have a ReadOnly property yet
end;


initialization
  { Register all your Mediator Views here }
  RegisterMediators;
  
end.

