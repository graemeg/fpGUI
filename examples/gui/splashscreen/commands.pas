{
  Here we define some commands that can be reused throughout a application.
  Command actions are kept separate from the UI code (Forms).
}
unit commands;

{$mode objfpc}{$H+}

interface

uses
  gfx_command_intf,
  gui_memo;
  
type
  // non reference counted interface
  TNullInterfacedObject = class(TObject)
  protected
    function QueryInterface(const IID: TGUID; out Obj): longint; stdcall;
    function _AddRef: longint; stdcall;
    function _Release: longint; stdcall;
  end;


  TAddCommand = class(TInterfacedObject, ICommand)
  private
    FMemo: TfpgMemo;
  public
    constructor Create(AMemo: TfpgMemo); reintroduce;
    procedure   Execute;
  end;


  TExitCommand = class(TInterfacedObject, ICommand)
  public
    procedure   Execute;
  end;


implementation

uses
  fpgfx, SysUtils;

{ TNullInterfacedObject }

function TNullInterfacedObject.QueryInterface(const IID: TGUID; out Obj): longint; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    result := integer(e_nointerface);
end;

function TNullInterfacedObject._AddRef: longint; stdcall;
begin
  Result := -1;
end;

function TNullInterfacedObject._Release: longint; stdcall;
begin
  Result := -1;
end;

{ TAddCommand }

constructor TAddCommand.Create(AMemo: TfpgMemo);
begin
  inherited Create;
  FMemo := AMemo;
end;

procedure TAddCommand.Execute;
begin
  Writeln('>> TAddComand.Execute');
  FMemo.Lines.Add('Hello ' + IntToStr(Random(500)));
  FMemo.Invalidate;
end;

{ TExitCommand }

procedure TExitCommand.Execute;
begin
  Writeln('>> TExitComand.Execute');
  fpgApplication.Terminated := True;
end;

end.

