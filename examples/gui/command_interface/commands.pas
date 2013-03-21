{
  Here we define some commands that can be reused throughout a application.
  Command actions are kept separate from the UI code (Forms).
}
unit commands;

{$mode objfpc}{$H+}

interface

uses
  fpg_command_intf,
  fpg_memo;
  
type
  // non reference counted interface
  TNullInterfacedObject = class(TObject, IUnknown)
  protected
    function QueryInterface(constref iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
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
  fpg_main, SysUtils;

{ TNullInterfacedObject }

function TNullInterfacedObject.QueryInterface(constref IID: TGUID; out Obj): longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    result := integer(e_nointerface);
end;

function TNullInterfacedObject._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

function TNullInterfacedObject._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
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
  DebugLn('>> TAddComand.Execute');
  FMemo.Lines.Add('Hello ' + IntToStr(Random(500)));
  FMemo.Invalidate;
end;

{ TExitCommand }

procedure TExitCommand.Execute;
begin
  DebugLn('>> TExitComand.Execute');
  fpgApplication.Terminated := True;
end;

end.

