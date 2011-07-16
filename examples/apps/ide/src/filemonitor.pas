unit filemonitor;

{$mode objfpc}

interface

uses
  Classes, SysUtils, fpg_main;

type
  TFileMonitor = class(TThread)
  private
    FInterval: integer;
  public
    procedure Execute; override;
    property  Interval: integer read FInterval write FInterval;
  end;

implementation

{ TFileMonitor }

procedure TFileMonitor.Execute;
begin
  while not Terminated do
  begin

  end;
end;

end.

