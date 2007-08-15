program filegrid;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpgfx,
  gui_form,
  gui_grid;

type
  TMainForm = class(TfpgForm)
  private
    FGrid: TfpgFileGrid;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Filegrid Test';
  SetPosition(100, 100, 620, 400);
  
  FGrid := TfpgFileGrid.Create(self);
  FGrid.SetPosition(8, 8, 600, 370);
  FGrid.FileList.ReadDirectory('*', False);
  FGrid.Anchors := [anLeft, anTop, anBottom, anRight];
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


