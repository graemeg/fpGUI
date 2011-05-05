unit fpg_style_win2k; 

{$mode objfpc}{$H+}

interface

uses
  fpg_main
  ,fpg_style
  ;

type
  TfpgWin2000Style = class(TfpgStyle)

  end;

implementation

uses
  fpg_stylemanager
  ;


initialization
  fpgStyleManager.RegisterClass(cDefaultStyle, TfpgWin2000Style);   // TODO: This will change later
  fpgStyleManager.RegisterClass('Win2000', TfpgWin2000Style);


end.

