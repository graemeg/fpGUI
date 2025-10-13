{
  This unit calls the RegisterTests procedure of each test unit. This makes
  it easy to display test units if needed for some reason.
}
unit testdependencies;

{$mode objfpc}{$h+}

interface

uses
//  tctreeview
//  ,tcfpgbase
  tcfontmanager
  ;

implementation

initialization
//  tctreeview.RegisterTests;
// tcfpgbase.RegisterTests;
  tcfontmanager.RegisterTests;

end.
