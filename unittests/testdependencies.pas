{
  This unit calls the RegisterTests procedure of each test unit. This makes
  it easy to display test units if needed for some reason.
}
unit testdependencies;

{$mode objfpc}{$h+}

interface

uses
  tctreeview
  ;

implementation

initialization
  tctreeview.RegisterTests;

end.
