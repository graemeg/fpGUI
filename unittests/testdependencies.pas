{
  This unit calls the RegisterTests procedure of each test unit. This makes
  it easy to display test units if needed for some reason.
}
unit testdependencies;

{$mode objfpc}{$h+}

interface

uses
  tctreeview,
  tcfpgbase,
  tcfontmanager,
  tcfontcacheremoval,
  tcfontdefinition,
  tclayoutmanager,
  tcflowlayout,
  tcborderlayout;

implementation

initialization
//  tctreeview.RegisterTests;
  tcfpgbase.RegisterTests;
  tcfontmanager.RegisterTests;
  tcfontcacheremoval.RegisterTests;
  tcfontdefinition.RegisterTests;
  tclayoutmanager.RegisterTests;
  tcflowlayout.RegisterTests;
  tcborderlayout.RegisterTests;
end.
