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
  tcborderlayout,
  tcmiglayout,
  tcmig_unitvalue,
  tcmig_boundsize,
  tcmig_platformdefaults,
  tcmig_dimconstraint,
  tcmig_ac,
  tcmig_lc,
  tcmig_cc,
  tcmig_resizeconstraint,
  tcmig_linkhandler,
  tcmig_layoututil,
  tcmig_grid;

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
  tcmiglayout.RegisterTests;
  // tcmig_unitvalue registers automatically via testregistry
end.
