program fpcunit_miglayout;

{$mode objfpc}{$H+}

uses
  Classes,
  GUITestRunner,
  constraintparser_tests,
  gui_mig_constraintparser;



begin
  // Register all tests
//  sample_tests.RegisterTests;

  RunRegisteredTests;
end.
