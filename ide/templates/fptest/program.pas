program ${%PROGRAMNAME%};

{$Mode objfpc}{$H+}


{$Define TextRunner}
{.$Define GUIRunner}


{$ifdef GuiRunner}
  {$apptype gui}
{$endif}


uses
  {$IFDEF TextRunner}
  TextTestRunner,
  {$ENDIF}
  {$IFDEF GUIRunner}
  GUITestRunner,
  {$ENDIF}
  Classes,
  SampleTests;

begin
  // Register all tests
  SampleTests.RegisterTests;

  RunRegisteredTests;
end.
