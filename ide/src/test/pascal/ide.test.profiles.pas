{
    fpGUI IDE - Profile Management Tests

    Tests for pure functions in ide.profiles: profile toggling and
    display text generation.
}
unit ide.test.profiles;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  ide.profiles;

type

  { TTestToggleProfileInList }

  TTestToggleProfileInList = class(TTestCase)
  published
    procedure TestAddProfile;
    procedure TestRemoveProfile;
    procedure TestToggleTwiceRestores;
  end;

  { TTestProfilesDisplayText }

  TTestProfilesDisplayText = class(TTestCase)
  published
    procedure TestEmptyList;
    procedure TestSingleProfile;
    procedure TestMultipleProfiles;
  end;

implementation

{ TTestToggleProfileInList }

procedure TTestToggleProfileInList.TestAddProfile;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ToggleProfileInList(list, 'unix');
    AssertEquals('should have one item', 1, list.Count);
    AssertEquals('should contain unix', 'unix', list[0]);
  finally
    list.Free;
  end;
end;

procedure TTestToggleProfileInList.TestRemoveProfile;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    list.Add('unix');
    list.Add('debug');
    ToggleProfileInList(list, 'unix');
    AssertEquals('should have one item', 1, list.Count);
    AssertEquals('remaining should be debug', 'debug', list[0]);
  finally
    list.Free;
  end;
end;

procedure TTestToggleProfileInList.TestToggleTwiceRestores;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ToggleProfileInList(list, 'agg');
    ToggleProfileInList(list, 'agg');
    AssertEquals('toggling twice should leave list empty', 0, list.Count);
  finally
    list.Free;
  end;
end;

{ TTestProfilesDisplayText }

procedure TTestProfilesDisplayText.TestEmptyList;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    AssertEquals('empty list should show placeholder',
      '(no profiles)', ProfilesDisplayText(list));
  finally
    list.Free;
  end;
end;

procedure TTestProfilesDisplayText.TestSingleProfile;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    list.Add('unix');
    AssertEquals('single profile', 'unix', ProfilesDisplayText(list));
  finally
    list.Free;
  end;
end;

procedure TTestProfilesDisplayText.TestMultipleProfiles;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    list.Add('unix');
    list.Add('debug');
    AssertEquals('multiple profiles', 'unix,debug', ProfilesDisplayText(list));
  finally
    list.Free;
  end;
end;

initialization
  RegisterTest(TTestToggleProfileInList);
  RegisterTest(TTestProfilesDisplayText);

end.
