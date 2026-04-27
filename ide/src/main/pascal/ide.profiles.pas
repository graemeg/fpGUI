{
    fpGUI IDE - Profile Management

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Extracted from ide.form.main.pas — separates profile management
      logic from the main form.

      ToggleProfileInList is a pure procedure that adds or removes a
      profile name from a string list.

      ProfilesDisplayText returns user-friendly display text for the
      current active profiles.
}
unit ide.profiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ Toggle a profile name in a string list: removes it if present, adds
  it if absent. }
procedure ToggleProfileInList(AProfiles: TStringList; const AProfileName: string);

{ Returns display text for active profiles: the CommaText if non-empty,
  or '(no profiles)' if empty. }
function ProfilesDisplayText(AProfiles: TStringList): string;

implementation

procedure ToggleProfileInList(AProfiles: TStringList; const AProfileName: string);
var
  idx: Integer;
begin
  idx := AProfiles.IndexOf(AProfileName);
  if idx >= 0 then
    AProfiles.Delete(idx)
  else
    AProfiles.Add(AProfileName);
end;

function ProfilesDisplayText(AProfiles: TStringList): string;
begin
  if AProfiles.Count > 0 then
    Result := AProfiles.CommaText
  else
    Result := '(no profiles)';
end;

end.
