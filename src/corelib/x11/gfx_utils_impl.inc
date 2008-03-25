{%mainunit gfx_utils.pas}

uses
  Unix;

// X11 specific filesystem implementations of encoding functions

function fpgToOSEncoding(aString: TfpgString): string;
begin
  Result := aString;
end;

function fpgFromOSEncoding(aString: string): TfpgString;
begin
  Result := aString;
end;

procedure fpgOpenURL(const aURL: TfpgString);
var
  Helper: string;
begin
//TODO: Catch "which" command output to run the browser from there
  Helper := '';
  if FileExists('/etc/alternatives/x-www-browser') then
    Helper := '/etc/alternatives/x-www-browser'
  else
  begin
    if fpsystem('which opera') = 0 then
      Helper := 'opera';
    if fpsystem('which mozilla') = 0 then
       Helper := 'mozilla';
    if fpsystem('which konqueror') = 0 then
      Helper := 'konqueror';
    if fpsystem('which firefox') = 0 then
      Helper := 'firefox';
  end;

  fpSystem(Helper + ' ' + aURL + '&');
end;

