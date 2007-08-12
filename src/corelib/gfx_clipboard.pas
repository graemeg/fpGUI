unit gfx_clipboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TfpgClipboard = class(TObject)
  private
    FClipboardData: string;
    function    GetText: string;
    procedure   SetText(const AValue: string);
  public
    property    Text: string read GetText write SetText;
  end;
  
// singleton
function fpgClipboard: TfpgClipboard;

implementation

var
  uClipboard: TfpgClipboard;

function fpgClipboard: TfpgClipboard;
begin
  if not Assigned(uClipboard) then
    uClipboard := TfpgClipboard.Create;
  Result := uClipboard;
end;
  
  
{ TfpgClipboard }

function TfpgClipboard.GetText: string;
begin
  // this is just temporary!!
  Result := FClipboardData;
end;

procedure TfpgClipboard.SetText(const AValue: string);
begin
  FClipboardData := AValue;
end;

initialization
  uClipboard := nil;
  
finalization
  uClipboard.Free;

end.

