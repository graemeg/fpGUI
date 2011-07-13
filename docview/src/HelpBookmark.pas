unit HelpBookmark;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  HelpFile,
  HelpTopic;

type
  TBookmark = class(TObject)
  private
    FContentsTopic: TTopic;
    FName: string;
  public
    constructor Load(var F: TextFile; HelpFile: THelpFile);
    procedure   Save(var F: TextFile);
    property    Name: string read FName write FName;
    property    ContentsTopic: TTopic read FContentsTopic write FContentsTopic;
  end;


implementation

{ TBookmark }

constructor TBookmark.Load(var F: TextFile; HelpFile: THelpFile);
var
  s: string;
  ContentsTopicIndex: integer;
begin
  inherited Create;
  ReadLn(F, FName);
  ReadLn(F, s);
  ContentsTopicIndex := StrToInt(s);
  ContentsTopic := HelpFile.Topics[ContentsTopicIndex];
end;

procedure TBookmark.Save(var F: TextFile);
begin

end;


end.

