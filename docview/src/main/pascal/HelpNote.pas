unit HelpNote;

{$mode objfpc}

interface

uses
  Classes, fpg_base, HelpTopic;

type
  { Simple data object store information about help note and position of note }
  THelpNote = class(TObject)
  public
    Text: TfpgString;
    Topic: TTopic;
    InsertPoint: longint;
    // calculated
    InsertText: TfpgString;
    constructor Create;
    destructor  Destroy; override;
  end;

implementation

{ THelpNote }

constructor THelpNote.Create;
begin
  inherited Create;
  Text          := '';
  InsertText    := '';
  Topic         := nil;
  InsertPoint   := -1;
end;

destructor THelpNote.Destroy;
begin
  inherited Destroy;
end;

end.

