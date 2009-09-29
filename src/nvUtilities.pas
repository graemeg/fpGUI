unit nvUtilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
 { TODO -oGraeme : Should this change to LineEnding (platfrom dependant) }
  EndLine= chr(13)+chr(10);
  TwoEndLines= chr(13)+chr(10)+chr(13)+chr(10);
  Quote = '''';
  DoubleQuote = '"';


// Removes and returns the first value in a separated
// value list (removes quotes if found)
Function ExtractNextValue(
          var S: string;
          const Separator: string ): string;

Function ExtractNextValueNoTrim(
           var S: string;
           const Separator: string ): string;

// Alias method which is the same as Move() but with less confusing name
procedure MemCopy(const src; var dest; size: SizeInt);


implementation


Function ExtractNextValue( var S: string;
                           const Separator: string ): string;
begin
  Result := ExtractNextValueNoTrim( S, Separator );
  Result := trim( Result );

  // Remove quotes if present
  if Result <> '' then
    if Result[ 1 ] = DoubleQuote then
      Delete( Result, 1, 1 );

  if Result <> '' then
    if Result[ length( Result ) ] = DoubleQuote then
      Delete( Result, length( Result ), 1 );
end;

Function ExtractNextValueNoTrim( var S: string;
                                 const Separator: string ): string;
Var
  SeparatorPos: integer;
Begin
  SeparatorPos := Pos( Separator, S );
  if SeparatorPos > 0 then
  begin
    Result := Copy( S, 1, SeparatorPos-1 );
    Delete( S, 1, SeparatorPos + length( Separator ) - 1 );
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

procedure MemCopy(const src; var dest; size: SizeInt);
begin
  Move(src, dest, size);
end;


end.

