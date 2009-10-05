unit nvUtilities;

{$mode objfpc}{$H+}

// disable to remove debugging output
{$Define DEBUG}

interface

uses
  Classes, SysUtils, fpg_base;

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
// Allows for debug output and quite disable of output
procedure ProfileEvent(const AString: string);
// Return AFilename's size in bytes
function GetFileSize(const AFilename: string): integer;

function IsDigit(const AChar: TfpgChar): boolean;
function IsAlpha(const AChar: TfpgChar): boolean;


implementation

//uses
//  character // from utf8tools package (pulls in LCL requirement which we MUST change)
//  ;


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

procedure ProfileEvent(const AString: string);
begin
  {$IFDEF DEBUG}
  writeln('DEBUG:  ', AString);
  {$ENDIF}
end;

function GetFileSize(const AFilename: string): integer;
var
  f: File;
begin
  Result := 0;
  Assign(f, AFileName);
  {$i-}
  Reset(f);
  {$i+}
  Result := FileSize(f);
  CloseFile(f);
end;

function IsDigit(const AChar: TfpgChar): boolean;
begin
  { TODO -oGraeme -cunicode : Not utf-8 compliant. }
  Result := ( AChar>='0' ) and ( AChar<='9' );
  //Result := TCharacter.IsDigit(AChar);
end;

function IsAlpha(const AChar: TfpgChar): boolean;
var
  UppercaseC: TfpgChar;
Begin
  { TODO -oGraeme -cunicode : Not utf-8 compliant. }
  UppercaseC := UpperCase( AChar );
  Result := ( UppercaseC >= 'A' ) and ( UppercaseC <= 'Z' );
  //Result := TCharacter.IsLetter(AChar);
end;

end.

