Unit ACLStringUtility;

{$mode objfpc}{$H+}

Interface

Uses
  Classes;

const
  EndLine= chr(13)+chr(10);
  TwoEndLines= chr(13)+chr(10)+chr(13)+chr(10);
  Quote = '''';
  DoubleQuote = '"';

// ----------- Character testing functions -----------------------

type
  TCharMatchFunction = function( const a: char ): boolean;

// Returns true if c is a digit 0..9
Function IsDigit( const c: char ): boolean;

// Returns true if c is not a digit
Function IsNonDigit( const c: char ): boolean;

// Returns true if c is an alphabetic character a..z A..Z
Function IsAlpha( const c: char ): boolean;

// Returns true if s is only spaces (or empty)
Function IsSpaces( const s: string ): boolean;

// ---------------------- Numeric conversions ---------------------------------------

// Converts a hex string to a longint
// May be upper or lower case
// Does not allow a sign
// Is not forgiving as StrToInt: all characters
// must be valid hex chars.
function HexToInt( s: string ): longint;

// Given a string with a number on the end, increments that
// number by one.
// If there is no number it adds a one.
// If the number is left zero padded then the result is similarly
// padded
Function IncrementNumberedString( StartString: string ): string;

// ---------------------- Pascal String Utilities ---------------------------------------

Function CaseInsensitivePos( const a: string;
                             const b: string ): longint;

// Looks for occurrences of QuoteChar and inserts a duplicate
Function InsertDuplicateChars( const S: string;
                               const QuoteChar: char ): string;

// Returns index of SubString in S, case-insensitve
Function FindSubstring( const SubString: string;
                        const S: string ): integer;

// Returns characters at the front of S that match according
// to a given function... for example, IsDigit, IsNonDigit, IsAlpha
Function MatchLeadingChars(
           const S: string;
           MatchFunction: TCharMatchFunction ): string;

// Same as MatchLeadingChars, but removes the matching chars from S
Function ExtractLeadingChars(
           Var S: string;
           MatchFunction: TCharMatchFunction ): string;

// Case insensitive compare
Function StringsSame( const a, b: string ): boolean;

// Quoting

// Note: these functions do not check for existing quotes within
// the string, they only add or delete characters at the end.

// Returns S in single quotes
Function StrQuote( const s: string ): string;

// Returns S without single quotes
Function StrUnQuote( const s: string ): string;

// Returns S in double quotes
Function StrDoubleQuote( const s: string ): string;

// Returns S in double quotes,
// with any double quotes in S duplicated
Function StrFullDoubleQuote( const s: string ): string;

// Returns S without double quotes
Function StrUnDoubleQuote( const s: string ): string;

//

// Substitutes given character
Function SubstituteChar( const S: string; const Find: Char; const Replace: Char ): string;

// Returns the count rightmost chars of S
Function StrRight( const S:string; const count:integer ):string;

// Returns the remainder of S starting at start
Function StrRightFrom( const S:string; const start:integer ):string;

// Returns the count leftmost chars of S
Function StrLeft( const S:string; const count:integer ):string;

// Returns S minus count characters from the right
Function StrLeftWithout( const S:string; const count:integer ):string;

// Returns S with leftCount chars removed from the left and
// rightCount chars removed from the right.
Function StrRemoveEnds( const S:string; const leftCount:integer; const rightCount:integer ):string;

// Produces a string from n padded on the left with 0's
// to width chars
Function StrLeft0Pad( const n: integer; const width: integer ): string;

// Returns true if s starts with start (case insensitive)
Function StrStarts( const start: string; const s: string ): boolean;

// Returns true if s ends with endstr (case insensitive)
Function StrEnds( const endStr: string; const s: string ): boolean;

// Returns first word from S
Function StrFirstWord( const S: String ): string;

// ------------ Seperated value utilities ---------------------

// Returns the next item starting at Index. Spaces are the separator,
// unless the item is quoted with QuoteChar, in which case it looks for
// a closing quote. Occurrences of the QuoteChar in the item itself,
// can be escaped with a duplicate, e.g. "He said ""bok"""
Procedure GetNextQuotedValue(
           const S: string;
           var Index: longint;
           var Value: string;
           const QuoteChar: char );

procedure GetNextValue(
           const S: String;
           var Index: longint;
           var Value: string;
           const Seperator: char );

// Removes and returns the first value in a separated
// value list (removes quotes if found)
Function ExtractNextValue(
          var S: string;
          const Separator: string ): string;

Function ExtractNextValueNoTrim(
           var S: string;
           const Separator: string ): string;


// Parses a line of the form
// key = value into it's components
Procedure ParseConfigLine( const S: string;
                           var KeyName: string;
                           var KeyValue: string );

// Removes spaces around the separator in the given string
Procedure RemoveSeparatorSpaces( var S: string;
                                 const Separator:string );

{$ifdef os2}
// ------------ Ansi String utilities ------------------------

// Right & left trim that works with AnsiStrings.
Function AnsiTrim( const S: AnsiString ): AnsiString;

Procedure AnsiParseConfigLine( const S: Ansistring;
                               var keyName: Ansistring;
                               var keyValue: Ansistring );

Function AnsiExtractNextValue( var CSVString: AnsiString;
                               const Separator: AnsiString ): AnsiString;

{$endif}

// ------------- Lists of strings, and strings as lists -----------

// Adds NewValue to S as a separated list
Procedure AddToListString( Var S: string;
                           const NewValue: string;
                           const Separator: string );

Function ListToString( List: TStrings;
                       const Separator: string ): string;

procedure StringToList( S: String;
                        List: TStrings;
                        const Separator: string );

// Reverse the given list. It must be set to not sorted
Procedure ReverseList( TheList: TStrings );

// Sort the given list into reverse alphabetical order
//Procedure ReverseSortList( TheList: TStringList );

// Find the given string in the given list, using
// case insensitive searching (and trimming)
// returns -1 if not found
Function FindStringInList( const TheString: string;
                           TheList:TStrings ):longint;

Procedure MergeStringLists( Dest: TStringList;
                            AdditionalList: TStringList );

// ---------------------- PCHAR Utilities ---------------------------------------

function StrNPas( const ps: PChar; const Length: integer ): String;

// Returns a - b
Function PCharDiff( const a: PChar; const b: Pchar ): longword;

// trims spaces and carriage returns of the end of Text
procedure TrimWhitespace( Text: PChar );

type
  TSetOfChars = set of char;

function TrimChars( const s: string;
                    chars: TSetOfChars ): string;

// Concatenates a pascal string onto a PCHar string
// Resizes if needed
procedure StrPCat( Var Dest: PChar;
                   const StringToAdd: string );

// Trim endlines (#10 or #13) off the end of
// the given string.
Procedure TrimEndLines( const S: PChar );

// Allocates enough memory for a copy of s as a PChar
// and copies s to it.
Function StrDupPas( const s: string ): PChar;

// Returns a copy of the first n chars of s
Function StrNDup( const s: PChar; const n: integer ): PChar;

// Returns a copy of the first line starting at lineStart
Function CopyFirstLine( const lineStart: PChar ): PChar;

// Returns next line p points to
Function NextLine( const p: PChar): PChar;

// Concatentate AddText to Text. Reallocate and expand
// Text if necessary. This is a size-safe StrCat
Procedure AddAndResize( Var Text: PChar;
                        const AddText: PChar );

// Copy Source to Dest. Reallocate and expand
// Dest if necessary. This is a size-safe StrCopy
Procedure StrCopyAndResize( Var Dest: PChar;
                            const Source: PChar );

// Return "True" or "False"
Function BoolToStr( const b: boolean ): string;

// Return true if param matches the form
// /Flag:value
// dash (-) can be used instead of slash (/)
// colon can be omitted
function MatchValueParam( const Param: string;
                          const Flag: string;
                          var Value: string ): boolean;

// Return true if param matches the form
// /Flag
// dash (-) can be used instead of slash (/)
function MatchFlagParam( const Param: string;
                         const Flag: string ): boolean;

Implementation

Uses
  SysUtils;//, ACLUtility;

// ---------------------- Pascal String Utilities ---------------------------------------

Procedure SkipChar( const S: string;
                    Var index: longint;
                    const C: Char );
begin
  while Index <= Length( S ) do
  begin
    if S[ Index ] <> C then
     break;
    inc( Index );
  end;
end;


Procedure GetNextQuotedValue(
           const S: string;
           var Index: longint;
           var Value: string;
           const QuoteChar: char );
begin
  Value := '';
  SkipChar( S, Index, ' ' );
  if Index > Length( S ) then
    exit;

  if S[ Index ] <> QuoteChar then
  begin
    // not quoted, go to next space
    while Index <= Length( S ) do
    begin
      if S[ Index ] = ' ' then
        break;
      Value := Value + S[ Index ];
      inc( Index );
    end;
    // skip following spaces
    SkipChar( S, Index, ' ' );
    exit;
  end;

  // quoted string
  inc( Index ); // skip opening quote

  while Index <= Length( S ) do
  begin
    if S[ Index ] = QuoteChar then
    begin
      inc( index ); // skip closing quote
      if Index > Length( S ) then
        break; // done
      if S[ Index ] <> QuoteChar then
        break; // done

      // escaped quote e.g "" so we do want it.
    end;
    Value := Value + S[ Index ];
    inc( Index );
  end;

  SkipChar( S, Index, ' ' );

end;

Function InsertDuplicateChars( const S: string;
                               const QuoteChar: char ): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length( S ) do
  begin
    Result := Result + S[ i ];
    if S[ i ] = QuoteChar then
      Result := Result + QuoteChar; // insert duplicate
  end;
end;

Function FindSubstring( const SubString: string;
                        const S: string ): integer;
begin
  Result := Pos( Uppercase( SubString ),
                 Uppercase( S ) );
end;

Function MatchLeadingChars(
           const S: string;
           MatchFunction: TCharMatchFunction ): string;
var
  i: integer;
  TheChar: char;
begin
  Result:= '';
  i := 1;
  while i <= Length( S ) do
  begin
    TheChar:= S[ i ];
    if not MatchFunction( TheChar ) then
      // found a non matching char. Stop looking
      break;
    Result:= Result + TheChar;
    inc( i );
  end;
end;

Function ExtractLeadingChars(
           Var S: string;
           MatchFunction: TCharMatchFunction ): string;
begin
  Result := MatchLeadingChars( s, MatchFunction );
  if Length( Result ) > 0 then
    // remove matching section from string
    Delete( S, 1, Length( Result ) );
end;

// Hex conversion: sheer extravagance. Conversion from
// a hex digit char to int is done by creating a lookup table
// in advance.
var
  MapHexDigitToInt: array[ Chr( 0 ) .. Chr( 255 ) ] of longint;

procedure InitHexDigitMap;
var
  c: char;
  IntValue: longint;
begin
  for c := Chr( 0 ) to Chr( 255 ) do
  begin
    IntValue := -1;
    if ( c >= '0' )
       and ( c <= '9' ) then
      IntValue := Ord( c ) - Ord( '0' );

    if ( Upcase( c ) >= 'A' )
       and ( Upcase( c ) <= 'F' ) then
      IntValue := 10 + Ord( Upcase( c ) ) - Ord( 'A' );

    MapHexDigitToInt[ c ] := IntValue;
  end;
end;

function HexDigitToInt( c: char ): longint;
begin
  Result := MapHexDigitToInt[ c ];
  if Result = -1 then
    raise EConvertError.Create( 'Invalid hex char: ' + c );
end;

function HexToInt( s: string ): longint;
var
  i: integer;
begin
  if Length( s ) = 0 then
    raise EConvertError.Create( 'No chars in hex string' );
  Result := 0;
  for i:= 1 to Length( s ) do
  begin
    Result := Result shl 4;
    inc( Result, HexDigitToInt( s[ i ] ) );
  end;
end;

Function StringsSame( const a, b: string ): boolean;
begin
  Result:= CompareText( a, b ) = 0;
end;

// Returns S in single quotes
Function StrQuote( const s: string ): string;
begin
  Result := Quote + s + Quote;
end;

// Returns S without double quotes
Function StrUnQuote( const s: string ): string;
begin
  Result := S;
  if S = '' then
    exit;

  if Result[ 1 ] = Quote then
    Delete( Result, 1, 1 );

  if Result = '' then
    exit;

  if Result[ Length( Result ) ] = Quote then
    Delete( Result, Length( Result ), 1 );
end;

Function StrDoubleQuote( const s: string ): string;
begin
  Result := DoubleQuote + s + DoubleQuote;
end;

Function StrFullDoubleQuote( const s: string ): string;
begin
  Result := DoubleQuote
            + InsertDuplicateChars( s, '"' )
            + DoubleQuote;
end;

// Returns S without double quotes
Function StrUnDoubleQuote( const s: string ): string;
begin
  Result := S;
  if S = '' then
    exit;

  if Result[ 1 ] = DoubleQuote then
    Delete( Result, 1, 1 );

  if Result = '' then
    exit;

  if Result[ Length( Result ) ] = DoubleQuote then
    Delete( Result, Length( Result ), 1 );
end;

Function SubstituteChar( const S: string; const Find: Char; const Replace: Char ): string;
Var
  i: longint;
Begin
  Result:= S;
  for i:=1 to length( S ) do
    if Result[ i ] = Find then
      Result[ i ]:= Replace;
End;

Function StrRight( const S:string; const count:integer ):string;
Begin
  if count>=length(s) then
  begin
    Result:=S;
  end
  else
  begin
    Result:=copy( S, length( s )-count+1, count );
  end;
end;

Function StrLeft( const S:string; const count:integer ):string;
Begin
  if count>=length(s) then
    Result:=S
  else
    Result:=copy( S, 1, count );
end;

// Returns S minus count characters from the right
Function StrLeftWithout( const S:string; const count:integer ):string;
Begin
  Result:= copy( S, 1, length( S )-count );
End;

Function StrRemoveEnds( const S:string; const leftCount:integer; const rightCount:integer ):string;
Begin
  Result:= S;
  Delete( Result, 1, leftCount );
  Delete( Result, length( S )-rightCount, rightCount );
End;

Function StrRightFrom( const S:string; const start:integer ):string;
Begin
  Result:= copy( S, start, length( S )-start+1 );
end;

Procedure ParseConfigLine( const S: string;
                           var keyName: string;
                           var keyValue: string );
Var
  line: String;
  EqualsPos: longint;
Begin
  KeyName:= '';
  KeyValue:= '';

  line:= trim( S );
  EqualsPos:= Pos( '=', line );

  if ( EqualsPos>0 ) then
  begin
    KeyName:= line;
    Delete( KeyName, EqualsPos, length( KeyName )-EqualsPos+1 );
    KeyName:= Trim( KeyName );

    KeyValue:= line;
    Delete( KeyValue, 1, EqualsPos );
    KeyValue:= Trim( KeyValue );
  end;
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

procedure GetNextValue( const S: String;
                        Var Index: longint;
                        Var Value: String;
                        const Seperator: Char );
var
  NextSeperatorPosition: longint;
  StringLen: longint;
begin
  Value := '';
  StringLen := Length( S );
  if Index > StringLen then
    exit;
  NextSeperatorPosition := Index;
  while NextSeperatorPosition < StringLen do
  begin
    if S[ NextSeperatorPosition ] = Seperator then
      break;
    inc( NextSeperatorPosition );
  end;

  if NextSeperatorPosition < StringLen then
  begin
    Value := Copy( S,
                   Index,
                   NextSeperatorPosition - Index );
    Index := NextSeperatorPosition + 1;
  end
  else
  begin
    Value := Copy( S,
                   Index,
                   StringLen - Index + 1 );
    Index := StringLen + 1;
  end;
  TrimRight( Value );
end;

Function IsDigit( const c: char ): boolean;
Begin
  Result:=( c>='0' ) and ( c<='9' );
End;

Function IsNonDigit( const c: char ): boolean;
Begin
  Result:=( c<'0' ) or ( c>'9' );
End;

Function IsAlpha( const c: char ): boolean;
var
  UppercaseC: char;
Begin
  UppercaseC := UpCase( c );
  Result := ( UppercaseC >= 'A' ) and ( UppercaseC <= 'Z' );
end;

{$ifdef os2}
// Returns true if s is only spaces (or empty)
Function IsSpaces( const s: string ): boolean;
Begin
  Asm
  MOV ESI,s   // get address of s into ESI
  MOV CL,[ESI]     // get length of s
  MOVZX ECX, CL      // widen CL
  INC ECX

!IsSpacesLoop:
  INC ESI   // move to next char
  DEC ECX
  JE !IsSpacesTrue

  MOV AL,[ESI] // load character
  CMP AL,32  // is it a space?
  JE !IsSpacesLoop // yes, go to next

  // no, return false
  MOV EAX, 0
  JMP !IsSpacesDone

!IsSpacesTrue:
  MOV EAX, 1

!IsSpacesDone:
  LEAVE
  RETN32 4
  End;

End;
{$else}
// Returns true if s is only spaces (or empty)
Function IsSpaces( const s: string ): boolean;
var
  i: longint;
Begin
  for i := 1 to length( s ) do
  begin
    if s[ i ] <> ' ' then
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;
{$endif}

Function StrLeft0Pad( const n: integer; const width: integer ): string;
Begin
  Result:= IntToStr( n );
  while length( Result )<width do
    Result:= '0' +Result;
End;

// Returns true if s starts with start
Function StrStarts( const start: string; const s: string ): boolean;
Var
  i: integer;
Begin
  Result:= false;
  if length( start ) > length( s ) then
    exit;
  for i:= 1 to length( start ) do
    if UpCase( s[ i ] ) <> UpCase( start[ i ] ) then
      exit;
  Result:= true;
End;

// Returns true if s ends with endstr (case insensitive)
Function StrEnds( const endStr: string; const s: string ): boolean;
Var
  i, j: integer;
Begin
  Result:= false;
  if Length( s ) < length( endStr ) then
    exit;
  j:= Length( s );
  for i:= length( endstr ) downto 1 do
  begin
    if UpCase( s[ j ] ) <> UpCase( endStr[ i ] ) then
      exit;
    dec( j );
  end;
  Result:= true;
End;

Procedure RemoveSeparatorSpaces( var S: string;
                                 const Separator:string );
Var
  SeparatorPos:integer;
  NewString: string;
Begin
  NewString := '';
  while S <> '' do
  begin
    SeparatorPos := pos( Separator, S );
    if SeparatorPos > 0 then
    begin
      NewString := NewString
                   + trim( copy( S, 1, SeparatorPos - 1 ) )
                   + Separator;
      Delete( S, 1, SeparatorPos );
    end
    else
    begin
      NewString := NewString + trim( S );
      S := '';
    end;
  end;
  S := NewString;
End;

Procedure AddToListString( Var S: string;
                           const NewValue: string;
                           const Separator: string );
Begin
  if trim( S )<>'' then
    S:=S+Separator;
  S:=S+NewValue;
End;

Function ListToString( List: TStrings;
                       const Separator: string ): string;
Var
  i: longint;
Begin
  Result:= '';
  for i:= 0 to List.Count - 1 do
    AddToListString( Result, List[ i ], Separator );
End;

procedure StringToList( S: String;
                        List: TStrings;
                        const Separator: string );
var
  Item: string;
begin
  List.Clear;
  while S <> '' do
  begin
    Item:= ExtractNextValue( S, Separator );
    List.Add( Item );
  end;
end;

Function StrFirstWord( const S: String ): string;
Var
  SpacePos: longint;
  temp: string;
Begin
  temp:= trimleft( S );
  SpacePos:= pos( ' ', temp );
  if SpacePos>0 then
    Result:= Copy( temp, 1, SpacePos-1 )
  else
    Result:= temp;
End;

Function IncrementNumberedString( StartString: string ): string;
Var
  Number: string;
  NewNumber: string;
  i: integer;
begin
  // Extract any digits at the end of the string
  i:= length( StartString );
  Number:= '';
  while i>0 do
  begin
    if isDigit( StartString[i] ) then
    begin
       Number:= StartString[i] + Number;
       i:= i - 1;
    end
    else
      break;
  end;

  if Number<>'' then
  begin
    // Found a numeric bit to play with
    // Copy the first part
    Result:= StrLeftWithout( StartString, length( Number ) );
    NewNumber:= StrLeft0Pad( StrToInt( Number ) + 1,
                               length( Number ) );
    Result:= Result + NewNumber;
  end
  else
    // No build number, add a 1
    Result:= StartString + '1';
end;

{$ifdef OS2}

Function AnsiTrim( const S: AnsiString ): AnsiString;
Var
  i: longint;
Begin
  i:= 1;
  while i<length( S) do
  begin
    if S[ i ]<>' ' then
      break;
    inc( i );
  end;
  Result:= S;
  if i>1 then
    AnsiDelete( Result, 1, i-1 );
  i:= length( Result );
  while i>=1 do
  begin
    if S[ i ]<>' ' then
      break;
    dec( i );
  end;
  AnsiSetLength( Result, i );
End;

Procedure AnsiParseConfigLine( const S: Ansistring;
                               var keyName: Ansistring;
                               var keyValue: Ansistring );
Var
  line: AnsiString;
  EqualsPos: longint;
Begin
  KeyName:= '';
  KeyValue:= '';

  line:= AnsiTrim( S );
  EqualsPos:= AnsiPos( '=', line );

  if ( EqualsPos>0 ) then
  begin
    KeyName:= AnsiCopy( line, 1, EqualsPos-1 );
    KeyName:= AnsiTrim( KeyName );

    KeyValue:= AnsiCopy( line, EqualsPos+1, length( line )-EqualsPos );
    KeyValue:= AnsiTrim( KeyValue );
  end;
end;

Function AnsiExtractNextValue( var CSVString: AnsiString;
                               const Separator: AnsiString ): AnsiString;
Var
  SeparatorPos: integer;
Begin
  SeparatorPos:= AnsiPos( Separator, CSVString );
  if SeparatorPos>0 then
  begin
    Result:= AnsiCopy( CSVString, 1, SeparatorPos-1 );
    AnsiDelete( CSVString, 1, SeparatorPos + length( Separator ) - 1 );
  end
  else
  begin
    Result:= CSVString;
    CSVString:= '';
  end;
  Result:= AnsiTrim( Result );
  // Remove qyotes if present
  if ( Result[1] = chr(34) )
     and ( Result[ length(Result) ] = chr(34) ) then
  begin
    AnsiDelete( Result, 1, 1 );
    AnsiDelete( Result, length( Result ), 1 );
    Result:= AnsiTrim( Result );
  end;
end;
{$Endif}

Procedure ReverseList( TheList:TStrings );
Var
  TempList: TStringList;
  i: integer;
Begin
  TempList:= TStringList.Create;
  for i:=TheList.count-1 downto 0 do
  begin
    TempList.AddObject( TheList.Strings[i],
                        TheList.Objects[i] );
  end;
  TheList.Assign( TempList );
  TempList.Destroy;
end;

Function FindStringInList( const TheString: string;
                           TheList:TStrings ): longint;
Var
  i: longint;
Begin
  for i:=0 to TheList.count-1 do
  begin
    if StringsSame( TheString, TheList[ i ] ) then
    begin
      // found
      Result:=i;
      exit;
    end;
  end;
  Result:=-1;
End;

Procedure MergeStringLists( Dest: TStringList;
                            AdditionalList: TStringList );
var
  i: integer;
  s: string;
begin
  for i:= 0 to AdditionalList.Count - 1 do
  begin
    s:= AdditionalList[ i ];
    if FindStringInList( s, Dest ) = -1 then
      Dest.AddObject( s, AdditionalList.Objects[ i ] );
  end;
end;

// ---------------------- PCHAR Utilities ---------------------------------------

function StrNPas( const Ps: PChar; const Length: integer ): String;
var
  i: integer;
begin
  Result:= '';
  i:= 0;
  while ( Ps[ i ] <> #0 ) and ( i < Length ) do
  begin
    Result:= Result + Ps[ i ];
    inc( i );
  end;
end;

Function PCharDiff( const a: PChar; const b: Pchar ): longword;
begin
  Result:= longword( a ) - longword( b );
end;

Procedure CheckPCharSize( Var Text: PChar;
                          const NeededSize: longword );
var
  temp: PChar;
  NewBufferSize: longword;
begin
  if   ( NeededSize + 1 ) // + 1 to allow for null terminator
     > StrBufSize( Text ) then
  begin
    // allocate new buffer, double the size...
    NewBufferSize:= StrBufSize( Text ) * 2;
    // or if that's not enough...
    if NewBufferSize < ( NeededSize + 1 ) then
      // double what we are going to need
      NewBufferSize:= NeededSize * 2;
    temp:= StrAlloc( NewBufferSize );

    // copy string to new buffer
    StrCopy( temp, Text );
    StrDispose( Text );
    Text:= temp;
  end;
end;

Procedure AddAndResize( Var Text: PChar;
                        const AddText: PChar );
begin
  CheckPCharSize( Text,
                  strlen( Text )
                  + strlen( AddText ) );
  StrCat( Text, AddText );
end;

Procedure StrCopyAndResize( Var Dest: PChar;
                            const Source: PChar );
begin
  CheckPCharSize( Dest, StrLen( Source ) );
  StrCopy( Dest, Source );
end;

// trims spaces and carriage returns of the end of Text
procedure TrimWhitespace( Text: PChar );
var
  P: PChar;
  IsWhitespace: boolean;
  TheChar: Char;
begin
  P:= Text + StrLen( Text );
  while P > Text do
  begin
    dec( P );
    TheChar:= P^;
    IsWhitespace:= TheChar in [ ' ', #13, #10, #9 ];
    if not IsWhiteSpace then
      // done
      break;
    P[ 0 ]:= #0; // Do no use P^ :=
  end;
end;

function TrimChars( const s: string;
                    chars: TSetOfChars ): string;
var
  i: longint;
  j: longint;
begin
  i := 1;
  while i < Length( s ) do
    if s[ i ] in chars then
      inc( i )
    else
      break;

  j := Length( s );
  while j > i do
    if s[ j ] in chars then
      dec( j )
    else
      break;

  result := Copy( s, i, j - i + 1 );
end;

procedure StrPCat( Var Dest: PChar;
                   const StringToAdd: string );
var
  Index: longint;
  DestP: PChar;
begin
  CheckPCharSize( Dest,
                  StrLen( Dest )
                  + longword( Length( StringToAdd ) ) );
  DestP:= Dest + StrLen( Dest );
  for Index:= 1 to Length( StringToAdd ) do
  begin
    DestP[ 0 ]:= StringToAdd[ Index ]; // do not use DestP^ :=
    inc( DestP );
  end;
  DestP[ 0 ]:= #0; // Do not use DestP^ := #0; Under Sibyl at least, this writes *** 2 NULL BYTES!!! ***
end;

Procedure TrimEndLines( const S: PChar );
var
  StringIndex: integer;
begin
  StringIndex:= strlen( S );
  while StringIndex > 0 do
  begin
    dec( StringIndex );
    if S[ StringIndex ] in [ #10, #13 ] then
    begin
      S[ StringIndex ]:= #0
    end
    else
      break;
  end;
end;

Function StrDupPas( const s: string ): PChar;
Begin
  Result:=StrAlloc( length( s )+1 );
  StrPCopy( Result, S );
//  Result^:=s;
End;

// Returns a copy of the first n chars of s
Function StrNDup( const s: PChar; const n: integer ): PChar;
Begin
  Result:= StrAlloc( n+1 );
  Result[ n ]:= '6';
  StrLCopy( Result, s, n );
End;

// Returns a copy of the first line starting at lineStart
Function CopyFirstLine( const lineStart: PChar ): PChar;
Var
  lineEnd: PChar;
  lineLength: integer;
Begin
  // look for an end of line
  lineEnd:= strpos( lineStart, EndLine );
  if lineEnd <> nil then
  begin
    // found, line length is difference between line end position and start of line
    lineLength:= longword( lineEnd )-longword( lineStart ); // ugly but how else can it be done?
    Result:= StrNDup( lineStart, lineLength );
    exit;
  end;

  // no eol found, return copy of remainder of string
  Result:= StrNew( lineStart );
end;

// Returns next line p points to
Function NextLine( const p: PChar): PChar;
Var
  lineEnd: PChar;
Begin
  // look for an end of line
  lineEnd:=strpos( p, EndLine );
  if lineEnd<>nil then
  begin
    // Advance the linestart over the eol
    Result:=lineEnd+length( EndLine );
    exit;
  end;

  // no eol found, return pointer to null term
  Result:=p+strlen( p );
end;

Function CaseInsensitivePos( const a: string; const b: string ): longint;
begin
  Result := Pos( UpperCase( a ), Uppercase( b ) );
end;


Function BoolToStr( const b: boolean ): string;
begin
  if b then
    Result := 'True'
  else
    Result := 'False';
end;

// Return true if param matches the form
// /Flag:value
// dash (-) can be used instead of slash (/)
// colon can be omitted
function MatchValueParam( const Param: string;
                          const Flag: string;
                          var Value: string ): boolean;
begin
  Result := false;

  if Param = '' then
    exit;

  if     ( Param[ 1 ] <> '/' )
     and ( Param[ 1 ] <> '-' ) then
    exit;

  if not StringsSame( Copy( Param, 2, Length( Flag ) ),
                      Flag ) then
    exit;

  Result := true;

  Value := StrRightFrom( Param, 2 + Length( Flag ) );
  if Value <> '' then
    if Value[ 1 ] = ':' then
      Delete( Value, 1, 1 );
end;

// Return true if param matches the form
// /Flag
// dash (-) can be used instead of slash (/)
function MatchFlagParam( const Param: string;
                         const Flag: string ): boolean;
begin
  Result := false;

  if Param = '' then
    exit;

  if     ( Param[ 1 ] <> '/' )
     and ( Param[ 1 ] <> '-' ) then
    exit;

  Result := StringsSame( StrRightFrom( Param, 2 ),
                         Flag );
end;

Initialization
  InitHexDigitMap;
End.
