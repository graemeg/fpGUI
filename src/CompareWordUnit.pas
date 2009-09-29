Unit CompareWordUnit;

{$mode objfpc}{$H+}

// NewView - a new OS/2 Help Viewer
// Copyright 2001 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Compares words and produces a match level (relevance) based
// on the relative sizes etc. Used in searching help files
// to sort by relevance.

const
  // word weightings
  mwExactWord = 20;
  mwWordStart = 10;
  mwWordWithin = 5;

// Compares the given search word against the given
// reference word. Returns a value indicating how well the
// search word matches, 0 = not at all.
function CompareWord( const SearchWord: string;
                      const ReferenceWord: string ): longint;

Implementation

uses
  SysUtils;

// LOoks for string a within string b, case insensitively
function CaseInsensitivePos( const a, b: string ): longint;
begin
  // Budget implementation to begin with.
  Result := Pos( UpperCase( a ), UpperCase( b ) );
end;

function CompareWord( const SearchWord: string;
                      const ReferenceWord: string ): longint;
var
  OccurrencePos: longint;
begin
  Result := 0;
  // First up, if the word we're searching for is longer than
  // this word, then it can't match at all.
  if Length( SearchWord ) > Length( ReferenceWord ) then
    exit;

  OccurrencePos := CaseInsensitivePos( SearchWord, ReferenceWord );
  if OccurrencePos = 0 then
    // no match.
    exit;

  if Length( SearchWord ) = Length( ReferenceWord ) then
  begin
    // exact word match (except case)
    Result := mwExactWord;
    exit;
  end;

  // partial word match
  if OccurrencePos = 1 then
  begin
    // word starts with searchword
    Result := mwWordStart
              * Length( SearchWord )
              div Length( ReferenceWord );
    if Result = 0 then
      Result := 1;
    exit;
  end;

  // Matched searchword somewhere within word
  Result := mwWordWithin
            * Length( SearchWord )
            div Length( ReferenceWord );
  if Result = 0 then
    Result := 1;

end;

{// Note: searchstring must be uppercase,
function IsMatching( const SearchString: string;
                     const SearchType: TSearchType;
                     const Item: string ): boolean;
var
  temp: string;
begin
  case SearchType of
    stStarts:
       Result:= StrStarts( SearchString, Item );

    stContains:
    begin
      temp:= UpperCase( Item );
      Result:= Pos( SearchString, temp ) <> 0;
    end;

    stMatches:
      Result:= CompareText( SearchString,
                            Item )= 0;
  end;
end;
}
Initialization
End.
