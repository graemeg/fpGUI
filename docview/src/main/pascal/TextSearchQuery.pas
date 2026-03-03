Unit TextSearchQuery;

{$mode objfpc}{$H+}

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Encapsulates a parsed search query.

uses
  Classes, SysUtils;

Type
  ESearchSyntaxError = class( Exception )
  end;

  TSearchTermCombineMethod =
  (
    cmOptional,
    cmRequired,
    cmExcluded
  );

  TSearchTerm = class(TObject)
  public
    Text: string;
    Parts: TStringList;
    CombineMethod: TSearchTermCombineMethod;
    constructor Create( const TheText: string;
                        const TheCombineMethod: TSearchTermCombineMethod );
    destructor Destroy; override;
  end;


  TTextSearchQuery = class(TObject)
  protected
    Terms: TList;
    function GetTerm( Index: longint ): TSearchTerm;
    function GetTermCount: longint;
  public
    constructor Create( const SearchString: string );
    destructor  Destroy; override;
    property    Term[ Index: longint ]: TSearchTerm read GetTerm;
    property    TermCount: longint read GetTermCount;
  end;


implementation

uses
  nvUtilities
  ,ACLStringUtility
  ;


const
  QueryErrorMissingWord1 = 'No search word given after';
  QueryErrorMissingWord2 = ' before ';


constructor TTextSearchQuery.Create( const SearchString: string );
var
  TermText: string;
  CombineMethod: TSearchTermCombineMethod;
  lTerm: TSearchTerm;
  tmpTerms : TStrings;
  i : integer;
begin
  inherited Create;
  Terms := TList.Create;
  try
    tmpTerms := TStringList.Create;
    StrExtractStringsQuoted(tmpTerms, SearchString);

    for i := 0 to tmpTerms.count-1 do
    begin
      TermText := tmpTerms[i];

      // Check for modifiers:
      //  + word must be matched
      //  - word must not be matched
      case TermText[ 1 ] of
       '+':
         CombineMethod := cmRequired;
       '-':
         CombineMethod := cmExcluded;
       else
         CombineMethod := cmOptional;
      end;

      if CombineMethod <> cmOptional then
      begin
        // delete + or -
        if Length( TermText ) = 1 then
          if (i < tmpTerms.count-1) then
            raise ESearchSyntaxError.Create( QueryErrorMissingWord1
                                             + StrInDoubleQuotes(TermText)
                                             + QueryErrorMissingWord2
                                             + StrInDoubleQuotes(tmpTerms[i+1]) )
          else
            raise ESearchSyntaxError.Create( QueryErrorMissingWord1
                                             + StrInDoubleQuotes(TermText));
        Delete( TermText, 1, 1 );
      end;

      lTerm := TSearchTerm.Create( TermText,
                                  CombineMethod );
      Terms.Add( lTerm );
    end;
    tmpTerms.Free;
  except
      while Terms.Count > 0 do
      begin
        lTerm := TSearchTerm(Terms.Last);
        Terms.Remove(lTerm);
        lTerm.Free;
      end;
      Terms.Free;
      raise; // reraise exception
  end;
end;

destructor TTextSearchQuery.Destroy;
begin
  DestroyListObjects( Terms );
  Terms.Free;
  inherited Destroy;
end;

function TTextSearchQuery.GetTerm( index: longint ): TSearchTerm;
begin
  Result := TSearchTerm(Terms[ Index ]);
end;

function TTextSearchQuery.GetTermCount: longint;
begin
  Result := Terms.Count;
end;

constructor TSearchTerm.Create( const TheText: string;
                                const TheCombineMethod: TSearchTermCombineMethod );
var
  TermParseIndex: longint;
  TermChar: char;
  TermPart: string;
begin
  Parts := TStringList.Create;

  Text := TheText;
  CombineMethod := TheCombineMethod;

  // Break out each part of the term as IPF does:
  // consecutive alphanumeric chars become a "word"
  // but each symbol is a separate word, and symbols break
  // up alphanumerics into multiple words. e.g.
  // CAKE_SAUSAGE becomes three words in IPF,
  // one each for "CAKE" "_" and "SAUSAGE"

  TermParseIndex := 1;
  while TermParseIndex <= Length( Text ) do
  begin
    // collect alphanumeric chars
    TermPart := '';
    while TermParseIndex <= Length( Text ) do
    begin
      TermChar := Text[ TermParseIndex ];
      if  (    IsAlpha( TermChar )
            or IsDigit( TermChar ) ) then
      begin
        // alpha numeric, collect it
        TermPart := TermPart + TermChar;
        inc( TermParseIndex );
      end
      else
      begin
        // not alpha numeric, so stop
        break;
      end;
    end;
    if Length( TermPart ) > 0 then
    begin
      Parts.Add( TermPart ); // add collected alphanumeric part
    end;

    if TermParseIndex <= Length( Text ) then
    begin
      // must be a symbol,
      // each symbol (excluding space) is an individual item
      if Text[ TermParseIndex ] <> ' ' then
        Parts.Add( Text[ TermParseIndex ] );
      inc( TermParseIndex );
    end;

  end;

end;

destructor TSearchTerm.Destroy;
begin
  Parts.Free;
  inherited Destroy;
end;


end.
