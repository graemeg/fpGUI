Unit TextSearchQuery;

{$mode objfpc}{$H+}

// NewView - a new OS/2 Help Viewer
// Copyright 2001 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Encapsulates a parsed search query.

uses
  Classes, SysUtils;

Type
  ESearchSyntaxError = class( Exception )
  end;

  TSearchTermCombineMethod = ( cmOr, cmAnd, cmNot );

  TSearchTerm = class
    Text: string;
    CombineMethod: TSearchTermCombineMethod;
  end;

  TTextSearchQuery = class
  protected
    Terms: TList;
    function GetTerm( Index: longint ): TSearchTerm;
    function GetTermCount: longint;
  public
    constructor Create( SearchString: string );
    destructor Destroy; override;

    property Term[ Index: longint ]: TSearchTerm read GetTerm;
    property TermCount: longint read GetTermCount;
  end;

Implementation

uses
  nvUtilities;
//  ACLStringUtility, ACLUtility, Dialogs;

constructor TTextSearchQuery.Create( SearchString: string );
var
  SearchWord: string;
  RemainingSearchString: string;
  CombineMethod: TSearchTermCombineMethod;
  lTerm: TSearchTerm;
begin
  Terms := TList.Create;
  try
    RemainingSearchString := Uppercase( SearchString );
    while RemainingSearchString <> '' do
    begin
      SearchWord := ExtractNextValue( RemainingSearchString, ' ' );

      // Check for modifiers + (word must be matched)
      // and - (word must not be matched)
      case SearchWord[ 1 ] of
       '+':
         CombineMethod := cmAnd;
       '-':
         CombineMethod := cmNot;
       else
         CombineMethod := cmOr;
      end;
      if CombineMethod <> cmOr then
      begin
        // delete + or -
        if Length( SearchWord ) = 1 then
          raise ESearchSyntaxError.Create( 'No search word given after "'
                                           + SearchWord + '" before "'
                                           + RemainingSearchString
                                           + '"' );
        Delete( SearchWord, 1, 1 );
      end;

      lTerm := TSearchTerm.Create;
      lTerm.Text := SearchWord;
      lTerm.CombineMethod := CombineMethod;
      Terms.Add( lTerm );
    end;
  except
    Destroy; // clean up
    raise; // reraise exception
  end;
end;

destructor TTextSearchQuery.Destroy;
var
  i: TSearchTerm;
begin
  while Terms.Count > 0 do
  begin
    i := TSearchTerm(Terms.Last);
    Terms.Remove(i);
    i.Free;
  end;

//  DestroyListObjects( Terms );
  Terms.Destroy;
end;

function TTextSearchQuery.GetTerm( index: longint ): TSearchTerm;
begin
  Result := TSearchTerm(Terms[ Index ]);
end;

function TTextSearchQuery.GetTermCount: longint;
begin
  Result := Terms.Count;
end;

Initialization
End.
