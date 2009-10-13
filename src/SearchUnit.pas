Unit SearchUnit;

{$mode objfpc}{$H+}

// NewView - a new OS/2 Help Viewer
// Copyright 2001 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Contains code to search help files.

uses
  Classes,
  HelpFile, TextSearchQuery, IPFFileFormatUnit;

const
  // match weightings
  mwFirstTitleWord = 50;
  mwTitleWord = 20;
  mwFirstIndexWord = 20;
  mwIndexWord = 10;
  mwTopicTextWord = 1;

  // note on weightings. The title/index weightings
  // are multipled by word weightings.
  // Topic text matches are equal to word weighting
  // times word weighting.

type
  TSearchType = ( stStarts, stContains, stMatches );

  procedure SearchHelpFile( HelpFile: THelpFile;
                            Query: TTextSearchQuery;
                            Results: TList;
                            HighlightWords: UInt32ArrayPointer );


Implementation

uses
  SysUtils,
//  ACLUtility, ACLStringUtility,
  HelpTopic, CompareWordUnit, nvUtilities;

// Search the help file dictionary for words that match
// the given search word. Partial matches are considered.
// Results returns the matching word indexes.
// Relevances returns the relevance of the word stored
// at the same position
procedure SearchDictionary( HelpFile: THelpFile;
                            SearchWord: string;
                            Results: UInt32ArrayPointer );
var
  DictIndex: integer;
  DictWord: string;
  WordRelevance: longint;
begin
  SearchWord:= UpperCase( SearchWord );
  FillUInt32Array( Results, HelpFile.DictionaryCount, 0 );

  for DictIndex:= 0 to HelpFile.DictionaryCount - 1 do
  begin
    DictWord := HelpFile.DictionaryWords[ DictIndex ];
    WordRelevance := CompareWord( SearchWord, DictWord );
    Results^[ DictIndex ]:= WordRelevance;
  end;
end;

// Search titles of topics for given searchword
procedure SearchTopicTitles( HelpFile: THelpFile;
                             SearchWord: string;
                             Results: UInt32ArrayPointer );
var
  TopicIndex: longint;
  Title: string;
  TitleWord: string;
  Topic: TTopic;
  TitleWordIndex: longint;
  WordRelevance: longint;
  TitleWordRelevance: longint;
begin
  // Search topic titles
  for TopicIndex:= 0 to HelpFile.TopicCount - 1 do
  begin
    Topic:= HelpFile.Topics[ TopicIndex ];
    Title:= Topic.Title;
    TitleWordIndex := 0;
    while Title <> '' do
    begin
      TitleWord:= ExtractNextValue( Title, ' ' );
      WordRelevance := CompareWOrd( SearchWord, TitleWord );
      if WordRelevance > 0 then
      begin
        if TitleWordIndex = 0 then
          // matching the first word is best
          TitleWordRelevance := mwFirstTitleWord * WordRelevance
        else
          TitleWordRelevance := mwTitleWord * WordRelevance;
        inc( Results^[ TopicIndex ], TitleWordRelevance );
      end;
      inc( TitleWordIndex );
    end;
  end;
end;

// Search index entries for given searchword
procedure SearchIndex( HelpFile: THelpFile;
                       SearchWord: string;
                       Results: UInt32ArrayPointer );
var
  IndexIndex: longint;
  IndexEntry: string;
  IndexEntryWord: string;
  Topic: TTopic;
  IndexEntryWordIndex: longint;
  WordRelevance: longint;
  IndexEntryWordRelevance: longint;
begin
  for IndexIndex:= 0 to HelpFile.Index.Count - 1 do
  begin
    Topic:= HelpFile.Index.Objects[ IndexIndex ] as TTopic;
    IndexEntry:= HelpFile.Index[ IndexIndex ];
    IndexEntryWordIndex := 0;
    while IndexEntry <> '' do
    begin
      IndexEntryWord:= ExtractNextValue( IndexEntry, ' ' );
      WordRelevance := CompareWord( SearchWord, IndexEntryWord );
      if WordRelevance > 0 then
      begin
        if IndexEntryWordIndex = 0 then
          // matching the first word is best
          IndexEntryWordRelevance := mwFirstIndexWord * WordRelevance
        else
          IndexEntryWordRelevance := mwIndexWord * WordRelevance;
        inc( Results^[ Topic.Index ], IndexEntryWordRelevance );
      end;
      inc( IndexEntryWordIndex );
    end;
  end;
end;

// Utility function used in decompression of search table.
// Updates the appropriate entry in Results array.
// The word being matched is given in DictIndex and is
// used to count the actual occurrences of the word
// within the topic
{procedure AddTopicFoundInTopicText( TopicIndex: int16;
                                    Results: Int32ArrayPointer;
                                     DictIndex: longint;
                                              WordRelevance: longint );
var
  Topic: TTopic;
  Relevance: longint;
begin
  Topic:= _Topics[ TopicIndex ];
  Relevance := mwTopicTextWord
            * Topic.CountWord( DictIndex )
            * WordRelevance;
  inc( Results^[ TopicIndex ], Relevance );

end;}

// ------------------------------------------------------

// Master search function. Given a search query,
// searches topic text, titles, index entries.
// Matching topics are added to TList, with their
// SearchRelevance set appropriately.
procedure SearchHelpFile( HelpFile: THelpFile;
                          Query: TTextSearchQuery;
                          Results: TList;
                          HighlightWords: UInt32ArrayPointer );
var
  Topic: TTopic;
  TopicIndex: longint;
  TermIndex: longint;
  Term: TSearchTerm;
  TopicMatches: UInt32ArrayPointer;
  TopicRelevancesForTerm: UInt32ArrayPointer;
  TopicMatchedTerm: boolean;

  WordRelevance: longint;
  DictionaryRelevances: UInt32ArrayPointer;
  DictIndex: longint;
  TopicRelevanceForTerm: longint;
  TopicWordCount: longint;
begin
  // Reset flags per topic
  for TopicIndex := 0 to HelpFile.TopicCount - 1 do
  begin
    Topic := HelpFile.Topics[ TopicIndex ];
    Topic.FoundInSearch := false;
    Topic.ExcludedInSearch := false;
    Topic.SearchRelevance := 0;
  end;

  if HighlightWords <> nil then
    // Clear the highlightwords array
    FillUInt32Array( HighlightWords, HelpFile.DictionaryCount, 0 );

  // Get memory for dictionary/topic relevance arrays
  GetMem( DictionaryRelevances, HelpFile.DictionaryCount * sizeof( longint ) );
  GetMem( TopicMatches, HelpFile.TopicCount * sizeof( longint ) );
  GetMem( TopicRelevancesForTerm, HelpFile.TopicCount * sizeof( longint ) );

  for TermIndex := 0 to Query.TermCount - 1 do
  begin
    Term := Query.Term[ TermIndex ];

    FillUInt32Array( TopicRelevancesForTerm, HelpFile.TopicCount, 0 );

    // Search the dictionary for matches.
    SearchDictionary( HelpFile, Term.Text, DictionaryRelevances );

    // Update the highlight words array.
    // (effectively an OR)
    if HighlightWords <> nil then
    begin
      if Term.CombineMethod in [ cmAnd, cmOr ] then
      begin
        for DictIndex := 0 to HelpFile.DictionaryCount - 1 do
          inc( HighlightWords^[ DictIndex ],  DictionaryRelevances^[ DictIndex ] );
      end;
    end;

    // For each word in the dictionary that matches
    // this search word, search topics/titles/index
    for DictIndex := 0 to HelpFile.DictionaryCount - 1 do
    begin
      WordRelevance := DictionaryRelevances^[ DictIndex ];
      if WordRelevance > 0 then
      begin
        // Search for occurrences of this word
        // within the text of topics
        HelpFile.SearchTable.Search( DictIndex, TopicMatches );

        // Work out total relevance for each topic found:
        for TopicIndex := 0 to HelpFile.TopicCount - 1 do
        begin
          if TopicMatches^[ TopicIndex ] > 0 then
          begin
            // Search table indicates word occurs in
            // this topic, so count number of
            // occurrences to get relevance
            Topic := HelpFile.Topics[ TopicIndex ];

            TopicWordCount := Topic.CountWord( DictIndex );
            TopicRelevancesForTerm^[ TopicIndex ] := TopicWordCount * WordRelevance;
          end;
        end;
      end;
    end;

    // Search titles and index
    SearchTopicTitles( HelpFile, Term.Text, TopicRelevancesForTerm );
    SearchIndex( HelpFile, Term.Text, TopicRelevancesForTerm );

    // Set match flags for each topic, marking
    // as found or excluded depending on combine
    // method
    for TopicIndex := 0 to HelpFile.TopicCount - 1 do
    begin
      Topic := HelpFile.Topics[ TopicIndex ];
      TopicRelevanceForTerm := TopicRelevancesForTerm^[ TopicIndex ];
      TopicMatchedTerm := TopicRelevanceForTerm > 0;
      case Term.CombineMethod of
       cmAnd:
         if not TopicMatchedTerm then
           Topic.ExcludedInSearch := true
         else
           Topic.FoundInSearch := true;

       cmNot:
         if TopicMatchedTerm then
           Topic.ExcludedInSearch := true;

       cmOr:
         if TopicMatchedTerm then
           Topic.FoundInSearch := true;
      end;
      if TopicMatchedTerm then
        inc( Topic.SearchRelevance, TopicRelevanceForTerm );
    end;

    // loop for next word...
  end;

  // Now find topics that DID have a match
  // and did NOT have an exclusion match
  // ... add the topic to result list
  for TopicIndex := 0 to HelpFile.TopicCount - 1 do
  begin
    Topic := HelpFile.Topics[ TopicIndex ];
    if Topic.FoundInSearch
       and ( not Topic.ExcludedInSearch ) then
    begin
      Results.Add( Topic );
    end;
  end;

  FreeMem( TopicRelevancesForTerm, HelpFile.TopicCount * sizeof( longint ) );
  FreeMem( TopicMatches, HelpFile.TopicCount * sizeof( longint ) );
  FreeMem( DictionaryRelevances, HelpFile.DictionaryCount * sizeof( longint ) );
end;

function ExtractNextIPFWordPart( var Word: string ): string;
var
  CharIndex: longint;
begin
  assert( Length( Word ) > 0 );
  CharIndex := 2;
  if IsDigit( Word[ 1 ] ) then
  begin
    // extract string of digits
    while CharIndex <= Length( Word ) do
    begin
      if not IsDigit( Word[ CharIndex ] ) then
        break;
      inc( CharIndex );
    end;
  end
  else if IsAlpha( Word[ 1 ] ) then
  begin
    // extract string of letters
    while CharIndex <= Length( Word ) do
    begin
      if not IsAlpha( Word[ CharIndex ] ) then
        break;
      inc( CharIndex );
    end;
  end
  else
  begin
    // extract single non-alphanumeric symbol
  end;
  assert( CharIndex > 1 );
  Result := Copy(Word, 0, CharIndex-1);
//  Result := StrLeft( Word, CharIndex - 1 );
  Delete( Word, 1, CharIndex - 1 )
end;

Initialization
End.
