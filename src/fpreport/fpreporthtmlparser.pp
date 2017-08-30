{
  See the section LICENSE/TERMS below for details about the copyright.

  TODO:
    - OnDone event when parser has finished
}
{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    FastHTMLParser unit to parse HTML
                  (disect html into its tags and text.)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 TITLE        : Fast HTML Parser (modified)
 CLASS        : TjsFastHTMLParser
 VERSION      : 0.5

 AUTHOR       : James Azarja
                http://www.jazarsoft.com/

 CONTRIBUTORS : L505
                http://z505.com

                Graeme Geldenhuys
                http://geldenhuys.co.uk

                YourName Here...


 LEGAL        : Copyright (C) 2004 Jazarsoft, All Rights Reserved.
                Modified 2005 Lars (L505)

--------------------------------------------------------------------------------

  - Modified for use as a pure command line unit (no dialogs) for freepascal.
  - Also added UPPERCASE tags so that when you check for <font> it returns all
    tags like <FONT> and <FoNt> and <font>

 Use it for what reasons:
    -make your own web browsers,
    -make your own text copies of web pages for caching purposes
    -Grab content from websites -without- using regular expressions
    -Seems to be MUCH MUCH FASTER than regular expressions, as it is after all
     a true parser
    -convert website tables into spreadsheets (parse <TD> and <TR>, turn in to
     CSV or similar)
    -convert websites into text files (parse all text, and tags <BR> <P> )
    -convert website tables into CSV/Database (<parse <TD> and <TR>)
    -find certain info from a web page.. i.e. all the bold text or hyperlinks in
     a page.
    -Parse websites remotely from a CGI app using something like Sockets or
     Synapse and SynWrap to first get the HTML site. This would allow you to
     dynamically parse info from websites and display data on your site in real
     time.
    -HTML editor.. WYSIWYG or a partial WYSIWYG editor. Ambitious, but possible.
    -HTML property editor. Not completely wysiwyg but ability to edit proprties
     of tags. Work would need to be done to parse each property in a tag.


--------------------------------------------------------------------------------
 LICENSE/TERMS
--------------------------------------------------------------------------------

 This code may be used and modified by anyone so long as  this header and
 copyright  information remains intact.

 The code is provided "AS-IS" and without WARRANTY OF ANY KIND,
 expressed, implied or otherwise, including and without limitation, any
 warranty of merchantability or fitness for a  particular purpose. 

 In no event shall the author be liable for any special, incidental,
 indirect or consequential damages whatsoever (including, without
 limitation, damages for loss of profits, business interruption, loss
 of information, or any other loss), whether or not advised of the
 possibility of damage, and on any theory of liability, arising out of
 or in connection with the use or inability to use this software.  


--------------------------------------------------------------------------------
 HISTORY:
--------------------------------------------------------------------------------

 0.1     -  James:
             Initial Development
             mostly based on Peter Irlam works & ideas

 0.2     -  James:
             Some minor bug has fixed

 0.3     -  James:
             Some jsHTMLUtil function bug has been fixed

 0.4     -  James:
             jsHTMLUtil Tag Attributes bug has been fixed
             thanks to Dmitry [mail@vader.ru]

 0.4L.1a -  L505:
             Made unit work with freepascal, added UPCASE (case insensitive)
             exec function

 0.4L.1b -  L505:
             Changed case insensitive version to a new class instead of
             the old ExecUpcase

 0.5     -  Graeme Geldenhuys <graeme@geldenhuys.co.uk>
             New functions to extract attibute details from the tags.
                                                                                                                                                          //
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

}

{$IFDEF FPC}{$MODE DELPHI}{$H+}{$ENDIF}


// {$DEFINE DEBUGLN_ON}

unit fpReportHTMLParser;


interface

uses
  SysUtils;


{$IFDEF DEBUGLN_ON}
  // dummy, default debugging
  procedure debugproc(s: string);
  // for custom debugging, assign this in your units 
  var debugln: procedure(s: string) = debugproc;
{$ENDIF}

type

  // when tag content found in HTML, including names and values
  // case insensitive analysis available via NoCaseTag
  TOnFoundTag = procedure(NoCaseTag, ActualTag: string) of object;

  // when text  found in the HTML
  TOnFoundText = procedure(Text: string) of object;

  // html parser, case insensitive or case sensitive
  THTMLParser = class(TObject)
    private
      FDone: Boolean;
      function CopyBuffer(StartIndex: PChar; Length: Integer): string;
    public
      OnFoundTag: TOnFoundTag;
      OnFoundText: TOnFoundText;
      Raw: Pchar;
      FCurrent : PChar;
      constructor Create(sRaw: string);overload;
      constructor Create(pRaw: PChar);overload;
      procedure Exec;
      procedure NilOnFoundTag(NoCaseTag, ActualTag: string);
      procedure NilOnFoundText(Text: string);
      function CurrentPos : Integer;
      property Done: Boolean read FDone write FDone;
    public
      {****  These are utility function - not used by the HTML parser **** }

      { return value of attrib, NAME case ignored, VALUE case preserved }
      function GetVal(const tag, attribname_ci: string): string;
      { Return tag name, case preserved }
      function GetTagName(const Tag: string): string;
      { Return name=value pair ignore case of NAME, preserve case of VALUE }
      function GetNameValPair(const tag, attribname_ci: string): string;
      { Get value of attribute, e.g WIDTH=36 returns 36, preserves case of value.
        The value is stripped from wrapped quotes. eg: bgcolor="black" returns 'black'. }
      function GetValFromNameVal(const namevalpair: string): string;
  end;


implementation


// default debugging, do nothing, let user do his own by assigning DebugLn var
procedure debugproc(s: string);
begin 
end;

{ THTMLParser }

function THTMLParser.CopyBuffer(StartIndex: PChar; Length: Integer): string;
begin
  SetLength(Result, Length);
  StrLCopy(@Result[1], StartIndex, Length);
end;

function THTMLParser.GetVal(const tag, attribname_ci: string): string;
var
  nameval: string;
begin
  result := '';
  if tag = '' then exit;
  if attribname_ci = '' then exit;
  // returns full name=value pair
  nameval := GetNameValPair(tag, attribname_ci);
  // extracts value portion only
  result := GetValFromNameVal(nameval);
end;

function THTMLParser.GetTagName(const tag: string): string;
var
  P: Pchar;
  S: Pchar;
begin
  P := Pchar(tag);
  while P^ in ['<',' ',#9] do inc(P);
  S := P;
  while Not (P^ in [' ','>',#0]) do inc(P);
  if P > S then
    Result := CopyBuffer( S, P-S);
end;

function THTMLParser.GetNameValPair(const tag, attribname_ci: string): string;
var
  P: Pchar;
  S: Pchar;
  UpperTag,
  UpperAttrib: string;
  Start: integer;
  L: integer;
  C: char;
begin
  result := '';
  if tag = '' then
    exit;
  if attribname_ci = '' then
    exit;
  // must be space before case insensitive NAME, i.e. <a HREF= STYLE=
  UpperAttrib := ' ' + upcase(attribname_ci);
  UpperTag := upcase(Tag);
  P := Pchar(UpperTag);
  S := StrPos(P, Pchar(UpperAttrib));

  if S <> nil then
  begin
    inc(S); // skip space
    P := S;

    // Skip until hopefully equal sign
    while not (P^ in ['=', ' ', '>', #0]) do
      inc(P);

    if (P^ = '=') then inc(P);

    while not (P^ in [' ','>',#0]) do
    begin
      if (P^ in ['"','''']) then
      begin
        C:= P^;
        inc(P); { Skip quote }
      end
      else
        C:= ' ';

      while not (P^ in [C, '>', #0]) do
        Inc(P);

      if (P^ <> '>') then inc(P); { Skip current character, except '>' }

      break;
    end;

    L := P - S;
    Start := S - Pchar(UpperTag);
    P := Pchar(Tag);
    S := P;
    inc(S, Start);

    { we use Trim() because non-ending Name/Value pairs have a trailing space }
    result := Trim(CopyBuffer(S, L));
  end;
end;

function THTMLParser.GetValFromNameVal(const namevalpair: string): string;
var
  P: Pchar;
  S: Pchar;
  C: Char;
begin
  result := '';
  if namevalpair = '' then exit;
  P := Pchar(namevalpair);
  S := StrPos(P, '=');

  if S <> nil then
  begin
    inc(S); // skip equal
    P := S;  // set P to a character after =

    if (P^ in ['"','''']) then
    begin
      C := P^;
      Inc(P); { Skip current character }
    end else
      C := ' ';

    S := P;
    while not (P^ in [C, #0]) do
      inc(P);

    if (P <> S) then
      Result := CopyBuffer(S, P - S)
    else
      Result := '';
  end;
end;

constructor THTMLParser.Create(pRaw: Pchar);
begin
  if pRaw = '' then exit;
  if pRaw = nil then exit;
  Raw:= pRaw;
end;

constructor THTMLParser.Create(sRaw: string);
begin
  if sRaw = '' then exit;
  Raw:= Pchar(sRaw);
end;

{ default dummy "do nothing" events if events are unassigned }
procedure THTMLParser.NilOnFoundTag(NoCaseTag, ActualTag: string);
begin 
end;

procedure THTMLParser.NilOnFoundText(Text: string);
begin 
end;

function THTMLParser.CurrentPos: Integer;
begin
  if Assigned(Raw) and Assigned(FCurrent) then
    Result:=FCurrent-Raw
  else
    Result:=0;
end;

procedure THTMLParser.Exec;
var
  L: Integer;
  TL: Integer;
  I: Integer;
  TagStart,
  TextStart,
  P: PChar;   // Pointer to current char.
  C: Char;
begin
  {$IFDEF DEBUGLN_ON}debugln('FastHtmlParser Exec Begin');{$ENDIF}
  { set nil events once rather than checking for nil each time tag is found }
  if not assigned(OnFoundText) then
    OnFoundText:= NilOnFoundText;
  if not assigned(OnFoundTag) then
    OnFoundTag:= NilOnFoundTag;

  TL:= StrLen(Raw);
  I:= 0;
  P:= Raw;
  Done:= False;
  if P <> nil then
  begin
    TagStart:= nil;
    repeat
      TextStart:= P;
      { Get next tag position }
      while Not (P^ in [ '<', #0 ]) do
      begin
        Inc(P); Inc(I);
        if I >= TL then
        begin
          Done:= True;
          Break;
        end;
      end;

      { Is there any text before ? }
      if (TextStart <> nil) and (P > TextStart) then
      begin
        L:= P - TextStart;
        { Yes, copy to buffer }
        FCurrent:=P;
        OnFoundText( CopyBuffer(TextStart, L) );
      end else
      begin
        TextStart:= nil;
      end;
      { No }

      if Done then Break;

      TagStart:= P;
      while Not (P^ in [ '>', #0]) do
      begin
        // Find string in tag
        if (P^ = '"') or (P^ = '''') then
        begin
          C:= P^;
          Inc(P); Inc(I); // Skip current char " or '

          // Skip until string end
          while Not (P^ in [C, #0]) do
          begin
            Inc(P);Inc(I);
          end;
        end;

        Inc(P);Inc(I);
        if I >= TL then
        begin
          Done:= True;
          Break;
        end;
      end;
      if Done then Break;

      { Copy this tag to buffer }
      L:= P - TagStart + 1;

      FCurrent:=P;
      OnFoundTag( uppercase(CopyBuffer(TagStart, L )), CopyBuffer(TagStart, L ) );
      Inc(P); Inc(I);
      if I >= TL then Break;
    until (Done);
  end;
  {$IFDEF DEBUGLN_ON}debugln('FastHtmlParser Exec End');{$ENDIF}
end;


end.




