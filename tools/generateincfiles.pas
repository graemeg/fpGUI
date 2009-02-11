{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author:
      Graeme Geldenhuys

  Name:
      generateincfiles - generates include files from po files.

  Synopsis:
      generateincfiles fpgui_base_dir

  Description:
      generateincfiles will generate the lang_<langID>.inc files in the
      src/corelib/ directory. The include files are used when changing the
      default language of fpGUI.

}

program GenarateIncFiles;

{$mode objfpc}{$H+}

{$ifdef Windows}
  {$define CaseInsensitiveFilenames}
{$endif}


uses
  Classes, SysUtils, AvL_Tree, contnrs;
  
const
  UTF8FileHeader = #$ef#$bb#$bf;
  
type
  TMsgItem = record
    Comment: string;
    ID: string;
    Str: string;
  end;
  PMsgItem = ^TMsgItem;
  
  TMsgItemClass = class(TObject)
  private
    function GetIdentifier: string;
    function GetValue: string;
  public
    Comment: string;
    ID: string;
    Str: string;
    property Identifier: string read GetIdentifier;
    property Value: string read GetValue;
  end;

{ TMsgItemClass }

function TMsgItemClass.GetIdentifier: string;
var
  s: string;
  p: integer;
begin
  if Comment = '' then
    Result := ''
  else
  begin
    p := Pos(':', Comment);
    if p = 0 then
    begin
      Result := '';
      Exit;
    end
    else
    begin
      s := Copy(Comment, p+1, Length(Comment)-p);
      p := Pos(':', s);
      Result := Copy(s, p+1, Length(s)-p);
    end;
  end;
end;

function TMsgItemClass.GetValue: string;
begin
  if Str <> '' then
    Result := Str
  else
    Result := ID;
end;
  
function CompareMsgItems(Data1, Data2: pointer): integer;
var
  MsgItem1: PMsgItem;
  MsgItem2: PMsgItem;
begin
  MsgItem1:=PMsgItem(Data1);
  MsgItem2:=PMsgItem(Data2);
  Result:=CompareStr(MsgItem1^.ID,MsgItem2^.ID);
end;

procedure DisposeMsgTree(var Tree: TAVLTree);
var
  Node: TAVLTreeNode;
  MsgItem: PMsgItem;
begin
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    MsgItem:=PMsgItem(Node.Data);
    Dispose(MsgItem);
    Node:=Tree.FindSuccessor(Node);
  end;
  Tree.Free;
  Tree:=nil;
end;

function GetAllFilesMask: string;
begin
  {$IFDEF WINDOWS}
  Result:='*.*';
  {$ELSE}
  Result:='*';
  {$ENDIF}
end;

function CompareFilenames(const Filename1, Filename2: string): integer;
begin
  {$IFDEF CaseInsensitiveFilenames}
  Result:=AnsiCompareText(Filename1, Filename2);
  {$ELSE}
  Result:=CompareStr(Filename1, Filename2);
  {$ENDIF}
end;


type
  TPoFile = class
  public
    Tree: TAVLTree;
    Header: TStringList;
    Items: TObjectList;
    UTF8Header: string;
    constructor Create;
    destructor Destroy; override;
  end;

{ TPoFile }

constructor TPoFile.Create;
begin
  Tree:=TAVLTree.Create(@CompareMsgItems);
  Header:=TStringList.Create;
  Items := TObjectList.Create;
end;

destructor TPoFile.Destroy;
begin
  DisposeMsgTree(Tree);
  Header.Free;
  Items.Free;
  inherited Destroy;
end;

//==============================================================================
var
  Files: TStringList;
  Prefix: string;
  BaseDir: string;
  
const
  cLang = PathDelim + 'languages' + PathDelim;
  cCorelib = PathDelim + 'src' + PathDelim + 'corelib' + PathDelim;

procedure IncPrefix;
begin
  Prefix:=Prefix+'  ';
end;

procedure DecPrefix;
begin
  Prefix:=LeftStr(Prefix,length(Prefix)-2);
end;

function ParamsValid: boolean;
begin
  Result := false;
  if ParamCount < 1 then
    Exit; //==>
  BaseDir := ParamStr(1);
  if not DirectoryExists(BaseDir) then
  begin
    writeln('ERROR: fpGUI base directory <'+BaseDir+'> does not exist.');
    Exit; //==>
  end;
  // Does it look like the fpGUI base directory? We do three simple tests.
  if not FileExists(BaseDir + PathDelim + 'AUTHORS.txt') then
  begin
    writeln('ERROR: <'+BaseDir+'> directory does not look like the fpGUI base directory.');
    Exit; //==>
  end;
  if not DirectoryExists(BaseDir + cLang) then
  begin
    writeln('ERROR: <'+BaseDir+'> directory does not look like the fpGUI base directory.');
    writeln('       The ' + cLang + ' directory is missing.');
    Exit; //==>
  end;
  if not DirectoryExists(BaseDir + cCoreLib) then
  begin
    writeln('ERROR: <'+BaseDir+'> directory does not look like the fpGUI base directory.');
    writeln('       The ' + cCoreLib + ' directory is missing.');
    Exit; //==>
  end;
  Result := true;
end;

function ReadMessageItem(SrcFile: TStringList; var Line: integer): PMsgItem;
var
  s: string;
begin
  New(Result);
  while Line<SrcFile.Count do begin
    s:=SrcFile[Line];
    if (s<>'') and (s[1]='#') then begin
      Result^.Comment:=Result^.Comment+copy(s,2,length(s));
    end
    else if (LeftStr(s,7)='msgid "') then begin
      // read ID
      Result^.ID:=copy(s,8,length(s)-8);
      inc(Line);
      while Line<SrcFile.Count do begin
        s:=SrcFile[Line];
        if (s<>'') and (s[1]='"') then begin
          Result^.ID:=Result^.ID+#10+copy(s,2,length(s)-2);
          inc(Line);
        end else
          break;
      end;
      // read Str
      if Line<SrcFile.Count then begin
        s:=SrcFile[Line];
        if LeftStr(s,8)='msgstr "' then begin
          Result^.Str:=copy(s,9,length(s)-9);
          inc(Line);
          while Line<SrcFile.Count do begin
            s:=SrcFile[Line];
            if (s<>'') and (s[1]='"') then begin
              Result^.Str:=Result^.Str+#10+copy(s,2,length(s)-2);
              inc(Line);
            end else
              break;
          end;
        end;
      end;
      exit;
    end;
    inc(Line);
  end;
end;

function CreateMsgItemClass(MsgItem: PMsgItem): TMsgItemClass;
begin
  Result := nil;
  if MsgItem^.Comment[1] <> ':' then
    exit;
  Result := TMsgItemClass.Create;
  Result.Comment := MsgItem^.Comment;
//  writeln(Prefix, ' Comment: ', Result.Comment);
  Result.ID := MsgItem^.ID;
  Result.Str := MsgItem^.Str;
end;

procedure WriteMessageItem(MsgItem: PMsgItem; DestFile: TStringList);

  procedure WriteItem(const Prefix: string; Str: string);
  var
    s: String;
    p: Integer;
  begin
    s:=Prefix+' "';
    p:=1;
    while (p<=length(Str)) do begin
      if Str[p]=#10 then begin
        // a new line
        s:=s+copy(Str,1,p-1)+'"';
        DestFile.Add(s);
        Str:=copy(Str,p+1,length(Str));
        p:=1;
        // start new line
        s:='"';
      end else
        inc(p);
    end;
    if (Str<>'') or (s<>'"') then begin
      s:=s+Str+'"';
      DestFile.Add(s);
    end;
  end;

begin
  if MsgItem^.Comment<>'' then
    DestFile.Add('#'+MsgItem^.Comment);
  WriteItem('msgid',MsgItem^.ID);
  WriteItem('msgstr',MsgItem^.Str);
  DestFile.Add('');
end;

function ReadPoFile(const Filename: string): TPoFile;
var
  SrcFile: TStringList;
  MsgItem: PMsgItem;
  Line: Integer;
  oMsgItem: TMsgItemClass;
begin
  Result:=TPoFile.Create;

  // read source .po file
  //writeln(Prefix,'Loading ',Filename,' ...');
  SrcFile:=TStringList.Create;
  SrcFile.LoadFromFile(Filename);
  
  if (SrcFile.Count>0) and (copy(SrcFile[0],1,3)=UTF8FileHeader) then
  begin
    Result.UTF8Header:=copy(SrcFile[0],1,3);
    SrcFile[0]:=copy(SrcFile[0],4,length(SrcFile[0]));
  end;
  
  Line:=0;
  while Line<SrcFile.Count do
  begin
    if (SrcFile[Line]='') then
    begin
      // empty line
      inc(Line);
    end
    else
    begin
      // message
      MsgItem:=ReadMessageItem(SrcFile,Line);
      // ignore doubles
      if (Result.Tree.FindKey(MsgItem,@CompareMsgItems)<>nil) then
      begin
        Dispose(MsgItem);
        continue;
      end;
      // message class
      oMsgItem := CreateMsgItemClass(MsgItem);
      // add message
      Result.Tree.Add(MsgItem);
      if oMsgItem <> nil then
        Result.Items.Add(oMsgItem);
    end;
  end;

  SrcFile.Free;
end;

procedure WritePoFile(PoFile: TPoFile; const Filename: string);
var
  DestFile: TStringList;
  Node: TAVLTreeNode;
  MsgItem: PMsgItem;
  Save: Boolean;
  OldDestFile: TStringList;
begin
  //writeln(Prefix,'Saving ',Filename,' ...');
  DestFile:=TStringList.Create;
  if (PoFile.Header.Count>0) then begin
    DestFile.Add('msgid ""');
    DestFile.Add('msgstr ""');
    DestFile.AddStrings(PoFile.Header);
    DestFile.Add('');
  end;
  Node:=PoFile.Tree.FindLowest;
  while Node<>nil do begin
    MsgItem:=PMsgItem(Node.Data);
    WriteMessageItem(MsgItem,DestFile);
    Node:=PoFile.Tree.FindSuccessor(Node);
  end;
  if (PoFile.UTF8Header<>'') and (DestFile.Count>0) then
    DestFile[0]:=PoFile.UTF8Header+DestFile[0];
  Save:=true;
  if FileExists(Filename) then begin
    OldDestFile:=TStringList.Create;
    OldDestFile.LoadFromFile(Filename);
    if OldDestFile.Text=DestFile.Text then Save:=false;
    OldDestFile.Free;
  end;
  if Save then
    DestFile.SaveToFile(Filename);
  DestFile.Free;
end;

procedure WriteIncludeFile(PoFile: TPoFile; const Filename: string);
var
  DestFile: TStringList;
  i: integer;
  oMsg: TMsgItemClass;
begin
  DestFile := TStringList.Create;
  DestFile.Add('{%mainunit fpg_constants.pas}');
  DestFile.Add('');
  DestFile.Add('{ This file is auto generated!  DO NOT EDIT. }');
  DestFile.Add('{ Only exception is the default language English - lang_en.inc  }');
  DestFile.Add('');
  for i := 0 to PoFile.Items.Count - 1 do
  begin
    oMsg := TMsgItemClass(PoFile.Items[i]);
    DestFile.Add(Format('%s  =  %s;', [oMsg.Identifier, QuotedStr(oMsg.Value)]));
    { ***** Handle special cases ***** }

    // Long and Short month May has the same text so one gets lost. Add it back.
    if SameText(oMsg.Identifier, 'rsShortMay') then
      DestFile.Add(Format('%s  =  %s;', ['rsLongMay', QuotedStr(oMsg.Value)]));
  end;
  DestFile.SaveToFile(Filename);
  DestFile.Free;
end;

function FindAllTranslatedPoFiles(const ALangDir: string): TStringList;
var
  Path: String;
  NameOnly: String;
  FileInfo: TSearchRec;
  CurExt: String;
begin
  Result := TStringList.Create;
  Path := ALangDir;
  NameOnly := 'fpgui';
  if SysUtils.FindFirst(Path+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      or (CompareFilenames(FileInfo.Name, 'fpgui.po')=0) then continue;
      CurExt:=ExtractFileExt(FileInfo.Name);
      if (CompareFilenames(CurExt,'.po')<>0)
      or (CompareFilenames(LeftStr(FileInfo.Name,length(NameOnly)),NameOnly)<>0)
      then
        continue;
      Result.Add(Path+FileInfo.Name);
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
end;

procedure MergePoTrees(SrcTree, DestTree: TAVLTree);
var
  SrcNode, DestNode: TAVLTreeNode;
  SrcMsgItem, DestMsgItem: PMsgItem;
  OldNode: TAVLTreeNode;
begin
  // add all message items from SrcTree into DestTree
  SrcNode:=SrcTree.FindLowest;
  while SrcNode<>nil do begin
    SrcMsgItem:=PMsgItem(SrcNode.Data);
    DestNode:=DestTree.FindKey(SrcMsgItem,@CompareMsgItems);
    if DestNode<>nil then begin
      // ID already exists -> update comment
      DestMsgItem:=PMsgItem(DestNode.Data);
      DestMsgItem^.Comment:=SrcMsgItem^.Comment;
    end else begin
      // new ID -> add new message item to DestTree
      New(DestMsgItem);
      DestMsgItem^.Comment:=SrcMsgItem^.Comment;
      DestMsgItem^.ID:=SrcMsgItem^.ID;
      DestMsgItem^.Str:=SrcMsgItem^.Str;
      DestTree.Add(DestMsgItem);
    end;
    SrcNode:=SrcTree.FindSuccessor(SrcNode);
  end;
  // remove all old messages in DestTree
  DestNode:=DestTree.FindLowest;
  while DestNode<>nil do begin
    DestMsgItem:=PMsgItem(DestNode.Data);
    OldNode:=DestNode;
    DestNode:=DestTree.FindSuccessor(DestNode);
    if (DestMsgItem^.ID<>'')
    and (SrcTree.FindKey(DestMsgItem,@CompareMsgItems)=nil) then begin
      // unused message -> delete it
      writeln('Deleting unused message "',DestMsgItem^.ID,'"');
      Dispose(DestMsgItem);
      DestTree.Delete(OldNode);
    end;
  end;
end;

procedure ProcessPoFile(const Filename: string);
var
  SrcFile: TPoFile;

  function Newfile: string;
  var
    s: string;
    p: integer;
  begin
    s := ExtractFileName(Filename);
    p := Pos('.', s);
    s := Copy(s, p+1, Length(s));
    s := StringReplace(s, '.po', '.inc', [rfIgnoreCase]);
    Result := BaseDir + cCorelib + 'lang_' + s;
//    writeln('  Newfile: ', Result);
  end;
  
begin
  writeln('Loading ',Filename,' ...');
  SrcFile := ReadPoFile(Filename);
  WriteIncludeFile(SrcFile, Newfile);
  SrcFile.Free;
end;

procedure ProcessAllPoFiles;
var
  i: Integer;
begin
  Files := FindAllTranslatedPoFiles(BaseDir + cLang);
  for i := 0 to Files.Count-1 do
    ProcessPoFile(Files[i]);
end;

begin
  Prefix:='';
  Files:=nil;
  if not ParamsValid then
  begin
    writeln('Usage: ',ExtractFileName(ParamStr(0)), ' fpgui_base_dir');
    Exit;
  end
  else
  begin
    ProcessAllPoFiles;
  end;
  Files.Free;
end.

