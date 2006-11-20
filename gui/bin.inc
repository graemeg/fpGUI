{
    fpGUI  -  Free Pascal GUI Library
    
    Bin widget declarations
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pp}

{$IFDEF read_interface}

  { Bin widget declarations }
  TBinWidget = class(TContainerWidget)
  protected
    FChild: TWidget;
    procedure   SetChild(AChild: TWidget);
    function    GetChildCount: Integer; override;
    function    GetChild(Index: Integer): TWidget; override;
    procedure   CalcSizes; override;
  public // !!!: temporarily
    property    Child: TWidget read FChild write SetChild;
    // really public :)
    function    ContainsChild(AChild: TWidget): Boolean; override;
    procedure   InsertChild(AChild: TWidget); override;
    procedure   RemoveChild(AChild: TWidget); override;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}



// ===================================================================
//   TBinWidget
// ===================================================================

function TBinWidget.ContainsChild(AChild: TWidget): Boolean;
begin
  Result := Assigned(AChild) and (FChild = AChild);
end;


procedure TBinWidget.InsertChild(AChild: TWidget);
begin
  if FChild <> AChild then
  begin
    if Assigned(FChild) then
      FChild.Parent := nil;
    FChild := AChild;
    FChild.Parent := Self;
  end;
end;


procedure TBinWidget.RemoveChild(AChild: TWidget);
begin
  if FChild = AChild then
  begin
    FChild := nil;
    AChild.Parent := nil;
  end;
end;


function TBinWidget.GetChildCount: Integer;
begin
  Result := Ord(Assigned(Child));
end;


function TBinWidget.GetChild(Index: Integer): TWidget;
begin
  if (Index = 0) and Assigned(Child) then
    Result := Child
  else
    TList.Error(SListIndexError, Index);
end;


procedure TBinWidget.CalcSizes;
begin
  LAYOUTTRACE('TBinWidget.CalcSizes for %s:%s', [Name, ClassName]);
  if Assigned(Child) then
  begin
    FMinSize := Child.MinSize;
    FMaxSize := Child.MaxSize;
    FDefSize := Child.DefSize;
  end;
end;


procedure TBinWidget.SetChild(AChild: TWidget);
begin
  InsertChild(AChild);
end;


{$ENDIF read_implementation}

