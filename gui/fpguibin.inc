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

{%mainunit fpgui.pas}

{$IFDEF read_interface}

  { Bin widget declarations }
  TFBinWidget = class(TFContainerWidget)
  protected
    FChild: TFWidget;
    procedure   SetChild(AChild: TFWidget);
    function    GetChildCount: Integer; override;
    function    GetChild(Index: Integer): TFWidget; override;
    procedure   CalcSizes; override;
  public // !!!: temporarily
    property    Child: TFWidget read FChild write SetChild;
    // really public :)
    function    ContainsChild(AChild: TFWidget): Boolean; override;
    procedure   InsertChild(AChild: TFWidget); override;
    procedure   RemoveChild(AChild: TFWidget); override;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}



// ===================================================================
//   TFBinWidget
// ===================================================================

function TFBinWidget.ContainsChild(AChild: TFWidget): Boolean;
begin
  Result := Assigned(AChild) and (FChild = AChild);
end;


procedure TFBinWidget.InsertChild(AChild: TFWidget);
begin
  if FChild <> AChild then
  begin
    if Assigned(FChild) then
      FChild.Parent := nil;
    FChild := AChild;
    FChild.Parent := Self;
  end;
end;


procedure TFBinWidget.RemoveChild(AChild: TFWidget);
begin
  if FChild = AChild then
  begin
    FChild := nil;
    AChild.Parent := nil;
  end;
end;


function TFBinWidget.GetChildCount: Integer;
begin
  Result := Ord(Assigned(Child));
end;


function TFBinWidget.GetChild(Index: Integer): TFWidget;
begin
  if (Index = 0) and Assigned(Child) then
    Result := Child
  else
    TList.Error(SListIndexError, Index);
end;


procedure TFBinWidget.CalcSizes;
begin
  LAYOUTTRACE('TBinWidget.CalcSizes for %s:%s', [Name, ClassName]);
  if Assigned(Child) then
  begin
    FMinSize := Child.MinSize;
    FMaxSize := Child.MaxSize;
    FDefSize := Child.DefSize;
  end;
end;


procedure TFBinWidget.SetChild(AChild: TFWidget);
begin
  InsertChild(AChild);
end;


{$ENDIF read_implementation}

