{
    fpGUI  -  Free Pascal GUI Library
    
    Container class declarations
    
    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pas}

{$IFDEF read_interface}

  { Container widget declarations }

  TFContainerWidget = class(TFWidget)
  protected
    function    DistributeEvent(Event: TEventObj): Boolean; override;
    function    GetChildCount: Integer; virtual; abstract;
    function    GetChild(Index: Integer): TFWidget; virtual; abstract;
    property    ChildCount: Integer read GetChildCount;
    property    Children[Index: Integer]: TFWidget read GetChild;
    // Move to public in decendant classes, if you want them visible
    procedure   InsertChild(AChild: TFWidget); dynamic; virtual;
    procedure   RemoveChild(AChild: TFWidget); dynamic; virtual;
  public
    function    GetChildAt(APoint: TPoint): TFWidget;
    function    ContainsChild(AChild: TFWidget): Boolean; dynamic; abstract;
  end;
  
{$ENDIF read_interface}



{$IFDEF read_implementation}

// ===================================================================
//   TContainerWidget
// ===================================================================

function TFContainerWidget.GetChildAt(APoint: TPoint): TFWidget;
var
  i: Integer;
begin
  for i := 0 to ChildCount - 1 do
  begin
    Result := Children[i];
    if PtInRect(Result.BoundsRect, APoint) then
      exit;
  end;
  Result := nil;
end;

function TFContainerWidget.DistributeEvent(Event: TEventObj): Boolean;
var
  i: Integer;
begin
  // Propagate the event to all children
  for i := 0 to ChildCount - 1 do
    if Event.SendToChild(Children[i]) then
    begin
      Result := True;
      exit;
    end;
  // The event hasn't been processed by any child:
  Result := inherited DistributeEvent(Event);
end;

procedure TFContainerWidget.InsertChild(AChild: TFWidget);
begin
  // do nothing
end;

procedure TFContainerWidget.RemoveChild(AChild: TFWidget);
begin
  // do nothing
end;


{$ENDIF read_implementation}

