{
    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther
    Copyright (C) 2006 by Graeme Geldenhuys 
      member of the fpGUI development team.

    Container class declarations

    See the file COPYING.fpGUI, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{%mainunit fpgui.pp}

{$IFDEF read_interface}

  { Container widget declarations }

  TContainerWidget = class(TWidget)
  protected
    function    DistributeEvent(Event: TEventObj): Boolean; override;
    function    GetChildCount: Integer; virtual; abstract;
    function    GetChild(Index: Integer): TWidget; virtual; abstract;
    property    ChildCount: Integer read GetChildCount;
    property    Children[Index: Integer]: TWidget read GetChild;
    // Move to public in decendant classes, if you want them visible
    procedure   InsertChild(AChild: TWidget); dynamic; virtual;
    procedure   RemoveChild(AChild: TWidget); dynamic; virtual;
  public
    function    GetChildAt(APoint: TPoint): TWidget;
    function    ContainsChild(AChild: TWidget): Boolean; dynamic; abstract;
  end;
  
{$ENDIF read_interface}



{$IFDEF read_implementation}

// ===================================================================
//   TContainerWidget
// ===================================================================

function TContainerWidget.GetChildAt(APoint: TPoint): TWidget;
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

function TContainerWidget.DistributeEvent(Event: TEventObj): Boolean;
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

procedure TContainerWidget.InsertChild(AChild: TWidget);
begin
  // do nothing
end;

procedure TContainerWidget.RemoveChild(AChild: TWidget);
begin
  // do nothing
end;


{$ENDIF read_implementation}

