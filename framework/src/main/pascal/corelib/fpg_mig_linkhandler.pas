unit fpg_mig_linkhandler;

{
  MigLayout for fpGUI - Link Handler

  Port of LinkHandler.java from MigLayout v11.4.2

  Manages component links - allowing components to reference positions/sizes
  of other components by ID.

  License (BSD):
  ==============

  Copyright (c) 2004, Mikael Grev, MiG InfoCom AB. (miglayout (at) miginfocom (dot) com)
  Pascal port Copyright (c) 2025, Graeme Geldenhuys
  All rights reserved.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections;

const
  { Indices for accessing bounds array }
  LINK_X = 0;
  LINK_Y = 1;
  LINK_WIDTH = 2;
  LINK_HEIGHT = 3;
  LINK_X2 = 4;
  LINK_Y2 = 5;

  NOT_SET = -2147471302;  // LayoutUtil.NOT_SET from Java

type
  { Array type for storing bounds [x, y, width, height, x2, y2] }
  TfpgMigBoundsArray = array[0..5] of Integer;
  PfpgMigBoundsArray = ^TfpgMigBoundsArray;

  { Internal storage for layout values }
  TfpgMigLayoutValues = class
  public
    Values: specialize TDictionary<string, PfpgMigBoundsArray>;
    ValuesTemp: specialize TDictionary<string, PfpgMigBoundsArray>;

    constructor Create;
    destructor Destroy; override;
  end;

  { Static link handler class - manages component links across layouts }
  TfpgMigLinkHandler = class
  private
    class var FLayouts: specialize TDictionary<TObject, TfpgMigLayoutValues>;
    class var FLock: TCriticalSection;

    class procedure InitializeIfNeeded;
    class procedure Lock;
    class procedure Unlock;
  public
    class constructor Create;
    class destructor Destroy;

    { Gets a value for a linked component
      @param ALayout The layout manager instance
      @param AKey The link ID
      @param AType Type of value to get (LINK_X, LINK_Y, LINK_WIDTH, etc.)
      @returns The value or nil if not found }
    class function GetValue(ALayout: TObject; const AKey: string; AType: Integer): PInteger;

    { Sets bounds for a linked component
      @param ALayout The layout manager instance
      @param AKey The link ID
      @param AX X position
      @param AY Y position
      @param AWidth Width
      @param AHeight Height
      @returns True if the value was changed }
    class function SetBounds(ALayout: TObject; const AKey: string;
                            AX, AY, AWidth, AHeight: Integer): Boolean; overload;

    { Sets bounds for a linked component (internal version with extra options)
      @param ALayout The layout manager instance
      @param AKey The link ID
      @param AX X position
      @param AY Y position
      @param AWidth Width
      @param AHeight Height
      @param ATemporary If true, stores in temporary storage
      @param AIncCur If true, expands current bounds instead of replacing
      @returns True if the value was changed }
    class function SetBounds(ALayout: TObject; const AKey: string;
                            AX, AY, AWidth, AHeight: Integer;
                            ATemporary, AIncCur: Boolean): Boolean; overload;

    { Clears bounds for a specific link
      @param ALayout The layout manager instance
      @param AKey The link ID to clear
      @returns True if something was removed }
    class function ClearBounds(ALayout: TObject; const AKey: string): Boolean;

    { Clears temporary bounds for a layout
      @param ALayout The layout manager instance }
    class procedure ClearTemporaryBounds(ALayout: TObject);

    { Clears all layouts - useful for memory management }
    class procedure ClearAll;
  end;

implementation

uses
  Math;

{ TfpgMigLayoutValues }

constructor TfpgMigLayoutValues.Create;
begin
  inherited Create;
  Values := specialize TDictionary<string, PfpgMigBoundsArray>.Create;
  ValuesTemp := specialize TDictionary<string, PfpgMigBoundsArray>.Create;
end;

destructor TfpgMigLayoutValues.Destroy;
var
  arr: PfpgMigBoundsArray;
begin
  // Free all allocated arrays in Values
  for arr in Values.Values do
    Dispose(arr);

  // Free all allocated arrays in ValuesTemp
  for arr in ValuesTemp.Values do
    if not Values.ContainsValue(arr) then  // Don't double-free
      Dispose(arr);

  Values.Free;
  ValuesTemp.Free;
  inherited Destroy;
end;

{ TfpgMigLinkHandler }

class constructor TfpgMigLinkHandler.Create;
begin
  FLock := TCriticalSection.Create;
  FLayouts := specialize TDictionary<TObject, TfpgMigLayoutValues>.Create;
end;

class destructor TfpgMigLinkHandler.Destroy;
begin
  ClearAll;
  FLayouts.Free;
  FLock.Free;
end;

class procedure TfpgMigLinkHandler.InitializeIfNeeded;
begin
  if FLayouts = nil then
    FLayouts := specialize TDictionary<TObject, TfpgMigLayoutValues>.Create;
end;

class procedure TfpgMigLinkHandler.Lock;
begin
  FLock.Enter;
end;

class procedure TfpgMigLinkHandler.Unlock;
begin
  FLock.Leave;
end;

class function TfpgMigLinkHandler.GetValue(ALayout: TObject; const AKey: string;
  AType: Integer): PInteger;
var
  layoutValues: TfpgMigLayoutValues;
  rect: PfpgMigBoundsArray;
begin
  Result := nil;

  Lock;
  try
    if not FLayouts.TryGetValue(ALayout, layoutValues) then
      Exit;

    // Try temporary values first
    if layoutValues.ValuesTemp.TryGetValue(AKey, rect) then
    begin
      if rect^[AType] <> NOT_SET then
      begin
        New(Result);
        Result^ := rect^[AType];
        Exit;
      end;
    end;

    // Try permanent values
    if layoutValues.Values.TryGetValue(AKey, rect) then
    begin
      if rect^[AType] <> NOT_SET then
      begin
        New(Result);
        Result^ := rect^[AType];
      end;
    end;
  finally
    Unlock;
  end;
end;

class function TfpgMigLinkHandler.SetBounds(ALayout: TObject; const AKey: string;
  AX, AY, AWidth, AHeight: Integer): Boolean;
begin
  Result := SetBounds(ALayout, AKey, AX, AY, AWidth, AHeight, False, False);
end;

class function TfpgMigLinkHandler.SetBounds(ALayout: TObject; const AKey: string;
  AX, AY, AWidth, AHeight: Integer; ATemporary, AIncCur: Boolean): Boolean;
var
  layoutValues: TfpgMigLayoutValues;
  map: specialize TDictionary<string, PfpgMigBoundsArray>;
  old: PfpgMigBoundsArray;
  bounds: PfpgMigBoundsArray;
  x2, y2: Integer;
  changed: Boolean;
begin
  Result := False;

  Lock;
  try
    InitializeIfNeeded;

    if FLayouts.TryGetValue(ALayout, layoutValues) then
    begin
      if ATemporary then
        map := layoutValues.ValuesTemp
      else
        map := layoutValues.Values;

      if map.TryGetValue(AKey, old) then
      begin
        // Check if values changed
        if (old^[LINK_X] <> AX) or (old^[LINK_Y] <> AY) or
           (old^[LINK_WIDTH] <> AWidth) or (old^[LINK_HEIGHT] <> AHeight) then
        begin
          if not AIncCur then
          begin
            // Replace values
            old^[LINK_X] := AX;
            old^[LINK_Y] := AY;
            old^[LINK_WIDTH] := AWidth;
            old^[LINK_HEIGHT] := AHeight;
            old^[LINK_X2] := AX + AWidth;
            old^[LINK_Y2] := AY + AHeight;
            Result := True;
          end
          else
          begin
            // Expand current bounds
            changed := False;

            if AX <> NOT_SET then
            begin
              if (old^[LINK_X] = NOT_SET) or (AX < old^[LINK_X]) then
              begin
                old^[LINK_X] := AX;
                old^[LINK_WIDTH] := old^[LINK_X2] - AX;
                changed := True;
              end;

              if AWidth <> NOT_SET then
              begin
                x2 := AX + AWidth;
                if (old^[LINK_X2] = NOT_SET) or (x2 > old^[LINK_X2]) then
                begin
                  old^[LINK_X2] := x2;
                  old^[LINK_WIDTH] := x2 - old^[LINK_X];
                  changed := True;
                end;
              end;
            end;

            if AY <> NOT_SET then
            begin
              if (old^[LINK_Y] = NOT_SET) or (AY < old^[LINK_Y]) then
              begin
                old^[LINK_Y] := AY;
                old^[LINK_HEIGHT] := old^[LINK_Y2] - AY;
                changed := True;
              end;

              if AHeight <> NOT_SET then
              begin
                y2 := AY + AHeight;
                if (old^[LINK_Y2] = NOT_SET) or (y2 > old^[LINK_Y2]) then
                begin
                  old^[LINK_Y2] := y2;
                  old^[LINK_HEIGHT] := y2 - old^[LINK_Y];
                  changed := True;
                end;
              end;
            end;

            Result := changed;
          end;
        end;
      end
      else
      begin
        // Create new entry
        New(bounds);
        bounds^[LINK_X] := AX;
        bounds^[LINK_Y] := AY;
        bounds^[LINK_WIDTH] := AWidth;
        bounds^[LINK_HEIGHT] := AHeight;
        bounds^[LINK_X2] := AX + AWidth;
        bounds^[LINK_Y2] := AY + AHeight;
        map.Add(AKey, bounds);
        Result := True;
      end;
    end
    else
    begin
      // Create new layout entry
      layoutValues := TfpgMigLayoutValues.Create;

      New(bounds);
      bounds^[LINK_X] := AX;
      bounds^[LINK_Y] := AY;
      bounds^[LINK_WIDTH] := AWidth;
      bounds^[LINK_HEIGHT] := AHeight;
      bounds^[LINK_X2] := AX + AWidth;
      bounds^[LINK_Y2] := AY + AHeight;

      if ATemporary then
        layoutValues.ValuesTemp.Add(AKey, bounds)
      else
        layoutValues.Values.Add(AKey, bounds);

      FLayouts.Add(ALayout, layoutValues);
      Result := True;
    end;
  finally
    Unlock;
  end;
end;

class function TfpgMigLinkHandler.ClearBounds(ALayout: TObject; const AKey: string): Boolean;
var
  layoutValues: TfpgMigLayoutValues;
  old: PfpgMigBoundsArray;
begin
  Result := False;

  Lock;
  try
    if FLayouts.TryGetValue(ALayout, layoutValues) then
    begin
      if layoutValues.Values.TryGetValue(AKey, old) then
      begin
        Dispose(old);
        layoutValues.Values.Remove(AKey);
        Result := True;
      end;
    end;
  finally
    Unlock;
  end;
end;

class procedure TfpgMigLinkHandler.ClearTemporaryBounds(ALayout: TObject);
var
  layoutValues: TfpgMigLayoutValues;
  arr: PfpgMigBoundsArray;
begin
  Lock;
  try
    if FLayouts.TryGetValue(ALayout, layoutValues) then
    begin
      // Free all temporary arrays
      for arr in layoutValues.ValuesTemp.Values do
        if not layoutValues.Values.ContainsValue(arr) then  // Don't double-free
          Dispose(arr);
      layoutValues.ValuesTemp.Clear;
    end;
  finally
    Unlock;
  end;
end;

class procedure TfpgMigLinkHandler.ClearAll;
var
  lv: TfpgMigLayoutValues;
begin
  Lock;
  try
    for lv in FLayouts.Values do
      lv.Free;
    FLayouts.Clear;
  finally
    Unlock;
  end;
end;

end.
