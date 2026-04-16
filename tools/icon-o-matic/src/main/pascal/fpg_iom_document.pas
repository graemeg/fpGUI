{
    Copyright (c) 2026 Graeme Geldenhuys

    This program is part of the fpGUI Toolkit project.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Icon-O-Matic document model — class-based representation of an HVIF icon
      suitable for interactive editing.

      This unit is the single source of truth for the in-memory state of an
      icon being edited. It is intentionally decoupled from all UI concerns
      (no TfpgWidget, no canvas, no event routing). The UI observes changes
      via the TIomDocument.OnChange callback.

    Architecture overview:

      TIomDocument                  — root; owns all paths, styles, shapes
        TObjectList (TIomPath)
        TObjectList (TIomStyle)
        TObjectList (TIomShape)
        TUndoStack                  — command pattern; max 100 levels

      All edits go through TUndoStack.Execute(cmd). Calling Execute:
        1. Calls cmd.Execute (mutates the model)
        2. Pushes cmd onto the undo stack
        3. Fires TIomDocument.OnChange

      TIomCommand  (abstract)
        TIomCmdMoveNode             — drag one path point (main + both handles)
        TIomCmdMoveHandle           — drag one Bezier handle only
        TIomCmdSetStyleColour       — change a style's solid colour
        TIomCmdAddShape             — add a new shape (Undo removes it)
        TIomCmdDeleteShape          — delete a shape (Undo restores it)
        TIomCmdAddPath              — add a new path (Undo removes it)
        TIomCmdDeletePath           — delete a path (Undo restores it)
        TIomCmdSetShapeName         — rename any named object

    HVIF round-trip:

      Load:  binary stream → THvifIcon (reader)
             THvifIcon exposes raw arrays via accessor methods (see NOTE below)
             → TIomDocument.FromHvifArrays

      Save:  TIomDocument.ToHvifArrays → THvifStyles/Paths/Shapes arrays
             → fpg_hvif_writer → binary stream

      NOTE: THvifIcon currently stores FStyles/FPaths/FShapes as private fields.
      Loading requires one of:
        A) Adding public array accessor methods to THvifIcon (preferred)
        B) Extracting the binary parser into a separate unit with no fpGUI dep
      Option A is implemented: THvifIcon exposes Styles[], Paths[], Shapes[]
      as read-only indexed properties backed by private getter functions.

    Coordinate space:
      HVIF icons use a fixed 64×64 unit coordinate space (0.0..64.0 on each axis).
      All TIomPoint.X/Y values are in HVIF units. The canvas layer handles the
      mapping to screen pixels (zoom, pan, widget origin).

    Object naming:
      HVIF binary format does not store names. When loading, objects receive
      auto-generated names: path_0, path_1 ... style_0 ... shape_0 ...
      Names are unique within a document, not globally. The user may rename any
      object at any time (TIomCmdSetShapeName handles undo).

    Thread safety:
      NOT thread-safe. All access must occur on the fpGUI main thread.
      The editor is single-threaded (standard fpGUI event-driven model).
}

unit fpg_iom_document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Math,
  fpg_hvif_model,
  fpg_hvif,
  fpg_hvif_writer;


{ ==================== Forward declarations ==================== }

type
  TIomDocument = class;
  TIomPath     = class;
  TIomStyle    = class;
  TIomShape    = class;
  TIomCommand  = class;
  TUndoStack   = class;


{ ==================== Change notification ==================== }

type
  { Fired after every Execute, Undo, or Redo that mutates the document.
    ACommand is the command that was executed/undone/redone.
    UI components connect to TIomDocument.OnChange to refresh. }
  TIomChangeEvent = procedure(Sender: TIomDocument; ACommand: TIomCommand) of object;


{ ==================== Path point ==================== }

type
  { One node in a path. X/Y is the main anchor point.
    InX/InY is the incoming Bezier control handle (used for the curve segment
    ending at this point). OutX/OutY is the outgoing handle (curve starting
    here). For a straight-line node: InX=X, InY=Y, OutX=X, OutY=Y.
    Smooth=True means the two handles are kept collinear through the anchor. }
  TIomPoint = record
    X,    Y:    Single;
    InX,  InY:  Single;
    OutX, OutY: Single;
    Smooth: Boolean;
  end;


{ ==================== TIomPath ==================== }

type
  TIomPath = class
  private
    FName:   string;
    FClosed: Boolean;
    FPoints: array of TIomPoint;

    function  GetPointCount: Integer;
    function  GetPoint(AIndex: Integer): TIomPoint;
    procedure SetPoint(AIndex: Integer; const AValue: TIomPoint);

  public
    constructor Create(const AName: string);

    { Point access }
    property PointCount: Integer read GetPointCount;
    property Points[AIndex: Integer]: TIomPoint read GetPoint write SetPoint;

    { Append a new point; returns its index. }
    function AddPoint(const APt: TIomPoint): Integer;

    { Delete the point at AIndex; shifts higher indices down. }
    procedure DeletePoint(AIndex: Integer);

    { Insert APt at AIndex, shifting higher indices up.
      AIndex = PointCount is equivalent to AddPoint (append). }
    procedure InsertPoint(AIndex: Integer; const APt: TIomPoint);

    { True when this is a straight-lines-only path (all control handles equal
      their anchor). Used to choose the HVIF NO_CURVES encoding on save. }
    function IsLinearOnly: Boolean;

    { Convert to/from the HVIF record type for serialisation. }
    function  ToHvifPath: THvifPath;
    procedure FromHvifPath(const ASrc: THvifPath);

    property Name:   string  read FName   write FName;
    property Closed: Boolean read FClosed write FClosed;
  end;


{ ==================== Gradient stop ==================== }

type
  TIomGradientStop = record
    Offset: Single;       { normalised 0.0..1.0 }
    Color:  THvifColor;
  end;


{ ==================== TIomStyle ==================== }

type
  TIomStyle = class
  private
    FName:         string;
    FStyleType:    THvifStyleType;
    FColor:        THvifColor;
    FGradientType: THvifGradientType;
    FGradTransform: array[0..5] of Single;
    FStops:        array of TIomGradientStop;

    function  GetStopCount: Integer;
    function  GetStop(AIndex: Integer): TIomGradientStop;
    procedure SetStop(AIndex: Integer; const AValue: TIomGradientStop);

  public
    constructor Create(const AName: string);

    { Stop management (for gradient styles) }
    property StopCount: Integer read GetStopCount;
    property Stops[AIndex: Integer]: TIomGradientStop read GetStop write SetStop;
    function  AddStop(AOffset: Single; AColor: THvifColor): Integer;
    procedure DeleteStop(AIndex: Integer);

    { True if this style is a gradient kind. }
    function IsGradient: Boolean;

    { Convert to/from the HVIF record type for serialisation. }
    function  ToHvifStyle: THvifStyle;
    procedure FromHvifStyle(const ASrc: THvifStyle);

    property Name:          string           read FName         write FName;
    property StyleType:     THvifStyleType   read FStyleType    write FStyleType;
    property Color:         THvifColor       read FColor        write FColor;
    property GradientType:  THvifGradientType read FGradientType write FGradientType;

    { Gradient transform: 6-element affine matrix in AggPas order [sx,shy,shx,sy,tx,ty].
      Maps gradient parameter space → HVIF 64-unit icon space. }
    procedure GetGradTransform(out AMatrix: array of Single);
    procedure SetGradTransform(const AMatrix: array of Single);
  end;


{ ==================== Transformer ==================== }

type
  TIomTransformerType = (
    ittNone        = 0,
    ittStroke      = 1,
    ittContour     = 2,
    ittPerspective = 3
  );

  TIomTransformer = record
    TransType:        TIomTransformerType;
    { Stroke / Contour parameters }
    Width:            Single;   { in HVIF 64-unit space }
    MiterLimit:       Single;
    LineCap:          Byte;     { 0=butt, 1=square, 2=round }
    LineJoin:         Byte;     { 0=miter, 2=round, 3=bevel }
  end;


{ ==================== Level-of-Detail ==================== }

type
  { Controls visibility of a shape based on the rendered icon size.
    The shape is visible when: MinSize <= rendered_size_px <= MaxSize.
    Use 0 for MinSize and MaxSingle for MaxSize to mean "always visible". }
  TIomLOD = record
    MinSize: Single;   { minimum rendered size in pixels; 0 = no lower bound }
    MaxSize: Single;   { maximum rendered size in pixels; MaxSingle = no upper bound }
  end;


{ ==================== TIomShape ==================== }

type
  { A shape references one TIomStyle and one or more TIomPath instances.
    It does NOT own the style or paths — those are owned by TIomDocument.

    One shape can reference multiple paths (all rendered together using the
    same style). This is a key HVIF efficiency feature: one outline path
    shared by a fill shape and a stroke shape. }
  TIomShape = class
  private
    FName:         string;
    FStyle:        TIomStyle;       { reference into TIomDocument.Styles; not owned }
    FPaths:        TList;           { list of TIomPath references; not owned }
    FTransform:    array[0..5] of Single;
    FHasTransform: Boolean;
    FTranslateX:   Single;
    FTranslateY:   Single;
    FHasTranslation: Boolean;
    FLOD:          TIomLOD;
    FVisible:      Boolean;
    FTransformer:  TIomTransformer;

    function  GetPathCount: Integer;
    function  GetPath(AIndex: Integer): TIomPath;

  public
    constructor Create(const AName: string);
    destructor  Destroy; override;

    { Path references. These are references into TIomDocument.Paths; not owned. }
    property PathCount: Integer read GetPathCount;
    property Paths[AIndex: Integer]: TIomPath read GetPath;
    procedure AddPathRef(APath: TIomPath);
    procedure RemovePathRef(APath: TIomPath);
    function  IndexOfPath(APath: TIomPath): Integer;

    { Convert to/from the HVIF record type for serialisation.
      AStyleIndex and APathIndices must be provided by TIomDocument.ToHvifArrays
      since TIomShape holds object references, not numeric indices. }
    function ToHvifShape(AStyleIndex: Byte;
                         const APathIndices: array of Byte): THvifShape;

    property Name:           string           read FName            write FName;
    property Style:          TIomStyle        read FStyle           write FStyle;
    property HasTransform:   Boolean          read FHasTransform    write FHasTransform;
    property TranslateX:     Single           read FTranslateX      write FTranslateX;
    property TranslateY:     Single           read FTranslateY      write FTranslateY;
    property HasTranslation: Boolean          read FHasTranslation  write FHasTranslation;
    property LOD:            TIomLOD          read FLOD             write FLOD;
    property Visible:        Boolean          read FVisible         write FVisible;
    property Transformer:    TIomTransformer  read FTransformer     write FTransformer;

    procedure GetTransform(out AMatrix: array of Single);
    procedure SetTransform(const AMatrix: array of Single);
  end;


{ ==================== TIomCommand (abstract) ==================== }

type
  TIomCommand = class
  private
    FDescription: string;
  public
    { Description shown in Undo/Redo menu items (e.g. "Move node", "Set colour"). }
    property Description: string read FDescription write FDescription;

    { Execute the command, mutating the model forward. Called by TUndoStack.Execute. }
    procedure Execute; virtual; abstract;

    { Reverse the effect of Execute. Called by TUndoStack.Undo. }
    procedure Undo; virtual; abstract;

    { Re-apply after an Undo. Default implementation calls Execute; override only
      if re-application differs from the initial execution. }
    procedure Redo; virtual;
  end;


{ ==================== Concrete commands ==================== }

type
  { Move one path node (anchor point + both Bezier handles as a unit).
    Stores before/after snapshots of the full TIomPoint record. }
  TIomCmdMoveNode = class(TIomCommand)
  private
    FPath:      TIomPath;
    FNodeIndex: Integer;
    FOldPoint:  TIomPoint;
    FNewPoint:  TIomPoint;
  public
    constructor Create(APath: TIomPath; ANodeIndex: Integer;
                       const AOldPoint, ANewPoint: TIomPoint);
    procedure Execute; override;
    procedure Undo;    override;

    property Path:      TIomPath read FPath;
    property NodeIndex: Integer  read FNodeIndex;
  end;


  { Move one Bezier handle independently (InX/InY or OutX/OutY), without
    moving the anchor. Used when smooth=False allows asymmetric handles. }
  TIomCmdMoveHandle = class(TIomCommand)
  private
    FPath:      TIomPath;
    FNodeIndex: Integer;
    FIsInHandle: Boolean;   { True = incoming handle, False = outgoing }
    FOldPoint:  TIomPoint;
    FNewPoint:  TIomPoint;
  public
    constructor Create(APath: TIomPath; ANodeIndex: Integer; AIsInHandle: Boolean;
                       const AOldPoint, ANewPoint: TIomPoint);
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Change the solid colour of a TIomStyle. }
  TIomCmdSetStyleColour = class(TIomCommand)
  private
    FStyle:    TIomStyle;
    FOldColor: THvifColor;
    FNewColor: THvifColor;
  public
    constructor Create(AStyle: TIomStyle;
                       const AOldColor, ANewColor: THvifColor);
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Add a new TIomShape to the document. The command takes ownership of the
    shape until Execute is called; after Execute the document owns it.
    Undo transfers ownership back to the command. }
  TIomCmdAddShape = class(TIomCommand)
  private
    FDocument:    TIomDocument;
    FShape:       TIomShape;
    FInsertIndex: Integer;    { -1 = append }
    FOwnsShape:   Boolean;    { True when command holds ownership }
  public
    constructor Create(ADocument: TIomDocument; AShape: TIomShape;
                       AInsertIndex: Integer = -1);
    destructor  Destroy; override;
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Remove a TIomShape from the document. The document releases ownership on
    Execute; Undo returns it. }
  TIomCmdDeleteShape = class(TIomCommand)
  private
    FDocument:  TIomDocument;
    FShape:     TIomShape;
    FSavedIndex: Integer;
    FOwnsShape: Boolean;
  public
    constructor Create(ADocument: TIomDocument; AShape: TIomShape);
    destructor  Destroy; override;
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Add a new TIomPath to the document. }
  TIomCmdAddPath = class(TIomCommand)
  private
    FDocument:    TIomDocument;
    FPath:        TIomPath;
    FInsertIndex: Integer;
    FOwnsPath:    Boolean;
  public
    constructor Create(ADocument: TIomDocument; APath: TIomPath;
                       AInsertIndex: Integer = -1);
    destructor  Destroy; override;
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Remove a TIomPath from the document. Fails (raises) if any shape
    still references the path — caller must remove the reference first. }
  TIomCmdDeletePath = class(TIomCommand)
  private
    FDocument:   TIomDocument;
    FPath:       TIomPath;
    FSavedIndex: Integer;
    FOwnsPath:   Boolean;
  public
    constructor Create(ADocument: TIomDocument; APath: TIomPath);
    destructor  Destroy; override;
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Delete one path node at ANodeIndex. Stores the full TIomPoint for Undo.
    Will not execute if deleting would leave fewer than 2 nodes. }
  TIomCmdDeletePoint = class(TIomCommand)
  private
    FPath:       TIomPath;
    FNodeIndex:  Integer;
    FSavedPoint: TIomPoint;
  public
    constructor Create(APath: TIomPath; ANodeIndex: Integer);
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Insert one path node produced by splitting a bezier segment via de Casteljau.
    Stores the new node, the insert index, and the before/after snapshots of both
    adjacent nodes (their control handles change as a result of the split). }
  TIomCmdAddPoint = class(TIomCommand)
  private
    FPath:         TIomPath;
    FInsertIndex:  Integer;   { index at which the new node is inserted }
    FPrevNodeIdx:  Integer;   { index of the preceding node }
    FNextNodeIdx:  Integer;   { index of the following node (before insert) }
    FNewPoint:     TIomPoint;
    FPrevPtBefore: TIomPoint; { preceding node before split }
    FPrevPtAfter:  TIomPoint; { preceding node after split (OutHandle updated) }
    FNextPtBefore: TIomPoint; { following node before split }
    FNextPtAfter:  TIomPoint; { following node after split (InHandle updated) }
  public
    constructor Create(APath: TIomPath;
                       AInsertIndex, APrevNodeIdx, ANextNodeIdx: Integer;
                       const ANewPoint: TIomPoint;
                       const APrevPtBefore, APrevPtAfter: TIomPoint;
                       const ANextPtBefore, ANextPtAfter: TIomPoint);
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Rename any named document object. Works for TIomPath, TIomStyle, TIomShape
    by passing the object's Name field as a PString. }
  TIomCmdRename = class(TIomCommand)
  private
    FNameField: PString;    { pointer into the object's FName field }
    FOldName:   string;
    FNewName:   string;
  public
    { ANameField must point to a field that outlives this command.
      Callers should use @(obj.Name) — valid because Name is a property
      backed by FName; use @obj.FName via a dedicated constructor overload. }
    constructor Create(ANameField: PString;
                       const AOldName, ANewName: string);
    procedure Execute; override;
    procedure Undo;    override;
  end;


{ ==================== TUndoStack ==================== }

type
  TUndoStack = class
  private
    FStack:     TObjectList;   { owns TIomCommand instances }
    FCursor:    Integer;       { index of next undo position; -1 = nothing to undo }
    FMaxLevels: Integer;
    FOnChange:  TNotifyEvent;  { fired after every Execute, Undo, Redo }

    procedure TrimToMaxLevels;
    procedure NotifyChange;

  public
    constructor Create;
    destructor  Destroy; override;

    { Execute ACmd and push it onto the stack. Any previously undone commands
      above FCursor are discarded. ACmd ownership transfers to the stack. }
    procedure Execute(ACmd: TIomCommand);

    { Step one command back. No-op if CanUndo is False. }
    procedure Undo;

    { Re-apply one command forward. No-op if CanRedo is False. }
    procedure Redo;

    { Fired after every Execute, Undo or Redo. Connect to TIomDocument or the UI. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    function CanUndo: Boolean;
    function CanRedo: Boolean;

    { Description of the command that would be undone/redone next. }
    function UndoDescription: string;
    function RedoDescription: string;

    { Discard all commands. Called when loading a new document. }
    procedure Clear;

    { Maximum number of undo levels (default 100). }
    property MaxLevels: Integer read FMaxLevels write FMaxLevels;
  end;


{ ==================== TIomDocument ==================== }

type
  TIomDocument = class
  private
    FPaths:     TObjectList;   { owns TIomPath instances }
    FStyles:    TObjectList;   { owns TIomStyle instances }
    FShapes:    TObjectList;   { owns TIomShape instances }
    FUndoStack: TUndoStack;
    FDirty:     Boolean;
    FOnChange:  TIomChangeEvent;

    function  GetPathCount:  Integer;
    function  GetStyleCount: Integer;
    function  GetShapeCount: Integer;
    function  GetPath(AIndex: Integer):  TIomPath;
    function  GetStyle(AIndex: Integer): TIomStyle;
    function  GetShape(AIndex: Integer): TIomShape;

    procedure NotifyChange(ACmd: TIomCommand);
    procedure HandleUndoChange(Sender: TObject);

    { Generate a unique name within the document with the given prefix.
      E.g. UniqueName('path') → 'path_0', 'path_1', etc. }
    function UniqueName(const APrefix: string): string;

  public
    constructor Create;
    destructor  Destroy; override;

    { --- Object access --- }
    property PathCount:  Integer    read GetPathCount;
    property StyleCount: Integer    read GetStyleCount;
    property ShapeCount: Integer    read GetShapeCount;
    property Paths[AIndex: Integer]:  TIomPath  read GetPath;
    property Styles[AIndex: Integer]: TIomStyle read GetStyle;
    property Shapes[AIndex: Integer]: TIomShape read GetShape;

    { Index lookup by object reference (-1 if not found). }
    function IndexOfPath(APath:   TIomPath):  Integer;
    function IndexOfStyle(AStyle: TIomStyle): Integer;
    function IndexOfShape(AShape: TIomShape): Integer;

    { Index lookup by name (case-sensitive; -1 if not found). }
    function FindPathByName(const AName:  string): TIomPath;
    function FindStyleByName(const AName: string): TIomStyle;
    function FindShapeByName(const AName: string): TIomShape;

    { Check whether AName is already in use (across all object types). }
    function NameExists(const AName: string): Boolean;

    { --- Direct list manipulation (called by command Execute/Undo only) ---
      UI code must go through TUndoStack.Execute rather than calling these. }
    procedure InternalAddPath(APath: TIomPath; AIndex: Integer = -1);
    procedure InternalRemovePath(APath: TIomPath);
    procedure InternalAddStyle(AStyle: TIomStyle; AIndex: Integer = -1);
    procedure InternalRemoveStyle(AStyle: TIomStyle);
    procedure InternalAddShape(AShape: TIomShape; AIndex: Integer = -1);
    procedure InternalRemoveShape(AShape: TIomShape);

    { --- Undo/redo pass-through --- }
    property UndoStack: TUndoStack read FUndoStack;

    { --- Serialisation --- }

    { Populate this document from raw HVIF arrays.
      NOTE: THvifIcon must expose public accessors for its internal arrays before
      this can be wired to a real load operation. For now the signature is defined;
      the caller (the application's File > Open handler) will obtain the arrays
      from THvifIcon and pass them here.
      Auto-naming: styles get 'style_N', paths get 'path_N', shapes get 'shape_N'. }
    procedure FromHvifArrays(const AStyles: array of THvifStyle;
                              const APaths:  array of THvifPath;
                              const AShapes: array of THvifShape);

    { Serialise this document to a THvifWriter, ready for byte-stream output.
      Indices in THvifShape are resolved by looking up each object reference
      in the FStyles/FPaths lists. Caller is responsible for freeing the writer. }
    function BuildWriter: THvifWriter;

    { Convenience wrappers using TStream. }
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);

    { --- Document state --- }
    property Dirty: Boolean read FDirty write FDirty;

    { Reset Dirty and clear the undo stack (call after save). }
    procedure MarkClean;

    { Fire OnChange without creating an undo entry — use for live-preview
      mutations that are committed to the undo stack on gesture completion. }
    procedure NotifyChanged;

    { --- Change notification --- }
    property OnChange: TIomChangeEvent read FOnChange write FOnChange;
  end;


implementation


{ ==================== TIomCommand ==================== }

procedure TIomCommand.Redo;
begin
  Execute;
end;


{ ==================== TIomPath ==================== }

constructor TIomPath.Create(const AName: string);
begin
  inherited Create;
  FName   := AName;
  FClosed := False;
  SetLength(FPoints, 0);
end;

function TIomPath.GetPointCount: Integer;
begin
  Result := Length(FPoints);
end;

function TIomPath.GetPoint(AIndex: Integer): TIomPoint;
begin
  Result := FPoints[AIndex];
end;

procedure TIomPath.SetPoint(AIndex: Integer; const AValue: TIomPoint);
begin
  FPoints[AIndex] := AValue;
end;

function TIomPath.AddPoint(const APt: TIomPoint): Integer;
begin
  Result := Length(FPoints);
  SetLength(FPoints, Result + 1);
  FPoints[Result] := APt;
end;

procedure TIomPath.DeletePoint(AIndex: Integer);
var
  i: Integer;
begin
  for i := AIndex to Length(FPoints) - 2 do
    FPoints[i] := FPoints[i + 1];
  SetLength(FPoints, Length(FPoints) - 1);
end;

procedure TIomPath.InsertPoint(AIndex: Integer; const APt: TIomPoint);
var
  i, n: Integer;
begin
  n := Length(FPoints);
  SetLength(FPoints, n + 1);
  for i := n downto AIndex + 1 do
    FPoints[i] := FPoints[i - 1];
  FPoints[AIndex] := APt;
end;

function TIomPath.IsLinearOnly: Boolean;
var
  i: Integer;
begin
  for i := 0 to High(FPoints) do
    if (FPoints[i].InX  <> FPoints[i].X) or
       (FPoints[i].InY  <> FPoints[i].Y) or
       (FPoints[i].OutX <> FPoints[i].X) or
       (FPoints[i].OutY <> FPoints[i].Y) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

function TIomPath.ToHvifPath: THvifPath;
var
  i: Integer;
begin
  Result.Closed := FClosed;
  SetLength(Result.Points, Length(FPoints));
  for i := 0 to High(FPoints) do
  begin
    Result.Points[i].X    := FPoints[i].X;
    Result.Points[i].Y    := FPoints[i].Y;
    Result.Points[i].InX  := FPoints[i].InX;
    Result.Points[i].InY  := FPoints[i].InY;
    Result.Points[i].OutX := FPoints[i].OutX;
    Result.Points[i].OutY := FPoints[i].OutY;
  end;
end;

procedure TIomPath.FromHvifPath(const ASrc: THvifPath);
var
  i: Integer;
begin
  FClosed := ASrc.Closed;
  SetLength(FPoints, Length(ASrc.Points));
  for i := 0 to High(ASrc.Points) do
  begin
    FPoints[i].X      := ASrc.Points[i].X;
    FPoints[i].Y      := ASrc.Points[i].Y;
    FPoints[i].InX    := ASrc.Points[i].InX;
    FPoints[i].InY    := ASrc.Points[i].InY;
    FPoints[i].OutX   := ASrc.Points[i].OutX;
    FPoints[i].OutY   := ASrc.Points[i].OutY;
    FPoints[i].Smooth := False;   { not encoded in HVIF; default to sharp }
  end;
end;


{ ==================== TIomStyle ==================== }

constructor TIomStyle.Create(const AName: string);
begin
  inherited Create;
  FName      := AName;
  FStyleType := hstSolidColor;
  FColor.R   := 0;
  FColor.G   := 0;
  FColor.B   := 0;
  FColor.A   := 255;
  SetLength(FStops, 0);
end;

function TIomStyle.GetStopCount: Integer;
begin
  Result := Length(FStops);
end;

function TIomStyle.GetStop(AIndex: Integer): TIomGradientStop;
begin
  Result := FStops[AIndex];
end;

procedure TIomStyle.SetStop(AIndex: Integer; const AValue: TIomGradientStop);
begin
  FStops[AIndex] := AValue;
end;

function TIomStyle.AddStop(AOffset: Single; AColor: THvifColor): Integer;
begin
  Result := Length(FStops);
  SetLength(FStops, Result + 1);
  FStops[Result].Offset := AOffset;
  FStops[Result].Color  := AColor;
end;

procedure TIomStyle.DeleteStop(AIndex: Integer);
var
  i: Integer;
begin
  for i := AIndex to Length(FStops) - 2 do
    FStops[i] := FStops[i + 1];
  SetLength(FStops, Length(FStops) - 1);
end;

function TIomStyle.IsGradient: Boolean;
begin
  Result := FStyleType = hstGradient;
end;

function TIomStyle.ToHvifStyle: THvifStyle;
var
  i: Integer;
begin
  Result.StyleType    := FStyleType;
  Result.Color        := FColor;
  Result.GradientType := FGradientType;
  Move(FGradTransform[0], Result.GradTransform[0], SizeOf(FGradTransform));
  Result.HasGradTransform := not IsGradient; { populated only for gradients }
  SetLength(Result.Stops, Length(FStops));
  for i := 0 to High(FStops) do
  begin
    Result.Stops[i].Offset := FStops[i].Offset;
    Result.Stops[i].Color  := FStops[i].Color;
  end;
end;

procedure TIomStyle.FromHvifStyle(const ASrc: THvifStyle);
var
  i: Integer;
begin
  FStyleType    := ASrc.StyleType;
  FColor        := ASrc.Color;
  FGradientType := ASrc.GradientType;
  Move(ASrc.GradTransform[0], FGradTransform[0], SizeOf(FGradTransform));
  SetLength(FStops, Length(ASrc.Stops));
  for i := 0 to High(ASrc.Stops) do
  begin
    FStops[i].Offset := ASrc.Stops[i].Offset;
    FStops[i].Color  := ASrc.Stops[i].Color;
  end;
end;

procedure TIomStyle.GetGradTransform(out AMatrix: array of Single);
begin
  Move(FGradTransform[0], AMatrix[0], 6 * SizeOf(Single));
end;

procedure TIomStyle.SetGradTransform(const AMatrix: array of Single);
begin
  Move(AMatrix[0], FGradTransform[0], 6 * SizeOf(Single));
end;


{ ==================== TIomShape ==================== }

constructor TIomShape.Create(const AName: string);
var
  i: Integer;
begin
  inherited Create;
  FName           := AName;
  FStyle          := nil;
  FPaths          := TList.Create;
  FHasTransform   := False;
  FHasTranslation := False;
  FTranslateX     := 0;
  FTranslateY     := 0;
  FLOD.MinSize    := 0;
  FLOD.MaxSize    := MaxSingle;
  FVisible        := True;
  FTransformer.TransType := ittNone;
  FTransformer.Width     := 1.0;
  FTransformer.MiterLimit := 4.0;
  FTransformer.LineCap   := 0;
  FTransformer.LineJoin  := 0;
  for i := 0 to 5 do
    FTransform[i] := 0;
  { Identity affine: sx=1, shy=0, shx=0, sy=1, tx=0, ty=0 }
  FTransform[0] := 1.0;
  FTransform[3] := 1.0;
end;

destructor TIomShape.Destroy;
begin
  FPaths.Free;   { does not free the path objects — they are owned by TIomDocument }
  inherited;
end;

function TIomShape.GetPathCount: Integer;
begin
  Result := FPaths.Count;
end;

function TIomShape.GetPath(AIndex: Integer): TIomPath;
begin
  Result := TIomPath(FPaths[AIndex]);
end;

procedure TIomShape.AddPathRef(APath: TIomPath);
begin
  if FPaths.IndexOf(APath) < 0 then
    FPaths.Add(APath);
end;

procedure TIomShape.RemovePathRef(APath: TIomPath);
begin
  FPaths.Remove(APath);
end;

function TIomShape.IndexOfPath(APath: TIomPath): Integer;
begin
  Result := FPaths.IndexOf(APath);
end;

function TIomShape.ToHvifShape(AStyleIndex: Byte;
                                const APathIndices: array of Byte): THvifShape;
var
  i: Integer;
begin
  Result.StyleIndex := AStyleIndex;
  SetLength(Result.PathIndices, Length(APathIndices));
  for i := 0 to High(APathIndices) do
    Result.PathIndices[i] := APathIndices[i];
  Result.HasTransform   := FHasTransform;
  Move(FTransform[0], Result.Transform[0], SizeOf(FTransform));
  Result.HasTranslation := FHasTranslation;
  Result.TranslateX     := FTranslateX;
  Result.TranslateY     := FTranslateY;
  { Stroke transformer }
  Result.HasStroke        := FTransformer.TransType = ittStroke;
  Result.StrokeWidth      := FTransformer.Width;
  Result.StrokeLineCap    := FTransformer.LineCap;
  Result.StrokeLineJoin   := FTransformer.LineJoin;
  Result.StrokeMiterLimit := FTransformer.MiterLimit;
end;

procedure TIomShape.GetTransform(out AMatrix: array of Single);
begin
  Move(FTransform[0], AMatrix[0], 6 * SizeOf(Single));
end;

procedure TIomShape.SetTransform(const AMatrix: array of Single);
begin
  Move(AMatrix[0], FTransform[0], 6 * SizeOf(Single));
end;


{ ==================== Concrete commands ==================== }

{ TIomCmdMoveNode }

constructor TIomCmdMoveNode.Create(APath: TIomPath; ANodeIndex: Integer;
                                    const AOldPoint, ANewPoint: TIomPoint);
begin
  inherited Create;
  FPath      := APath;
  FNodeIndex := ANodeIndex;
  FOldPoint  := AOldPoint;
  FNewPoint  := ANewPoint;
  Description := 'Move node';
end;

procedure TIomCmdMoveNode.Execute;
begin
  FPath.Points[FNodeIndex] := FNewPoint;
end;

procedure TIomCmdMoveNode.Undo;
begin
  FPath.Points[FNodeIndex] := FOldPoint;
end;


{ TIomCmdMoveHandle }

constructor TIomCmdMoveHandle.Create(APath: TIomPath; ANodeIndex: Integer;
                                      AIsInHandle: Boolean;
                                      const AOldPoint, ANewPoint: TIomPoint);
begin
  inherited Create;
  FPath        := APath;
  FNodeIndex   := ANodeIndex;
  FIsInHandle  := AIsInHandle;
  FOldPoint    := AOldPoint;
  FNewPoint    := ANewPoint;
  if AIsInHandle then
    Description := 'Move incoming handle'
  else
    Description := 'Move outgoing handle';
end;

procedure TIomCmdMoveHandle.Execute;
begin
  FPath.Points[FNodeIndex] := FNewPoint;
end;

procedure TIomCmdMoveHandle.Undo;
begin
  FPath.Points[FNodeIndex] := FOldPoint;
end;


{ TIomCmdSetStyleColour }

constructor TIomCmdSetStyleColour.Create(AStyle: TIomStyle;
                                          const AOldColor, ANewColor: THvifColor);
begin
  inherited Create;
  FStyle    := AStyle;
  FOldColor := AOldColor;
  FNewColor := ANewColor;
  Description := 'Set colour';
end;

procedure TIomCmdSetStyleColour.Execute;
begin
  FStyle.Color := FNewColor;
end;

procedure TIomCmdSetStyleColour.Undo;
begin
  FStyle.Color := FOldColor;
end;


{ TIomCmdAddShape }

constructor TIomCmdAddShape.Create(ADocument: TIomDocument; AShape: TIomShape;
                                    AInsertIndex: Integer);
begin
  inherited Create;
  FDocument    := ADocument;
  FShape       := AShape;
  FInsertIndex := AInsertIndex;
  FOwnsShape   := True;
  Description  := 'Add shape';
end;

destructor TIomCmdAddShape.Destroy;
begin
  if FOwnsShape then
    FShape.Free;
  inherited;
end;

procedure TIomCmdAddShape.Execute;
begin
  FDocument.InternalAddShape(FShape, FInsertIndex);
  FOwnsShape := False;
end;

procedure TIomCmdAddShape.Undo;
begin
  FDocument.InternalRemoveShape(FShape);
  FOwnsShape := True;
end;


{ TIomCmdDeleteShape }

constructor TIomCmdDeleteShape.Create(ADocument: TIomDocument; AShape: TIomShape);
begin
  inherited Create;
  FDocument   := ADocument;
  FShape      := AShape;
  FSavedIndex := ADocument.IndexOfShape(AShape);
  FOwnsShape  := False;
  Description := 'Delete shape';
end;

destructor TIomCmdDeleteShape.Destroy;
begin
  if FOwnsShape then
    FShape.Free;
  inherited;
end;

procedure TIomCmdDeleteShape.Execute;
begin
  FDocument.InternalRemoveShape(FShape);
  FOwnsShape := True;
end;

procedure TIomCmdDeleteShape.Undo;
begin
  FDocument.InternalAddShape(FShape, FSavedIndex);
  FOwnsShape := False;
end;


{ TIomCmdAddPath }

constructor TIomCmdAddPath.Create(ADocument: TIomDocument; APath: TIomPath;
                                   AInsertIndex: Integer);
begin
  inherited Create;
  FDocument    := ADocument;
  FPath        := APath;
  FInsertIndex := AInsertIndex;
  FOwnsPath    := True;
  Description  := 'Add path';
end;

destructor TIomCmdAddPath.Destroy;
begin
  if FOwnsPath then
    FPath.Free;
  inherited;
end;

procedure TIomCmdAddPath.Execute;
begin
  FDocument.InternalAddPath(FPath, FInsertIndex);
  FOwnsPath := False;
end;

procedure TIomCmdAddPath.Undo;
begin
  FDocument.InternalRemovePath(FPath);
  FOwnsPath := True;
end;


{ TIomCmdDeletePath }

constructor TIomCmdDeletePath.Create(ADocument: TIomDocument; APath: TIomPath);
begin
  inherited Create;
  FDocument   := ADocument;
  FPath       := APath;
  FSavedIndex := ADocument.IndexOfPath(APath);
  FOwnsPath   := False;
  Description := 'Delete path';
end;

destructor TIomCmdDeletePath.Destroy;
begin
  if FOwnsPath then
    FPath.Free;
  inherited;
end;

procedure TIomCmdDeletePath.Execute;
begin
  FDocument.InternalRemovePath(FPath);
  FOwnsPath := True;
end;

procedure TIomCmdDeletePath.Undo;
begin
  FDocument.InternalAddPath(FPath, FSavedIndex);
  FOwnsPath := False;
end;


{ TIomCmdRename }

constructor TIomCmdRename.Create(ANameField: PString;
                                  const AOldName, ANewName: string);
begin
  inherited Create;
  FNameField  := ANameField;
  FOldName    := AOldName;
  FNewName    := ANewName;
  Description := Format('Rename to "%s"', [ANewName]);
end;

procedure TIomCmdRename.Execute;
begin
  FNameField^ := FNewName;
end;

procedure TIomCmdRename.Undo;
begin
  FNameField^ := FOldName;
end;


{ TIomCmdDeletePoint }

constructor TIomCmdDeletePoint.Create(APath: TIomPath; ANodeIndex: Integer);
begin
  inherited Create;
  FPath       := APath;
  FNodeIndex  := ANodeIndex;
  FSavedPoint := APath.Points[ANodeIndex];
  Description := 'Delete node';
end;

procedure TIomCmdDeletePoint.Execute;
begin
  FPath.DeletePoint(FNodeIndex);
end;

procedure TIomCmdDeletePoint.Undo;
begin
  FPath.InsertPoint(FNodeIndex, FSavedPoint);
end;


{ TIomCmdAddPoint }

constructor TIomCmdAddPoint.Create(APath: TIomPath;
    AInsertIndex, APrevNodeIdx, ANextNodeIdx: Integer;
    const ANewPoint: TIomPoint;
    const APrevPtBefore, APrevPtAfter: TIomPoint;
    const ANextPtBefore, ANextPtAfter: TIomPoint);
begin
  inherited Create;
  FPath         := APath;
  FInsertIndex  := AInsertIndex;
  FPrevNodeIdx  := APrevNodeIdx;
  FNextNodeIdx  := ANextNodeIdx;
  FNewPoint     := ANewPoint;
  FPrevPtBefore := APrevPtBefore;
  FPrevPtAfter  := APrevPtAfter;
  FNextPtBefore := ANextPtBefore;
  FNextPtAfter  := ANextPtAfter;
  Description   := 'Add node';
end;

procedure TIomCmdAddPoint.Execute;
begin
  { Update adjacent handles first, then insert the new node.
    The insert shifts FNextNodeIdx up by one, but we update it before that. }
  FPath.Points[FPrevNodeIdx] := FPrevPtAfter;
  FPath.Points[FNextNodeIdx] := FNextPtAfter;
  FPath.InsertPoint(FInsertIndex, FNewPoint);
end;

procedure TIomCmdAddPoint.Undo;
begin
  { Delete the new node first (restores FNextNodeIdx to its original index),
    then restore both adjacent node snapshots. }
  FPath.DeletePoint(FInsertIndex);
  FPath.Points[FPrevNodeIdx] := FPrevPtBefore;
  FPath.Points[FNextNodeIdx] := FNextPtBefore;
end;


{ ==================== TUndoStack ==================== }

constructor TUndoStack.Create;
begin
  inherited Create;
  FStack     := TObjectList.Create(True);  { owns commands }
  FCursor    := -1;
  FMaxLevels := 100;
end;

destructor TUndoStack.Destroy;
begin
  FStack.Free;
  inherited;
end;

procedure TUndoStack.NotifyChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TUndoStack.TrimToMaxLevels;
begin
  while FStack.Count > FMaxLevels do
  begin
    FStack.Delete(0);
    Dec(FCursor);
  end;
  if FCursor < -1 then
    FCursor := -1;
end;

procedure TUndoStack.Execute(ACmd: TIomCommand);
var
  i: Integer;
begin
  { Discard any redoable commands above the cursor }
  for i := FStack.Count - 1 downto FCursor + 1 do
    FStack.Delete(i);
  { Execute and push }
  ACmd.Execute;
  FStack.Add(ACmd);
  FCursor := FStack.Count - 1;
  TrimToMaxLevels;
  NotifyChange;
end;

procedure TUndoStack.Undo;
begin
  if not CanUndo then
    Exit;
  TIomCommand(FStack[FCursor]).Undo;
  Dec(FCursor);
  NotifyChange;
end;

procedure TUndoStack.Redo;
begin
  if not CanRedo then
    Exit;
  Inc(FCursor);
  TIomCommand(FStack[FCursor]).Redo;
  NotifyChange;
end;

function TUndoStack.CanUndo: Boolean;
begin
  Result := FCursor >= 0;
end;

function TUndoStack.CanRedo: Boolean;
begin
  Result := FCursor < FStack.Count - 1;
end;

function TUndoStack.UndoDescription: string;
begin
  if CanUndo then
    Result := TIomCommand(FStack[FCursor]).Description
  else
    Result := '';
end;

function TUndoStack.RedoDescription: string;
begin
  if CanRedo then
    Result := TIomCommand(FStack[FCursor + 1]).Description
  else
    Result := '';
end;

procedure TUndoStack.Clear;
begin
  FStack.Clear;
  FCursor := -1;
end;


{ ==================== TIomDocument ==================== }

constructor TIomDocument.Create;
begin
  inherited Create;
  FPaths     := TObjectList.Create(True);  { owns TIomPath }
  FStyles    := TObjectList.Create(True);  { owns TIomStyle }
  FShapes    := TObjectList.Create(True);  { owns TIomShape }
  FUndoStack := TUndoStack.Create;
  FUndoStack.OnChange := @HandleUndoChange;
  FDirty     := False;
  FOnChange  := nil;
end;

destructor TIomDocument.Destroy;
begin
  FUndoStack.Free;
  FShapes.Free;
  FStyles.Free;
  FPaths.Free;
  inherited;
end;

function TIomDocument.GetPathCount:  Integer; begin Result := FPaths.Count;  end;
function TIomDocument.GetStyleCount: Integer; begin Result := FStyles.Count; end;
function TIomDocument.GetShapeCount: Integer; begin Result := FShapes.Count; end;

function TIomDocument.GetPath(AIndex: Integer):  TIomPath;  begin Result := TIomPath(FPaths[AIndex]);   end;
function TIomDocument.GetStyle(AIndex: Integer): TIomStyle; begin Result := TIomStyle(FStyles[AIndex]); end;
function TIomDocument.GetShape(AIndex: Integer): TIomShape; begin Result := TIomShape(FShapes[AIndex]); end;

procedure TIomDocument.NotifyChange(ACmd: TIomCommand);
begin
  FDirty := True;
  if Assigned(FOnChange) then
    FOnChange(Self, ACmd);
end;

procedure TIomDocument.NotifyChanged;
begin
  NotifyChange(nil);
end;

procedure TIomDocument.HandleUndoChange(Sender: TObject);
begin
  NotifyChange(nil);
end;

function TIomDocument.UniqueName(const APrefix: string): string;
var
  n: Integer;
begin
  n := 0;
  repeat
    Result := Format('%s_%d', [APrefix, n]);
    Inc(n);
  until not NameExists(Result);
end;

function TIomDocument.IndexOfPath(APath: TIomPath):   Integer; begin Result := FPaths.IndexOf(APath);   end;
function TIomDocument.IndexOfStyle(AStyle: TIomStyle): Integer; begin Result := FStyles.IndexOf(AStyle); end;
function TIomDocument.IndexOfShape(AShape: TIomShape): Integer; begin Result := FShapes.IndexOf(AShape); end;

function TIomDocument.FindPathByName(const AName: string): TIomPath;
var
  i: Integer;
begin
  for i := 0 to FPaths.Count - 1 do
    if TIomPath(FPaths[i]).Name = AName then
      Exit(TIomPath(FPaths[i]));
  Result := nil;
end;

function TIomDocument.FindStyleByName(const AName: string): TIomStyle;
var
  i: Integer;
begin
  for i := 0 to FStyles.Count - 1 do
    if TIomStyle(FStyles[i]).Name = AName then
      Exit(TIomStyle(FStyles[i]));
  Result := nil;
end;

function TIomDocument.FindShapeByName(const AName: string): TIomShape;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    if TIomShape(FShapes[i]).Name = AName then
      Exit(TIomShape(FShapes[i]));
  Result := nil;
end;

function TIomDocument.NameExists(const AName: string): Boolean;
begin
  Result := Assigned(FindPathByName(AName))
         or Assigned(FindStyleByName(AName))
         or Assigned(FindShapeByName(AName));
end;

procedure TIomDocument.InternalAddPath(APath: TIomPath; AIndex: Integer);
begin
  if AIndex < 0 then
    FPaths.Add(APath)
  else
    FPaths.Insert(AIndex, APath);
end;

procedure TIomDocument.InternalRemovePath(APath: TIomPath);
begin
  FPaths.Extract(APath);   { removes without freeing }
end;

procedure TIomDocument.InternalAddStyle(AStyle: TIomStyle; AIndex: Integer);
begin
  if AIndex < 0 then
    FStyles.Add(AStyle)
  else
    FStyles.Insert(AIndex, AStyle);
end;

procedure TIomDocument.InternalRemoveStyle(AStyle: TIomStyle);
begin
  FStyles.Extract(AStyle);
end;

procedure TIomDocument.InternalAddShape(AShape: TIomShape; AIndex: Integer);
begin
  if AIndex < 0 then
    FShapes.Add(AShape)
  else
    FShapes.Insert(AIndex, AShape);
end;

procedure TIomDocument.InternalRemoveShape(AShape: TIomShape);
begin
  FShapes.Extract(AShape);
end;

procedure TIomDocument.FromHvifArrays(const AStyles: array of THvifStyle;
                                       const APaths:  array of THvifPath;
                                       const AShapes: array of THvifShape);
var
  i, j:   Integer;
  style:  TIomStyle;
  path:   TIomPath;
  shape:  TIomShape;
  pIdx:   Byte;
  xf:     TIomTransformer;
begin
  FUndoStack.Clear;
  FPaths.Clear;
  FStyles.Clear;
  FShapes.Clear;

  for i := 0 to High(AStyles) do
  begin
    style := TIomStyle.Create(UniqueName('style'));
    style.FromHvifStyle(AStyles[i]);
    FStyles.Add(style);
  end;

  for i := 0 to High(APaths) do
  begin
    path := TIomPath.Create(UniqueName('path'));
    path.FromHvifPath(APaths[i]);
    FPaths.Add(path);
  end;

  for i := 0 to High(AShapes) do
  begin
    shape := TIomShape.Create(UniqueName('shape'));
    { Resolve style index → object reference }
    if AShapes[i].StyleIndex < FStyles.Count then
      shape.Style := TIomStyle(FStyles[AShapes[i].StyleIndex]);
    { Resolve path indices → object references }
    for j := 0 to High(AShapes[i].PathIndices) do
    begin
      pIdx := AShapes[i].PathIndices[j];
      if pIdx < FPaths.Count then
        shape.AddPathRef(TIomPath(FPaths[pIdx]));
    end;
    { Copy transform, LOD, transformer from raw record }
    shape.HasTransform   := AShapes[i].HasTransform;
    shape.HasTranslation := AShapes[i].HasTranslation;
    shape.TranslateX     := AShapes[i].TranslateX;
    shape.TranslateY     := AShapes[i].TranslateY;
    if AShapes[i].HasStroke then
    begin
      xf := Default(TIomTransformer);
      xf.TransType  := ittStroke;
      xf.Width      := AShapes[i].StrokeWidth;
      xf.LineCap    := AShapes[i].StrokeLineCap;
      xf.LineJoin   := AShapes[i].StrokeLineJoin;
      xf.MiterLimit := AShapes[i].StrokeMiterLimit;
      shape.Transformer := xf;
    end;
    FShapes.Add(shape);
  end;

  FDirty := False;
end;

function TIomDocument.BuildWriter: THvifWriter;
var
  i, j:     Integer;
  shape:    TIomShape;
  pathIdxs: array of Byte;
  sIdx:     Integer;
  writer:   THvifWriter;
begin
  writer := THvifWriter.Create;
  try
    for i := 0 to FStyles.Count - 1 do
      writer.AddStyle(TIomStyle(FStyles[i]).ToHvifStyle);

    for i := 0 to FPaths.Count - 1 do
      writer.AddPath(TIomPath(FPaths[i]).ToHvifPath);

    for i := 0 to FShapes.Count - 1 do
    begin
      shape := TIomShape(FShapes[i]);
      sIdx  := FStyles.IndexOf(shape.Style);
      if sIdx < 0 then
        raise EHvifError.CreateFmt(
          'Shape "%s" references an unknown style', [shape.Name]);
      SetLength(pathIdxs, shape.PathCount);
      for j := 0 to shape.PathCount - 1 do
      begin
        pathIdxs[j] := FPaths.IndexOf(shape.Paths[j]);
        if Integer(pathIdxs[j]) < 0 then
          raise EHvifError.CreateFmt(
            'Shape "%s" references an unknown path', [shape.Name]);
      end;
      writer.AddShape(shape.ToHvifShape(sIdx, pathIdxs));
    end;
    Result := writer;
    writer := nil;  { transfer ownership to caller }
  finally
    writer.Free;    { frees only on exception (writer=nil after success) }
  end;
end;

procedure TIomDocument.LoadFromStream(AStream: TStream);
var
  icon:      THvifIcon;
  rawStyles: array of THvifStyle;
  rawPaths:  array of THvifPath;
  rawShapes: array of THvifShape;
  i:         Integer;
begin
  icon := THvifIcon.CreateFromStream(AStream);
  try
    SetLength(rawStyles, icon.StyleCount);
    for i := 0 to icon.StyleCount - 1 do
      rawStyles[i] := icon.Styles[i];

    SetLength(rawPaths, icon.PathCount);
    for i := 0 to icon.PathCount - 1 do
      rawPaths[i] := icon.Paths[i];

    SetLength(rawShapes, icon.ShapeCount);
    for i := 0 to icon.ShapeCount - 1 do
      rawShapes[i] := icon.Shapes[i];
  finally
    icon.Free;
  end;
  FromHvifArrays(rawStyles, rawPaths, rawShapes);
end;

procedure TIomDocument.SaveToStream(AStream: TStream);
var
  writer: THvifWriter;
begin
  writer := BuildWriter;
  try
    writer.SaveToStream(AStream);
  finally
    writer.Free;
  end;
end;

procedure TIomDocument.LoadFromFile(const AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TIomDocument.SaveToFile(const AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  MarkClean;
end;

procedure TIomDocument.MarkClean;
begin
  FDirty := False;
  FUndoStack.Clear;
end;


end.
