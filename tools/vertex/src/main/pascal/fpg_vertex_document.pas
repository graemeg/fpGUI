{
    Copyright (c) 2026 Graeme Geldenhuys

    This program is part of the fpGUI Toolkit project.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Vertex document model — class-based representation of an HVIF icon
      suitable for interactive editing.

      This unit is the single source of truth for the in-memory state of an
      icon being edited. It is intentionally decoupled from all UI concerns
      (no TfpgWidget, no canvas, no event routing). The UI observes changes
      via the TVertexDocument.OnChange callback.

    Architecture overview:

      TVertexDocument               — root; owns all paths, styles, shapes
        TObjectList (TVertexPath)
        TObjectList (TVertexStyle)
        TObjectList (TVertexShape)
        TUndoStack                  — command pattern; max 100 levels

      All edits go through TUndoStack.Execute(cmd). Calling Execute:
        1. Calls cmd.Execute (mutates the model)
        2. Pushes cmd onto the undo stack
        3. Fires TVertexDocument.OnChange

      TVertexCommand  (abstract)
        TVertexCmdMoveNode          — drag one path point (main + both handles)
        TVertexCmdMoveHandle        — drag one Bezier handle only
        TVertexCmdSetStyleColour    — change a style's solid colour
        TVertexCmdAddShape          — add a new shape (Undo removes it)
        TVertexCmdDeleteShape       — delete a shape (Undo restores it)
        TVertexCmdAddPath           — add a new path (Undo removes it)
        TVertexCmdDeletePath        — delete a path (Undo restores it)
        TVertexCmdRename            — rename any named object

    HVIF round-trip:

      Load:  binary stream → THvifIcon (reader)
             THvifIcon exposes raw arrays via accessor methods (see NOTE below)
             → TVertexDocument.FromHvifArrays

      Save:  TVertexDocument.ToHvifArrays → THvifStyles/Paths/Shapes arrays
             → fpg_hvif_writer → binary stream

      NOTE: THvifIcon currently stores FStyles/FPaths/FShapes as private fields.
      Loading requires one of:
        A) Adding public array accessor methods to THvifIcon (preferred)
        B) Extracting the binary parser into a separate unit with no fpGUI dep
      Option A is implemented: THvifIcon exposes Styles[], Paths[], Shapes[]
      as read-only indexed properties backed by private getter functions.

    Coordinate space:
      HVIF icons use a fixed 64×64 unit coordinate space (0.0..64.0 on each axis).
      All TVertexPoint.X/Y values are in HVIF units. The canvas layer handles the
      mapping to screen pixels (zoom, pan, widget origin).

    Object naming:
      HVIF binary format does not store names. When loading, objects receive
      auto-generated names: path_0, path_1 ... style_0 ... shape_0 ...
      Names are unique within a document, not globally. The user may rename any
      object at any time (TVertexCmdRename handles undo).

    Thread safety:
      NOT thread-safe. All access must occur on the fpGUI main thread.
      The editor is single-threaded (standard fpGUI event-driven model).
}

unit fpg_vertex_document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Math,
  fpg_hvif_model,
  fpg_hvif,
  fpg_hvif_writer;


{ ==================== Forward declarations ==================== }

type
  TVertexDocument = class;
  TVertexPath     = class;
  TVertexStyle    = class;
  TVertexShape    = class;
  TVertexCommand  = class;
  TUndoStack   = class;


{ ==================== Change notification ==================== }

type
  { Fired after every Execute, Undo, or Redo that mutates the document.
    ACommand is the command that was executed/undone/redone.
    UI components connect to TVertexDocument.OnChange to refresh. }
  TVertexChangeEvent = procedure(Sender: TVertexDocument; ACommand: TVertexCommand) of object;


{ ==================== Path point ==================== }

type
  { One node in a path. X/Y is the main anchor point.
    InX/InY is the incoming Bezier control handle (used for the curve segment
    ending at this point). OutX/OutY is the outgoing handle (curve starting
    here). For a straight-line node: InX=X, InY=Y, OutX=X, OutY=Y.
    Smooth=True means the two handles are kept collinear through the anchor. }
  TVertexPoint = record
    X,    Y:    Single;
    InX,  InY:  Single;
    OutX, OutY: Single;
    Smooth: Boolean;
  end;


{ ==================== TVertexPath ==================== }

type
  TVertexPath = class
  private
    FName:   string;
    FClosed: Boolean;
    FPoints: array of TVertexPoint;

    function  GetPointCount: Integer;
    function  GetPoint(AIndex: Integer): TVertexPoint;
    procedure SetPoint(AIndex: Integer; const AValue: TVertexPoint);

  public
    constructor Create(const AName: string);

    { Point access }
    property PointCount: Integer read GetPointCount;
    property Points[AIndex: Integer]: TVertexPoint read GetPoint write SetPoint;

    { Append a new point; returns its index. }
    function AddPoint(const APt: TVertexPoint): Integer;

    { Delete the point at AIndex; shifts higher indices down. }
    procedure DeletePoint(AIndex: Integer);

    { Insert APt at AIndex, shifting higher indices up.
      AIndex = PointCount is equivalent to AddPoint (append). }
    procedure InsertPoint(AIndex: Integer; const APt: TVertexPoint);

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
  TVertexGradientStop = record
    Offset: Single;       { normalised 0.0..1.0 }
    Color:  THvifColor;
  end;


{ ==================== TVertexStyle ==================== }

type
  TVertexStyle = class
  private
    FName:         string;
    FStyleType:    THvifStyleType;
    FColor:        THvifColor;
    FGradientType: THvifGradientType;
    FGradTransform: array[0..5] of Single;
    FStops:        array of TVertexGradientStop;

    function  GetStopCount: Integer;
    function  GetStop(AIndex: Integer): TVertexGradientStop;
    procedure SetStop(AIndex: Integer; const AValue: TVertexGradientStop);

  public
    constructor Create(const AName: string);

    { Stop management (for gradient styles) }
    property StopCount: Integer read GetStopCount;
    property Stops[AIndex: Integer]: TVertexGradientStop read GetStop write SetStop;
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
  TVertexTransformerType = (
    ittNone        = 0,
    ittStroke      = 1,
    ittContour     = 2,
    ittPerspective = 3
  );

  TVertexTransformer = record
    TransType:        TVertexTransformerType;
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
  TVertexLOD = record
    MinSize: Single;   { minimum rendered size in pixels; 0 = no lower bound }
    MaxSize: Single;   { maximum rendered size in pixels; MaxSingle = no upper bound }
  end;


{ ==================== TVertexShape ==================== }

type
  { A shape references one TVertexStyle and one or more TVertexPath instances.
    It does NOT own the style or paths — those are owned by TVertexDocument.

    One shape can reference multiple paths (all rendered together using the
    same style). This is a key HVIF efficiency feature: one outline path
    shared by a fill shape and a stroke shape. }
  TVertexShape = class
  private
    FName:         string;
    FStyle:        TVertexStyle;       { reference into TVertexDocument.Styles; not owned }
    FPaths:        TList;           { list of TVertexPath references; not owned }
    FTransform:    array[0..5] of Single;
    FHasTransform: Boolean;
    FTranslateX:   Single;
    FTranslateY:   Single;
    FHasTranslation: Boolean;
    FLOD:          TVertexLOD;
    FVisible:      Boolean;
    FTransformer:  TVertexTransformer;

    function  GetPathCount: Integer;
    function  GetPath(AIndex: Integer): TVertexPath;

  public
    constructor Create(const AName: string);
    destructor  Destroy; override;

    { Path references. These are references into TVertexDocument.Paths; not owned. }
    property PathCount: Integer read GetPathCount;
    property Paths[AIndex: Integer]: TVertexPath read GetPath;
    procedure AddPathRef(APath: TVertexPath);
    procedure RemovePathRef(APath: TVertexPath);
    function  IndexOfPath(APath: TVertexPath): Integer;

    { Convert to/from the HVIF record type for serialisation.
      AStyleIndex and APathIndices must be provided by TVertexDocument.ToHvifArrays
      since TVertexShape holds object references, not numeric indices. }
    function ToHvifShape(AStyleIndex: Byte;
                         const APathIndices: array of Byte): THvifShape;

    property Name:           string           read FName            write FName;
    property Style:          TVertexStyle        read FStyle           write FStyle;
    property HasTransform:   Boolean          read FHasTransform    write FHasTransform;
    property TranslateX:     Single           read FTranslateX      write FTranslateX;
    property TranslateY:     Single           read FTranslateY      write FTranslateY;
    property HasTranslation: Boolean          read FHasTranslation  write FHasTranslation;
    property LOD:            TVertexLOD          read FLOD             write FLOD;
    property Visible:        Boolean          read FVisible         write FVisible;
    property Transformer:    TVertexTransformer  read FTransformer     write FTransformer;

    procedure GetTransform(out AMatrix: array of Single);
    procedure SetTransform(const AMatrix: array of Single);
  end;


{ ==================== TVertexCommand (abstract) ==================== }

type
  TVertexCommand = class
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
    Stores before/after snapshots of the full TVertexPoint record. }
  TVertexCmdMoveNode = class(TVertexCommand)
  private
    FPath:      TVertexPath;
    FNodeIndex: Integer;
    FOldPoint:  TVertexPoint;
    FNewPoint:  TVertexPoint;
  public
    constructor Create(APath: TVertexPath; ANodeIndex: Integer;
                       const AOldPoint, ANewPoint: TVertexPoint);
    procedure Execute; override;
    procedure Undo;    override;

    property Path:      TVertexPath read FPath;
    property NodeIndex: Integer  read FNodeIndex;
  end;


  { Move one Bezier handle independently (InX/InY or OutX/OutY), without
    moving the anchor. Used when smooth=False allows asymmetric handles. }
  TVertexCmdMoveHandle = class(TVertexCommand)
  private
    FPath:      TVertexPath;
    FNodeIndex: Integer;
    FIsInHandle: Boolean;   { True = incoming handle, False = outgoing }
    FOldPoint:  TVertexPoint;
    FNewPoint:  TVertexPoint;
  public
    constructor Create(APath: TVertexPath; ANodeIndex: Integer; AIsInHandle: Boolean;
                       const AOldPoint, ANewPoint: TVertexPoint);
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Change the solid colour of a TVertexStyle. }
  TVertexCmdSetStyleColour = class(TVertexCommand)
  private
    FStyle:    TVertexStyle;
    FOldColor: THvifColor;
    FNewColor: THvifColor;
  public
    constructor Create(AStyle: TVertexStyle;
                       const AOldColor, ANewColor: THvifColor);
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Add a new TVertexShape to the document. The command takes ownership of the
    shape until Execute is called; after Execute the document owns it.
    Undo transfers ownership back to the command. }
  TVertexCmdAddShape = class(TVertexCommand)
  private
    FDocument:    TVertexDocument;
    FShape:       TVertexShape;
    FInsertIndex: Integer;    { -1 = append }
    FOwnsShape:   Boolean;    { True when command holds ownership }
  public
    constructor Create(ADocument: TVertexDocument; AShape: TVertexShape;
                       AInsertIndex: Integer = -1);
    destructor  Destroy; override;
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Remove a TVertexShape from the document. The document releases ownership on
    Execute; Undo returns it. }
  TVertexCmdDeleteShape = class(TVertexCommand)
  private
    FDocument:  TVertexDocument;
    FShape:     TVertexShape;
    FSavedIndex: Integer;
    FOwnsShape: Boolean;
  public
    constructor Create(ADocument: TVertexDocument; AShape: TVertexShape);
    destructor  Destroy; override;
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Move a shape from AOldIndex to ANewIndex within the document's shape list.
    In HVIF, later shapes draw on top, so reordering changes the rendering order. }
  TVertexCmdMoveShape = class(TVertexCommand)
  private
    FDocument: TVertexDocument;
    FOldIndex: Integer;
    FNewIndex: Integer;
  public
    constructor Create(ADocument: TVertexDocument; AOldIndex, ANewIndex: Integer);
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Create a brand-new shape complete with its own style and path.
    All three objects are owned by the command until Execute; after Execute the
    document owns them.  Undo removes all three from the document. }
  TVertexCmdNewShape = class(TVertexCommand)
  private
    FDocument: TVertexDocument;
    FStyle:    TVertexStyle;
    FPath:     TVertexPath;
    FShape:    TVertexShape;
    FOwns:     Boolean;
  public
    constructor Create(ADocument: TVertexDocument;
                       AStyle: TVertexStyle; APath: TVertexPath; AShape: TVertexShape);
    destructor  Destroy; override;
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Add a new TVertexPath to the document. }
  TVertexCmdAddPath = class(TVertexCommand)
  private
    FDocument:    TVertexDocument;
    FPath:        TVertexPath;
    FInsertIndex: Integer;
    FOwnsPath:    Boolean;
  public
    constructor Create(ADocument: TVertexDocument; APath: TVertexPath;
                       AInsertIndex: Integer = -1);
    destructor  Destroy; override;
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Remove a TVertexPath from the document. Fails (raises) if any shape
    still references the path — caller must remove the reference first. }
  TVertexCmdDeletePath = class(TVertexCommand)
  private
    FDocument:   TVertexDocument;
    FPath:       TVertexPath;
    FSavedIndex: Integer;
    FOwnsPath:   Boolean;
  public
    constructor Create(ADocument: TVertexDocument; APath: TVertexPath);
    destructor  Destroy; override;
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Delete one path node at ANodeIndex. Stores the full TVertexPoint for Undo.
    Will not execute if deleting would leave fewer than 2 nodes. }
  TVertexCmdDeletePoint = class(TVertexCommand)
  private
    FPath:       TVertexPath;
    FNodeIndex:  Integer;
    FSavedPoint: TVertexPoint;
  public
    constructor Create(APath: TVertexPath; ANodeIndex: Integer);
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Insert one path node produced by splitting a bezier segment via de Casteljau.
    Stores the new node, the insert index, and the before/after snapshots of both
    adjacent nodes (their control handles change as a result of the split). }
  TVertexCmdAddPoint = class(TVertexCommand)
  private
    FPath:         TVertexPath;
    FInsertIndex:  Integer;   { index at which the new node is inserted }
    FPrevNodeIdx:  Integer;   { index of the preceding node }
    FNextNodeIdx:  Integer;   { index of the following node (before insert) }
    FNewPoint:     TVertexPoint;
    FPrevPtBefore: TVertexPoint; { preceding node before split }
    FPrevPtAfter:  TVertexPoint; { preceding node after split (OutHandle updated) }
    FNextPtBefore: TVertexPoint; { following node before split }
    FNextPtAfter:  TVertexPoint; { following node after split (InHandle updated) }
  public
    constructor Create(APath: TVertexPath;
                       AInsertIndex, APrevNodeIdx, ANextNodeIdx: Integer;
                       const ANewPoint: TVertexPoint;
                       const APrevPtBefore, APrevPtAfter: TVertexPoint;
                       const ANextPtBefore, ANextPtAfter: TVertexPoint);
    procedure Execute; override;
    procedure Undo;    override;
  end;


  { Rename any named document object. Works for TVertexPath, TVertexStyle, TVertexShape
    by passing the object's Name field as a PString. }
  TVertexCmdRename = class(TVertexCommand)
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
    FStack:     TObjectList;   { owns TVertexCommand instances }
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
    procedure Execute(ACmd: TVertexCommand);

    { Step one command back. No-op if CanUndo is False. }
    procedure Undo;

    { Re-apply one command forward. No-op if CanRedo is False. }
    procedure Redo;

    { Fired after every Execute, Undo or Redo. Connect to TVertexDocument or the UI. }
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


{ ==================== TVertexDocument ==================== }

type
  TVertexDocument = class
  private
    FPaths:     TObjectList;   { owns TVertexPath instances }
    FStyles:    TObjectList;   { owns TVertexStyle instances }
    FShapes:    TObjectList;   { owns TVertexShape instances }
    FUndoStack: TUndoStack;
    FDirty:     Boolean;
    FOnChange:  TVertexChangeEvent;

    function  GetPathCount:  Integer;
    function  GetStyleCount: Integer;
    function  GetShapeCount: Integer;
    function  GetPath(AIndex: Integer):  TVertexPath;
    function  GetStyle(AIndex: Integer): TVertexStyle;
    function  GetShape(AIndex: Integer): TVertexShape;

    procedure NotifyChange(ACmd: TVertexCommand);
    procedure HandleUndoChange(Sender: TObject);

  public
    constructor Create;
    destructor  Destroy; override;

    { --- Object access --- }
    property PathCount:  Integer    read GetPathCount;
    property StyleCount: Integer    read GetStyleCount;
    property ShapeCount: Integer    read GetShapeCount;
    property Paths[AIndex: Integer]:  TVertexPath  read GetPath;
    property Styles[AIndex: Integer]: TVertexStyle read GetStyle;
    property Shapes[AIndex: Integer]: TVertexShape read GetShape;

    { Index lookup by object reference (-1 if not found). }
    function IndexOfPath(APath:   TVertexPath):  Integer;
    function IndexOfStyle(AStyle: TVertexStyle): Integer;
    function IndexOfShape(AShape: TVertexShape): Integer;

    { Index lookup by name (case-sensitive; -1 if not found). }
    function FindPathByName(const AName:  string): TVertexPath;
    function FindStyleByName(const AName: string): TVertexStyle;
    function FindShapeByName(const AName: string): TVertexShape;

    { Check whether AName is already in use (across all object types). }
    function NameExists(const AName: string): Boolean;

    { Generate a unique name within the document with the given prefix.
      E.g. UniqueName('path') → 'path_0', 'path_1', etc. }
    function UniqueName(const APrefix: string): string;

    { --- Direct list manipulation (called by command Execute/Undo only) ---
      UI code must go through TUndoStack.Execute rather than calling these. }
    procedure InternalAddPath(APath: TVertexPath; AIndex: Integer = -1);
    procedure InternalRemovePath(APath: TVertexPath);
    procedure InternalAddStyle(AStyle: TVertexStyle; AIndex: Integer = -1);
    procedure InternalRemoveStyle(AStyle: TVertexStyle);
    procedure InternalAddShape(AShape: TVertexShape; AIndex: Integer = -1);
    procedure InternalRemoveShape(AShape: TVertexShape);
    { Move a shape from AOldIndex to ANewIndex (both in current list space). }
    procedure InternalMoveShape(AOldIndex, ANewIndex: Integer);

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
    property OnChange: TVertexChangeEvent read FOnChange write FOnChange;
  end;


implementation


{ ==================== TVertexCommand ==================== }

procedure TVertexCommand.Redo;
begin
  Execute;
end;


{ ==================== TVertexPath ==================== }

constructor TVertexPath.Create(const AName: string);
begin
  inherited Create;
  FName   := AName;
  FClosed := False;
  SetLength(FPoints, 0);
end;

function TVertexPath.GetPointCount: Integer;
begin
  Result := Length(FPoints);
end;

function TVertexPath.GetPoint(AIndex: Integer): TVertexPoint;
begin
  Result := FPoints[AIndex];
end;

procedure TVertexPath.SetPoint(AIndex: Integer; const AValue: TVertexPoint);
begin
  FPoints[AIndex] := AValue;
end;

function TVertexPath.AddPoint(const APt: TVertexPoint): Integer;
begin
  Result := Length(FPoints);
  SetLength(FPoints, Result + 1);
  FPoints[Result] := APt;
end;

procedure TVertexPath.DeletePoint(AIndex: Integer);
var
  i: Integer;
begin
  for i := AIndex to Length(FPoints) - 2 do
    FPoints[i] := FPoints[i + 1];
  SetLength(FPoints, Length(FPoints) - 1);
end;

procedure TVertexPath.InsertPoint(AIndex: Integer; const APt: TVertexPoint);
var
  i, n: Integer;
begin
  n := Length(FPoints);
  SetLength(FPoints, n + 1);
  for i := n downto AIndex + 1 do
    FPoints[i] := FPoints[i - 1];
  FPoints[AIndex] := APt;
end;

function TVertexPath.IsLinearOnly: Boolean;
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

function TVertexPath.ToHvifPath: THvifPath;
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

procedure TVertexPath.FromHvifPath(const ASrc: THvifPath);
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


{ ==================== TVertexStyle ==================== }

constructor TVertexStyle.Create(const AName: string);
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

function TVertexStyle.GetStopCount: Integer;
begin
  Result := Length(FStops);
end;

function TVertexStyle.GetStop(AIndex: Integer): TVertexGradientStop;
begin
  Result := FStops[AIndex];
end;

procedure TVertexStyle.SetStop(AIndex: Integer; const AValue: TVertexGradientStop);
begin
  FStops[AIndex] := AValue;
end;

function TVertexStyle.AddStop(AOffset: Single; AColor: THvifColor): Integer;
begin
  Result := Length(FStops);
  SetLength(FStops, Result + 1);
  FStops[Result].Offset := AOffset;
  FStops[Result].Color  := AColor;
end;

procedure TVertexStyle.DeleteStop(AIndex: Integer);
var
  i: Integer;
begin
  for i := AIndex to Length(FStops) - 2 do
    FStops[i] := FStops[i + 1];
  SetLength(FStops, Length(FStops) - 1);
end;

function TVertexStyle.IsGradient: Boolean;
begin
  Result := FStyleType = hstGradient;
end;

function TVertexStyle.ToHvifStyle: THvifStyle;
var
  i: Integer;
begin
  Result.StyleType    := FStyleType;
  Result.Color        := FColor;
  Result.GradientType := FGradientType;
  Move(FGradTransform[0], Result.GradTransform[0], SizeOf(FGradTransform));
  Result.HasGradTransform := IsGradient; { gradient styles always carry a transform }
  SetLength(Result.Stops, Length(FStops));
  for i := 0 to High(FStops) do
  begin
    Result.Stops[i].Offset := FStops[i].Offset;
    Result.Stops[i].Color  := FStops[i].Color;
  end;
end;

procedure TVertexStyle.FromHvifStyle(const ASrc: THvifStyle);
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

procedure TVertexStyle.GetGradTransform(out AMatrix: array of Single);
begin
  Move(FGradTransform[0], AMatrix[0], 6 * SizeOf(Single));
end;

procedure TVertexStyle.SetGradTransform(const AMatrix: array of Single);
begin
  Move(AMatrix[0], FGradTransform[0], 6 * SizeOf(Single));
end;


{ ==================== TVertexShape ==================== }

constructor TVertexShape.Create(const AName: string);
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

destructor TVertexShape.Destroy;
begin
  FPaths.Free;   { does not free the path objects — they are owned by TVertexDocument }
  inherited;
end;

function TVertexShape.GetPathCount: Integer;
begin
  Result := FPaths.Count;
end;

function TVertexShape.GetPath(AIndex: Integer): TVertexPath;
begin
  Result := TVertexPath(FPaths[AIndex]);
end;

procedure TVertexShape.AddPathRef(APath: TVertexPath);
begin
  if FPaths.IndexOf(APath) < 0 then
    FPaths.Add(APath);
end;

procedure TVertexShape.RemovePathRef(APath: TVertexPath);
begin
  FPaths.Remove(APath);
end;

function TVertexShape.IndexOfPath(APath: TVertexPath): Integer;
begin
  Result := FPaths.IndexOf(APath);
end;

function TVertexShape.ToHvifShape(AStyleIndex: Byte;
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

procedure TVertexShape.GetTransform(out AMatrix: array of Single);
begin
  Move(FTransform[0], AMatrix[0], 6 * SizeOf(Single));
end;

procedure TVertexShape.SetTransform(const AMatrix: array of Single);
begin
  Move(AMatrix[0], FTransform[0], 6 * SizeOf(Single));
end;


{ ==================== Concrete commands ==================== }

{ TVertexCmdMoveNode }

constructor TVertexCmdMoveNode.Create(APath: TVertexPath; ANodeIndex: Integer;
                                    const AOldPoint, ANewPoint: TVertexPoint);
begin
  inherited Create;
  FPath      := APath;
  FNodeIndex := ANodeIndex;
  FOldPoint  := AOldPoint;
  FNewPoint  := ANewPoint;
  Description := 'Move node';
end;

procedure TVertexCmdMoveNode.Execute;
begin
  FPath.Points[FNodeIndex] := FNewPoint;
end;

procedure TVertexCmdMoveNode.Undo;
begin
  FPath.Points[FNodeIndex] := FOldPoint;
end;


{ TVertexCmdMoveHandle }

constructor TVertexCmdMoveHandle.Create(APath: TVertexPath; ANodeIndex: Integer;
                                      AIsInHandle: Boolean;
                                      const AOldPoint, ANewPoint: TVertexPoint);
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

procedure TVertexCmdMoveHandle.Execute;
begin
  FPath.Points[FNodeIndex] := FNewPoint;
end;

procedure TVertexCmdMoveHandle.Undo;
begin
  FPath.Points[FNodeIndex] := FOldPoint;
end;


{ TVertexCmdSetStyleColour }

constructor TVertexCmdSetStyleColour.Create(AStyle: TVertexStyle;
                                          const AOldColor, ANewColor: THvifColor);
begin
  inherited Create;
  FStyle    := AStyle;
  FOldColor := AOldColor;
  FNewColor := ANewColor;
  Description := 'Set colour';
end;

procedure TVertexCmdSetStyleColour.Execute;
begin
  FStyle.Color := FNewColor;
end;

procedure TVertexCmdSetStyleColour.Undo;
begin
  FStyle.Color := FOldColor;
end;


{ TVertexCmdAddShape }

constructor TVertexCmdAddShape.Create(ADocument: TVertexDocument; AShape: TVertexShape;
                                    AInsertIndex: Integer);
begin
  inherited Create;
  FDocument    := ADocument;
  FShape       := AShape;
  FInsertIndex := AInsertIndex;
  FOwnsShape   := True;
  Description  := 'Add shape';
end;

destructor TVertexCmdAddShape.Destroy;
begin
  if FOwnsShape then
    FShape.Free;
  inherited;
end;

procedure TVertexCmdAddShape.Execute;
begin
  FDocument.InternalAddShape(FShape, FInsertIndex);
  FOwnsShape := False;
end;

procedure TVertexCmdAddShape.Undo;
begin
  FDocument.InternalRemoveShape(FShape);
  FOwnsShape := True;
end;


{ TVertexCmdDeleteShape }

constructor TVertexCmdDeleteShape.Create(ADocument: TVertexDocument; AShape: TVertexShape);
begin
  inherited Create;
  FDocument   := ADocument;
  FShape      := AShape;
  FSavedIndex := ADocument.IndexOfShape(AShape);
  FOwnsShape  := False;
  Description := 'Delete shape';
end;

destructor TVertexCmdDeleteShape.Destroy;
begin
  if FOwnsShape then
    FShape.Free;
  inherited;
end;

procedure TVertexCmdDeleteShape.Execute;
begin
  FDocument.InternalRemoveShape(FShape);
  FOwnsShape := True;
end;

procedure TVertexCmdDeleteShape.Undo;
begin
  FDocument.InternalAddShape(FShape, FSavedIndex);
  FOwnsShape := False;
end;


{ TVertexCmdMoveShape }

constructor TVertexCmdMoveShape.Create(ADocument: TVertexDocument;
    AOldIndex, ANewIndex: Integer);
begin
  inherited Create;
  FDocument   := ADocument;
  FOldIndex   := AOldIndex;
  FNewIndex   := ANewIndex;
  Description := 'Move shape';
end;

procedure TVertexCmdMoveShape.Execute;
begin
  FDocument.InternalMoveShape(FOldIndex, FNewIndex);
end;

procedure TVertexCmdMoveShape.Undo;
begin
  FDocument.InternalMoveShape(FNewIndex, FOldIndex);
end;


{ TVertexCmdNewShape }

constructor TVertexCmdNewShape.Create(ADocument: TVertexDocument;
    AStyle: TVertexStyle; APath: TVertexPath; AShape: TVertexShape);
begin
  inherited Create;
  FDocument   := ADocument;
  FStyle      := AStyle;
  FPath       := APath;
  FShape      := AShape;
  FOwns       := True;
  Description := 'Add shape';
end;

destructor TVertexCmdNewShape.Destroy;
begin
  if FOwns then
  begin
    FShape.Free;   { shape does not own its style/path refs }
    FPath.Free;
    FStyle.Free;
  end;
  inherited;
end;

procedure TVertexCmdNewShape.Execute;
begin
  FDocument.InternalAddStyle(FStyle);
  FDocument.InternalAddPath(FPath);
  FDocument.InternalAddShape(FShape);
  FOwns := False;
end;

procedure TVertexCmdNewShape.Undo;
begin
  FDocument.InternalRemoveShape(FShape);
  FDocument.InternalRemovePath(FPath);
  FDocument.InternalRemoveStyle(FStyle);
  FOwns := True;
end;


{ TVertexCmdAddPath }

constructor TVertexCmdAddPath.Create(ADocument: TVertexDocument; APath: TVertexPath;
                                   AInsertIndex: Integer);
begin
  inherited Create;
  FDocument    := ADocument;
  FPath        := APath;
  FInsertIndex := AInsertIndex;
  FOwnsPath    := True;
  Description  := 'Add path';
end;

destructor TVertexCmdAddPath.Destroy;
begin
  if FOwnsPath then
    FPath.Free;
  inherited;
end;

procedure TVertexCmdAddPath.Execute;
begin
  FDocument.InternalAddPath(FPath, FInsertIndex);
  FOwnsPath := False;
end;

procedure TVertexCmdAddPath.Undo;
begin
  FDocument.InternalRemovePath(FPath);
  FOwnsPath := True;
end;


{ TVertexCmdDeletePath }

constructor TVertexCmdDeletePath.Create(ADocument: TVertexDocument; APath: TVertexPath);
begin
  inherited Create;
  FDocument   := ADocument;
  FPath       := APath;
  FSavedIndex := ADocument.IndexOfPath(APath);
  FOwnsPath   := False;
  Description := 'Delete path';
end;

destructor TVertexCmdDeletePath.Destroy;
begin
  if FOwnsPath then
    FPath.Free;
  inherited;
end;

procedure TVertexCmdDeletePath.Execute;
begin
  FDocument.InternalRemovePath(FPath);
  FOwnsPath := True;
end;

procedure TVertexCmdDeletePath.Undo;
begin
  FDocument.InternalAddPath(FPath, FSavedIndex);
  FOwnsPath := False;
end;


{ TVertexCmdRename }

constructor TVertexCmdRename.Create(ANameField: PString;
                                  const AOldName, ANewName: string);
begin
  inherited Create;
  FNameField  := ANameField;
  FOldName    := AOldName;
  FNewName    := ANewName;
  Description := Format('Rename to "%s"', [ANewName]);
end;

procedure TVertexCmdRename.Execute;
begin
  FNameField^ := FNewName;
end;

procedure TVertexCmdRename.Undo;
begin
  FNameField^ := FOldName;
end;


{ TVertexCmdDeletePoint }

constructor TVertexCmdDeletePoint.Create(APath: TVertexPath; ANodeIndex: Integer);
begin
  inherited Create;
  FPath       := APath;
  FNodeIndex  := ANodeIndex;
  FSavedPoint := APath.Points[ANodeIndex];
  Description := 'Delete node';
end;

procedure TVertexCmdDeletePoint.Execute;
begin
  FPath.DeletePoint(FNodeIndex);
end;

procedure TVertexCmdDeletePoint.Undo;
begin
  FPath.InsertPoint(FNodeIndex, FSavedPoint);
end;


{ TVertexCmdAddPoint }

constructor TVertexCmdAddPoint.Create(APath: TVertexPath;
    AInsertIndex, APrevNodeIdx, ANextNodeIdx: Integer;
    const ANewPoint: TVertexPoint;
    const APrevPtBefore, APrevPtAfter: TVertexPoint;
    const ANextPtBefore, ANextPtAfter: TVertexPoint);
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

procedure TVertexCmdAddPoint.Execute;
begin
  { Update adjacent handles first, then insert the new node.
    The insert shifts FNextNodeIdx up by one, but we update it before that. }
  FPath.Points[FPrevNodeIdx] := FPrevPtAfter;
  FPath.Points[FNextNodeIdx] := FNextPtAfter;
  FPath.InsertPoint(FInsertIndex, FNewPoint);
end;

procedure TVertexCmdAddPoint.Undo;
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

procedure TUndoStack.Execute(ACmd: TVertexCommand);
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
  TVertexCommand(FStack[FCursor]).Undo;
  Dec(FCursor);
  NotifyChange;
end;

procedure TUndoStack.Redo;
begin
  if not CanRedo then
    Exit;
  Inc(FCursor);
  TVertexCommand(FStack[FCursor]).Redo;
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
    Result := TVertexCommand(FStack[FCursor]).Description
  else
    Result := '';
end;

function TUndoStack.RedoDescription: string;
begin
  if CanRedo then
    Result := TVertexCommand(FStack[FCursor + 1]).Description
  else
    Result := '';
end;

procedure TUndoStack.Clear;
begin
  FStack.Clear;
  FCursor := -1;
end;


{ ==================== TVertexDocument ==================== }

constructor TVertexDocument.Create;
begin
  inherited Create;
  FPaths     := TObjectList.Create(True);  { owns TVertexPath }
  FStyles    := TObjectList.Create(True);  { owns TVertexStyle }
  FShapes    := TObjectList.Create(True);  { owns TVertexShape }
  FUndoStack := TUndoStack.Create;
  FUndoStack.OnChange := @HandleUndoChange;
  FDirty     := False;
  FOnChange  := nil;
end;

destructor TVertexDocument.Destroy;
begin
  FUndoStack.Free;
  FShapes.Free;
  FStyles.Free;
  FPaths.Free;
  inherited;
end;

function TVertexDocument.GetPathCount:  Integer; begin Result := FPaths.Count;  end;
function TVertexDocument.GetStyleCount: Integer; begin Result := FStyles.Count; end;
function TVertexDocument.GetShapeCount: Integer; begin Result := FShapes.Count; end;

function TVertexDocument.GetPath(AIndex: Integer):  TVertexPath;  begin Result := TVertexPath(FPaths[AIndex]);   end;
function TVertexDocument.GetStyle(AIndex: Integer): TVertexStyle; begin Result := TVertexStyle(FStyles[AIndex]); end;
function TVertexDocument.GetShape(AIndex: Integer): TVertexShape; begin Result := TVertexShape(FShapes[AIndex]); end;

procedure TVertexDocument.NotifyChange(ACmd: TVertexCommand);
begin
  FDirty := True;
  if Assigned(FOnChange) then
    FOnChange(Self, ACmd);
end;

procedure TVertexDocument.NotifyChanged;
begin
  NotifyChange(nil);
end;

procedure TVertexDocument.HandleUndoChange(Sender: TObject);
begin
  NotifyChange(nil);
end;

function TVertexDocument.UniqueName(const APrefix: string): string;
var
  n: Integer;
begin
  n := 0;
  repeat
    Result := Format('%s_%d', [APrefix, n]);
    Inc(n);
  until not NameExists(Result);
end;

function TVertexDocument.IndexOfPath(APath: TVertexPath):   Integer; begin Result := FPaths.IndexOf(APath);   end;
function TVertexDocument.IndexOfStyle(AStyle: TVertexStyle): Integer; begin Result := FStyles.IndexOf(AStyle); end;
function TVertexDocument.IndexOfShape(AShape: TVertexShape): Integer; begin Result := FShapes.IndexOf(AShape); end;

function TVertexDocument.FindPathByName(const AName: string): TVertexPath;
var
  i: Integer;
begin
  for i := 0 to FPaths.Count - 1 do
    if TVertexPath(FPaths[i]).Name = AName then
      Exit(TVertexPath(FPaths[i]));
  Result := nil;
end;

function TVertexDocument.FindStyleByName(const AName: string): TVertexStyle;
var
  i: Integer;
begin
  for i := 0 to FStyles.Count - 1 do
    if TVertexStyle(FStyles[i]).Name = AName then
      Exit(TVertexStyle(FStyles[i]));
  Result := nil;
end;

function TVertexDocument.FindShapeByName(const AName: string): TVertexShape;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    if TVertexShape(FShapes[i]).Name = AName then
      Exit(TVertexShape(FShapes[i]));
  Result := nil;
end;

function TVertexDocument.NameExists(const AName: string): Boolean;
begin
  Result := Assigned(FindPathByName(AName))
         or Assigned(FindStyleByName(AName))
         or Assigned(FindShapeByName(AName));
end;

procedure TVertexDocument.InternalAddPath(APath: TVertexPath; AIndex: Integer);
begin
  if AIndex < 0 then
    FPaths.Add(APath)
  else
    FPaths.Insert(AIndex, APath);
end;

procedure TVertexDocument.InternalRemovePath(APath: TVertexPath);
begin
  FPaths.Extract(APath);   { removes without freeing }
end;

procedure TVertexDocument.InternalAddStyle(AStyle: TVertexStyle; AIndex: Integer);
begin
  if AIndex < 0 then
    FStyles.Add(AStyle)
  else
    FStyles.Insert(AIndex, AStyle);
end;

procedure TVertexDocument.InternalRemoveStyle(AStyle: TVertexStyle);
begin
  FStyles.Extract(AStyle);
end;

procedure TVertexDocument.InternalAddShape(AShape: TVertexShape; AIndex: Integer);
begin
  if AIndex < 0 then
    FShapes.Add(AShape)
  else
    FShapes.Insert(AIndex, AShape);
end;

procedure TVertexDocument.InternalRemoveShape(AShape: TVertexShape);
begin
  FShapes.Extract(AShape);
end;

procedure TVertexDocument.InternalMoveShape(AOldIndex, ANewIndex: Integer);
var
  obj: TObject;
begin
  obj := FShapes[AOldIndex];
  FShapes.Extract(obj);
  if ANewIndex >= FShapes.Count then
    FShapes.Add(obj)
  else
    FShapes.Insert(ANewIndex, obj);
end;

procedure TVertexDocument.FromHvifArrays(const AStyles: array of THvifStyle;
                                       const APaths:  array of THvifPath;
                                       const AShapes: array of THvifShape);
var
  i, j:   Integer;
  style:  TVertexStyle;
  path:   TVertexPath;
  shape:  TVertexShape;
  pIdx:   Byte;
  xf:     TVertexTransformer;
begin
  FUndoStack.Clear;
  FPaths.Clear;
  FStyles.Clear;
  FShapes.Clear;

  for i := 0 to High(AStyles) do
  begin
    style := TVertexStyle.Create(UniqueName('style'));
    style.FromHvifStyle(AStyles[i]);
    FStyles.Add(style);
  end;

  for i := 0 to High(APaths) do
  begin
    path := TVertexPath.Create(UniqueName('path'));
    path.FromHvifPath(APaths[i]);
    FPaths.Add(path);
  end;

  for i := 0 to High(AShapes) do
  begin
    shape := TVertexShape.Create(UniqueName('shape'));
    { Resolve style index → object reference }
    if AShapes[i].StyleIndex < FStyles.Count then
      shape.Style := TVertexStyle(FStyles[AShapes[i].StyleIndex]);
    { Resolve path indices → object references }
    for j := 0 to High(AShapes[i].PathIndices) do
    begin
      pIdx := AShapes[i].PathIndices[j];
      if pIdx < FPaths.Count then
        shape.AddPathRef(TVertexPath(FPaths[pIdx]));
    end;
    { Copy transform, LOD, transformer from raw record }
    shape.HasTransform   := AShapes[i].HasTransform;
    shape.HasTranslation := AShapes[i].HasTranslation;
    shape.TranslateX     := AShapes[i].TranslateX;
    shape.TranslateY     := AShapes[i].TranslateY;
    if AShapes[i].HasStroke then
    begin
      xf := Default(TVertexTransformer);
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

function TVertexDocument.BuildWriter: THvifWriter;
var
  i, j:     Integer;
  shape:    TVertexShape;
  pathIdxs: array of Byte;
  sIdx:     Integer;
  writer:   THvifWriter;
begin
  writer := THvifWriter.Create;
  try
    for i := 0 to FStyles.Count - 1 do
      writer.AddStyle(TVertexStyle(FStyles[i]).ToHvifStyle);

    for i := 0 to FPaths.Count - 1 do
      writer.AddPath(TVertexPath(FPaths[i]).ToHvifPath);

    for i := 0 to FShapes.Count - 1 do
    begin
      shape := TVertexShape(FShapes[i]);
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

procedure TVertexDocument.LoadFromStream(AStream: TStream);
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

procedure TVertexDocument.SaveToStream(AStream: TStream);
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

procedure TVertexDocument.LoadFromFile(const AFileName: string);
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

procedure TVertexDocument.SaveToFile(const AFileName: string);
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

procedure TVertexDocument.MarkClean;
begin
  FDirty := False;
  FUndoStack.Clear;
end;


end.
