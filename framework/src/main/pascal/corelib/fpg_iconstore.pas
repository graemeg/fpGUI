{
    Copyright (c) 2026 Graeme Geldenhuys

    This program is part of the fpGUI Toolkit project.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      DPI-aware HVIF icon registry for fpGUI.

      TfpgIconStore holds a string-keyed map of registered icons (HVIF or
      bitmap).  It mirrors the fpgImages API but adds:
        - Render-on-demand at any logical size (via THvifIcon).
        - DPI awareness: SetDPI / InvalidateCache flush rendered images so the
          next paint re-renders at the new physical resolution.
        - Mixed HVIF / bitmap registration under the same string id namespace.

      Usage:

        // Register at startup (e.g. in application OnCreate or InternalInit):
        fpgIcons.RegisterFromConst('app.build', @hvif_build, SizeOf(hvif_build));
        fpgIcons.RegisterFromResource('app.run', HInstance, 'MYAPP_ICON_RUN');
        fpgIcons.RegisterBitmap('app.logo', fpgImages.GetImage('mylogo'));

        // In a widget paint handler:
        img := fpgIcons.GetIcon('app.build', 24);   // borrowed — do NOT free
        canvas.DrawImage(x, y, img);

        // Owned copy (caller must free):
        img := fpgIcons.GetIconCopy('app.build', 24);
        try
          canvas.DrawImage(x, y, img);
        finally
          img.Free;
        end;

      Ownership rules:
        - RegisterFromConst / RegisterFromStream / RegisterFromResource:
            The TfpgIconStore owns the created THvifIcon instance.
        - RegisterBitmap:
            The TfpgIconStore takes ownership of the TfpgImage passed in.
        - GetIcon:
            Returns a borrowed reference owned by the store.  Do NOT free it.
        - GetIconCopy:
            Returns a new TfpgImage.  The caller is responsible for freeing it.
}

unit fpg_iconstore;

{$I fpg_defines.inc}

interface

uses
  Classes,
  SysUtils,
  fpg_main,
  fpg_hvif;

type
  TfpgIconKind = (ikHVIF, ikBitmap);

  { TfpgIconEntry — internal container owned by TfpgIconStore }
  TfpgIconEntry = class
  private
    FKind:   TfpgIconKind;
    FHVIF:   THvifIcon;   { owned when Kind = ikHVIF   }
    FBitmap: TfpgImage;   { owned when Kind = ikBitmap }
  public
    destructor Destroy; override;
    property Kind:   TfpgIconKind read FKind;
    property HVIF:   THvifIcon    read FHVIF;
    property Bitmap: TfpgImage    read FBitmap;
  end;

  { TfpgIconStore — string-keyed icon registry }
  TfpgIconStore = class
  private
    FEntries: TStringList;  { sorted; Objects[] are TfpgIconEntry (owned) }
    FDPI:     Integer;

    { Single ownership funnel for all HVIF registration paths }
    procedure DoRegisterHVIF(const AId: string; AIcon: THvifIcon);

    { Convert a logical pixel size to physical pixels for the current DPI }
    function  PhysicalSize(ALogical: Integer): Integer;

  public
    constructor Create;
    destructor  Destroy; override;

    { Register an HVIF icon from an in-memory const byte array.
      The icon data is copied into a TBytes buffer; the caller's pointer
      need not remain valid after the call. }
    procedure RegisterFromConst(const AId: string; AData: PByte; ASize: SizeInt);

    { Register an HVIF icon by reading the full contents of AStream. }
    procedure RegisterFromStream(const AId: string; AStream: TStream);

    { Register an HVIF icon from an FPC RCDATA resource embedded in the
      binary at link time.  AInst is normally HInstance (or 0 on Linux). }
    procedure RegisterFromResource(const AId: string; AInst: THandle;
                                   const AResName: string);

    { Register a pre-loaded bitmap.  The store takes ownership of ABitmap. }
    procedure RegisterBitmap(const AId: string; ABitmap: TfpgImage);

    { Returns True when AId has been registered. }
    function  HasIcon(const AId: string): Boolean;

    { Returns a TfpgImage rendered at ASize logical pixels (converted to
      physical pixels via the current DPI).  The image is owned by this
      store — do NOT free it.
      For bitmap entries the ASize parameter is ignored. }
    function  GetIcon(const AId: string; ASize: Integer): TfpgImage; overload;

    { Convenience overload for bitmap-only entries where size is irrelevant. }
    function  GetIcon(const AId: string): TfpgImage; overload;

    { Returns a newly allocated TfpgImage copy rendered at ASize logical
      pixels.  The caller is responsible for freeing it. }
    function  GetIconCopy(const AId: string; ASize: Integer): TfpgImage;

    { Notify the store that the screen DPI has changed.  Clears the render
      cache inside every HVIF icon so that the next GetIcon call re-renders
      at the new physical resolution. }
    procedure SetDPI(AValue: Integer);

    { Flush cached rendered images in every registered HVIF icon. }
    procedure InvalidateCache;
  end;


var
  fpgIcons: TfpgIconStore;


implementation


{ TfpgIconEntry }

destructor TfpgIconEntry.Destroy;
begin
  FHVIF.Free;
  FBitmap.Free;
  inherited Destroy;
end;


{ TfpgIconStore }

constructor TfpgIconStore.Create;
begin
  inherited Create;
  FEntries := TStringList.Create;
  FEntries.Sorted     := True;
  FEntries.Duplicates := dupError;
  FDPI := 96;
end;

destructor TfpgIconStore.Destroy;
var
  i: Integer;
begin
  for i := 0 to FEntries.Count - 1 do
    FEntries.Objects[i].Free;
  FEntries.Free;
  inherited Destroy;
end;

procedure TfpgIconStore.DoRegisterHVIF(const AId: string; AIcon: THvifIcon);
var
  entry: TfpgIconEntry;
  idx:   Integer;
begin
  { Replace any existing entry for this id }
  idx := FEntries.IndexOf(AId);
  if idx >= 0 then
  begin
    FEntries.Objects[idx].Free;
    entry        := TfpgIconEntry.Create;
    entry.FKind  := ikHVIF;
    entry.FHVIF  := AIcon;
    FEntries.Objects[idx] := entry;
  end
  else
  begin
    entry        := TfpgIconEntry.Create;
    entry.FKind  := ikHVIF;
    entry.FHVIF  := AIcon;
    FEntries.AddObject(AId, entry);
  end;
end;

function TfpgIconStore.PhysicalSize(ALogical: Integer): Integer;
begin
  if FDPI = 96 then
    Result := ALogical
  else
    Result := (ALogical * FDPI + 48) div 96;  { rounded integer scale }
end;

procedure TfpgIconStore.RegisterFromConst(const AId: string; AData: PByte;
  ASize: SizeInt);
var
  ms:   TMemoryStream;
  icon: THvifIcon;
begin
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(AData^, ASize);
    ms.Position := 0;
    icon := THvifIcon.CreateFromStream(ms);
  finally
    ms.Free;
  end;
  DoRegisterHVIF(AId, icon);
end;

procedure TfpgIconStore.RegisterFromStream(const AId: string; AStream: TStream);
var
  icon: THvifIcon;
begin
  icon := THvifIcon.CreateFromStream(AStream);
  DoRegisterHVIF(AId, icon);
end;

procedure TfpgIconStore.RegisterFromResource(const AId: string; AInst: THandle;
  const AResName: string);
var
  icon: THvifIcon;
begin
  icon := THvifIcon.CreateFromResource(AInst, AResName);
  DoRegisterHVIF(AId, icon);
end;

procedure TfpgIconStore.RegisterBitmap(const AId: string; ABitmap: TfpgImage);
var
  entry: TfpgIconEntry;
  idx:   Integer;
begin
  idx := FEntries.IndexOf(AId);
  if idx >= 0 then
  begin
    FEntries.Objects[idx].Free;
    entry         := TfpgIconEntry.Create;
    entry.FKind   := ikBitmap;
    entry.FBitmap := ABitmap;
    FEntries.Objects[idx] := entry;
  end
  else
  begin
    entry         := TfpgIconEntry.Create;
    entry.FKind   := ikBitmap;
    entry.FBitmap := ABitmap;
    FEntries.AddObject(AId, entry);
  end;
end;

function TfpgIconStore.HasIcon(const AId: string): Boolean;
begin
  Result := FEntries.IndexOf(AId) >= 0;
end;

function TfpgIconStore.GetIcon(const AId: string; ASize: Integer): TfpgImage;
var
  idx:   Integer;
  entry: TfpgIconEntry;
  phys:  Integer;
begin
  Result := nil;
  idx := FEntries.IndexOf(AId);
  if idx < 0 then
    Exit;
  entry := TfpgIconEntry(FEntries.Objects[idx]);
  case entry.Kind of
    ikHVIF:
      begin
        phys   := PhysicalSize(ASize);
        Result := entry.HVIF.GetImage(phys, phys);
      end;
    ikBitmap:
      Result := entry.Bitmap;
  end;
end;

function TfpgIconStore.GetIcon(const AId: string): TfpgImage;
begin
  Result := GetIcon(AId, 0);
end;

function TfpgIconStore.GetIconCopy(const AId: string; ASize: Integer): TfpgImage;
var
  src: TfpgImage;
begin
  src := GetIcon(AId, ASize);
  if src = nil then
    Result := nil
  else
    Result := src.ImageFromSource;
end;

procedure TfpgIconStore.SetDPI(AValue: Integer);
begin
  if AValue <= 0 then
    AValue := 96;
  if AValue = FDPI then
    Exit;
  FDPI := AValue;
  InvalidateCache;
end;

procedure TfpgIconStore.InvalidateCache;
var
  i:     Integer;
  entry: TfpgIconEntry;
begin
  for i := 0 to FEntries.Count - 1 do
  begin
    entry := TfpgIconEntry(FEntries.Objects[i]);
    if entry.Kind = ikHVIF then
      entry.HVIF.ClearCache;
  end;
end;


initialization
  fpgIcons := nil;   { Created by TfpgApplication.InternalInit }

finalization
  fpgIcons.Free;
  fpgIcons := nil;

end.
