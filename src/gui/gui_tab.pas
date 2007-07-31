unit gui_tab;

{$mode objfpc}{$H+}

{
  Incomplete:  I'm currently busy developing this component.
}

interface

uses
  Classes,
  SysUtils,
  fpgfx,
  gfx_widget,
  gui_button;
  
type
  // forward declaration
  TfpgPageControl = class;

  TfpgTabSheet = class(TfpgWidget)
  private
    function    GetPageControl: TfpgPageControl;
    function    GetPageIndex: Integer;
    function    GetText: string;
    procedure   SetPageControl(const AValue: TfpgPageControl);
    procedure   SetPageIndex(const AValue: Integer);
    procedure   SetText(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Text: string read GetText write SetText;
    property    PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property    PageControl: TfpgPageControl read GetPageControl write SetPageControl;
  end;


  TTabSheetChange = procedure(Sender: TObject; NewActiveSheet: TfpgTabSheet);
  
  
  TfpgPageControl = class(TfpgWidget)
  private
    FPages: TList;
    FActivePageIndex: integer;
    FOnChange: TTabSheetChange;
    function    GetActivePageIndex: integer;
    function    GetPageCount: Integer;
    procedure   InsertPage(const APage: TfpgTabSheet);
    procedure   RemovePage(const APage: TfpgTabSheet);
  protected
    procedure   UnregisterTabSheet(ATabSheet: TfpgTabSheet);
    procedure   RegisterTabSheet(ATabSheet: TfpgTabSheet);
  public
    constructor Create(AOwner: TComponent); override;
    property    PageCount: Integer read GetPageCount;
    property    ActivePageIndex: integer read GetActivePageIndex write FActivePageIndex;
    property    OnChange: TTabSheetChange read FOnChange write FOnChange;
  end;


implementation

{ TfpgTabSheet }

function TfpgTabSheet.GetPageControl: TfpgPageControl;
begin
  if Owner is TfpgPageControl then
    Result := TfpgPageControl(Owner)
  else
    Result := nil;
end;

function TfpgTabSheet.GetPageIndex: Integer;
begin
  if PageControl <> nil then
    Result := PageControl.FPages.IndexOf(Self)
  else
    Result := -1;
end;

function TfpgTabSheet.GetText: string;
begin

end;

procedure TfpgTabSheet.SetPageControl(const AValue: TfpgPageControl);
begin
  if PageControl <> AValue then
  begin
    if PageControl <> nil then
      PageControl.RemovePage(self);
//    Owner := AValue;
    if AValue <> nil then
      AValue.InsertPage(self);
  end;
end;

procedure TfpgTabSheet.SetPageIndex(const AValue: Integer);
begin

end;

procedure TfpgTabSheet.SetText(const AValue: string);
begin

end;

constructor TfpgTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFocusable := True;
  if Owner is TfpgPageControl then
  begin
    TfpgPageControl(Owner).RegisterTabSheet(self);
//    FPageIndex := TfpgPageControl(Owner).PageCount + 1;
  end;
end;

destructor TfpgTabSheet.Destroy;
begin
  if Owner is TfpgPageControl then
    TfpgPageControl(Owner).UnregisterTabSheet(self);
  inherited Destroy;
end;

{ TfpgPageControl }

function TfpgPageControl.GetActivePageIndex: integer;
begin

end;

function TfpgPageControl.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TfpgPageControl.InsertPage(const APage: TfpgTabSheet);
begin

end;

procedure TfpgPageControl.RemovePage(const APage: TfpgTabSheet);
begin

end;

procedure TfpgPageControl.UnregisterTabSheet(ATabSheet: TfpgTabSheet);
begin

end;

procedure TfpgPageControl.RegisterTabSheet(ATabSheet: TfpgTabSheet);
begin

end;

constructor TfpgPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPages := TList.Create;
  FOnChange := nil;
end;

end.

