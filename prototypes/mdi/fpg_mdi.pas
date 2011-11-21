unit fpg_mdi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, fpg_main, fpg_widget, fpg_scrollbar, fpg_panel,
  fpg_button;

type
	// forward declarations
	TfpgMDIChildForm = class;


	TfpgMDIWorkArea = class(TfpgWidget)
	private
		FHorBar: TfpgScrollbar;
		FVerBar: TfpgScrollbar;
		FList: TList;
		FActiveWindow: TfpgMDIChildForm;
		procedure InternalMsgFreeMe(var msg: TfpgMessageRec); message FPGM_FREEME;
		procedure SetActiveWindow(AValue: TfpgMDIChildForm);
		function GetChildWindowCount: integer;
	protected
		procedure HandlePaint; override;
		procedure PositionScrollBars;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function AddWindow(AWindowClass: TfpgFrameClass): TfpgFrame;
		property ActiveWindow: TfpgMDIChildForm read FActiveWindow write SetActiveWindow;
		property ChildWindowCount: integer read GetChildWindowCount;
	end;


	TfpgMDIChildForm = class(TfpgWidget)
	private
		{@VFD_HEAD_BEGIN: MDIChildForm}
		Panel1: TfpgPanel;
		bevLeft: TfpgBevel;
		Bevel2: TfpgBevel;
		bevBottom: TfpgBevel;
		Bevel4: TfpgBevel;
		bevRight: TfpgBevel;
		Button1: TfpgButton;
		Button2: TfpgButton;
		Button3: TfpgButton;
		Button4: TfpgButton;
		bvlClientArea: TfpgBevel;
		{@VFD_HEAD_END: MDIChildForm}
		FMDIWorkArea: TfpgMDIWorkArea;
		FWindowTitle: TfpgString;
		FIsMouseDown: boolean;
		FLastPos: TPoint;
		FActive: boolean;
		procedure SetWindowTitle(AValue: TfpgString);
		procedure TitleMouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
		procedure TitleMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
		procedure TitleMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
		procedure TitleMouseExit(Sender: TObject);
		procedure CloseMDIWindowClicked(Sender: TObject);
		procedure SetActive(AValue: boolean);
	protected
		property Active: boolean read FActive write SetActive;
	public
		constructor Create(AOwner: TfpgMDIWorkArea); reintroduce;
		property WindowTitle: TfpgString read FWindowTitle write SetWindowTitle;
		procedure SetClientFrame(AFrame: TfpgFrame);
		procedure UpdateWindowTitle;
		procedure Close;
	end;

implementation

uses
	dbugintf;

{ TfpgMDIChildForm }

procedure TfpgMDIChildForm.TitleMouseMove(Sender: TObject; AShift: TShiftState;
	const AMousePos: TPoint);
var
  dx, dy: integer;
  pt: TPoint;
begin
	pt := WindowToScreen(self, AMousePos);
	if not FIsMouseDown then
	begin
		FLastPos := pt;
		Exit;
	end;

	dx := pt.X - FLastPos.X;
	dy := pt.Y - FLastPos.Y;
	Left := Left + dx;
	Top := Top + dy;
	FLastPos := pt;
	UpdateWindowPosition;
end;

procedure TfpgMDIChildForm.TitleMouseUp(Sender: TObject; AButton: TMouseButton;
	AShift: TShiftState; const AMousePos: TPoint);
begin
	FIsMouseDown := False;
	Panel1.ReleaseMouse;
end;

procedure TfpgMDIChildForm.TitleMouseDown(Sender: TObject; AButton: TMouseButton;
	AShift: TShiftState; const AMousePos: TPoint);
begin
	FMDIWorkArea.ActiveWindow := self;
	FIsMouseDown := True;
	FLastPos := Panel1.WindowToScreen(self, AMousePos);
	Panel1.CaptureMouse;
end;

procedure TfpgMDIChildForm.TitleMouseExit(Sender: TObject);
begin
//	FIsMouseDown := False;
end;

procedure TfpgMDIChildForm.CloseMDIWindowClicked(Sender: TObject);
begin
	Close;
end;

procedure TfpgMDIChildForm.SetActive(AValue: boolean);
begin
	if FActive = AValue then
		Exit;
	FActive := AValue;
	if FActive then
	begin
		Panel1.BackgroundColor := clNavy;
		bevLeft.BackgroundColor := clNavy;
		bevBottom.BackgroundColor := clNavy;
		bevRight.BackgroundColor := clNavy;
		Bevel2.BackgroundColor := clNavy;
		Bevel4.BackgroundColor := clNavy;
	end
	else
	begin
		Panel1.BackgroundColor := clMedGray;
		bevLeft.BackgroundColor := clMedGray;
		bevBottom.BackgroundColor := clMedGray;
		bevRight.BackgroundColor := clMedGray;
		Bevel2.BackgroundColor := clMedGray;
		Bevel4.BackgroundColor := clMedGray;
	end;
end;

procedure TfpgMDIChildForm.SetWindowTitle(AValue: TfpgString);
begin
	if FWindowTitle = AValue then
		Exit;
	FWindowTitle := AValue;
	if not (csLoading in ComponentState) then
		Panel1.Text := FWindowTitle;
end;

constructor TfpgMDIChildForm.Create(AOwner: TfpgMDIWorkArea);
begin
	inherited Create(AOwner);
	FMDIWorkArea := AOwner;
	FIsMouseDown := False;
	FLastPos := Point(0,0);
	{@VFD_BODY_BEGIN: MDIChildForm}
	Name := 'MDIChildForm';
	SetPosition(369, 166, 300, 250);
	WindowTitle := 'ChildForm1';
	Hint := '';

	Panel1 := TfpgPanel.Create(self);
	with Panel1 do
	begin
		Name := 'Panel1';
		SetPosition(0, 0, 301, 24);
		Anchors := [anLeft,anRight,anTop];
		BackgroundColor := TfpgColor($0A0081);
		FontDesc := '#Label2';
		Hint := '';
		Text := 'Window Title';
		TextColor := TfpgColor($FFFFFF);
		OnMouseDown := @TitleMouseDown;
		OnMouseUp := @TitleMouseUp;
		OnMouseMove := @TitleMouseMove;
		OnMouseExit  := @TitleMouseExit;
	end;

	bevLeft := TfpgBevel.Create(self);
	with bevLeft do
	begin
		Name := 'bevLeft';
		SetPosition(0, 24, 3, 211);
		Anchors := [anLeft,anTop,anBottom];
		BackgroundColor := TfpgColor($000080);
		Hint := '';
		Shape := bsSpacer;
	end;

	Bevel2 := TfpgBevel.Create(self);
	with Bevel2 do
	begin
		Name := 'Bevel2';
		SetPosition(0, 235, 16, 16);
		Anchors := [anLeft,anBottom];
		BackgroundColor := TfpgColor($000080);
		Hint := '';
	end;

	bevBottom := TfpgBevel.Create(self);
	with bevBottom do
	begin
		Name := 'bevBottom';
		SetPosition(16, 248, 269, 3);
		Anchors := [anLeft,anRight,anBottom];
		BackgroundColor := TfpgColor($000080);
		Hint := '';
		Shape := bsSpacer;
	end;

	Bevel4 := TfpgBevel.Create(self);
	with Bevel4 do
	begin
		Name := 'Bevel4';
		SetPosition(285, 235, 16, 16);
		Anchors := [anRight,anBottom];
		BackgroundColor := TfpgColor($000080);
		Hint := '';
	end;

	bevRight := TfpgBevel.Create(self);
	with bevRight do
	begin
		Name := 'bevRight';
		SetPosition(297, 24, 3, 211);
		Anchors := [anRight,anTop,anBottom];
		BackgroundColor := TfpgColor($000080);
		Hint := '';
		Shape := bsSpacer;
	end;

	Button1 := TfpgButton.Create(Panel1);
	with Button1 do
	begin
		Name := 'Button1';
		SetPosition(3, 4, 16, 16);
		Text := '-';
		Embedded := True;
		FontDesc := '#Grid';
		Hint := '';
		ImageName := '';
		TabOrder := 1;
		TextColor := TfpgColor($000000);
	end;

	Button2 := TfpgButton.Create(Panel1);
	with Button2 do
	begin
		Name := 'Button2';
		SetPosition(251, 4, 16, 16);
		Anchors := [anRight,anTop];
		Text := '_';
		Embedded := True;
		FontDesc := '#Grid';
		Hint := '';
		ImageName := '';
		TabOrder := 1;
		TextColor := TfpgColor($000000);
	end;

	Button3 := TfpgButton.Create(Panel1);
	with Button3 do
	begin
		Name := 'Button3';
		SetPosition(267, 4, 16, 16);
		Anchors := [anRight,anTop];
		Text := 'o';
		Embedded := True;
		FontDesc := '#Grid';
		Hint := '';
		ImageName := '';
		TabOrder := 1;
		TextColor := TfpgColor($000000);
	end;

	Button4 := TfpgButton.Create(Panel1);
	with Button4 do
	begin
		Name := 'Button4';
		SetPosition(283, 4, 16, 16);
		Anchors := [anRight,anTop];
		Text := 'X';
		Embedded := True;
		FontDesc := '#Grid';
		Hint := '';
		ImageName := '';
		TabOrder := 1;
		TextColor := TfpgColor($000000);
		OnClick := @CloseMDIWindowClicked;
	end;

	bvlClientArea := TfpgBevel.Create(self);
	with bvlClientArea do
	begin
		Name := 'bvlClientArea';
		SetPosition(2, 24, 296, 224);
		Anchors := [anLeft,anRight,anTop,anBottom];
		Hint := '';
		Shape := bsSpacer;
	end;

	{@VFD_BODY_END: MDIChildForm}
	Name := 'MDIChildForm' + IntToStr(Random(MaxInt));
end;

procedure TfpgMDIChildForm.SetClientFrame(AFrame: TfpgFrame);
begin
//	AFrame.Owner := bvlClientArea;
	AFrame.Align := alClient;
	AFrame.Visible := True;
	UpdateWindowTitle;
end;

procedure TfpgMDIChildForm.UpdateWindowTitle;
begin
	Panel1.Text := FWindowTitle;
end;

procedure TfpgMDIChildForm.Close;
begin
	// We can't free ourselves, somebody else needs to do it
	fpgPostMessage(Self, FMDIWorkArea, FPGM_FREEME);
end;

{ TfpgMDIWorkArea }

procedure TfpgMDIWorkArea.InternalMsgFreeMe(var msg: TfpgMessageRec);
var
	i: integer;
begin
	if Assigned(msg.Sender) then
	begin
		if csDestroying in TComponent(msg.Sender).ComponentState then
			Exit;
		RemoveComponent(TfpgMDIChildForm(msg.Sender));
		i := FList.IndexOf(TfpgMDIChildForm(msg.Sender));
		if i = -1 then
			raise Exception.Create('Could not find MDI Child Form');
		FList.Delete(i);
		if FList.Count >= i+1 then
			{ set focus to next child window after the one just deleted }
			ActiveWidget := TfpgMDIChildForm(FList.Items[i])
		else if FList.Count > 0 then
			{ fallback to the first child window we created }
			ActiveWidget := TfpgMDIChildForm(FList.Items[0])
		else
			{ there simply isn't any more child windows }
			ActiveWidget := nil;
		TfpgMDIChildForm(msg.Sender).Free;
	end;
end;

procedure TfpgMDIWorkArea.SetActiveWindow(AValue: TfpgMDIChildForm);
var
  i: integer;
  w: TfpgMDIChildForm;
begin
	if FActiveWindow = AValue then
		Exit;
	FActiveWindow := AValue;
	FActiveWindow.BringToFront;
	ActiveWidget := FActiveWindow;
	for i := 0 to FList.Count-1 do
	begin
		w := TfpgMDIChildForm(FList[i]);
		w.Active := (w = AValue);
	end;
end;

function TfpgMDIWorkArea.GetChildWindowCount: integer;
begin
	Result := FList.Count;
end;

procedure TfpgMDIWorkArea.HandlePaint;
begin
	inherited HandlePaint;
	Canvas.Clear(clLtGray);
end;

procedure TfpgMDIWorkArea.PositionScrollBars;
begin
	FHorBar.Left := Left;
	FHorBar.Top := Height - FHorBar.Height;
	FHorBar.Width := Width;
	FHorBar.Anchors := [anLeft, anBottom, anRight];
	FVerBar.Left := Width - FVerBar.Width;
	FVerBar.Top := 0;
	FVerBar.Height := Height;
	FVerBar.Anchors := [anRight, anTop, anBottom];
end;

constructor TfpgMDIWorkArea.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FIsContainer := True;
	FHorBar := TfpgScrollbar.Create(self);
	FHorBar.Visible := False;
	FHorBar.Orientation := orHorizontal;
	FVerBar := TfpgScrollbar.Create(self);
	FVerBar.Visible := False;
	FVerBar.Orientation := orVertical;
	PositionScrollBars;
	FList := TList.Create;
	FActiveWindow := nil;
end;

destructor TfpgMDIWorkArea.Destroy;
begin
	FList.Free;
	inherited Destroy;
end;

function TfpgMDIWorkArea.AddWindow(AWindowClass: TfpgFrameClass): TfpgFrame;
var
	frm: TfpgMDIChildForm;
begin
	frm := TfpgMDIChildForm.Create(self);
	Result := AWindowClass.Create(frm.bvlClientArea);
	frm.SetClientFrame(Result);
	FList.Add(frm);
	ActiveWindow := frm;
end;

end.

