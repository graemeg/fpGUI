{$ifndef READ_IMPLEMENTATION}

type
  TFWidget = class(TFWindow)
    constructor Create(AParent: TFCustomWindow); virtual;
  end;

{$else}

constructor TFWidget.Create(AParent: TFCustomWindow);
begin
  if (AParent = nil) then inherited Create(nil, [woWindow])
  else inherited Create(AParent, [woChildWindow]);

  SetClientSize(Size(125, 125));
  SetMinMaxClientSize(Size(125, 125), Size(125, 125));
end;

{$endif}

