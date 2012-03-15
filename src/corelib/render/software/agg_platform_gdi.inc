{%mainunit Agg2D.pas}

{$ifdef uses_interface}
  Windows,
{$endif}


{$ifdef agg_platform_interface}
  protected
//    m_ximg_window : PXImage;
{$endif}


{$ifdef uses_implementation}
  // nothing
{$endif}


{$ifdef agg_platform_implementation}

procedure TAgg2D.DoBeginDraw(awin: TfpgWindowBase; buffered: boolean);
begin

end;

procedure TAgg2D.DoEndDraw;
begin

end;

{$endif}

