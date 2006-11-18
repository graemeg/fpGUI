object ListBoxForm: TListBoxForm
  BorderWidth = 8
  Text = 'List box test'
  object ListBox: TListBox
    Items.Strings = (
      'procedure KeyPressed(var Event: TXKeyPressedEvent); message X.KeyPress;'
      'procedure KeyReleased(var Event: TXKeyReleasedEvent); message X.KeyRelease;'
      'procedure ButtonPressed(var Event: TXButtonPressedEvent); message X.ButtonPress;'
      'procedure ButtonReleased(var Event: TXButtonReleasedEvent); message X.ButtonRelease;'
      'procedure EnterWindow(var Event :TXEnterWindowEvent); message X.EnterNotify;'
      'procedure LeaveWindow(var Event :TXLeaveWindowEvent); message X.LeaveNotify;'
      'procedure PointerMoved(var Event: TXPointerMovedEvent); message X.MotionNotify;'
      'procedure Expose(var Event: TXExposeEvent); message X.Expose;'
      'procedure FocusIn(var Event: TXFocusInEvent); message X.FocusIn;'
      'procedure FocusOut(var Event: TXFocusOutEvent); message X.FocusOut;'
      'procedure Map(var Event: TXMapEvent); message X.MapNotify;'
      'procedure Unmap(var Event: TXUnmapEvent); message X.UnmapNotify;'
      'procedure Reparent(var Event: TXReparentEvent); message X.ReparentNotify;'
      'procedure DestroyWindow(var Event: TXDestroyWindowEvent); message X.DestroyNotify;'
      'procedure Configure(var Event: TXConfigureEvent); message X.ConfigureNotify;'
      'procedure ClientMessage(var Event: TXClientMessageEvent); message X.ClientMessage;')
  end
end
