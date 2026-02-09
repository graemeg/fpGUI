  pmTray := TfpgPopupMenu.Create(self);
  with pmTray do
  begin
    Name := 'pmTray';
    SetPosition(256, 128, 120, 20);
    AddMenuItem('Start Timer...', '', nil);
    AddMenuItem('Stop Timer...', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Show Form', '', @miShowFormClicked);
    AddMenuItem('-', '', nil);
    AddMenuItem('Quit', '', @miFileQuitClicked);
  end;

  TrayIcon := TfpgSystemTrayIcon.Create(self);
  with TrayIcon do
  begin
    Name := 'TrayIcon';
    SetPosition(256, 240, 20, 20);
    PopupMenu := pmTray;
    ImageName := 'stdimg.folderhome';
  end;
