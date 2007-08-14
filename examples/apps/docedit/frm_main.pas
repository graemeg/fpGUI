unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpgfx,
  gui_form,
  gui_button,
  gui_edit,
  gui_label;

type
  TMainForm = class(TfpgForm)
  private
    btnQuit: TfpgButton;
    lblXMLFile: TfpgLabel;
    edXMLFile: TfpgEdit;
    btnOpen: TfpgButton;
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnOpenClicked(Sender: TObject);
    procedure   InitializeComponents;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

implementation

{ TMainForm }

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnOpenClicked(Sender: TObject);
begin
  // open xml file
end;

procedure TMainForm.InitializeComponents;
begin
  btnQuit := CreateButton(self, Width-88, Height-31, 80, 'Quit', @btnQuitClicked);
  with btnQuit do
  begin
    ImageName := 'stdimg.Quit';
    ShowImage := True;
    Anchors   := [anRight, anBottom];
  end;
  
  lblXMLFile := CreateLabel(self, 8, 8, 'XML File:');
  edXMLFile  := CreateEdit(self, lblXMLFile.Right+8, lblXMLFile.Top-2, 485, 23);
  edXMLFile.Text := '/home/graemeg/programming/fpgui/docs/xml/corelib/gfx_clipboard.xml';
  
  btnOpen := CreateButton(self, edXMLFile.Right+10, edXMLFile.Top, 80, 'Open', @btnOpenClicked);
  with btnOpen do
  begin
    ImageName := 'stdimg.Open';
    ShowImage := True;
  end;

end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Documentation Editor';
  Sizeable := False;
  // Golden ratio 1.618
  Width   := 650;
  Height  := 402;

  InitializeComponents;
end;

end.

