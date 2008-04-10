unit view_impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, gui_listbox, gui_combobox, basic_intf;
  
type

  TListBoxView = class(TfpgListBox, IObserver)
  private
    procedure IObserver.Update = ObserverUpdate;
    procedure ObserverUpdate(Subject: IInterface);
  end;


  TComboBoxView = class(TfpgComboBox, IObserver)
  private
    procedure IObserver.Update = ObserverUpdate;
    procedure ObserverUpdate(Subject: IInterface);
  end;


implementation

{ TListBoxView }

procedure TListBoxView.ObserverUpdate(Subject: IInterface);
var
  Obj: IListModel;
  i: integer;
begin
  Subject.QueryInterface(IListModel, Obj);
  if Obj <> nil then
  begin
    Items.BeginUpdate;
    Items.Clear;
//    for i := 0 to Obj.Count-1 do
//      Items.Add(Obj.Item[i]);
    Items.EndUpdate;
  end;
end;

{ TComboBoxView }

procedure TComboBoxView.ObserverUpdate(Subject: IInterface);
var
  Obj: IListModel;
  i: integer;
begin
  Subject.QueryInterface(IListModel, Obj);
  if Obj <> nil then
  begin
    Items.BeginUpdate;
    Items.Clear;
//    for i := 0 to Obj.Count-1 do
//      Items.Add(Obj.Item[i]);
    FocusItem := 1;
    Items.EndUpdate;
  end;
end;

end.

