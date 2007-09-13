#!/bin/sh
# fpc/bin must be in your PATH
fpdoc --package=GUI \
  --format=html \
  --output=html/gui/  \
  --content=html/gui.cnt \
  --import=html/corelib.cnt,../corelib/ \
  --descr=xml/gui/gui_basegrid.xml --input='-Fi../src/gui ../src/gui/gui_basegrid.pas' \
  --descr=xml/gui/gui_bevel.xml --input='-Fi../src/gui ../src/gui/gui_bevel.pas' \
  --descr=xml/gui/gui_button.xml --input='-Fi../src/gui ../src/gui/gui_button.pas' \
  --descr=xml/gui/gui_checkbox.xml --input='-Fi../src/gui ../src/gui/gui_checkbox.pas' \
  --descr=xml/gui/gui_combobox.xml --input='-Fi../src/gui ../src/gui/gui_combobox.pas' \
  --descr=xml/gui/gui_customgrid.xml --input='-Fi../src/gui ../src/gui/gui_customgrid.pas' \
  --descr=xml/gui/gui_dialogs.xml --input='-Fi../src/gui ../src/gui/gui_dialogs.pas' \
  --descr=xml/gui/gui_edit.xml --input='-Fi../src/gui ../src/gui/gui_edit.pas' \
  --descr=xml/gui/gui_form.xml --input='-Fi../src/gui ../src/gui/gui_form.pas' \
  --descr=xml/gui/gui_grid.xml --input='-Fi../src/gui ../src/gui/gui_grid.pas' \
  --descr=xml/gui/gui_label.xml --input='-Fi../src/gui ../src/gui/gui_label.pas' \
  --descr=xml/gui/gui_listbox.xml --input='-Fi../src/gui ../src/gui/gui_listbox.pas' \
  --descr=xml/gui/gui_listview.xml --input='-Fi../src/gui ../src/gui/gui_listview.pas' \
  --descr=xml/gui/gui_memo.xml --input='-Fi../src/gui ../src/gui/gui_memo.pas' \
  --descr=xml/gui/gui_menu.xml --input='-Fi../src/gui ../src/gui/gui_menu.pas' \
  --descr=xml/gui/gui_progressbar.xml --input='-Fi../src/gui ../src/gui/gui_progressbar.pas' \
  --descr=xml/gui/gui_radiobutton.xml --input='-Fi../src/gui ../src/gui/gui_radiobutton.pas' \
  --descr=xml/gui/gui_scrollbar.xml --input='-Fi../src/gui ../src/gui/gui_scrollbar.pas' \
  --descr=xml/gui/gui_style.xml --input='-Fi../src/gui ../src/gui/gui_style.pas' \
  --descr=xml/gui/gui_tab.xml --input='-Fi../src/gui ../src/gui/gui_tab.pas' \
  --descr=xml/gui/gui_trackbar.xml --input='-Fi../src/gui ../src/gui/gui_trackbar.pas' \
  --descr=xml/gui/gui_iniutils.xml --input='-Fi../src/gui ../src/gui/gui_iniutils.pas'


#  --descr=xml/gui/gui_.xml --input='-Fi../src/gui ../src/gui/gui_.pas' \

