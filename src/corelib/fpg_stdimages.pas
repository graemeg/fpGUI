{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Standard fpGUI images that will be used in things like Message,
      FileOpen, FileSave dialogs, Buttons, etc..
}

unit fpg_stdimages;

{$mode objfpc}{$H+}

interface

uses
  fpg_main;

procedure fpgCreateStandardImages;

implementation

{$I stdimages.inc}

Const
  // 60x12 pixel 4bpp bitmap
(*
  RadioBitmapData: array[0..359] of Byte = (
    $00, $00, $22, $22, $00, $00, $00, $00, $22, $22, $00, $00, $00, $00, $22, $22, $00, $00, $00, $00, $22, $22, $00, $00, $00, $00, $22, $22, $00, $00,
    $00, $22, $11, $11, $22, $00, $00, $22, $11, $11, $22, $00, $00, $22, $11, $11, $22, $00, $00, $22, $11, $11, $22, $00, $00, $22, $11, $11, $22, $00,
    $02, $11, $77, $77, $11, $50, $02, $11, $77, $77, $11, $50, $02, $11, $33, $33, $11, $50, $02, $11, $33, $33, $11, $50, $02, $11, $33, $33, $11, $50,
    $02, $17, $77, $77, $74, $50, $02, $17, $77, $77, $74, $50, $02, $13, $33, $33, $34, $50, $02, $13, $33, $33, $34, $50, $02, $13, $33, $33, $34, $50,
    $21, $77, $77, $77, $77, $45, $21, $77, $76, $67, $77, $45, $21, $33, $33, $33, $33, $45, $21, $33, $36, $63, $33, $45, $21, $33, $32, $23, $33, $45,
    $21, $77, $77, $77, $77, $45, $21, $77, $66, $66, $77, $45, $21, $33, $33, $33, $33, $45, $21, $33, $66, $66, $33, $45, $21, $33, $22, $22, $33, $45,
    $21, $77, $77, $77, $77, $45, $21, $77, $66, $66, $77, $45, $21, $33, $33, $33, $33, $45, $21, $33, $66, $66, $33, $45, $21, $33, $22, $22, $33, $45,
    $21, $77, $77, $77, $77, $45, $21, $77, $76, $67, $77, $45, $21, $33, $33, $33, $33, $45, $21, $33, $36, $63, $33, $45, $21, $33, $32, $23, $33, $45,
    $02, $17, $77, $77, $74, $50, $02, $17, $77, $77, $74, $50, $02, $13, $33, $33, $34, $50, $02, $13, $33, $33, $34, $50, $02, $13, $33, $33, $34, $50,
    $02, $44, $77, $77, $44, $50, $02, $44, $77, $77, $44, $50, $02, $44, $33, $33, $44, $50, $02, $44, $33, $33, $44, $50, $02, $44, $33, $33, $44, $50,
    $00, $55, $44, $44, $55, $00, $00, $55, $44, $44, $55, $00, $00, $55, $44, $44, $55, $00, $00, $55, $44, $44, $55, $00, $00, $55, $44, $44, $55, $00,
    $00, $00, $55, $55, $00, $00, $00, $00, $55, $55, $00, $00, $00, $00, $55, $55, $00, $00, $00, $00, $55, $55, $00, $00, $00, $00, $55, $55, $00, $00
  );
*)
  // 12x12 pixel monochrome bitmap
  RadioMaskData: array[0..23] of Byte = ($0f, $00, $3f, $c0, $7f, $e0, $7f,
    $e0, $ff, $f0, $ff, $f0, $ff, $f0, $ff, $f0, $7f, $e0, $7f, $e0, $3f, $c0,
    $0f, $00);

  // 64x8 pixel 4bpp bitmap
  ArrowBitmapData: array[0..255] of Byte = (
    $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $13, $33, $33, $31, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $23, $33, $33, $23, $33, $33,
    $33, $33, $33, $33, $33, $33, $33, $33, $33, $31, $13, $33, $33, $31, $13, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $32, $25, $33, $33, $22, $33, $33,
    $33, $31, $33, $33, $11, $11, $11, $13, $33, $11, $13, $33, $33, $31, $11, $33, $33, $32, $33, $33, $22, $22, $22, $23, $33, $22, $25, $33, $33, $22, $23, $33,
    $33, $11, $13, $33, $31, $11, $11, $33, $31, $11, $13, $33, $33, $31, $11, $13, $33, $22, $23, $33, $32, $22, $22, $55, $32, $22, $25, $33, $33, $22, $22, $33,
    $31, $11, $11, $33, $33, $11, $13, $33, $33, $11, $13, $33, $33, $31, $11, $33, $32, $22, $22, $33, $33, $22, $25, $53, $33, $22, $25, $33, $33, $22, $25, $53,
    $11, $11, $11, $13, $33, $31, $33, $33, $33, $31, $13, $33, $33, $31, $13, $33, $22, $22, $22, $23, $33, $32, $55, $33, $33, $32, $25, $33, $33, $22, $55, $33,
    $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $13, $33, $33, $31, $33, $33, $35, $55, $55, $55, $33, $33, $53, $33, $33, $33, $25, $33, $33, $25, $53, $33,
    $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $35, $33, $33, $35, $33, $33
  );



procedure fpgCreateStandardImages;
begin
  // system images. Change these to the composite arrow bmp that includes
  // disabled state
  fpgImages.AddMaskedBMP(  // 7x4 image
            'sys.sb.up',
            @stdimg_arrow_up,
      sizeof(stdimg_arrow_up), 0, 0);

  fpgImages.AddMaskedBMP(  // 7x4 image
            'sys.sb.down',
            @stdimg_arrow_down,
      sizeof(stdimg_arrow_down), 0, 3);

  fpgImages.AddMaskedBMP(  // 4x7 image
            'sys.sb.left',
            @stdimg_arrow_left,
      sizeof(stdimg_arrow_left), 0, 0);

  fpgImages.AddMaskedBMP(  // 4x7 image
            'sys.sb.right',
            @stdimg_arrow_right,
      sizeof(stdimg_arrow_right), 3, 0);
      
  fpgImages.AddMaskedBMP(  // 60x12 in total.  5 images of 12x12 each.
            'sys.radiobuttons',
            @stdimg_radiobuttons,
      sizeof(stdimg_radiobuttons), 0,0);

  fpgImages.AddBMP(  // 65x13 pixels. 5 images of 13x13 each.
            'sys.checkboxes',
            @stdimg_checkboxes,
      sizeof(stdimg_checkboxes));

  fpgImages.AddMaskedBMP(
            'stdimg.ellipse',
            @stdimg_ellipse,
      sizeof(stdimg_ellipse), 0,0);


  // General purpose images:
  fpgImages.AddMaskedBMP(
            'stdimg.ok',
            @stdimg_btn_ok_16,
      sizeof(stdimg_btn_ok_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.cancel',
            @stdimg_btn_cancel_16,
      sizeof(stdimg_btn_cancel_16), 0,0);
            
  fpgImages.AddMaskedBMP(
            'stdimg.yes',
            @stdimg_choice_yes_16,
      sizeof(stdimg_choice_yes_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.no',
            @stdimg_choice_no_16,
      sizeof(stdimg_choice_no_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.close',
            @stdimg_btn_close_16,
      sizeof(stdimg_btn_close_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.quit',
            @stdimg_menu_quit_16,
      sizeof(stdimg_menu_quit_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.exit',
            @stdimg_menu_exit_16,
      sizeof(stdimg_menu_exit_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.configure',
            @stdimg_menu_preferences_16,
      sizeof(stdimg_menu_preferences_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.check',
            @stdimg_menu_check_16,
      sizeof(stdimg_menu_check_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.document',
            @stdimg_document,
      sizeof(stdimg_document), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.search',
            @stdimg_search_16,
      sizeof(stdimg_search_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.refresh',
            @stdimg_refresh_16,
      sizeof(stdimg_refresh_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.font',
            @stdimg_font_16,
      sizeof(stdimg_font_16), 15,0);

  fpgImages.AddMaskedBMP(
            'stdimg.copy',
            @stdimg_edit_copy_16,
      sizeof(stdimg_edit_copy_16), 15,0);

  fpgImages.AddMaskedBMP(
            'stdimg.cut',
            @stdimg_edit_cut_16,
      sizeof(stdimg_edit_cut_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.paste',
            @stdimg_edit_paste_16,
      sizeof(stdimg_edit_paste_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.delete',
            @stdimg_edit_delete_16,
      sizeof(stdimg_edit_delete_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.edit',
            @stdimg_edit,
      sizeof(stdimg_edit), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.folder',
            @stdimg_folder_16,
      sizeof(stdimg_folder_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.foldernew',
            @stdimg_folder_new_16,
      sizeof(stdimg_folder_new_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.folderopen',
            @stdimg_folder_open_16,
      sizeof(stdimg_folder_open_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.folderup',
            @stdimg_folder_up_16,
      sizeof(stdimg_folder_up_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.folderfile',
            @stdimg_folder_open_file_16,
      sizeof(stdimg_folder_open_file_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.folderhome',
            @stdimg_folder_home_16,
      sizeof(stdimg_folder_home_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.bookmark',
            @stdimg_bookmark_16,
      sizeof(stdimg_bookmark_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.open',
            @stdimg_folder_open_16,
      sizeof(stdimg_folder_open_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.save',
            @stdimg_menu_save_16,
      sizeof(stdimg_menu_save_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.saveas',
            @stdimg_menu_saveas_16,
      sizeof(stdimg_menu_saveas_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.saveall',
            @stdimg_menu_save_all_16,
      sizeof(stdimg_menu_save_all_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.help',
            @stdimg_help_16,
      sizeof(stdimg_help_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.about',
            @stdimg_about_16,
      sizeof(stdimg_about_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.hidden',
            @stdimg_hidden,
      sizeof(stdimg_hidden), 0,0);

  fpgImages.AddBMP(
            'stdimg.link',
            @stdimg_link,
      sizeof(stdimg_link));
      
  fpgImages.AddMaskedBMP(
            'stdimg.add',
            @stdimg_list_add_16,
      sizeof(stdimg_list_add_16), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.remove',
            @stdimg_list_remove_16,
      sizeof(stdimg_list_remove_16), 0,0);
            
  fpgImages.AddMaskedBMP(
            'stdimg.executable',
            @stdimg_executable_16,
      sizeof(stdimg_executable_16), 0,0);


  // Dialog icons
  fpgImages.AddMaskedBMP(
            'stdimg.dlg.help',
            @stdimg_dialog_confirmation_32,
      sizeof(stdimg_dialog_confirmation_32), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.dlg.info',
            @stdimg_dialog_information_32,
      sizeof(stdimg_dialog_information_32), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.dlg.warning',
            @stdimg_dialog_warning_32,
      sizeof(stdimg_dialog_warning_32), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.dlg.critical',
            @stdimg_dialog_error_32,
      sizeof(stdimg_dialog_error_32), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.print',
            @stdimg_print,
      sizeof(stdimg_print), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.Adobe_pdf',
            @stdimg_Adobe_pdf,
      sizeof(stdimg_Adobe_pdf), 0,0);

  fpgImages.AddMaskedBMP(
            'stdimg.preview',
            @stdimg_preview,
      sizeof(stdimg_preview), 0,0);

{
  Here is a template for more images

  fpgImages.AddMaskedBMP(
            'stdimg.',
            @stdimg_,
      sizeof(stdimg_), 0,0);
}
end;

end.

