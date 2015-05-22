{
  Copyright (C) 2009-2015 by Graeme Geldenhuys

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  
  Abstract:
    This unit adds a new project type to the Lazarus IDE.

    New Project Type:
      fpGUI Application - A pure fpGUI Toolkit based application.
      fpGUI+Agg2D Application - An fpGUI application with a TAgg2D instance.

}

unit fpGUILazIDEIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, Controls, Forms;

type

  TfpGUIApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
  end;
  
  TfpGUIAgg2dApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
  end;

var
  ProjectDescriptorfpGUIApplication: TfpGUIApplicationDescriptor;
  ProjectDescriptorfpGUIAgg2dApplication: TfpGUIAgg2dApplicationDescriptor;


procedure Register;


implementation

const
  le: string = LineEnding;


procedure Register;
begin
  ProjectDescriptorfpGUIApplication := TfpGUIApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorfpGUIApplication);

  ProjectDescriptorfpGUIAgg2dApplication := TfpGUIAgg2dApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorfpGUIAgg2dApplication);
end;

{ TfpGUIApplicationDescriptor }

constructor TfpGUIApplicationDescriptor.Create;
begin
  inherited Create;
  Name := 'fpGUI Application';
end;

function TfpGUIApplicationDescriptor.GetLocalizedName: string;
begin
  Result := 'fpGUI Toolkit Application';
end;

function TfpGUIApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result := 'fpGUI Toolkit Application'+le+le
           +'An application based purely on the fpGUI Toolkit.';
end;

function TfpGUIApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile := AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject := true;
  AProject.AddFile(MainFile, false);
  AProject.MainFileID := 0;

  // create program source
  NewSource := 'program Project1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  {$IFDEF UNIX}{$IFDEF UseCThreads}'+le
    +'  cthreads,'+le
    +'  {$ENDIF}{$ENDIF}'+le
    +'  Classes, fpg_base, fpg_main, fpg_form;'+le
    +le
    +'type'+le
    +le
    +'  TMainForm = class(TfpgForm)'+le
    +'  private'+le
    +'    {@VFD_HEAD_BEGIN: MainForm}'+le
    +'    {@VFD_HEAD_END: MainForm}'+le
    +'  public'+le
    +'    procedure AfterCreate; override;'+le
    +'  end;'+le
    +le
    +'{@VFD_NEWFORM_DECL}'+le
    +le
    +le
    +le
    +'{@VFD_NEWFORM_IMPL}'+le
    +le
    +'procedure TMainForm.AfterCreate;'+le
    +'begin'+le
    +'  {%region ''Auto-generated GUI code'' -fold}' + le
    +'  {@VFD_BODY_BEGIN: MainForm}'+le
    +'  Name := ''MainForm'';'+le
    +'  SetPosition(316, 186, 300, 250);'+le
    +'  WindowTitle := ''MainForm'';'+le
    +le
    +'  {@VFD_BODY_END: MainForm}'+le
    +'  {%endregion}' + le
    +'end;'+le
    +le
    +le
    +'procedure MainProc;'+le
    +'var'+le
    +'  frm: TMainForm;'+le
    +'begin'+le
    +'  fpgApplication.Initialize;'+le
    +'  fpgApplication.CreateForm(TMainForm, frm);'+le
    +'  try'+le
    +'    frm.Show;'+le
    +'    fpgApplication.Run;'+le
    +'  finally'+le
    +'    frm.Free;'+le
    +'  end;'+le
    +'end;'+le
    +le
    +'begin'+le
    +'  MainProc;'+le
    +'end.'+le
    +le;
    
  AProject.MainFile.SetSourceText(NewSource);
  AProject.AddPackageDependency('fpgui_toolkit');
  // compiler options
  AProject.LazCompilerOptions.UseLineInfoUnit := True;

  Result := mrOK;
end;

{ TfpGUIAgg2dApplicationDescriptor }

constructor TfpGUIAgg2dApplicationDescriptor.Create;
begin
  inherited Create;
  Name := 'fpGUI+Agg2D Application';
end;

function TfpGUIAgg2dApplicationDescriptor.GetLocalizedName: string;
begin
  Result := 'fpGUI+Agg2D Application';
end;

function TfpGUIAgg2dApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result := 'fpGUI+Agg2D Application'+le+le
           +'An application based on the fpGUI Toolkit'+le
           +'and uses Agg2D to render to an image buffer. Great '
           +'for quick demos.';
end;

function TfpGUIAgg2dApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile := AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject := true;
  AProject.AddFile(MainFile, false);
  AProject.MainFileID := 0;

  // create program source
  NewSource := 'program Project1;'+le
    +le
    +'uses'+le
    +'  {$IFDEF UNIX}{$IFDEF UseCThreads}'+le
    +'  cthreads,'+le
    +'  {$ENDIF}{$ENDIF}'+le
    +'  Classes, SysUtils,'+le
    +'  fpg_base, fpg_main, fpg_form, Agg2D;'+le
    +le
    +'type'+le
    +''+le
    +'  TMainForm = class(TfpgForm)'+le
    +'  private'+le
    +'    {@VFD_HEAD_BEGIN: MainForm}'+le
    +'    {@VFD_HEAD_END: MainForm}'+le
    +'    FImg: TfpgImage;'+le
    +'    FAgg2D: TAgg2D;'+le
    +'    procedure InitComposedImage;'+le
    +'    procedure FormCreate(Sender: TObject);'+le
    +'    procedure DoAggPainting;'+le
    +'    procedure FormPaint(Sender: TObject);'+le
    +'  public'+le
    +'    destructor Destroy; override;'+le
    +'    procedure AfterCreate; override;'+le
    +'  end;'+le
    +le
    +'{@VFD_NEWFORM_DECL}'+le
    +le
    +le
    +le
    +'{@VFD_NEWFORM_IMPL}'+le
    +''+le
    +'procedure TMainForm.DoAggPainting;'+le
    +'begin'+le
    +'  // **** DO YOUR AGG2D PAINTING HERE ****'+le
    +le
    +'  // Paint composedimage white'+le
    +'  FAgg2D.ClearAll(255, 255, 255);'+le
    +'  // So some advanced painting to the ComposedImage'+le
    +'  FAgg2D.LineWidth(10);'+le
    +'  FAgg2D.LineColor($32, $cd, $32);'+le
    +'  FAgg2D.FillColor($ff, $d7, $00);'+le
    +'  FAgg2D.Star(100, 100, 30, 70, 55, 5);'+le
    +'end;'+le
    +le
    +'procedure TMainForm.InitComposedImage;'+le
    +'begin'+le
    +'  FImg := TfpgImage.Create;'+le
    +'  FImg.AllocateImage(32, Width, Height);'+le
    +'  FAgg2D.Attach(FImg);'+le
    +'end;'+le
    +le
    +'procedure TMainForm.FormCreate(Sender: TObject);'+le
    +'begin'+le
    +'  FAgg2D := TAgg2D.Create(self);'+le
    +'  InitComposedImage;'+le
    +'  DoAggPainting;'+le
    +'end;'+le
    +le
    +'procedure TMainForm.FormPaint(Sender: TObject);'+le
    +'begin'+le
    +'  // Finalise image internals, then paint it to the Window'+le
    +'  FImg.UpdateImage;'+le
    +'  Canvas.DrawImage(0, 0, FImg);'+le
    +'end;'+le
    +le
    +'destructor TMainForm.Destroy;'+le
    +'begin'+le
    +'  FAgg2D.Free;'+le
    +'  FImg.Free;'+le
    +'  inherited Destroy;'+le
    +'end;'+le
    +le
    +'procedure TMainForm.AfterCreate;'+le
    +'begin'+le
    +'  {%region ''Auto-generated GUI code''}'+le
    +'  {@VFD_BODY_BEGIN: MainForm}'+le
    +'  Name := ''MainForm'';'+le
    +'  SetPosition(316, 186, 501, 450);'+le
    +'  WindowTitle := ''TAgg2D.Attach() demo'';'+le
    +'  Hint := '''';'+le
    +'  WindowPosition := wpOneThirdDown;'+le
    +'  OnPaint := @FormPaint;'+le
    +'  OnCreate := @FormCreate;'+le
    +'  {@VFD_BODY_END: MainForm}'+le
    +'  {%endregion}'+le
    +'end;'+le
    +le
    +le
    +'procedure MainProc;'+le
    +'var'+le
    +'  frm: TMainForm;'+le
    +'begin'+le
    +'  fpgApplication.Initialize;'+le
    +'  fpgApplication.CreateForm(TMainForm, frm);'+le
    +'  try'+le
    +'    frm.Show;'+le
    +'    fpgApplication.Run;'+le
    +'  finally'+le
    +'    frm.Free;'+le
    +'  end;'+le
    +'end;'+le
    +le
    +'begin'+le
    +'  MainProc;'+le
    +'end.'+le;

  AProject.MainFile.SetSourceText(NewSource);
  AProject.AddPackageDependency('fpgui_toolkit');
  // compiler options
  AProject.LazCompilerOptions.UseLineInfoUnit := True;

  Result := mrOK;
end;

end.

