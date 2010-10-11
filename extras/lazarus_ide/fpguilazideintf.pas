{
  Copyright (C) 2009 by Graeme Geldenhuys

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
      fpGUI Application - A Free Pascal program for fpGUI Toolkit.
      
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
//    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;
  

var
  ProjectDescriptorfpGUIApplication: TfpGUIApplicationDescriptor;

procedure Register;


implementation


procedure Register;
begin
  ProjectDescriptorfpGUIApplication := TfpGUIApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorfpGUIApplication);
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
var
  le: string;
begin
  le := System.LineEnding;
  Result := 'fpGUI Toolkit Application'+le+le
           +'An application based on the fpGUI Toolkit.'+le
           +'The program file is automatically maintained by Lazarus.';
end;

function TfpGUIApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  le: string;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile := AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject := true;
  AProject.AddFile(MainFile, false);
  AProject.MainFileID := 0;

  // create program source
  le := LineEnding;
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
    +'  frm := TMainForm.Create(nil);'+le
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

  // add
  AProject.AddPackageDependency('fpgui_toolkit');

  // compiler options
  AProject.LazCompilerOptions.UseLineInfoUnit := True;
//  AProject.LazCompilerOptions.CustomOptions := '-FUunits';

  Result := mrOK;
end;

{
function TfpGUIApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorfpGUIUnit,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result:=mrOK;
end;
}

end.

