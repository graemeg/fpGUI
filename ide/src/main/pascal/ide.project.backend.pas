{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Abstract project backend interface. Concrete implementations
      provide support for different project formats (legacy .project
      INI files, PasBuild project.xml, etc).
}

unit ide.project.backend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ide.project.unitlist, fpg_base;

type
  TProjectFormat = (pfLegacy, pfPasBuild);

  TIDEProjectBackend = class(TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
    { Persistence }
    function    Load(const AProjectFile: TfpgString): Boolean; virtual; abstract;
    function    Save(const AFile: TfpgString = ''): Boolean; virtual; abstract;
    { Build }
    function    GenerateCmdLine(const AShowOnly: Boolean = False; const ABuildMode: integer = -1): TfpgString; virtual; abstract;
    { Project identity }
    function    GetProjectName: TfpgString; virtual; abstract;
    procedure   SetProjectName(const AValue: TfpgString); virtual; abstract;
    function    GetProjectDir: TfpgString; virtual; abstract;
    procedure   SetProjectDir(const AValue: TfpgString); virtual; abstract;
    function    GetMainSource: TfpgString; virtual; abstract;
    procedure   SetMainSource(const AValue: TfpgString); virtual; abstract;
    function    GetTargetFile: TfpgString; virtual; abstract;
    procedure   SetTargetFile(const AValue: TfpgString); virtual; abstract;
    function    GetUnitOutputDir: TfpgString; virtual; abstract;
    procedure   SetUnitOutputDir(const AValue: TfpgString); virtual; abstract;
    { Unit management }
    function    GetUnitList: TUnitList; virtual; abstract;
    function    GetUnitDirs: TStringList; virtual; abstract;
    { Metadata }
    function    GetProjectFormat: TProjectFormat; virtual; abstract;
    { Properties }
    property    ProjectName: TfpgString read GetProjectName write SetProjectName;
    property    ProjectDir: TfpgString read GetProjectDir write SetProjectDir;
    property    MainUnit: TfpgString read GetMainSource write SetMainSource;
    property    TargetFile: TfpgString read GetTargetFile write SetTargetFile;
    property    UnitOutputDir: TfpgString read GetUnitOutputDir write SetUnitOutputDir;
    property    UnitList: TUnitList read GetUnitList;
    property    UnitDirs: TStringList read GetUnitDirs;
    property    ProjectFormat: TProjectFormat read GetProjectFormat;
  end;

  TIDEProjectBackendClass = class of TIDEProjectBackend;


implementation


{ TIDEProjectBackend }

constructor TIDEProjectBackend.Create;
begin
  inherited Create;
end;

destructor TIDEProjectBackend.Destroy;
begin
  inherited Destroy;
end;


end.
