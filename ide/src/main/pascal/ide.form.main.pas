{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Maximus IDE is an example application, to showcase a bit more of
      what fpGUI can do in a larger project. It also ties in a lot of
      various fpGUI widgets and framework functionality.
}

unit ide.form.main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_menu, fpg_panel,
  fpg_button, fpg_splitter, fpg_tab, fpg_memo, fpg_label, fpg_grid,
  fpg_tree, fpg_textedit, fpg_imagelist, fpg_mru,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc,
  ide.filemonitor, ide.highlighter, ide.editor.theme, ide.bracketmatch,
  ide.highlight.renderer, ide.build.dispatch, ide.projecttree,
  ide.editor.tabs, ide.profiles, ide.project.pasbuild,
  ide.cursorhistory, ide.filefinder, ide.form.filefinder,
  ide.symbolfinder, ide.form.symbolfinder;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    mainmenu: TfpgMenuBar;
    Toolbar: TfpgBevel;
    btnQuit: TfpgButton;
    btnOpen: TfpgButton;
    btnSave: TfpgButton;
    btnSaveAll: TfpgButton;
    pnlStatusBar: TfpgBevel;
    lblStatus: TfpgLabel;
    lblCursorPos: TfpgLabel;
    lblGitBranch: TfpgLabel;
    lblProfiles: TfpgLabel;
    pnlClientArea: TfpgBevel;
    pnlWindow: TfpgPageControl;
    tsMessages: TfpgTabSheet;
    grdMessages: TfpgStringGrid;
    tsScribble: TfpgTabSheet;
    memScribble: TfpgMemo;
    tsTerminal: TfpgTabSheet;
    pnlTool: TfpgPageControl;
    tsProject: TfpgTabSheet;
    tvProject: TfpgTreeView;
    tsFiles: TfpgTabSheet;
    grdFiles: TfpgFileGrid;
    SplitterV: TfpgMigSplitter;
    SplitterH: TfpgMigSplitter;
    pcEditor: TfpgPageControl;
    tseditor: TfpgTabSheet;
    TextEditor: TfpgTextEdit;
    mnuFile: TfpgPopupMenu;
    mnuEdit: TfpgPopupMenu;
    mnuSearch: TfpgPopupMenu;
    mnuView: TfpgPopupMenu;
    mnuProject: TfpgPopupMenu;
    mnuRun: TfpgPopupMenu;
    mnuTools: TfpgPopupMenu;
    mnuSettings: TfpgPopupMenu;
    mnuHelp: TfpgPopupMenu;
    {@VFD_HEAD_END: MainForm}
    pmOpenRecentMenu: TfpgPopupMenu;
    pmTabMenu: TfpgPopupMenu;
    pmModuleMenu: TfpgPopupMenu;
    pmProjectTreeMenu: TfpgPopupMenu;
    pmProfileMenu: TfpgPopupMenu;
    FProfileStateImages: TfpgImageList;
    FLastTabClickPos: TPoint;
    miFile: TfpgMenuItem;
    miRecentProjects: TfpgMenuItem;
    FRecentFiles: TfpgMRU;
    FTheme: TEditorTheme;
    FFileMonitor: TFileMonitor;
    FHighlightCache: THighlighterCache;
    FBracketMatch: TBracketMatchResult;
    FStatusBarLayout: TfpgMigLayoutManager;
    FCursorHistory: TCursorHistory;
    FLastSearchText: TfpgString;
    FLastFindOptions: TfpgFindOptions;
    FLastFindBackward: Boolean;
    FLastFileDir: TfpgString;
    procedure   MonitoredFileChanged(Sender: TObject; AData: TFileMonitorEventData);
    procedure   FormShow(Sender: TObject);
    procedure   FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnOpenFileClicked(Sender: TObject);
    procedure   miFileNewUnit(Sender: TObject);
    procedure   miFileSave(Sender: TObject);
    procedure   miFileSaveAs(Sender: TObject);
    procedure   miFileClose(Sender: TObject);
    procedure   miEditUndoClicked(Sender: TObject);
    procedure   miEditRedoClicked(Sender: TObject);
    procedure   miEditCutClicked(Sender: TObject);
    procedure   miEditCopyClicked(Sender: TObject);
    procedure   miEditPasteClicked(Sender: TObject);
    procedure   miEditDuplicateLineClicked(Sender: TObject);
    procedure   miEditDeleteLineClicked(Sender: TObject);
    procedure   miFindClicked(Sender: TObject);
    procedure   miFindNextClicked(Sender: TObject);
    procedure   miFindPrevClicked(Sender: TObject);
    procedure   miGoToLineClick(Sender: TObject);
    procedure   miSearchProcedureList(Sender: TObject);
    procedure   miAboutFPGuiClicked(Sender: TObject);
    procedure   miAboutIDE(Sender: TObject);
    procedure   miRunMake(Sender: TObject);
    procedure   miRunBuild(Sender: TObject);
    procedure   miRunMake1(Sender: TObject);
    procedure   miRunMake2(Sender: TObject);
    procedure   miRunMake3(Sender: TObject);
    procedure   miRunMake4(Sender: TObject);
    procedure   miRunClean(Sender: TObject);
    procedure   miRunRebuild(Sender: TObject);
    procedure   miRunTest(Sender: TObject);
    procedure   StartBuildGoal(const AGoal: string);
    procedure   miProjectDependencyTree(Sender: TObject);
    procedure   pmTreeDependencyTreeClick(Sender: TObject);
    procedure   miConfigureIDE(Sender: TObject);
    procedure   miViewDebug(Sender: TObject);
    procedure   miProjectNew(Sender: TObject);
    procedure   miProjectNewFromTemplate(Sender: TObject);
    procedure   miProjectOptions(Sender: TObject);
    procedure   miProjectOpen(Sender: TObject);
    procedure   miRecentProjectsClick(Sender: TObject; const FileName: String);
    procedure   miProjectSave(Sender: TObject);
    procedure   miProjectSaveAs(Sender: TObject);
    procedure   AddUnitToProject(const AUnitName: TfpgString);
    procedure   miProjectAddUnitToProject(Sender: TObject);
    procedure   tvProjectDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   tvProjectKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   grdMessageKeyPressed(Sender: TObject; var KeyCode: Word; var ShiftState: TShiftState; var Consumed: Boolean);
    procedure   pcEditorMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   pmTabCloseClick(Sender: TObject);
    procedure   pmTabCloseOthersClick(Sender: TObject);
    procedure   pmTabCloseAllClick(Sender: TObject);
    procedure   pmTabCopyPathClick(Sender: TObject);
    procedure   pmModuleBuildClick(Sender: TObject);
    procedure   pmModuleCleanClick(Sender: TObject);
    procedure   pmModuleRebuildClick(Sender: TObject);
    procedure   EditorChanged(Sender: TObject);
    procedure   EditorTabChanged(Sender: TObject; ATabSheet: TfpgTabSheet);
    procedure   RetokeniseEditor(AEditor: TfpgTextEdit);
    procedure   EditorCaretChanged(Sender: TObject; ALine, ACol: Integer);
    procedure   TabSheetClosing(Sender: TObject; ATabSheet: TfpgTabSheet);
    procedure   BuildTerminated(Sender: TObject);
    procedure   BuildOutput(Sender: TObject; const ALine: string);
    procedure   UpdateStatus(const AText: TfpgString);
    procedure   UpdateCursorPos(const ALine, ACol: Integer);
    procedure   UpdateGitBranch;
    procedure   UpdateProfilesDisplay;
    procedure   ToggleProfile(const AProfileName: TfpgString);
    procedure   lblProfilesClicked(Sender: TObject);
    procedure   pmProfileClicked(Sender: TObject);
    procedure   tvProfileStateImageClicked(Sender: TObject; ANode: TfpgTreeNode);
    procedure   SetupProjectTree;
    procedure   PopuplateProjectTree;
    procedure   PopulatePasBuildTree;
    procedure   SetupFilesGrid;
    procedure   AddMessage(const AMsg: TfpgString);
    procedure   ClearMessagesWindow;
    procedure   CloseAllTabs;
    procedure   SaveSession;
    procedure   LoadProject(const AFilename: TfpgString);
    function    CreateNewEditorTab(const ATitle: TfpgString): TfpgTabSheet;
    function    OpenEditorPage(const AFilename: TfpgString): TfpgTabSheet;
    function    GetUnitsNode: TfpgTreeNode;
    procedure   UpdateWindowTitle;
    procedure   HighlightWithTokens(AHighlighter: TEditorHighlighter; Sender: TObject; ALineText: TfpgString; ALineIndex: Integer; ACanvas: TfpgCanvas; ATextRect: TfpgRect; var AllowSelfDraw: Boolean; AShowBracketMatch: Boolean = False);
    procedure   HighlightObjectPascal(Sender: TObject; ALineText: TfpgString; ALineIndex: Integer; ACanvas: TfpgCanvas; ATextRect: TfpgRect; var AllowSelfDraw: Boolean);
    procedure   HighlightINI(Sender: TObject; ALineText: TfpgString; ALineIndex: Integer; ACanvas: TfpgCanvas; ATextRect: TfpgRect; var AllowSelfDraw: Boolean);
    procedure   HighlightXML(Sender: TObject; ALineText: TfpgString; ALineIndex: Integer; ACanvas: TfpgCanvas; ATextRect: TfpgRect; var AllowSelfDraw: Boolean);
    procedure   HighlightPatch(Sender: TObject; ALineText: TfpgString; ALineIndex: Integer; ACanvas: TfpgCanvas; ATextRect: TfpgRect; var AllowSelfDraw: Boolean);
    procedure   LoadThemeByName(const AName: string);
    procedure   SetupEditorPreference;
    procedure   miJumpToInterface(Sender: TObject);
    procedure   miJumpToImplementation(Sender: TObject);
    procedure   miJumpToggleIntfImpl(Sender: TObject);
    procedure   miGoToDeclaration(Sender: TObject);
    function    GetCurrentCursorLocation: TCursorLocation;
    procedure   RecordCursorLocation;
    procedure   NavigateToLocation(const ALoc: TCursorLocation);
    procedure   miNavigateBack(Sender: TObject);
    procedure   miNavigateForward(Sender: TObject);
    procedure   miNavigateToFile(Sender: TObject);
    procedure   miNavigateToSymbol(Sender: TObject);
    procedure   CheckGitIgnoreForIdeDir;
    procedure   uiCreateToolBar;
    procedure   uiCreateStatusBar;
    procedure   uiCreateClientArea;
    procedure   uiCreateMenus;
  protected
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  process
  ,fpg_iniutils
  ,fpg_dialogs
  ,fpg_utils
  ,fpg_stringutils
  ,fpg_constants
  ,fpg_widget
  ,ide.form.configure
  ,ide.form.projectoptions
  ,ide.form.debug
  ,ide.form.procedurelist
  ,ide.form.find
  ,fpg_basegrid
  ,ide.consts
  ,ide.macros
  ,ide.project.backend
  ,ide.project
  ,ide.project.unitlist
  ,ide.builder.thread
  ,ide.utils
  ,ide.session
  ,ide.navigation
  ,ide.declaration
  ,ide.highlighter.ini
  ,ide.highlighter.xml
  ,fpg_imgfmt_bmp
  ;


const
  cTitle = 'Maximus IDE - %s';
  cFileFilterTemplate  = '%s (%s)|%s';
  cSourceFiles = '*.pas;*.pp;*.lpr;*.dpr;*.inc';
  cProjectFiles = '*.project;project.xml';

  { 16x16 checkbox images for tree view state icons (BMP format, mask colour at 0,0) }
  cCheckboxUnchecked: array[0..821] of byte = (
      66, 77, 54,  3,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
       0, 16,  0,  0,  0, 16,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
       0,  3,  0,  0,100,  0,  0,  0,100,  0,  0,  0,  0,  0,  0,  0,  0,
       0,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,
     132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,132,132,132,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     255,255,255,255,255,255,255,255,255,255,132,132,132,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,132,132,132,255,255,255,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     255,255,255,255,255,255,255,132,132,132,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,132,132,132,255,255,255,255,255,255,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     255,255,255,255,132,132,132,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,132,132,132,255,255,255,255,255,255,255,255,255,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     255,132,132,132,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,132,132,132,255,255,255,255,255,255,255,255,255,255,255,255,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,132,132,
     132,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,132,
     132,132,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     255,255,255,255,255,255,255,255,255,255,255,255,132,132,132,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,132,132,132,255,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     255,255,255,255,255,255,255,255,255,132,132,132,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,132,132,132,255,255,255,255,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     255,255,255,255,255,255,132,132,132,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,132,132,132,255,255,255,255,255,255,255,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     255,255,255,132,132,132,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,132,132,132,132,132,132,132,132,132,132,132,132,132,
     132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,
     132,132,132,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255);

  cCheckboxChecked: array[0..821] of byte = (
      66, 77, 54,  3,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
       0, 16,  0,  0,  0, 16,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
       0,  3,  0,  0,100,  0,  0,  0,100,  0,  0,  0,  0,  0,  0,  0,  0,
       0,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,
     132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,132,132,132,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     255,255,255,255,255,255,255,255,255,255,132,132,132,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,132,132,132,255,255,255,
     255,255,255,255,255,255,  0,  0,  0,255,255,255,255,255,255,255,255,
     255,255,255,255,255,255,255,132,132,132,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,132,132,132,255,255,255,255,255,255,
       0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,255,
     255,255,255,255,132,132,132,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,132,132,132,255,255,255,  0,  0,  0,  0,  0,  0,
       0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,255,
     255,132,132,132,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,132,132,132,255,255,255,  0,  0,  0,  0,  0,  0,255,255,255,
       0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,132,132,
     132,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,132,
     132,132,255,255,255,  0,  0,  0,255,255,255,255,255,255,255,255,255,
       0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,132,132,132,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,132,132,132,255,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
       0,  0,  0,  0,  0,  0,255,255,255,132,132,132,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,132,132,132,255,255,255,255,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
       0,  0,  0,255,255,255,132,132,132,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,132,132,132,255,255,255,255,255,255,255,
     255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
     255,255,255,132,132,132,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,132,132,132,132,132,132,132,132,132,132,132,132,132,
     132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,132,
     132,132,132,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255);


{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnOpenFileClicked(Sender: TObject);
var
  s: TfpgString;
begin
  s := SelectFileDialog(sfdOpen, Format(cFileFilterTemplate, ['Source Files', cSourceFiles, cSourceFiles]), FLastFileDir);
  if s <> '' then
  begin
    FLastFileDir := fpgExtractFileDir(s);
    OpenEditorPage(s);
  end;
end;

procedure TMainForm.miFileNewUnit(Sender: TObject);
var
  newunit: TfpgString;
  sl: TStringList;
  FInternalMacroList: TIDEMacroList;
  i: integer;
begin
  if fpgInputQuery('New Unit', 'Please give the new unit a file name', newunit) then
  begin
    if GProject.UnitList.FileExists(newunit) then
    begin
      ShowMessage(Format('The unit <%s> already exists in the project', [newunit]));
      Exit;
    end;
    sl := TStringList.Create;
    try
      sl.LoadFromFile(GMacroList.ExpandMacro('${TEMPLATEDIR}default/unit.pas'));
      sl.Text := StringReplace(sl.Text, '${UNITNAME}', fpgChangeFileExt(fpgExtractFileName(newunit), ''), [rfReplaceAll, rfIgnoreCase]);
      sl.SaveToFile(GProject.ProjectDir + newunit);
    finally
      sl.Free;
    end;
//    AddUnitToProject(newunit);

    OpenEditorPage(newunit);
  end;
end;

procedure TMainForm.miFileSave(Sender: TObject);
var
  s: TfpgString;
  ts: TfpgTabSheet;
  edt: TfpgTextEdit;
begin
  ts := pcEditor.ActivePage;
  edt := TfpgTextEdit(ts.Components[0]);

  s := ts.Hint;
  if s <> '' then
  begin
    edt.SaveToFile(s);
    { Clear modified indicator }
    if (Length(ts.Text) > 2) and (Copy(ts.Text, 1, 2) = '* ') then
      ts.Text := Copy(ts.Text, 3, Length(ts.Text) - 2);
  end;
  AddMessage('File saved');
end;

procedure TMainForm.miFileSaveAs(Sender: TObject);
var
  s: TfpgString;
begin
  s := SelectFileDialog(sfdSave, '', FLastFileDir);
  if s <> '' then
  begin
    FLastFileDir := fpgExtractFileDir(s);
    TfpgTextEdit(pcEditor.ActivePage.Components[0]).SaveToFile(s);
  end;
end;

procedure TMainForm.miFileClose(Sender: TObject);
var
  ts: TfpgTabSheet;
  i: integer;
begin
  pcEditor.BeginUpdate;
  try
    ts := pcEditor.ActivePage;
    pcEditor.RemoveTabSheet(ts);
    ts.Free;
  finally
    pcEditor.EndUpdate;
  end;
end;

procedure TMainForm.miEditUndoClicked(Sender: TObject);
var
  edt: TfpgTextEdit;
begin
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  edt.Undo;
end;

procedure TMainForm.miEditRedoClicked(Sender: TObject);
var
  edt: TfpgTextEdit;
begin
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  edt.Redo;
end;

procedure TMainForm.miEditCutClicked(Sender: TObject);
var
  edt: TfpgTextEdit;
begin
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  edt.CutToClipboard;
end;

procedure TMainForm.miEditCopyClicked(Sender: TObject);
var
  edt: TfpgTextEdit;
begin
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  edt.CopyToClipboard;
end;

procedure TMainForm.miEditPasteClicked(Sender: TObject);
var
  edt: TfpgTextEdit;
begin
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  edt.PasteFromClipboard;
end;

procedure TMainForm.miEditDuplicateLineClicked(Sender: TObject);
var
  edt: TfpgTextEdit;
begin
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  edt.DuplicateLine;
end;

procedure TMainForm.miEditDeleteLineClicked(Sender: TObject);
var
  edt: TfpgTextEdit;
begin
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  edt.DeleteLine;
end;

procedure TMainForm.miFindClicked(Sender: TObject);
var
  s: TfpgString;
  edt: TfpgTextEdit;
begin
  FLastFindBackward := False;
  FLastFindOptions := [];
  DisplayFindForm(s, FLastFindOptions, FLastFindBackward);
  if s = '' then
    exit;
  FLastSearchText := s;
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  edt.FindText(s, FLastFindOptions, FLastFindBackward);
  edt.SetFocus;
end;

procedure TMainForm.miFindNextClicked(Sender: TObject);
var
  edt: TfpgTextEdit;
begin
  if FLastSearchText = '' then
    Exit;
  FLastFindBackward := False;
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  if not Assigned(edt) then
    Exit;
  edt.FindText(FLastSearchText, FLastFindOptions, FLastFindBackward);
  edt.SetFocus;
end;

procedure TMainForm.miFindPrevClicked(Sender: TObject);
var
  edt: TfpgTextEdit;
begin
  if FLastSearchText = '' then
    Exit;
  FLastFindBackward := True;
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  if not Assigned(edt) then
    Exit;
  edt.FindText(FLastSearchText, FLastFindOptions, FLastFindBackward);
  edt.SetFocus;
end;

procedure TMainForm.miGoToLineClick(Sender: TObject);
var
  sValue: string;
  i: integer;
  iMax: integer;
  edt: TfpgTextEdit;
begin
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  if not Assigned(edt) then
    Exit;
  iMax := edt.Lines.Count;
  sValue := IntToStr(edt.CaretPos_V + 1);
  if fpgInputQuery('Go to line', Format('Line %d of %d — enter line number:', [edt.CaretPos_V + 1, iMax]), sValue) then
  begin
    try
      i := StrToInt(sValue);
      if (i < 1) or (i > iMax) then
        ShowMessage(Format('Line number must be between 1 and %d.', [iMax]))
      else
      begin
        RecordCursorLocation;
        edt.GotoLine(i);
      end;
    except
      on E: Exception do
         ShowMessage('Invalid line number.' + LineEnding + E.Message);
    end;
  end;
end;

procedure TMainForm.miSearchProcedureList(Sender: TObject);
var
  s: TfpgString;
  edt: TfpgTextEdit;
begin
  s := pcEditor.ActivePage.Hint;
  if s <> '' then
  begin
    edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
    DisplayProcedureList(s, edt);
  end;
end;

procedure TMainForm.miAboutFPGuiClicked(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGui;
end;

procedure TMainForm.miAboutIDE(Sender: TObject);
begin
  TfpgMessageDialog.Information('About fpGUI IDE',
      'fpGUI''s Maximus IDE version ' + FPGUI_VERSION + LineEnding + LineEnding
      + 'Created by Graeme Geldenhuys' + LineEnding
      + 'Compiled with FPC ' + FPCVersion);
end;

procedure TMainForm.miRunMake(Sender: TObject);
var
  thd: TBuilderThread;
  pb: TPasBuildProjectBackend;
  ModInfo: TAggregatorModuleInfo;
  FilePath: TfpgString;
begin
  ClearMessagesWindow;
  thd := TBuilderThread.Create(True);
  { For aggregator projects, detect the active module from the current editor tab }
  if (GProject.ProjectFormat = pfPasBuild) then
  begin
    pb := TPasBuildProjectBackend(GProject);
    if pb.IsAggregator and (pcEditor.ActivePage <> nil) then
    begin
      FilePath := pcEditor.ActivePage.Hint;
      if FilePath <> '' then
      begin
        ModInfo := pb.FindModuleInfoForFile(FilePath);
        if ModInfo <> nil then
          thd.BuildModule := ModInfo.Name;
      end;
    end;
  end;
  thd.OnTerminate := @BuildTerminated;
  thd.OnAvailableOutput := @BuildOutput;
  thd.Resume;
end;

procedure TMainForm.miRunBuild(Sender: TObject);
var
  thd: TBuilderThread;
begin
  ClearMessagesWindow;
  thd := TBuilderThread.Create(True);
  { Build All: for PasBuild aggregator projects, use 'compile' goal without
    specifying a module — pasbuild will compile all modules in dependency order.
    For legacy projects, use BuildMode 1. }
  if GProject.ProjectFormat = pfPasBuild then
    thd.BuildGoal := 'compile'
  else
    thd.BuildMode := 1;
  thd.OnTerminate := @BuildTerminated;
  thd.OnAvailableOutput := @BuildOutput;
  thd.Resume;
end;

procedure TMainForm.miRunMake1(Sender: TObject);
var
  thd: TBuilderThread;
begin
  ClearMessagesWindow;
  thd := TBuilderThread.Create(True);
  thd.BuildMode := 2;
  thd.OnTerminate := @BuildTerminated;
  thd.OnAvailableOutput := @BuildOutput;
  thd.Resume;
end;

procedure TMainForm.miRunMake2(Sender: TObject);
var
  thd: TBuilderThread;
begin
  ClearMessagesWindow;
  thd := TBuilderThread.Create(True);
  thd.BuildMode := 3;
  thd.OnTerminate := @BuildTerminated;
  thd.OnAvailableOutput := @BuildOutput;
  thd.Resume;
end;

procedure TMainForm.miRunMake3(Sender: TObject);
var
  thd: TBuilderThread;
begin
  ClearMessagesWindow;
  thd := TBuilderThread.Create(True);
  thd.BuildMode := 4;
  thd.OnTerminate := @BuildTerminated;
  thd.OnAvailableOutput := @BuildOutput;
  thd.Resume;
end;

procedure TMainForm.miRunMake4(Sender: TObject);
var
  thd: TBuilderThread;
begin
  ClearMessagesWindow;
  thd := TBuilderThread.Create(True);
  thd.BuildMode := 5;
  thd.OnTerminate := @BuildTerminated;
  thd.OnAvailableOutput := @BuildOutput;
  thd.Resume;
end;

procedure TMainForm.miRunClean(Sender: TObject);
begin
  StartBuildGoal('clean');
end;

procedure TMainForm.miRunRebuild(Sender: TObject);
begin
  StartBuildGoal('rebuild');
end;

procedure TMainForm.miRunTest(Sender: TObject);
begin
  StartBuildGoal('test');
end;

procedure TMainForm.StartBuildGoal(const AGoal: string);
var
  thd: TBuilderThread;
  pb: TPasBuildProjectBackend;
  ModInfo: TAggregatorModuleInfo;
  FilePath: TfpgString;
begin
  if GProject.ProjectFormat <> pfPasBuild then
  begin
    AddMessage('This action is only available for PasBuild projects.');
    Exit;
  end;
  ClearMessagesWindow;
  thd := TBuilderThread.Create(True);
  thd.BuildGoal := AGoal;
  { For aggregator projects, detect the active module from the current editor tab }
  pb := TPasBuildProjectBackend(GProject);
  if pb.IsAggregator and (pcEditor.ActivePage <> nil) then
  begin
    FilePath := pcEditor.ActivePage.Hint;
    if FilePath <> '' then
    begin
      ModInfo := pb.FindModuleInfoForFile(FilePath);
      if ModInfo <> nil then
        thd.BuildModule := ModInfo.Name;
    end;
  end;
  thd.OnTerminate := @BuildTerminated;
  thd.OnAvailableOutput := @BuildOutput;
  thd.Resume;
end;

procedure TMainForm.miProjectDependencyTree(Sender: TObject);
begin
  StartBuildGoal('dependency-tree');
end;

procedure TMainForm.pmTreeDependencyTreeClick(Sender: TObject);
var
  ModName: string;
  thd: TBuilderThread;
begin
  if GProject.ProjectFormat <> pfPasBuild then
  begin
    AddMessage('This action is only available for PasBuild projects.');
    Exit;
  end;
  ClearMessagesWindow;
  ModName := DetectModuleFromTreeNode(tvProject.Selection);
  StartModuleBuild('dependency-tree', ModName, @BuildTerminated, @BuildOutput);
end;

procedure TMainForm.miConfigureIDE(Sender: TObject);
begin
  DisplayConfigureIDE;
  SetupEditorPreference;
end;

procedure TMainForm.miViewDebug(Sender: TObject);
begin
  if not Assigned(DebugForm) then
    fpgApplication.CreateForm(TDebugForm, TfpgWindowBase(DebugForm));
  DebugForm.Show;
end;

procedure TMainForm.miProjectNew(Sender: TObject);
begin
  CloseAllTabs;
  FreeProject;
  GProject.ProjectName := 'empty.project';
  GProject.MainUnit := 'empty.pas';
  OpenEditorPage(GProject.MainUnit);
  miProjectSaveAs(nil);
end;

procedure TMainForm.miProjectNewFromTemplate(Sender: TObject);
var
  dlg: TfpgFileDialog;
  lFilename: TfpgString;
begin
  CloseAllTabs;
  FreeProject;
  dlg := TfpgFileDialog.Create(nil);
  try
    dlg.InitialDir := GMacroList.ExpandMacro(cMacro_TemplateDir);
    dlg.Filter := Format(cFileFilterTemplate, ['Project Files', cProjectFiles, cProjectFiles])
                  + '|' + rsAllFiles+' ('+AllFilesMask+')'+'|'+AllFilesMask;
    if dlg.RunOpenFile then
    begin
      lFilename := dlg.FileName;
      SetProject(CreateProjectBackend(lFilename));
      GProject.Load(lFilename);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.miProjectOptions(Sender: TObject);
begin
  DisplayProjectOptions;
end;

procedure TMainForm.miProjectOpen(Sender: TObject);
var
  s: TfpgString;
begin
  s := SelectFileDialog(sfdOpen, Format(cFileFilterTemplate, ['Project Files', cProjectFiles, cProjectFiles]));
  if s <> '' then
  begin
    LoadProject(s);
  end;
end;

procedure TMainForm.miRecentProjectsClick(Sender: TObject; const FileName: String);
begin
  LoadProject(Filename);
end;

procedure TMainForm.miProjectSave(Sender: TObject);
begin
  try
    GProject.Save;
  except
    on E: Exception do
    begin
      TfpgMessageDialog.Critical('', E.Message);
    end;
  end;
  AddMessage('Project saved.');
end;

procedure TMainForm.miProjectSaveAs(Sender: TObject);
var
  s: TfpgString;
begin
  s := SelectFileDialog(sfdSave, Format(cFileFilterTemplate, ['Project Files', cProjectFiles, cProjectFiles]));
  if s <> '' then
  begin
    if fpgExtractFileExt(s) = '' then
      s := s + cProjectExt;
    try
      GProject.Save(s);
      FRecentFiles.AddItem(s);
    except
      on E: Exception do
      begin
        TfpgMessageDialog.Critical('', E.Message);
      end;
    end;
    UpdateWindowTitle;
    AddMessage(Format('Project saved as <%s>.', [s]));
  end;
end;

procedure TMainForm.AddUnitToProject(const AUnitName: TfpgString);
var
  u: TUnit;
  s: TfpgString;
  r: TfpgTreeNode;
  n: TfpgTreeNode;
begin
  u := GProject.UnitList.AddFilename(AUnitName);
  if Assigned(n) then
  begin
    // add reference to tabsheet
    pcEditor.ActivePage.TagPointer := u;
    s := u.GetRelativePath;
    r := GetUnitsNode;
    n := r.AppendText(s);
    // add reference to treenode
    n.Data := u;
    tvProject.Invalidate;
  end;
end;

procedure TMainForm.miProjectAddUnitToProject(Sender: TObject);
var
  s: TfpgString;
begin
  s := pcEditor.ActivePage.Hint;
//  writeln('adding unit: ', s);
  if s = '' then
    Exit;
  if GProject.UnitList.FileExists(s) then
    Exit;
  AddUnitToProject(s);
end;

procedure TMainForm.tvProjectDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  n: TfpgTreeNode;
  ts: TfpgTabSheet;
  u: TUnit = nil;
  pb: TPasBuildProjectBackend;
  FilePath: TfpgString;
begin
  n := tvProject.Selection;
  if n = nil then
    Exit;

  if GProject.ProjectFormat = pfPasBuild then
  begin
    pb := TPasBuildProjectBackend(GProject);
    FilePath := ResolveNodeFilePath(n, pb.ProjectDir, pb.SourceDirectory,
      pb.IsAggregator);
    if (FilePath <> '') and fpgFileExists(FilePath) then
    begin
      RecordCursorLocation;
      OpenEditorPage(FilePath);
    end;
  end
  else
  begin
    { Legacy: nodes carry TUnit in Data }
    if n.Data <> nil then
      u := TUnit(n.Data);
    if u <> nil then
    begin
      RecordCursorLocation;
      ts := OpenEditorPage(u.FileName);
      u.Opened := True;
      ts.TagPointer := u;
    end;
  end;
end;

procedure TMainForm.tvProjectKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
var
  r: TfpgTreeNode;
  n: TfpgTreeNode;
  i: integer;
begin
  { Delete key removes unit from legacy projects only }
  if (keyCode = keyDelete) and (GProject.ProjectFormat = pfLegacy) then
  begin
    r := GetUnitsNode;
    if (r <> nil) and (r.FindSubNode(tvProject.Selection.Text, False) = tvProject.Selection) then
    begin
      n := tvProject.Selection;
      tvProject.GotoNextNodeUp;
      r.Remove(n);
      tvProject.Invalidate;
      GProject.UnitList.Remove(TUnit(n.Data));

      for i := 0 to pcEditor.PageCount-1 do
      begin
        if pcEditor.Pages[i].TagPointer = n.Data then
        begin
          pcEditor.Pages[i].TagPointer := nil;
          break
        end;
      end;
      TUnit(n.Data).Free;
      n.Free;
    end;
  end;
end;

procedure TMainForm.grdMessageKeyPressed(Sender: TObject; var KeyCode: Word; var ShiftState: TShiftState; var Consumed: Boolean);
var
  cr: TClipboardKeyType;
  i: integer;
  s: TfpgString;
begin
  cr := CheckClipboardKey(KeyCode, ShiftState);
  if cr = ckCopy then
  begin
    s := '';
    for i := 0 to grdMessages.RowCount-1 do
      s := s + grdMessages.Cells[0, i] + LineEnding;
    fpgClipboard.Text := s;
  end;
end;

procedure TMainForm.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
var
  idx: Integer;
begin
  if (ssCtrl in shiftstate) and not (ssShift in shiftstate) then
  begin
    case keycode of
      keyPrior:  { Ctrl+PageUp: previous tab }
        begin
          idx := pcEditor.ActivePageIndex - 1;
          if idx < 0 then
            idx := pcEditor.PageCount - 1;
          pcEditor.ActivePageIndex := idx;
          consumed := True;
        end;
      keyNext:   { Ctrl+PageDown: next tab }
        begin
          idx := pcEditor.ActivePageIndex + 1;
          if idx >= pcEditor.PageCount then
            idx := 0;
          pcEditor.ActivePageIndex := idx;
          consumed := True;
        end;
      keyB:  { Ctrl+B: go to declaration }
        begin
          writeln('DEBUG: Ctrl+B shortcut to miGoToDeclaration');
          miGoToDeclaration(nil);
          consumed := True;
        end;
    end;
  end;
  { Ctrl+Shift shortcuts for interface/implementation navigation }
  if not consumed and ([ssCtrl, ssShift] <= shiftstate) and not (ssAlt in shiftstate) then
  begin
    case keycode of
      keyUp:  { Ctrl+Shift+Up: jump to interface declaration }
        begin
          miJumpToInterface(nil);
          consumed := True;
        end;
      keyDown:  { Ctrl+Shift+Down: jump to implementation }
        begin
          miJumpToImplementation(nil);
          consumed := True;
        end;
    end;
  end;
  { Alt+Left/Right: navigate back/forward in cursor history }
  if not consumed and (ssAlt in shiftstate) and not (ssCtrl in shiftstate) and not (ssShift in shiftstate) then
  begin
    case keycode of
      keyLeft:
        begin
          miNavigateBack(nil);
          consumed := True;
        end;
      keyRight:
        begin
          miNavigateForward(nil);
          consumed := True;
        end;
    end;
  end;
  if not consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TMainForm.pcEditorMouseUp(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
var
  ts: TfpgTabSheet;
begin
  if (AButton = mbMiddle) and (AShift * [ssCtrl, ssShift, ssAlt] = []) then
  begin
    ts := pcEditor.TabSheetAtPos(AMousePos.X, AMousePos.Y);
    if Assigned(ts) then
    begin
      pcEditor.RemoveTabSheet(ts);
      ts.Free;
    end;
  end
  else if (AButton = mbRight) and (AShift * [ssCtrl, ssShift, ssAlt] = []) then
  begin
    ts := pcEditor.TabSheetAtPos(AMousePos.X, AMousePos.Y);
    if Assigned(ts) then
    begin
      FLastTabClickPos := AMousePos;
      pmTabMenu.ShowAt(pcEditor, AMousePos.X, AMousePos.Y);
    end;
  end;
end;

procedure TMainForm.pmTabCloseClick(Sender: TObject);
var
  ts: TfpgTabSheet;
begin
  ts := pcEditor.TabSheetAtPos(FLastTabClickPos.X, FLastTabClickPos.Y);
  if Assigned(ts) then
  begin
    pcEditor.RemoveTabSheet(ts);
    ts.Free;
  end;
end;

procedure TMainForm.pmTabCloseOthersClick(Sender: TObject);
var
  ts, target: TfpgTabSheet;
  I: Integer;
begin
  target := pcEditor.TabSheetAtPos(FLastTabClickPos.X, FLastTabClickPos.Y);
  if not Assigned(target) then
    Exit;
  pcEditor.ActivePage := target;
  for I := pcEditor.PageCount - 1 downto 0 do
  begin
    ts := pcEditor.Pages[I];
    if ts <> target then
    begin
      pcEditor.RemoveTabSheet(ts);
      ts.Free;
    end;
  end;
end;

procedure TMainForm.pmTabCloseAllClick(Sender: TObject);
begin
  CloseAllTabs;
end;

procedure TMainForm.pmTabCopyPathClick(Sender: TObject);
var
  ts: TfpgTabSheet;
begin
  ts := pcEditor.TabSheetAtPos(FLastTabClickPos.X, FLastTabClickPos.Y);
  if Assigned(ts) then
    fpgClipboard.Text := ts.Hint;
end;

procedure TMainForm.pmModuleBuildClick(Sender: TObject);
var
  ModName: string;
begin
  ModName := DetectModuleFromTreeNode(tvProject.Selection);
  if ModName = '' then
    Exit;
  ClearMessagesWindow;
  StartModuleBuild('compile', ModName, @BuildTerminated, @BuildOutput);
end;

procedure TMainForm.pmModuleCleanClick(Sender: TObject);
var
  ModName: string;
begin
  ModName := DetectModuleFromTreeNode(tvProject.Selection);
  if ModName = '' then
    Exit;
  ClearMessagesWindow;
  StartModuleBuild('clean', ModName, @BuildTerminated, @BuildOutput);
end;

procedure TMainForm.pmModuleRebuildClick(Sender: TObject);
var
  ModName: string;
begin
  ModName := DetectModuleFromTreeNode(tvProject.Selection);
  if ModName = '' then
    Exit;
  ClearMessagesWindow;
  StartModuleBuild('rebuild', ModName, @BuildTerminated, @BuildOutput);
end;

procedure TMainForm.EditorChanged(Sender: TObject);
var
  edt: TfpgTextEdit;
  ts: TfpgTabSheet;
begin
  edt := Sender as TfpgTextEdit;
  ts := edt.Parent as TfpgTabSheet;
  if Assigned(ts) and (Copy(ts.Text, 1, 2) <> '* ') then
    ts.Text := '* ' + ts.Text;
  { Invalidate cached highlighter state so the next paint retokenises }
  FHighlightCache.InvalidateEditor(edt);
end;

procedure TMainForm.EditorTabChanged(Sender: TObject; ATabSheet: TfpgTabSheet);
var
  edt: TfpgTextEdit;
begin
  FBracketMatch.Found := False;
  if Assigned(ATabSheet) and (ATabSheet.ComponentCount > 0) then
  begin
    edt := ATabSheet.Components[0] as TfpgTextEdit;
    FHighlightCache.EnsurePascalTokenised(edt, edt.Lines);
    UpdateStatus(ATabSheet.Hint);
    UpdateCursorPos(edt.CaretPos_V, edt.CaretPos_H);
  end
  else
  begin
    UpdateStatus('');
    lblCursorPos.Text := '';
  end;
end;

procedure TMainForm.RetokeniseEditor(AEditor: TfpgTextEdit);
begin
  if not Assigned(AEditor) then
    Exit;
  FHighlightCache.EnsurePascalTokenised(AEditor, AEditor.Lines);
end;

procedure TMainForm.EditorCaretChanged(Sender: TObject; ALine, ACol: Integer);
var
  edt: TfpgTextEdit;
  OldMatch: TBracketMatchResult;
begin
  UpdateCursorPos(ALine, ACol);
  if not Assigned(FHighlightCache) then
    Exit;
  edt := TfpgTextEdit(Sender);
  FHighlightCache.EnsurePascalTokenised(edt, edt.Lines);

  OldMatch := FBracketMatch;
  FBracketMatch := FindMatchingBracket(FHighlightCache.PascalHighlighter, ALine, ACol, edt.Lines);

  { Only repaint if match state changed }
  if OldMatch.Found or FBracketMatch.Found then
    edt.Invalidate;
end;

procedure TMainForm.TabSheetClosing(Sender: TObject; ATabSheet: TfpgTabSheet);
var
  u: TUnit;
begin
  { Clear highlighter references if this tab's editor is being tracked }
  if Assigned(ATabSheet) and (ATabSheet.ComponentCount > 0) then
    FHighlightCache.InvalidateEditor(ATabSheet.Components[0]);
  u := TUnit(ATabSheet.TagPointer);
  if Assigned(u) then
  begin
    FFileMonitor.RemoveFile(u.FileName);
    u.Opened := False;
  end;
end;

procedure TMainForm.BuildTerminated(Sender: TObject);
begin
  AddMessage('Done.');
end;

procedure TMainForm.BuildOutput(Sender: TObject; const ALine: string);
begin
  AddMessage(ALine);
end;

procedure TMainForm.UpdateStatus(const AText: TfpgString);
begin
  lblStatus.Text := AText;
end;

procedure TMainForm.UpdateCursorPos(const ALine, ACol: Integer);
begin
  { ALine and ACol are 0-based from TfpgTextEdit; display as 1-based }
  lblCursorPos.Text := Format('Ln %d Col %d', [ALine + 1, ACol + 1]);
end;

procedure TMainForm.UpdateGitBranch;
var
  p: TProcess;
  s: TStringList;
begin
  lblGitBranch.Text := '';
  if GProject.ProjectDir = '' then
    Exit;
  p := TProcess.Create(nil);
  s := TStringList.Create;
  try
    p.Executable := 'git';
    p.Parameters.Add('rev-parse');
    p.Parameters.Add('--abbrev-ref');
    p.Parameters.Add('HEAD');
    p.CurrentDirectory := GProject.ProjectDir;
    p.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    try
      p.Execute;
      s.LoadFromStream(p.Output);
      p.WaitOnExit;
      if (p.ExitCode = 0) and (s.Count > 0) then
        lblGitBranch.Text := s[0];
    except
      { git not available or not a git repo — silently ignore }
    end;
  finally
    s.Free;
    p.Free;
  end;
end;

procedure TMainForm.UpdateProfilesDisplay;
begin
  if GProject.ProjectFormat = pfPasBuild then
    lblProfiles.Text := ProfilesDisplayText(
      TPasBuildProjectBackend(GProject).ActiveProfiles)
  else
    lblProfiles.Text := '';
end;

procedure TMainForm.lblProfilesClicked(Sender: TObject);
var
  pb: TPasBuildProjectBackend;
  mi: TfpgMenuItem;
  i: integer;
  lFont: TfpgFontResourceBase;
  lItemHeight: integer;
  lPopupHeight: integer;
begin
  if GProject.ProjectFormat <> pfPasBuild then
    Exit;
  pb := TPasBuildProjectBackend(GProject);
  if pb.AvailableProfiles.Count = 0 then
    Exit;

  { Rebuild menu items each time — profile list may change after project reload }
  pmProfileMenu.Free;
  pmProfileMenu := TfpgPopupMenu.Create(self);
  for i := 0 to pb.AvailableProfiles.Count - 1 do
  begin
    mi := pmProfileMenu.AddMenuItem(pb.AvailableProfiles[i], '', @pmProfileClicked);
    mi.Checked := pb.ActiveProfiles.IndexOf(pb.AvailableProfiles[i]) >= 0;
  end;

  { Calculate popup height using menu font metrics (HiDPI-aware) }
  lFont := fpgApplication.FontManager.GetFont(fpgStyle.MenuFontDef.FontDesc);
  lItemHeight := lFont.GetHeight + 2;
  lFont := nil;
  lPopupHeight := 6 + (pb.AvailableProfiles.Count * lItemHeight);  { 6 = margin*2 }

  { Show popup above the label }
  pmProfileMenu.ShowAt(lblProfiles, 0, -lPopupHeight, True);
end;

procedure TMainForm.ToggleProfile(const AProfileName: TfpgString);
var
  pb: TPasBuildProjectBackend;
begin
  if GProject.ProjectFormat <> pfPasBuild then
    Exit;
  pb := TPasBuildProjectBackend(GProject);
  ToggleProfileInList(pb.ActiveProfiles, AProfileName);

  { Re-resolve with new profiles and refresh UI }
  pb.Resolve(pb.ActiveProfiles.CommaText);
  SetupProjectTree;
  PopuplateProjectTree;
  UpdateProfilesDisplay;
  AddMessage('Active profiles: ' + pb.ActiveProfiles.CommaText);
end;

procedure TMainForm.pmProfileClicked(Sender: TObject);
var
  mi: TfpgMenuItem;
begin
  if not (Sender is TfpgMenuItem) then
    Exit;
  mi := TfpgMenuItem(Sender);
  ToggleProfile(mi.Text);
end;

procedure TMainForm.tvProfileStateImageClicked(Sender: TObject; ANode: TfpgTreeNode);
begin
  { Only handle profile nodes (children of 'Build Profiles' parent) }
  if (ANode.Parent <> nil) and (ANode.Parent.Text = 'Build Profiles') then
    ToggleProfile(ANode.Text);
end;

procedure TMainForm.SetupProjectTree;
begin
  tvProject.RootNode.Clear;
  if GProject.ProjectFormat <> pfPasBuild then
  begin
    tvProject.RootNode.AppendText('Units');
    tvProject.RootNode.AppendText('Images');
    tvProject.RootNode.AppendText('Help Files');
    tvProject.RootNode.AppendText('Text');
    tvProject.RootNode.AppendText('Other');
  end;
end;

procedure TMainForm.PopuplateProjectTree;
var
  r: TfpgTreeNode;
  n: TfpgTreeNode;
  i: integer;
  s: TfpgString;
begin
  if GProject.ProjectFormat = pfPasBuild then
  begin
    PopulatePasBuildTree;
    Exit;
  end;

  { Legacy project tree }
  r := GetUnitsNode;
  tvProject.Selection := r;
  if Assigned(r) then
  begin
    for i := 0 to GProject.UnitList.Count-1 do
    begin
      s := fpgExtractRelativepath(GProject.ProjectDir, GProject.UnitList[i].FileName);
      n := r.AppendText(s);
      n.Data := GProject.UnitList[i];
    end;
  end;
  r.Expand;
  tvProject.Invalidate;
end;

procedure TMainForm.PopulatePasBuildTree;
var
  pb: TPasBuildProjectBackend;
  RootNode: TfpgTreeNode;
  DirNode: TfpgTreeNode;
  ProfileNode: TfpgTreeNode;
  RootLabel: TfpgString;
  i: integer;
begin
  pb := TPasBuildProjectBackend(GProject);
  if pb.Version <> '' then
    RootLabel := pb.ProjectName + ' (' + pb.Version + ')'
  else
    RootLabel := pb.ProjectName;
  RootNode := tvProject.RootNode.AppendText(RootLabel);
  RootNode.TextColor := clText2;

  if pb.IsAggregator then
  begin
    { Aggregator project — show Modules with per-module subtrees }
    tvProject.PopupMenu := pmModuleMenu;
    if pb.ModuleInfos.Count > 0 then
    begin
      DirNode := RootNode.AppendText('Modules');
      DirNode.TextColor := clText2;
      for i := 0 to pb.ModuleInfos.Count - 1 do
        AddModuleInfoToTree(DirNode, TAggregatorModuleInfo(pb.ModuleInfos[i]));
      DirNode.Expand;
    end;
  end
  else
  begin
    { Single module — show Sources/Tests/Resources/Dependencies directly }
    tvProject.PopupMenu := pmProjectTreeMenu;
    AddModuleSubtree(RootNode, pb.ProjectDir, pb.SourceDirectory,
      pb.DeclaredDeps, True);
  end;

  { Build Profiles — from <profiles> in project.xml (aggregator or module) }
  if pb.AvailableProfiles.Count > 0 then
  begin
    DirNode := RootNode.AppendText('Build Profiles');
    DirNode.TextColor := clText2;
    for i := 0 to pb.AvailableProfiles.Count - 1 do
    begin
      ProfileNode := DirNode.AppendText(pb.AvailableProfiles[i]);
      ProfileNode.TextColor := clText1;
      if pb.ActiveProfiles.IndexOf(pb.AvailableProfiles[i]) >= 0 then
        ProfileNode.StateImageIndex := 1   { checked }
      else
        ProfileNode.StateImageIndex := 0;  { unchecked }
    end;
    DirNode.Expand;
  end;

  RootNode.Expand;
  tvProject.Selection := RootNode;
  tvProject.Invalidate;
end;

procedure TMainForm.SetupFilesGrid;
begin
  grdFiles.FileList.FileMask := AllFilesMask;
  grdFiles.FileList.ShowHidden := False;
  grdFiles.FileList.ReadDirectory;
  grdFiles.FileList.Sort(soFileName);
  grdFiles.Invalidate;
end;

procedure TMainForm.AddMessage(const AMsg: TfpgString);
begin
  grdMessages.BeginUpdate;
  grdMessages.RowCount := grdMessages.RowCount + 1;
  grdMessages.Cells[0,grdMessages.RowCount-1] := AMsg;
  grdMessages.FocusRow := grdMessages.RowCount;
  grdMessages.EndUpdate;
//  fpgApplication.ProcessMessages;
end;

procedure TMainForm.ClearMessagesWindow;
begin
  grdMessages.RowCount := 0;
end;

procedure TMainForm.CloseAllTabs;
var
  ts: TfpgTabSheet;
  i: integer;
begin
  FHighlightCache.InvalidateEditor(nil);
  pcEditor.BeginUpdate;
  try
    for i := 0 to pcEditor.PageCount-1 do
    begin
      ts := pcEditor.Pages[0];
      pcEditor.RemoveTabSheet(ts);
      ts.Free;
    end;
  finally
    pcEditor.EndUpdate;
  end;
end;

procedure TMainForm.SaveSession;
var
  Session: TIDESession;
  I: Integer;
  ts: TfpgTabSheet;
  editor: TfpgTextEdit;
begin
  if GProject.ProjectDir = '' then
    Exit;
  Session := TIDESession.Create(GProject.ProjectDir);
  try
    Session.ActiveTab := pcEditor.ActivePageIndex;
    Session.ToolPanelWidth := pnlTool.PreferredSize.W;
    Session.BottomPanelHeight := pnlWindow.PreferredSize.H;
    if GProject.ProjectFormat = pfPasBuild then
      Session.ActiveProfiles.Assign(TPasBuildProjectBackend(GProject).ActiveProfiles);
    for I := 0 to pcEditor.PageCount - 1 do
    begin
      ts := pcEditor.Pages[I];
      if ts.Hint <> '' then
      begin
        editor := TfpgTextEdit(ts.Components[0]);
        Session.AddOpenFile(ts.Hint,
          editor.CaretPos_V, editor.CaretPos_H,
          editor.ScrollPos_V, editor.ScrollPos_H, I);
      end;
    end;
    Session.Save;
  finally
    Session.Free;
  end;
end;

procedure TMainForm.LoadProject(const AFilename: TfpgString);
var
  i: integer;
  ts: TfpgTabSheet;
  Session: TIDESession;
  Info: TOpenFileInfo;
  AbsPath: TfpgString;
  editor: TfpgTextEdit;
  SessionLoaded: Boolean;
begin
  { save session info of current project, before opening new project }
  if GProject.ProjectDir <> '' then
    SaveSession;

  // remove all project info
  CloseAllTabs;
  FreeProject;
  // create the appropriate backend for this project format
  SetProject(CreateProjectBackend(AFilename));
  // now load new project info
  GProject.Load(AFilename);
  SetupProjectTree;
  FLastFileDir := GProject.ProjectDir;
  FRecentFiles.AddItem(AFilename);

  // try to restore session state
  SessionLoaded := False;
  Session := TIDESession.Create(GProject.ProjectDir);
  try
    if Session.SessionFileExists then
    begin
      Session.Load;
      SessionLoaded := True;
      for i := 0 to Session.OpenFileCount - 1 do
      begin
        Info := Session.OpenFiles[i];
        AbsPath := ExpandFileName(GProject.ProjectDir + Info.Path);
        if fpgFileExists(AbsPath) then
        begin
          ts := OpenEditorPage(AbsPath);
          editor := TfpgTextEdit(ts.Components[0]);
          editor.CaretPos_V := Info.CaretLine;
          editor.CaretPos_H := Info.CaretCol;
          editor.ScrollPos_V := Info.ScrollTop;
          editor.ScrollPos_H := Info.ScrollLeft;
        end;
      end;
      if (Session.ActiveTab >= 0) and (Session.ActiveTab < pcEditor.PageCount) then
        pcEditor.ActivePageIndex := Session.ActiveTab;
      { Restore active profiles for PasBuild projects }
      if (GProject.ProjectFormat = pfPasBuild) and (Session.ActiveProfiles.Count > 0) then
      begin
        TPasBuildProjectBackend(GProject).ActiveProfiles.Assign(Session.ActiveProfiles);
        TPasBuildProjectBackend(GProject).Resolve(Session.ActiveProfiles.CommaText);
      end;
      { Restore splitter positions }
      if Session.ToolPanelWidth > 0 then
        pnlTool.PreferredSize := fpgSize(Session.ToolPanelWidth, pnlTool.PreferredSize.H);
      if Session.BottomPanelHeight > 0 then
        pnlWindow.PreferredSize := fpgSize(pnlWindow.PreferredSize.W, Session.BottomPanelHeight);
      if (Session.ToolPanelWidth > 0) or (Session.BottomPanelHeight > 0) then
        pnlClientArea.Realign;
    end;
  finally
    Session.Free;
  end;

  // fall back to legacy Opened flags if no session file
  if not SessionLoaded then
  begin
    for i := 0 to GProject.UnitList.Count-1 do
    begin
      if GProject.UnitList[i].Opened then
      begin
        ts := OpenEditorPage(GProject.UnitList[i].FileName);
        ts.TagPointer := GProject.UnitList[i];
      end;
    end;
  end;

  PopuplateProjectTree;
  UpdateWindowTitle;
  UpdateProfilesDisplay;
  UpdateGitBranch;
  CheckGitIgnoreForIdeDir;
  AddMessage('Project loaded');
end;

function TMainForm.CreateNewEditorTab(const ATitle: TfpgString): TfpgTabSheet;
var
  m: TfpgTextEdit;
begin
  Result := pcEditor.AppendTabSheet(ATitle);
  m := TfpgTextEdit.Create(Result);
  m.Left := 1;
  m.Top := 1;
  m.Width := pcEditor.ActualWidth;
  m.Height := pcEditor.ActualHeight;
  m.Align := alClient;
  m.FontDesc := gINI.ReadString(cEditor, 'Font', '#Edit2');
  m.GutterVisible := True;
  m.GutterShowLineNumbers := True;
  m.RightEdge := True;
  m.BackgroundColor := FTheme.Chrome.Background;
  m.FontColor := FTheme.Chrome.Foreground;
  m.SelectionColor := FTheme.Chrome.Selection;
  m.SelectionTextColor := FTheme.Chrome.SelectionText;
  m.LineHighlightColor := FTheme.Chrome.CurrentLine;
end;

function TMainForm.OpenEditorPage(const AFilename: TfpgString): TfpgTabSheet;
var
  s: TfpgString;
  f: TfpgString;
  i: integer;
  found: Boolean;
  ts: TfpgTabSheet;
  ext: TfpgString;
  pos_h: integer;
  pos_v: integer;
  cur_pos_h: integer;
  cur_pos_v: integer;
  editor: TfpgTextEdit;
begin
  s := AFilename;
  f := fpgExtractFileName(s);
  found := False;
  for i := 0 to pcEditor.PageCount-1 do
  begin
    if (pcEditor.Pages[i].Text = f) or (pcEditor.Pages[i].Text = '* ' + f) then
      found := True;
    if found then
      break;
  end;
  if found then
  begin
    // reuse existing tab
    editor := TfpgTextEdit(pcEditor.Pages[i].Components[0]);
    pos_h := editor.ScrollPos_H;
    pos_v := editor.ScrollPos_V;
    cur_pos_h := editor.CaretPos_H;
    cur_pos_v := editor.CaretPos_V;
    editor.OnChange := nil;  // suppress modified indicator during reload
    editor.Lines.BeginUpdate;
    editor.LoadFromFile(s);
    editor.ScrollPos_H := pos_h;
    editor.ScrollPos_V := pos_v;
    editor.CaretPos_H := cur_pos_h;
    editor.CaretPos_V := cur_pos_v;
    editor.UpdateScrollBars;
    editor.Lines.EndUpdate;
    editor.OnChange := @EditorChanged;
    pcEditor.ActivePageIndex := i;
    ts := pcEditor.ActivePage;
    { Clear modified indicator - file now matches disk }
    if Copy(ts.Text, 1, 2) = '* ' then
      ts.Text := Copy(ts.Text, 3, Length(ts.Text));
    { Re-tokenise for syntax highlighting }
    if Assigned(editor.OnDrawLine) then
      RetokeniseEditor(editor);
    AddMessage('File reloaded: ' + s);
  end
  else
  begin
    // we need a new tabsheet
    ts := CreateNewEditorTab(f);
    editor := ts.Components[0] as TfpgTextEdit;
    editor.Lines.BeginUpdate;
    if fpgFileExists(s) then
      editor.Lines.LoadFromFile(s);
    editor.Lines.EndUpdate;
    if gINI.ReadBool(cEditor, 'SyntaxHighlighting', True) then
    begin
      ext := fpgExtractFileExt(AFilename);
      case HighlightKindForExtension(ext) of
        hkPascal:
          begin
            editor.OnDrawLine := @HighlightObjectPascal;
            editor.OnCaretChange := @EditorCaretChanged;
            RetokeniseEditor(editor);
          end;
        hkPatch:
          editor.OnDrawLine := @HighlightPatch;
        hkINI:
          editor.OnDrawLine := @HighlightINI;
        hkXML:
          editor.OnDrawLine := @HighlightXML;
      else
        editor.OnDrawLine := nil;
      end;
    end;
    ts.Realign;
    pcEditor.ActivePage := ts;
    FFileMonitor.AddFile(AFilename);
    editor.OnChange := @EditorChanged;
  end;
  ts.Hint := s;
  Result := ts;
  UpdateStatus(s);
end;

function TMainForm.GetUnitsNode: TfpgTreeNode;
begin
  Result := tvProject.RootNode.FindSubNode('Units', True);
end;

procedure TMainForm.UpdateWindowTitle;
begin
  WindowTitle := Format(cTitle, [GProject.ProjectName]);
end;

procedure TMainForm.HighlightWithTokens(AHighlighter: TEditorHighlighter;
  Sender: TObject; ALineText: TfpgString; ALineIndex: Integer;
  ACanvas: TfpgCanvas; ATextRect: TfpgRect; var AllowSelfDraw: Boolean;
  AShowBracketMatch: Boolean);
var
  edt: TfpgTextEdit;
  segs: TRenderSegmentArray;
  lastCol: Integer;
begin
  edt := TfpgTextEdit(Sender);
  if not Assigned(AHighlighter) then
    Exit;
  AllowSelfDraw := False;
  segs := BuildRenderSegments(AHighlighter, ALineText, ALineIndex,
    FTheme, FBracketMatch, AShowBracketMatch);
  if Length(segs) = 0 then
  begin
    { Empty line — fill background }
    ACanvas.Color := FTheme.Chrome.Background;
    ACanvas.FillRectangle(ATextRect);
    Exit;
  end;
  PaintSegments(segs, ALineText, edt.FontWidth, ACanvas, ATextRect,
    FTheme, edt.FontDesc);
  lastCol := segs[High(segs)].Column + segs[High(segs)].Length;
  PaintTrailingGap(lastCol, ALineText, edt.FontWidth, ACanvas, ATextRect, FTheme);
end;

procedure TMainForm.HighlightObjectPascal(Sender: TObject; ALineText: TfpgString;
  ALineIndex: Integer; ACanvas: TfpgCanvas; ATextRect: TfpgRect;
  var AllowSelfDraw: Boolean);
var
  edt: TfpgTextEdit;
begin
  edt := TfpgTextEdit(Sender);
  FHighlightCache.EnsurePascalTokenised(edt, edt.Lines);
  HighlightWithTokens(FHighlightCache.PascalHighlighter, Sender, ALineText,
    ALineIndex, ACanvas, ATextRect, AllowSelfDraw, True);
end;

procedure TMainForm.HighlightINI(Sender: TObject; ALineText: TfpgString;
  ALineIndex: Integer; ACanvas: TfpgCanvas; ATextRect: TfpgRect;
  var AllowSelfDraw: Boolean);
var
  edt: TfpgTextEdit;
begin
  edt := TfpgTextEdit(Sender);
  FHighlightCache.EnsureINITokenised(edt, edt.Lines);
  HighlightWithTokens(FHighlightCache.INIHighlighter, Sender, ALineText,
    ALineIndex, ACanvas, ATextRect, AllowSelfDraw);
end;

procedure TMainForm.HighlightXML(Sender: TObject; ALineText: TfpgString;
  ALineIndex: Integer; ACanvas: TfpgCanvas; ATextRect: TfpgRect;
  var AllowSelfDraw: Boolean);
var
  edt: TfpgTextEdit;
begin
  edt := TfpgTextEdit(Sender);
  FHighlightCache.EnsureXMLTokenised(edt, edt.Lines);
  HighlightWithTokens(FHighlightCache.XMLHighlighter, Sender, ALineText,
    ALineIndex, ACanvas, ATextRect, AllowSelfDraw);
end;

procedure TMainForm.HighlightPatch(Sender: TObject; ALineText: TfpgString;
  ALineIndex: Integer; ACanvas: TfpgCanvas; ATextRect: TfpgRect;
  var AllowSelfDraw: Boolean);
var
  edt: TfpgTextEdit;
  segs: TRenderSegmentArray;
begin
  edt := TfpgTextEdit(Sender);
  AllowSelfDraw := False;
  segs := BuildPatchRenderSegments(ALineText, FTheme);
  if Length(segs) = 0 then
  begin
    ACanvas.Color := FTheme.Chrome.Background;
    ACanvas.FillRectangle(ATextRect);
    Exit;
  end;
  PaintSegments(segs, ALineText, edt.FontWidth, ACanvas, ATextRect,
    FTheme, edt.FontDesc);
end;

procedure TMainForm.LoadThemeByName(const AName: string);
var
  themeDir: string;
  files: TStringList;
  i: Integer;
  t: TEditorTheme;
begin
  { Check built-in themes first }
  if AName = 'Dark' then
    FTheme := DarkTheme
  else if AName = 'Solarized Dark' then
    FTheme := SolarizedDarkTheme
  else if AName = 'Solarized Light' then
    FTheme := SolarizedLightTheme
  else if AName = 'Default' then
    FTheme := DefaultTheme
  else
  begin
    { Search external INI files for a matching theme name }
    themeDir := fpgExtractFilePath(ParamStr(0)) + 'editor-themes';
    if fpgDirectoryExists(themeDir) then
    begin
      files := TStringList.Create;
      try
        FindThemeFiles(themeDir, files);
        for i := 0 to files.Count - 1 do
        begin
          t := LoadThemeFromINI(files[i]);
          if t.Name = AName then
          begin
            FTheme := t;
            Exit;
          end;
        end;
      finally
        files.Free;
      end;
    end;
    { Fallback to default if not found }
    FTheme := DefaultTheme;
  end;
end;

procedure TMainForm.miJumpToInterface(Sender: TObject);
var
  edt: TfpgTextEdit;
  nav: TNavigationResult;
  ts: TfpgTabSheet;
begin
  if pcEditor.ActivePage = nil then
    Exit;
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  FHighlightCache.EnsurePascalTokenised(edt, edt.Lines);
  nav := NavigateToInterface(FHighlightCache.PascalHighlighter, edt.Lines,
    edt.CaretPos_V, pcEditor.ActivePage.Hint);
  if nav.Found then
  begin
    RecordCursorLocation;
    if nav.Filename <> '' then
    begin
      ts := OpenEditorPage(nav.Filename);
      if ts <> nil then
      begin
        edt := TfpgTextEdit(ts.Components[0]);
        edt.GotoLine(nav.Line + 1);
      end;
    end
    else
      edt.GotoLine(nav.Line + 1);
  end;
end;

procedure TMainForm.miJumpToImplementation(Sender: TObject);
var
  edt: TfpgTextEdit;
  nav: TNavigationResult;
  ts: TfpgTabSheet;
begin
  if pcEditor.ActivePage = nil then
    Exit;
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  FHighlightCache.EnsurePascalTokenised(edt, edt.Lines);
  nav := NavigateToImplementation(FHighlightCache.PascalHighlighter, edt.Lines,
    edt.CaretPos_V, pcEditor.ActivePage.Hint);
  if nav.Found then
  begin
    RecordCursorLocation;
    if nav.Filename <> '' then
    begin
      { Implementation is in an include file — open it }
      ts := OpenEditorPage(nav.Filename);
      if ts <> nil then
      begin
        edt := TfpgTextEdit(ts.Components[0]);
        edt.GotoLine(nav.Line + 1);
      end;
    end
    else
      edt.GotoLine(nav.Line + 1);
  end;
end;

procedure TMainForm.miJumpToggleIntfImpl(Sender: TObject);
var
  edt: TfpgTextEdit;
  nav: TNavigationResult;
  ts: TfpgTabSheet;
begin
  if pcEditor.ActivePage = nil then
    Exit;
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  FHighlightCache.EnsurePascalTokenised(edt, edt.Lines);
  nav := NavigateInterfaceImplementation(FHighlightCache.PascalHighlighter, edt.Lines,
    edt.CaretPos_V, pcEditor.ActivePage.Hint);
  if nav.Found then
  begin
    RecordCursorLocation;
    if nav.Filename <> '' then
    begin
      ts := OpenEditorPage(nav.Filename);
      if ts <> nil then
      begin
        edt := TfpgTextEdit(ts.Components[0]);
        edt.GotoLine(nav.Line + 1);
      end;
    end
    else
      edt.GotoLine(nav.Line + 1);
  end;
end;

procedure TMainForm.miGoToDeclaration(Sender: TObject);

  procedure AddPathIfNew(AList: TStringList; const APath: string);
  begin
    if AList.IndexOf(APath) < 0 then
      AList.Add(APath);
  end;

  function MakeAbsolute(const ABase, APath: string): string;
  begin
    {$ifdef unix}
    if (Length(APath) > 0) and (APath[1] = '/') then
    {$else}
    if (Length(APath) > 1) and (APath[2] = ':') then
    {$endif}
      Result := APath
    else
      Result := IncludeTrailingPathDelimiter(ABase) + APath;
  end;

  procedure AddSubdirectories(AList: TStringList; const ADir: string);
  var
    sr: TSearchRec;
    full: string;
  begin
    AddPathIfNew(AList, IncludeTrailingPathDelimiter(ADir));
    if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faDirectory, sr) = 0 then
    try
      repeat
        if (sr.Attr and faDirectory) <> 0 then
          if (sr.Name <> '.') and (sr.Name <> '..') then
          begin
            full := IncludeTrailingPathDelimiter(ADir) + sr.Name;
            if Pos('target', sr.Name) = 0 then
              AddSubdirectories(AList, full);
          end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;

  procedure CollectPaths(AModule: TPasBuildModule;
    AUnitPaths, AIncludePaths: TStringList);
  var
    i: Integer;
    p, absPath: string;
    dep: TPasBuildDependency;
  begin
    { Add module's own source paths (skip compiled output dirs) }
    for i := 0 to AModule.UnitPaths.Count - 1 do
    begin
      p := AModule.UnitPaths[i];
      if Pos('target/', p) > 0 then
        Continue;
      absPath := MakeAbsolute(AModule.ProjectDir, p);
      AddPathIfNew(AUnitPaths, absPath);
    end;
    for i := 0 to AModule.IncludePaths.Count - 1 do
    begin
      p := AModule.IncludePaths[i];
      if Pos('target/', p) > 0 then
        Continue;
      absPath := MakeAbsolute(AModule.ProjectDir, p);
      AddPathIfNew(AIncludePaths, absPath);
    end;
    { Add dependency source directories -- recursively scan for subdirs
      because dep.SourceDir points to the base (e.g. src/main/pascal) while
      actual unit sources live in subdirectories (corelib, gui, etc.) }
    for i := 0 to AModule.Dependencies.Count - 1 do
    begin
      dep := TPasBuildDependency(AModule.Dependencies[i]);
      if dep.SourceDir <> '' then
        AddSubdirectories(AUnitPaths, dep.SourceDir);
    end;
  end;

var
  edt: TfpgTextEdit;
  decl: TDeclarationResult;
  ts: TfpgTabSheet;
  pb: TPasBuildProjectBackend;
  m: TPasBuildModule;
  ownedUnitPaths, ownedIncludePaths: TStringList;
  unitPaths, includePaths: TStrings;
  i: Integer;
begin
  if pcEditor.ActivePage = nil then
    Exit;
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  FHighlightCache.EnsurePascalTokenised(edt, edt.Lines);

  { Build unit/include paths from project and dependencies }
  ownedUnitPaths := nil;
  ownedIncludePaths := nil;
  unitPaths := nil;
  includePaths := nil;
  if GProject.ProjectFormat = pfPasBuild then
  begin
    pb := TPasBuildProjectBackend(GProject);
    { Ensure project is resolved -- may not be if session had no profiles }
    if not pb.Resolved then
    begin
      WriteLn('DEBUG: Project not resolved, calling Resolve. ActiveProfiles=', pb.ActiveProfiles.CommaText);
      pb.Resolve;
    end;
    WriteLn('DEBUG: Resolved=', pb.Resolved, ' ModuleCount=', pb.Modules.Count);
    m := pb.FindModuleForFile(pcEditor.ActivePage.Hint);
    WriteLn('DEBUG: FindModuleForFile("', pcEditor.ActivePage.Hint, '") = ', m <> nil);
    if m <> nil then
    begin
      ownedUnitPaths := TStringList.Create;
      ownedIncludePaths := TStringList.Create;
      CollectPaths(m, ownedUnitPaths, ownedIncludePaths);
      WriteLn('DEBUG: CollectPaths unitPaths.Count=', ownedUnitPaths.Count,
        ' includePaths.Count=', ownedIncludePaths.Count);
      {$IFDEF DEBUG}
      for i := 0 to ownedUnitPaths.Count - 1 do
        WriteLn('DEBUG:   unitPath[', i, ']=', ownedUnitPaths[i]);
      {$ENDIF}
      unitPaths := ownedUnitPaths;
      includePaths := ownedIncludePaths;
    end;
  end;
  if unitPaths = nil then
    unitPaths := GProject.UnitDirs;

  try
    decl := FindDeclaration(FHighlightCache.PascalHighlighter, edt.Lines,
      pcEditor.ActivePage.Hint, edt.CaretPos_V, edt.CaretPos_H,
      unitPaths, includePaths);
  finally
    ownedUnitPaths.Free;
    ownedIncludePaths.Free;
  end;
  if decl.Found then
  begin
    RecordCursorLocation;
    if (decl.DeclFile <> '') and (decl.DeclFile <> pcEditor.ActivePage.Hint) then
    begin
      ts := OpenEditorPage(decl.DeclFile);
      if ts <> nil then
      begin
        edt := TfpgTextEdit(ts.Components[0]);
        edt.GotoLine(decl.DeclLine);
      end;
    end
    else
      edt.GotoLine(decl.DeclLine);
  end;
end;

function TMainForm.GetCurrentCursorLocation: TCursorLocation;
var
  edt: TfpgTextEdit;
begin
  Result.Filename := '';
  Result.Line := 0;
  Result.Col := 0;
  if pcEditor.ActivePage = nil then
    Exit;
  edt := TfpgTextEdit(pcEditor.ActivePage.Components[0]);
  Result.Filename := pcEditor.ActivePage.Hint;
  Result.Line := edt.CaretPos_V;
  Result.Col := edt.CaretPos_H;
end;

procedure TMainForm.RecordCursorLocation;
var
  loc: TCursorLocation;
begin
  loc := GetCurrentCursorLocation;
  if loc.Filename <> '' then
    FCursorHistory.RecordLocation(loc);
end;

procedure TMainForm.NavigateToLocation(const ALoc: TCursorLocation);
var
  ts: TfpgTabSheet;
  editor: TfpgTextEdit;
begin
  ts := OpenEditorPage(ALoc.Filename);
  if ts = nil then
    Exit;
  editor := TfpgTextEdit(ts.Components[0]);
  editor.GotoLine(ALoc.Line + 1);  { GotoLine is 1-based, ALoc.Line is 0-based }
  editor.CaretPos_H := ALoc.Col;
end;

procedure TMainForm.miNavigateBack(Sender: TObject);
var
  loc, current: TCursorLocation;
begin
  if not FCursorHistory.CanGoBack then
    Exit;
  current := GetCurrentCursorLocation;
  FCursorHistory.RecordBeforeJump(current);
  if FCursorHistory.GoBack(loc) then
    NavigateToLocation(loc);
end;

procedure TMainForm.miNavigateForward(Sender: TObject);
var
  loc: TCursorLocation;
begin
  if FCursorHistory.GoForward(loc) then
    NavigateToLocation(loc);
end;

procedure TMainForm.miNavigateToFile(Sender: TObject);
var
  pb: TPasBuildProjectBackend;
  mi: TAggregatorModuleInfo;
  Exts: TStringList;
  Files: TFileEntryArray;
  SelectedFile: string;
  i: Integer;
begin
  if GProject.ProjectFormat <> pfPasBuild then
    Exit;
  pb := TPasBuildProjectBackend(GProject);
  Exts := TStringList.Create;
  try
    Exts.Add('.pas');
    Exts.Add('.pp');
    Exts.Add('.lpr');
    Exts.Add('.dpr');
    Exts.Add('.inc');
    SetLength(Files, 0);
    if pb.IsAggregator then
    begin
      for i := 0 to pb.ModuleInfos.Count - 1 do
      begin
        mi := TAggregatorModuleInfo(pb.ModuleInfos[i]);
        if not mi.IsAggregator then
          CollectSourceFiles(mi.ProjectDir + mi.SourceDirectory,
            pb.ProjectDir, Exts, Files);
      end;
    end
    else
      CollectSourceFiles(pb.ProjectDir + pb.SourceDirectory,
        pb.ProjectDir, Exts, Files);
  finally
    Exts.Free;
  end;

  SelectedFile := DisplayFileFinder(Files);
  if SelectedFile <> '' then
  begin
    RecordCursorLocation;
    OpenEditorPage(SelectedFile);
  end;
end;

procedure TMainForm.miNavigateToSymbol(Sender: TObject);
var
  pb: TPasBuildProjectBackend;
  mi: TAggregatorModuleInfo;
  Exts: TStringList;
  Files: TFileEntryArray;
  Symbols: TSymbolEntryArray;
  Res: TSymbolFinderResult;
  ts: TfpgTabSheet;
  editor: TfpgTextEdit;
  i: Integer;
begin
  if GProject.ProjectFormat <> pfPasBuild then
    Exit;
  pb := TPasBuildProjectBackend(GProject);
  Exts := TStringList.Create;
  try
    Exts.Add('.pas');
    Exts.Add('.pp');
    Exts.Add('.lpr');
    Exts.Add('.dpr');
    SetLength(Files, 0);
    if pb.IsAggregator then
    begin
      for i := 0 to pb.ModuleInfos.Count - 1 do
      begin
        mi := TAggregatorModuleInfo(pb.ModuleInfos[i]);
        if not mi.IsAggregator then
          CollectSourceFiles(mi.ProjectDir + mi.SourceDirectory,
            pb.ProjectDir, Exts, Files);
      end;
    end
    else
      CollectSourceFiles(pb.ProjectDir + pb.SourceDirectory,
        pb.ProjectDir, Exts, Files);
  finally
    Exts.Free;
  end;

  SetLength(Symbols, 0);
  CollectProjectSymbols(Files, Symbols);

  Res := DisplaySymbolFinder(Symbols);
  if Res.FullPath <> '' then
  begin
    RecordCursorLocation;
    ts := OpenEditorPage(Res.FullPath);
    if ts <> nil then
    begin
      editor := TfpgTextEdit(ts.Components[0]);
      editor.GotoLine(Res.Line);
    end;
  end;
end;

procedure TMainForm.CheckGitIgnoreForIdeDir;
var
  GitIgnorePath: TfpgString;
  Content: TStringList;
  F: TextFile;
begin
  GitIgnorePath := GProject.ProjectDir + '.gitignore';

  { Check if .gitignore already contains .ide/ }
  if fpgFileExists(GitIgnorePath) then
  begin
    Content := TStringList.Create;
    try
      Content.LoadFromFile(GitIgnorePath);
      if (Content.IndexOf('.ide/') >= 0) or (Content.IndexOf('.ide') >= 0) then
        Exit; { already present }
    finally
      Content.Free;
    end;
  end
  else
  begin
    { No .gitignore at all — only offer if .ide/ directory exists }
    if not fpgDirectoryExists(GProject.ProjectDir + '.ide') then
      Exit;
  end;

  if TfpgMessageDialog.Question('Add .ide/ to .gitignore?',
      'The .ide/ directory contains session data that should not be committed. '
      + 'Add it to .gitignore?') = mbYes then
  begin
    AssignFile(F, GitIgnorePath);
    if fpgFileExists(GitIgnorePath) then
      Append(F)
    else
      Rewrite(F);
    try
      WriteLn(F, '.ide/');
    finally
      CloseFile(F);
    end;
  end;
end;

procedure TMainForm.SetupEditorPreference;
var
  i: integer;
begin
  pcEditor.TabPosition := TfpgTabPosition(gINI.ReadInteger(cEditor, 'TabPosition', 0));
  pcEditor.ActiveTabColor := gINI.ReadColor(cEditor, 'ActiveTabColor', pcEditor.BackgroundColor);
  LoadThemeByName(gINI.ReadString(cEditor, 'Theme', 'Default'));
  for i := 0 to pcEditor.PageCount-1 do
  begin
    TfpgTextEdit(pcEditor.Pages[i].Components[0]).FontDesc := gINI.ReadString(cEditor, 'Font', '#Edit2');
    TfpgTextEdit(pcEditor.Pages[i].Components[0]).BackgroundColor := FTheme.Chrome.Background;
    TfpgTextEdit(pcEditor.Pages[i].Components[0]).FontColor := FTheme.Chrome.Foreground;
    TfpgTextEdit(pcEditor.Pages[i].Components[0]).SelectionColor := FTheme.Chrome.Selection;
    TfpgTextEdit(pcEditor.Pages[i].Components[0]).SelectionTextColor := FTheme.Chrome.SelectionText;
    TfpgTextEdit(pcEditor.Pages[i].Components[0]).LineHighlightColor := FTheme.Chrome.CurrentLine;
  end;
end;

procedure TMainForm.MonitoredFileChanged(Sender: TObject; AData: TFileMonitorEventData);
begin
  OpenEditorPage(AData.FileName);
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  lErrPos: integer;
begin
  Left := gINI.ReadInteger(Name + 'State', 'Left', Left);
  Top := gINI.ReadInteger(Name + 'State', 'Top', Top);
  Width := gINI.ReadInteger(Name + 'State', 'Width', ActualWidth);
  Height := gINI.ReadInteger(Name + 'State', 'Height', ActualHeight);
  UpdatePosition;

  SetupProjectTree;
  SetupFilesGrid;
  SetupEditorPreference;

  TextEditor.Clear;
  TextEditor.SetFocus;

  FFileMonitor.Resume;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Session: TIDESession;
  I: Integer;
  ts: TfpgTabSheet;
  editor: TfpgTextEdit;
begin
  CloseAction := caFree;
  gINI.WriteInteger(Name + 'State', 'Left', Left);
  gINI.WriteInteger(Name + 'State', 'Top', Top);
  gINI.WriteInteger(Name + 'State', 'Width', ActualWidth);
  gINI.WriteInteger(Name + 'State', 'Height', ActualHeight);

  SaveSession;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow  := @FormShow;
  OnClose := @FormClose;
  FFileMonitor := TFileMonitor.CreateCustom;
  FFileMonitor.OnFileChanged  := @MonitoredFileChanged;
  FHighlightCache := THighlighterCache.Create;
  FCursorHistory := TCursorHistory.Create(50);
  FTheme := DefaultTheme;

  { Build state image list for tree checkboxes (16x16 masked BMPs) }
  FProfileStateImages := TfpgImageList.Create;
  FProfileStateImages.AddImage(
    CreateImage_BMP(@cCheckboxUnchecked, SizeOf(cCheckboxUnchecked)), 0);
  FProfileStateImages.Items[0].Image.CreateMaskFromSample(0, 0);
  FProfileStateImages.Items[0].Image.UpdateImage;
  FProfileStateImages.AddImage(
    CreateImage_BMP(@cCheckboxChecked, SizeOf(cCheckboxChecked)), 1);
  FProfileStateImages.Items[1].Image.CreateMaskFromSample(0, 0);
  FProfileStateImages.Items[1].Image.UpdateImage;
end;

destructor TMainForm.Destroy;
begin
  FFileMonitor.Terminate;
  FFileMonitor.Free;
  FreeAndNil(FHighlightCache);
  FreeAndNil(FCursorHistory);
  if Assigned(tvProject) then
    tvProject.StateImageList := nil;
  FreeAndNil(FProfileStateImages);
  inherited Destroy;
end;

procedure TMainForm.uiCreateToolBar;
var
  mig: TfpgMigLayoutManager;
begin
  mig := TfpgMigLayoutManager.Create;

  Toolbar := TfpgBevel.Create(self);
  with Toolbar do
  begin
    Name := 'Toolbar';
    PreferredSize := fpgSize(600, 28);
    Shape := bsSpacer;
  end;

  btnQuit := TfpgButton.Create(Toolbar);
  with btnQuit do
  begin
    Name := 'btnQuit';
    PreferredSize := fpgSize(24, 24);
    Text := '';
    Embedded := True;
    ImageMargin := 0;
    ImageName := 'stdimg.quit';
    OnClick := @btnQuitClicked;
  end;

  btnOpen := TfpgButton.Create(Toolbar);
  with btnOpen do
  begin
    Name := 'btnOpen';
    PreferredSize := fpgSize(24, 24);
    Text := '';
    Embedded := True;
    ImageMargin := 0;
    ImageName := 'stdimg.open';
    OnClick := @btnOpenFileClicked;
  end;

  btnSave := TfpgButton.Create(Toolbar);
  with btnSave do
  begin
    Name := 'btnSave';
    PreferredSize := fpgSize(24, 24);
    Text := '';
    Embedded := True;
    ImageMargin := 0;
    ImageName := 'stdimg.save';
    OnClick := @miFileSave;
  end;

  btnSaveAll := TfpgButton.Create(Toolbar);
  with btnSaveAll do
  begin
    Name := 'btnSaveAll';
    PreferredSize := fpgSize(24, 24);
    Text := '';
    Embedded := True;
    Enabled := False;
    ImageMargin := 0;
    ImageName := 'stdimg.saveall';
  end;

  Toolbar.LayoutManager := mig;
  mig.LC.InsetsAll('2lp').Fill;
  mig.AddLayoutComponent(btnQuit, TfpgMigCC.Create.MinWidth('24lp'));
  mig.AddLayoutComponent(btnOpen, TfpgMigCC.Create.MinWidth('24lp'));
  mig.AddLayoutComponent(btnSave, TfpgMigCC.Create.MinWidth('24lp'));
  mig.AddLayoutComponent(btnSaveAll, TfpgMigCC.Create.MinWidth('24lp').PushX);
end;

procedure TMainForm.uiCreateStatusBar;
begin
  FStatusBarLayout := TfpgMigLayoutManager.Create;

  pnlStatusBar := TfpgBevel.Create(self);
  with pnlStatusBar do
  begin
    Name := 'pnlStatusBar';
    PreferredSize := fpgSize(600, 22);
    Style := bsLowered;
  end;

  lblStatus := TfpgLabel.Create(pnlStatusBar);
  with lblStatus do
  begin
    Name := 'lblStatus';
    PreferredSize := fpgSize(400, 16);
    FontDesc := '#Label1';
    Text := '';
  end;

  lblCursorPos := TfpgLabel.Create(pnlStatusBar);
  with lblCursorPos do
  begin
    Name := 'lblCursorPos';
    PreferredSize := fpgSize(100, 16);
    FontDesc := '#Label1';
    Text := '';
  end;

  lblGitBranch := TfpgLabel.Create(pnlStatusBar);
  with lblGitBranch do
  begin
    Name := 'lblGitBranch';
    PreferredSize := fpgSize(100, 16);
    FontDesc := '#Label1';
    Text := '';
  end;

  lblProfiles := TfpgLabel.Create(pnlStatusBar);
  with lblProfiles do
  begin
    Name := 'lblProfiles';
    PreferredSize := fpgSize(150, 16);
    FontDesc := '#Label1';
    Hint := 'Active build profiles — click to change';
    Text := '';
    OnClick := @lblProfilesClicked;
  end;

  pmProfileMenu := TfpgPopupMenu.Create(self);

  pnlStatusBar.LayoutManager := FStatusBarLayout;
  FStatusBarLayout.LC.InsetsAll('2lp').FillX;
  FStatusBarLayout.AddLayoutComponent(lblStatus, TfpgMigCC.Create.GrowX.PushX);
  FStatusBarLayout.AddLayoutComponent(lblCursorPos, TfpgMigCC.Create.AlignX('right'));
  FStatusBarLayout.AddLayoutComponent(lblGitBranch, TfpgMigCC.Create.AlignX('right'));
  FStatusBarLayout.AddLayoutComponent(lblProfiles, TfpgMigCC.Create.AlignX('right'));
end;

procedure TMainForm.uiCreateClientArea;
var
  lm: TfpgMigLayoutManager;
begin
  lm := TfpgMigLayoutManager.Create;

  {%region 'client area' -fold}
  pnlClientArea := TfpgBevel.Create(self);
  with pnlClientArea do
  begin
    Name := 'pnlClientArea';
    PreferredSize := fpgSize(600, 350);
    Shape := bsSpacer;
  end;

  { Left tool panel — project tree and files }
  pnlTool := TfpgPageControl.Create(pnlClientArea);
  with pnlTool do
  begin
    Name := 'pnlTool';
    PreferredSize := fpgSize(160, 250);
    ActivePageIndex := 0;
  end;

  tsProject := TfpgTabSheet.Create(pnlTool);
  with tsProject do
  begin
    Name := 'tsProject';
    Text := 'Project';
  end;

  tvProject := TfpgTreeView.Create(tsProject);
  with tvProject do
  begin
    Name := 'tvProject';
    Align := alClient;
    FontDesc := '#Label1';
    ShowImages := True;
    StateImageList := FProfileStateImages;
    IndentNodeWithNoImage := False;
    OnDoubleClick := @tvProjectDoubleClick;
    OnKeyPress := @tvProjectKeyPressed;
    OnStateImageClicked := @tvProfileStateImageClicked;
  end;

  tsFiles := TfpgTabSheet.Create(pnlTool);
  with tsFiles do
  begin
    Name := 'tsFiles';
    Text := 'Files';
  end;

  grdFiles := TfpgFileGrid.Create(tsFiles);
  with grdFiles do
  begin
    Name := 'grdFiles';
    Align := alClient;
    Options := Options + [go_SmoothScroll];
  end;

  { Vertical splitter — between tool panel and editor }
  SplitterV := TfpgMigSplitter.Create(pnlClientArea);
  with SplitterV do
  begin
    Name := 'SplitterV';
    Orientation := soVertical;
    Control := pnlTool;
    MinSize := 100;
  end;

  { Centre editor area }
  pcEditor := TfpgPageControl.Create(pnlClientArea);
  with pcEditor do
  begin
    Name := 'pcEditor';
    PreferredSize := fpgSize(400, 250);
    ActivePageIndex := 0;
    TabPosition := tpRight;
    OnClosingTabSheet := @TabSheetClosing;
    OnMouseUp := @pcEditorMouseUp;
    OnChange := @EditorTabChanged;
  end;

  tseditor := TfpgTabSheet.Create(pcEditor);
  with tseditor do
  begin
    Name := 'tseditor';
    Text := 'Tabsheet1';
  end;

  TextEditor := TfpgTextEdit.Create(tseditor);
  with TextEditor do
  begin
    Name := 'TextEditor';
    Align := alClient;
    GutterVisible := True;
    GutterShowLineNumbers := True;
    FontDesc := '#Edit2';
  end;

  { Horizontal splitter — between editor row and bottom panel }
  SplitterH := TfpgMigSplitter.Create(pnlClientArea);
  with SplitterH do
  begin
    Name := 'SplitterH';
    Orientation := soHorizontal;
    MinSize := 60;
    // Control set after pnlWindow is created
  end;

  { Bottom output panel — messages, scribble, terminal }
  pnlWindow := TfpgPageControl.Create(pnlClientArea);
  with pnlWindow do
  begin
    Name := 'pnlWindow';
    PreferredSize := fpgSize(600, 100);
    ActivePageIndex := 0;
    TabPosition := tpRight;
  end;

  tsMessages := TfpgTabSheet.Create(pnlWindow);
  with tsMessages do
  begin
    Name := 'tsMessages';
    Text := 'Messages';
  end;

  grdMessages := TfpgStringGrid.Create(tsMessages);
  with grdMessages do
  begin
    Name := 'grdMessages';
    Align := alClient;
    BackgroundColor := TfpgColor($80000002);
    AddColumn('New', 2000, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    RowCount := 0;
    RowSelect := True;
    ShowHeader := False;
    OnKeyPress := @grdMessageKeyPressed;
  end;

  tsScribble := TfpgTabSheet.Create(pnlWindow);
  with tsScribble do
  begin
    Name := 'tsScribble';
    Text := 'Scribble';
  end;

  memScribble := TfpgMemo.Create(tsScribble);
  with memScribble do
  begin
    Name := 'memScribble';
    Align := alClient;
    FontDesc := '#Edit2';
    Lines.Add('Make notes, use it as a clipboard');
    Lines.Add('or type whatever you want...');
  end;

  tsTerminal := TfpgTabSheet.Create(pnlWindow);
  with tsTerminal do
  begin
    Name := 'tsTerminal';
    Text := 'Terminal';
  end;
  {%endregion}

  { Context menu for editor tabs }
  pmTabMenu := TfpgPopupMenu.Create(self);
  with pmTabMenu do
  begin
    AddMenuItem('Close', '', @pmTabCloseClick);
    AddMenuItem('Close Others', '', @pmTabCloseOthersClick);
    AddMenuItem('Close All', '', @pmTabCloseAllClick);
    AddSeparator;
    AddMenuItem('Copy Path', '', @pmTabCopyPathClick);
  end;

  { Context menu for module tree nodes }
  pmModuleMenu := TfpgPopupMenu.Create(self);
  with pmModuleMenu do
  begin
    AddMenuItem('Build Module', '', @pmModuleBuildClick);
    AddMenuItem('Clean Module', '', @pmModuleCleanClick);
    AddMenuItem('Rebuild Module', '', @pmModuleRebuildClick);
    AddSeparator;
    AddMenuItem('Show Dependency Tree', '', @pmTreeDependencyTreeClick);
  end;

  { Context menu for project tree (non-aggregator PasBuild projects) }
  pmProjectTreeMenu := TfpgPopupMenu.Create(self);
  with pmProjectTreeMenu do
  begin
    AddMenuItem('Show Dependency Tree', '', @pmTreeDependencyTreeClick);
  end;

  SplitterH.Control := pnlWindow;

  { Lay out the client area as a grid:
    Row 0: pnlTool | SplitterV | pcEditor        (wrap)
    Row 1: (span)  | (span)    | SplitterH       (wrap)
    Row 2: (span)  | (span)    | pnlWindow

    pnlTool and SplitterV span all 3 rows. }
  pnlClientArea.LayoutManager := lm;
  lm.LC.InsetsAll('0').Fill;
  lm.AddLayoutComponent(pnlTool, TfpgMigCC.Create.SpanY(3).MinWidth('100lp').GrowY.PushY);
  lm.AddLayoutComponent(SplitterV, TfpgMigCC.Create.SpanY(3).Width('3lp!').GrowY);
  lm.AddLayoutComponent(pcEditor, TfpgMigCC.Create.GrowX.GrowY.Push.Wrap);
  lm.AddLayoutComponent(SplitterH, TfpgMigCC.Create.GrowX.Height('3lp!').Wrap);
  lm.AddLayoutComponent(pnlWindow, TfpgMigCC.Create.GrowX.MinHeight('60lp'));
end;

procedure TMainForm.uiCreateMenus;
begin
  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    miFile := AddMenuItem('New...', rsKeyCtrl+'N', @miFileNewUnit);
    AddSeparator;
    AddMenuItem('Open...', rsKeyCtrl+'O', @btnOpenFileClicked);
    AddMenuItem('Open Recent', '', nil).Enabled := False;
    AddSeparator;
    AddMenuItem('Save', rsKeyCtrl+'S', @miFileSave);
    AddMenuItem('Save As...', '', @miFileSaveAs);
    AddMenuItem('Save All', rsKeyCtrl+rsKeyShift+'S', nil).Enabled := False;
    AddSeparator;
    AddMenuItem('Close', rsKeyCtrl+'F4', @miFileClose);
    AddMenuItem('Quit', rsKeyCtrl+'Q', @btnQuitClicked);
  end;

  mnuEdit := TfpgPopupMenu.Create(self);
  with mnuEdit do
  begin
    Name := 'mnuEdit';
    AddMenuItem('Undo', rsKeyCtrl+'Z', @miEditUndoClicked);
    AddMenuItem('Redo', rsKeyCtrl+rsKeyShift+'Z', @miEditRedoClicked);
    AddSeparator;
    AddMenuItem('Cut', rsKeyCtrl+'X', @miEditCutClicked);
    AddMenuItem('Copy', rsKeyCtrl+'C', @miEditCopyClicked);
    AddMenuItem('Paste', rsKeyCtrl+'V', @miEditPasteClicked);
    AddSeparator;
    AddMenuItem('Duplicate Line', rsKeyCtrl+'D', @miEditDuplicateLineClicked);
    AddMenuItem('Delete Line', rsKeyCtrl+'Y', @miEditDeleteLineClicked);
    AddSeparator;
    AddMenuItem('Indent selection', rsKeyCtrl+'I', nil).Enabled := False;
    AddMenuItem('Unindent selection', rsKeyCtrl+'U', nil).Enabled := False;
    AddMenuItem('Insert $IFDEF...', rsKeyCtrl+rsKeyShift+'D', nil).Enabled := False;
  end;

  mnuSearch := TfpgPopupMenu.Create(self);
  with mnuSearch do
  begin
    Name := 'mnuSearch';
    AddMenuItem('Find...', rsKeyCtrl+'F', @miFindClicked);
    AddMenuItem('Find Next', 'F3', @miFindNextClicked);
    AddMenuItem('Find Previous', rsKeyShift+'F3', @miFindPrevClicked);
    AddMenuItem('Find in Files...', rsKeyCtrl+rsKeyShift+'F', nil).Enabled := False;
    AddMenuItem('Replace...', rsKeyCtrl+'R', nil).Enabled := False;
    AddSeparator;
    AddMenuItem('Procedure List...', rsKeyCtrl+'G', @miSearchProcedureList);
    AddMenuItem('Go to line...', rsKeyAlt+'G', @miGoToLineClick);
    AddMenuItem('Navigate to File...', rsKeyCtrl+rsKeyShift+'N', @miNavigateToFile);
    AddMenuItem('Navigate to Symbol...', rsKeyCtrl+rsKeyShift+'O', @miNavigateToSymbol);
    AddSeparator;
    AddMenuItem('Jump to Interface', rsKeyCtrl+rsKeyShift+'Up', @miJumpToInterface);
    AddMenuItem('Jump to Implementation', rsKeyCtrl+rsKeyShift+'Down', @miJumpToImplementation);
    AddMenuItem('Toggle Interface/Implementation', rsKeyCtrl+rsKeyShift+'J', @miJumpToggleIntfImpl);
    AddMenuItem('Go to Declaration', rsKeyCtrl+'B', @miGoToDeclaration);
    AddSeparator;
    AddMenuItem('Navigate Back', rsKeyAlt+'Left', @miNavigateBack);
    AddMenuItem('Navigate Forward', rsKeyAlt+'Right', @miNavigateForward);
  end;

  mnuView := TfpgPopupMenu.Create(self);
  with mnuView do
  begin
    Name := 'mnuView';
    AddMenuItem('Todo List...', rsKeyCtrl+'F2', nil).Enabled := False;
    AddMenuItem('Debug Windows', '', @miViewDebug);
  end;

  mnuProject := TfpgPopupMenu.Create(self);
  with mnuProject do
  begin
    Name := 'mnuProject';
    AddMenuItem('Options...', rsKeyCtrl+rsKeyShift+'F11', @miProjectOptions);
    AddSeparator;
    AddMenuItem('New (empty)...', '', @miProjectNew);
    AddMenuItem('New from Template...', '', @miProjectNewFromTemplate);
    AddMenuItem('Open...', '', @miProjectOpen);
    miRecentProjects := AddMenuItem('Open Recent', '', nil);
    AddMenuItem('Save', rsKeyCtrl+rsKeyAlt+'S', @miProjectSave);
    AddMenuItem('Save As...', '', @miProjectSaveAs);
    AddSeparator;
    AddMenuItem('View Source', '', nil);
    AddMenuItem('Add editor file to Project', rsKeyCtrl+rsKeyShift+'A', @miProjectAddUnitToProject);
    AddSeparator;
    AddMenuItem('Show Dependency Tree', '', @miProjectDependencyTree);
  end;

  mnuRun := TfpgPopupMenu.Create(self);
  with mnuRun do
  begin
    Name := 'mnuRun';
    AddMenuItem('Make', rsKeyCtrl+'F9', @miRunMake);
    AddMenuItem('Build All', rsKeyCtrl+rsKeyShift+'F9', @miRunBuild);
    AddMenuItem('Make 1', rsKeyCtrl+rsKeyAlt+'1', @miRunMake1);
    AddMenuItem('Make 2', rsKeyCtrl+rsKeyAlt+'2', @miRunMake2);
    AddMenuItem('Make 3', rsKeyCtrl+rsKeyAlt+'3', @miRunMake3);
    AddMenuItem('Make 4', rsKeyCtrl+rsKeyAlt+'4', @miRunMake4);
    AddSeparator;
    AddMenuItem('Clean', '', @miRunClean);
    AddMenuItem('Rebuild', '', @miRunRebuild);
    AddMenuItem('Test', rsKeyCtrl+rsKeyShift+'F10', @miRunTest);
    AddSeparator;
    AddMenuItem('Run', 'F9', nil);
    AddMenuItem('Run Parameters...', rsKeyShift+'F9', nil);
  end;

  mnuTools := TfpgPopupMenu.Create(self);
  with mnuTools do
  begin
    Name := 'mnuTools';
    AddMenuItem('fpGUI UI Designer...', 'F12', nil);
    AddMenuItem('fpGUI DocView...', rsKeyCtrl+'F1', nil);
  end;

  mnuSettings := TfpgPopupMenu.Create(self);
  with mnuSettings do
  begin
    Name := 'mnuSettings';
    AddMenuItem('Configure IDE...', '', @miConfigureIDE);
  end;

  mnuHelp := TfpgPopupMenu.Create(self);
  with mnuHelp do
  begin
    Name := 'mnuHelp';
    AddMenuItem('Contents...', '', nil);
    AddSeparator;
    AddMenuItem('About fpGUI Toolkit...', '', @miAboutFPGuiClicked);
    AddMenuItem('About fpGUI IDE...', '', @miAboutIDE);
  end;

  mainmenu.AddMenuItem('&File', nil).SubMenu := mnuFile;
  mainmenu.AddMenuItem('&Edit', nil).SubMenu := mnuEdit;
  mainmenu.AddMenuItem('&Search', nil).SubMenu := mnuSearch;
  mainmenu.AddMenuItem('&View', nil).SubMenu := mnuView;
  mainmenu.AddMenuItem('&Project', nil).SubMenu := mnuProject;
  mainmenu.AddMenuItem('&Run', nil).SubMenu := mnuRun;
  mainmenu.AddMenuItem('&Tools', nil).SubMenu := mnuTools;
  mainmenu.AddMenuItem('Sett&ings', nil).SubMenu := mnuSettings;
  mainmenu.AddMenuItem('&Help', nil).SubMenu := mnuHelp;

  pmOpenRecentMenu := TfpgPopupMenu.Create(self);
  with pmOpenRecentMenu do
  begin
    Name := 'pmOpenRecentMenu';
  end;

  miRecentProjects.SubMenu := pmOpenRecentMenu;

  FRecentFiles := TfpgMRU.Create(self);
  FRecentFiles.ParentMenuItem := pmOpenRecentMenu;
  FRecentFiles.OnClick        := @miRecentProjectsClick;
  FRecentFiles.MaxItems       := gINI.ReadInteger('Options', 'MRUProjectCount', 10);
  FRecentFiles.ShowFullPath   := gINI.ReadBool('Options', 'ShowFullPath', True);
  FRecentFiles.LoadMRU;
end;

procedure TMainForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
begin
  Name := 'MainForm';
  SetPosition(310, 206, 638, 428);
  WindowTitle := 'fpGUI Maximus IDE - %s';
  WindowPosition := wpOneThirdDown;
  MinWidth := 580;
  MinHeight := 400;

  { Top-level MigLayout for the form }
  mig := TfpgMigLayoutManager.Create;
  mig.LC.InsetsAll('0').Fill;
  LayoutManager := mig;

  { Menu bar — docked north }
  mainmenu := TfpgMenuBar.Create(self);
  with mainmenu do
  begin
    Name := 'mainmenu';
    PreferredSize := fpgSize(600, 24);
  end;

  uiCreateToolBar;
  uiCreateStatusBar;
  uiCreateClientArea;
  uiCreateMenus;

  mig.AddLayoutComponent(mainmenu, TfpgMigCC.Create.DockNorth.GrowX.Height('24lp!'));
  mig.AddLayoutComponent(Toolbar, TfpgMigCC.Create.DockNorth.GrowX.Height('28lp!'));
  mig.AddLayoutComponent(pnlStatusBar, TfpgMigCC.Create.DockSouth.GrowX.Height('22lp!'));
  mig.AddLayoutComponent(pnlClientArea, TfpgMigCC.Create.GrowX.GrowY.Push);
end;


end.
