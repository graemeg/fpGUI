.* This file is encoded using IBM850 encoding
.* :encoding=IBM850:wrap=hard:tabSize=2:noTabs=true:

.* =============================================
.* Copyright (c) 2016-2026 by Graeme Geldenhuys
.* =============================================

.* =============================================
.* Layout Management chapter
.* For inclusion in: a_quick_guide_to_fpgui.ipf
.* =============================================

:h3 id=ch_layout_management.Layout Management
:cgraphic.
þþþþþþþþþþþþþþþþþþþ
 Layout Management
þþþþþþþþþþþþþþþþþþþ
:ecgraphic.

:h4 id=ch_what_are_lm.What Are Layout Managers?
:cgraphic.
þþþþþþþþþþþþþþþþþþþþþþþþþþþþ
 What Are Layout Managers?
þþþþþþþþþþþþþþþþþþþþþþþþþþþþ
:ecgraphic.
:p.
A layout manager is an object that controls the positioning and sizing of
child widgets within a container. Instead of manually calculating pixel
coordinates for every widget, a layout manager applies an algorithm to arrange
widgets automatically.

:p.
&fpg. provides three built-in layout managers&colon.

:table cols='30 50'.
:row.
:c.:hp2.Layout Manager:ehp2.
:c.:hp2.Description:ehp2.
:row.
:c.:color fc=darkred.TfpgFlowLayoutManager:color fc=default.
:c.Arranges widgets in a horizontal row, wrapping to the next line when the
container edge is reached. Similar to how text wraps in a paragraph.
:row.
:c.:color fc=darkred.TfpgBorderLayoutManager:color fc=default.
:c.Divides the container into five regions&colon. North, South, East, West, and
Center. Widgets dock to edges, and the center widget fills the remaining space.
:row.
:c.:color fc=darkred.TfpgMigLayoutManager:color fc=default.
:c.A powerful grid-based layout manager ported from Java's MigLayout v11.
Supports column/row spanning, growth priorities, alignment, docking, and
constraint-based sizing.
:etable.

:h5 id=ch_why_use_lm.Why Use Layout Managers?
:p.
Layout managers solve several common problems with manual widget positioning&colon.

:ul.
:li.:hp2.No manual coordinate calculations:ehp2. &endash. widgets are positioned
automatically
:li.:hp2.Automatic resize behaviour:ehp2. &endash. the layout adapts when the
window is resized
:li.:hp2.Consistent spacing:ehp2. &endash. gaps between widgets are managed
uniformly
:li.:hp2.DPI awareness:ehp2. &endash. layouts scale correctly across different
display densities
:li.:hp2.Maintainability:ehp2. &endash. adding or removing a widget does not
require recalculating positions for every other widget
:eul.

:h5 id=ch_coexist_align.Coexistence with Align/Anchors
:p.
Layout managers coexist with &fpg.'s traditional :color fc=darkred.Align:color
fc=default. and :color fc=darkred.Anchors:color fc=default. system. They are
entirely optional. When a container has a layout manager assigned, it delegates
positioning to that manager. When no layout manager is assigned, the existing
Align/Anchors behaviour applies unchanged.

:p.
A container can use a layout manager for its children while itself being
positioned by its parent's Align/Anchors system. This allows gradual adoption
without rewriting existing code.


:h4 id=ch_lm_core_concepts.Core Concepts
:cgraphic.
þþþþþþþþþþþþþþþ
 Core Concepts
þþþþþþþþþþþþþþþ
:ecgraphic.
:p.
Every layout manager follows the same three-step pattern&colon.

:ol compact.
:li.:hp2.Create:ehp2. a layout manager instance
:li.:hp2.Assign:ehp2. it to a container's :color fc=darkred.LayoutManager:color
fc=default. property
:li.:hp2.Add:ehp2. child widgets with :color fc=darkred.AddLayoutComponent(widget,
constraint):color fc=default.
:eol.

:p.
Each layout manager defines its own constraint class. Constraints describe how a
specific widget should behave within the layout &endash. for example, which grid
cell it occupies, whether it should grow to fill available space, or which
region it docks to.


:h4 id=ch_flowlayout.FlowLayout &endash. The Simplest Layout Manager
:cgraphic.
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
 FlowLayout &endash. The Simplest Layout Manager
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
:ecgraphic.
:p.
:color fc=darkred.TfpgFlowLayoutManager:color fc=default. arranges widgets in a
horizontal row from left to right. When a widget would exceed the container
width, it wraps to the next line. This is the simplest layout manager and a
good starting point.

:h5 id=ch_flow_example.Minimal Example
:xmp.
:hp2.uses:ehp2.
  SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_button,
  fpg_flowlayout,       :color fc=darkcyan.// Flow layout manager:color fc=default.
  fpg_layouttypes;      :color fc=darkcyan.// TfpgLayoutConstraint base class:color fc=default.

:hp2.type:ehp2.
  TMainForm = :hp2.class:ehp2.(TfpgForm)
  :hp2.public:ehp2.
    :hp2.procedure:ehp2. AfterCreate; :hp2.override:ehp2.;
  :hp2.end:ehp2.;

:hp2.procedure:ehp2. TMainForm.AfterCreate;
:hp2.var:ehp2.
  flow: TfpgFlowLayoutManager;
  btn: TfpgButton;
  i: Integer;
:hp2.begin:ehp2.
  WindowTitle := 'Flow Layout Demo';
  Width := 400;
  Height := 200;

  :color fc=darkcyan.// 1. Create the layout manager:color fc=default.
  flow := TfpgFlowLayoutManager.Create;
  flow.HGap := 6;   :color fc=darkcyan.// horizontal gap between widgets:color fc=default.
  flow.VGap := 6;   :color fc=darkcyan.// vertical gap between rows:color fc=default.

  :color fc=darkcyan.// 2. Assign it to the form:color fc=default.
  LayoutManager := flow;

  :color fc=darkcyan.// 3. Add widgets:color fc=default.
  :hp2.for:ehp2. i := 1 :hp2.to:ehp2. 8 :hp2.do:ehp2.
  :hp2.begin:ehp2.
    btn := TfpgButton.Create(Self);
    btn.Text := Format('Button %d', [i]);
    btn.Width := 80;
    btn.Height := 28;
    flow.AddLayoutComponent(btn, TfpgLayoutConstraint.Create);
  :hp2.end:ehp2.;
:hp2.end:ehp2.;
:exmp.

:h5 id=ch_flow_properties.FlowLayout Properties

:table cols='15 40'.
:row.
:c.:hp2.Property:ehp2.
:c.:hp2.Description:ehp2.
:row.
:c.:color fc=darkred.HGap:color fc=default.
:c.Horizontal gap in pixels between adjacent widgets (default&colon. 4)
:row.
:c.:color fc=darkred.VGap:color fc=default.
:c.Vertical gap in pixels between rows (default&colon. 4)
:row.
:c.:color fc=darkred.Alignment:color fc=default.
:c.Horizontal alignment of each row&colon. flaLeft, flaCenter, flaRight
:row.
:c.:color fc=darkred.VAlignment:color fc=default.
:c.Vertical alignment of widgets within a row&colon. flvaTop, flvaCenter,
flvaBottom
:etable.


:h4 id=ch_borderlayout.BorderLayout &endash. Five-Region Docking
:cgraphic.
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
 BorderLayout &endash. Five-Region Docking
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
:ecgraphic.
:p.
:color fc=darkred.TfpgBorderLayoutManager:color fc=default. divides a container
into five regions. North and South span the full width. West and East occupy the
sides between North and South. Center fills the remaining space. This is
well-suited for application frames with toolbars, status bars, side panels, and
a main content area.

:h5 id=ch_border_example.Example
:xmp.
:hp2.uses:ehp2.
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_panel,
  fpg_borderlayout;     :color fc=darkcyan.// Border layout manager + constraint:color fc=default.

:hp2.type:ehp2.
  TMainForm = :hp2.class:ehp2.(TfpgForm)
  :hp2.public:ehp2.
    :hp2.procedure:ehp2. AfterCreate; :hp2.override:ehp2.;
  :hp2.end:ehp2.;

:hp2.procedure:ehp2. TMainForm.AfterCreate;
:hp2.var:ehp2.
  lm: TfpgBorderLayoutManager;
  constraint: TfpgBorderLayoutConstraint;
  pnl: TfpgPanel;
  btn: TfpgButton;
:hp2.begin:ehp2.
  WindowTitle := 'Border Layout Demo';
  Width := 500;
  Height := 350;

  :color fc=darkcyan.// 1. Create with gaps (horizontal=8, vertical=4):color fc=default.
  lm := TfpgBorderLayoutManager.Create(8, 4);

  :color fc=darkcyan.// 2. Assign to form:color fc=default.
  LayoutManager := lm;

  :color fc=darkcyan.// 3. Add widgets to regions:color fc=default.
  :color fc=darkcyan.// -- North (toolbar area) --:color fc=default.
  btn := TfpgButton.Create(Self);
  btn.Text := 'North - Toolbar';
  btn.Height := 32;
  constraint := TfpgBorderLayoutConstraint.Create;
  constraint.Region := blrNorth;
  lm.AddLayoutComponent(btn, constraint);

  :color fc=darkcyan.// -- South (status bar) --:color fc=default.
  btn := TfpgButton.Create(Self);
  btn.Text := 'South - Status';
  btn.Height := 24;
  constraint := TfpgBorderLayoutConstraint.Create;
  constraint.Region := blrSouth;
  lm.AddLayoutComponent(btn, constraint);

  :color fc=darkcyan.// -- West (side panel) --:color fc=default.
  btn := TfpgButton.Create(Self);
  btn.Text := 'West';
  btn.Width := 100;
  constraint := TfpgBorderLayoutConstraint.Create;
  constraint.Region := blrWest;
  lm.AddLayoutComponent(btn, constraint);

  :color fc=darkcyan.// -- Center (main content) --:color fc=default.
  pnl := TfpgPanel.Create(Self);
  pnl.BackgroundColor := clWindowBackground;
  constraint := TfpgBorderLayoutConstraint.Create;
  constraint.Region := blrCenter;
  lm.AddLayoutComponent(pnl, constraint);
:hp2.end:ehp2.;
:exmp.

:h5 id=ch_border_regions.BorderLayout Regions

:table cols='12 40'.
:row.
:c.:hp2.Region:ehp2.
:c.:hp2.Behaviour:ehp2.
:row.
:c.:color fc=darkred.blrNorth:color fc=default.
:c.Spans full width at the top. Height is determined by the widget.
:row.
:c.:color fc=darkred.blrSouth:color fc=default.
:c.Spans full width at the bottom. Height is determined by the widget.
:row.
:c.:color fc=darkred.blrWest:color fc=default.
:c.Left side between North and South. Width is determined by the widget.
:row.
:c.:color fc=darkred.blrEast:color fc=default.
:c.Right side between North and South. Width is determined by the widget.
:row.
:c.:color fc=darkred.blrCenter:color fc=default.
:c.Fills all remaining space after the other four regions are allocated.
:etable.


:h4 id=ch_miglayout.MigLayout &endash. The Grid Layout Manager
:cgraphic.
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
 MigLayout &endash. The Grid Layout Manager
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
:ecgraphic.
:p.
:color fc=darkred.TfpgMigLayoutManager:color fc=default. is the most capable
layout manager, ported from Java's MigLayout v11.4.2. It organises widgets in a
flexible grid with support for spanning, growth, alignment, docking, and
fine-grained sizing control.

:p.
MigLayout uses three levels of constraints&colon.

:ul.
:li.:hp2.LC (Layout Constraints):ehp2. &endash. container-level settings such as
column count, fill mode, and debug visualisation
:li.:hp2.AC (Axis Constraints):ehp2. &endash. per-column and per-row constraints
such as size and growth
:li.:hp2.CC (Component Constraints):ehp2. &endash. per-widget constraints such as
span, alignment, and growth priority
:eul.

:h5 id=ch_mig_basic_example.Basic Grid Example
:p.
This example creates a simple two-column form with labels and edit fields.
:xmp.
:hp2.uses:ehp2.
  fpg_base, fpg_main, fpg_form, fpg_label, fpg_edit, fpg_button,
  fpg_miglayout,    :color fc=darkcyan.// TfpgMigLayoutManager:color fc=default.
  fpg_mig_lc,       :color fc=darkcyan.// Layout Constraints:color fc=default.
  fpg_mig_cc;       :color fc=darkcyan.// Component Constraints:color fc=default.

:hp2.type:ehp2.
  TLoginForm = :hp2.class:ehp2.(TfpgForm)
  :hp2.public:ehp2.
    :hp2.procedure:ehp2. AfterCreate; :hp2.override:ehp2.;
  :hp2.end:ehp2.;

:hp2.procedure:ehp2. TLoginForm.AfterCreate;
:hp2.var:ehp2.
  mig: TfpgMigLayoutManager;
  lbl: TfpgLabel;
  edt: TfpgEdit;
  btn: TfpgButton;
:hp2.begin:ehp2.
  WindowTitle := 'Login';
  Width := 350;
  Height := 150;

  :color fc=darkcyan.// 1. Create and configure the layout manager:color fc=default.
  mig := TfpgMigLayoutManager.Create;
  mig.LC.SetWrapAfter(2);   :color fc=darkcyan.// 2-column grid:color fc=default.

  :color fc=darkcyan.// 2. Assign to form:color fc=default.
  LayoutManager := mig;

  :color fc=darkcyan.// 3. Add widgets with constraints:color fc=default.
  :color fc=darkcyan.// Row 1: Username:color fc=default.
  lbl := TfpgLabel.Create(Self);
  lbl.Text := 'Username:';
  mig.AddLayoutComponent(lbl, TfpgMigCC.Create);

  edt := TfpgEdit.Create(Self);
  edt.PreferredSize := fpgSize(200, 24);
  mig.AddLayoutComponent(edt, TfpgMigCC.Create.GrowX);

  :color fc=darkcyan.// Row 2: Password:color fc=default.
  lbl := TfpgLabel.Create(Self);
  lbl.Text := 'Password:';
  mig.AddLayoutComponent(lbl, TfpgMigCC.Create);

  edt := TfpgEdit.Create(Self);
  edt.PasswordMode := True;
  edt.PreferredSize := fpgSize(200, 24);
  mig.AddLayoutComponent(edt, TfpgMigCC.Create.GrowX);

  :color fc=darkcyan.// Row 3: Buttons spanning both columns:color fc=default.
  btn := TfpgButton.Create(Self);
  btn.Text := 'Login';
  btn.PreferredSize := fpgSize(80, 26);
  mig.AddLayoutComponent(btn,
    TfpgMigCC.Create.SpanX(2).AlignX('right'));
:hp2.end:ehp2.;
:exmp.

:p.
The :color fc=darkred..GrowX:color fc=default. call tells the widget to expand
horizontally to fill the space allocated to its column. The :color
fc=darkred..SpanX(2):color fc=default. call spans both columns, and :color
fc=darkred..AlignX('right'):color fc=default. right-aligns the button within the
spanned area.

:h5 id=ch_mig_cc_ref.Component Constraints (TfpgMigCC) Reference
:p.
The :color fc=darkred.TfpgMigCC:color fc=default. class uses a fluent API.
Methods return the same CC instance, allowing calls to be chained.

:table cols='25 50'.
:row.
:c.:hp2.Method:ehp2.
:c.:hp2.Description:ehp2.
:row.
:c.:color fc=darkred..GrowX / .GrowY:color fc=default.
:c.Allow the widget to expand horizontally / vertically to fill the space within
 its cell. Does not affect how much space the column or row itself receives.
:row.
:c.:color fc=darkred..PushX / .PushY:color fc=default.
:c.Make the column / row that contains this widget grow, taking any extra space
 in the container. Unlike .GrowX, this affects column/row sizing, not just the
 widget within its cell.
:row.
:c.:color fc=darkred..SpanX(n) / .SpanY(n):color fc=default.
:c.Span across n columns / rows. Use .SpanX without arguments to span all
 remaining columns.
:row.
:c.:color fc=darkred..AlignX / .AlignY:color fc=default.
:c.Horizontal or vertical alignment within the cell. Values&colon. 'left',
 'center', 'right' for X; 'top', 'center', 'bottom' for Y.
:row.
:c.:color fc=darkred..Split(n):color fc=default.
:c.Split the current cell into n sub-cells, allowing multiple widgets in one
 grid position.
:row.
:c.:color fc=darkred..Wrap:color fc=default.
:c.Force a line break after this widget.
:row.
:c.:color fc=darkred..NewLine:color fc=default.
:c.Force a line break before this widget.
:row.
:c.:color fc=darkred..Tag(name):color fc=default.
:c.Assign an identifier tag (used for platform-specific button ordering).
:row.
:c.:color fc=darkred..DockNorth / .DockSouth / .DockWest / .DockEast:color fc=default.
:c.Dock this widget to a container edge, outside the normal grid.
:row.
:c.:color fc=darkred..GapLeft / .GapRight / .GapTop / .GapBottom:color fc=default.
:c.Set explicit gaps around this widget (e.g. '10px', '1mm').
:etable.

:h5 id=ch_mig_lc_ref.Layout Constraints (LC)
:p.
Layout constraints configure the container as a whole. They are accessed via the
:color fc=darkred.mig.LC:color fc=default. property.

:table cols='25 50'.
:row.
:c.:hp2.Method:ehp2.
:c.:hp2.Description:ehp2.
:row.
:c.:color fc=darkred..SetWrapAfter(n):color fc=default.
:c.Automatically wrap to the next row after every n components.
:row.
:c.:color fc=darkred..FillX / .FillY:color fc=default.
:c.Make the grid fill the container width / height.
:row.
:c.:color fc=darkred..Fill:color fc=default.
:c.Shorthand for both FillX and FillY.
:row.
:c.:color fc=darkred..SetInsets(top, left, bottom, right):color fc=default.
:c.Set container padding in pixels.
:row.
:c.:color fc=darkred..FlowY:color fc=default.
:c.Change the primary flow direction to vertical (top-to-bottom) instead of
 horizontal.
:row.
:c.:color fc=darkred..Debug:color fc=default.
:c.Enable debug overlay that highlights grid cells and gaps.
:etable.

:h5 id=ch_mig_growth.Growth and Fill
:p.
MigLayout distinguishes between two levels of growth, controlled by different
CC methods&colon.

:p.
:hp2..GrowX / .GrowY &endash. widget fills its cell:ehp2.
:p.
The widget expands to consume the space allocated to its column or row. This
does not change how large the column or row itself becomes relative to others.

:p.
:hp2..PushX / .PushY &endash. column or row grows:ehp2.
:p.
The column (or row) that contains this widget takes any extra container space.
This operates at the grid level, not the widget level. Combine with :color
fc=darkred..GrowX:color fc=default. if you also want the widget to fill the
grown column.

:xmp.
:color fc=darkcyan.// Widget fills its cell horizontally (cell size unchanged):color fc=default.
mig.AddLayoutComponent(edt, TfpgMigCC.Create.GrowX);

:color fc=darkcyan.// Widget grows in both directions:color fc=default.
mig.AddLayoutComponent(memo, TfpgMigCC.Create.GrowX.GrowY);

:color fc=darkcyan.// Column 2 takes all extra horizontal space; widget fills that column:color fc=default.
mig.AddLayoutComponent(edt1, TfpgMigCC.Create);
mig.AddLayoutComponent(edt2, TfpgMigCC.Create.PushX.GrowX);

:color fc=darkcyan.// Two columns sharing growth equally (both have equal push weight):color fc=default.
mig.AddLayoutComponent(edt1, TfpgMigCC.Create.PushX.GrowX);
mig.AddLayoutComponent(edt2, TfpgMigCC.Create.PushX.GrowX);
:exmp.

:nt.For :color fc=darkred..GrowX:color fc=default. to have a visible effect, the
column must have space to grow into. Use :color fc=darkred.mig.LC.FillX:color
fc=default. (or :color fc=darkred..Fill:color fc=default.) so that the grid
expands to fill the container, then :color fc=darkred..PushX:color fc=default.
on the widget whose column should receive that extra space. Without :color
fc=darkred..PushX:color fc=default. (or without :color fc=darkred.FillX:color
fc=default.), all columns share any extra space equally.:ent.

:h5 id=ch_mig_push_why.Why is PushX/PushY on CC rather than AC?
:p.
At first glance it may seem odd that :color fc=darkred..PushX:color fc=default.
and :color fc=darkred..PushY:color fc=default. live on the component constraint
(CC) when they act on the column or row, not the widget itself. The reason is
that MigLayout is a :hp1.flow-based:ehp1. layout manager&colon. components are
placed in order and rows or columns are created dynamically. When you add a
component, you often do not know &endash. and should not need to know &endash.
which row index it will ultimately occupy.

:p.
If push were only available on AC, you would have to write something like&colon.
:xmp.
:color fc=darkcyan.// Fragile: must know that the treeview lands in row index 1:color fc=default.
mig.RowConstraints := TfpgMigAC.Create.Index(1).Push;
mig.AddLayoutComponent(tvContents, TfpgMigCC.Create.GrowY);
:exmp.

:p.
This couples the AC configuration to the component's position. Add a component
earlier, change :color fc=darkred.WrapAfter:color fc=default., or rearrange
components, and the index silently points at the wrong row. By placing :color
fc=darkred..PushY:color fc=default. on the CC instead, you express the intent
alongside the component itself&colon.
:xmp.
:color fc=darkcyan.// Robust: row grows wherever this component ends up:color fc=default.
mig.AddLayoutComponent(tvContents, TfpgMigCC.Create.GrowY.PushY);
:exmp.

:p.
Think of :color fc=darkred..PushX:color fc=default. / :color
fc=darkred..PushY:color fc=default. on CC as a shorthand that means&colon.
:hp1."whichever column or row this component lands in, let that column or row
absorb any extra space.":ehp1.

:p.
Use AC when you want to set constraints for rows or columns :hp2.by explicit
index:ehp2., for example to fix a column to a specific minimum width regardless
of what component occupies it. Use CC push when you want a specific component to
drive growth, without caring about indices.

:h5 id=ch_mig_one_growing.Common Pattern&colon. One Growing Row
:p.
A very common requirement is a layout where the first row contains a toolbar or
button bar at its natural height, and the remaining rows contain a scrollable
widget that should fill all available space&colon.
:xmp.
:color fc=darkcyan.// Single-column layout; second row fills all remaining vertical space:color fc=default.
mig.LC.WrapAfter(1).Fill;

mig.AddLayoutComponent(btnGo, TfpgMigCC.Create.AlignX('right'));
mig.AddLayoutComponent(tvContents, TfpgMigCC.Create.GrowX.GrowY.PushY);
:exmp.

:ul.
:li.:color fc=darkred..PushY:color fc=default. makes the treeview's row absorb
 all extra vertical space.
:li.:color fc=darkred..GrowY:color fc=default. makes the treeview widget fill
 that grown row.
:li.Without :color fc=darkred..PushY:color fc=default., both rows would share
 the extra space equally.
:eul.

:h5 id=ch_mig_spanning.Column Spanning
:p.
Spanning allows a widget to occupy multiple grid cells.
:xmp.
:color fc=darkcyan.// A heading label spanning 3 columns:color fc=default.
mig.AddLayoutComponent(lblHeading,
  TfpgMigCC.Create.SpanX(3).AlignX('center'));

:color fc=darkcyan.// A text area spanning 2 columns and 3 rows:color fc=default.
mig.AddLayoutComponent(memo,
  TfpgMigCC.Create.SpanX(2).SpanY(3).GrowX.GrowY);
:exmp.

:h5 id=ch_mig_docking.Docking
:p.
MigLayout supports docking widgets to container edges, outside the normal grid.
Docked widgets are laid out first, and the grid occupies the remaining space.
:xmp.
:color fc=darkcyan.// Dock a toolbar to the top:color fc=default.
mig.AddLayoutComponent(toolbar, TfpgMigCC.Create.DockNorth);

:color fc=darkcyan.// Dock a status bar to the bottom:color fc=default.
mig.AddLayoutComponent(statusbar, TfpgMigCC.Create.DockSouth);

:color fc=darkcyan.// Dock a tree view to the left:color fc=default.
mig.AddLayoutComponent(tree, TfpgMigCC.Create.DockWest);

:color fc=darkcyan.// The remaining widgets form the grid in the center area:color fc=default.
mig.AddLayoutComponent(content, TfpgMigCC.Create.GrowX.GrowY);
:exmp.


:h4 id=ch_widget_sizing.Widget Sizing
:cgraphic.
þþþþþþþþþþþþþþþ
 Widget Sizing
þþþþþþþþþþþþþþþ
:ecgraphic.
:p.
Widgets in &fpg. have both a :hp1.preferred size:ehp1. (the size the developer
intends) and an :hp1.actual size:ehp1. (the size after the layout manager and
window system have made their calculations). Layout managers read a widget's
preferred size to determine how much space it needs.

:p.
There are two ways to set preferred size&colon.
:xmp.
:color fc=darkcyan.// Option 1: Set Width and Height (these define the preferred size):color fc=default.
btn.Width := 100;
btn.Height := 30;

:color fc=darkcyan.// Option 2: Use PreferredSize directly:color fc=default.
btn.PreferredSize := fpgSize(100, 30);
:exmp.

:p.
Some widgets (such as :color fc=darkred.TfpgLabel:color fc=default. and :color
fc=darkred.TfpgButton:color fc=default.) calculate their preferred size
automatically based on their content and font. For these widgets, it is usually
unnecessary to set an explicit size.


:h4 id=ch_mixing_layouts.Mixing Layout Managers
:cgraphic.
þþþþþþþþþþþþþþþþþþþþþþþþ
 Mixing Layout Managers
þþþþþþþþþþþþþþþþþþþþþþþþ
:ecgraphic.
:p.
Layout managers can be nested. A form may use BorderLayout for its overall
structure, while individual panels use MigLayout or FlowLayout for their
content.
:xmp.
:hp2.procedure:ehp2. TMainForm.AfterCreate;
:hp2.var:ehp2.
  border: TfpgBorderLayoutManager;
  mig: TfpgMigLayoutManager;
  pnlContent: TfpgPanel;
  pnlToolbar: TfpgPanel;
  constraint: TfpgBorderLayoutConstraint;
:hp2.begin:ehp2.
  :color fc=darkcyan.// Form uses BorderLayout:color fc=default.
  border := TfpgBorderLayoutManager.Create(0, 4);
  LayoutManager := border;

  :color fc=darkcyan.// Toolbar panel docked to North:color fc=default.
  pnlToolbar := TfpgPanel.Create(Self);
  pnlToolbar.Height := 36;
  constraint := TfpgBorderLayoutConstraint.Create;
  constraint.Region := blrNorth;
  border.AddLayoutComponent(pnlToolbar, constraint);

  :color fc=darkcyan.// Content panel fills Center:color fc=default.
  pnlContent := TfpgPanel.Create(Self);
  constraint := TfpgBorderLayoutConstraint.Create;
  constraint.Region := blrCenter;
  border.AddLayoutComponent(pnlContent, constraint);

  :color fc=darkcyan.// Content panel uses MigLayout internally:color fc=default.
  mig := TfpgMigLayoutManager.Create;
  mig.LC.SetWrapAfter(2);
  pnlContent.LayoutManager := mig;

  :color fc=darkcyan.// Add widgets to the content panel via MigLayout:color fc=default.
  :color fc=darkcyan.// ...:color fc=default.
:hp2.end:ehp2.;
:exmp.


:h4 id=ch_running_lm_examples.Running the Examples
:cgraphic.
þþþþþþþþþþþþþþþþþþþþþþ
 Running the Examples
þþþþþþþþþþþþþþþþþþþþþþ
:ecgraphic.
:p.
&fpg. ships with working examples for each layout manager&colon.

:ul.
:li.:font facename='System Monospaced' size=0x0.examples/gui/lm-flow/:font
facename=default size=0x0. &endash. FlowLayout with basic and advanced demos
:li.:font facename='System Monospaced' size=0x0.examples/gui/lm-border/:font
facename=default size=0x0. &endash. BorderLayout five-region demo
:li.:font facename='System Monospaced' size=0x0.examples/gui/lm-mig/:font
facename=default size=0x0. &endash. MigLayout demo with eight feature panels
covering basic grids, alignment, spanning, growth, docking, constraint parsing,
and button ordering
:eul.

:p.
Use PasBuild from the project root, to make sure the &fpg. framework has been
compiled&colon.
:xmp.
 pasbuild compile -p unix,agg,debug
:exmp.

:p.
Then build and run an example&colon.
:xmp.
 cd examples/gui/lm-mig
 fpc @extrafpc.cfg lm-mig.lpr
 ./lm-mig
:exmp.


:h4 id=ch_debug_vis.Debug Visualisation
:cgraphic.
þþþþþþþþþþþþþþþþþþþþþ
 Debug Visualisation
þþþþþþþþþþþþþþþþþþþþþ
:ecgraphic.
:p.
MigLayout includes a built-in debug mode that draws grid cell boundaries, gaps,
and component outlines as coloured overlays. This is invaluable for
understanding how the layout engine distributes space.

:p.
Enable it by calling :color fc=darkred.Debug():color fc=default. on the LC&colon.
:xmp.
mig.LC.Debug;
:exmp.

:p.
The debug overlay uses&colon.

:ul.
:li.:hp2.Blue outlines:ehp2. for grid cell boundaries
:li.:hp2.Red outlines:ehp2. for component bounds
:li.:hp2.Green dashes:ehp2. for gaps between cells
:eul.


:h4 id=ch_lm_further_reading.Further Reading
:cgraphic.
þþþþþþþþþþþþþþþþþ
 Further Reading
þþþþþþþþþþþþþþþþþ
:ecgraphic.

:ul.
:li.:font facename='System Monospaced' size=0x0.<fpgui>/docs/miglayout_v11_port_plan.adoc:font
facename=default size=0x0. &endash. Detailed MigLayout v11 port implementation plan
:li.:font facename='System Monospaced' size=0x0.<fpgui>/docs/layout_manager_implementation_plan.adoc:font
facename=default size=0x0. &endash. Original architecture design document
:li.:font facename='System Monospaced' size=0x0.<fpgui>/docs/toolkit_sizing_comparison.adoc:font
facename=default size=0x0. &endash. Widget sizing API design (intent vs actual size)
:li.Java MigLayout documentation at https://miglayout.com applies directly to the
&fpg. port. For reference, the original Java code can be found here: https://github.com/mikaelgrev/miglayout
:eul.
