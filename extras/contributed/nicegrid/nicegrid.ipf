.* This file is encoded using IBM850 encoding
.* :encoding=IBM850:
:userdoc.
:docprof toc=123456.
:title.NiceGrid Component

:h1.NiceGrid
:p.
NiceGrid is a free Delphi and fpGUI component that aimed to be a
standard string grid replacement. It is written from scratch, not descended
from TStringGrid.

:ul compact.
:li.:link reftype=hd refid=001.Introduction:elink.
:li.:link reftype=hd refid=002.Working with Headers:elink.
:eul.

:h2 id=001.Introduction
:p.
Originally created by Priyatna [http://www.priyatna.org/], for use with Delphi,
and with permission, ported by Jean Pierre Anghel <jean-pierre.anghel@orange.fr> to fpGUI Toolkit.
:p.
NiceGrid is a free Delphi (and now fpGUI) component that aimed to be a
standard string grid replacement. It is written from scratch, not descended
from TStringGrid. The main reason why the component was written was to have a
grid component that is nice and smooth. Here's some feature of NiceGrid:

:ul compact.
:li.Headers can be merged and or multilined.
:li.Smooth scrolling, not aligned to top left cell coordinate.
:li.All aspect of grid colors can be customized: header light color, header
dark color, header color, grid color, text color, etc.; resulting a real nice
looking grid.
:li.Alternate row color.
:li.Can be customized at design time.
:li.Each column can have its own horizontal and vertical alignment, color, and font.
:li.Each column can be hidden.
:li.Can be auto fit to width.
:li.Can be auto calculate column width.
:li.BeginUpdate and EndUpdate method for bulk cells access.
:eul.

:p.
There are several main differences between NiceGrid and TStringGrid:

:ul compact.
:li.Headers are excluded from cells, unlike TStringGrid that treats fixed rows
as regular cells (Row 0, for example), Cells[0,0] in NiceGrid will access the
top left editable cells, not fixed cell.
:li.The only way to access the data is using Cells property or using direct
array referencing style: NiceGrid1[0,0]. There are not (yet) Cols, or Rows
property.
:li.FixedRows -> Header, FixedCols -> Gutter.
:eul.

:h2 id=002.Working with Headers
:p.
All features explained below are available at design time, but I will cover it
using codes, for easy following.

:p.
NiceGrid will automatically scan column title and make appropriate merging and
multilining. The only thing you must do is set each column's :hp2.Title:ehp2.
property, which is a :hp1.String:ehp1. type property. To make a multilined
caption, use the ';' (semicolon) character.

:p.
For example:
:xmp.
  NiceGrid1.Columns[0].Title := 'First Line;Second Line';
:exmp.

:p.
will make
:cgraphic.
  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³             First Line             ³ 
  ³            Second Line             ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
:ecgraphic.

:p.
The :hp2.HeaderLine:ehp2. property determines how many lines will be allocated
in the header. Each line in the header can also be set via the column's :hp2.Title:ehp2.
property, separated by '|' character.

:p.
For example:
:xmp.
  NiceGrid1.HeaderLine := 2;
  NiceGrid1.Columns[0].Title := 'First Line|Second Line';
:exmp.

:p.
will make
:cgraphic.
  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³             First Line             ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³            Second Line             ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
:ecgraphic.

:p.
To merge two header cells, set each cell to exactly the same value, including ';'
characters if they are multilined.

:p.
For example:
:xmp.
  NiceGrid1.HeaderLine := 2;
  NiceGrid1.ColCount := 2;
  NiceGrid1.Columns[0].Title := 'One|Two';
  NiceGrid1.Columns[1].Title := 'One|Three';
:exmp.

:p.
will make
:cgraphic.
  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³                One                 ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³      Two        ³       Three      ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
:ecgraphic.

:p.
Using combination of '|' and ';' characters, we can make a complex header.

:p.
For example:
:xmp.
  NiceGrid1.HeaderLine := 2;
  NiceGrid1.ColCount := 5;
  NiceGrid1.Columns[0].Title := 'Merged;Multilined|Merged;Multilined';
  NiceGrid1.Columns[1].Title := 'First Group|One';
  NiceGrid1.Columns[2].Title := 'First Group|Two';
  NiceGrid1.Columns[3].Title := 'Second Group|Three';
  NiceGrid1.Columns[4].Title := 'Second Group|Four';
:exmp.

:p.
will make
:cgraphic.
  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³      Merged     ³      First Group         ³      Second Group      ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ´
  ³     Multilined  ³   One      ³    Two      ³   Three    ³   Four    ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ
:ecgraphic.

:euserdoc.
