:userdoc.
:title.Free Pascal&colon. Language Reference guide
:docprof toc=123456.
.* ==============================================================
.* Custom symbols
.nameit symbol='fpc' text='Free Pascal'
.nameit symbol='delphi' text='Delphi'
.nameit symbol='tp' text='Turbo Pascal'
.nameit symbol='fpcversion' text='2&per.4'
.nameit symbol='date' text='December 2009'
.nameit symbol='progref' text='Programmer&apos.s Guide [ http://www.freepascal.org/docs.var ]'
.nameit symbol='ra' text='►'
.nameit symbol='la' text='◄'
.nameit symbol='dar' text='▼'
.nameit symbol='uar' text='^'
.nameit symbol='linux' text='Linux'
.* ==============================================================
:h1.Free Pascal&colon. Language Reference guide
:p.
:hp2.Language Reference guide for Free Pascal version &fpcversion. :ehp2.

:p.
Document version &fpcversion. (r617)
.br
&date.

:p.
:p.
:p.
:p.
Written by :hp1.Michael van Canneyt:ehp1.
.br
LaTeX to IPF conversion by :hp1.Graeme Geldenhuys:ehp1.
:p.
:p.
:p.
:note.:color fc=red.Please switch DocView to using the UTF-8 text encoding for this document.:color fc=default.

.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % About this guide
.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:h2.About this guide
:p.
This document serves as the reference for the Pascal langauge as implemented
by the &fpc. compiler. It describes all Pascal constructs supported by 
&fpc., and lists all supported data types. It does not, however, give a 
detailed explanation of the Pascal language: it is not a tutorial. 
The aim is to list which Pascal constructs are supported, and to show 
where the &fpc. implementation differs from the &tp. or &delphi.
implementations.

:p.
The &tp. and &delphi. Pascal compilers introduced various features in the 
Pascal language. The Free Pascal compiler emulates these compilers in the
appropriate mode of the compiler: certain features are available only
if the compiler is switched to the appropriate mode. When required for 
a certain feature, the use of the :hp1.-M:ehp1. command-line switch or 
:hp1.{$MODE}:ehp1. directive will be indicated in the text. More information
about the various modes can be found in the user's manual and the
programmer's manual.

:p.
Earlier versions of this document also contained the reference documentation
of the :hp1.system:ehp1. unit and :hp1.objpas:ehp1. unit. This has been moved to the 
RTL reference guide.

.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Notations
:h3.Notations
:p.
Throughout this document, we will refer to functions, types and variables 
with :font facename=Courier size=16x16.typewriter:font facename='System Proportional'. font.
Files are referred to with a sans font: :font facename=Sans size=16x16.filename:font facename='System Proportional'..


.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Syntax diagrams
:h3.Syntax diagrams
:p.
All elements of the Pascal language are explained in syntax diagrams. Syntax diagrams are like flow
charts. Reading a syntax diagram means getting from the left side to the right side, following the
arrows. When the right side of a syntax diagram is reached, and it ends with a single arrow, this
means the syntax diagram is continued on the next line. If the line ends on 2 arrows pointing to each
other, then the diagram is ended.
:p.
Syntactical elements are written like this:
:cgraphic.
&ra.&ra.─── syntactical elemements are like this ───────────────────────────────────&ra.&la.
:ecgraphic.

:p.
Keywords which must be typed exactly as in the diagram:
:cgraphic.
&ra.&ra.─── :hp2.keywords are like this:ehp2. ─────────────────────────────────────────────────&ra.&la.
:ecgraphic.

:p.
When something can be repeated, there is an arrow around it:
:cgraphic.
&ra.&ra.─────┬─ this can be repeated ─┬─────────────────────────────────────────────&ra.&la.
       ^────────────────────────┘
:ecgraphic.

:p.
When there are different possibilities, they are listed in rows:
:cgraphic.
&ra.&ra.─────┬─ First possibility ──┬───────────────────────────────────────────────&ra.&la.
       └─ Second possibility ─┘
:ecgraphic.

:p.
Note, that one of the possibilities can be empty:
:cgraphic.
&ra.&ra.─────┬──────────────────────┬───────────────────────────────────────────────&ra.&la.
       ├─ First possibility  ─┤
       └─ Second possibility ─┘
:ecgraphic.

:p.
This means that both the first or second possibility are optional. Of course, all these elements can be
combined and nested.


.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % About the Pascal Language
.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:h2.About the Pascal Language
:p.
The language Pascal was originally designed by Niklaus Wirth around 1970. It 
has evolved significantly since that day, with a lot of contributions by the
various compiler constructors (Notably: Borland). The basic elements have been
kept throughout the years:
:ul.
:li. Easy syntax, rather verbose, yet easy to read. Ideal for teaching.
:li. Strongly typed.
:li. Procedural.
:li. Case insensitive.
:li. Allows nested procedures.
:li. Easy input/output routines built-in.
:eul.

:p.
The &tp. and &delphi. Pascal compilers introduced various features in
the Pascal language, most notably easier string handling and object 
orientedness. The &fpc. compiler initially emulated most of &tp.
and later on &delphi.. It emulates these compilers in the appropriate mode
of the compiler: certain features are available only if the compiler is 
switched to the appropriate mode. When required for a certain feature, the use
of the -M command-line switch or {$MODE } directive will be indicated in the
text. More information about the various modes can be found in the User's 
Manual and the Programmer's Manual.


.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Pascal Tokens
.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:h2.Pascal Tokens
:p.
Tokens are the basic lexical building blocks of source code: they 
are the 'words' of the language: characters are combined into tokens according
to the rules of the programming language. There are five classes of tokens:

:parml tsize=20 break=none.
:pt.:hp2.reserved words:ehp2.
:pd. These are words which have a fixed meaning in the language. They cannot 
be changed or redefined.

:pt.:hp2.identifiers:ehp2.
:pd.These are names of symbols that the programmer defines. They can be changed
and re-used. They are subject to the scope rules of the language.

:pt.:hp2.operators:ehp2.
:pd.These are usually symbols for mathematical or other operations: +, -, * and
so on.

:pt.:hp2.separators:ehp2.
:pd.This is usually white-space.

:pt.:hp2.constants:ehp2.
:pd.Numerical or character constants are used to denote actual values in the
source code, such as 1 (integer constant) or 2.3 (float constant) or 
'String constant' (a string: a piece of text).
:eparml.

:p.
In this chapter we describe all the Pascal reserved words&comma. as well as the
various ways to denote strings&comma. numbers&comma. identifiers etc&per.


.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Symbols
:h3.Symbols
:p.
&fpc. allows all characters, digits and some special character
symbols in a Pascal source file.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Recognised symbols:ehp2.

&ra.&ra.─── letter ──┬─ A..Z ─┬─────────────────────────────────────────────────────&ra.&la.
               └─ a..z ─┘

&ra.&ra.─── digit ─ 0..9 ───────────────────────────────────────────────────────────&ra.&la.

&ra.&ra.─── hex digit ──┬─ 0..9 ─┬──────────────────────────────────────────────────&ra.&la.
                  ├─ A..Z ─┤
                  └─ a..z ─┘
└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
The following characters have a special meaning:
:xmp.
+ - * / = < > [ ] . , ( ) : ^ @ { } $ #
:exmp.

:p.
and the following character pairs too:
:xmp.
<= >= := += -= *= /= (* *) (. .) //
:exmp.

:p.
When used in a range specifier, the character pair (. is equivalent to the 
left square bracket [. Likewise, the character pair .) is equivalent to the 
right square bracket ]. When used for comment delimiters, the character pair
(* is equivalent to the left brace { and the character pair *) is equivalent
to the right brace }. These character pairs retain their normal meaning in 
string expressions.

.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Comments
:h3.Comments
:p.
Comments are pieces of the source code which are completely discarded by the
compiler. They exist only for the benefit of the programmer, so he can explain
certain pieces of code. For the compiler, it is as if the comments were not 
present.

:p.
The following piece of code demonstrates a comment:
:xmp.
(* My beautiful function returns an interesting result *)
Function Beautiful: Integer;
:exmp.

:p.
The use of (* and *) as comment delimiters dates from the very first days of
the Pascal language. It has been replaced mostly by the use of { and } as 
comment delimiters, as in the following example:
:xmp.
{ My beautiful function returns an interesting result }
Function Beautiful: Integer;
:exmp.

:p.
The comment can also span multiple lines:
:xmp.
{
   My beautiful function returns an interesting result,
   but only if the argument A is less than B.
}
Function Beautiful(A, B: Integer): Integer;
:exmp.

:p.
Single line comments can also be made with the // delimiter:
:xmp.
// My beautiful function returns an interesting result
Function Beautiful: Integer;
:exmp.

:p.
The comment extends from the // character till the end of the line. This kind
of comment was introduced by Borland in the &delphi. Pascal compiler.
:p.
&fpc. supports the use of nested comments. The following constructs are
valid comments:
:xmp.
(* This is an old style comment *)
{ This is a Turbo Pascal comment }
// This is a &delphi. comment. All is ignored till the end of the line.
:exmp.

:p.
The following are valid ways of nesting comments:
:xmp.
{ Comment 1 (* comment 2 *) }
(* Comment 1 { comment 2 } *)
{ comment 1 // Comment 2 }
(* comment 1 // Comment 2 *)
// comment 1 (* comment 2 *)
// comment 1 { comment 2 }
:exmp.

:p.
The last two comments must be on one line. The following two will give errors:
:xmp.
// Valid comment { No longer valid comment !!
   }
:exmp.

:p.
and
:xmp.
// Valid comment (* No longer valid comment !!
   *)
:exmp.

:p.
The compiler will react with a 'invalid character' error when it encounters
such constructs, regardless of the :hp1.-Mturbo:ehp1. switch.

:nt.
In :hp1.TP:ehp1. and :hp1.Delphi:ehp1. mode, nested comments are not allowed, 
for maximum compatibility with existing code for those compilers.
:ent.



.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Reserved words
:h3.Reserved words
:p.
Reserved words are part of the Pascal language, and as such, cannot be
redefined by the programmer. Throughout the syntax diagrams they will be 
denoted using a :hp2.bold:ehp2. typeface. Pascal is not case sensitive so the compiler
will accept any combination of upper or lower case letters for reserved words.

:p.
We make a distinction between &tp. and &delphi. reserved words. In 
:hp2.TP:ehp2. mode, only the &tp. reserved words are recognised, but
the &delphi. ones can be redefined. By default, &fpc. recognises the &delphi.
reserved words.

:h4.&tp. reserved words
:p.
The following keywords exist in &tp. mode:
.*              *                *                *
:xmp.
absolute        file             object           shr
and             for              of               string
array           function         on               then
asm             goto             operator         to
begin           if               or               type
case            implementation   packed           unit
const           in               procedure        until
constructor     inherited        program          uses
destructor      inline           record           var
div             interface        reintroduce      while
do              label            repeat           with
downto          mod              self             xor
else            nil              set
end             not              shl
:exmp.

:h4.&fpc. reserved words
:p.
On top of the &tp. reserved words, &fpc. also considers the 
following as reserved words:
.*              *                *                *
:xmp.
dispose         false            true
exit            new
:exmp.


:h4.Object Pascal reserved words
:p.
The reserved words of Object Pascal (used in :hp2.Delphi:ehp2. or :hp2.ObjPas:ehp2. mode) are the
same as the &tp. ones, with the following additional keywords:
.*              *                *                *
:xmp.
as              finalization     library          raise
class           finally          on               resourcestring
dispinterface   initialization   out              threadvar
except          inline           packed           try
exports         is               property

:exmp.


:h4.Modifiers
:p.
The following is a list of all modifiers. They are not exactly reserved words
in the sense that they can be used as identifiers, but in specific places, they
have a special meaning for the compiler, i.e., the compiler considers them as
part of the Pascal language.
.*              *                *                *
:xmp.
absolute        external         nostackframe     read
abstract        far              oldfpccall       register
alias           far16            override         reintroduce
assembler       forward          pascal           safecall
cdecl           index            private          softfloat
cppdecl         local            protected        stdcall
default         name             public           virtual
export          near             published        write
:exmp.

:p.
:nt.
Predefined types such as :hp1.Byte:ehp1., :hp1.Boolean:ehp1. and constants such
as :hp1.maxint:ehp1. are not reserved words. They are identifiers, declared in the system unit. This means
that these types can be redefined in other units. The programmer is however
:hp2.not:ehp2. encouraged to do this, as it will cause a lot of confusion.
:ent.

:h3.Identifiers
:p.
Identifiers denote programmer defined names for specific constants, types,
variables, procedures and functions, units, and programs. All programmer
defined names in the source code – excluding reserved words – are designated
as identifiers.

:p.
Identifiers consist of between 1 and 127 significant characters (letters,
digits and the underscore character), of which the first must be an alphanumeric
character, or an underscore (_). The following diagram gives the basic syntax
for identifiers.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Identifiers:ehp2.

&ra.&ra.─── identifier ──┬─ letter ─┬─┬──────────────┬──────────────────────────────&ra.&la.
                   └─── _ ────┘ ^─┬─ letter ─┬─┘
                                  ├─ digit ──┤
                                  └─── _ ────┘
└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
Like Pascal reserved words, identifiers are case insensitive, that is, both
:xmp.
myprocedure;
:exmp.

:p.
and
:xmp.
MyProcedure;
:exmp.

:p.
refer to the same procedure.

:nt.
As of version 2.5.1 it is possible to specify a reserved word as an identifier
by prepending it with an ampersand (&amp.). This means that the following is
possible:

:xmp.
program testdo;

procedure &amp.do;
begin
end;

begin
  &amp.do;
end.
:exmp.

:p.
The reserved word :hp1.do:ehp1. is used as an identifier for the declaration
as well as the invocation of the procedure 'do'.
:ent.


.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Hint directives
:h3.Hint directives
:p.
Most identifiers (constants, variables, functions or methods, properties) can
have a hint directive appended to their definition:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Hint directives:ehp2.

&ra.&ra.─── hint directive ──┬─────────────────┬────────────────────────────────────&ra.&la.
                       ├── Deprecated ───┤
                       ├─ Experimental ──┤
                       ├─── Platform ────┤
                       └─ Unimplemented ─┘
└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
Whenever an identifier marked with a hint directive is later encountered by the
compiler, then a warning will be displayed, corresponding to the specified hint.

:parml tsize=20 break=none.

:pt.:hp2.deprecated:ehp2.
:pd.The use of this identifier is deprecated, use an alternative instead.

:pt.:hp2.experimental:ehp2.
:pd.The use of this identifier is experimental: this can be used to flag new
features that should be used with caution.

:pt.:hp2.platform:ehp2.
:pd.This is a platform-dependent identifier: it may not be defined on all
platforms.

:pt.:hp2.unimplemented:ehp2.
:pd.This should be used on functions and procedures only. It should be used to
signal that a particular feature has not yet been implemented.

:eparml.

:p.
The following are examples:

:xmp.
const
  AConst = 12 deprecated;

var
  p: integer platform;

function Something: Integer; experimental;
begin
  Something := P + AConst;
end;

begin
  Something;
end.
:exmp.

:p.
This would result in the following output:

:xmp.
testhd.pp(11,15) Warning: Symbol "p" is not portable
testhd.pp(11,22) Warning: Symbol "AConst" is deprecated
testhd.pp(15,3) Warning: Symbol "Something" is experimental
:exmp.

:p.
Hint directives can follow all kinds of identifiers: units, constants, types,
variables, functions, procedures and methods.



.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Numbers
:h3.Numbers
:p.
Numbers are by default denoted in decimal notation.
Real (or decimal) numbers are written using engineering or scientific
notation (e.g. 0.314E1).

:p.
For integer type constants, &fpc. supports 4 formats:

:ol.
:li. Normal, decimal format (base 10). This is the standard format.

:li. Hexadecimal format (base 16), in the same way as &tp. does.
To specify a constant value in hexadecimal format, prepend it with a dollar
sign ($). Thus, the hexadecimal $FF equals 255 decimal.
Note that case is insignificant when using hexadecimal constants.

:li. As of version 1.0.7, Octal format (base 8) is also supported.
To specify a constant in octal format, prepend it with a ampersand (&amp.).
For instance 15 is specified in octal notation as &amp.17. 

:li. Binary notation (base 2). A binary number can be specified
by preceding it with a percent sign (%). Thus, 255 can be
specified in binary notation as %11111111.
:eol.

:p.
The following diagrams show the syntax for numbers.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Numbers:ehp2.

&ra.&ra.─── hex digit sequence ──┬── hex digit ──┬──────────────────────────────────&ra.&la.
                           &uar.───────────────┘

&ra.&ra.─── octal digit sequence ──┬── octal digit ──┬──────────────────────────────&ra.&la.
                             &uar.─────────────────┘

&ra.&ra.─── binary digit sequence ──┬┬─ 1 ─┬┬───────────────────────────────────────&ra.&la.
                              │└─ 0 ─┘│
                              &uar.───────┘

&ra.&ra.─── digit sequence ──┬── digit ──┬──────────────────────────────────────────&ra.&la.
                       ^───────────┘

&ra.&ra.─── unsigned integer ──┬────── digit sequence ───────┬──────────────────────&ra.&la.
                         ├─ $ ─ hex digit sequence ────┤
                         ├─ & ─ octal digit sequence ──┤
                         └─ % ─ binary digit sequence ─┘

&ra.&ra.─── hex digit sequence ──┬── hex digit ──┬──────────────────────────────────&ra.&la.
                           ^───────────────┘

&ra.&ra.─── sign ──┬── + ──┬────────────────────────────────────────────────────────&ra.&la.
             └── - ──┘

&ra.&ra.─── unsigned real ─ digit sequence ─┬────────────────────┬┬──────────────┬──&ra.&la.
                                      └ . ─ digit sequence ┘└ scale factor ┘

&ra.&ra.─── scale factor ─┬─ E ─┬┬──────┬─ digit sequence ──────────────────────────&ra.&la.
                    └─ e ─┘└ sign ┘

&ra.&ra.─── unsigned number ──┬─── unsigned real ──┬────────────────────────────────&ra.&la.
                        └─ unsigned integer ─┘

&ra.&ra.─── signed number ──┬────────┬─ unsigned number ────────────────────────────&ra.&la.
                      └─ sign ─┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:nt.
Octal and Binary notation are not supported in TP or Delphi compatibility mode.
:ent.



.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Labels
:h3.Labels
:p.
A label is a name for a location in the source code to which can be 
jumped to from another location with a :hp2.goto:ehp2. statement. A Label is a
standard identifier with the exception that it can start with a digit.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Label:ehp2.

&ra.&ra.─── label ──┬─ digit sequence ─┬────────────────────────────────────────────&ra.&la.
              └── identifier ────┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:nt.
The -Sg or -Mtp switches must be specified before labels can be used. By
default, &fpc. doesn't support :hp2.label:ehp2. and :hp2.goto:ehp2. statements.
The :hp2.{$GOTO ON}:ehp2. directive can also be used to allow use of labels and
the goto statement.
:ent.


.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Character strings
:h3.Character strings
:p.
A character string (or string for short) is a sequence of zero or more
characters (byte sized), enclosed in single quotes, and on a single 
line of the program source code: no literal carriage return or linefeed 
characters can appear in the string.

:p.
A character set with nothing between the quotes ('') is an empty string.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Character strings:ehp2.

&ra.&ra.─── character string ──┬┬─ quoted string ──┬┬───────────────────────────────&ra.&la.
                         │└─ control string ─┘│
                         ^────────────────────┘

&ra.&ra.─── quoted string ─ ' ──┬─ string character ─┬─ ' ──────────────────────────&ra.&la.
                          ^────────────────────┘

&ra.&ra.─── string character ──┬─ Any character except ' or CR ─┬───────────────────&ra.&la.
                         └────────────── " ───────────────┘ 

&ra.&ra.─── control string ──┬─ # ─ unsigned integer ─┬─────────────────────────────&ra.&la.
                       ^────────────────────────┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
The string consists of standard, 8-bit ASCII characters or Unicode (normally
UTF-8 encoded) characters. The :hp1.control string:ehp1. can be used to specify
characters which cannot be typed on a keyboard, such as :hp1.#27:ehp1. for
the escape character. 

:p.
The single quote character can be embedded in the string by typing it twice. 
The C construct of escaping characters in the string (using a backslash) 
is not supported in Pascal.

:p.
The following are valid string constants:
:xmp.
  'This is a pascal string'
  ''
  'a'
  'A tabulator character: '#9' is easy to embed'
:exmp.

:p.
The following is an invalid string:

:xmp.
  'the string starts here
   and continues here'
:exmp.

:p.
The above string must be typed as:

:xmp.
  'the string starts here'#13#10'   and continues here'
:exmp.

or
:xmp.
  'the string starts here'#10'   and continues here'
:exmp.

:p.
on unices (including Mac OS X), and as

:xmp.
  'the string starts here'#13'   and continues here'
:exmp.

on a classic Mac-like operating system.
 
It is possible to use other character sets in strings: in that case the 
codepage of the source file must be specified with the :hp1.{$CODEPAGE XXX}:ehp1.
directive or with the :hp2.-Fc:ehp2. command line option for the compiler. In that
case the characters in a string will be interpreted as characters from the
specified codepage.




.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Constants
.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:h2.Constants
:p.
Just as in &tp., &fpc. supports both ordinary and typed constants.

:ul.
:li.:link reftype=hd refid=constants_ordinary.Ordinary constants:elink.
:li.:link reftype=hd refid=constants_typed.Typed constants:elink.
:li.:link reftype=hd refid=constants_resource.Resource strings:elink.
:eul.

.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Ordinary constants
:h3 name=constants_ordinary.Ordinary constants
:p.
Ordinary constants declarations are constructed using an identifier name 
followed by an "=" token, and followed by an optional expression consisting 
of legal combinations of numbers, characters, boolean values or enumerated 
values as appropriate. The following syntax diagram shows how to construct 
a legal declaration of an ordinary constant.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Constant declaration:ehp2.

&ra.&ra.─── constant declaration ─┬─ identifier ─ = ─ expression ─ hint directives ─ ; ─┬───&ra.&la.
                            ^─────────────────────────────────────────────────────┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
The compiler must be able to evaluate the expression in a constant
declaration at compile time.  This means that most of the functions
in the Run-Time library cannot be used in a constant
declaration.

:p.
Operators such as +, -, *, /, not, and, or, div, mod, ord, chr,
sizeof, pi, int, trunc, round, frac, odd can be used, however. 
For more information on expressions, see the section :link reftype=hd refid=expressions.Expressions:elink.&per.


:p.
Only constants of the following types can be declared: :hp1.Ordinal types:ehp1.&comma.
:hp1.Real types:ehp1.&comma. :hp1.Char:ehp1.&comma. and :hp1.String:ehp1.&per.
The following are all valid constant declarations&colon.

:xmp.
Const
  e = 2.7182818;  { Real type constant. }
  a = 2;          { Ordinal (Integer) type constant. }
  c = '4';        { Character type constant. }
  s = 'This is a constant string'; {String type constant.}
  s = chr(32)
  ls = SizeOf(Longint);
:exmp.

:p.
Assigning a value to an ordinary constant is not permitted.
Thus, given the previous declaration, the following will result
in a compiler error:
:xmp.
  s := 'some other string';
:exmp.

:p.
For string constants, the type of the string is dependent on some compiler
switches. If a specific type is desired, a typed constant should be used, 
as explained in the following section.

:p.
Prior to version 1.9, &fpc. did not correctly support 64-bit constants. As
of version 1.9, 64-bit constants can be specified.



.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Typed constants
:h3 name=constants_typed.Typed constants
:p.
Sometimes it is necessary to specify the type of a constant, for instance
for constants of complex structures (defined later in the manual).
Their definition is quite simple.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Constant declaration:ehp2.

       typed
&ra.&ra.─── constant ───┬─ identifier ─ : ─ type ─ = ─ type constant ─ hint directives ─ ; ─┬───&ra.&la.
     declaration  ^───────────────────────────────────────────────────────────────────┘

&ra.&ra.─── typed constant ──┬────── constant ───────┬──────────────────────────────&ra.&la.
                       ├─   address constant  ─┤
                       ├─    array constant   ─┤
                       ├─   record constant   ─┤
                       └─ procedural constant ─┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
Contrary to ordinary constants, a value can be assigned to them at 
run-time. This is an old concept from &tp., which has been 
replaced with support for initialized variables: For a detailed 
description, see :link reftype=hd refid='variables_initializedvars'.Initialized variables:elink..

Support for assigning values to typed constants is controlled by the 
:hp2.{$J}:ehp2. directive: it can be switched off, but is on by default 
(for &tp. compatibility). Initialized variables are always allowed.

:nt.
It should be stressed that typed constants are automatically initialized at program start.
This is also true for :hp1.local:ehp1. typed constants and initialized variables. 
Local typed constants are also initialized at program start. If their value was 
changed during previous invocations of the function, they will retain their 
changed value, i.e. they are not initialized each time the function is invoked.
:ent.

.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.* % Ordinary constants
:h3 name=constants_resource.Resource strings
:p.
A special kind of constant declaration block is the :hp1.resourcestring:ehp1.
block. Resourcestring declarations are much like constant string
declarations: resource strings act as constant strings, but they 
can be localized by means of a set of special routines in the 
:hp1.objpas:ehp1. unit. A resource string declaration block
is only allowed in the :hp1.Delphi:ehp1. or :hp1.ObjFPC:ehp1. modes.

:p.
The following is an example of a resourcestring definition:
:xmp.
resourcestring
  FileMenu = '&amp.File...';
  EditMenu = '&amp.Edit...';
:exmp.

:p.
All string constants defined in the resourcestring section are stored
in special tables. The strings in these tables can be manipulated
at runtime with some special mechanisms in the :hp1.objpas:ehp1. unit.

:p.
Semantically, the strings act like ordinary constants; It is not allowed
to assign values to them (except through the special mechanisms in the 
objpas unit). However, they can be used in assignments or expressions as 
ordinary string constants. The main use of the resourcestring section is 
to provide an easy means of internationalization.

:p.
More on the subject of resourcestrings can be found in the 
:link reftype=hd database='prog.inf' refid=0.&progref.:elink., and
in the :hp1.objpas:ehp1. unit reference.

:nt.
Note that a resource string which is given as an expression will not change if
the parts of the expression are changed:
:xmp.
resourcestring
  Part1 = 'First part of a long string.';
  Part2 = 'Second part of a long string.';
  Sentence = Part1 + ' ' + Part2;
:exmp.

:p.
If the localization routines translate :hp1.Part1:ehp1. and :hp1.Part2:ehp1., the
:hp1.Sentence:ehp1. constant will not be translated automatically: it has a
separate entry in the resource string tables, and must therefor be
translated separately. The above construct simply says that the 
initial value of :hp1.Sentence:ehp1. equals :hp1.Part1+' '+Part2:ehp1..
:ent.

:nt.
Likewise, when using resource strings in a constant array, only the initial
values of the resource strings will be used in the array: when the
individual constants are translated, the elements in the array will retain
their original value.
:xmp.
resourcestring
  Yes = 'Yes.';
  No = 'No.';

var
  YesNo: Array[Boolean] of string = (No, Yes);
  B: Boolean;

begin
  Writeln(YesNo[B]);
end.
:exmp.

:p.
This will print 'Yes.' or 'No.' depending on the value of B, even if the 
constants Yes and No have been localized by some localization mechanism.
:ent.


.* ==============================================================
:h2 name=types.Types
:p.
All variables have a type. &fpc. supports the same basic types as &tp., with
some extra types from &delphi..
The programmer can declare his own types, which is in essence defining an identifier
that can be used to denote this custom type when declaring variables further
in the source code.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Type declaration:ehp2.

&ra.&ra.─── type declaration ─── identifier ─ = ─ type ─ ; ─────────────────────────&ra.&la.

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
There are 7 major type classes:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Types:ehp2.

&ra.&ra.─── type  ──┬─── simple type ────┬──────────────────────────────────────────&ra.&la.
              ├─   string type    ─┤
              ├─ structured type  ─┤
              ├─  pointer type    ─┤
              ├─ procedural type  ─┤
              ├─  generic type    ─┤
              ├─ specialized type ─┤
              └─ type identifier  ─┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
The last case, :hp1.type identifier:ehp1., is just a means to give another
name to a type. This presents a way to make types platform independent, by
only using these types, and then defining these types for each platform
individually. Any programmer who then uses these custom types doesn't have to worry
about the underlying type size: it is opaque to him. It also allows to use shortcut names 
for fully qualified type names. e.g. define :hp1.system.longint:ehp1. as
:hp1.Olongint:ehp1. and then redefine :hp1.longint:ehp1..


.* --------------------------------------------------------------
:h3 name=base_types.Base types
:p.
The base or simple types of &fpc. are the &delphi. types.
We will discuss each type separately.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Simple types:ehp2.

&ra.&ra.─── simple type  ──┬─ ordinal type ─┬───────────────────────────────────────&ra.&la.
                     └─  real type   ─┘

&ra.&ra.─── real type ─── real type identifier ─────────────────────────────────────&ra.&la.

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.


.* ...................................................................
:h4.Ordinal types
:p.
With the exception of :hp1.int64:ehp1., :hp1.qword:ehp1. and Real types, 
all base types are ordinal types. Ordinal types have the following 
characteristics:
:ol.
:li. Ordinal types are countable and ordered, i.e. it is, in principle,
possible to start counting them one by one, in a specified order.
This property allows the operation of functions as :hp2.Inc:ehp2., :hp2.Ord:ehp2.,
:hp2.Dec:ehp2. on ordinal types to be defined.
:li. Ordinal values have a smallest possible value. Trying to apply the
:hp2.Pred:ehp2. function on the smallest possible value will generate a range
check error if range checking is enabled.
:li. Ordinal values have a largest possible value. Trying to apply the
:hp2.Succ:ehp2. function on the largest possible value will generate a range
check error if range checking is enabled.
:eol.


:h5.Integers
:p.
A list of pre-defined integer types are presented below.

:lm margin=10.
:cgraphic.
  :hp2.Name:ehp2.
────────────
  Integer
  Shortint
  SmallInt
  Longint
  Longword
  Int64
  Byte
  Word
  Cardinal
  QWord
  Boolean
  ByteBool
  WordBool
  LongBool
  Char
────────────
:ecgraphic.
:lm margin=1.

:p.
The integer types, and their ranges and sizes, that are predefined in
&fpc. are listed in in the table below. Please note that
the :hp1.qword:ehp1. and :hp1.int64:ehp1. types are not true ordinals, so
some Pascal constructs will not work with these two integer types.

:cgraphic.
 :hp2.Type                         Range                   Size in bytes:ehp2.
────────────────────────────────────────────────────────────────────
 Byte                         0 .. 255                            1
 Shortint                  -128 .. 127                            1
 Smallint                -32768 .. 32767                          2
 Word                         0 .. 65535                          2
 Integer              either smallint or longint        size 2 or 4
 Cardinal                    longword                             4
 Longint            -2147483648 .. 2147483647                     4
 Longword                     0 .. 4294967295                     4
 Int64     -9223372036854775808 .. 9223372036854775807            8
 QWord                        0 .. 18446744073709551615           8
────────────────────────────────────────────────────────────────────
:ecgraphic.

:p.
The :hp1.integer:ehp1. type maps to the smallint type in the default
&fpc. mode. It maps to either a longint in either Delphi or ObjFPC
mode. The :hp1.cardinal:ehp1. type is currently always mapped to the 
longword type.

:nt.
All decimal constants which do no fit within the -2147483648..2147483647 range 
are silently and automatically parsed as 64-bit integer constants as of version 
1.9.0. Earlier versions would convert it to a real-typed constant.
:ent.

&fpc. does automatic type conversion in expressions where different kinds of
integer types are used.


:h5.Boolean types
:p.
&fpc. supports the :hp1.Boolean:ehp1. type, with its two pre-defined possible
values :hp1.True:ehp1. and :hp1.False:ehp1.. These are the only two values that can be
assigned to a :hp1.Boolean:ehp1. type. Of course, any expression that resolves
to a boolean value, can also be assigned to a boolean type.

:cgraphic.
 :hp2.Name             Size        Ord(True):ehp2.
─────────────────────────────────────────────────
 Boolean           1          1
 ByteBool          1          Any nonzero value
 WordBool          2          Any nonzero value
 LongBool          4          Any nonzero value
─────────────────────────────────────────────────
:ecgraphic.

:p.
&fpc. also supports the :hp1.ByteBool:ehp1., :hp1.WordBool:ehp1. and :hp1.LongBool:ehp1. types.
These are of type :hp1.Byte:ehp1., :hp1.Word:ehp1. or :hp1.Longint:ehp1., but are
assignment compatible with a :hp1.Boolean:ehp1.: the value :hp1.False:ehp1. is 
equivalent to 0 (zero) and any nonzero value is considered :hp1.True:ehp1. when
converting to a boolean value. A boolean value of :hp1.True:ehp1. is converted
to -1 in case it is assigned to a variable of type :hp1.LongBool:ehp1..

:p.
Assuming :hp1.B:ehp1. to be of type :hp1.Boolean:ehp1., the following are valid
assignments:

:xmp.
 B := True;
 B := False;
 B := 1 <> 2;  { Results in B := True }
:exmp.

:p.
Boolean expressions are also used in conditions.

:nt.
In &fpc., boolean expressions are by default always evaluated in such a
way that when the result is known, the rest of the expression will no longer
be evaluated: this is called short-cut boolean evaluation.

:p.
In the following example, the function :hp1.Func:ehp1. will never be called, 
which may have strange side-effects.

:xmp.
 ...
 B := False;
 A := B and Func;
:exmp.

:p.
Here :hp1.Func:ehp1. is a function which returns a :hp1.Boolean:ehp1. type.

This behaviour is controllable by the :hp2.{$B}:ehp2. compiler directive.
:ent.


.* ...................................................................
:h5.Enumeration types
:p.
Enumeration types are supported in &fpc.. On top of the &tp.
implementation, &fpc. also allows a C-style extension of the
enumeration type, where a value is assigned to a particular element of
the enumeration list.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Enumeration types:ehp2.

&ra.&ra.─── enumerated type ─ ( ─┬┬── identifier list ───┬┬─ ) ─────────────────────&ra.&la.
                           │└─ assigned enum list ─┘│
                           ^────────── , ───────────┘

&ra.&ra.─── identifier list ──┬─ identifier ─┬──────────────────────────────────────&ra.&la.
                        ^────── , ─────┘

&ra.&ra.─── assigned enum list ──┬─ identifier ─ := ─ expression ─┬─────────────────&ra.&la.
                           ^─────────────── , ──────────────┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
(see :link reftype=hd refid=expressions.Expressions:elink. for how to use expressions)
When using assigned enumerated types, the assigned elements must be in
ascending numerical order in the list, or the compiler will complain.
The expressions used in assigned enumerated elements must be known at
compile time. So the following is a correct enumerated type declaration:
:xmp.
Type
  Direction = (North, East, South, West);
:exmp.

:p.
A C-style enumeration type looks as follows:

:xmp.
Type
  EnumType = (one, two, three, forty := 40, fortyone);
:exmp.

:p.
As a result, the ordinal number of :hp1.forty:ehp1. is 40, and not 3,
as it would be when the ':= 40' wasn't present.
The ordinal value of :hp1.fortyone:ehp1. is then 41, and not 4, as it
would be when the assignment wasn't present. After an assignment in an
enumerated definition the compiler adds 1 to the assigned value to assign to
the next enumerated value.
:p.
When specifying such an enumeration type, it is important to keep in mind
that the enumerated elements should be kept in ascending order. The
following will produce a compiler error:

:xmp.
Type
  EnumType = (one, two, three, forty := 40, thirty := 30);
:exmp.

:p.
It is necessary to keep :hp1.forty:ehp1. and :hp1.thirty:ehp1. in the correct order.
When using enumeration types it is important to keep the following points
in mind:

:ol.
:li. The :hp2.Pred:ehp2. and :hp2.Succ:ehp2. functions cannot be used on
this kind of enumeration types. Trying to do this anyhow will result in a
compiler error.
:li. Enumeration types are stored using a default, independent of the
actual number of values: the compiler does not try to optimize for space.
This behaviour can be changed with the :hp2.{$PACKENUM n}:ehp2. compiler 
directive, which tells the compiler the minimal number of bytes to be 
used for enumeration types. For instance:

:xmp.
type
{$PACKENUM 4}
  LargeEnum = ( BigOne, BigTwo, BigThree );
{$PACKENUM 1}
  SmallEnum = ( one, two, three );
var
  S: SmallEnum;
  L: LargeEnum;
begin
  WriteLn('Small enum : ', SizeOf(S));
  WriteLn('Large enum : ', SizeOf(L));
end.
:exmp.
:eol.

More information can be found in the &progref., in the Compiler Directives section.


.* ...................................................................
:h5.Subrange types
:p.
A subrange type is a range of values from an ordinal type (the host type). To define a subrange type,
one must specify its limiting values: the highest and lowest value of the type.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Subrange types:ehp2.

&ra.&ra.─── subrange type ─ constant ─ .. ─ constant ───────────────────────────────&ra.&la.

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
Some of the predefined integer types are defined as subrange types:

:xmp.
type
  Longint  = $80000000..$7fffffff;
  Integer  = -32768..32767;
  shortint = -128..127;
  byte     = 0..255;
  Word     = 0..65535;
:exmp.

:p.
Subrange types of enumeration types can also be defined:

:xmp.
type
  Days = (monday, tuesday, wednesday, thursday, friday, saturday, sunday);
  WorkDays = monday..friday;
  WeekEnd  = saturday..sunday;
:exmp.


.* ...................................................................
:h4.Real types
:p.
&fpc. uses the math coprocessor (or emulation) for all its floating-point calculations. The Real
native type is processor dependent, but it is either Single or Double. Only the IEEE floating point
types are supported, and these depend on the target processor and emulation options. The true &tp.
compatible types are listed in the table below. The :hp1.Comp:ehp1. type is, in effect, a 64-bit integer and
is not available on all target platforms. To get more information on the supported types for each
platform, refer to the &progref..


:cgraphic.
 :hp2.Type                Range           Significant digits     Size in bytes:ehp2.
──────────────────────────────────────────────────────────────────────────
 Real          platform dependent           ???                  4 or 8
 Single        1.5E-45 .. 3.4E38            7-8                       4
 Double       5.0E-324 .. 1.7E308          15-16                      8
 Extended    1.9E-4932 .. 1.1E4932         19-20                     10
 Comp          -2E64+1 .. 2E63-1           19-20                      8
 Currency    -922337203685477.5808 ..      19-20                      8
              922337203685477.5807
──────────────────────────────────────────────────────────────────────────
:ecgraphic.

:p.
The currency type is a fixed-point real data type which is internally used as an 64-bit integer type
(automatically scaled with a factor 10000), this minimalizes rounding errors.



.* --------------------------------------------------------------
:h3 name=character_types.Character types
:p.
We have eight character types in &fpc..

:ul.
:li.:link reftype=hd refid=character_types_char.Char:elink.
:li.:link reftype=hd refid=character_types_strings.Strings:elink.
:li.:link reftype=hd refid=character_types_shortstrings.Short strings:elink.
:li.:link reftype=hd refid=character_types_ansistrings.AnsiStrings:elink.
:li.:link reftype=hd refid=character_types_widestrings.WideStrings:elink.
:li.:link reftype=hd refid=character_types_unicodestrings.UnicodeStrings:elink.
:li.:link reftype=hd refid=character_types_constantstrings.Constant strings:elink.
:li.:link reftype=hd refid=character_types_pcharstrings.PChar - Null terminated strings:elink.
:eul.


.* ...................................................................
:h4 name=character_types_char.Char
:p.
&fpc. supports the type :hp2.Char:ehp2.. A :hp2.Char:ehp2. is exactly 1 byte in
size, and contains one ASCII character.

:p.
A character constant can be specified by enclosing the character in single
quotes, as follows: 'a' or 'A' are both character constants.

:p.
A character can also be specified by its character
value (commonly an ASCII code), by preceding the ordinal value with the 
number symbol (#). For example specifying :hp1.#65:ehp1. would be the same as :hp1.'A':ehp1.

:p.
Also, the caret character (^) can be used in combination with a letter to
specify a character with ASCII value less than 27. Thus ^G equals
#7 - G is the seventh letter in the alphabet.

:p.
When the single quote character must be represented, it should be typed
two times successively, thus :hp1.'''':ehp1. represents the single quote character.


.* ...................................................................
:h4 name=character_types_strings.Strings
:p.
&fpc. supports the :hp2.String:ehp2. type as it is defined in &tp.:
a sequence of characters with an optional size specification.
It also supports AnsiStrings (with unlimited length) as in Delphi.

:p.
To declare a variable as a string, use the following type specification:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.String Type:ehp2.

&ra.&ra.─── string type ─ :hp2.string:ehp2. ──┬──────────────────────────────┬─────────────────&ra.&la.
                             └─ [ ── unsigned integer ── ] ─┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
If there is a size specifier, then its maximum value - indicating the maximum 
size of the string - is 255.
:p.
The meaning of a string declaration statement without size indicator is 
interpreted differently depending on the :hp2.{$H}:ehp2. switch. If no size
indication is present, the above declaration can declare an AnsiString or 
a short string.
:p.
Whatever the actual type, AnsiStrings and short strings can be used
interchangeably. The compiler always takes care of the necessary type
conversions. Note, however, that the result of an expression that contains
ansistrings and short strings will always be an AnsiString.


:h4 name=character_types_shortstrings.Short strings
:p.
A string declaration declares a short string in the following cases:
:ol.
:li. If the switch is off: {$H-}, the string declaration
will always be a short string declaration.
:li. If the switch is on {$H+}, and there is a maximum length (the
size) specifier, the declaration is a short string declaration.
:eol.

:p.
The predefined type :hp2.ShortString:ehp2. is defined as a string of size 255:

:xmp.
 ShortString = String[255];
:exmp.

:p.
If the size of the string is not specified, 255 is taken as a
default. The actual length of the string can be obtained with the
:hp1.Length():ehp1. standard runtime routine. For example in

:xmp.
{$H-}

type
  NameString = String[10];
  StreetString = String;
:exmp.

:p.
:hp1.NameString:ehp1. can contain a maximum of 10 characters. While
:hp1.StreetString:ehp1. can contain up to 255 characters.

:nt.
Short strings have a maximum length of 255 characters: when specifying a
maximum length, the maximum length may not exceed 255. If a length larger
than 255 is attempted, then the compiler will give an error message:

:xmp.
Error: string length must be a value from 1 to 255
:exmp.

:p.
For short strings, the length is stored in the character at index 0. Old
&tp. code relies on this, and it is implemented similarly in &fpc.. 
Despite this, to write portable code, it is best to set the length of a 
shortstring  with the :hp1.SetLength():ehp1. call, and to retrieve
it with the :hp1.Length():ehp1. call. These functions will always work, whatever
the internal representation of the short strings or other strings in use:
this allows easy switching between the various string types.
:ent.


.* ...................................................................
:h4 name=character_types_ansistrings.AnsiStrings
:p.
AnsiStrings are strings that have no length limit. They are reference
counted and are guaranteed to be null terminated. Internally, an ansistring is treated as 
a pointer: the actual content of the string is stored on the heap, as much
memory as needed to store the string content is allocated. 

This is all handled transparantly, i.e. they can be manipulated as a normal 
short string. Ansistrings can be defined using the predefined :hp2.AnsiString:ehp2. 
type. 

:nt.
The null-termination does not mean that null characters (char(0) or #0) 
cannot be used: the null-termination is not used internally, but is there for
convenience when dealing with external routines that expect a
null-terminated string (as most C routines do).
:ent.

:p.
If the {$H} switch is on, then a string definition using the
regular :hp1.String:ehp1. keyword and that doesn't contain a length specifier, 
will be regarded as an ansistring as well. If a length specifier is present,
a short string will be used, regardless of the {$H} setting.

:p.
If the string is empty (''), then the internal pointer representation
of the string pointer is :hp1.Nil:ehp1.. If the string is not empty, then the 
pointer points to a structure in heap memory.

:p.
The internal representation as a pointer, and the automatic null-termination
make it possible to typecast an ansistring to a pchar. If the string is empty 
(so the pointer is Nil) then the compiler makes sure that the typecasted 
pchar will point to a null byte.

:p.
Assigning one ansistring to another doesn't involve moving the actual
string. A statement

:xmp.
  S2 := S1;
:exmp.

:p.
results in the reference count of :hp1.S2:ehp1. being decreased with 1, 
the reference count of :hp1.S1:ehp1. is increased by 1, and finally :hp1.S1:ehp1.
(as a pointer) is copied to :hp1.S2:ehp1.. This is a significant speed-up in
the code.

:p.
If the reference count of a string reaches zero, then the memory occupied 
by the string is deallocated automatically, and the pointer is set to
:hp1.Nil:ehp1., so no memory leaks arise.

:p.
When an ansistring is declared, the &fpc. compiler initially
allocates just memory for a pointer, not more. This pointer is guaranteed
to be :hp1.Nil:ehp1., meaning that the string is initially empty. This is
true for local and global ansistrings or ansistrings that are part of a 
structure (arrays, records or objects).

:p.
This does introduce an overhead. For instance, declaring

:xmp.
var
  A: array[1..100000] of string;
:exmp.

:p.
will copy the value :hp1.Nil:ehp1. 100,000 times into :hp1.A:ehp1.. 
When :hp1.A:ehp1. goes out of scope, then the reference 
count of the 100,000 strings will be decreased by 1 for each
of these strings. All this happens invisible to the programmer, 
but when considering performance issues, this is important.

:p.
Memory for the string content will be allocated only when the string is 
assigned a value. If the string goes out of scope, then its reference 
count is automatically decreased by 1. If the reference count reaches 
zero, the memory reserved for the string is released.

:p.
If a value is assigned to a character of a string that has a reference count
greater than 1, such as in the following statements:

:xmp.
  S := T;  { reference count for S and T are now 2 }
  S[I] := '@';
:exmp.

:p.
then a copy of the string is created before the assignment. This is known
as :hp1.copy-on-write:ehp1. semantics. It is possible to force a string to have
a reference count equal to 1 with the :hp1.UniqueString():ehp1. call:

:xmp.
  S := T;
  R := T; // Reference count of T is at least 3
  UniqueString(T); // Reference count of T is quaranteed 1
:exmp.

:p.
It's recommended to do this e.g. when typecasting an ansistring to a PChar var
and passing it to a C routine that modifies the string.

:p.
The :hp1.Length():ehp1. function must be used to get the length of an
ansistring: the length is not stored at character 0 of the ansistring. 
The construct

:xmp.
 L := ord(S[0]);
:exmp.

:p.
which was valid for &tp. shortstrings, is no longer correct for
AnsiStrings. The compiler will warn if such a construct is encountered.

:p.
To set the length of an ansistring, the :hp1.SetLength():ehp1. function must be used.
Constant ansistrings have a reference count of -1 and are treated specially.
The same remark as for :hp1.Length():ehp1. must be given: The construct

:xmp.
  L := 12;
  S[0] := Char(L);
:exmp.

:p.
which was valid for &tp. shortstrings, is no longer correct for
AnsiStrings. The compiler will warn if such a construct is encountered.

:p.
AnsiStrings are converted to short strings by the compiler if needed,
this means that the use of ansistrings and short strings can be mixed
without problems.

:p.
AnsiStrings can be typecasted to :hp1.PChar:ehp1. or :hp1.Pointer:ehp1. types:

:xmp.
var
  P: Pointer;
  PC: PChar;
  S: AnsiString;
begin
  S  := 'This is an ansistring';
  PC := Pchar(S);
  P  := Pointer(S);
:exmp.

:p.
There is a difference between the two typecasts. When an empty
ansistring is typecasted to a pointer, the pointer wil be :hp1.Nil:ehp1.. If an
empty ansistring is typecasted to a :hp1.PChar:ehp1., then the result will be a pointer to a
zero byte (an empty string).

:p.
The result of such a typecast must be used with care. In general, it is best
to consider the result of such a typecast as read-only, i.e. only suitable for
passing to a procedure that needs a constant pchar argument.

:p.
It is therefore :hp2.not:ehp2. advisable to typecast one of the following:

:ol.
:li. Expressions.
:li. Strings that have a reference count larger than 1.
In this case you should call :hp1.UniqueString():ehp1. to ensure the 
string has a reference count 1.
:eol.



.* ...................................................................
:h4 name=character_types_widestrings.WideStrings
:p.
WideStrings (used to represent Unicode character strings) are implemented in much 
the same way as AnsiStrings: reference counted, null-terminated arrays, only they 
are implemented as arrays of :hp1.WideChars:ehp1. instead of regular :hp1.Chars:ehp1..
A :hp1.WideChar:ehp1. is a two-byte character (an element of a DBCS: Double Byte
Character Set). Mostly the same rules apply for WideStrings as for 
AnsiStrings. The compiler transparently converts WideStrings to
AnsiStrings and vice versa.

:p.
Similarly to the typecast of an Ansistring to a PChar null-terminated
array of characters, a WideString can be converted to a PWideChar
null-terminated array of characters. 
Note that the :hp1.PWideChar:ehp1. array is terminated by 2 null bytes instead of
1, so a typecast to a PChar is not automatic.

:p.
The compiler itself provides no support for any conversion from Unicode to
AnsiStrings or vice versa. The :hp2.system:ehp2. unit has a widestring manager
record, which can be initialized with some OS-specific Unicode handling
routines. For more information, see the :hp2.system:ehp2. unit reference.


.* ...................................................................
:h4 name=character_types_unicodestrings.UnicodeStrings
:p.
[text to be written]
:p.
Same as WideString, but reference counted on all platforms.



.* ...................................................................
:h4 name=character_types_constantstrings.Constant strings
:p.
To specify a constant string, it must be enclosed in single quotes, just
as a :hp1.Char:ehp1. type, only now more than one character is allowed.
Given that :hp1.S:ehp1. is of type :hp1.String:ehp1., the following are valid
assignments:

:xmp.
S := 'This is a string.';
S := 'One'+', Two'+', Three';
S := 'This isn''t difficult!';
S := 'This is a weird character: '#145' !';
:exmp.

:p.
As can be seen, the single quote character is represented by 2 single-quote
characters next to each other. Strange characters can be specified by their
character value (usually an ASCII code).
The example shows also that two strings can be added. The resulting string is
just the concatenation of the first with the second string, without spaces in
between them. Strings can not be substracted, however.

:p.
Whether the constant string is stored as an AnsiString or a short string
depends on the settings of the :hp2.{$H}:ehp2. switch.



.* ...................................................................
:h4 name=character_types_pcharstrings.PChar - Null terminated strings
:p.
&fpc. supports the Delphi implementation of the :hp1.PChar:ehp1. type. :hp1.PChar:ehp1.
is defined as a pointer to a \var{Char} type, but allows additional
operations.
The :hp1.PChar:ehp1. type can be understood best as the Pascal equivalent of a
C-style null-terminated string, i.e. a variable of type :hp1.PChar:ehp1. is a
pointer that points to an array of type :hp1.Char:ehp1., which is ended by a
null-character (#0).
&fpc. supports initializing of :hp1.PChar:ehp1. typed constants, or a direct
assignment. For example, the following pieces of code are equivalent:

:xmp.
program one;
var
  p: PChar;
begin
  P := 'This is a null-terminated string.';
  WriteLn (P);
end.
:exmp.

:p.
Results in the same as

:xmp.
program two;
const
  P: PChar = 'This is a null-terminated string.'
begin
  WriteLn (P);
end.
:exmp.

:p.
These examples also show that it is possible to write the contents of
the string to a file of type :hp1.Text:ehp1.
The :hp2.strings:ehp2. unit contains procedures and functions that manipulate the
:hp1.PChar:ehp1. type as in the standard C library.
Since it is equivalent to a pointer to a type :hp1.Char:ehp1. variable, it is
also possible to do the following:

:xmp.
program three;
var
  S: String[30];
  P: PChar;
begin
  S := 'This is a null-terminated string.'#0;
  P := @S[1];
  WriteLn (P);
end.
:exmp.

:p.
This will have the same result as the previous two examples.
Null-terminated strings cannot be added as normal Pascal
strings. If two :hp1.PChar:ehp1. strings must be concatenated; the functions from
the unit :hp2.strings:ehp2. must be used.

:p.
However, it is possible to do some pointer arithmetic. The
operators + and - can be used to do operations 
on :hp1.PChar:ehp1. pointers.
In the table below, :hp1.P:ehp1. and :hp1.Q:ehp1. are of type :hp1.PChar:ehp1., and
:hp1.I:ehp1. is of type :hp1.Longint:ehp1..

:cgraphic.
 :hp2.Operation                                                  Result:ehp2.
────────────────────────────────────────────────────────────────────
 P + I                       Adds I to the address pointed to by P.
 I + P                       Adds I to the address pointed to by P.
 P - I               Substracts I from the address pointed to by P.
 P - Q     Returns, as an integer, the distance between 2 addresses
                      (or the number of characters between P and Q)
────────────────────────────────────────────────────────────────────
:ecgraphic.



.* --------------------------------------------------------------
:h3 name=structured_types.Structured types
:p.
A structured type is a type that can hold multiple values in one variable.
Stuctured types can be nested to unlimited levels.

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Structured Types:ehp2.

&ra.&ra.─── structured type  ──┬────  array type    ────┬───────────────────────────&ra.&la.
                         ├────  record type   ────┤
                         ├────  object type   ────┤
                         ├────  class type    ────┤
                         ├─ class reference type ─┤
                         ├──── interface type ────┤
                         ├────    set type    ────┤
                         └─────  file type  ──────┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
Unlike Delphi, &fpc. does not support the keyword :hp1.packed:ehp1. for all
structured types.  In the following sections each of the possible 
structured types is discussed. It will be mentioned when a type supports 
the :hp1.packed:ehp1. keyword.


.* ...................................................................
:h4 name=structured_types_packed.Packed structured types
:p.
When a structured type is declared, no assumptions should be made about
the internal position of the elements in the type. The compiler will lay
out the elements of the structure in memory as it thinks will be most
suitable. That is, the order of the elements will be kept, but the location
of the elements are not guaranteed, and is partially governed by the :hp2.$PACKRECORDS:ehp2.
directive (this directive is explained in the &progref.).

:p.
However, &fpc. allows controlling the layout with the :hp1.Packed:ehp1. and
:hp1.Bitpacked:ehp1. keywords. The meaning of these words depends on the context:

:parml tsize=15 break=none.
:pt.:hp2.Bitpacked:ehp2.
:pd.In this case, the compiler will attempt to align ordinal
types on bit boundaries, as explained below.

:pt.:hp2.Packed:ehp2.
:pd.The meaning of the :hp1.Packed:ehp1. keyword depends on the
situation:
:ol.
:li. In :hp1.MACPAS:ehp1. mode, it is equivalent to the :hp1.Bitpacked:ehp1. keyword.
:li. In other modes, with the :hp1.$BITPACKING:ehp1. directive set to :hp1.ON:ehp1.,
it is also equivalent to the :hp1.Bitpacked:ehp1. keyword.
:li. In other modes, with the :hp1.$BITPACKING:ehp1. directive set to :hp1.OFF:ehp1.,
it signifies normal packing on byte boundaries.
:lp. Packing on byte boundaries means that each new element of a structured type
starts on a byte boundary.
:eol.
:eparml.

:p.
The byte packing mechanism is simple: the compiler aligns each element of
the structure on the first available byte boundary, even if the size of the
previous element (small enumerated types, subrange types) is less than a
byte.

:p.
When using the bit packing mechanism, the compiler calculates for each
ordinal type how many bits are needed to store it. The next ordinal type
is then stored on the next free bit. Non-ordinal types - which include but
are not limited to - sets, floats, strings, (bitpacked) records, (bitpacked)
arrays, pointers, classes, objects, and procedural variables, are stored
on the first available byte boundary.

:p.
Note that the internals of the bitpacking are opaque: they can change
at any time in the future. What is more: the internal packing depends
on the endianness of the platform for which the compilation is done,
and no conversion between platforms are possible. This makes bitpacked
structures unsuitable for storing on disk or transport over networks.
The format is however the same as the one used by the GNU Pascal
Compiler, and the &fpc. team aims to retain this compatibility in the future.

:p.
There are some more restrictions to elements of bitpacked structures:

:ul.
:li. The address cannot be retrieved, unless the bit size is a multiple of
8 and the element happens to be stored on a byte boundary.
:li. An element of a bitpacked structure cannot be used as a var parameter,
unless the bit size is a multiple of 8 and the element happens to be stored 
on a byte boundary.
:eul.

:p.
To determine the size of an element in a bitpacked structure, there is the 
:hp1.BitSizeOf():ehp1. function. It returns the size - in bits - of the element. 
For other types or elements of structures which are not bitpacked, this will 
simply return the size in bytes multiplied by 8, i.e., the return value is 
then the same as :hp1.8*SizeOf:ehp1..

:p.
The size of bitpacked records and arrays is limited:

:ul.
:li. On 32 bit systems the maximal size is 2^29 (2 to the power 29) bytes (512 MB).
:li. On 64 bit systems the maximal size is 2^61 (2 to the power 61) bytes.
:eul.

:p.
The reason is that the offset of an element must be calculated with 
the maximum integer size of the system.


.* ...................................................................
:h4 name=structured_types_arrays.Arrays
:p.
&fpc. supports arrays as in &tp.. Multi-dimensional arrays and (bit)packed 
arrays are also supported, as well as the dynamic arrays of &delphi.:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Array types:ehp2.

&ra.&ra.── array type ─┬───────────────┬─ :hp2.array:ehp2. ─┬────────────────────────────┬─ :hp2.of:ehp2. ─ type ──&ra.&la.
                 ├─   :hp2.packed:ehp2.    ─┤         └─ :hp2.[:ehp2. ─┬─ ordinal type ─┬─ :hp2.]:ehp2. ─┘
                 └─  :hp2.bitpacked:ehp2.  ─┘               ^────── , ───────┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.



:h5.Static arrays
:p.
When the range of the array is included in the array definition, it is
called a static array. Trying to access an element with an index that is
outside the declared range will generate a run-time error (if range checking
is on).  The following is an example of a valid array declaration:

:xmp.
type
  RealArray = Array [1..100] of Real;
:exmp.

:p.
Valid indexes for accessing an element of the array are between 1 and 100,
where the borders 1 and 100 are included.
As in &tp., if the array component type is in itself an array, it is
possible to combine the two arrays into one multi-dimensional array. The
following declaration:

:xmp.
type
   APoints = array[1..100] of Array[1..3] of Real;
:exmp.

:p.
is equivalent to the declaration:

:xmp.
type
   APoints = array[1..100, 1..3] of Real;
:exmp.

:p.
The functions :hp1.High():ehp1. and :hp1.Low():ehp1. return the high and low bounds of
the leftmost index type of the array. In the above case, this would be 100
and 1. You should use them whenever possible, since it improves maintainability
of your code. The use of both functions is just as efficient as using
constants, because they are evaluated at compile time.

:p.
When static array-type variables are assigned to each other, the contents of the
whole array is copied. This is also true for multi-dimensional arrays:

:xmp.
program testarray1;

type
  TA = Array[0..9, 0..9] of Integer;
  
var   
  A, B: TA;
  I, J: Integer;
begin
  for I := 0 to 9 do
    for J := 0 to 9 do 
      A[I,J] := I * J;
  for I := 0 to 9 do
  begin
    for J := 0 to 9 do 
      Write(A[I, J]:2, ' ');
    writeln;
  end;
  B := A;
  writeln;
  for I := 0 to 9 do
    for J := 0 to 9 do 
      A[9-I, 9-J] := I * J;
  for I := 0 to 9 do
  begin
    for J := 0 to 9 do 
      write(B[I, J]:2, ' ');
    writeln;
  end;
end.  
:exmp.

:p.
The output of this program will be 2 identical matrices.



:h5.Dynamic arrays
:p.
As of version 1.1, &fpc. also knows dynamic arrays: In that case the array
range is omitted, as in the following example:

:xmp.
Type
  TByteArray = Array of Byte;
:exmp.

:p.
When declaring a variable of a dynamic array type, the initial length of the
array is zero. The actual length of the array must be set with the standard
:hp1.SetLength():ehp1. function, which will allocate the necessary memory to contain 
the array elements on the heap. The following example will set the length to
1000:

:xmp.
var 
  A: TByteArray;
begin
  SetLength(A, 1000);
:exmp.

:p.
After a call to :hp1.SetLength():ehp1., valid array indexes are 0 to 999: the array
index is always zero-based.

:p.
Note that the length of the array is set in elements, not in bytes of 
allocated memory (although these may be the same). The amount of 
memory allocated is the size of the array multiplied by the size of 
1 element in the array. The memory will be disposed of at the exit of the
current procedure or function. 

:p.
It is also possible to resize the array: in that case, as much of the 
elements in the array as will fit in the new size, will be kept. The array
can be resized to zero, which effectively resets the variable.

:p.
At all times, trying to access an element of the array with an index 
that is not in the current length of the array will generate a run-time 
error.

:p.
Dynamic arrays are reference counted: assignment of one dynamic array-type 
variable to another will let both variables point to the same array. 
Contrary to ansistrings, an assignment to an element of one array will 
be reflected in the other: there is no copy-on-write. Consider the following
example:

:xmp.
var
  A, B: TByteArray;
begin
  SetLength(A, 10);
  A[0] := 33;
  B := A;
  A[0] := 31;
:exmp.

:p.
After the second assignment, the first element in B will also contain 31.

:p.
It can also be seen from the output of the following example:

:xmp.
program testarray1;

type
  TA = array of array of Integer;
  
var   
  A, B: TA;
  I, J: Integer;
begin
  Setlength(A, 10, 10);
  for I := 0 to 9 do
    for J := 0 to 9 do 
      A[I, J] := I * J;
  for I:=0 to 9 do
  begin
    for J := 0 to 9 do 
      Write(A[I, J]:2, ' ');
    writeln;
  end;
  B := A;
  writeln;
  for I := 0 to 9 do
    for J := 0 to 9 do 
      A[9-I, 9-J] := I * J;
  for I := 0 to 9 do
  begin
    for J := 0 to 9 do 
      Write(B[I, J]:2, ' ');
    writeln;
  end;
end.  
:exmp.

:p.
The output of this program will be a matrix of numbers, and then the same
matrix, mirrorred.

:p.
As remarked earlier, dynamic arrays are reference counted: if in one of the previous examples A
goes out of scope and B does not, then the array is not yet disposed of: the
reference count of A (and B) is decreased with 1. As soon as the reference
count reaches zero the memory, allocated for the contents of the array, is disposed of.

:p.
It is also possible to copy and/or resize the array with the standard 
:hp1.Copy():ehp1. function, which acts as the copy function for strings:

:xmp.
program testarray3;

type
  TA = array of Integer;
  
var   
  A, B: TA;
  I: Integer;

begin
  Setlength(A, 10);
  for I := 0 to 9 do
      A[I] := I;
  B := Copy(A, 3, 6);    
  for I := 0 to 5 do
    Writeln(B[I]);
end.  
:exmp.

:p.
The :hp1.Copy():ehp1. function will copy 6 elements of the array to a new array.
Starting at the element at index 3 (i.e. the fourth element) of the array.

:p.
The :hp1.Length():ehp1. function will return the number of elements in the array.
The :hp1.Low():ehp1. function on a dynamic array will always return 0, and the
:hp1.High():ehp1. function will return the value :hp1.Length-1:ehp1., i.e., the value of the
highest allowed array index. 



:h5.Packing and unpacking an array
:p.
Arrays can be packed and bitpacked. Two array types which have the same index
type and element type, but which are differently packed are not assignment 
compatible.

:p.
However, it is possible to convert a normal array to a bitpacked array with the
:hp1.pack:ehp1. routine. The reverse operation is possible as well; a bitpacked
array can be converted to a normally packed array using the :hp1.unpack:ehp1.
routine, as in the following example:

:xmp.
var
  foo: array [ 'a'..'f' ] of Boolean 
    = ( false, false, true, false, false, false );
  bar: packed array [ 42..47 ] of Boolean;
  baz: array [ '0'..'5' ] of Boolean;

begin
  pack(foo,'a',bar);
  unpack(bar,baz,'0');
end.
:exmp.

:p.
More information about the pack and unpack routines can be found in the
:hp2.system:ehp2. unit reference.


:h4 name='record_types'.Record types
:p.
&fpc. supports fixed records and records with variant parts.
The syntax diagram for a record type is:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Record types:ehp2.

&ra.&ra.── record type ─┬───────────────┬─ :hp2.record:ehp2. ─┬──────────────┬─ :hp2.end:ehp2. ───────────&ra.&la.
                  ├─   :hp2.packed:ehp2.    ─┤          └─ field list ─┘
                  └─  :hp2.bitpacked:ehp2.  ─┘

&ra.&ra.── field list ─┬─── fixed fields ───────────────────────┬─┬─────┬───────────&ra.&la.
                 └─┬─────────────────────┬─ variant part ─┘ └─ ; ─┘
                   └─  fixed fields ─ ; ─┘

&ra.&ra.── fixed fields ─┬─ identifier list ─ : ─ type ─┬───────────────────────────&ra.&la.
                   └──────────── ; ───────────────┘

&ra.&ra.── variant part ─ :hp2.case:ehp2. ─┬────────────────┬─ ordinal type identifier ─ :hp2.of:ehp2. ─┬─ variant─┬───&ra.&la.
                          └ identifier ─ : ┘                                ^──── ; ───┘

&ra.&ra.── variant ─┬─ constant ─ , ─┬─ : ─ ( ─┬────────────────┬─ ) ───────────────&ra.&la.
              ^────────────────┘         ^── field list ──┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
So the following are valid record type declarations:

:xmp.
type
  Point = record
    X, Y, Z: Real;
  end;

  RPoint = record
    case Boolean of
      False: (X, Y, Z: Real);
      True: (R, theta, phi: Real);
  end;

  BetterRPoint = record
    case UsePolar: Boolean of
      False: (X, Y, Z: Real);
      True: (R, theta, phi: Real);
  end;
:exmp.


.* :fn id=record1.
.* However, it is up to the programmer to maintain this field.
.* :efn.

:p.
The variant part must be last in the record. The optional identifier in the
case statement serves to access the tag field value, which otherwise would
be invisible to the programmer. It can be used to see which variant is
active at a certain time [However, it is up to the programmer to maintain this field].
.* :link refid=record1 reftype=fn.(footnote here):elink.].
In effect, it introduces a new field in the record.

:note.
It is possible to nest variant parts, as in&colon.

:xmp.
type
  MyRec = Record
    X: Longint;
    case byte of
      2: (Y: Longint;
      case byte of
        3: (Z: Longint);
      );
  end;
:exmp.

:p.
By default the size of a record is the sum of the sizes of its fields, each size of a
field is rounded up to a power of two. If the record contains a variant part, the size
of the variant part is the size of the biggest variant, plus the size of the
tag field type :hp1.if an identifier was declared for it:ehp1.. Here also, the size of
each part is first rounded up to two. So in the above example:
:ul.
:li. :hp1.SizeOf():ehp1. would return 24 for :hp1.Point:ehp1.
:li. It would result in 24 for :hp1.RPoint}:ehp1.
:li. Finally, 26 would be the size of :hp1.BetterRPoint:ehp1. 
:li. For :hp1.MyRec:ehp1., the value would be 12.
:eul.

:p.
If a typed file with records, produced by a &tp. program, must be read,
then chances are that attempting to read that file correctly will fail.
The reason for this is that by default, elements of a record are aligned at
2-byte boundaries, for performance reasons. 
:p.
This default behaviour can be changed with the :hp1.{$PACKRECORDS N}:ehp1. 
switch. Possible values for :hp1.N:ehp1. are 1, 2, 4, 16 or :hp1.Default:ehp1.
This switch tells the compiler to align elements of a record or object or
class that have size larger than :hp1.n:ehp1. on :hp1.n:ehp1. byte boundaries.
:p.
Elements that have size smaller or equal than :hp1.n:ehp1. are aligned on
natural boundaries, i.e. to the first power of two that is larger than or
equal to the size of the record element.
:p.
The keyword :hp1.Default:ehp1. selects the default value for the platform
that the code is compiled for (currently, this is 2 on all platforms)
Take a look at the following program:

:xmp.
Program PackRecordsDemo;

type
    {$PackRecords 2}
    Trec1 = Record
      A : byte;
      B : Word;
    end;

    {$PackRecords 1}
    Trec2 = Record
      A : Byte;
      B : Word;
    end;

    {$PackRecords 2}
    Trec3 = Record
      A,B : byte;
    end;

    {$PackRecords 1}
    Trec4 = Record
      A,B : Byte;
    end;
   
    {$PackRecords 4}
    Trec5 = Record
      A : Byte;
      B : Array[1..3] of byte;
      C : byte;
    end;

    {$PackRecords 8}
    Trec6 = Record
      A : Byte;
      B : Array[1..3] of byte;
      C : byte;
    end;
   
    {$PackRecords 4}
    Trec7 = Record
      A : Byte;
      B : Array[1..7] of byte;
      C : byte;
    end;

    {$PackRecords 8}
    Trec8 = Record
      A : Byte;
      B : Array[1..7] of byte;
      C : byte;
    end;

var
  rec1 : TRec1;
  rec2 : TRec2;
  rec3 : TRec3;
  rec4 : TRec4;
  rec5 : TRec5;
  rec6 : TRec6;
  rec7 : TRec7;
  rec8 : TRec8;

begin
  Write ('Size TRec1 : ',SizeOf(Trec1));
  Writeln (' Offset B : ',Longint(@rec1.B)-Longint(@rec1));
  Write ('Size TRec2 : ',SizeOf(Trec2));
  Writeln (' Offset B : ',Longint(@rec2.B)-Longint(@rec2));
  Write ('Size TRec3 : ',SizeOf(Trec3));
  Writeln (' Offset B : ',Longint(@rec3.B)-Longint(@rec3));
  Write ('Size TRec4 : ',SizeOf(Trec4));
  Writeln (' Offset B : ',Longint(@rec4.B)-Longint(@rec4));
  Write ('Size TRec5 : ',SizeOf(Trec5));
  Writeln (' Offset B : ',Longint(@rec5.B)-Longint(@rec5),
           ' Offset C : ',Longint(@rec5.C)-Longint(@rec5));
  Write ('Size TRec6 : ',SizeOf(Trec6));
  Writeln (' Offset B : ',Longint(@rec6.B)-Longint(@rec6),
           ' Offset C : ',Longint(@rec6.C)-Longint(@rec6));
  Write ('Size TRec7 : ',SizeOf(Trec7));
  Writeln (' Offset B : ',Longint(@rec7.B)-Longint(@rec7),
           ' Offset C : ',Longint(@rec7.C)-Longint(@rec7));
  Write ('Size TRec8 : ',SizeOf(Trec8));
  Writeln (' Offset B : ',Longint(@rec8.B)-Longint(@rec8),
           ' Offset C : ',Longint(@rec8.C)-Longint(@rec8));
end.
:exmp.

:p.
The output of this program will be:

:xmp.
Size TRec1 : 4 Offset B : 2
Size TRec2 : 3 Offset B : 1
Size TRec3 : 2 Offset B : 1
Size TRec4 : 2 Offset B : 1
Size TRec5 : 8 Offset B : 4 Offset C : 7
Size TRec6 : 8 Offset B : 4 Offset C : 7
Size TRec7 : 12 Offset B : 4 Offset C : 11
Size TRec8 : 16 Offset B : 8 Offset C : 15
:exmp.

:p.
And this is as expected:

:ul.
:li. In Trec1, since B has size 2, it is aligned on a 2 byte boundary, thus leaving an empty byte
between A and B, and making the total size 4. In Trec2, B is aligned on a 1-byte boundary,
right after A, hence, the total size of the record is 3.

:li.For Trec3, the sizes of A,B are 1, and hence they are aligned on 1 byte boundaries. The same
is true for Trec4.

:li.For Trec5, since the size of B – 3 – is smaller than 4, B will be on a 4-byte boundary, as this
is the first power of two that is larger than its size. The same holds for Trec6.

:li.For Trec7, B is aligned on a 4 byte boundary, since its size – 7 – is larger than 4. However, in
Trec8, it is aligned on a 8-byte boundary, since 8 is the first power of two that is greater than
7, thus making the total size of the record 16.
:eul.

:p.
&fpc. supports also the 'packed record', this is a record where all the elements are byte-aligned.
Thus the two following declarations are equivalent:

:xmp.
     {$PackRecords 1}
     Trec2 = Record
       A : Byte;
       B : Word;
     end;
     {$PackRecords 2}
:exmp.

:p.
and

:xmp.
     Trec2 = Packed Record
       A : Byte;
       B : Word;
     end;
:exmp.

:p.
Note the :hp1.{$PackRecords 2}:ehp1. after the first declaration!



:h4 name='set_types'.Set types
:p.
&fpc. supports the set types as in &tp.. The prototype of a set declaration is:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Set types:ehp2.

&ra.&ra.── set type ── :hp2.set:ehp2. ── :hp2.of:ehp2. ── ordinal type ───────────────────────────────────&ra.&la.

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
Each of the elements of :hp1.SetType:ehp1. must be of type :hp1.TargetType:ehp1..
:hp1.TargetType:ehp1. can be any ordinal
type with a range between 0 and 255. A set can contain at most 255 elements.
The following are valid set declaration:

:xmp.
type
  Junk = set of Char;

  Days = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
  WorkDays: set of Days;
:exmp.

:p.
Given these declarations, the following assignment is legal:

:xmp.
WorkDays := [Mon, Tue, Wed, Thu, Fri];
:exmp.

:p.
The compiler stores small sets (less than 32 elements) in a Longint, if the
type range allows it. This
allows for faster processing and decreases program size. Otherwise, sets
are stored in 32 bytes.
:p.
Several operations can be done on sets: taking unions or differences, adding
or removing elements,
comparisons. These are documented in :link refid='set_operators' reftype=hd.set operators:elink..



:h4 name='file_types'.File types
:p.
File types are types that store a sequence of some base type, which can be any type except another file
type. It can contain (in principle) an infinite number of elements. File types are used commonly to
store data on disk. However, nothing prevents the programmer, from writing a file driver that stores
its data for instance in memory.
:p.
Here is the type declaration for a file type:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.File types:ehp2.

&ra.&ra.── file type ── :hp2.file:ehp2. ─┬─────────────┬───────────────────────────────────────&ra.&la.
                        └─ :hp2.of:ehp2. ─ type ─┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
If no type identifier is given, then the file is an untyped file; it can be
considered as equivalent to a file
of bytes. Untyped files require special commands to act on them
(see :hp1.Blockread:ehp1., :hp1.Blockwrite:ehp1.).
The following declaration declares a file of records:

:xmp.
type
  Point = Record
     X,Y,Z : real;
     end;
  PointFile = File of Point;
:exmp.

:p.
Internally, files are represented by the :hp1.FileRec:ehp1. record, which is declared
in the Dos or SysUtils units.
:p.
A special file type is the :hp1.Text:ehp1. file type, represented by the :hp1.TextRec:ehp1. record.
A file of type :hp1.Text:ehp1. uses special input-output routines. The default :hp1.Input:ehp1.,
:hp1.Output:ehp1. and :hp1.StdErr:ehp1. file types are defined in the system unit: they are all of
type :hp1.Text:ehp1., and are opened by the system unit initialization code.




.* --------------------------------------------------------------
:h3 name=pointer_types.Pointers
:p.
&fpc. supports the use of pointers. A variable of the pointer type contains an
address in memory, where the data of another variable may be stored. A pointer
type can be defined as follows:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Pointer types:ehp2.

&ra.&ra.── pointer type ── ^ ── type identifier ────────────────────────────────────&ra.&la.

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
As can be seen from this diagram, pointers are typed, which means that they point to a particular
kind of data. The type of this data must be known at compile time.
:p.
Dereferencing the pointer (denoted by adding ^ after the variable name) behaves then like a variable.
This variable has the type declared in the pointer declaration, and the variable is stored in the address
that is pointed to by the pointer variable. Consider the following example:

:xmp.
program pointers;

type
  Buffer = String[255];
  BufPtr = ^Buffer;

var
  B: Buffer;
  BP: BufPtr;
  PP: Pointer;
etc...
:exmp.

:p.
In this example, BP :hp1.is a pointer to:ehp1. a Buffer type; while B is a variable of type Buffer. B takes
256 bytes memory, and BP only takes 4 (or 8) bytes of memory: enough memory to store an address.
:p.
The expression

:xmp.
BP^
:exmp.

:p.
is known as the dereferencing of BP. The result is of type Buffer, so

:xmp.
BP^[23]
:exmp.

:p.
Denotes the 23-rd character in the string pointed to by BP.

:nt. &fpc. treats pointers much the same way as C does. This means that a pointer to some type
can be treated as being an array of this type.
:p.
From this point of view, the pointer then points to the zeroeth element of this
array. Thus the following pointer declaration

:xmp.
var
  p: ^Longint;
:exmp.

:p.
can be considered equivalent to the following array declaration:

:xmp.
var
  p: array[0..Infinity] of Longint;
:exmp.

:p.
The difference is that the former declaration allocates memory for the pointer only (not for the array),
and the second declaration allocates memory for the entire array. If the former is used, the memory
must be allocated manually, using the :hp1.Getmem:ehp1. function. The reference P^ is then the same as p[0].
The following program illustrates this maybe more clear:

:xmp.
program PointerArray;
var
  i: Longint;
  p: ^Longint;
  pp: array[0..100] of Longint;
begin
  for i := 0 to 100 do pp[i] := i; { Fill array }
  p := @pp[0];                     { Let p point to pp }
  for i := 0 to 100 do
    if p[i] <> pp[i] then
      WriteLn ('Ohoh, problem !')
end.
:exmp.:ent.

:p.
&fpc. supports pointer arithmetic as C does. This means that, if P is a typed pointer, the
instructions

:xmp.
Inc(P);
Dec(P);
:exmp.

:p.
Will increase, respectively decrease the address the pointer points to with the size of the type P is a
pointer to. For example

:xmp.
var
  P: ^Longint;
&dot.&dot.&dot.
 Inc(p);
:exmp.

:p.
will increase P with 4, because 4 is the size of a longint. If the pointer is untyped, a size of 1 byte is
assumed (i.e. as if the pointer were a pointer to a byte: ^byte.)
:p.
Normal arithmetic operators on pointers can also be used, that is, the following are valid pointer
arithmetic operations:

:xmp.
var
  p1, p2: ^Longint;
  L: Longint;
begin
  P1 := @P2;
  P2 := @L;
  L  := P1-P2;
  P1 := P1-4;
  P2 := P2+4;
end.
:exmp.

:p.
Here, the value that is added or substracted is multiplied by the size of the type the pointer points to.
In the previous example P1 will be decremented by 16 bytes, and P2 will be incremented by 16.



.* --------------------------------------------------------------
:h3 name=forward_type_declarations.Forward type declarations
:p.
Programs often need to maintain a linked list of records. Each record then contains a pointer to the
next record (and possibly to the previous record as well). For type safety, it is best to define this
pointer as a typed pointer, so the next record can be allocated on the heap using the New call. In
order to do so, the record should be defined something like this:

:xmp.
type
  TListItem = Record
     Data: Integer;
     Next: ^TListItem;
  end;
:exmp.

:p.
When trying to compile this, the compiler will complain that the TListItem type is not yet defined
when it encounters the :hp1.Next:ehp1. declaration: This is correct, as the definition is still being parsed.
:p.
To be able to have the Next element as a typed pointer, a 'Forward type declaration'
must be introduced:

:xmp.
type
  PListItem = ^TListItem;
  TListItem = Record
  Data : Integer;
  Next : PTListItem;
end;
:exmp.

:p.
When the compiler encounters a typed pointer declaration where the referenced type is not yet known,
it postpones resolving the reference till later. The pointer definition is a 'Forward type declaration'.
:p.
The referenced type should be introduced later in the same Type block. No other block may come
between the definition of the pointer type and the referenced type. Indeed, even the word Type
itself may not re-appear: in effect it would start a new type-block, causing the compiler to resolve all
pending declarations in the current block.
:p.
In most cases, the definition of the referenced type will follow immediatly after the definition of
the pointer type, as shown in the above listing. The forward defined type can be used in any type
definition following its declaration.
:p.
Note that a forward type declaration is only possible with pointer types and classes, not with other
types.



.* --------------------------------------------------------------
:h3 name=procedural_types.Procedural types
:p.
&fpc. has support for procedural types, although it differs a little from the &tp. or
&delphi. implementation of them. The type declaration remains the same, as can be seen in the following
syntax diagram:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Procedural types:ehp2.

&ra.&ra.── procedural type ─┬─ function header ──┬┬───────────────┬┬─────────────────────┬──────&ra.&la.
                      └─ procedure header ─┘└─ :hp2.of:ehp2. ─ :hp2.object:ehp2. ─┘└─ ; ─ call modifier ─┘

&ra.&ra.── function header ── :hp2.function:ehp2. ─ formal parameter list ─ : ─ result type ───────────────&ra.&la.

&ra.&ra.── procedure header ── :hp2.procedure:ehp2. ─ formal parameter list ───────────────────────────────&ra.&la.

&ra.&ra.── call modifiers ─┬─ :hp2.register:ehp2. ─┬───────────────────────────────────────────────────────&ra.&la.
                     ├─  :hp2.cdecl:ehp2.   ─┤
                     ├─  :hp2.pascal:ehp2.  ─┤
                     ├─ :hp2.stdcall:ehp2.  ─┤
                     ├─ :hp2.safecall:ehp2. ─┤
                     └─  :hp2.inline:ehp2.  ─┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
For a description of formal parameter lists, see :link refid='procedure_declarations' reftype=hd. Procedure declarations:elink..
The two following examples are valid type declarations:

:xmp.
type
  TOneArg = Procedure (Var X : integer);
  TNoArg = Function : Real;
var
  proc : TOneArg;
  func : TNoArg;
:exmp.

:p.
One can assign the following values to a procedural type variable:

:ol.
:li. Nil, for both normal procedure pointers and method pointers.
:li. A variable reference of a procedural type, i.e. another variable of the same type.
:li. A global procedure or function address, with matching function or procedure header
and calling convention.
:li. A method address.
:eol.

:p.
Given these declarations, the following assignments are valid:

:xmp.
Procedure printit (Var X : Integer);
begin
  WriteLn (x);
end;
&dot.&dot.&dot.
Proc := @printit;
Func := @Pi;
:exmp.

:p.
From this example, the difference with &tp. is clear: In &tp. it isn't necessary to
use the address operator (@) when assigning a procedural type variable, whereas
in &fpc. it is required. In case the :hp1.-MDelphi:ehp1. or :hp1.-MTP:ehp1. switches
are used, the address operator can be dropped.

:nt.
The modifiers concerning the calling conventions must be the same as the
declaration; i.e. the following code would give an error:

:xmp.
type
  TOneArgCcall = procedure(Var X: integer); cdecl;

var
  proc: TOneArgCcall;

procedure printit(Var X : Integer);
begin
  WriteLn (x);
end;

begin
  Proc := @printit;
end.
:exmp.

Because the TOneArgCcall type is a procedure that uses the cdecl calling convention.
:ent.



.* --------------------------------------------------------------
:h3 name=variant_types.Variant types
:ul.
:li.:link refid='variant_types_definition' reftype='hd'.Definition:elink.
:li.:link refid='variant_types_inassignment' reftype='hd'.Variants in assignments and expressions:elink.
:li.:link refid='variant_types_andinterfaces' reftype='hd'.Variants and interfaces:elink.
:eul.

.* ...............................................................
:h4 name='variant_types_definition'.Definition
:p.
As of version 1.1, &fpc. has support for variants. For maximum variant support it is recommended to
add the variants unit to the uses clause of every unit that uses variants in some way: the variants unit
contains support for examining and transforming variants other than the default support offered by
the :hp1.System:ehp1. or :hp1.ObjPas:ehp1. units.
:p.
The type of a value stored in a variant is only determined at runtime: it depends what has been
assigned to the to the variant. Almost any simple type can be assigned to variants: ordinal types,
string types, int64 types.
:p.
Structured types such as sets, records, arrays, files, objects and classes are not assignment-compatible
with a variant, as well as pointers. Interfaces and COM or CORBA objects can be assigned to a
variant (basically because they are simply a pointer).
:p.
This means that the following assignments are valid:

:xmp.
type
  TMyEnum = (One, Two, Three);

var
  V: Variant;
  I: Integer;
  B: Byte;
  W: Word;
  Q: Int64;
  E: Extended;
  D: Double;
  En: TMyEnum;
  AS: AnsiString;
  WS: WideString;

begin
  V := I;
  V := B;
  V := W;
  V := Q;
  V := E;
  V := En;
  V := D:
  V := AS;
  V := WS;
end;
:exmp.

:p.
And of course vice-versa as well.
:p.
A variant can hold an an array of values: All elements in the array have the same type (but can be of
type 'variant'). For a variant that contains an array, the variant can be indexed:

:xmp.
program testv;

uses
  Variants;

var
  A: variant;
  I: integer;
begin
  A := VarArrayCreate([1, 10], varInteger);
  For I := 1 to 10 do
    A[I] := I;
end.
:exmp.

:p.
For the explanation of :hp1.VarArrayCreate:ehp1., see Unit Reference.
:p.
Note that when the array contains a string, this is not considered an 'array of characters', and so the
variant cannot be indexed to retrieve a character at a certain position in the string.



.* ..................................................................
:h4 name='variant_types_inassignment'.Variants in assignments and expressions
:p.
As can be seen from the definition above, most simple types can be assigned to a variant. Likewise,
a variant can be assigned to a simple type: If possible, the value of the variant will be converted to
the type that is being assigned to. This may fail: Assigning a variant containing a string to an integer
will fail unless the string represents a valid integer. In the following example, the first assignment
will work, the second will fail:

:xmp.
program testv3;

uses
  Variants;

var
  V: Variant;
  I: Integer;

begin
  V := '100';
  I := V;
  writeln('I : ', I);
  V := 'Something else';
  I := V;
  writeln('I : ', I);
end.
:exmp.

:p.
The first assignment will work, but the second will not, as :hp1.Something else:ehp1. cannot be converted
to a valid integer value. An EConvertError exception will be the result.
:p.
The result of an expression involving a variant will be of type variant again, but this can be assigned
to a variable of a different type - if the result can be converted to a variable of this type.
:p.
Note that expressions involving variants take more time to be evaluated, and should therefore be used
with caution. If a lot of calculations need to be made, it is best to avoid the use of variants.
:p.
When considering implicit type conversions (e.g. byte to integer, integer to double, char to string)
the compiler will ignore variants unless a variant appears explicitly in the expression.




.* ..................................................................
:h4 name='variant_types_andinterfaces'.Variants and interfaces
:p.
:note.Dispatch interface support for variants is currently broken in the compiler.
:p.
Variants can contain a reference to an interface - a normal interface (descending from IInterface)
or a dispatchinterface (descending from IDispatch). Variants containing a reference to a dispatch
interface can be used to control the object behind it: the compiler will use late binding to perform
the call to the dispatch interface: there will be no run-time checking of the function names and
parameters or arguments given to the functions. The result type is also not checked. The compiler
will simply insert code to make the dispatch call and retrieve the result.
:p.
This means basically, that you can do the following on Windows:

:xmp.
var
  W: Variant;
  V: String;
begin
  W := CreateOleObject('Word.Application');
  V := W.Application.Version;
  Writeln('Installed version of MS Word is : ', V);
end;
:exmp.

:p.
The line

:xmp.
V := W.Application.Version;
:exmp.

:p.
is executed by inserting the necessary code to query the dispatch interface stored in the variant W, and
execute the call if the needed dispatch information is found.



.* ==============================================================
:h2 name=variables.Variables
:p.
:ul.
:li.:link refid='variables_definition' reftype='hd'.Definition:elink.
:li.:link refid='variables_declaration' reftype='hd'.Declaration:elink.
:li.:link refid='variables_scope' reftype='hd'.Scope:elink.
:li.:link refid='variables_initializedvars' reftype='hd'.Initialized variables:elink.
:li.:link refid='variables_threadvars' reftype='hd'.Thread variables:elink.
:li.:link refid='variables_properties' reftype='hd'.Properties:elink.
:eul.

:h3 name='variables_definition'.Definition
:p.
Variables are explicitly named memory locations with a certain type. When assigning values to
variables, the Free Pascal compiler generates machine code to move the value to the memory location
reserved for this variable. Where this variable is stored depends on where it is declared:

:ul.
:li.Global variables are variables declared in a unit or program, but not inside a procedure or
function. They are stored in fixed memory locations, and are available during the whole execution
time of the program.
:li.Local variables are declared inside a procedure or function. Their value is stored on the
program stack, i.e. not at fixed locations.
:eul.

:p.
The &fpc. compiler handles the allocation of these memory locations transparantly, although
this location can be influenced in the declaration.
:p.
The &fpc. compiler also handles reading values from or writing values to the variables transparantly.
But even this can be explicitly handled by the programmer when using properties.
:p.
Variables must be explicitly declared when they are needed. No memory is allocated unless a variable
is declared. Using an variable identifier (for instance, a loop variable) which is not declared first, is
an error which will be reported by the compiler.



:h3 name='variables_declaration'.Declaration
:p.
The variables must be declared in a variable declaration section of a unit or a procedure or function.
It looks as follows:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Variable declaration:ehp2.

&ra.&ra.── variable declaration  ── identifier ── : ── type ─┬──────────────────┬──&ra.
                                                       └─ :hp2.=:ehp2. ─ expression ─┘

&ra.───┬──────────────────────┬─ hintdirective ── :hp2.;:ehp2. ─────────────────────────────&ra.&la.
    └─ variable modifiers ─┘

&ra.&ra.── variable modifiers ─┬─┬───── :hp2.absolute:ehp2. ─┬─ integer expression ─┬───────────────────────┬┬─&ra.
                         ^ │                └─     identifier     ─┘                       ││
                         │ ├────────────────────── :hp2.; export:ehp2. ───────────────────────────────┤│
                         │ ├────────────────────── :hp2.; cvar:ehp2. ─────────────────────────────────┤│
                         │ ├─ :hp2.; external:ehp2. ─┬───────────────────┬┬──────────────────────────┬┤│
                         │ │              └─ string constant ─┘└─ :hp2.name:ehp2. ─ string constant ─┘││
                         │ └────────────────────── hintdirective ──────────────────────────┘│
                         └──────────────────────────────────────────────────────────────────┘

&ra.─────────────────────────────────────────────────────────────────────────────&ra.&la.

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
This means that the following are valid variable declarations:

:xmp.
var
  curterm1: integer;

  curterm2: integer; cvar;
  curterm3: integer; cvar; external;

  curterm4: integer; external name 'curterm3';
  curterm5: integer; external 'libc' name 'curterm9';

  curterm6: integer absolute curterm1;

  curterm7: integer; cvar; export;
  curterm8: integer; cvar; public;
  curterm9: integer; export name 'me';
  curterm10: integer; public name 'ma';

  curterm11: integer = 1;
:exmp.

:p.
The difference between these declarations is as follows:

:ol.
:li.The first form (curterm1) defines a regular variable. The compiler manages everything by
itself.
:li.The second form (curterm2) declares also a regular variable, but specifies that the assembler
name for this variable equals the name of the variable as written in the source.
:li.The third form (curterm3) declares a variable which is located externally: the compiler will
assume memory is located elsewhere, and that the assembler name of this location is specified
by the name of the variable, as written in the source. The name may not be specified.
:li.The fourth form is completely equivalent to the third, it declares a variable which is stored
externally, and explicitly gives the assembler name of the location. If cvar is not used, the
name must be specified.
:li.The fifth form is a variant of the fourth form, only the name of the library in which the memory
is reserved is specified as well.
:li.The sixth form declares a variable (curterm6), and tells the compiler that it is stored in the
same location as another variable (curterm1).
:li.The seventh form declares a variable (curterm7), and tells the compiler that the assembler
label of this variable should be the name of the variable (case sensitive) and must be made
public. i.e. it can be referenced from other object files.
:li.The eighth form (curterm8) is equivalent to the seventh: 'public' is an alias for 'export'.
:li.The ninth and tenth form are equivalent: they specify the assembler name of the variable.
:li.The eleventh form declares a variable (curterm11) and initializes it with a value (1 in the
above case).
:eol.

:p.
Note that assembler names must be unique. It’s not possible to declare or export 2 variables with the
same assembler name.


:h3 name='variables_scope'.Scope
:p.
Variables, just as any identifier, obey the general rules of scope. In addition,
initialized variables are initialized when they enter scope:

:ul.
:li.Global initialized variables are initialized once, when the program starts.
:li.Local initialized variables are initialized each time the procedure is entered.
:eul.

:p.
Note that the behaviour for local initialized variables is different from the one
of a local typed constant. A local typed constant behaves like a global initialized
variable.


:h3 name='variables_initializedvars'.Initialized variables
:p.
By default, variables in Pascal are not initialized after their declaration. Any assumption that they
contain 0 or any other default value is erroneous: They can contain rubbish. To remedy this, the
concept of initialized variables exists. The difference with normal variables is that their declaration
includes an initial value, as can be seen in the diagram in the previous section.
:p.
Given the declaration:

:xmp.
var
  S: String = 'This is an initialized string';
:exmp.

:p.
The value of the variable following will be initialized with the provided value. The following is an
even better way of doing this:

:xmp.
const
  SDefault = 'This is an initialized string';
  
var
  S: String = SDefault;
:exmp.

:p.
Initialization is often used to initialize arrays and records. For arrays, the initialized elements must
be specified, surrounded by round brackets, and separated by commas. The number of initialized
elements must be exactly the same as the number of elements in the declaration of the type. As an
example:

:xmp.
var
  tt: array [1..3] of string[20] = ('ikke', 'gij', 'hij');
  ti: array [1..3] of Longint = (1,2,3);
:exmp.

:p.
For constant records, each element of the record should be specified, in
the form :hp1.Field: Value:ehp1., separated by semicolons, and surrounded by round
brackets.

:xmp.
type
  Point = record
    X, Y: Real
  end;
var
  Origin: Point = (X:0.0; Y:0.0);
:exmp.

:p.
The order of the fields in a constant record needs to be the same as in the type declaration, otherwise
a compile-time error will occur.

:nt.
It should be stressed that initialized variables are initialized when they come into scope, in difference
with typed constants, which are initialized at program start. This is also true for local initialized
variables. Local initialized are initialized whenever the routine is called. Any changes that occurred
in the previous invocation of the routine will be undone, because they are again initialized.
:ent.


:h3 name='variables_threadvars'.Thread variables
:p.
For a program which uses threads, the variables can be really global, i.e. the same for all threads, or
thread-local: this means that each thread gets a copy of the variable. Local variables (defined inside
a procedure) are always thread-local. Global variables are normally the same for all threads. A
global variable can be declared thread-local by replacing the :hp1.var:ehp1. keyword at the start of the variable
declaration block with :hp1.Threadvar:ehp1.:

:xmp.
Threadvar
  IOResult: Integer;
:exmp.

:p.
If no threads are used, the variable behaves as an ordinary variable. If threads are used then a copy is
made for each thread (including the main thread). Note that the copy is made with the original value
of the variable, :hp1.not:ehp1. with the value of the variable at the time the thread is started.
:p.
Threadvars should be used sparingly: There is an overhead for retrieving or setting the variable's
value. If possible at all, consider using local variables; they are always faster than thread variables.
:p.
Threads are not enabled by default. For more information about programming threads, see the chapter
on threads in the &progref..


:h3 name='variables_properties'.Properties
:p.
A global block can declare properties, just as they could be defined in a class. The difference is that
the global property does not need a class instance: there is only 1 instance of this property. Other
than that, a global property behaves like a class property. The read/write specifiers for the global
property must also be regular procedures, not methods.
:p.
The concept of a global property is specific to &fpc., and does not exist in &delphi.. :hp1.ObjFPC:ehp1.
mode is required to work with properties.
:p.
The concept of a global property can be used to 'hide' the location of the value, or to calculate the
value on the fly, or to check the values which are written to the property.
:p.
The declaration is as follows:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Properties:ehp2.

&ra.&ra.── property definition  ── identifier ─┬──────────────────────┬─ property specifiers ──────&ra.&la.
                                         └─ property interface ─┘

&ra.&ra.── property interface ─┬───────────────────────────┬─ : ── type identifier ──&ra.
                         └─ property parameter list ─┘

&ra.───┬───────────────────────────┬─────────────────────────────────────────────&ra.&la.
    └─ :hp2.index:ehp2. ─ integerconstant ─┘

&ra.&ra.── property parameter list ── :hp2.[:ehp2. ─┬─ parameter declaration ─┬─ :hp2.]:ehp2. ────────────&ra.&la.
                                   ^─────────── ; ───────────┘

&ra.&ra.── property specifiers ─┬──────────────────┬┬───────────────────┬┬─────────────────────┬───&ra.&la.
                          └─ read specifier ─┘└─ write specifier ─┘└─ default specifier ─┘

&ra.&ra.── read specifier ── :hp2.read:ehp2. ── field or function ─────────────────────────────&ra.&la.

&ra.&ra.── write specifier ── :hp2.write:ehp2. ── field or function ───────────────────────────&ra.&la.

&ra.&ra.── default specifier ──┬─ :hp2.default:ehp2. ──┬────────────┬┬─────────────────────────&ra.&la.
                         │            └─ constant ─┘│
                         └──────── :hp2.nodefault:ehp2. ───────┘                      

&ra.&ra.── field or procedure ──┬─── field identifier ───┬──────────────────────────&ra.&la.
                          └─ procedure identifier ─┘

&ra.&ra.── field or function ──┬─── field identifier ───┬───────────────────────────&ra.&la.
                         └─ function identifier ──┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
The following is an example:

:xmp.
unit testprop;

{$mode objfpc}{$H+}

interface

function GetMyInt: Integer;
procedure SetMyInt(AValue: Integer);

property MyProp: Integer read GetMyInt write SetMyInt;
  
implementation

uses
  sysutils;

var
  FMyInt: Integer;  
  
function GetMyInt: Integer;
begin
  Result := FMyInt;
end;

procedure SetMyInt(AValue: Integer);
begin
  if ((AValue mod 2) = 1) then
    raise Exception.Create('MyProp can only contain even value');
  FMyInt := AValue;  
end;

end.
:exmp.

:p.
The read/write specifiers can be hidden by declaring them in another unit which must be in the :hp1.uses:ehp1.
clause of the unit. This can be used to hide the read/write access specifiers for programmers, just as
if they were in a private section of a class (discussed below). For the previous example, this could
look as follows:

:xmp.
unit testrw;

{$mode objfpc}{$H+}

interface

function GetMyInt: Integer;
procedure SetMyInt(AValue: Integer);

implementation

uses
  sysutils;

var
  FMyInt: Integer;  
  
function GetMyInt: Integer;
begin
  Result := FMyInt;
end;

procedure SetMyInt(AValue: Integer);
begin
  If ((AValue mod 2) = 1) then
    Raise Exception.Create('Only even values are allowed');
  FMyInt := AValue;  
end;

end.
:exmp.

:p.
The unit testprop would then look like:

:xmp.
unit testprop;

{$mode objfpc}{$H+}

interface

uses
  testrw;

property MyProp: Integer read GetMyInt write SetMyInt;

implementation

end.  
:exmp.

:p.
More information about properties can be found in the :link refid='classes' reftype='hd'.Classes chapter:elink..




.* ==============================================================
:h2 name=objects.Objects
:p.
:ul.
:li.:link refid='objects_declaration' reftype='hd'.Declaration:elink.
:li.:link refid='objects_fields' reftype='hd'.Fields:elink.
:li.:link refid='objects_staticfields' reftype='hd'.Static Fields:elink.
:li.:link refid='objects_constructordestructor' reftype='hd'.Constructors and Destructors:elink.
:li.:link refid='objects_methods' reftype='hd'.Methods:elink.
:li.:link refid='objects_visibility' reftype='hd'.Visibility:elink.
:eul.

:h3 name='objects_declaration'.Declaration
:p.
&fpc. supports object oriented programming. In fact, most of the compiler is written using
objects. Here we present some technical questions regarding object oriented programming in &fpc.

:p.
Objects should be treated as a special kind of record. The record contains all the fields that are
declared in the objects definition, and pointers to the methods that are associated to the objects’ type.

:p.
An object is declared just as a record would be declared; except that now, procedures and functions
can be declared as if they were part of the record. Objects can “inherit” fields and methods from
“parent” objects. This means that these fields and methods can be used as if they were included in
the objects declared as a “child” object.

:p.
Furthermore, a concept of visibility is introduced: fields, procedures and functions can be declared as
public, protected or private. By default, fields and methods are public, and are exported
outside the current unit.

:p.
Fields or methods that are declared private are only accessible in the current unit: their scope is
limited to the implementation of the current unit.

:p.
The prototype declaration of an object is as follows:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Object types:ehp2.

&ra.&ra.───┬──────────┬─ :hp2.object:ehp2. ─┬────────────┬─┬─ component list ─┬─ end ────────────&ra.&la.
     └─ :hp2.packed:ehp2. ─┘          └─ heritage ─┘ ^──────────────────┘

&ra.&ra.─── heritage ── ( ── object type identifier ── ) ─────────────────────────────&ra.&la.

&ra.&ra.─── component list ─┬───────────────────────────────┬┬──────────────────────┬─&ra.
                      └─ object visibility specifier ─┘└┬─ field definition ─┬┘
                                                        ^────────────────────┘

&ra.───┬───────────────────────┬───────────────────────────────────────────────────&ra.&la.
    └┬─ method definition ─┬┘
     ^─────────────────────┘

&ra.&ra.─── field definition ── identifier list ── : ── type ── ; ──┬───────────┬─────&ra.&la.
                                                              └─ :hp2.static;:ehp2. ─┘

&ra.&ra.─── object visibility specifier ─┬─  :hp2.private:ehp2.  ─┬──────────────────────────────&ra.&la.
                                   ├─ :hp2.protected:ehp2. ─┤
                                   └─  :hp2.public:ehp2.   ─┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

.* TODO:  The "method definition" is missing from the above graph.

:p.
As can be seen, as many private and public blocks as needed can be declared.

:p.
The following is a valid definition of an object:

:xmp.
type
  TObj = object
  private
    Caption: ShortString;
  public
    constructor init;
    destructor done;
    procedure SetCaption(AValue: String);
    property GetCaption: String;
  end;
:exmp.

:p.
It contains a constructor/destructor pair, and a method to get and set a
caption. The :hp1.Caption:ehp1. field is private to the object: it cannot be accessed
outside the unit in which :hp1.TObj:ehp1. is declared.

:note.
In MacPas mode, the :hp1.Object:ehp1. keyword is replaced by the :hp1.class:ehp1.
keyword for compatibility with other pascal compilers available on the Mac. 
That means that objects cannot be used in MacPas mode.

:nt.
&fpc. also supports the packed object. This is the same as an object, only
the elements (fields) of the object are byte-aligned, just as in the packed
record. The declaration of a packed object is similar to the declaration
of a packed record:

:xmp.
type
  TObj = packed object
    constructor init;
    ...
  end;
  Pobj = ^TObj;

var
  pp: Pobj;
:exmp.

:p.
Similarly, the :hp1.{$PackRecords}:ehp1. directive acts on objects as well.
:ent.


:h3 name='objects_fields'.Fields
:p.
Object Fields are like record fields. They are accessed in the same way as
a record field  would be accessed: by using a qualified identifier. Given the
following declaration:

:xmp.
type
  TAnObject = object
    AField: Longint;
    procedure AMethod;
  end;

var
  AnObject: TAnObject;
:exmp.

:p.
then the following would be a valid assignment:

:xmp.
AnObject.AField := 0;
:exmp.

:p.
Inside methods, fields can be accessed using the short identifier:

:xmp.
procedure TAnObject.AMethod;
begin
  ...
  AField := 0;
  ...
end;
:exmp.

:p.
Or, one can use the :hp1.self:ehp1. identifier. The :hp1.self:ehp1. identifier refers
to the current instance of the object:

:xmp.
procedure TAnObject.AMethod;
begin
  ...
  self.AField := 0;
  ...
end;
:exmp.

:p.
One cannot access fields that are in a private or protected sections of an object from
outside the objects’ methods. If this is attempted anyway, the compiler will complain about
an unknown identifier.

:p.
It is also possible to use the :hp1.with:ehp1. statement with an object instance,
just as with a record:

:xmp.
with AnObject do
begin
  AField := 12;
  AMethod;
end;
:exmp.

:p.
In this example, between the :hp1.begin:ehp1. and :hp1.end:ehp1., it is as if
:hp1.AnObject:ehp1. was prepended to the :hp1.AField:ehp1. and :hp1.AMethod:ehp1.
identifiers. More about this in :link refid='statements_structured_with' reftype='hd'.The With Statement:elink..


:h3 name='objects_staticfields'.Static Fields
:p.
When the :hp1.{$STATIC ON}:ehp1. directive is active, then an object
can contain static fields: these fields are global to the object type, and act
like global variables, but are known only as part of the object. They can be
referenced from within the objects methods, but can also be referenced from
outside the object by providing the fully qualified name.

:p.
For instance, the output of the following program:

:xmp.
{$static on}
type
  cl = object
    l: longint; static;
  end;

var
  c1, c2: cl;
begin
  c1.l := 2;
  writeln(c2.l);
  c2.l := 3;
  writeln(c1.l);
  writeln(cl.l);
end.
:exmp.

:p.
will be the following

:xmp.
2
3
3
:exmp.

:p.
Note that the last line of code references the object type itself (cl), 
and not an instance of the object (cl1 or cl2).


:h3 name='objects_constructordestructor'.Constructors and Destructors
:p.
As can be seen in the syntax diagram for an object declaration, &fpc. supports
constructors and destructors. The programmer is responsible for calling the
constructor and the destructor explicitly when using objects.

:p.
The declaration of a constructor or destructor is as follows:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Constructors and Destructors:ehp2.

&ra.&ra.─── constructor declaration ── constructor header ── ; ── subroutine block ──────&ra.&la.

&ra.&ra.─── destructor declaration ── destructor header ── ; ── subroutine block ────────&ra.&la.

&ra.&ra.─── constructor header ── :hp2.constructor:ehp2. ──┬───────── identifier ──────────┬────────&ra.
                                          └─ qualified method identifier ─┘

&ra.──── formal parameter list ───────────────────────────────────────────────────────&ra.&la.

&ra.&ra.─── destructor header ── :hp2.destructor:ehp2. ──┬───────── identifier ──────────┬──────────&ra.
                                        └─ qualified method identifier ─┘

&ra.──── formal parameter list ───────────────────────────────────────────────────────&ra.&la.

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
A constructor/destructor pair is :hp1.required:ehp1. if the object uses virtual methods.
The reason is that for an object with virtual methods, some internal
housekeeping must be done: this housekeeping is done by the
constructor [A pointer to the VMT must be set up].

:p.
In the declaration of the object type, a simple identifier should be used
for the name of the constuctor or destructor. When the constructor or destructor
is implemented, A qualified method identifier should be used,
i.e. an identifier of the form :hp1.objectidentifier.methodidentifier:ehp1..

:p.
&fpc. supports also the extended syntax of the :hp1.New:ehp1. and :hp1.Dispose:ehp1.
procedures. In case a dynamic variable of an object type must be allocated
the constructor’s name can be specified in the call to :hp1.New:ehp1..
The :hp1.New:ehp1. is implemented as a function which returns a pointer to the
instantiated object. Consider the following declarations:

:xmp.
type
  TObj = object;
    constructor init;
    ...
  end;
  Pobj = ^TObj;

var
  PP: Pobj;
:exmp.

:p.
Then the following 3 calls are equivalent:

:xmp.
pp := new (Pobj,Init);
:exmp.

:p.
and

:xmp.
new(pp,init);
:exmp.

:p.
and also

:xmp.
new (pp);
pp^.init;
:exmp.

:p.
In the last case, the compiler will issue a warning that the
extended syntax of :hp1.New:ehp1. and :hp1.Dispose:ehp1. must be used to generate instances of an
object. It is possible to ignore this warning, but it’s better programming practice to
use the extended syntax to create instances of an object.
Similarly, the :hp1.Dispose:ehp1. procedure accepts the name of a destructor. The
destructor will then be called, before removing the object from the heap.

:p.
In view of the compiler warning remark, the following chapter presents the
&delphi. approach to object-oriented programming, and may be considered a
more natural way of object-oriented programming.


:h3 name='objects_methods'.Methods
:p.
Object methods are just like ordinary procedures or functions, only they
have an implicit extra parameter: :hp1.self:ehp1.. Self points to the object
with which the method was invoked.
When implementing methods, the fully qualified identifier must be given
in the function header. When declaring methods, a normal identifier must be
given.


:h4 name='objects_methods_declaration'.Declaration
:p.
The declaration of a method is much like a normal function or procedure
declaration, with some additional specifiers, as can be seen from the
following diagram, which is part of the object declaration:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Methods:ehp2.

&ra.&ra.─── method definition ─┬─  function header   ─┬─ ; ── method directives ─────────────&ra.&la.
                         ├─  procedure header  ─┤
                         ├─ constructor header ─┤
                         └─ destructor header  ─┘

&ra.&ra.─── method directives ─┬──────────────────────────────────┬┬─────────────────────┬───&ra.&la.
                         └─ :hp2.virtual:ehp2. ─ ; ─┬────────────────┬─┘└─ call modifier ─ ; ─┘
                                         └─ :hp2.abstract:ehp2. ─ ; ─┘
                                               
└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
from the point of view of declarations, :hp1.Method definitions:ehp1. are 
normal function or procedure declarations.
Contrary to &tp. and &delphi., fields can be declared after methods in the same 
block, i.e. the following will generate an error when compiling with &delphi.
or &tp., but not with &fpc.:

:xmp.
type 
  MyObj = object
    procedure Doit;
    Field: Longint;
  end;
:exmp.



:h4 name='objects_method_invocation'.Method invocation
:p.
Methods are called just as normal procedures are called, only they have an
object instance identifier prepended to them (see also :link refid='statements' reftype='hd'.Statements:elink.).
To determine which method is called, it is necessary to know the type of
the method. We treat the different types in what follows.

:ul.
:li.
:link refid='objects_static_methods' reftype='hd'.Static Methods:elink.
:li.
:link refid='objects_virtual_methods' reftype='hd'.Virtual Methods:elink.
:li.
:link refid='objects_abstract_methods' reftype='hd'.Abstract Methods:elink.
:eul.

:h5 name='objects_static_methods'.Static Methods
:p.
Static methods are methods that have been declared without a abstract or virtual keyword.
When calling a static method, the declared (i.e. compile time) method of the object is used. For
example, consider the following declarations:

:xmp.
type
  TParent = object
    ...
    procedure Doit;
    ...
    end;

  PParent = ^TParent;

  TChild = Object(TParent)
    ...
    procedure Doit;
    ...
    end;

  PChild = ^TChild;
:exmp.

:p.
As it is visible, both the parent and child objects have a method
called :hp1.Doit:ehp1.. Consider now the following declarations and calls:

:xmp.
var
  ParentA, ParentB: PParent;
  Child: PChild;
begin
   ParentA := New(PParent,Init);
   ParentB := New(PChild,Init);
   Child := New(PChild,Init);
   ParentA^.Doit;
   ParentB^.Doit;
   Child^.Doit;
:exmp.

:p.
Of the three invocations of :hp1.Doit:ehp1., only the last one will call
:hp1.TChild.Doit:ehp1., the other two calls will call :hp1.TParent.Doit:ehp1..
This is because for static methods, the compiler determines at compile
time which method should be called. Since :hp1.ParentB:ehp1. is of type
:hp1.TParent:ehp1., the compiler decides that it must be called with
:hp1.TParent.Doit:ehp1., even though it will be created as a :hp1.TChild:ehp1..
There may be times when the method that is actually called should
depend on the actual type of the object at run-time. If so, the method
cannot be a static method, but must be a virtual method.


:h5 name='objects_virtual_methods'.Virtual Methods
:p.
To remedy the situation in the previous section, :hp1.virtual:ehp1. methods are
created. This is simply done by appending the method declaration with the
:hp1.virtual:ehp1. modifier. The descendent object can then override the method
with a new implementation by re-declaring the method (with the same
parameter list) using the :hp1.virtual:ehp1. keyword.

:p.
Going back to the previous example, consider the following alternative
declaration:

:xmp.
type
  TParent = object
    ...
    procedure Doit; virtual;
    ...
    end;

  PParent = ^TParent;

  TChild = Object(TParent)
    ...
    procedure Doit; virtual;
    ...
    end;

  PChild = ^TChild;
:exmp.

:p.
As it is visible, both the parent and child objects have a method called
:hp1.Doit:ehp1.. Consider now the following declarations and calls:

:xmp.
var 
  ParentA, ParentB: PParent;
  Child: PChild;
begin
   ParentA := New(PParent, Init);
   ParentB := New(PChild, Init);
   Child := New(PChild, Init);
   ParentA^.Doit;
   ParentB^.Doit;
   Child^.Doit;
:exmp.

:p.
Now, different methods will be called, depending on the actual run-time type
of the object. For :hp1.ParentA:ehp1., nothing changes, since it is created as
a :hp1.TParent:ehp1. instance. For :hp1.Child:ehp1., the situation also doesn't
change: it is again created as an instance of :hp1.TChild:ehp1..

:p.
For :hp1.ParentB:ehp1. however, the situation does change: Even though it was
declared as a :hp1.TParent:ehp1., it is created as an instance of :hp1.TChild:ehp1..
Now, when the program runs, before calling :hp1.Doit:ehp1., the program
checks what the actual type of :hp1.ParentB:ehp1. is, and only then decides which
method must be called. Seeing that :hp1.ParentB:ehp1. is of type :hp1.TChild:ehp1.,
:hp1.TChild.Doit:ehp1. will be called. The code for this run-time checking of
the actual type of an object is inserted by the compiler at compile time.

The :hp1.TChild.Doit:ehp1. is said to "override" the
:hp1.TParent.Doit:ehp1.. It is possible to acces the :hp1.TParent.Doit:ehp1. from
within the :hp1.TChild.Doit:ehp1., with the :hp1.inherited:ehp1. keyword:

:xmp.
procedure TChild.Doit;
begin
  inherited Doit;
  ...
end;
:exmp.

:p.
In the above example, when :hp1.TChild.Doit:ehp1. is called, the first thing it
does is call :hp1.TParent.Doit:ehp1.. The inherited keyword cannot be used in
static methods, only on virtual methods.

:p.
To be able to do this, the compiler keeps - per object type - a table with
virtual methods: the VMT (Virtual Method Table). This is simply a table 
with pointers to each of the virtual methods: each virtual method has its
fixed location in this table (an index). The compiler uses this table to 
look up the actual method that must be used. When a descendent object
overrides a method, the entry of the parent method is overwritten in the
VMT. More information about the VMT can be found in :link reftype=hd database='prog.inf' refid=0.&progref.:elink..

:p.
As remarked earlier, objects that have a VMT must be initialized with a
constructor: the object variable must be initialized with a pointer to
the VMT of the actual type that it was created with.


:h5 name='objects_abstract_methods'.Abstract Methods
:p.
An abstract method is a special kind of virtual method. A method that is
declared :hp1.abstract:ehp1. does not have an implementation for this method. 
It is up to inherited objects to override and implement this method.

:p.
From this it follows that a method can not be abstract if it is not virtual 
(this can be seen from the syntax diagram). A second consequence is that 
an instance of an object that has an abstract method cannot be created
directly.

:p.
The reason is obvious: there is no method where the compiler could jump to!
A method that is declared :hp1.abstract:ehp1. does not have an implementation for
this method. It is up to inherited objects to override and implement this
method. Continuing our example, take a look at this:

:xmp.
type
  TParent = object
    ...
    procedure Doit; virtual; abstract;
    ...
    end;

  PParent=^TParent;

  TChild = Object(TParent)
    ...
    procedure Doit;virtual;
    ...
    end;

  PChild = ^TChild;
:exmp.

:p.
As it is visible, both the parent and child objects have a method called
:hp1.Doit:ehp1.. Consider now the following declarations and calls:

:xmp.
var
  ParentA, ParentB: PParent;
  Child: PChild;
begin
   ParentA := New(PParent, Init);
   ParentB := New(PChild, Init);
   Child := New(PChild, Init);
   ParentA^.Doit;
   ParentB^.Doit;
   Child^.Doit;
:exmp.

:p.
First of all, Line 3 will generate a compiler error, stating that one cannot
generate instances of objects with abstract methods: The compiler has
detected that :hp1.PParent:ehp1. points to an object which has an abstract
method. Commenting line 3 would allow compilation of the program.

:nt.
If an abstract method is overridden, The parent method cannot be called
with :hp1.inherited:ehp1., since there is no parent method; The compiler
will detect this, and complain about it, like this:

:xmp.
testo.pp(32,3) Error: Abstract methods can't be called directly
:exmp.

:p.
If, through some mechanism, an abstract method is called at run-time,
then a run-time error will occur. (run-time error 211, to be precise)
:ent.

:h3 name='objects_visibility'.Visibility
:p.
For objects, three visibility specifiers exist: :hp1.private:ehp1., :hp1.protected:ehp1. and
:hp1.public:ehp1.. If a visibility specifier is not specified, :hp1.public:ehp1.
is assumed. Both methods and fields can be hidden from a programmer by putting them
in a :hp1.private:ehp1. section. The exact visibility rule is as follows:

:parml tsize=15 break=none.
:pt.:hp2.Private:ehp2.
:pd. All fields and methods that are in a :hp1.private:ehp1. block,
can only be accessed in the module (i.e. unit or program) that contains
the object definition.
They can be accessed from inside the object's methods or from outside them
e.g. from other objects' methods, or global functions.

:pt.:hp2.Protected:ehp2.
:pd. Is the same as :hp1.Private:ehp1., except that the members of
a :hp1.Protected:ehp1. section are also accessible to descendent types, even if
they are implemented in other modules.

:pt.:hp2.Public:ehp2.
:pd. Fields and methods are always accessible, from everywhere.
Fields and methods in a :hp1.Public:ehp1. section behave as though they were part
of an ordinary :hp1.record:ehp1. type.
:eparml.



.* ==============================================================
:h2 name=classes.Classes
:p.
In the &delphi. approach to Object Oriented Programming, everything revolves
around the concept of 'Classes'.  A class can be seen as a pointer to an
object, or a pointer to a record, with methods associated with it.

:p.
The difference between objects and classes is mainly that an object
is allocated on the stack, as an ordinary record would be, and that
classes are always allocated on the heap. In the following example:

:xmp.
var
  A: TSomeObject; // an Object
  B: TSomeClass;  // a Class
:exmp.

:p.
The main difference is that the variable A will take up as much 
space on the stack as the size of the object (TSomeObject). The
variable B, on the other hand, will always take just the size of
a pointer on the stack. The actual class data is on the heap.

:p.
From this, a second difference follows: a class must :hp2.always:ehp2. be initialized
through its constructor, whereas for an object, this is not necessary.
Calling the constructor allocates the necessary memory on the heap for the
class instance data. 

:nt.
In earlier versions of &fpc. it was necessary, in order to use classes,
to put the :hp1.objpas:ehp1. unit in the uses clause of a unit or program.
:hp1. This is no longer needed as of version 0.99.12.:ehp1. As of this version,
the unit will be loaded automatically when the :hp2.-MObjfpc:ehp2. or
:hp2.-MDelphi:ehp2. options are specified, or their corresponding directives are
used:

:xmp.
{$mode objfpc}
{$mode delphi}
:exmp.

:p.
In fact, the compiler will give a warning if it encounters the
:hp1.objpas:ehp1. unit in a uses clause.
:ent.

:h3.Class definitions
:p.
The prototype declaration of a class is as follows:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Class types:ehp2.

&ra.&ra.───┬──────────┬─ :hp2.class:ehp2. ─┬────────────┬─┬────────────────────┬─ end ──────────────&ra.&la.
     └─ :hp2.packed:ehp2. ─┘         └─ heritage ─┘ └┬ component list ─┬─┘
                                          ^─────────────────┘

&ra.&ra.─── heritage ── ( ── class type identifier ──┬──────────────────────────┬─ ) ────&ra.&la.
                                               └─ implemented interfaces ─┘ 

&ra.&ra.─── implemented interfaces ─┬ , ── interface identifier ──┬──────────────────────&ra.&la.
                              ^─────────────────────────────┘ 

&ra.&ra.─── component list ─┬────────────────────────┬┬──────────────────────┬─&ra.
                      └─ visibility specifier ─┘└┬─ field definition ─┬┘
                                                 ^────────────────────┘

&ra.───┬───────────────────────────┬──────────────────────────────────────────────────&ra.&la.
    └┬┬── method definition ──┬┬┘
     │└─ property definition ─┘│
     ^─────────────────────────┘

&ra.&ra.─── field definition ── identifier list ── : ── type ── ; ──┬───────────┬────────&ra.&la.
                                                              └─ :hp2.static;:ehp2. ─┘

&ra.&ra.─── method definition ─┬┬─────────┬┬─ function header ──┬┬── ; ────────&ra.
                         │└─ :hp2.class:ehp2. ─┘└─ procedure header ─┘│
                         ├───── constructor header ────────┤
                         └───── destructor header ─────────┘

&ra.────┬─────────────────────────────────────────┬┬──────────────────────┬───────────&ra.&la.
     └─┬┬─ :hp2.virtual:ehp2. ─┬┬──────────────────┬┬─ ; ─┘└─ call modifiers ─ ; ─┘
       │└─ :hp2.dynamic:ehp2. ─┘└─ ; ── :hp2.abstract:ehp2. ──┘│
       ├──────── :hp2.override:ehp2. ───────────────┤
       └─ :hp2.message:ehp2. ─┬─ integer constant ─┬┘
                   └─ string constant ──┘
                         
&ra.&ra.─── class visibility specifier ─┬─  :hp2.private:ehp2.  ─┬──────────────────────────────────&ra.&la.
                                  ├─ :hp2.protected:ehp2. ─┤
                                  ├─  :hp2.public:ehp2.   ─┤
                                  └─ :hp2.published:ehp2. ─┘

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
:nt.
In MacPas mode, the :hp1.Object:ehp1. keyword is replaced by the :hp1.class:ehp1.
keyword for compatibility with other pascal compilers available on the Mac. 
That means that in MacPas mode, the reserved word 'class' in the above
diagram may be replaced by the reserved word 'object'.
:ent.

:p.
In a class declaration, as many :hp1.private:ehp1., :hp1.protected:ehp1., :hp1.published:ehp1.
and :hp1.public:ehp1. blocks as needed can be used: the various blocks can be
repeated, and there is no special order in which they must appear.

:p.
Methods are normal function or procedure declarations.
As can be seen, the declaration of a class is almost identical to the
declaration of an object. The real difference between objects and classes
is in the way they are created (see further in this chapter).
The visibility of the different sections are as follows:

:parml tsize=15 break=none.
:pt.:hp2.Private:ehp2.
:pd. All fields and methods that are in a :hp1.private:ehp1. block, can
only be accessed in the module (i.e. unit) that contains the class definition.
They can be accessed from inside the classes' methods or from outside them
(e.g. from other classes' methods).

:pt.:hp2.Protected:ehp2.
:pd.Is the same as :hp1.Private:ehp1., except that the members of
a :hp1.Protected:ehp1. section are also accessible to descendent types, even if
they are implemented in other modules.

:pt.:hp2.Public:ehp2.
:pd.sections are always accessible.

:pt.:hp2.Published:ehp2.
:pd.Is the same as a :hp1.Public:ehp1. section, but the compiler
generates also type information that is needed for automatic streaming of
these classes if the compiler is in the {$M+} state. Fields defined in 
a :hp1.published:ehp1. section must be of class type.
Array properties cannot be in a published section.

:eparml.

:p.
In the syntax diagram, it can be seen that a class can list implemented
interfaces. This feature will be discussed in the next chapter.

:p.
Classes can contain :hp1.class:ehp1. methods: these are functions that do not
require an instance. The :hp1.self:ehp1. identifier is valid in such methods, 
but refers to the class pointer (the VMT). 

:p.
Similar to objects, if the {$STATIC ON} directive is active, then a class
can contain static fields: these fields are global to the class, and act
like global variables, but are known only as part of the class. They can be
referenced from within the classes' methods, but can also be referenced from
outside the class by providing the fully qualified name.

:p.
For instance, the output of the following program:

:xmp.
{$mode objfpc}
{$static on}
type
  TMyClass = class
    l: longint; static;
  end;
var
  c1, c2: TMyClass;
begin
  c1 := TMyClass.create;
  c2 := TMyClass.create;
  c1.l := 2;
  writeln(c2.l);
  c2.l := 3;
  writeln(c1.l);
  writeln(TMyClass.l);
end.
:exmp.

:p.
will be the following

:xmp.
2
3
3
:exmp.

:p.
Note that the last line of code references the class type itself (TMyClass), 
and not an instance of the class (c1 or c2).

:p.
It is also possible to define class reference types:


:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Class reference type:ehp2.

&ra.&ra.─── :hp2.class of:ehp2. ── classtype ──────────────────────────────────────────────────&ra.&la.

└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
Class reference types are used to create instances of a certain class, which
is not yet known at compile time, but which is specified at run time. 
Essentially, a variable of a class reference type contains a pointer to the
definition of the speficied class. This can be used to construct an instance 
of the class corresponding to the definition, or to check inheritance. 
The following example shows how it works:

:xmp.
type
  TComponentClass = class of TComponent;

function CreateComponent(AClass: TComponentClass; AOwner: TComponent): TComponent;
begin
  // ...
  Result := AClass.Create(AOwner);
  // ...
end;
:exmp.

:p.
This function can be passed a class reference of any class that descends
from :hp1.TComponent:ehp1.. The following is a valid call:

:xmp.
var
  C: TComponent;
begin
  C := CreateComponent(TEdit, Form1);
end;
:exmp.

:p.
On return of the :hp1.CreateComponent:ehp1. function, C will contain an 
instance of the class TEdit. Note that the following call will fail to
compile:

:xmp.
var
  C: TComponent;
begin
  C := CreateComponent(TStream, Form1);
end;
:exmp.

:p.
because :hp1.TStream:ehp1. does not descend from :hp1.TComponent:ehp1., and
:hp1.AClass:ehp1. refers to a :hp1.TComponent:ehp1. class. The compiler can
(and will) check this at compile time, and will produce an error.

:p.
References to classes can also be used to check inheritance:
:xmp.
type
  TMinClass = class of TMyClass;
  TMaxClass = class of TMyClassChild;

function CheckObjectBetween(Instance: TObject): boolean;
begin
  if not (Instance is TMinClass) 
     or ((Instance is TMaxClass) and (Instance.ClassType <> TMaxClass)) then
    raise Exception.Create('SomeError')
end;
:exmp.

:p.
The above example will raise an exception if the passed instance
is not a descendent of :hp1.TMinClass:ehp1. or a descendent if :hp1.TMaxClass:ehp1..

:p.
More about instantiating a class can be found in the next section.



:h3.Class instantiation
:p.
Classes must be created using one of their constructors (there can be
multiple constructors). Remember that a class is a pointer to an object on
the heap. When a variable of some class is declared, the compiler just 
allocates room for this pointer, not the entire object. The constructor of
a class returns a pointer to an initialized instance of the object on the
heap. So, to initialize an instance of some class, one would do the following:

:xmp.
  ClassVar := ClassType.ConstructorName;
:exmp.

:p.
The extended syntax of :hp1.new:ehp1. and :hp1.dispose:ehp1. can :hp2.not:ehp2. be used to
instantiate and destroy class instances.
That construct is reserved for use with objects only.
Calling the constructor will provoke a call to :hp1.getmem:ehp1., to allocate
enough space to hold the class instance data.
After that, the constuctor's code is executed.
The constructor has a pointer to its data, in :hp1.self:ehp1..

:nt.
:lm margin=5.
:ul.
:li. The {$PackRecords} directive also affects classes.
i.e. the alignment in memory of the different fields depends on the
value of  the {$PackRecords} directive.
:li. Just as for objects and records, a packed class can be declared.
This has the same effect as on an object, or record, namely that the
elements are aligned on 1-byte boundaries. i.e. as close as possible.
:li. :hp1.SizeOf(class):ehp1. will return the same as :hp1.SizeOf(Pointer):ehp1., 
since a class is but a pointer to an object. To get the size of the class 
instance data, use the :hp1.TObject.InstanceSize:ehp1. method.
:eul.
:ent.



:h3.Methods
:p.
:ul.
:li.:link refid='class_declaration' reftype='hd'.Declaration:elink.
:li.:link refid='class_invocation' reftype='hd'.Invocation:elink.
:li.:link refid='class_virtual_methods' reftype='hd'.Virtual methods:elink.
:li.:link refid='class_class_methods' reftype='hd'.Class methods:elink.
:li.:link refid='class_message_methods' reftype='hd'.Message methods:elink.
:li.:link refid='class_using_inherited' reftype='hd'.Using inherited:elink.
:eul.


:h4 name='class_declaration'.Declaration
:p.
Declaration of methods in classes follows the same rules as method
declarations in objects:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Methods:ehp2.

&ra.&ra.─── method definition ─┬─  function header   ─┬─ ; ── method directives ─────────────────&ra.&la.
                         ├─  procedure header  ─┤
                         ├─ constructor header ─┤
                         └─ destructor header  ─┘

&ra.&ra.─── method directives ──┬──────────────────────────────────┬┬──────────────────────┬─────&ra.&la.
                          ├─ :hp2.virtual:ehp2. ─ ; ─┬─────────────────┬┘└─ call modifiers ─ ; ─┘
                          │               └─ :hp2.abstract:ehp2. ─ ; ──┤
                          ├──────── :hp2.reintroduce:ehp2. ─ ; ────────┤
                          └─ :hp2.message:ehp2. ─ constant expression ─┘
                         
└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.


:h4 name='class_invocation'.Invocation
:p.
Method invocation for classes is no different than for objects. The
following is a valid method invocation:

:xmp.
var
  AnObject: TAnObject;
begin
  AnObject := TAnObject.Create;
  AnObject.AMethod;
:exmp.



:h4 name='class_virtual_methods'.Virtual methods
:p.
Classes have virtual methods, just as objects do. There is however a
difference between the two. For objects, it is sufficient to redeclare the
same method in a descendent object with the keyword :hp1.virtual:ehp1. to
override it. For classes, the situation is different: virtual methods 
:hp2.must:ehp2. be overridden with the :hp1.override:ehp1. keyword. Failing to do so,
will start a :hp2.new:ehp2. batch of virtual methods, hiding the previous
one.  The :hp1.Inherited:ehp1. keyword will not jump to the inherited method, if
:hp1.Virtual:ehp1. was used.

:p.
The following code is :hp2.wrong:ehp2.:

:xmp.
type 
  TObjParent = class
    procedure MyProc; virtual;
  end;
  
  ObjChild = class(TObjParent)
    procedure MyProc; virtual;
  end;
:exmp.

:p.
The compiler will produce a warning:

:xmp.
Warning: An inherited method is hidden by OBJCHILD.MYPROC
:exmp.

:p.
The compiler will compile it, but using :hp1.Inherited:ehp1. can
produce strange effects.

:p.
The correct declaration is as follows:

:xmp.
type 
  TObjParent = class
    procedure MyProc; virtual;
  end;
  
  TObjChild  = Class(TObjParent)
    procedure MyProc; override;
  end;
:exmp.

:p.
This will compile and run without warnings or errors.

:p.
If the virtual method should really be replaced with a method with the 
same name, then the :hp1.reintroduce:ehp1. keyword can be used:

:xmp.
type 
  TObjParent = class
    procedure MyProc; virtual;
  end;
  
  TObjChild  = Class(TObjParent)
    procedure MyProc; reintroduce;
  end;
:exmp.

:p.
This new method is no longer virtual.

:p.
To be able to do this, the compiler keeps - per class type - a table with
virtual methods: the VMT (Virtual Method Table). This is simply a table 
with pointers to each of the virtual methods: each virtual method has its
fixed location in this table (an index). The compiler uses this table to 
look up the actual method that must be used at runtime. When a descendent object
overrides a method, the entry of the parent method is overwritten in the
VMT. More information about the VMT can be found in the &progref..

:nt.
The keyword 'virtual' can be replaced with the 'dynamic' keyword: dynamic
methods behave the same as virtual methods. Unlike in &delphi., in &fpc. the
implementation of dynamic methods is equal to the implementation of virtual
methods.
:ent.



:h4 name='class_class_methods'.Class methods
:p.
Class methods are identified by the keyword :hp1.Class:ehp1. in front of the
procedure or function declaration, as in the following example:

:xmp.
  class function ClassName: string;
:exmp.

:p.
Class methods are methods that do not have an instance (i.e. Self does not
point to a class instance) but which follow the scoping and inheritance 
rules of a class. They can be used to return information about the current
class, for instance for registration or use in a class factory. Since no 
instance is available, no information available in instances can be used.

:p.
Class methods can be called from inside a regular method, but can also be called 
using a class identifier:

:xmp.
var
  AClass: TClass;
begin
  ...
  if CompareText(AClass.ClassName,'TCOMPONENT')=0 then
  ...
:exmp.

:p.
But calling them from an instance is also possible:

:xmp.
var
  MyClass: TObject;
begin
  ...
  MyClass := TObject.Create;
  if CompareText(MyClass.ClassName, 'TCOMPONENT')=0 then
  ...
:exmp.

:p.
The reverse is not possible: Inside a class method, the Self identifier 
points to the VMT table of the class. No fields, properties or 
regular methods are available inside a class method. Accessing a regular 
property or method will result in a compiler error. 

:p.
Note that class methods can be virtual, and can be overridden.

:p.
Class methods cannot be used as read or write specifiers for a
property.



:h4 name='class_message_methods'.Message methods
:p.
New in classes are :hp1.message:ehp1. methods. Pointers to message methods are
stored in a special table, together with the integer or string constant that
they were declared with. They are primarily intended to ease programming of
callback functions in several GUI toolkits, such as Win32 or
GTK. In difference with &delphi., &fpc. also accepts strings as message
identifiers. Message methods are always virtual.

:p.
As can be seen in the class declaration diagram, message methods are 
declared with a :hp1.message:ehp1. keyword, followed by an integer constant
expression.

:p.
Additionally, they can take only one var argument (typed or not):

:xmp.
  procedure TMyObject.MyHandler(var Msg); message 1;
:exmp.

:p.
The method implementation of a message function is not different from an
ordinary method. It is also possible to call a message method directly,
but this should not be done. Instead, the TObject.Dispatch method
should be used. Message methods are automatically virtual,
i.e. they can be overridden in descendent classes.

:p.
The TObject.Dispatch method can be used to call a message handler.
It is declared in the :hp1.System:ehp1. unit and will accept a var
parameter which must have at the first position a cardinal with the
message ID that should be called. For example:

:xmp.
type
  TMsg = record
    MsgID: Cardinal;
    Data: Pointer;
  end;
  
var
  Msg: TMSg;
begin
  ...
  MyObject.Dispatch(Msg);
:exmp.

:p.
In this example, the Dispatch() method will look at the object and all
its ancestors (starting at the object, and searching up the inheritance 
class tree), to see if a message method with message :hp1.MsgID:ehp1. has been
declared. If such a method is found, it is called, and passed the
Msg parameter.

:p.
If no such method is found, :hp1.DefaultHandlerStr():ehp1. is called.
DefaultHandlerStr() is a virtual method of TObject that doesn't do
anything, but which can be overridden to provide any processing that might be
needed. DefaultHandlerStr() is declared as follows:

:xmp.
  procedure DefaultHandlerStr(var message); virtual;
:exmp.

:p.
In addition to this mechanism, a string message method accepts a :hp1.self:ehp1.
parameter:

:xmp.
  procedure StrMsgHandler(Data: Pointer; Self: TMyObject); Message 'OnClick';
:exmp.

:p.
When encountering such a method, the compiler will generate code that loads
the :hp1.Self:ehp1. parameter into the object instance pointer. The result of
this is that it is possible to pass Self as a parameter to such a
method.

:nt.
The type of the :hp1.Self:ehp1. parameter must be of the same class
as the class the method is defined in.
:ent.



:h4 name='class_using_inherited'.Using inherited
:p.
In an overridden virtual method, it is often necessary to call the parent
class' implementation of the virtual method. This can be done with the
:hp1.inherited:ehp1. keyword. Likewise, the :hp1.inherited:ehp1. keyword can be used
to call any method of the parent class.

:p.
The first case is the simplest:

:xmp.
type
  TMyClass = class(TComponent)
    constructor Create(AOwner: TComponent); override;
  end;

constructor TMyClass.Create(AOwner: TComponent); 
begin
  inherited;
  // Do more things
end;
:exmp.

:p.
In the above example, the :hp1.Inherited:ehp1. statement will call Create()
of TComponent, passing it AOwner as a parameter: the same
parameters that were passed to the current method will be passed to the
parent's method. They must not be specified again: if none are specified,
the compiler will pass the same arguments as the ones received.

:p.
The second case is slightly more complicated:

:xmp.
type
  TMyClass = class(TComponent)
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; DoExtra: Boolean);
  end;

constructor TMyClass.Create(AOwner: TComponent); 
begin
  inherited;
end;

constructor TMyClass.CreateNew(AOwner: TComponent; DoExtra: Boolean); 
begin
  inherited Create(AOwner);
  // Do stuff
end;
:exmp.

:p.
The CreateNew() method will first call TComponent.Create() and
will pass it AOwner as a parameter. It will not call
TMyClass.Create().

:p.
Although the examples were given using constructors, the use of
Inherited is not restricted to constructors, it can be used
for any procedure or function or destructor as well.



:h3.Properties
:p.
:ul.
:li.:link refid='class_prop_definition' reftype='hd'.Definition:elink.
:li.:link refid='class_indexed props' reftype='hd'.Indexed properties:elink.
:li.:link refid='class_array props' reftype='hd'.Array properties:elink.
:li.:link refid='class_default props' reftype='hd'.Default properties:elink.
:li.:link refid='class_storage information' reftype='hd'.Storage information:elink.
:li.:link refid='class_overriding props' reftype='hd'.Overriding properties:elink.
:eul.





:h4 name='class_prop_definition'.Definition
:p.
Classes can contain properties as part of their fields list. A property
acts like a normal field, i.e. its value can be retrieved or set, but it
allows to redirect the access of the field through functions and
procedures. They provide a means to associate an action with an assignment
of, or a reading from a class 'field'. This allows for e.g. checking that a
value is valid when assigning, or, when reading, it allows to construct the
value on the fly. Moreover, properties can be read-only or write only.
The prototype declaration of a property is as follows:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Properties:ehp2.

&ra.&ra.─── property definition ── :hp2.property:ehp2. ── identifier ─┬──────────────────────┬───&ra.
                                                     └─ property interface ─┘
                                                     
&ra.──── property specifier ── hint directive ─────────────────────────────────────&ra.&la.

&ra.&ra.─── property interface ─┬───────────────────────────┬─ : ── type identifier ──&ra.
                          └─ property parameter list ─┘

&ra.─────┬─────────────────────────────┬───────────────────────────────────────────&ra.&la.
      └─ :hp2.index:ehp2. ── integer constant ─┘

&ra.&ra.─── property parameter list ── [ ── parameter declaration ─┬──────────────────&ra.&la.
                                    ^───────── ; ────────────┘

&ra.&ra.─── property specifiers ──┬──────────────────┬─┬────────────────────────┬─────&ra.
                            └─ read specifier ─┘ ├─── write specifier ────┤
                                                 └─ implements specifier ─┘

&ra.─────┬─────────────────────┬┬────────────────────┬─────────────────────────────&ra.
      └─ default specifier ─┘└─ stored specifier ─┘

&ra.─────┬────────────────────────────────────┬────────────────────────────────────&ra.&la.
      └─ default array property specifier ─┘

&ra.&ra.──── read specifier ── :hp2.read:ehp2. ── field or method ───────────────────────────────&ra.&la.

&ra.&ra.──── write specifier ── :hp2.write:ehp2. ── field or method ─────────────────────────────&ra.&la.

&ra.&ra.──── implements specifier ── :hp2.implements:ehp2. ── identifier ────────────────────────&ra.&la.

&ra.&ra.──── default specifier ─┬─ :hp2.default:ehp2. ─┬────────────┬┬───────────────────────────&ra.&la.
                          │           └─ constant ─┘│
                          └───── :hp2.nodefault:ehp2. ─────────┘

&ra.&ra.──── stored specifier ── :hp2.stored:ehp2. ─┬─ constant ───┬─────────────────────────────&ra.&la.
                                   └─ identifier ─┘
                          
&ra.&ra.──── field or method ──┬─ field identifier ──┬────────────────────────────────&ra.&la.
                         └─ method identifier ─┘

&ra.&ra.──── default array property specifier ── ; ── :hp2.default:ehp2. ────────────────────────&ra.&la.
                         
└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
A :hp1.read specifier:ehp1. is either the name of a field that contains the property, or the name of a
method function that has the same return type as the property type. In the case of a simple type, this
function must not accept an argument. In case of an array property, the function must accept a single
argument of the same type as the index. In case of an indexed property, it must accept a integer as an
argument.

:p.
A :hp1.read specifier:ehp1. is optional, making the property write-only. Note that class methods cannot
be used as read specifiers.

:p.
A :hp1.write specifier:ehp1. is optional: If there is no :hp1.write specifier:ehp1., the property is read-only.
A write specifier is either the name of a field, or the name of a method procedure that accepts as a sole
argument a variable of the same type as the property. In case of an array property, the procedure must
accept 2 arguments: the first argument must have the same type as the index, the second argument
must be of the same type as the property. Similarly, in case of an indexed property, the first parameter
must be an integer.

:p.
The section (private, published) in which the specified function or procedure resides is irrelevant.
Usually, however, this will be a protected or private method.

:p.
For example, given the following declaration:

:xmp.
type
  MyClass = class
  private
    Field1: Longint;
    Field2: Longint;
    Field3: Longint;
    procedure Sety(AValue: Longint);
    function Gety: Longint;
    function Getz: Longint;
  public
    property X: Longint read Field1 write Field2;
    property Y: Longint read GetY write Sety;
    property Z: Longint read GetZ;
  end;

var 
  MyClass: TMyClass;
:exmp.

:p.
The following are valid statements:

:xmp.
WriteLn ('X : ', MyClass.X);
WriteLn ('Y : ', MyClass.Y);
WriteLn ('Z : ', MyClass.Z);
MyClass.X := 0;
MyClass.Y := 0;
:exmp.

:p.
But the following would generate an error:

:xmp.
MyClass.Z := 0;
:exmp.

:p.
because Z is a read-only property.

:p.
What happens in the above statements is that when a value needs to be read,
the compiler inserts a call to the various :hp1.getNNN:ehp1. methods of the
object, and the result of this call is used. When an assignment is made,
the compiler passes the value that must be assigned as a paramater to
the various :hp1.setNNN:ehp1. methods.

:p.
Because of this mechanism, properties cannot be passed as var arguments to a
function or procedure, since there is no known address of the property (at
least, not always).


:h4 name='class_indexed props'.Indexed properties
:p.
If the property definition contains an index, then the read and write specifiers must be a function and
a procedure. Moreover, these functions require an additional parameter: An integer parameter. This
allows to read or write several properties with the same function. For this, the properties must have
the same type. The following is an example of a property with an index:

:xmp.
{$mode objfpc}
type 
  TPoint = class(TObject)
  private
    FX: Longint;
    FY: Longint;
    function GetCoord(Index: Integer): Longint;
    procedure SetCoord(Index: Integer; Value: Longint);
  public
    property X: Longint index 1 read GetCoord write SetCoord;
    property Y: Longint index 2 read GetCoord write SetCoord;
    property Coords[Index: Integer]: Longint read GetCoord;
  end;

procedure TPoint.SetCoord(Index: Integer; Value: Longint);
begin
  case Index of
   1 : FX := Value;
   2 : FY := Value;
  end;
end;

function TPoint.GetCoord(Index: Integer): Longint;
begin
  case Index of
   1 : Result := FX;
   2 : Result := FY;
  end;
end;

var 
  P: TPoint;

begin
  P := TPoint.Create;
  P.X := 2;
  P.Y := 3;
  with P do
    WriteLn('X=', X, ' Y=', Y);
end.
:exmp.

:p.
When the compiler encounters an assignment to X, then :hp1.SetCoord:ehp1. is called with as first parameter
the index (1 in the above case) and with as a second parameter the value to be set. Conversely, when
reading the value of X, the compiler calls :hp1.GetCoord:ehp1. and passes it index 1. Indexes can only be
integer values.


:h4 name='class_array props'.Array properties
:p.
Array properties also exist. These are properties that accept an
index, just as an array does. Only now the index doesn't have to be an
ordinal type, but can be any type.

:p.
A :hp1.read specifier:ehp1. for an array property is the name method function
that has the same return type as the property type.
The function must accept as a sole arguent a variable of the same type as
the index type. For an array property, one cannot specify fields as :hp1.read
specifiers:ehp1..

:p.
A :hp1.write specifier:ehp1. for an array property is the name of a method
procedure that accepts two arguments: The first argument has the same
type as the index, and the second argument is a parameter of the same
type as the property type. As an example, see the following declaration:

:xmp.
type 
  TIntList = class
  private
    Function GetInt(I: Longint): Longint;
    Function GetAsString(A: String): String;
    Procedure SetInt(I: Longint; Value: Longint;);
    Procedure SetAsString(A: String; Value: String);
  public
    property Items[i: Longint]: Longint read GetInt write SetInt;
    property StrItems[S: String]: String read GetAsString write SetAsstring;
  end;

var 
  AIntList: TIntList;
:exmp.

:p.
Then the following statements would be valid:

:xmp.
AIntList.Items[26] := 1;
AIntList.StrItems['twenty-five'] := 'zero';
WriteLn('Item 26 : ', AIntList.Items[26]);
WriteLn('Item 25 : ', AIntList.StrItems['twenty-five']);
:exmp.

:p.
While the following statements would generate errors:

:xmp.
AIntList.Items['twenty-five'] := 1;
AIntList.StrItems[26] := 'zero';
:exmp.

:p.
Because the index types are wrong.


:h4 name='class_default props'.Default properties
:p.
Array properties can be declared as :hp1.default:ehp1. properties. This means that
it is not necessary to specify the property name when assigning or reading
it. In the previous example, if the definition of the items property would
have been

:xmp.
property Items[i: Longint]: Longint read GetInt write SetInt; default;
:exmp.

:p.
Then the assignment

:xmp.
AIntList.Items[26] := 1;
:exmp.

:p.
Would be equivalent to the following abbreviation.

:xmp.
AIntList[26] := 1;
:exmp.

:p.
Only one default property per class is allowed, and descendent classes
cannot redeclare the default property.



:h4 name='class_storage information'.Storage information
:p.
The :hp1.stored specifier:ehp1. should be either a boolean constant, a boolean
field of the class, or a parameterless function which returns a boolean
result. This specifier has no result on the class behaviour. It is an aid
for the streaming system: the stored specifier is specified in the RTTI
generated for a class (it can only be streamed if RTTI is generated), 
and is used to determine whether a property should be streamed or not: 
it saves space in a stream. It is not possible to specify the 'Stored'
directive for array properties.

:p.
The :hp1.default specifier:ehp1. can be specified for ordinal types and sets.
It serves the same purpose as the :hp1.stored specifier:ehp1.: Properties that
have as value their default value, will not be written to the stream by the
streaming system. The default value is stored in the RTTI that is generated
for the class. Note that

:ol.
:li.When the class is instantiated, the default value is not automatically
applied to the property, it is the responsability of the programmer to do
this in the constructor of the class.
:li.The value 2147483648 cannot be used as a default value, as it is used
internally to denote 'nodefault'.
:li.It is not possible to specify a default for array properties.
:eol.


:h4 name='class_overriding props'.Overriding properties
:p.
aoeu
.* START HERE !!!!!!!!!!!!!!!!!!!!!!


.* ==============================================================
:h2 name=interfaces.Interfaces
:p.aa

.* ==============================================================
:h2 name=generics.Generics
:p.aa

.* ==============================================================
:h2 name=expressions.Expressions
:p.aa
:h3 name=expression_syntax.Expression syntax
:p.aa
:h3 name=function_calls.Function calls
:p.aa
:h3 name=set_constructors.Set constructors
:p.aa
:h3 name=value_typecasts.Value typecasts
:p.aa
:h3 name=varibale_typecasts.Variable typecasts
:p.aa
:h3 name=unaligned_typecasts.Unaligned typecasts
:p.aa
:h3 name=the_at_operator.The @ operator
:p.aa
:h3 name=operators.Operators
:p.aa
:h4 name=arithmetic_operators.Arithmetic operators
:p.aa
:h4 name=logical_operators.Logical operators
:p.aa
:h4 name=boolean_operators.Boolean operators
:p.aa
:h4 name=string_operators.String operators
:p.aa
:h4 name=set_operators.Set operators
:p.aa
:h4 name=relational_operators.Relational operators
:p.aa
:h4 name=class_operators.Class operators
:p.aa

.* ==============================================================
:h2 name=statements.Statements
:p.aa
:h3 name=statements_simple.Simple Statements
:p.aa
:h3 name=statements_structured.Structured Statements
:p.aa
:h4 name=statements_structured_with.The With Statement
:p.aa

.* ==============================================================
:h2 name=functions.Using functions and procedures
:p.aa
:h3 name=procedure_declarations.Procedure declarations
:p.aa
:h3 name=function-declarations.Function declarations
:p.aa

.* ==============================================================
:h2 name=operator_overloading.Operator overloading
:p.aa





.* ==============================================================
:h2 name=programs_units_blocks.Programs&comma. units and blocks
:p.A Pascal program can consist of modules called :hp1.units:ehp1.. A unit can be used
to group pieces of code together, or to give someone code without giving
the sources.
Both programs and units consist of code blocks, which are mixtures of
statements, procedures, and variable or type declarations.

.* --------------------------------------------------------------
:h3.Programs
:p.A Pascal program consists of the program header, followed possibly by a
'uses' clause, and a block.

:xmp.
                     [diagram goes here]
:exmp.

:p.
The program header is provided for backwards compatibility, and is ignored
by the compiler.

:p.
The uses clause serves to identify all units that are needed by the program.
All identifiers which are declared in the the interface section of the units
in the uses clause are added to the known identifiers of the program.
The system unit doesn't have to be in this list, since it is always loaded
by the compiler.

:p.
The order in which the units appear is significant, it determines in
which order they are initialized. Units are initialized in the same order
as they appear in the uses clause. Identifiers are searched in the opposite
order, i.e. when the compiler searches for an identifier, then it looks
first in the last unit in the uses clause, then the last but one, and so on.
This is important in case two units declare different types with the same
identifier.

:p.
When the compiler looks for unit files, it adds the extension :hp1..ppu:ehp1.
to the name of the unit. On &linux. and in operating systems where filenames 
are case sensitive when looking for a unit, the following mechanism is
used:

:ol.
:li.The unit is first looked for in the original case.
:li.The unit is looked for in all-lowercase letters.
:li.The unit is looked for in all-uppercase letters.
:eol.

:p.
Additionally, If a unit name is longer than 8 characters, the compiler 
will first look for a unit name with this length, and then it will 
truncate the name to 8 characters and look for it again. 
For compatibility reasons, this is also true on platforms that 
support long file names.

:p.
Note that the above search is performed in each directory in the search
path. 

:p.
The program block contains the statements that will be executed when the
program is started. Note that these statements need not necessarily be the 
first statements that are executed: the initialization code of the units
may also contain statements that are executed prior to the program code.

:p.
The structure of a program block is discussed below.

.* --------------------------------------------------------------
:h3.Units
:p.A unit contains a set of declarations, procedures and functions that can be
used by a program or another unit. The syntax for a unit is as follows:

:cgraphic.
┌──────────────────────────────────────────────────────────────────────────────┐
:hp2.Units:ehp2.

&ra.&ra.─── unit ── unit header ── interface part ──  implementation part ────────────&ra.

&ra.─────┬────────────────────────────────────────────┬─ :hp2.end:ehp2. ── . ─────────────────&ra.&la.
      ├─ initialization part ─┬───────────────────┬┤
      │                       └ finalization part ┘│
      └─  :hp2.begin:ehp2. ─── statement ─┬───────────────────┘
                 ^───── ; ─────┘

&ra.&ra.─── unit header ── :hp2.unit:ehp2. ── unit identifier ── ; ──────────────────────────────&ra.&la.
                                                     
&ra.&ra.─── interface part ── :hp2.interface:ehp2. ─┬───────────────┬┬┬─────────────────────────────┬┬──&ra.&la.
                                   └─ uses clause ─┘│├─ constant declaration part ─┤│
                                                    │├─   type declatation part   ─┤│
                                                    │└─  procedure headers part   ─┘│
                                                    ^───────────────────────────────┘
                                   
&ra.&ra.─── procedure headers part ─┬─ procedure header ─┬─ ; ─┬───────────────────────┬─────&ra.&la.
                              └─ function header  ─┘     └─ call modifiers ── ; ─┘

&ra.&ra.─── implementation part ── :hp2.implementation:ehp2. ─┬───────────────┬─ declaration part ──────&ra.&la.
                                             ^─ uses clause ─┘

&ra.&ra.─── initialization part ── :hp2.initialization:ehp2. ─┬─ statement ─┬────────────────────&ra.&la.
                                             ^───── ; ─────┘

&ra.&ra.─── finalization part ── :hp2.finalization:ehp2. ─┬─ statement ─┬────────────────────────&ra.&la.
                                         ^───── ; ─────┘
                         
└──────────────────────────────────────────────────────────────────────────────┘
:ecgraphic.

:p.
As can be seen from the syntax diagram, a unit always consists of a
interface and an implementation part. Optionally, there is an initialization
block and a finalization block, containing code that will be executed when
the program is started, and when the program stops, respectively.

:p.
Both the interface part or implementation part can be empty, but the
keywords :hp1.Interface:ehp1. and :hp1.implementation:ehp1. must be specified.
The following is a completely valid unit:

:xmp.
unit a;

interface

implementation

end.
:exmp.

:p.
The interface part declares all identifiers that must be exported from the
unit. This can be constant, type or variable identifiers, and also procedure
or function identifier declarations.  The interface part cannot contain code
that is executed: only declarations are allowed. The following is a valid
interface part:

:xmp.
unit a;

interface

uses b;

function MyFunction: SomeBType;

implementation
:exmp.

:p.
The type :hp1.SomeBType:ehp1. is defined in unit :hp1.b:ehp1..

:p.
All functions and methods that are declared in the interface part must
be implemented in the implementation part of the unit, except for
declarations of external functions or procedures. If a declared method 
or function is not implemented in the implementation part, the compiler
will give an error, for example the following:

:xmp.
unit unita;

interface

function MyFunction: Integer;

implementation

end.
:exmp.

:p.
Will result in the following error:

:xmp.
unita.pp(5,10) Error: Forward declaration not solved "MyFunction&colon.SmallInt;"
:exmp.

:p.
The implementation part is primarily intended for the implementation of the
functions and procedures declared in the interface part. However, it can
also contain declarations of it's own: The declarations inside the 
implementation part are :hp2.not:ehp2. accessible outside the unit. 

:p.
The initialization and finalization part of a unit are optional:

:p.
The initialization block is used to initialize certain variables or 
execute code that is necessary for the correct functioning of the unit. 
The initialization parts of the units
are executed in the order that the compiler loaded the units when compiling 
a program. They are executed before the first statement of the program is
executed.

:p.
The finalization part of the units are executed in the reverse order of the
initialization execution. They are used for instance to clean up any resources 
allocated in the initialization part  of the unit, or during the lifetime of
the program. The finalization part is always executed in the case of a
normal program termination: whether it is because the final :hp1.end:ehp1. is
reached in the program code or because a :hp1.Halt:ehp1. instruction was executed
somewhere.

:p.
In case the program stops during the execution of the initialization blocks
of one of the units, only the units that were already initialized will be
finalized. Note that if a :hp1.finalization:ehp1. block is present, an
:hp1.initialization:ehp1. block must be present, but it can be empty:

:xmp.
initialization

finalization
  CleanupUnit;
end.
:exmp.

:p.
An initialization section by itself (i.e. without finalization) may simply be 
replaced by a statement block. That is, the following:

:xmp.
initialization
  InitializeUnit;
end.
:exmp.

:p.
is completely equivalent to

:xmp.
begin
  InitializeUnit;
end.
:exmp.

.* --------------------------------------------------------------
:h3.Unit dependencies
:p.aa





.* ==============================================================
:h2 name=exceptions.Exceptions
:p.aa

.* ==============================================================
:h2 name=assembler.Using assembler
:p.
&fpc. supports the use of assembler in code, but not inline
assembler macros. To have more information on the processor
specific assembler syntax and its limitations, see the &progref..

.* --------------------------------------------------------------
:h3.Assembler statements
The following is an example of assembler inclusion in Pascal code.

:xmp.
 ...
 Statements;
 ...
 Asm
   the asm code here
   ...
 end;
 ...
 Statements;
:exmp.

:p.
The assembler instructions between the :hp1.Asm:ehp1. and :hp1.end:ehp1. keywords will
be inserted in the assembler generated by the compiler.
Conditionals can be used in assembler code, the compiler will recognise them,
and treat them as any other conditionals.

.* --------------------------------------------------------------
:h3.Assembler procedures and functions
:p.
Assembler procedures and functions are declared using the
:hp1.Assembler:ehp1. directive.  This permits the code generator to make a number
of code generation optimizations.

:p.
The code generator does not generate any stack frame (entry and exit
code for the routine) if it contains no local variables and no
parameters. In the case of functions, ordinal values must be returned
in the accumulator. In the case of floating point values, these depend
on the target processor and emulation options.

:h2.Object Pascal Grammar
:p.This section describes the Object Pascal grammar in a EBNF
(Extended Backus–Naur Form) like style. The syntax only covers the
:hp1.ObjFPC:ehp1. mode of the &fpc. compiler.

:cgraphic.
Goal -> (Program | Package | Library | Unit)
Program -> [PROGRAM Ident ['(' IdentList ')'] ';']
           ProgramBlock '.'
Unit -> UNIT Ident [HintDirective] ';'
        InterfaceSection
        ImplementationSection
        InitSection '.'
Package -> PACKAGE Ident ';'
           [RequiresClause]
           [ContainsClause]
           END '.'
Library -> LIBRARY Ident ';'
           ProgramBlock '.'
ProgramBlock -> [UsesClause]
                Block
UsesClause -> USES IdentList ';'
HintDirective -> deprecated  [String]
              -> experimental
              -> library
              -> platform
              -> unimplemented
InterfaceSection -> INTERFACE
                    [UsesClause]
                    [InterfaceDecl]...
InterfaceDecl ->  ConstSection
              ->  TypeSection
              ->  VarSection
              ->  ExportedHeading
ExportedHeading -> ProcedureHeading ';' [Directive]
                -> FunctionHeading ';' [Directive]
ImplementationSection -> IMPLEMENTATION
                         [UsesClause]
                         [DeclSection]...
                         [ExportsStmt]...
Block -> [DeclSection]
         [ExportsStmt]...
         CompoundStmt
         [ExportsStmt]...
ExportsStmt -> EXPORTS ExportsItem [, ExportsItem]...
ExportsItem -> Ident [NAME|INDEX "'" ConstExpr "'"]
                     [INDEX|NAME "'" ConstExpr "'"]
DeclSection -> LabelDeclSection
            -> ConstSection
            -> TypeSection
            -> VarSection
            -> ProcedureDeclSection
LabelDeclSection -> LABEL LabelId
ConstSection -> CONST (ConstantDecl ';')...
ConstantDecl -> Ident '=' ConstExpr [HintDirective]
             -> Ident ':' TypeId '=' TypedConstant [HintDirective]
TypeSection -> TYPE (TypeDecl ';')
TypeDecl -> Ident '=' [TYPE] Type [HintDirective]
         -> Ident '=' [TYPE] RestrictedType [HintDirective]
TypedConstant -> (ConstExpr | ArrayConstant | RecordConstant)
ArrayConstant -> '(' TypedConstant ',' ')'
RecordConstant -> '(' RecordFieldConstant ';'... ')'
RecordFieldConstant -> Ident ':' TypedConstant
Type -> TypeId
     -> SimpleType
     -> StrucType
     -> PointerType
     -> StringType
     -> ProcedureType
     -> VariantType
     -> ClassRefType
RestrictedType -> ObjectType
               -> ClassType
               -> InterfaceType
ClassRefType -> CLASS OF TypeId
SimpleType -> (OrdinalType | RealType)
RealType ->  REAL48
         ->  REAL
         ->  SINGLE
         ->  DOUBLE
         ->  EXTENDED
         ->  CURRENCY
         ->  COMP
OrdinalType -> (SubrangeType | EnumeratedType | OrdIdent)
OrdIdent ->  SHORTINT
         ->  SMALLINT
         ->  INTEGER
         ->  BYTE
         ->  LONGINT
         ->  INT64
         ->  WORD
         ->  BOOLEAN
         ->  CHAR
         ->  WIDECHAR
         ->  LONGWORD
         ->  PCHAR
VariantType -> VARIANT
            -> OLEVARIANT
SubrangeType -> ConstExpr '..' ConstExpr
EnumeratedType -> '(' EnumeratedTypeElement ','... ')'
EnumeratedTypeElement -> Ident [ '=' ConstExpr ]
StringType -> STRING
           -> ANSISTRING
           -> WIDESTRING
           -> STRING '[' ConstExpr ']'
           -> UNICODESTRING
StrucType -> [PACKED] (ArrayType | SetType | FileType | RecType [PACKED])
ArrayType -> ARRAY ['[' OrdinalType ','... ']'] OF Type [HintDirective]
RecType -> RECORD [FieldList] END [HintDirective]
FieldList -> FieldDecl ';'... [VariantSection] [';']
FieldDecl -> IdentList ':' Type [HintDirective]
VariantSection -> CASE [Ident ':'] TypeId OF RecVariant ';'...
RecVariant -> ConstExpr ','... ':' '(' [FieldList] ')'
SetType -> SET OF OrdinalType [HintDirective]
FileType -> FILE OF TypeId [HintDirective]
PointerType -> '^' TypeId [HintDirective]
ProcedureType -> (ProcedureHeading | FunctionHeading) [OF OBJECT]
VarSection -> VAR (VarDecl ';')...
VarDecl
  On Windows -> IdentList ':' Type [(ABSOLUTE (Ident | ConstExpr)) | '=' ConstExpr] [HintDirective]
  On Linux   -> IdentList ':' Type [ABSOLUTE (Ident) | '=' ConstExpr] [HintDirective]
Expression -> SimpleExpression [RelOp SimpleExpression]...
SimpleExpression -> ['+' | '-'] Term [AddOp Term]...
Term -> Factor [MulOp Factor]...
Factor ->  Designator ['(' ExprList ')']
        -> '@' Designator
        -> Number
        -> String
        -> NIL
        -> '(' Expression ')'
        -> NOT Factor
        -> SetConstructor
        -> TypeId '(' Expression ')'
RelOp ->  '>'
      ->  '<'
      ->  '<='
      ->  '>='
      ->  '<>'
      ->  IN
      ->  IS
      ->  AS
AddOp ->  '+'
      ->  '-'
      ->  OR
      ->  XOR
MulOp ->  '*'
      ->  '/'
      ->  DIV
      ->  MOD
      ->  AND
      ->  SHL
      ->  SHR
Designator -> QualId ['.' Ident | '[' ExprList ']' | '^']...
SetConstructor -> '[' [SetElement ','...] ']'
SetElement -> Expression ['..' Expression]
ExprList -> Expression ','...
Statement -> [LabelId ':'] [SimpleStatement | StructStmt]
StmtList -> Statement ';'
SimpleStatement -> Designator ['(' [ExprList] ')']
                -> Designator ':=' Expression
                -> INHERITED
                -> GOTO LabelId
StructStmt -> CompoundStmt
           -> ConditionalStmt
           -> LoopStmt
           -> WithStmt
           -> TryExceptStmt
           -> TryFinallyStmt
           -> RaiseStmt
           -> AssemblerStmt
CompoundStmt -> BEGIN StmtList END
ConditionalStmt -> IfStmt
                -> CaseStmt
IfStmt -> IF Expression THEN Statement [ELSE Statement]
CaseStmt -> CASE Expression OF CaseSelector ';'... [ELSE StmtList] [';'] END
CaseSelector -> CaseLabel ','... ':' Statement
CaseLabel -> ConstExpr ['..' ConstExpr]
LoopStmt -> RepeatStmt
         -> WhileStmt
         -> ForStmt
         -> ForInStmt
RepeatStmt -> REPEAT Statement UNTIL Expression
WhileStmt -> WHILE Expression DO Statement
ForStmt -> FOR QualId ':=' Expression (TO | DOWNTO) Expression DO Statement
ForInStmt -> FOR QualId IN Expression DO Statement
WithStmt -> WITH IdentList DO Statement
TryExceptStmt -> TRY
                   Statement...
                 EXCEPT
                   ExceptionBlock
                 END
ExceptionBlock -> [ON [Ident ':'] TypeID DO Statement]...
                  [ELSE Statement...]
TryFinallyStmt -> TRY
                    Statement
                  FINALLY
                    Statement
                  END
RaiseStmt -> RAISE [object] [AT address]
AssemblerStatement -> ASM
                   -> <assemblylanguage>
                   -> END
ProcedureDeclSection -> ProcedureDecl
                     -> FunctionDecl
ProcedureDecl -> ProcedureHeading ';' [Directive] [HintDirective]
                 Block ';'
FunctionDecl -> FunctionHeading ';' [Directive] [HintDirective]
                Block ';'
FunctionHeading -> FUNCTION Ident [FormalParameters] ':' (SimpleType | STRING)
ProcedureHeading -> PROCEDURE Ident [FormalParameters]
FormalParameters -> '(' [FormalParm ';'...] ')'
FormalParm -> [VAR | CONST | CONSTREF | OUT] Parameter
Parameter -> IdentList [':' ([ARRAY OF] SimpleType | STRING | FILE)]
          -> Ident ':' SimpleType '=' ConstExpr
Directive ->  CDECL
          ->  REGISTER
          ->  DYNAMIC
          ->  VIRTUAL
          ->  EXPORT
          ->  EXTERNAL
          ->  NEAR
          ->  FAR
          ->  FORWARD
          ->  MESSAGE ConstExpr
          ->  OVERRIDE
          ->  OVERLOAD
          ->  PASCAL
          ->  REINTRODUCE
          ->  SAFECALL
          ->  STDCALL
          ->  VARARGS
          ->  LOCAL
          ->  ABSTRACT
ObjectType -> OBJECT [ObjHeritage] [ObjFieldList] [MethodList] END
ObjHeritage -> '(' QualId ')'
MethodList -> (MethodHeading [';' VIRTUAL]) ';'...
MethodHeading ->  ProcedureHeading
              ->  FunctionHeading
              ->  ConstructorHeading
              ->  DestructorHeading
ConstructorHeading -> CONSTRUCTOR Ident [FormalParameters]
DestructorHeading -> DESTRUCTOR Ident [FormalParameters]
ObjFieldList -> (IdentList ':' Type) ';'
InitSection -> INITIALIZATION StmtList [FINALIZATION StmtList] END
            -> BEGIN StmtList END
            -> END
ClassType -> CLASS [ClassHeritage]
             [ClassVisibility]
             [ClassFieldList]
             [ClassMethodList]
             [ClassPropertyList]
             END
ClassHeritage -> '(' IdentList ')'
ClassVisibility -> [[STRICT] PRIVATE | PROTECTED | PUBLIC | PUBLISHED]
ClassFieldList -> (ClassVisibility ObjFieldList) ';'...
ClassMethodList -> (ClassVisibility MethodList) ';'...
ClassPropertyList -> (ClassVisibility PropertyList ';')...
PropertyList -> PROPERTY Ident [PropertyInterface] [PropertySpecifiers] [HintDirective]
PropertyInterface -> [PropertyParameterList] ':' Ident
PropertyParameterList -> '[' (IdentList ':' TypeId) ';'... ']'
PropertySpecifiers -> [INDEX ConstExpr]
                      [READ Ident]
                      [WRITE Ident]
                      [STORED (Ident | Constant)]
                      [(DEFAULT ConstExpr) | NODEFAULT]
                      [IMPLEMENTS TypeId]
InterfaceType -> INTERFACE [InterfaceHeritage]
                 [ClassMethodList]
                 [ClassPropertyList]
                 ...
                 END
InterfaceHeritage -> '(' IdentList ')'
RequiresClause -> REQUIRES IdentList... ';'
ContainsClause -> CONTAINS IdentList... ';'
IdentList -> Ident ','...
QualId -> [UnitId '.'] Ident
TypeId -> [UnitId '.'] <type-identifier>
Ident -> <identifier>
ConstExpr -> <constant-expression>
UnitId -> <unit-identifier>
LabelId -> <label-identifier>
Number -> <number>
String -> <string>

:ecgraphic.

:euserdoc.

