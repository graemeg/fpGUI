:userdoc.
:title.Maximus IDE Help

.nameit symbol=max text='Maximus IDE'
.nameit symbol=fpgui text='fpGUI Toolkit'


.* ************************************************************
.* Introduction
.* ************************************************************
:h1.Welcome
:p.
&max. is a sample application of the &fpgui.. Due to most
of my paid work being commercial and closed sourced, I wanted to find another
way of showing others what &fpgui. is capable of. With &max.
I am trying to show that - a more advanced application [than the rest of the
demos] written with &fpgui.. If &max. turns out to be a useful product, consider
that a bonus.


.* ************************************************************
.* Sample Regular Expressions
.* ************************************************************
:h1.Sample Regular Expressions
:p.
Here is a quick summary of some of the most used control characters used
in regular expressions.

:table rules=both frame=box cols='15 80'.
:row.
:c.:hp2.EXPRESSION:ehp2.
:c.:hp2.MEANING:ehp2.
:row.
:c.^abc
:c.Match "abc" at beginning of line
:row.
:c.abc$
:c.Match "abc" at end of line
:row.
:c.^abc$
:c.Match the line "abc" exactly
:row.
:c.^\s*abc
:c.Match "abc" at beginning of line, but allow leading whitespace
:row.
:c.^\s*end;?\s*$
:c.Match a line containing only "end" or "end;" with leading or trailing whitespace
:row.
:c.abc|def
:c.Matches either "abc" or "def"
:row.
:c.a(b|c)d
:c.Matches "abd" or "acd"
:row.
:c.a(b|c)d\1
:c.Matches "abdb" or "acdc", but it does not match "abdc"
:etable.

:p.
Control characters used in above samples

:table rules=both frame=box cols='15 40'.
:row.
:c.:hp2.CHARACTER:ehp2.
:c.:hp2.MEANING:ehp2.
:row.
:c.^
:c.Beginning of line
:row.
:c.$
:c.End of line
:row.
:c.\s
:c.Whitespace (a tab or space)
:row.
:c.*
:c.Zero or more of the preceding character
:row.
:c.?
:c.Preceding character is optional
:row.
:c.|
:c.Alternative expression
:row.
:c.( )
:c.Subexpressions
:row.
:c.\1
:c.Back reference to 1st subexpression
:etable.

:p.
:hp2.See also:ehp2.
.br
:link reftype=hd refid=100.Regular Expression Reference:elink.


.* ************************************************************
.* Regular Expressions Reference
.* ************************************************************
:h1 id=100.Regular Expression Reference
:p.
Regular Expressions are a powerful way to define patterns for searching and 
matching. &max. allows you to use regular expressions when searching 
through text, and when specifying rules for classifying text. The regular 
expression support in &max. is a subset of the Perl Compatible Regular 
Expression (PCRE) syntax.

:p.
While Regular Expressions can be a complex topic, there are several excellent
resources about them. One such resource is a book called :hp1.Mastering Regular 
Expressions:ehp1.. Another excellent resource is Steve Mansour's :hp1.A Tao of
Regular Expressions:ehp1., a copy of which can be found at:

:p.
:lm margin=4.
    www.scootersoftware.com/RegEx.html
:lm margin=0.

:p.
A regular expression is composed of two types of characters: normal characters 
and metacharacters. When performing a match, metacharacters take on special 
meanings, controlling how the match is made and serving as wildcards. Normal 
characters always match against only themselves. To match against a 
metacharacter, escape it, by prefixing it with a backslash "\". There are 
multiple types of metacharacters, each detailed below.

:euserdoc.

