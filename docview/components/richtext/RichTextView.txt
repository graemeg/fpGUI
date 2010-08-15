TRichTextView component
for fpGUI Toolkit

Summary
-------

This component displays 'rich' text, with various fonts, colors, 
styles and alignment.

The major features are:
  Fast, accurate drawing of text

Features to come...
  Selection and copy
  Built-in default popup menu


Using the component
-------------------

Put a component on your form. Adjust the properties as you see fit.
At runtime, load the text into the control using AddText, AddParagraph, 
and Clear. 

Formatting syntax

This is a HTML-like set of tags. But note that tag pairs don't have to
match up.

  <b> </b>       bold on, off
  <u> </u>       underline on, off
  <i> </i>       italic on, off
  <h1> <h2> <h3> heading 1-3, set with Heading1Font etc
  </h>           normal text
  <tt> </tt>     fixed font 
  <red> etc      colors
  <left>         left alignment (word wrap)
  <unaligned>    no right margin
  <center>       centered
  <right>        right alignment
  <justify>      full justification (not implemented)
  <defaultalign> default alignment
  <margin x>     set left margin to x pixels
  <link linktext> </link>
    start, end link. 
    The OnClickLink and OnOverLink events are called with linktext
  <image x>      Display image x from associated TImageList
    

Example

RichText.AddParagraph( '<h1>This is a big heading</h>' );  
RichText.AddParagraph( 'Here is some <b>bold</b> text' );  
  

Problems/limitations
--------------------
Yes, there probably are some. :)


