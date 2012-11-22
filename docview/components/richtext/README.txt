TRichTextView component
for fpGUI Toolkit


Summary
-------
This component displays “rich text”, with various fonts, colors,
styles and alignment.

The major features are:
  Fast, accurate drawing of text

Features to come…
  Selection and copy
  Built-in default popup menu


Using the component
-------------------
Put a component on your form. Adjust the properties as you see fit.
At runtime, load the text into the control using AddText, AddParagraph, 
and Clear. 


Formatting syntax
-----------------
This is a HTML-like set of tags. But note that tag pairs don't have to
match up.

Implemented tags:

  <b> </b>           bold on, off
  <u> </u>           underline on, off
  <i> </i>           italic on, off
  <h1> <h2> <h3>     heading 1–3, set with Heading1Font etc
  </h>               changes heading text back to normal text
  <tt> </tt>         fixed font
  <color x> </color> set text to the color xxx, where xxx is a convenience
                     color (red, green, blue, yellow, black, purple, cyan) or a
                     color value #RRGGBB like #008000 (which is a shade of
                     green).
  <red>              sets text color to red (convenience tag)
  <blue>             sets text color to blue (convenience tag)
  <green>            sets text color to green (convenience tag)
  <black>            sets text color to black (convenience tag)
  <backcolor xxx>    sets the background color of text to xxx
    </backcolor>
  <align>            default left text alignment
  <align left>       Same as above. Left text alignment
  <align center>     Centered text alignment
  <align right>      Right text alignment
  <leftmargin xx>    Text will begin xx spaces from the left
  <link linktext> </link>
                     Start, End link.
                     The OnClickLink and OnOverLink events are called with linktext
  <image x>          Display image x from associated TImageList
  <wrap no>          No text wrapping will occur
  <wrap yes>         Text wrapping is enabled again.
  <font "font name" x>
                     Where "font name" is the name of the font, and x is the
                     point size of the font.


Not implemented yet:
  <justify>          full text justification
  <rightmargin xx>   Text will end xx spaces from the right
  <defaultalign>     Set alignment back to the default RichTextSettings.DefaultAlignment
                     value


Example
-------
RichText.AddParagraph( '<h1>This is a big heading</h>' );  
RichText.AddParagraph( 'Here is some <b>bold</b> text' );  
  

Problems/limitations
--------------------
Yes, there probably are some. :)

