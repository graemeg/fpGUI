------[Forwarded message from Jean-Marc Levecque]------

Hi,

Here is a first step in embedding true type fonts in the report tool.

Attached is a set of patches for u_pdf and u_demo to show how it works.

In addition, I wrote a utility to create the definition file for any ttf
file, called makefonts. I gave the *.fnt extension to the generated font
definition file.

A major problem is to compress the ttf file.
From the site fpdf.org, I found a way to get this compressed file done
by use of the zlib library.
If anybody knows how to do the equivalent compression using fpc, that
would be really great.
For now, on this site, going to Tutorials>Tutorial 7, then down to see
the link to "on line", one can select a ttf file, choose the encoding
and download the *.z file which is the compressed embeddable file for pdf.

To simplify the tests, I have attached the *.fnt and *.z files for all
DejaVu and Liberation fonts I have on my distribution. These files can
be put anywhere, as the demo requires to select the directoy containing
them.
They are all encoded with cp1252 which is a Microsoft extension of
ISO-8859-1, and it would be easy to get any other encoding.

I also tried to use Microsoft Comic font, but despite they are installed
on my PC, they do not show up correctly in the preview, while due to the
embedding, they display correctly in pdf.

Trying to embed uncompressed ttf files does not seem to work, but as per
pdf specification, it should. I must have missed something :(

Another tool may be used if one wants to use a font from a different
format: from the site freeconverter.com, it is possible to convert a
font file from a format to another one, with a large choice of formats.

Remember that font licence may not allow embedding.

Best regards
Jean-Marc
