#!/bin/bash

# Ease up the process of having nice icons for INF files in Nautilus,
# and for launching the correct application when you click on those
# INF files. This script also adds DocView in the Applications menu.
#                - Graeme Geldenhuys (graemeg@gmail.com)
#
# This was tested and working on Ubuntu 10.04

cp x-docview-extension-inf.xml ~/.local/share/mime/packages/
cp docview.desktop ~/.local/share/applications/
cp ../images/inf-book-48x48.png ~/.icons/application-x-docview-extension-inf.png
cp ../images/docview-48x48.png ~/.icons/docview.png

# this registers the mime-type and related icon
update-mime-database ~/.local/share/mime/

# this registers the applications (*.desktop) with the mime-type
update-desktop-database ~/.local/share/applications/
