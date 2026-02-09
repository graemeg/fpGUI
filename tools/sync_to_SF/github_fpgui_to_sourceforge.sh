#!/bin/sh
# Fetches latest revisions for fpGUI on Github and
# pushes the changes to SourceForge.
# Created by Graeme Geldenhuys - 2015

GIT="/usr/local/bin/git"

# fpGUI repository
cd /data/git/fpgui.git/
$GIT fetch github
$GIT push sourceforge

