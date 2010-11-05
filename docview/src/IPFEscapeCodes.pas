Unit IPFEscapeCodes;

{$mode objfpc}{$H+}


// NewView - a new OS/2 Help Viewer
// Copyright 2001 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

uses
  Classes;

// List of IPF escape codes. Not complete! Many are just used
// as magic numbers in HelpTopic.pas

const
  // Basic byte codes
  IPF_END_PARA = $fa;
  IPF_CENTER = $fb;
  IPF_INVERT_SPACING = $fc;
  IPF_LINEBREAK = $fd;
  IPF_SPACE = $fe;
  IPF_ESC = $ff;

  // Subescape codes of
  HPART_DEFINE = 0;
  HPART_PT_HDREF = 1;
  HPART_PT_FNREF = 2;
  HPART_PT_SPREF = 3;
  HPART_HDREF = 4;
  HPART_FNREF = 5;
  HPART_SPREF = 6;
  HPART_LAUNCH = 7;
  HPART_PT_LAUNCH = 8;
  HPART_INFORM = 9;
  HPART_PT_INFORM = 10;
  // ?? 11 ??
  HPART_EXTERN_PT_HDREF = 12;
  HPART_EXTERN_PT_SPREF = 13;
  HPART_EXTERN_HDREF = 14;
  HPART_EXTERN_SPREF = 15;
  HPART_GLOBAL_HDREF = 16;
  HPART_GLOBAL_PT_HDREF = 17;

Implementation

Initialization
End.
