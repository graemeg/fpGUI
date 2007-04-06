
{%mainunit fpgui.pp}


  { NOTE:
      The colors commented out below are not applicable to systems other than
      Windows. For this reason I don't see the need to define them. Under Linux,
      the Windows Manager theme will draw those parts anyway and we have no
      control over that or the colors used. }

  // UI element colors
  clScrollBar       = TColor($80000000);
//  clBackground          = TColor($80000001);
//  clActiveCaption       = TColor($80000002);
//  clInactiveCaption     = TColor($80000003);
  clMenu            = TColor($80000004);
  clWindow          = TColor($80000005);
//  clWindowFrame         = TColor($80000006);
  clMenuText        = TColor($80000007);
  clWindowText      = TColor($80000008);
//  clCaptionText         = TColor($80000009);
//  clActiveBorder        = TColor($8000000a);
//  clInactiveBorder      = TColor($8000000b);
  clAppWorkSpace    = TColor($8000000c);
  clHighlight       = TColor($8000000d);
  clHighlightText   = TColor($8000000e);
  cl3DFace          = TColor($8000000f);
  cl3DShadow        = TColor($80000010);
  clGrayText        = TColor($80000011);
  clBtnText         = TColor($80000012);
//  clInactiveCaptionText = TColor($80000013);
  cl3DHighlight     = TColor($80000014);
  cl3DDkShadow      = TColor($80000015);
  cl3DLight         = TColor($80000016);
  clInfoText        = TColor($80000017);
  clInfoBk          = TColor($80000018);

  // The following colors match the predefined Delphi Colors
  clBlack           = TColor($000000);
  clMaroon          = TColor($000080);
  clGreen           = TColor($008000);
  clOlive           = TColor($008080);
  clNavy            = TColor($800000);
  clPurple          = TColor($800080);
  clTeal            = TColor($808000);
  clGray            = TColor($808080);
  clSilver          = TColor($C0C0C0);
  clRed             = TColor($0000FF);
  clLime            = TColor($00FF00);
  clYellow          = TColor($00FFFF);
  clBlue            = TColor($FF0000);
  clFuchsia         = TColor($FF00FF);
  clAqua            = TColor($FFFF00);
  clLtGray          = TColor($C0C0C0);
  clDkGray          = TColor($808080);
  clWhite           = TColor($FFFFFF);
  clCream           = TColor($F0FBFF);
  clMoneyGreen      = TColor($C0DCC0);
  clSkyBlue         = TColor($F0CAA6);
//  clNone            = TColor($1FFFFFFF);
//  clDefault         = TColor($20000000);

  // alias color names
  clBtnFace         = cl3DFace;
  clBtnShadow       = cl3DShadow;
  clBtnHighlight    = cl3DHighlight;
  clCyan            = clAqua;
  clMagenta         = clFuchsia;

  // web/html named colors - De-facto NS & MSIE recognized HTML color names
  clAliceBlue   	  = TColor($FFF8F0);
  clAntiqueWhite   	= TColor($D7EBFA);
//  clAqua    	      = TColor($FFFF00);
  clAquamarine   	  = TColor($D4FF7F);
  clAzure   	      = TColor($FFFFF0);
  clBeige           = TColor($DCF5F5);
  clBisque   	      = TColor($C4E4FF);
//  clBlack         = TColor($000000);
  clBlanchedAlmond 	= TColor($CDEBFF);
//  clBlue    	 #0000ff
  clBlueViolet   	  = TColor($E22B8A);
  clBrown   	      = TColor($2A2AA5);
  clBurlyWood   	  = TColor($87B8DE);
  clCadetBlue   	  = TColor($A09E5F);
  clChartreuse   	  = TColor($00FF7F);
  clChocolate   	  = TColor($1E69D2);


  clMedGray         = TColor($A4A0A0);
  clForestGreen     = TColor($228B22);
  clRoyalBlue       = TColor($E16941);
  clLightYellow     = TColor($E0FFFF);
  clCornsilk        = TColor($DCF8FF);
  clMidnightBlue    = TColor($701919);
  clDarkWhite       = TColor($E0E0E0);

{
# De-facto NS & MSIE recognized HTML color names
Coral   	 #ff7f50
CornflowerBlue 	 #6495ed
Cornsilk   	 #fff8dc
Crimson   	 #dc143c
Cyan    	 #00ffff
DarkBlue   	 #00008b
DarkCyan   	 #008b8b
DarkGoldenrod  	 #b8860b
DarkGray   	 #a9a9a9
DarkGreen   	 #006400
DarkKhaki   	 #bdb76b
DarkMagenta   	 #8b008b
DarkOliveGreen 	 #556b2f
DarkOrange   	 #ff8c00
DarkOrchid   	 #9932cc
DarkRed   	 #8b0000
DarkSalmon   	 #e9967a
DarkSeaGreen   	 #8fbc8f
DarkSlateBlue  	 #483d8b
DarkSlateGray  	 #2f4f4f
DarkTurquoise  	 #00ced1
DarkViolet   	 #9400d3
DeepPink   	 #ff1493
DeepSkyBlue   	 #00bfff
DimGray   	 #696969
DodgerBlue   	 #1e90ff
FireBrick   	 #b22222
FloralWhite   	 #fffaf0
ForestGreen   	 #228b22
Fuchsia   	 #ff00ff
Gainsboro   	 #dcdcdc
GhostWhite   	 #f8f8ff
Gold    	 #ffd700
Goldenrod   	 #daa520
Gray    	 #808080
Green   	 #008000
GreenYellow   	 #adff2f
Honeydew   	 #f0fff0
HotPink   	 #ff69b4
IndianRed   	 #cd5c5c
Indigo   	 #4b0082
Ivory   	 #fffff0
Khaki   	 #f0e68c
Lavender   	 #e6e6fa
LavenderBlush  	 #fff0f5
LawnGreen   	 #7cfc00
}
  clLemonChiffon    = TColor($CDFAFF);   //  #fffacd
{
LightBlue   	 #add8e6
LightCoral   	 #f08080
LightCyan   	 #e0ffff
LightGoldenrodYellow	#fafad2
LightGreen   	 #90ee90
}
  clLightGrey    = TColor($D3D3D3);   // #d3d3d3
{
LightPink   	 #ffb6c1
LightSalmon   	 #ffa07a
LightSeaGreen  	 #20b2aa
LightSkyBlue   	 #87cefa
LightSlateGray 	 #778899
LightSteelBlue 	 #b0c4de
LightYellow   	 #ffffe0
Lime    	 #00ff00
LimeGreen   	 #32cd32
Linen   	 #faf0e6
Magenta   	 #ff00ff
Maroon   	 #800000
MediumAquamarine #66cdaa
MediumBlue   	 #0000cd
MediumOrchid   	 #ba55d3
MediumPurple   	 #9370db
MediumSeaGreen 	 #3cb371
MediumSlateBlue	 #7b68ee
MediumSpringGreen	#00fa9a
MediumTurquoise	 #48d1cc
MediumVioletRed	 #c71585
MidnightBlue   	 #191970
MintCream   	 #f5fffa
MistyRose   	 #ffe4e1
Moccasin   	 #ffe4b5
NavajoWhite   	 #ffdead
Navy    	 #000080
OldLace   	 #fdf5e6
Olive   	 #808000
OliveDrab   	 #6b8e23
Orange   	 #ffa500
OrangeRed   	 #ff4500
Orchid   	 #da70d6
PaleGoldenrod  	 #eee8aa
PaleGreen   	 #98fb98
PaleTurquoise  	 #afeeee
PaleVioletRed  	 #db7093
PapayaWhip   	 #ffefd5
PeachPuff   	 #ffdab9
Peru    	 #cd853f
Pink    	 #ffc0cb
Plum    	 #dda0dd
PowderBlue   	 #b0e0e6
Purple   	 #800080
Red     	 #ff0000
RosyBrown   	 #bc8f8f
RoyalBlue   	 #4169e1
SaddleBrown   	 #8b4513
Salmon   	 #fa8072
SandyBrown   	 #f4a460
SeaGreen   	 #2e8b57
Seashell   	 #fff5ee
Sienna   	 #a0522d
Silver   	 #c0c0c0
SkyBlue   	 #87ceeb
SlateBlue   	 #6a5acd
SlateGray   	 #708090
Snow    	 #fffafa
SpringGreen   	 #00ff7f
SteelBlue   	 #4682b4
Tan     	 #d2b48c
Teal    	 #008080
Thistle   	 #d8bfd8
Tomato   	 #ff6347
Turquoise   	 #40e0d0
Violet   	 #ee82ee
Wheat   	 #f5deb3
White   	 #ffffff
WhiteSmoke   	 #f5f5f5
Yellow   	 #ffff00
YellowGreen   	 #9acd32
}


