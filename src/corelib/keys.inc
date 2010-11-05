{
  We use GGI's key definitions; the names just
  have been somewhat pascalified...
  GGI info at <http://www.ggi-project.org>
  GII info at <http://www.ggi-project.org/packages/libgii.html>
}

{%mainunit fpg_base.pas}

const

  // ASCII keys

  keyNul		      = $00;
  keyBackSpace		= $08;
  keyTab		      = $09;
  keyLinefeed		  = $0a;
  keyReturn		    = $0d;
  keyEnter        = keyReturn;  // alias
  keyEscape		    = $1b;
  keyDelete		    = $7f;
  keySpace        = $20;

  // special keys

  keyVoid		      = $e000;
  keyBreak		    = $e005;
  keyScrollForw 	= $e00a;
  keyScrollBack 	= $e00b;
  keyBoot		      = $e00c;
  keyCompose		  = $e00e;
  keySAK		      = $e00f;
  keyUndo		      = $e017;
  keyRedo		      = $e018;
  keyMenu		      = $e019;
  keyCancel		    = $e01a;
  keyPrintScreen	= $e01b;
  keyExecute		  = $e01c;
  keyFind		      = $e01e;
  keyBegin		    = $e01f;
  keyClear		    = $e020;
  keyInsert		    = $e022;
  keySelect		    = $e023;
  keyMacro		    = $e026;
  keyHelp		      = $e027;
  keyDo			      = $e028;
  keyPause		    = $e029;
  keyStop		      = keyPause;
  keySysRq		    = $e02a;
  keyModeSwitch 	= $e02b;
  keyUp			      = $e032;
  keyDown		      = $e033;
  keyLeft		      = $e034;
  keyRight		    = $e035;
  keyPrior		    = $e036;
  keyPageUp		    = keyPrior;
  keyNext		      = $e037;
  keyPageDown		  = keyNext;
  keyHome		      = $e038;
  keyEnd		      = $e039;


  // function keys

  keyF0			= $e100;
  keyF1			= $e101;
  keyF2			= $e102;
  keyF3			= $e103;
  keyF4			= $e104;
  keyF5			= $e105;
  keyF6			= $e106;
  keyF7			= $e107;
  keyF8			= $e108;
  keyF9			= $e109;
  keyF10		= $e10a;
  keyF11		= $e10b;
  keyF12		= $e10c;
  keyF13		= $e10d;
  keyF14		= $e10e;
  keyF15		= $e10f;
  keyF16		= $e110;
  keyF17		= $e111;
  keyF18		= $e112;
  keyF19		= $e113;
  keyF20		= $e114;
  keyF21		= $e115;
  keyF22		= $e116;
  keyF23		= $e117;
  keyF24		= $e118;
  keyF25		= $e119;
  keyF26		= $e11a;
  keyF27		= $e11b;
  keyF28		= $e11c;
  keyF29		= $e11d;
  keyF30		= $e11e;
  keyF31		= $e11f;
  keyF32		= $e120;
  keyF33		= $e121;
  keyF34		= $e122;
  keyF35		= $e123;
  keyF36		= $e124;
  keyF37		= $e125;
  keyF38		= $e126;
  keyF39		= $e127;
  keyF40		= $e128;
  keyF41		= $e129;
  keyF42		= $e12a;
  keyF43		= $e12b;
  keyF44		= $e12c;
  keyF45		= $e12d;
  keyF46		= $e12e;
  keyF47		= $e12f;
  keyF48		= $e130;
  keyF49		= $e131;
  keyF50		= $e132;
  keyF51		= $e133;
  keyF52		= $e134;
  keyF53		= $e135;
  keyF54		= $e136;
  keyF55		= $e137;
  keyF56		= $e138;
  keyF57		= $e139;
  keyF58		= $e13a;
  keyF59		= $e13b;
  keyF60		= $e13c;
  keyF61		= $e13d;
  keyF62		= $e13e;
  keyF63		= $e13f;
  keyF64		= $e140;


  // keys on the numeric keypad
  keyP0			= $e230;
  keyP1			= $e231;
  keyP2			= $e232;
  keyP3			= $e233;
  keyP4			= $e234;
  keyP5			= $e235;
  keyP6			= $e236;
  keyP7			= $e237;
  keyP8			= $e238;
  keyP9			= $e239;
  keyPA			= $e241;
  keyPB			= $e242;
  keyPC			= $e243;
  keyPD			= $e244;
  keyPE			= $e245;
  keyPF			= $e246;
  keyPPlus		    = $e200 + Ord('+');
  keyPMinus		    = $e200 + Ord('-');
  keyPSlash		    = $e200 + Ord('/');
  keyPAsterisk		= $e200 + Ord('*');
  keyPStar		    = keyPAsterisk;
  keyPEqual		    = $e200 + Ord('=');
  keyPSeparator		= $e200 + Ord(',');
  keyPDecimal		  = $e200 + Ord('.');
  keyPParenLeft		= $e200 + Ord('(');
  keyPParenRight	= $e200 + Ord(')');
  keyPSpace		    = $e200 + Ord(' ');
  keyPEnter		    = $e20d;
  keyPTab		      = $e208;
  keyPPlusMinus		= $e280;
  keyPBegin		    = $e281;
  keyPF1		= $e291;
  keyPF2		= $e292;
  keyPF3		= $e293;
  keyPF4		= $e294;
  keyPF5		= $e295;
  keyPF6		= $e296;
  keyPF7		= $e297;
  keyPF8		= $e298;
  keyPF9		= $e299;


  // modifier keys

  keyShift		= $e300;
  keyCtrl		  = $e301;
  keyAlt		  = $e302;
  keyMeta		  = $e303;
  keySuper		= $e304;
  keyHyper		= $e305;
  keyAltGr		= $e306;
  keyCaps		  = $e307;
  keyNum		  = $e308;
  keyScroll		= $e309;


  // modifier _labels_

  keyShiftL		    = $e300;
  keyShiftR		    = $e340;
  keyCtrlL		    = $e301;
  keyCtrlR		    = $e341;
  keyAltL		      = $e302;
  keyAltR		      = $e342;
  keyMetaL		    = $e303;
  keyMetaR		    = $e343;
  keySuperL		    = $e304;
  keySuperR		    = $e344;
  keyHyperL		    = $e305;
  keyHyperR		    = $e345;
  keyShiftLock		= $e380;
  keyCtrlLock		  = $e381;
  keyAltLock		  = $e382;
  keyMetaLock		  = $e383;
  keySuperLock		= $e384;
  keyHyperLock		= $e385;
  keyAltGrLock		= $e386;
  keyCapsLock		  = $e387;
  keyNumLock		  = $e388;
  keyScrollLock		= $e389;

  // Dead keys
  keyDeadRing		          = $e400;
  keyDeadCaron		        = $e401;
  keyDeadOgonek		        = $e402;
  keyDeadIota		          = $e403;
  keyDeadDoubleAcute	    = $e404;
  keyDeadBreve		        = $e405;
  keyDeadAboveDot	        = $e406;
  keyDeadBelowDot	        = $e407;
  keyDeadVoicedSound	    = $e408;
  keyDeadSemiVoicedSound  = $e409;


  keyDeadAcute		  = $e4b4;
  keyDeadCedilla	  = $e4b8;
  keyDeadCircumflex	= $e45e;
  keyDeadDiaeresis	= $e4a8;
  keyDeadGrave		  = $e460;
  keyDeadTilde		  = $e47e;
  keyDeadMacron		  = $e4af;

  // miscellaneous
  keyNIL		= $ffff;	// used to indicate "not mapped yet"


  // currencies (NOTE: These values are not defined in GGI/GII!
  // !!!: Check if GII has got currency codes in the meantime
  keyEcuSign		    = $e500;
  keyColonSign		  = $e501;
  keyCruzeiroSign	  = $e502;
  keyFFrancSign		  = $e503;
  keyLiraSign		    = $e504;
  keyMillSign		    = $e505;
  keyNairaSign		  = $e506;
  keyPesetaSign		  = $e507;
  keyRupeeSign		  = $e508;
  keyWonSign		    = $e509;
  keyNewSheqelSign	= $e50a;
  keyDongSign		    = $e50b;
  keyEuroSign		    = $e50c;



