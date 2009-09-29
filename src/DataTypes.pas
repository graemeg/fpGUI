Unit DataTypes;

{$mode objfpc}{$H+}
{$ASMMODE intel}

// NewView - a new OS/2 Help Viewer
// Copyright 2001 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Just defines various types useful in manipulating help files.

type
  int32 = longword;
  int16 = word;
  int8 = byte;
  pInt16 = ^int16;
  pInt32 = ^int32;
  pInt8 = ^byte;

  PCharArray = array[ 0..0 ] of PCHar;
  Int32Array = array[ 0..0 ] of Int32;
  Int16Array = array[ 0..0 ] of Int16;
  Int8Array = array[ 0..0 ] of Int8;

  PCharArrayPointer = ^PCharArray;
  Int32ArrayPointer = ^Int32Array;
  Int16ArrayPointer = ^Int16Array;
  Int8ArrayPointer = ^Int8Array;

  TBooleanArray = array[ 0..0 ] of boolean;
  BooleanArrayPointer = ^TBooleanArray;

procedure FillInt32Array( pArray: Int32ArrayPointer;
                          Size: longint;
                          Value: Int32 );

Implementation

// This is a nice fast implementation of filling an
// array of dwords (Int32/longword)
procedure FillInt32Array( pArray: Int32ArrayPointer;
                          Size: longint;
                          Value: Int32 );
begin
  assert( Size > 0 );
  Asm
    Mov EAX, Value
    Mov EDI, pArray
    Mov ECX, Size
    CLD        // direction = up
    REP STOSD   // store double word, until ECX = 0
  End;
end;

Initialization
End.
