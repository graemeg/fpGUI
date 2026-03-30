{$mode objfpc}{$H+}
{$inline on}

{$ifdef fpc}
  {$define UsePChar}
  {$define HasInt64}
  {$define UseAnsiStrings}
  {$define HasStreams}
  {$IF FPC_FULLVERSION<30101}
    {$define EmulateArrayInsert}
  {$endif}
  {$define HasFS}
{$endif}

{$IFDEF NODEJS}
  {$define HasFS}
{$ENDIF}

{$IF FPC_FULLVERSION>30100}
  {$warn 6058 off : cannot inline}
  {$WARN 6018 off : Unreachable code}
{$ENDIF}

