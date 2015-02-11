unit gui_mig_exceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type

  EIllegalArgument = class(Exception);
  EIllegalAccess = class(Exception);


implementation

end.

