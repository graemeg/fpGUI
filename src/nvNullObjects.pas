unit nvNullObjects;

{$mode objfpc}{$H+}

// disable to remove debugging output
{$Define DEBUG}

interface

uses
  contnrs, SysUtils;

type
  EHelpBitmapException = class(Exception);

  // forward declaration
  THelpBitmap = class;


  TImageList = class(TObjectList)
  public
    procedure Add(ABitmap: THelpBitmap; AParam2: TObject);
  end;


  THelpBitmap = class(TObject)
  public
    class function CreateFromHelpFile(AData: pointer): THelpBitmap;
    procedure LoadFromResourceName(const AName: string);
  end;


procedure ProfileEvent(const AString: string);


implementation


procedure ProfileEvent(const AString: string);
begin
  {$IFDEF DEBUG}
  writeln('DEBUG:  ', AString);
  {$ENDIF}
end;

{ TImageList }

procedure TImageList.Add(ABitmap: THelpBitmap; AParam2: TObject);
begin
  //
end;

{ THelpBitmap }

class function THelpBitmap.CreateFromHelpFile(AData: pointer): THelpBitmap;
begin
  Result := nil;
end;

procedure THelpBitmap.LoadFromResourceName(const AName: string);
begin
  //
end;


end.

