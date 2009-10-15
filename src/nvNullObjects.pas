unit nvNullObjects;

{$mode objfpc}{$H+}

interface

uses
  contnrs, Classes, SysUtils;

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
    constructor CreateFromHelpFile( FileHandle: TFileStream; Offset: longint );
    procedure LoadFromResourceName(const AName: string);
  end;



implementation



{ TImageList }

procedure TImageList.Add(ABitmap: THelpBitmap; AParam2: TObject);
begin
  //
end;

{ THelpBitmap }


constructor THelpBitmap.CreateFromHelpFile(FileHandle: TFileStream; Offset: longint);
begin
  inherited Create;
end;

procedure THelpBitmap.LoadFromResourceName(const AName: string);
begin
  //
end;


end.

