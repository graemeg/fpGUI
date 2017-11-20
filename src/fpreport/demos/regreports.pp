unit regreports;

{$mode objfpc}{$H+}

{$i demos.inc}

interface

uses
  rptsimplelist,
  rptexpressions,
  rptgrouping,
  rptframes,
  rptimages,
  rptttf,
  rptshapes,
  rptdataset,
  rptcolumns,
  rptmasterdetail,
{$IFDEF USEFIREBIRD}
  rptmasterdetaildataset,
{$ENDIF}
  rptjson,
  rptcontnr,
  rptnestedgroups,
  rptBarcode,
  rptQRcode,
  udapp
  ;

Procedure RegisterReports;

implementation

procedure RegisterReports;

  Procedure R(AName : String; AClass : TReportDemoAppClass);

  begin
    TReportDemoApplication.RegisterReport(aName,AClass);
  end;


begin
  R('simplelist',TSimpleListDemo);
  R('expressions',TExpressionsDemo);
  R('grouping',TGroupingDemo);
  R('frames',TFramesDemo);
  R('Images',TImagesDemo);
  R('shapes',TShapesDemo);
  R('truetypefonts',TTTFDemo);
  R('dataset',TDatasetDemo);
  R('columns',TColumnsDemo);
  R('masterdetail',TMasterDetailDemo);
  {$IFDEF USEFIREBIRD}
  R('masterdetaildataset',TMasterDetailDatasetDemo);
  {$ENDIF}
  R('jsondata',TJSONDemo);
  R('collectiondata',TCollectionDemo);
  R('objectlistdata',TObjectListDemo);
  R('nestedgroups',TNestedGroupsDemo);
  R('barcode',TBarcodeDemo);
  R('QRCode',TQRcodeDemo);
end;

initialization
  RegisterReports;
end.

