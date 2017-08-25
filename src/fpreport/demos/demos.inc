
{ the default report export filters }
{$define ExportPDF}
{$define ExportFPImage}
{$define ExportHTML}
{.$define USEFIREBIRD}

{.$DEFINE USEPOLYGON}

{ Enable if you want LCL preview window support. To compile you will also
  have to add the LCL packages to your project dependencies. }

{.$define ExportLCL}


{ Enable if you want LCL preview window support. To compile you will also
  have to add the LCL packages to your project dependencies. }

{$define ExportFPGui}


{ Enable if you want PNG (AggPas) exporting. Remember to set the "aggpas"
  project macro to point to the correct AggPas source location.  }

{$define ExportAggPas}


{ Colour code the bands for debugging purposes and to better visualise the
  report design. }
{$define ColorBands}

{ Enable this define to get more verbose debug output to the console. }

{.$define gDebug}

