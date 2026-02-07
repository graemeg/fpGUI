{
  Anti-Grain Geometry
  Copyright (c) 2016 - Graeme Geldenhuys <graemeg@gmail.com>
    All rights reserved.

  Permission to copy, use, modify, sell and distribute this software
  is granted provided this copyright notice appears in all copies.
  This software is provided "as is" without express or implied
  warranty, and with no claim as to its suitability for any purpose.
}
unit agg_pixfmt_rgb24_lcd;

{$I agg_mode.inc }
{$Q- }
{$R- }

interface

uses
 agg_basics ,
 agg_pixfmt ,
 agg_color ,
 agg_rendering_buffer ;

{ GLOBAL PROCEDURES }
 procedure pixfmt_rgb24_lcd(var pixf : pixel_formats; rb : rendering_buffer_ptr );

implementation

uses
  math;

type

   TUnsignedCharArray = array [0..255] of unsigned;

    lcd_distribution_lut = object
      m_primary: TUnsignedCharArray;
      m_secondary: TUnsignedCharArray;
      m_tertiary: TUnsignedCharArray;
      constructor Construct(prim: double; second: double; tert: double);
      function    primary(const v: unsigned): unsigned;
      function    secondary(const v: unsigned): unsigned;
      function    tertiary(const v: unsigned): unsigned;
    end;

function fmt24_row(this : pixel_formats_ptr; x ,y : int ) : row_data_type;
begin
 result.Construct(
   x ,this._width - 1 ,
   int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * 3 * sizeof(int8u ) ) );
end;

procedure fmt24_copy_from(this : pixel_formats_ptr; from : rendering_buffer_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned );
begin
 move(
   int8u_ptr(ptrcomp(from.row(ysrc ) ) + xsrc * 3 * sizeof(int8u ) )^ ,
   int8u_ptr(ptrcomp(this.m_rbuf.row(ydst ) ) + xdst * 3 * sizeof(int8u ) )^ ,
   sizeof(int8u ) * 3 * len );
end;

procedure order24_gamma_dir_apply(this : pixel_formats; p : int8u_ptr );
begin
 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(this.m_apply.dir(int8u_ptr(ptrcomp(p ) + this.m_order.R )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(this.m_apply.dir(int8u_ptr(ptrcomp(p ) + this.m_order.G )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(this.m_apply.dir(int8u_ptr(ptrcomp(p ) + this.m_order.B )^ ) );
end;

procedure order24_gamma_inv_apply(this : pixel_formats; p : int8u_ptr );
begin
 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(this.m_apply.inv(int8u_ptr(ptrcomp(p ) + this.m_order.R )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(this.m_apply.inv(int8u_ptr(ptrcomp(p ) + this.m_order.G )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(this.m_apply.inv(int8u_ptr(ptrcomp(p ) + this.m_order.B )^ ) );
end;

{$I pf_rgb24.inc }

procedure order24_for_each_pixel(this : pixel_formats_ptr; f : func_apply_gamma );
var
 y ,len : unsigned;
 p : int8u_ptr;
begin
  y:=0;

  while y < this._height do
  begin
    len:=this._width;

    p:=this.m_rbuf.row(y );

    repeat
     f(this ,p );

     inc(ptrcomp(p ) ,3 );
     dec(len );

    until len = 0;

    inc(y );
  end;
end;

procedure rgb24_lcd_blend_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
  p : int8u_ptr;
  alpha : unsigned;
begin
 if c.a <> 0 then
  begin
   p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x );

   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    repeat
     order_rgb(pointer(p )^ ).R:=c.r;
     order_rgb(pointer(p )^ ).G:=c.g;
     order_rgb(pointer(p )^ ).B:=c.b;

     inc(ptrcomp(p ) ,3 );
     dec(len );

    until len = 0
   else
    repeat
     blend_pix_rgb(p ,c.r ,c.g ,c.b ,alpha );

     inc(ptrcomp(p ) ,3 );
     dec(len );

    until len = 0;

  end;
end;

procedure rgb24_lcd_blend_solid_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
  p : int8u_ptr;
  alpha : unsigned;
  c3: array[0..6143] of int8u; {2048 * 3}
begin
  if c.a <> 0 then
  begin
    p := int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x );
    alpha := covers^ * c.a;

    repeat
      order_rgb(pointer(p)^).R := ((((c.r - order_rgb(pointer(p)^).R) * alpha) + (order_rgb(pointer(p)^).R shl 16)) shr 16);
      order_rgb(pointer(p)^).G := ((((c.g - order_rgb(pointer(p)^).G) * alpha) + (order_rgb(pointer(p)^).G shl 16)) shr 16);
      order_rgb(pointer(p)^).B := ((((c.b - order_rgb(pointer(p)^).B) * alpha) + (order_rgb(pointer(p)^).B shl 16)) shr 16);

      inc(ptrcomp(p ) ,3 );
      dec(len );
    until len = 0;
  end;
end;



procedure pixfmt_rgb24_lcd(var pixf: pixel_formats; rb: rendering_buffer_ptr);
begin
 pixf.Construct(rb );

 pixf.m_order:=rgb_order;

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@rgb24_copy_pixel;
 pixf.blend_pixel:=@rgb24_blend_pixel;

 pixf.pixel:=@rgb24_pixel;
 pixf.row  :=@fmt24_row;

 pixf.copy_hline:=@rgb24_copy_hline;
 pixf.copy_vline:=@rgb24_copy_vline;

 pixf.blend_hline:=@rgb24_lcd_blend_hline;
 pixf.blend_vline:=@rgb24_blend_vline;

 pixf.blend_solid_hspan:=@rgb24_lcd_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgb24_blend_solid_vspan;

 pixf.copy_color_hspan:=@rgb24_copy_color_hspan;
 pixf.copy_color_vspan:=@rgb24_copy_color_vspan;

 pixf.blend_color_hspan:=@rgb24_blend_color_hspan;
 pixf.blend_color_vspan:=@rgb24_blend_color_vspan;

 pixf.copy_from :=@fmt24_copy_from;
 pixf.blend_from:=@rgb24_blend_from;

 pixf.blend_from_color:=@rgb24_blend_from_color;
 pixf.blend_from_lut  :=@rgb24_blend_from_lut;

 pixf.for_each_pixel :=@order24_for_each_pixel;
 pixf.gamma_dir_apply:=@order24_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order24_gamma_inv_apply;
end;

{ lcd_distribution_lut }

constructor lcd_distribution_lut.Construct(prim: double; second: double; tert: double);
var
  norm: double;
  i: integer;
begin
  norm := 1.0 / (prim + second*2 + tert*2);
  prim := prim * norm;
  second := second * norm;
  tert := tert * norm;
  for i := 0 to 255 do
  begin
    m_primary[i] := floor(prim * i);
    m_secondary[i] := floor(second * i);
    m_tertiary[i] := floor(tert * i);
  end;
end;

function lcd_distribution_lut.primary(const v: unsigned): unsigned;
begin
  Result := m_primary[v];
end;

function lcd_distribution_lut.secondary(const v: unsigned): unsigned;
begin
  Result := m_secondary[v];
end;

function lcd_distribution_lut.tertiary(const v: unsigned): unsigned;
begin
  Result := m_tertiary[v];
end;

end.

