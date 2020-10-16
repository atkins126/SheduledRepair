(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(*                                                                            *)
(* Copyright (c) 2020                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpassqlite                ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* This source  is free software;  you can redistribute  it and/or modify  it *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation; either version 3 of the License.                      *)
(*                                                                            *)
(* This code is distributed in the  hope that it will  be useful, but WITHOUT *)
(* ANY  WARRANTY;  without even  the implied  warranty of MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License for *)
(* more details.                                                              *)
(*                                                                            *)
(* A copy  of the  GNU General Public License is available  on the World Wide *)
(* Web at <http://www.gnu.org/copyleft/gpl.html>. You  can also obtain  it by *)
(* writing to the Free Software Foundation, Inc., 51  Franklin Street - Fifth *)
(* Floor, Boston, MA 02110-1335, USA.                                         *)
(*                                                                            *)
(******************************************************************************)
unit renderer.objects.common;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  objects.measure, Graphics, BGRABitmap, BGRABitmapTypes, 
  renderer.profile.profileitem, renderer.profile.profile;

type
  PBGRABitmap = ^TBGRABitmap;

  generic TCommonRenderer<T> = class
  public
    constructor Create;
    destructor Destroy; override;


  protected
    procedure DrawBackground (ABitmap : PBGRABitmap; AColor : TBGRAPixel);
    procedure DrawSquareBorder (ABitmap : PBGRABitmap; AWidth : Integer; 
      AMargin : TRendererProfile.TMargin; AColor : TBGRAPixel);
    procedure DrawSquareRoundBorder (ABitmap : PBGRABitmap; AWidth : Integer;
      AMargin : TRendererProfile.TMargin; AColor : TBGRAPixel; ARoundRadius :
      Integer);
  protected
    
  end;

implementation

{ TCommonRenderer }

constructor TCommonRenderer.Create;
begin

end;

destructor TCommonRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TCommonRenderer.DrawBackground (ABitmap : PBGRABitmap; AColor : 
  TBGRAPixel);
begin
  ABitmap^.FillRect(0, 0, ABitmap^.Width, ABitmap^.Height, AColor,
    dmDrawWithTransparency);
end;

procedure TCommonRenderer.DrawSquareBorder (ABitmap : PBGRABitmap; AWidth :
  Integer; AMargin : TRendererProfile.TMargin; AColor : TBGRAPixel);
begin
  ABitmap^.RectangleAntialias(AMargin.Top, AMargin.Right, ABitmap^.Width -
    AMargin.Left, ABitmap^.Height - AMargin.Bottom, AColor, AWidth);
end;

procedure TCommonRenderer.DrawSquareRoundBorder (ABitmap : PBGRABitmap; 
  AWidth : Integer; AMargin : TRendererProfile.TMargin; AColor : TBGRAPixel; 
  ARoundRadius : Integer);
begin
  ABitmap^.RoundRectAntialias(AMargin.Top, AMargin.Right, ABitmap^.Width -
    AMargin.Left, ABitmap^.Height - AMargin.Bottom, ARoundRadius, ARoundRadius,
    AColor, AWidth, []);
end;

end.
