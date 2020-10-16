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
unit renderer.objects.measure;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  renderer.objects.common, objects.measure, BGRABitmap, BGRABitmapTypes,
  Graphics, renderer.profile.objectprofile, renderer.profile.profile;

type
  TMeasureRenderer = class(specialize TCommonRenderer<TMeasure>)
  public
    constructor Create (AObject : TMeasure; AProfile : TRendererObjectProfile);
    destructor Destroy; override;

    procedure Draw (ABitmap : PBGRABitmap);  
  private
    FObject : TMeasure;
    FProfile : TRendererObjectProfile;
  end;

implementation

{ TMeasureRenderer }

constructor TMeasureRenderer.Create (AObject : TMeasure; AProfile :
  TRendererObjectProfile);
begin
  FObject := AObject;
  FProfile := AProfile;
end;

destructor TMeasureRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TMeasureRenderer.Draw (ABitmap : PBGRABitmap);
begin
  DrawBackground(ABitmap, FProfile.DefaultProfile.Background);
  DrawSquareRoundBorder(ABitmap, FProfile.DefaultProfile.Border,
    FProfile.DefaultProfile.BorderMargin, FProfile.DefaultProfile.BorderColor,
    FProfile.DefaultProfile.BorderRadius);
end;

end.
