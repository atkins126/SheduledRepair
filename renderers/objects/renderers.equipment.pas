(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(*                                                                            *)
(* Copyright (c) 2020                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/SheduledRepair              ivan@semenkov.pro *)
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
unit renderers.equipment;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Types, renderers.common, 
  renderer.profile.profile, renderer.profile.profileitem, objects.common, 
  objects.equipment;

type
  TEquipmentRenderer = class(TCommonRenderer)
  public
    { Draw object using renderer profile. }
    procedure Draw (AObject : TCommonObject; AProfile : TRendererProfile;
      ACanvas : TCanvas; ARect : TRect); override;

    { Calculate columns. }
    procedure CalculateColumns (AFullWidth : Cardinal); override;
  protected
    { Draw menu item background. }
    procedure DrawBackground (AProfile : TRendererProfile; ACanvas : TCanvas;
      ARect : TRect);
    {$IFNDEF DEBUG}inline;{$ENDIF}

    { Draw name. }
    procedure DrawName (AObject : TEquipment; AProfileItem : 
      TRendererProfileItem; ACanvas : TCanvas; ARect : TRect);
    {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

implementation

{ TEquipmentRenderer }

procedure TEquipmentRenderer.CalculateColumns (AFullWidth : Cardinal);
begin
  Clear;
  AppendColumn(AFullWidth);
end;

procedure TEquipmentRenderer.DrawBackground(AProfile : TRendererProfile;
  ACanvas : TCanvas; ARect : TRect);
begin
  ACanvas.Brush.Color := AProfile.Background;
  ACanvas.FillRect(ARect);
end;

procedure TEquipmentRenderer.DrawName(AObject : TEquipment; AProfileItem :
  TRendererProfileItem; ACanvas : TCanvas; ARect : TRect);

  function CalculateXPos : Integer; {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    Result := Round(AProfileItem.Position.X * ARect.Width / 100);
  end;

  function CalculateYPos : Integer; {$IFNDEF DEBUG}inline;{$ENDIF}
  begin
    Result := Round(AProfileItem.Position.Y * ARect.Height / 100);
  end;

  function CalculateNameRect : TRect; {$IFNDEF DEBUG}inline;{$ENDIF}
  var
    TitleSize : TSize;
  begin
    ACanvas.Font.Color := AProfileItem.FontColor;
    ACanvas.Font.Size := AProfileItem.FontSize;
    ACanvas.Font.Name := AProfileItem.FontName;
    TitleSize := ACanvas.TextExtent(AObject.Name);

    case AProfileItem.PositionType of
      POSITION_FIXED : begin
        Result := Rect(AProfileItem.Position.X + AProfileItem.Padding.Left,
          AProfileItem.Position.Y + AProfileItem.Padding.Top,
          TitleSize.cx + AProfileItem.Padding.Right,
          TitleSize.cy + AProfileItem.Padding.Bottom);
      end;
      POSITION_FLOAT : begin
        Result := Rect(CalculateXPos + AProfileItem.Padding.Left,
          CalculateYPos + AProfileItem.Padding.Top,
          TitleSize.cx + AProfileItem.Padding.Right,
          TitleSize.cy + AProfileItem.Padding.Bottom);
      end;
    end;
  end;

var
  NameRect : TRect;
begin
  if not AProfileItem.Enable then
    Exit;

  NameRect := CalculateNameRect;

  case AProfileItem.BackgroundFillType of
    FILL_NONE : begin { Transparent background. } end;
    FILL_SQUARE : begin
      ACanvas.Brush.Color := AProfileItem.Background;
      ACanvas.FillRect(NameRect);
    end;
    FILL_SQUARE_ROUND_CORNER : begin
      ACanvas.Brush.Color := AProfileItem.Background;
      ACanvas.RoundRect(NameRect, AProfileItem.BackgroundRoundRadius,
        AProfileItem.BackgroundRoundRadius);
    end;
  end;

  ACanvas.Font.Color := AProfileItem.FontColor;
  ACanvas.Font.Size := AProfileItem.FontSize;
  ACanvas.Font.Name := AProfileItem.FontName;
  ACanvas.TextOut(NameRect.Left, NameRect.Top, AObject.Name);
end;

procedure TEquipmentRenderer.Draw(AObject : TCommonObject; AProfile : 
  TRendererProfile; ACanvas : TCanvas; ARect : TRect);
begin
  DrawBackground(AProfile, ACanvas, ARect);
  DrawName(TEquipment(AObject), AProfile.Items['Name'], ACanvas, ARect);
end;

end.
