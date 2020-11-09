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
unit renderers.mainmenu;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Types, renderers.common, 
  renderers.virtualtreeview, renderer.profile.objectprofile,
  renderer.profile.profileitem, objects.common, objects.mainmenu.item, 
  dataproviders.common;

type
  TMainMenuRenderer = class(TCommonRenderer)
  protected
    { Draw item. }
    procedure Draw (AItemType : Integer; ACanvas : TCanvas; ARect : TRect;
      AState : TItemStates; AObject : TCommonObject; AProfile :
      TRendererObjectProfile); override;
  end;

implementation

{ TMainMenuRenderer }

procedure TMainMenuRenderer.Draw(AItemType : Integer; ACanvas : TCanvas; 
  ARect : TRect; AState : TItemStates; AObject : TCommonObject; AProfile : 
  TRendererObjectProfile);
begin
  ACanvas.Brush.Color := AProfile.DefaultProfile.Background;

  if ITEM_SELECTED in AState then
    ACanvas.Brush.Color := AProfile.SelectedProfile.Background;

  if ITEM_HOVER in AState then
    ACanvas.Brush.Color := AProfile.HoverProfile.Background;

  ACanvas.FillRect(ARect);
end;

end.
