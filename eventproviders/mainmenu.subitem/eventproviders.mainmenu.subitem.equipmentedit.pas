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
unit eventproviders.mainmenu.subitem.equipmentedit;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, eventproviders.common, objects.common, objects.equipment;

type
  TMainMenuSubitemEquipmentEditEventProvider = class(TCommonEventProvider)
  public
    constructor Create; override;
  private
    procedure EquipmentEditSelectedEvent ({%H-}AObject : TCommonObject);
  end;

implementation

uses
  dataprovider, mainmenuprovider;

{ TMainMenuSubitemEquipmentEditEventProvider }

constructor TMainMenuSubitemEquipmentEditEventProvider.Create;
begin
  inherited Create;
  
  Register(EVENT_OBJECT_SELECT, @EquipmentEditSelectedEvent);
end;

procedure TMainMenuSubitemEquipmentEditEventProvider.EquipmentEditSelectedEvent 
  (AObject : TCommonObject);
var
  EquipmentObject : TCommonObject;
begin
  EquipmentObject := Provider.GetSelectedObject;
  if Assigned(EquipmentObject) then
    Provider.ShowEditor(TEquipment(EquipmentObject));
end;

end.
