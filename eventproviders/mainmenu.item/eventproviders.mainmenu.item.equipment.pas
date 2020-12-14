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
unit eventproviders.mainmenu.item.equipment;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, eventproviders.common, objects.common;

type
  TMainMenuItemEquipmentEventProvider = class(TCommonEventProvider)
  public
    constructor Create; override;
  private
    procedure EquipmentClickEvent ({%H-}AObject : TCommonObject);
    procedure EquipmentUnselectEvent ({%H-}AObject : TCommonObject);
    procedure EquipmentAttachDynamicMenuEvent ({%H-}AObject : TCommonObject);
    procedure EquipmentDetachDynamicMenuEvent ({%H-}AObject : TCommonObject);
  end;

implementation

uses
  dataprovider, datahandlers, mainmenuprovider, profilesprovider.mainmenu,
  dataproviders.mainmenu;

{ TMainMenuItemEquipmentEventProvider }

constructor TMainMenuItemEquipmentEventProvider.Create;
begin
  inherited Create;
  
  Register(EVENT_OBJECT_CLICK, @EquipmentClickEvent);
  Register(EVENT_OBJECT_UNSELECT, @EquipmentUnselectEvent);
  Register(EVENT_OBJECT_ATTACH_DYNAMIC_MENU, @EquipmentAttachDynamicMenuEvent);
  Register(EVENT_OBJECT_DETACH_DYNAMIC_MENU, @EquipmentDetachDynamicMenuEvent);
end;

procedure TMainMenuItemEquipmentEventProvider.EquipmentClickEvent (AObject : 
  TCommonObject);
begin
  Provider.ChangeData(TEquipmentDataHandler.Create);
  MainMenu.DetachObject(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT);
end;

procedure TMainMenuItemEquipmentEventProvider.EquipmentUnselectEvent (AObject :
  TCommonObject);
begin
  MainMenu.DetachObject(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT);
end;

procedure TMainMenuItemEquipmentEventProvider.EquipmentAttachDynamicMenuEvent 
  (AObject : TCommonObject);
begin
  MainMenu.AttachDynamicMenu(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT,
    TMenuSubitemEquipmentDataProvider.Create, 
    TMenuSubitemEquipmentProfilesProvider.Create);
end;

procedure TMainMenuItemEquipmentEventProvider.EquipmentDetachDynamicMenuEvent 
  (AObject : TCommonObject);
begin
  MainMenu.DetachAllDynamicMenus(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT);
end;

end.
