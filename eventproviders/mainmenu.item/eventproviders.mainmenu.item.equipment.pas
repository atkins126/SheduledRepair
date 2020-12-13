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
    procedure EquipmentSelectedEvent ({%H-}AObject : TCommonObject);
    procedure EquipmentUnselectEvent ({%H-}AObject : TCommonObject);
    procedure EquipmentAttachMenuEvent ({%H-}AObject : TCommonObject);
    procedure EquipmentDetachMenuEvent ({%H-}AObject : TCommonObject);
  end;

implementation

uses
  dataprovider, datahandlers, mainmenuprovider, profilesprovider.mainmenu,
  dataproviders.mainmenu;

{ TMainMenuItemEquipmentEventProvider }

constructor TMainMenuItemEquipmentEventProvider.Create;
begin
  inherited Create;
  OnObjectSelect := @EquipmentSelectedEvent;
  OnObjectUnselect := @EquipmentUnselectEvent;
  OnObjectAttachDynamicMenu := @EquipmentAttachMenuEvent;
  OnObjectDetachDynamicMenu := @EquipmentDetachMenuEvent;
end;

procedure TMainMenuItemEquipmentEventProvider.EquipmentSelectedEvent (AObject : 
  TCommonObject);
begin
  MainMenu.AttachDynamicMenu(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT,
    TMenuSubitemEquipmentDataProvider.Create, 
    TMenuSubitemEquipmentProfilesProvider.Create);
  Provider.ChangeData(TEquipmentDataHandler.Create);
end;

procedure TMainMenuItemEquipmentEventProvider.EquipmentUnselectEvent (AObject :
  TCommonObject);
begin
  MainMenu.DetachAllDynamicMenus(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT);
  MainMenu.DetachObject(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT);
end;

procedure TMainMenuItemEquipmentEventProvider.EquipmentAttachMenuEvent 
  (AObject : TCommonObject);
begin
  {MainMenu.AttachDynamicMenu(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT,
    TMenuSubitemEquipmentDataProvider.Create, 
    TMenuSubitemEquipmentProfilesProvider.Create);}
end;

procedure TMainMenuItemEquipmentEventProvider.EquipmentDetachMenuEvent 
  (AObject : TCommonObject);
begin
  MainMenu.DetachAllDynamicMenus(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT);
end;

end.
