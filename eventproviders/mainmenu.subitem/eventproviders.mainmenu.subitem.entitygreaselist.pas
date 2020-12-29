(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(* This is a software for creating schedules  for repair work, accounting and *)
(* monitoring  their  implementation, accounting for the  necessary materials *) 
(* and spare parts.                                                           *)                 
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
unit eventproviders.mainmenu.subitem.entitygreaselist;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, eventproviders.common, objects.common, objects.entity;

type
  TMainMenuSubitemEntityGreaseListEventProvider = class(TCommonEventProvider)
  public
    constructor Create; override;
  private
    function EntityGreaseClickEvent ({%H-}AObject : TCommonObject) : Boolean;
  end;

implementation

uses
  dataprovider, datahandlers, mainmenuprovider, dataproviders.mainmenu,
  profilesprovider.mainmenu;

{ TMainMenuSubitemEntityGreaseListEventProvider }

constructor TMainMenuSubitemEntityGreaseListEventProvider.Create;
begin
  inherited Create;
  
  Register(EVENT_OBJECT_CLICK, {$IFDEF FPC}@{$ENDIF}EntityGreaseClickEvent);
end;

function TMainMenuSubitemEntityGreaseListEventProvider.EntityGreaseClickEvent 
  (AObject : TCommonObject) : Boolean;
begin
  MainMenu.AttachObject(TMainMenu.MAIN_MENU_ITEM_ENTITY,
    TEntity(Provider.GetSelectedObject));
  Provider.ChangeData(TEntityGreaseDataHandler.Create(
    TEntity(Provider.GetSelectedObject)));
  
  MainMenu.DetachAllDynamicMenus(TMainMenu.MAIN_MENU_ITEM_ENTITY);

  MainMenu.AttachDynamicMenu(TMainMenu.MAIN_MENU_ITEM_ENTITY,
    TMenuSubitemEntityGreaseDataProvider.Create,
    TMainMenuItemProfilesProvider.Create);
  MainMenu.AttachDynamicMenu(TMainMenu.MAIN_MENU_ITEM_ENTITY_GREASE,
    TMenuSubitemEntityGreaseCreateDataProvider.Create,
    TMainMenuSubitemProfilesProvider.Create);

  MainMenu.SelectMenuItem(TMainMenu.MAIN_MENU_ITEM_ENTITY_GREASE);

  Result := True;
end;

end.
