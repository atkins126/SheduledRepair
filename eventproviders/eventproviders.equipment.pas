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
unit eventproviders.equipment;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, eventproviders.common, objects.common, objects.equipment;

type
  TEquipmentEventProvider = class(TCommonEventProvider)
  public
    constructor Create; override;
  private
    FEditMenuAttached : Boolean;
    
    function OnObjectSelectEvent ({%H-}AObject : TCommonObject) : Boolean;

    { Object on double click event. }
    function OnObjectDoubleClickEvent (AObject : TCommonObject) : Boolean;
  end;

implementation

uses
  dataprovider, datahandlers, mainmenuprovider, dataproviders.mainmenu,
  profilesprovider.mainmenu;

{ TEquipmentEventProvider }

constructor TEquipmentEventProvider.Create;
begin
  inherited Create;
  FEditMenuAttached := False;
  
  Register(EVENT_OBJECT_SELECT, @OnObjectSelectEvent);
  Register(EVENT_OBJECT_DOUBLE_CLICK, @OnObjectDoubleClickEvent);
end;

function TEquipmentEventProvider.OnObjectSelectEvent (AObject : TCommonObject) :
  Boolean;
begin
  if (not FEditMenuAttached) and (Assigned(Provider.GetSelectedObject)) then
  begin
    MainMenu.AttachDynamicMenu(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT,
      TMenuSubitemEquipmentEditDataProvider.Create,
      TMainMenuSubitemProfilesProvider.Create);

    FEditMenuAttached := True;
  end;
  
  Result := True;
end;

function TEquipmentEventProvider.OnObjectDoubleClickEvent (AObject :
  TCommonObject) : Boolean;
begin
  Provider.ChangeData(TEquipmentEntityDataHandler.Create(TEquipment(AObject)));
  MainMenu.AttachObject(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT,
    TEquipment(AObject));
  
  MainMenu.DetachAllDynamicMenus(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT);

  MainMenu.AttachDynamicMenu(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT,
    TMenuSubitemEntityDataProvider.Create,
    TMainMenuItemProfilesProvider.Create);
  {
  MainMenu.AttachDynamicMenu(TMainMenu.MAIN_MENU_ITEM_ENTITY,
    TMenuSubitemEntityCreateDataProvider.Create,
    TMainMenuSubitemProfilesProvider.Create);
  }
  Result := True;
end;

end.
