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
    
    procedure OnObjectSelectEvent ({%H-}AObject : TCommonObject);

    { Object on double click event. }
    procedure OnObjectDoubleClickEvent (AObject : TCommonObject);
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

procedure TEquipmentEventProvider.OnObjectSelectEvent (AObject : TCommonObject);
begin
  if (not FEditMenuAttached) and (Assigned(Provider.GetSelectedObject)) then
  begin
    MainMenu.AttachDynamicMenu(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT,
      TMenuSubitemEquipmentEditDataProvider.Create,
      TMenuSubitemEquipmentProfilesProvider.Create);
    MainMenu.UpdateDynamicMenu;
    FEditMenuAttached := True;
  end;
end;

procedure TEquipmentEventProvider.OnObjectDoubleClickEvent (AObject :
  TCommonObject);
begin
  MainMenu.DetachAllDynamicMenus(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT);
  MainMenu.AttachObject(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT,
    TEquipment(AObject));
  Provider.ChangeData(TEquipmentEntityDataHandler.Create(TEquipment(AObject)));
  MainMenu.UpdateDynamicMenu;
end;

end.
