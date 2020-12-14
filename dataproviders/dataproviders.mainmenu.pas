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
unit dataproviders.mainmenu;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, dataproviders.common, objects.common, objects.mainmenu.item,
  eventproviders.mainmenu.item.job,
  eventproviders.mainmenu.item.equipment, 
  eventproviders.mainmenu.subitem.jobcreate, 
  eventproviders.mainmenu.subitem.jobedit,
  eventproviders.mainmenu.subitem.equipmentcreate,
  eventproviders.mainmenu.subitem.equipmentedit;

type
  TMainMenuDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemJobDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemJobEditDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemEquipmentDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

implementation

uses
  mainmenuprovider;

{ TMainMenuDataProvider }

function TMainMenuDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_LOGO, 
    MENU_ITEM_TYPE_LOGO, 'SheduledRepair', False));
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_JOB, 
    MENU_ITEM_TYPE_ITEM, 'Job', True, TMainMenuItemJobEventProvider.Create));
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT, 
    MENU_ITEM_TYPE_ITEM, 'Equipment', True, 
    TMainMenuItemEquipmentEventProvider.Create));
  
  Result := True;
end;

function TMainMenuDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMainMenuDataProvider.LoadConcreteObject (AID : Int64) : TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemJobDataProvider }

function TMenuSubitemJobDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 'Create', False,
    TMainMenuSubitemJobCreateEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemJobDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemJobDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemJobEditDataProvider }

function TMenuSubitemJobEditDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 'Edit', False,
    TMainMenuSubitemJobEditEventProvider.Create));
  
  Result := True;
end;

function TMenuSubitemJobEditDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemJobEditDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemEquipmentDataProvider }

function TMenuSubitemEquipmentDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 'Create', False,
    TMainMenuSubitemEquipmentCreateEventProvider.Create));
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 'Edit', False,
    TMainMenuSubitemEquipmentEditEventProvider.Create));
  
  Result := True;
end;

function TMenuSubitemEquipmentDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemEquipmentDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

end.
