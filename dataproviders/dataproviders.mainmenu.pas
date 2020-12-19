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
  eventproviders.mainmenu.item.entity,
  eventproviders.mainmenu.subitem.jobcreate, 
  eventproviders.mainmenu.subitem.jobedit,
  eventproviders.mainmenu.subitem.equipmentcreate,
  eventproviders.mainmenu.subitem.equipmentedit,
  eventproviders.mainmenu.subitem.entitycreate,
  eventproviders.mainmenu.subitem.entityedit;

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

  TMenuSubitemJobCreateDataProvider = class(TCommonDataProvider)
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

  TMenuSubitemEquipmentCreateDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemEquipmentEditDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemEntityDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemEntityCreateDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemEntityEditDataProvider = class(TCommonDataProvider)
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
    MENU_ITEM_TYPE_LOGO, 'SheduledRepair'));
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_JOB, 
    MENU_ITEM_TYPE_ITEM, 'Job', TMainMenuItemJobEventProvider.Create));
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT, 
    MENU_ITEM_TYPE_ITEM, 'Equipment',
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

{ TMenuSubitemJobCreateDataProvider }

function TMenuSubitemJobCreateDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 'Create',
    TMainMenuSubitemJobCreateEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemJobCreateDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemJobCreateDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemJobEditDataProvider }

function TMenuSubitemJobEditDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 'Edit',
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

function TMenuSubitemEquipmentCreateDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 'Create',
    TMainMenuSubitemEquipmentCreateEventProvider.Create));
  
  Result := True;
end;

function TMenuSubitemEquipmentCreateDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemEquipmentCreateDataProvider.LoadConcreteObject (AID : 
  Int64) : TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemEquipmentEditDataProvider }

function TMenuSubitemEquipmentEditDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 'Edit',
    TMainMenuSubitemEquipmentEditEventProvider.Create));
  
  Result := True;
end;

function TMenuSubitemEquipmentEditDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemEquipmentEditDataProvider.LoadConcreteObject (AID : Int64) 
  : TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemEntityDataProvider }

function TMenuSubitemEntityDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_ENTITY, 
    MENU_ITEM_TYPE_ITEM, 'Entity', TMainMenuItemEntityEventProvider.Create));
  
  Result := True;
end;

function TMenuSubitemEntityDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemEntityDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemEntityCreateDataProvider }

function TMenuSubitemEntityCreateDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 'Create',
    TMainMenuSubitemEntityCreateEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemEntityCreateDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemEntityCreateDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemEntityEditDataProvider }

function TMenuSubitemEntityEditDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 'Edit',
    TMainMenuSubitemEntityEditEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemEntityEditDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemEntityEditDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

end.
