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
unit dataproviders.mainmenu;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, dataproviders.common, objects.common, objects.mainmenu.item,
  eventproviders.mainmenu.item.job,
  eventproviders.mainmenu.item.equipment, 
  eventproviders.mainmenu.item.entity,
  eventproviders.mainmenu.item.entitygrease,
  eventproviders.mainmenu.item.node,
  eventproviders.mainmenu.item.nodegrease,
  eventproviders.mainmenu.subitem.jobcreate, 
  eventproviders.mainmenu.subitem.jobedit,
  eventproviders.mainmenu.subitem.equipmentcreate,
  eventproviders.mainmenu.subitem.equipmentedit,
  eventproviders.mainmenu.subitem.entitycreate,
  eventproviders.mainmenu.subitem.entityedit,
  eventproviders.mainmenu.subitem.entitygreaselist,
  eventproviders.mainmenu.subitem.entitygreasecreate,
  eventproviders.mainmenu.subitem.entitygreaseedit,
  eventproviders.mainmenu.subitem.nodecreate,
  eventproviders.mainmenu.subitem.nodeedit,
  eventproviders.mainmenu.subitem.nodegreaselist,
  eventproviders.mainmenu.subitem.nodegreasecreate,
  eventproviders.mainmenu.subitem.nodegreaseedit;

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

  TMenuSubitemEntityGreaseListDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemEntityGreaseDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemEntityGreaseCreateDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemEntityGreaseEditDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemNodeDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemNodeCreateDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemNodeEditDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemNodeGreaseListDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemNodeGreaseDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemNodeGreaseCreateDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  end;

  TMenuSubitemNodeGreaseEditDataProvider = class(TCommonDataProvider)
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
  mainmenuprovider, configuration;

{ TMainMenuDataProvider }

function TMainMenuDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_LOGO, 
    MENU_ITEM_TYPE_LOGO, 'SheduledRepair'));
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_JOB, 
    MENU_ITEM_TYPE_ITEM, Config.GetValue('Job', 'Job'), 
    TMainMenuItemJobEventProvider.Create));
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_EQUIPMENT, 
    MENU_ITEM_TYPE_ITEM, Config.GetValue('Equipment', 'Equipment'),
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
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Create', 'Create'),
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
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Edit', 'Edit'),
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
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Create', 'Create'),
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
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Edit', 'Edit'),
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
    MENU_ITEM_TYPE_ITEM, Config.GetValue('Entity', 'Entity'), 
    TMainMenuItemEntityEventProvider.Create));
  
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
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Create', 'Create'),
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
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Edit', 'Edit'),
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

{ TMenuSubitemEntityGreaseListDataProvider }

function TMenuSubitemEntityGreaseListDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Grease', 'Grease'),
    TMainMenuSubitemEntityGreaseListEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemEntityGreaseListDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemEntityGreaseListDataProvider.LoadConcreteObject (AID : 
  Int64) : TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemNodeDataProvider }

function TMenuSubitemNodeDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_NODE, 
    MENU_ITEM_TYPE_ITEM, Config.GetValue('Node', 'Node'), 
    TMainMenuItemNodeEventProvider.Create));
  
  Result := True;
end;

function TMenuSubitemNodeDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemNodeDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemNodeCreateDataProvider }

function TMenuSubitemNodeCreateDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Create', 'Create'),
    TMainMenuSubitemNodeCreateEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemNodeCreateDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemNodeCreateDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemNodeEditDataProvider }

function TMenuSubitemNodeEditDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Edit', 'Edit'),
    TMainMenuSubitemNodeEditEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemNodeEditDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemNodeEditDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemEntityGreaseDataProvider }

function TMenuSubitemEntityGreaseDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_ENTITY_GREASE, 
    MENU_ITEM_TYPE_ITEM, Config.GetValue('Grease', 'Grease'),
    TMainMenuItemEntityGreaseEventProvider.Create));
  
  Result := True;
end;

function TMenuSubitemEntityGreaseDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemEntityGreaseDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemEntityGreaseCreateDataProvider }

function TMenuSubitemEntityGreaseCreateDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Create', 'Create'),
    TMainMenuSubitemEntityGreaseCreateEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemEntityGreaseCreateDataProvider.LoadObjectsTableName : 
  String;
begin
  Result := '';
end;

function TMenuSubitemEntityGreaseCreateDataProvider.LoadConcreteObject (AID : 
  Int64) : TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemEntityGreaseEditDataProvider }

function TMenuSubitemEntityGreaseEditDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Edit', 'Edit'),
    TMainMenuSubitemEntityGreaseEditEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemEntityGreaseEditDataProvider.LoadObjectsTableName : 
  String;
begin
  Result := '';
end;

function TMenuSubitemEntityGreaseEditDataProvider.LoadConcreteObject (AID : 
  Int64) : TCommonObject;
begin
  Result := nil;
end;


{ TMenuSubitemNodeGreaseListDataProvider }

function TMenuSubitemNodeGreaseListDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Grease', 'Grease'),
    TMainMenuSubitemNodeGreaseListEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemNodeGreaseListDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemNodeGreaseListDataProvider.LoadConcreteObject (AID : 
  Int64) : TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemNodeGreaseDataProvider }

function TMenuSubitemNodeGreaseDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(TMainMenu.MAIN_MENU_ITEM_NODE_GREASE, 
    MENU_ITEM_TYPE_ITEM, Config.GetValue('Grease', 'Grease'),
    TMainMenuItemNodeGreaseEventProvider.Create));
  
  Result := True;
end;

function TMenuSubitemNodeGreaseDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemNodeGreaseDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemNodeGreaseCreateDataProvider }

function TMenuSubitemNodeGreaseCreateDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Create', 'Create'),
    TMainMenuSubitemNodeGreaseCreateEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemNodeGreaseCreateDataProvider.LoadObjectsTableName : 
  String;
begin
  Result := '';
end;

function TMenuSubitemNodeGreaseCreateDataProvider.LoadConcreteObject (AID : 
  Int64) : TCommonObject;
begin
  Result := nil;
end;

{ TMenuSubitemNodeGreaseEditDataProvider }

function TMenuSubitemNodeGreaseEditDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_ITEM_TYPE_SUBITEM, 
    Config.GetValue('Edit', 'Edit'),
    TMainMenuSubitemNodeGreaseEditEventProvider.Create));
    
  Result := True;
end;

function TMenuSubitemNodeGreaseEditDataProvider.LoadObjectsTableName : 
  String;
begin
  Result := '';
end;

function TMenuSubitemNodeGreaseEditDataProvider.LoadConcreteObject (AID : 
  Int64) : TCommonObject;
begin
  Result := nil;
end;

end.
