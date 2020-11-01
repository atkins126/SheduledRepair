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
unit objects.equipment;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  objects.entitybag;

type
  TEquipment = class(TCommonObject)
  private
    const
      EQUIPMENT_TABLE_NAME = 'equipment';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Delete object from database. }
    function Delete : Boolean; override;

    { Object deep copy. }
    procedure Assign (AEquipment : TEquipment);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;
  protected
    FName : String;
    FEntityBag : TEntityBag;
  public
    property Name : String read FName write FName;
    property EntityBag : TEntityBag read FEntityBag write FEntityBag;
  end;

implementation

{ TEquipment }

constructor TEquipment.Create (AID : Int64);
begin
  inherited Create (AID);
  FName := '';
  FEntityBag := TEntityBag.Create(-1, Self);
end;

destructor TEquipment.Destroy;
begin
  FreeAndNil(FEntityBag);
  inherited Destroy;
end;

procedure TEquipment.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Text('name').NotNull;
end;

function TEquipment.CheckDepentSchemes : Boolean;
begin
  Result := FEntityBag.CheckSchema;
end;

function TEquipment.Table : String;
begin
  Result := EQUIPMENT_TABLE_NAME;
end;

function TEquipment.LoadCurrentObject : Boolean;
begin
  Result := inherited LoadCurrentObject;

  FName := GetStringProperty('name');
end;

function TEquipment.LoadDepentObjects : Boolean;
begin
  Result := FEntityBag.Reload(-1);
end;

function TEquipment.Save : Boolean;
begin
  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('name', FName).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('name', FName).Get > 0);
    UpdateObjectID;
  end;

  FEntityBag.Save;
end;

function TEquipment.Delete : Boolean;
begin
  if ID <> -1 then
  begin
    FEntityBag.Delete;
    Result := DeleteCurrentObject;
  end else 
    Result := False;
end;

procedure TEquipment.Assign (AEquipment : TEquipment);
begin
  FName := AEquipment.Name;
  FEntityBag.Assign(AEquipment.EntityBag);
end;

end.
