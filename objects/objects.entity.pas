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
unit objects.entity;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.greasableobject, sqlite3.schema, objects.greasebag, 
  objects.nodebag, objects.shedule, objects.period, objects.quantity;

type
  TEntity = class(TGreasableObject)
  private
    const
      ENTITY_TABLE_NAME = 'entity';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Load object from database. }
    function Load : Boolean; override;

    { Object deep copy. }
    procedure Assign (AEntity : TEntity);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;

    { Store current object to database. }
    procedure SaveCurrentObject; override;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; override;

    { Delete all dependent objects. }
    function DeleteDepentObjects : Boolean; override;
  protected
    FNodeBag : TNodeBag;
    FShedule : TShedule;
    FQuantity : TQuantity;
    FPeriod : TPeriod;
  public
    property NodeBag : TNodeBag read FNodeBag write FNodeBag;
    property Shedule : TShedule read FShedule write FShedule;
    property Quantity : TQuantity read FQuantity write FQuantity;
    property Period : TPeriod read FPeriod write FPeriod;
  end;

implementation

{ TEntity }

constructor TEntity.Create (AID : Int64);
begin
  inherited Create (AID);
  FNodeBag := TNodeBag.Create(-1, Self);
  FShedule := TShedule.Create(-1);
  FQuantity := TQuantity.Create(-1);
  FPeriod := TPeriod.Create(-1);
end;

destructor TEntity.Destroy;
begin
  FreeAndNil(FNodeBag);
  FreeAndNil(FShedule);
  FreeAndNil(FQuantity);
  FreeAndNil(FPeriod);
  inherited Destroy;
end;

procedure TEntity.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Text('name').NotNull
    .Integer('quantity_id')
    .Integer('period_id')
    .Integer('shedule_id');
end;

function TEntity.CheckDepentSchemes : Boolean;
begin
  Result := FGreaseBag.CheckSchema and FNodeBag.CheckSchema and
    FShedule.CheckSchema and FQuantity.CheckSchema and FPeriod.CheckSchema;
end;

function TEntity.Table : String;
begin
  Result := ENTITY_TABLE_NAME;
end;

function TEntity.LoadCurrentObject : Boolean;
begin
  Result := inherited LoadCurrentObject;

  FName := GetStringProperty('name');
end;

function TEntity.LoadDepentObjects : Boolean;
begin
  Result := FQuantity.Reload(GetIntegerProperty('quantity_id')) and
    FPeriod.Reload(GetIntegerProperty('period_id')) and
    FShedule.Reload(GetIntegerProperty('shedule_id'));
end;

function TEntity.Load : Boolean;
begin
  Result := inherited Load and FGreaseBag.Reload(-1) and FNodeBag.Reload(-1);
end;

function TEntity.SaveDepentObjects : Boolean;
begin
  Result := FShedule.Save and FQuantity.Save and FPeriod.Save;
end;

procedure TEntity.SaveCurrentObject;
begin
  SetStringProperty('name', FName);
  SetIntegerProperty('quantity_id', FQuantity.ID);
  SetIntegerProperty('period_id', FPeriod.ID);
  SetIntegerProperty('shedule_id', FShedule.ID);
end;

function TEntity.Save : Boolean;
begin
  Result := inherited Save and FGreaseBag.Save and FNodeBag.Save;
end;

function TEntity.DeleteDepentObjects : Boolean;
begin
  Result := FShedule.Delete and FQuantity.Delete and FPeriod.Delete;
end;

procedure TEntity.Assign (AEntity : TEntity);
begin
  FName := AEntity.Name;
  FGreaseBag.Assign(AEntity.GreaseBag);
  FNodeBag.Assign(AEntity.NodeBag);
  FShedule.Assign(AEntity.Shedule);
  FQuantity.Assign(AEntity.Quantity);
  FPeriod.Assign(AEntity.Period);
end;

end.
