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

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  objects.greasebag, objects.nodebag, objects.shedule, objects.period,
  objects.quantity;

type
  TEntity = class(TCommonObject)
  private
    const
      ENTITY_TABLE_NAME = 'entity';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Load object from database. }
    function Load : Boolean; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Delete object from database. }
    function Delete : Boolean; override;

    { Object deep copy. }
    procedure Assign (AEntity : TEntity);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;
  protected
    FName : String;
    FGreaseBag : TGreaseBag;
    FNodeBag : TNodeBag;
    FShedule : TShedule;
    FQuantity : TQuantity;
    FPeriod : TPeriod;
  public
    property Name : String read FName write FName;
    property GreaseBag : TGreaseBag read FGreaseBag write FGreaseBag;
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
  FName := '';
  FGreaseBag := TGreaseBag.Create(-1, Self);
  FNodeBag := TNodeBag.Create(-1, Self);
  FShedule := TShedule.Create(-1);
  FQuantity := TQuantity.Create(-1);
  FPeriod := TPeriod.Create(-1);
end;

destructor TEntity.Destroy;
begin
  FreeAndNil(FGreaseBag);
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

function TEntity.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
begin
  if ID = -1 then
    Exit(False);

  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  FName := row.Row.GetStringValue('name');
  FGreaseBag.Reload(-1);
  FNodeBag.Reload(-1);

  Result := FQuantity.Reload(row.Row.GetIntegerValue('quantity_id')) and
    FPeriod.Reload(row.Row.GetIntegerValue('period_id')) and
    FShedule.Reload(row.Row.GetIntegerValue('shedule_id'));
end;

function TEntity.Save : Boolean;
var
  updated_rows : Integer;
begin
  if not FShedule.Save then
    Exit(False);

  if not FQuantity.Save then
    Exit(False);

  if not FPeriod.Save then
    Exit(False);

  if ID <> -1 then
  begin
    updated_rows := UpdateRow.Update('name', FName)
      .Update('quantity_id', FQuantity.ID).Update('period_id', FPeriod.ID)
      .Update('shedule_id', FShedule.ID).Get;
  end else 
  begin
    updated_rows := InsertRow.Value('name', FName)
      .Value('quantity_id', FQuantity.ID).Value('period_id', FPeriod.ID)
      .Value('shedule_id', FShedule.ID).Get;
    UpdateObjectID;
  end;

  FGreaseBag.Save;
  FNodeBag.Save;

  Result := (updated_rows > 0);
end;

function TEntity.Delete : Boolean;
begin
  if ID <> -1 then
    Result := FShedule.Delete and FQuantity.Delete and FPeriod.Delete and
      (DeleteRow.Get > 0)
  else
    Result := False;
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
