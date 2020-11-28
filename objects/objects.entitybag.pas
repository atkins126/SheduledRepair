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
unit objects.entitybag;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  container.arraylist, utils.functor, objects.entity;

type
  TEntityBag = class(TCommonObject)
  private
    const
      ENTITY_BAG_TABLE_NAME = 'entitybag';
  public
    constructor Create (AID : Int64; AObject : TCommonObject); reintroduce;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Add new grease bundle to current bag. }
    procedure Append (AEntity : TEntity);

    { Remove grease bundle from current bag. }
    procedure Remove (AEntity : TEntity);

    { Object deep copy. }
    procedure Assign (AEntityBag : TEntityBag);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; override;

    { Save all grease bundles associated with current object. }
    function SaveEntities : Boolean;

    { Load all grease bundles associated with current object. }
    function LoadEntities : Boolean;

    { Delete current object from database. }
    function DeleteCurrentObject : Boolean; override;

    { Delete all grease bundles associated with current object. }
    function DeleteEntities : Boolean;

    { Delete all dependent objects. }
    function DeleteDepentObjects : Boolean; override;
  public
    type
      TEntityCompareFunctor = class
       (specialize TBinaryFunctor<TEntity, Integer>)
      public
        function Call (AValue1, AValue2 : TEntity) : Integer; override;
      end;
      
      TEntityList = class
        (specialize TArrayList<TEntity, TEntityCompareFunctor>);  
  public
    { Get enumerator for in operator. }
    function GetEnumerator : TEntityList.TIterator;
  private
    FObject : TCommonObject;
    FEntityList : TEntityList;
  end;

implementation

{ TEntityBag.TEntityCompareFunctor }

function TEntityBag.TEntityCompareFunctor.Call (AValue1, AValue2 : TEntity) :
  Integer;
begin
  if AValue1.Name < AValue2.Name then
    Result := -1
  else if AValue2.Name < AValue1.Name then
    Result := 1
  else
    Result := 0;
end;

{ TEntityBag }

constructor TEntityBag.Create (AID : Int64; AObject : TCommonObject);
begin
  inherited Create (AID);
  FObject := AObject;
  FEntityList := TEntityList.Create;
end;

destructor TEntityBag.Destroy;
begin
  FreeAndNil(FEntityList);
  inherited Destroy;
end;

procedure TEntityBag.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Integer('entity_id').NotNull
    .Text('object_name').NotNull
    .Integer('object_id').NotNull;
end;

function TEntityBag.CheckDepentSchemes : Boolean;
var
  Entity : TEntity;
begin
  Entity := TEntity.Create(-1);
  Result := Entity.CheckSchema;
  FreeAndNil(Entity); 
end;

function TEntityBag.Table : String;
begin
  Result := ENTITY_BAG_TABLE_NAME;
end;

function TEntityBag.LoadCurrentObject : Boolean;
begin
  Result := True;
end;

function TEntityBag.LoadDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := LoadEntities;
end;

function TEntityBag.SaveDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := SaveEntities;
end;

function TEntityBag.SaveEntities : Boolean;
var
  Entity : TEntity;
begin
  if not FEntityList.FirstEntry.HasValue then
    Exit(True);

  Result := True;
  for Entity in FEntityList do
  begin
    if not Entity.Save then
      Continue;

    { Check if current entity stored in database. }
    if FTable.Update
      .Update('entity_id', Entity.ID)
      .Where('entity_id', Entity.ID)
      .Where('object_name', FObject.Table)
      .Where('object_id', FObject.ID)
      .Get > 0 then
      Continue;

    { Save current entity in database. }
    Result := Result and (FTable.Insert
      .Value('entity_id', Entity.ID)
      .Value('object_name', FObject.Table)
      .Value('object_id', FObject.ID)
      .Get > 0);
  end;
end;

function TEntityBag.LoadEntities : Boolean;
var
  ResultRows : TSQLite3Result;
  Row : TSQLite3ResultRow;
  Entity : TEntity;
begin
  ResultRows := FTable.Select.All
    .Where('object_name', FObject.Table)
    .Where('object_id', FObject.ID)
    .Get;

  for Row in ResultRows do
  begin
    Entity := TEntity.Create(Row.GetIntegerValue('entity_id'));

    if Entity.Load then
      FEntityList.Append(Entity);
  end;

  Result := True;
end;

function TEntityBag.DeleteCurrentObject : Boolean;
begin
  Result := True;
end;

function TEntityBag.DeleteEntities : Boolean;
var
  Entity : TEntity;
begin
  if not FEntityList.FirstEntry.HasValue then
    Exit(True);

  for Entity in FEntityList do
  begin
    Entity.Delete;
  end;

  FTable.Delete
    .Where('object_name', FObject.Table)
    .Where('object_id', FObject.ID)
    .Get;

  FEntityList.Clear;
  Result := True;
end;

function TEntityBag.DeleteDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := DeleteEntities;
end;

procedure TEntityBag.Append (AEntity : TEntity);
begin
  FEntityList.Append(AEntity);
end;

procedure TEntityBag.Remove (AEntity : TEntity);
var
  Index : Integer;
begin
  Index := FEntityList.IndexOf(AEntity);

  if Index <> -1 then
    FEntityList.Remove(Index);
end;

function TEntityBag.GetEnumerator : TEntityList.TIterator;
begin
  Result := FEntityList.GetEnumerator;
end;

procedure TEntityBag.Assign (AEntityBag : TEntityBag);
var
  entity_item, entity_new : TEntity;
begin
  if not AEntityBag.FEntityList.FirstEntry.HasValue then
    Exit;

  for entity_item in AEntityBag.FEntityList do
  begin
    entity_new := TEntity.Create(-1);
    entity_new.Assign(entity_item);
    FEntityList.Append(entity_new);
  end;
end;

end.
