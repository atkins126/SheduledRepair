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
    constructor Create (AID : Int64; AObject : TCommonObject);
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Delete object from database. }
    function Delete : Boolean; override;

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

function TEntityBag.TEntityCompareFunctor.Call (AValue1, AValue2 :
  TEntity) : Integer;
begin
  if AValue1.ID < AValue2.ID then
    Result := -1
  else if AValue2.ID < AValue1.ID then
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
var
  result_rows : TSQLite3Result;
  row : TSQLite3ResultRow;
  Ent : TEntity;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := inherited LoadCurrentObject;

  result_rows := FTable.Select.All.Where('object_id', FObject.ID)
    .Where('object_name', FObject.Table).Get;
  FEntityList.Clear;

  for row in result_rows do
  begin
    Ent := TEntity.Create(row.GetIntegerValue('entity_id'));

    if Ent.Load then
      FEntityList.Append(Ent);
  end;

  Result := True;
end;

function TEntityBag.Save : Boolean;
var
  Ent : TEntity;
  updated_rows : Integer;
begin
  if (FObject = nil) or (FObject.ID = -1) or 
    (not FEntityList.FirstEntry.HasValue) then
    Exit(False);

  for Ent in FEntityList do
  begin
    Ent.Save;
    
    updated_rows := UpdateRow.Update('entity_id', Ent.ID)
      .Where('object_id', FObject.ID).Where('object_name', FObject.Table)
      .Where('entity_id', Ent.ID).Get;

    if updated_rows > 0 then
      continue;
    
    InsertRow.Value('entity_id', Ent.ID)
      .Value('object_id', FObject.ID).Value('object_name', FObject.Table).Get;
    UpdateObjectID;
  end;

  Result := True;
end;

function TEntityBag.Delete : Boolean;
var
  Ent : TEntity;
begin
  if (FObject = nil) or (ID = -1) then
    Exit(False);

  for Ent in FEntityList do
  begin
    Ent.Delete;
  end;

  Result := DeleteCurrentObject;
end;

procedure TEntityBag.Append (AEntity : TEntity);
var
  updated_rows : Integer;
begin
  FEntityList.Append(AEntity);

  if (FObject <> nil) and (FObject.ID <> -1) then
  begin
    if not AEntity.Save then
      Exit;

    updated_rows := UpdateRow.Update('entity_id', AEntity.ID)
      .Where('object_id', FObject.ID).Where('object_name', FObject.Table)
      .Where('entity_id', AEntity.ID).Get;

    if updated_rows > 0 then
      Exit;

    InsertRow.Value('entity_id', AEntity.ID)
      .Value('object_id', FObject.ID).Value('object_name', FObject.Table).Get;
  end;
end;

procedure TEntityBag.Remove (AEntity : TEntity);
var
  Index : Integer;
begin
  Index := FEntityList.IndexOf(AEntity);

  if Index <> -1 then
  begin
    FEntityList.Remove(Index);
    
    if (FObject <> nil) and (FObject.ID <> -1) then
    begin
      FTable.Delete.Where('entity_id', AEntity.ID)
        .Where('object_id', FObject.ID).Where('object_name', FObject.Table)
        .Get;
    end;
  end;
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
