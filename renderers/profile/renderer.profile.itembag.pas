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
unit renderer.profile.itembag;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  container.arraylist, utils.functor, renderer.profile.profileitem;

type
  TProfileItemBag = class(TCommonObject)
  private
    const
      PROFILE_ITEM_BAG_TABLE_NAME = 'profileitem_bag';
  public
    constructor {%H-}Create (AID : Int64; AObject : TCommonObject);
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Add new profile item to current bag. }
    procedure Append (AProfileItem : TRendererProfileItem);

    { Remove profile item from current bag. }
    procedure Remove (AProfileItem : TRendererProfileItem);

    { Object deep copy. }
    procedure Assign (AItemBag : TProfileItemBag);

    { Find profile item by name in current bag. }
    function Search (AName : String) : TRendererProfileItem;
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

    { Save all profile items associated with current object. }
    function SaveItems : Boolean;

    { Load all profile items associated with current object. }
    function LoadItems : Boolean;

    { Delete current object from database. }
    function DeleteCurrentObject : Boolean; override;

    { Delete all profile items associated with current object. }
    function DeleteItems : Boolean;

    { Delete all dependent objects. }
    function DeleteDepentObjects : Boolean; override;
  protected
    type
      TProfileItemsCompareFunctor = class
        ({$IFDEF FPC}specialize{$ENDIF} TBinaryFunctor<TRendererProfileItem,
        Integer>)
      public
        function Call (AValue1, AValue2 : TRendererProfileItem) : Integer;
          override;
      end;
      
      TProfileItemsList = {$IFDEF FPC}type specialize{$ENDIF}
        TArrayList<TRendererProfileItem,
        TProfileItemsCompareFunctor>;
  public
    type
      TIterator = TProfileItemsList.TIterator;
  public
    { Get enumerator for in operator. }
    function GetEnumerator : TProfileItemsList.TIterator;
  private
    FObject : TCommonObject;
    FProfileItemsList : TProfileItemsList;
  end;

implementation

{ TProfileItemsCompareFunctor }

function TProfileItemBag.TProfileItemsCompareFunctor.Call (AValue1, AValue2 :
  TRendererProfileItem) : Integer;
begin
  if AValue1.Name < AValue2.Name then
    Result := -1
  else if AValue2.Name < AValue1.Name then
    Result := 1
  else
    Result := 0;
end;

{ TProfileItemBag }

constructor TProfileItemBag.Create (AID : Int64; AObject : TCommonObject);
begin
  inherited Create (AID);
  FObject := AObject;
  FProfileItemsList := TProfileItemsList.Create;
end;

destructor TProfileItemBag.Destroy;
begin
  FreeAndNil(FProfileItemsList);
  inherited Destroy;
end;

procedure TProfileItemBag.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Integer('profileitem_id').NotNull
    .Text('object_name').NotNull
    .Integer('object_id').NotNull;
end;

function TProfileItemBag.CheckDepentSchemes : Boolean;
var
  ProfileItem : TRendererProfileItem;
begin
  ProfileItem := TRendererProfileItem.Create(-1);
  Result := ProfileItem.CheckSchema;
  FreeAndNil(ProfileItem);
end;

function TProfileItemBag.Table : String;
begin
  Result := PROFILE_ITEM_BAG_TABLE_NAME;
end;

function TProfileItemBag.LoadCurrentObject : Boolean;
begin
  Result := True;
end;

function TProfileItemBag.LoadDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := LoadItems;
end;

function TProfileItemBag.SaveDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := SaveItems;
end;

function TProfileItemBag.SaveItems : Boolean;
var
  ProfileItem : TRendererProfileItem;
begin
  if not FProfileItemsList.FirstEntry.HasValue then
    Exit(True);

  Result := True;
  for ProfileItem in FProfileItemsList do
  begin
    if not ProfileItem.Save then
      Continue;

    { Check if current profile item stored in database. }  
    if FTable.Update
      .Update('profileitem_id', ProfileItem.ID)
      .Where('profileitem_id', ProfileItem.ID)
      .Where('object_name', FObject.Table)
      .Where('object_id', FObject.ID)
      .Get > 0 then
      Continue;

    { Save current profile item in database. }
    Result := Result and (FTable.Insert
      .Value('profileitem_id', ProfileItem.ID)
      .Value('object_name', FObject.Table)
      .Value('object_id', FObject.ID)
      .Get > 0);
  end;
end;

function TProfileItemBag.LoadItems : Boolean;
var
  ResultRows : TSQLite3Result;
  Row : TSQLite3ResultRow;
  ProfileItem : TRendererProfileItem;
begin
  ResultRows := FTable.Select.All
    .Where('object_name', FObject.Table)
    .Where('object_id', FObject.ID)
    .Get;

  for Row in ResultRows do
  begin
    ProfileItem := TRendererProfileItem.Create(
      Row.GetIntegerValue('profileitem_id')
    );

    if ProfileItem.Load then
      FProfileItemsList.Append(ProfileItem);
  end;

  Result := True;
end;

function TProfileItemBag.DeleteCurrentObject : Boolean;
begin
  Result := True;
end;

function TProfileItemBag.DeleteItems : Boolean;
var
  ProfileItem : TRendererProfileItem;
begin
  if not FProfileItemsList.FirstEntry.HasValue then
    Exit(True);

  for ProfileItem in FProfileItemsList do
  begin
    ProfileItem.Delete;
  end;

  FTable.Delete
    .Where('object_name', FObject.Table)
    .Where('object_id', FObject.ID)
    .Get;

  FProfileItemsList.Clear;
  Result := True;
end;

function TProfileItemBag.DeleteDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := DeleteItems;
end;

procedure TProfileItemBag.Append (AProfileItem : TRendererProfileItem);
begin
  FProfileItemsList.Append(AProfileItem);
end;

procedure TProfileItemBag.Remove (AProfileItem : TRendererProfileItem);
var
  Index : Integer;
begin
  Index := FProfileItemsList.IndexOf(AProfileItem);

  if Index <> -1 then
    FProfileItemsList.Remove(Index);
end;

function TProfileItemBag.GetEnumerator : TProfileItemsList.TIterator;
begin
  Result := FProfileItemsList.GetEnumerator;
end;

procedure TProfileItemBag.Assign (AItemBag : TProfileItemBag);
var
  ProfileItem, Item : TRendererProfileItem;
begin
  if not AItemBag.FProfileItemsList.FirstEntry.HasValue then
    Exit;

  for ProfileItem in AItemBag.FProfileItemsList do
  begin
    Item := TRendererProfileItem.Create(-1);
    Item.Assign(ProfileItem);
    FProfileItemsList.Append(Item);
  end;
end;

function TProfileItemBag.Search (AName : String) : TRendererProfileItem;
var
  Index : Integer;
  ProfileItem : TRendererProfileItem;
begin
  ProfileItem := TRendererProfileItem.Create(-1);
  ProfileItem.Name := AName;

  Index := FProfileItemsList.IndexOf(ProfileItem);
  if Index <> -1 then
    Exit(FProfileItemsList.Value[Index]);

  Result := nil;
end;

end.
