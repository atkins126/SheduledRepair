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
unit objects.greasebag;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  container.arraylist, utils.functor, objects.greasebundle;

type
  TGreaseBag = class(TCommonObject)
  private
    const
      GREASE_BAG_TABLE_NAME = 'greasebag';
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
    procedure Append (AGreaseBundle : TGreaseBundle);

    { Remove grease bundle from current bag. }
    procedure Remove (AGreaseBundle : TGreaseBundle);

    { Object deep copy. }
    procedure Assign (AGreaseBag : TGreaseBag);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;

    { Store current object to database. }
    procedure SaveCurrentObject; override;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; override;

    { Save all grease bundles associated with current object. }
    function SaveGreaseBundles : Boolean;

    { Load all grease bundles associated with current object. }
    function LoadGreaseBundles : Boolean;

    { Delete all grease bundles associated with current object. }
    function DeleteGreaseBundles : Boolean;
  protected
    type
      TGreaseBundleCompareFunctor = class
        (specialize TUnsortableFunctor<TGreaseBundle>);
      
      TGreaseBundleList = class
        (specialize TArrayList<TGreaseBundle, TGreaseBundleCompareFunctor>);  
  public
    { Get enumerator for in operator. }
    function GetEnumerator : TGreaseBundleList.TIterator;
  private
    FObject : TCommonObject;
    FGreaseBundleList : TGreaseBundleList;
  end;

implementation

{ TGreaseBag }

constructor TGreaseBag.Create (AID : Int64; AObject : TCommonObject);
begin
  inherited Create (AID);
  FObject := AObject;
  FGreaseBundleList := TGreaseBundleList.Create;
end;

destructor TGreaseBag.Destroy;
begin
  FreeAndNil(FGreaseBundleList);
  inherited Destroy;
end;

procedure TGreaseBag.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Integer('greasebundle_id').NotNull
    .Text('object_name').NotNull
    .Integer('object_id').NotNull;
end;

function TGreaseBag.CheckDepentSchemes : Boolean;
var
  Bundle : TGreaseBundle;
begin
  Bundle := TGreaseBundle.Create(-1);
  Result := Bundle.CheckSchema;
  FreeAndNil(Bundle);
end;

function TGreaseBag.Table : String;
begin
  Result := GREASE_BAG_TABLE_NAME;
end;

function TGreaseBag.LoadDepentObjects : Boolean;
var
  result_rows : TSQLite3Result;
  row : TSQLite3ResultRow;
  GreaseBundle : TGreaseBundle;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := inherited LoadCurrentObject;

  result_rows := FTable.Select.All.Where('object_id', FObject.ID)
    .Where('object_name', FObject.Table).Get;
  FGreaseBundleList.Clear;

  for row in result_rows do
  begin
    GreaseBundle := 
      TGreaseBundle.Create(row.GetIntegerValue('greasebundle_id'));

    if GreaseBundle.Load then
      FGreaseBundleList.Append(GreaseBundle);
  end;

  Result := True;
end;

function TGreaseBag.SaveDepentObjects : Boolean;
begin
  
end;

procedure TGreaseBag.SaveCurrentObject;
begin
  
end;

function TGreaseBag.SaveGreaseBundles : Boolean;
var
  GreaseBundle : TGreaseBundle;
begin
  if not FGreaseBundleList.FirstEntry.HasValue then
    Exit(True);

  Result := True;
  for GreaseBundle in FGreaseBundleList do
  begin
    if not GreaseBundle.Save then
      Continue;

    { Check if current grease bundle stored in database. }  
    if FTable.Update
      .Update('greasebundle_id', GreaseBundle.ID)
      .Where('greasebundle_id', GreaseBundle.ID)
      .Where('object_name', FObject.Table)
      .Where('object_id', FObject.ID)
      .Get > 0 then
      Continue;

    { Save current grease bundle in database. }
    Result := Result and (FTable.Insert
      .Value('greasebundle_id', GreaseBundle.ID)
      .Value('object_name', FObject.Table)
      .Value('object_id', FObject.ID)
      .Get > 0);
  end;
end;

function TGreaseBag.LoadGreaseBundles : Boolean;
begin

end;

function TGreaseBag.Save : Boolean;
var
  GreaseBundle : TGreaseBundle;
  updated_rows : Integer;
begin
  if FObject = nil then
    Exit(False);

  if FObject.ID = -1 then
    FObject.Save;

  if not FGreaseBundleList.FirstEntry.HasValue then
    Exit(False);

  for GreaseBundle in FGreaseBundleList do
  begin
    GreaseBundle.Save;
    
    updated_rows := UpdateRow.Update('greasebundle_id', GreaseBundle.ID)
      .Where('object_id', FObject.ID).Where('object_name', FObject.Table)
      .Where('greasebundle_id', GreaseBundle.ID).Get;

    if updated_rows > 0 then
      continue;
    
    InsertRow.Value('greasebundle_id', GreaseBundle.ID)
      .Value('object_id', FObject.ID).Value('object_name', FObject.Table).Get;
    UpdateObjectID;
  end;

  Result := True;
end;

function TGreaseBag.DeleteGreaseBundles : Boolean;
begin
  Result := FTable.Delete
    .Where('object_name', FObject.Table)
    .Where('object_id', FObject.ID)
    .Get > 0;
end;

function TGreaseBag.Delete : Boolean;
var
  GreaseBundle : TGreaseBundle;
begin
  if (FObject = nil) or (ID = -1) then
    Exit(False);

  for GreaseBundle in FGreaseBundleList do
  begin
    GreaseBundle.Delete;
  end; 

  Result := inherited Delete;
end;

procedure TGreaseBag.Append (AGreaseBundle : TGreaseBundle);
var
  updated_rows : Integer;
begin
  FGreaseBundleList.Append(AGreaseBundle);

  {if (FObject <> nil) and (FObject.ID <> -1) then
  begin
    if not AGreaseBundle.Save then
      Exit;

    updated_rows := UpdateRow.Update('greasebundle_id', AGreaseBundle.ID)
      .Where('object_id', FObject.ID).Where('object_name', FObject.Table)
      .Where('greasebundle_id', AGreaseBundle.ID).Get;

    if updated_rows > 0 then
      Exit;

    InsertRow.Value('greasebundle_id', AGreaseBundle.ID)
      .Value('object_id', FObject.ID).Value('object_name', FObject.Table).Get;
  end;}
end;

procedure TGreaseBag.Remove (AGreaseBundle : TGreaseBundle);
var
  Index : Integer;
begin
  Index := FGreaseBundleList.IndexOf(AGreaseBundle);

  if Index <> -1 then
  begin
    FGreaseBundleList.Remove(Index);
    {
    if (FObject <> nil) and (FObject.ID <> -1) then
    begin
      FTable.Delete.Where('greasebundle_id', AGreaseBundle.ID)
        .Where('object_id', FObject.ID).Where('object_name', FObject.Table)
        .Get;
    end;}
  end;
end;

function TGreaseBag.GetEnumerator : TGreaseBundleList.TIterator;
begin
  Result := FGreaseBundleList.GetEnumerator;
end;

procedure TGreaseBag.Assign (AGreaseBag : TGreaseBag);
var
  grease_bundle : TGreaseBundle;
  bundle : TGreaseBundle;
begin
  if not AGreaseBag.FGreaseBundleList.FirstEntry.HasValue then
    Exit;

  for grease_bundle in AGreaseBag.FGreaseBundleList do
  begin
    bundle := TGreaseBundle.Create(-1);
    bundle.Assign(grease_bundle);
    FGreaseBundleList.Append(bundle);
  end;
end;

end.
