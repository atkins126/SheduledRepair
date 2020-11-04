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
  container.arraylist, utils.functor, objects.greasebundle, objects.supplier,
  objects.grade;

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

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; override;

    { Save all grease bundles associated with current object. }
    function SaveGreaseBundles : Boolean;

    { Load all grease bundles associated with current object. }
    function LoadGreaseBundles : Boolean;

    { Delete current object from database. }
    function DeleteCurrentObject : Boolean; override;

    { Delete all grease bundles associated with current object. }
    function DeleteGreaseBundles : Boolean;

    { Delete all dependent objects. }
    function DeleteDepentObjects : Boolean; override;
  protected
    type
      TGreaseBundleCompareFunctor = class
        (specialize TBinaryFunctor<TGreaseBundle, Integer>)
      protected
        function CompareGreaseSupplier (ASupplier1, ASupplier2 : TSupplier) :
          Integer;
        function CompareGreaseGrade (AGrade1, AGrade2 : TGrade) : Integer;
      public
        function Call (AValue1, AValue2 : TGreaseBundle) : Integer; override;
      end;
      
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

{ TGreaseBag.TGreaseBundleCompareFunctor }

function TGreaseBag.TGreaseBundleCompareFunctor.CompareGreaseSupplier 
  (ASupplier1, ASupplier2 : TSupplier) : Integer;
begin
  if ASupplier1.Name < ASupplier2.Name then
    Result := -1
  else if ASupplier2.Name < ASupplier1.Name then
    Result := 1
  else
    Result := 0;
end;

function TGreaseBag.TGreaseBundleCompareFunctor.CompareGreaseGrade 
  (AGrade1, AGrade2 : TGrade) : Integer;
begin
  if AGrade1.Name < AGrade2.Name then
    Result := -1
  else if AGrade2.Name < AGrade1.Name then
    Result := 1
  else
    Result := 0;
end;

function TGreaseBag.TGreaseBundleCompareFunctor.Call (AValue1, AValue2 : 
  TGreaseBundle) : Integer;
begin
  if CompareGreaseSupplier(AValue1.Grease.Supplier, 
    AValue2.Grease.Supplier) < 1 then
    Result := -1
  else if CompareGreaseSupplier(AValue2.Grease.Supplier, 
    AValue1.Grease.Supplier) < 1 then
    Result := 1
  else begin
    if CompareGreaseGrade(AValue1.Grease.Grade, AValue2.Grease.Grade) < 1 then
      Result := -1
    else if CompareGreaseGrade(AValue2.Grease.Grade, 
      AValue2.Grease.Grade) < 1 then
      Result := 1
    else begin
      if AValue1.Quantity.Count < AValue2.Quantity.Count then
        Result := -1
      else if AValue2.Quantity.Count < AValue1.Quantity.Count then
        Result := 1
      else
        Result := 0; 
    end;
  end;
end;

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

function TGreaseBag.LoadCurrentObject : Boolean;
begin
  Result := True;
end;

function TGreaseBag.LoadDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := LoadGreaseBundles;
end;

function TGreaseBag.SaveDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := SaveGreaseBundles;
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
var
  ResultRows : TSQLite3Result;
  Row : TSQLite3ResultRow;
  GreaseBundle : TGreaseBundle;
begin
  ResultRows := FTable.Select.All
    .Where('object_name', FObject.Table)
    .Where('object_id', FObject.ID)
    .Get;

  for Row in ResultRows do
  begin
    GreaseBundle := TGreaseBundle.Create(
      Row.GetIntegerValue('greasebundle_id')
    );

    if GreaseBundle.Load then
      FGreaseBundleList.Append(GreaseBundle);
  end;

  Result := True;
end;

function TGreaseBag.DeleteCurrentObject : Boolean;
begin
  Result := True;
end;

function TGreaseBag.DeleteGreaseBundles : Boolean;
var
  GreaseBundle : TGreaseBundle;
begin
  if not FGreaseBundleList.FirstEntry.HasValue then
    Exit(True);

  for GreaseBundle in FGreaseBundleList do
  begin
    GreaseBundle.Delete;
  end;

  FTable.Delete
    .Where('object_name', FObject.Table)
    .Where('object_id', FObject.ID)
    .Get;

  FGreaseBundleList.Clear;
  Result := True;
end;

function TGreaseBag.DeleteDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := DeleteGreaseBundles;
end;

procedure TGreaseBag.Append (AGreaseBundle : TGreaseBundle);
begin
  FGreaseBundleList.Append(AGreaseBundle);
end;

procedure TGreaseBag.Remove (AGreaseBundle : TGreaseBundle);
var
  Index : Integer;
begin
  Index := FGreaseBundleList.IndexOf(AGreaseBundle);

  if Index <> -1 then
    FGreaseBundleList.Remove(Index);
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
