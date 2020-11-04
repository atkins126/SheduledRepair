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
unit dataproviders.common;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, database, sqlite3.table, container.arraylist, utils.functor,
  sqlite3.result, sqlite3.result_row, renderer.profile.objectprofile, 
  objects.common, rules.rulesbag;

type
  TCommonDataProvider = class
  public
    constructor Create;
    destructor Destroy; override;
    
    function Load : Boolean; virtual;

    procedure Append (AObject : TCommonObject);
    procedure Remove (AObjectIndex : Cardinal);

    function GetObject (AObjectIndex : Cardinal) : TCommonObject;
    function GetObjectProfile (AObjectIndex : Cardinal) : 
      TRendererObjectProfile;
  protected
    { Calculate object renderer profile by rules. }
    function CalculateObjectProfile (AObject : TCommonObject) : 
      TRendererObjectProfile; virtual;

    { Set default object renderer profile. }
    function DefaultObjectProfile :  TRendererObjectProfile; virtual;

    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; virtual; abstract;

    { Load concrete object. }
    function LoadConcreteObject (AID : Int64) : TCommonObject; virtual; 
      abstract;
  protected
    type
      TObjectItem = class
      public
        constructor Create (AObject : TCommonObject; AProfile : 
          TRendererObjectProfile);
      protected
        FObject : TCommonObject;
        FProfile : TRendererObjectProfile;
      public
        property Obj : TCommonObject read FObject;
        property Profile : TRendererObjectProfile read FProfile;
      end;

      TObjectItemCompareFunctor = class
        (specialize TBinaryFunctor<TObjectItem, Integer)
      public
        function Call (AValue1, AValue2 : TObjectItem) : Integer; override;
      end;

      TObjectsList = class
        (specialize TArrayList<TObjectItem, TObjectItemCompareFunctor>);
  protected
    FObjectsList : TObjectsList;
    FRulesBag : TRulesBag;
  end;

implementation

{ TCommonDataProvider.TObjectItem }

constructor TCommonDataProvider.TObjectItem.Create (AObject : TCommonObject;
  AProfile : TRendererObjectProfile);
begin
  FObject := AObject;
  FProfile := AProfile;
end;

{ TCommonDataProvider.TObjectItemCompareFunctor }

function TCommonDataProvider.TObjectItemCompareFunctor.Call (AValue1, AValue2 :
  TObjectItem) : Integer;
begin
  if AValue1.Obj.ID < AValue2.Obj.ID then
    Result := -1
  else if AValue2.Obj.ID < AValue1.Obj.ID then
    Result := 1
  else 
    Result := 0;
end;

{ TCommonDataProvider }

constructor TCommonDataProvider.Create;
begin
  FObjectsList := TObjectsList.Create;
  FRulesBag := TRulesBag.Create(-1, nil);
end;

destructor TCommonDataProvider.Destroy;
begin
  FreeAndNil(FObjectsList);
  FreeAndNil(FRulesBag);
  inherited Destroy;
end;

function TCommonDataProvider.DefaultObjectProfile : TRendererObjectProfile;
begin
  Result := TRendererObjectProfile.Create(-1);
end;

function TCommonDataProvider.CalculateObjectProfile (AObject : TCommonObject) : 
  TRendererObjectProfile;
begin
  if not FRulesBag.FirstEntry.HasValue then
    Exit(DefaultObjectProfile);

  Result := FRulesBag.FirstEntry.Value.Profile;  
end;

procedure TCommonDataProvider.Append (AObject : TCommonObject);
begin
  FObjectsList.Append(TObjectItem.Create(AObject, 
    CalculateObjectProfile(AObject)));
end;

procedure TCommonDataProvider.Remove (AObjectIndex : Cardinal);
begin
  if AObjectIndex < FObjectsList.Length then
    FObjectsList.Remove(AObjectIndex);
end;

function TCommonDataProvider.GetObject (AObjectIndex : Cardinal) : 
  TCommonObject;
begin
  if AObjectIndex < FObjectsList.Length then
    Exit(FObjectsList.Value[AObjectIndex].Obj);
  
  Result := nil;
end;

function TCommonDataProvider.GetObjectProfile (AObjectIndex : Cardinal) : 
  TRendererObjectProfile;
begin
  if AObjectIndex < FObjectsList.Length then
    Exit(FObjectsList.Value[AObjectIndex].Profile);
  
  Result := nil;
end;

function TCommonDataProvider.Load : Boolean;
  Table : TSQLite3Table;
  ResultRows : TSQLite3Result;
  Row : TSQLite3ResultRow;
  Item : TCommonObject;
begin
  Table := TSQLite3Table.Create(DB.Errors, DB.Handle, LoadObjectsTableName);
  ResultRows := Table.Select.Field('id').Get;

  for Row in ResultRows do
  begin
    Item := LoadConcreteObject(Row.GetIntegerValue('id'));

    if Item = nil then
      Continue;
    
    FObjectsList.Append(TObjectItem.Create(Item, 
      CalculateObjectProfile(Item)));
  end;
end;



end.
