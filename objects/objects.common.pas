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
unit objects.common;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, database, sqlite3.table, sqlite3.insert, sqlite3.update, 
  sqlite3.delete, sqlite3.result, sqlite3.schema, container.list, utils.functor;

type
  TCommonObject = class
  public
    constructor Create (AID : Int64); virtual;
    destructor Destroy; override;
    
     { Check database table scheme. }
    function CheckSchema : Boolean;

    { Get object database table name. }
    function Table : String; virtual; abstract;

    { Load object from database. }
    function Load : Boolean;

    { Reload object from database. }
    function Reload (AID : Int64) : Boolean;

    { Save object to database. }
    function Save : Boolean; virtual;

    { Delete object from database. }
    function Delete : Boolean; virtual;

    { Return object ID. }
    function ID : Int64;
  protected
    type
      { Object property value types. }
      TValueType = (
        TYPE_INTEGER,
        TYPE_DOUBLE,
        TYPE_STRING
      );

      { Object property value type. }
      TValue = record
        Name : String;
        ValueType : TValueType;
        IntegerValue : Integer;
        DoubleValue : Double;
        StringValue : String;
      end;

      { Object properties list. }
      TValueUnsortableFunctor = class(specialize TUnsortableFunctor<TValue>);
      TValuesList = class(specialize TList<TValue, TValueUnsortableFunctor>);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); virtual; abstract;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; virtual;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; virtual;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; virtual;

    { Get current object property from database result row. }
    function GetStringProperty (AName : String) : String;
    function GetIntegerProperty (AName : String) : Integer;
    function GetDoubleProperty (AName : String) : Double;

    { Store current object to database. }
    function SaveCurrentObject : Boolean; virtual;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; virtual;

    { Set current object property. }
    procedure SetStringProperty (AName : String; AValue : String);
    procedure SetIntegerProperty (AName : String; AValue : Integer);
    procedure SetDoubleProperty (AName : String; AValue : Double);

    { Delete current object from database. }
    function DeleteCurrentObject : Boolean;


    { Return row by object id. }
    function GetRowIterator : TSQLite3Result.TRowIterator;

    { Return TSQLite3Insert for current object. }
    function InsertRow : TSQLite3Insert;

    { Return TSQLite3Update for current object. }
    function UpdateRow : TSQLite3Update;

    { Return TSQLite3Delete for current object. }
    function DeleteRow : TSQLite3Delete;

    { Update object ID. }
    procedure UpdateObjectID;
  protected
    FID : Int64;
    FTable : TSQLite3Table;
    FRow : TSQLite3Result.TRowIterator;
    FPropertiesList : TValuesList;
  end;

implementation

{ TCommonObject }

constructor TCommonObject.Create (AID : Int64);
begin
  FID := AID;
  FTable := TSQLite3Table.Create(DB.Errors, DB.Handle, Table);
  FRow := nil;
  FPropertiesList := TValuesList.Create;
end;

destructor TCommonObject.Destroy;
begin
  FreeAndNil(FRow);
  FreeAndNil(FTable);
  FreeAndNil(FPropertiesList);
  inherited Destroy;
end;

function TCommonObject.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;
  PrepareSchema(Schema);

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema) and CheckDepentSchemes;
  FreeAndNil(Schema);  
end;

function TCommonObject.CheckDepentSchemes : Boolean;
begin
  Result := True;
end;

function TCommonObject.Reload (AID : Int64) : Boolean;
begin
  FID := AID;
  Result := Load;
end;

function TCommonObject.ID : Int64;
begin
  Result := FID;
end;

function TCommonObject.Load : Boolean;
begin
  if not LoadCurrentObject then
    Exit(False);
  
  Result := LoadDepentObjects;
end;

function TCommonObject.LoadCurrentObject : Boolean;
begin
  if FID = -1 then
    Exit(False);

  FRow := FTable.Select.All.Where('id', FID).Get.FirstRow;

  if not FRow.HasRow then
    Exit(False);

  Result := True;
end;

function TCommonObject.LoadDepentObjects : Boolean;
begin
  Result := True;
end;

function TCommonObject.GetStringProperty (AName : String) : String;
begin
  if FRow = nil then
    Exit('');

  Result := FRow.Row.GetStringValue(AName);
end;

function TCommonObject.GetIntegerProperty (AName : String) : Integer;
begin
  if FRow = nil then
    Exit(0);

  Result := FRow.Row.GetIntegerValue(AName);
end;

function TCommonObject.GetDoubleProperty (AName : String) : Double;
begin
  if FRow = nil then
    Exit(0);

  Result := FRow.Row.GetDoubleValue(AName);
end;

procedure TCommonObject.SetStringProperty (AName : String; AValue : String);
var
  Value : TValue;
begin
  Value.Name := AName;
  Value.ValueType := TYPE_STRING;
  Value.StringValue := AValue;
  FPropertiesList.Append(Value);
end;

procedure TCommonObject.SetIntegerProperty (AName : String; AValue : Integer);
var
  Value : TValue;
begin
  Value.Name := AName;
  Value.ValueType := TYPE_INTEGER;
  Value.IntegerValue := AValue;
  FPropertiesList.Append(Value);
end;

procedure TCommonObject.SetDoubleProperty (AName : String; AValue : Double);
var
  Value : TValue;
begin
  Value.Name := AName;
  Value.ValueType := TYPE_DOUBLE;
  Value.DoubleValue := AValue;
  FPropertiesList.Append(Value);
end;

function TCommonObject.Save : Boolean;
begin
  if not SaveDepentObjects then
    Exit(False);

  Result := SaveCurrentObject;
end;

function TCommonObject.SaveCurrentObject : Boolean;
var
  Value : TValue;
  Update : TSQLite3Update;
  Insert : TSQLite3Insert;
begin
  if not FPropertiesList.FirstEntry.HasValue then
    Exit(True);

  if FID = -1 then
  begin
    Update := FTable.Update.Where('id', FID);
    for Value in FPropertiesList do
    begin
      case Value.ValueType of
        TYPE_INTEGER : Update.Update(Value.Name, Value.IntegerValue);
        TYPE_DOUBLE  : Update.Update(Value.Name, Value.DoubleValue);
        TYPE_STRING  : Update.Update(Value.Name, Value.StringValue);
      end;
    end;
    Result := Update.Get > 0;
  end else
  begin
    Insert := FTable.Insert;
    for Value in FPropertiesList do
    begin
      case Value.ValueType of
        TYPE_INTEGER : Insert.Value(Value.Name, Value.IntegerValue);
        TYPE_DOUBLE  : Insert.Value(Value.Name, Value.DoubleValue);
        TYPE_STRING  : Insert.Value(Value.Name, Value.StringValue);
      end;
    end;
    Result := Insert.Get > 0;

    if Result then
      FID := DB.GetLastInsertID;
  end;
end;

function TCommonObject.SaveDepentObjects : Boolean;
begin
  Result := True;
end;

function TCommonObject.Delete : Boolean;
begin
  Result := DeleteCurrentObject;
end;

function TCommonObject.DeleteCurrentObject : Boolean;
begin
  if ID = -1 then
    Exit(False);

  Result := (FTable.Delete.Where('id', FID).Get > 0);
  FID := -1;
end;



function TCommonObject.GetRowIterator : TSQLite3Result.TRowIterator;
begin
  Result := FTable.Select.All.Where('id', ID).Get.FirstRow;
end;

function TCommonObject.InsertRow : TSQLite3Insert;
begin
  Result := FTable.Insert;
end;

function TCommonObject.UpdateRow : TSQLite3Update;
begin
  Result := FTable.Update.Where('id', ID);
end;

function TCommonObject.DeleteRow : TSQLite3Delete;
begin
  Result := FTable.Delete.Where('id', ID);
end;

procedure TCommonObject.UpdateObjectID;
begin
  FID := DB.GetLastInsertID;
end;

end.
