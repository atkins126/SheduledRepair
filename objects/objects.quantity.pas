(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(*                                                                            *)
(* Copyright (c) 2020                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpassqlite                ivan@semenkov.pro *)
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
unit objects.quantity;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  objects.measure;

type
  TQuantity = class(TCommonObject)
  private
    const
      QUANTITY_TABLE_NAME = 'quantity';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Check database table scheme. }
    function CheckSchema : Boolean; override;

    { Get object database table name. }
    function Table : String; override;

    { Load object from database. }
    function Load : Boolean; override;

    { Save object to database. }
    function Save : Boolean; override;
  protected
    FCount : Double;
    FMeasure : TMeasure;
  public
    property Count : Double read FCount write FCount;
    property Measure : TMeasure read FMeasure write FMeasure;
  end;

implementation

{ TQuantity }

constructor TQuantity.Create (AID : Int64);
begin
  inherited Create (AID);
  FCount := 0;
  FMeasure := TMeasure.Create(-1);
end;

destructor TQuantity.Destroy;
begin
  FreeAndNil(FMeasure);
  inherited Destroy;
end;

function TQuantity.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;
  
  Schema
    .Id
    .Float('count').NotNull
    .Integer('measure_id').NotNull;

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema) and FMeasure.CheckSchema;  

  FreeAndNil(Schema);
end;

function TQuantity.Table : String;
begin
  Result := QUANTITY_TABLE_NAME;
end;

function TQuantity.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
begin
  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  FCount := row.Row.GetDoubleValue('count');
  Result := FMeasure.Reload(row.Row.GetIntegerValue('measure_id'));
end;

function TQuantity.Save : Boolean;
begin
  if not FMeasure.Save then
    Exit(False);

  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('count', FCount)
      .Update('measure_id', FMeasure.ID).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('count', FCount)
      .Value('measure_id', FMeasure.ID).Get > 0);
    UpdateObjectID;
  end;
end;

end.
