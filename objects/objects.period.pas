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
unit objects.period;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  objects.quantity;

type
  TPeriod = class(TCommonObject)
  private
    const
      PERIOD_TABLE_NAME = 'period';
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
    FQuantity : TQuantity;
  public
    property Quantity : TQuantity read FQuantity write FQuantity;
  end;

implementation

{ TPeriod }

constructor TPeriod.Create (AID : Int64);
begin
  inherited Create (AID);
  FQuantity := TQuantity.Create(-1);
end;

destructor TPeriod.Destroy;
begin
  FreeAndNil(FQuantity);
  inherited Destroy;
end;

function TPeriod.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;
  
  Schema
    .Id
    .Integer('quantity_id');

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema);  

  FreeAndNil(Schema);
end;

function TPeriod.Table : String;
begin
  Result := PERIOD_TABLE_NAME;
end;

function TPeriod.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
begin
  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  FQuantity.Reload(row.Row.GetIntegerValue('quantity_id'));
  Result := True;
end;

function TPeriod.Save : Boolean;
begin
  if not FQuantity.Save then
    Exit(False);  

  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('quantity_id', FQuantity.ID).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('quantity_id', FQuantity.ID).Get > 0);
    UpdateObjectID;
  end;
end;

end.
