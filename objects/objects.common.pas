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
  sqlite3.delete, sqlite3.result, sqlite3.schema;

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
    function Load : Boolean; virtual; abstract;

    { Reload object from database. }
    function Reload (AID : Int64) : Boolean;

    { Save object to database. }
    function Save : Boolean; virtual; abstract;

    { Delete object from database. }
    function Delete : Boolean; virtual; abstract;

    { Return object ID. }
    function ID : Int64;
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); virtual; abstract;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; virtual;

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
  end;

implementation

{ TCommonObject }

constructor TCommonObject.Create (AID : Int64);
begin
  FID := AID;
  FTable := TSQLite3Table.Create(DB.Errors, DB.Handle, Table);
end;

destructor TCommonObject.Destroy;
begin
  FreeAndNil(FTable);
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
