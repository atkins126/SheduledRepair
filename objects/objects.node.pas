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
unit objects.node;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  objects.greasebag, objects.period, objects.shedule;

type
  TNode = class(TCommonObject)
  private
    const
      NODE_TABLE_NAME = 'node';
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
    FName : String;
    FGreaseBag : TGreaseBag;
    FPeriod : TPeriod;
    FShedule : TShedule;
  public
    property Name : String read FName write FName;
    property GreaseBag : TGreaseBag read FGreaseBag write FGreaseBag;
    property Period : TPeriod read FPeriod write FPeriod;
    property Shedule : TShedule read FShedule write FShedule;
  end;

implementation

{ TPeriod }

constructor TNode.Create (AID : Int64);
begin
  inherited Create (AID);
  FName := '';
  FGreaseBag := TGreaseBag.Create(-1);
  FPeriod := TPeriod.Create(-1);
  FShedule := TShedule.Create(-1);
end;

destructor TNode.Destroy;
begin
  FreeAndNil(FGreaseBag);
  FreeAndNil(FPeriod);
  FreeAndNil(FShedule);
  inherited Destroy;
end;

function TNode.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;
  
  Schema
    .Id
    .Text('name').NotNull
    .Integer('period_id')
    .Integer('shedule_id');

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema) and FGreaseBag.CheckSchema and
    FPeriod.CheckSchema and FShedule.CheckSchema;  

  FreeAndNil(Schema);
end;

function TNode.Table : String;
begin
  Result := NODE_TABLE_NAME;
end;

function TNode.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
begin
  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  FName := row.Row.GetStringValue('name');
  FGreaseBag.Entity := @Self;
  Result := FGreaseBag.Reload(-1) and
    FPeriod.Reload(row.Row.GetIntegerValue('period_id')) and
    FShedule.Reload(row.Row.GetIntegerValue('shedule_id'));
end;

function TNode.Save : Boolean;
var
  updated_rows : Integer;
begin
  if not FPeriod.Save then
    Exit(False);

  if not FShedule.Save then
    Exit(False);    

  if ID <> -1 then
  begin
    updated_rows := UpdateRow.Update('name', FName)
      .Update('period_id', FPeriod.ID)
      .Update('shedule_id', FShedule.ID).Get;
  end else 
  begin
    updated_rows := InsertRow.Value('name', FName)
      .Value('period_id', FPeriod.ID)
      .Value('shedule_id', FShedule.ID).Get;
    UpdateObjectID;
  end;

  FGreaseBag.Entity := @Self;
  if not FGreaseBag.Save then
    Exit(False);

  Result := (updated_rows > 0);
end;

end.
