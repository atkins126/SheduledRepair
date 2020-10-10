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
unit objects.job;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  objects.entity, objects.period, objects.shedule;

type
  TJob = class(TCommonObject)
  private
    const
      JOB_TABLE_NAME = 'job';
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

    { Delete object from database. }
    function Delete : Boolean; override;
  protected
    FName : String;
    FEntity : TEntity;
    FPeriod : TPeriod;
    FShedule : TShedule;
  public
    property Name : String read FName write FName;
    property Entity : TEntity read FEntity write FEntity;
    property Period : TPeriod read FPeriod write FPeriod;
    property Shedule : TShedule read FShedule write FShedule;
  end;

implementation

{ TCommonObject }

constructor TJob.Create (AID : Int64);
begin
  inherited Create (AID);
  FName := '';
  FEntity := TEntity.Create(-1);
  FPeriod := TPeriod.Create(-1);
  FShedule := TShedule.Create(-1);
end;

destructor TJob.Destroy;
begin
  FreeAndNil(FEntity);
  FreeAndNil(FPeriod);
  FreeAndNil(FShedule);
  inherited Destroy;
end;

function TJob.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;
  
  Schema
    .Id
    .Text('name').NotNull
    .Integer('entity_id').NotNull
    .Integer('period_id')
    .Integer('shedule_id');

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema);  

  FreeAndNil(Schema);
end;

function TJob.Table : String;
begin
  Result := JOB_TABLE_NAME;
end;

function TJob.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
begin
  if ID = -1 then
    Exit(False);

  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  FName := row.Row.GetStringValue('name');
  Result := FEntity.Reload(row.Row.GetIntegerValue('entity_id')) and
    FPeriod.Reload(row.Row.GetIntegerValue('period_id')) and
    FShedule.Reload(row.Row.GetIntegerValue('shedule_id'));
end;

function TJob.Save : Boolean;
begin
  if not FEntity.Save then
    Exit(False);

  if not FPeriod.Save then
    Exit(False);

  if not FShedule.Save then
    Exit(False);

  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('name', FName)
      .Update('entity_id', FEntity.Id).Update('period_id', FPeriod.ID)
      .Update('shedule_id', FShedule.ID).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('name', FName)
      .Value('entity_id', FEntity.ID).Value('period_id', FPeriod.ID)
      .Value('shedule_id', FShedule.ID).Get > 0);
    UpdateObjectID;
  end;
end;

function TJob.Delete : Boolean;
begin
  if ID <> -1 then
    Result := FEntity.Delete and FPeriod.Delete and FShedule.Delete and
      (DeleteRow.Get > 0)
  else
    Result := False;
end;

end.
