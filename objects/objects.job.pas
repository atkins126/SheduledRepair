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
unit objects.job;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.namedobject, sqlite3.schema, objects.entity, objects.period,
  objects.shedule;

type
  TJob = class(TNamedObject)
  private
    const
      JOB_TABLE_NAME = 'job';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Delete object from database. }
    function Delete : Boolean; override;

    { Object deep copy. }
    procedure Assign (AJob : TJob);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;

    { Store current object to database. }
    procedure SaveCurrentObject; override;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; override;
  protected
    FEntity : TEntity;
    FPeriod : TPeriod;
    FShedule : TShedule;
  public
    property Entity : TEntity read FEntity write FEntity;
    property Period : TPeriod read FPeriod write FPeriod;
    property Shedule : TShedule read FShedule write FShedule;
  end;

implementation

{ TCommonObject }

constructor TJob.Create (AID : Int64);
begin
  inherited Create (AID);
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

procedure TJob.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Text('name').NotNull
    .Integer('entity_id').NotNull
    .Integer('period_id')
    .Integer('shedule_id');
end;

function TJob.CheckDepentSchemes : Boolean;
begin
  Result := FEntity.CheckSchema and FPeriod.CheckSchema and
    FShedule.CheckSchema;
end;

function TJob.Table : String;
begin
  Result := JOB_TABLE_NAME;
end;

function TJob.LoadCurrentObject : Boolean;
begin
  Result := inherited LoadCurrentObject;

  FName := GetStringProperty('name');
end;

function TJob.LoadDepentObjects : Boolean;
begin
  Result := FEntity.Reload(GetIntegerProperty('entity_id')) and
    FPeriod.Reload(GetIntegerProperty('period_id')) and
    FShedule.Reload(GetIntegerProperty('shedule_id'));
end;

function TJob.SaveDepentObjects : Boolean;
begin
  Result := FEntity.Save and FPeriod.Save and FShedule.Save;
end;

procedure TJob.SaveCurrentObject;
begin
  SetStringProperty('name', FName);
  SetIntegerProperty('entity_id', FEntity.ID);
  SetIntegerProperty('period_id', FPeriod.ID);
  SetIntegerProperty('shedule_id', FShedule.ID);
end;

function TJob.Delete : Boolean;
begin
  Result := FPeriod.Delete and FShedule.Delete and inherited Delete;
end;

procedure TJob.Assign (AJob : TJob);
begin
  FName := AJob.Name;
  FEntity.Assign(AJob.Entity);
  FPeriod.Assign(AJob.Period);
  FShedule.Assign(AJob.Shedule);
end;

end.
