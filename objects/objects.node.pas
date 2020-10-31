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
    
    { Get object database table name. }
    function Table : String; override;

    { Load object from database. }
    function Load : Boolean; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Delete object from database. }
    function Delete : Boolean; override;

    { Object deep copy. }
    procedure Assign (ANode : TNode);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;
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
  FGreaseBag := TGreaseBag.Create(-1, Self);
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

procedure TNode.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Text('name').NotNull
    .Integer('period_id')
    .Integer('shedule_id');
end;

function TNode.CheckDepentSchemes : Boolean;
begin
  Result := FGreaseBag.CheckSchema and FPeriod.CheckSchema and
    FShedule.CheckSchema;
end;

function TNode.Table : String;
begin
  Result := NODE_TABLE_NAME;
end;

function TNode.Load : Boolean;
begin
  if not LoadCurrentObject then
    Exit(False);

  FName := GetStringProperty('name');
  Result := FGreaseBag.Reload(-1) and
    FPeriod.Reload(GetIntegerProperty('period_id')) and
    FShedule.Reload(GetIntegerProperty('shedule_id'));
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

  if not FGreaseBag.Save then
    Exit(False);

  Result := (updated_rows > 0);
end;

function TNode.Delete : Boolean;
begin
  if ID <> -1 then
  begin
    FGreaseBag.Delete;
    Result := FPeriod.Delete and FShedule.Delete and DeleteCurrentObject;
  end
  else
    Result := False;
end;

procedure TNode.Assign (ANode : TNode);
begin
  FName := ANode.Name;
  FGreaseBag.Assign(ANode.GreaseBag);
  FPeriod.Assign(ANode.Period);
  FShedule.Assign(ANode.Shedule);
end;

end.
