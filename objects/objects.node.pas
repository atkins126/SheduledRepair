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

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.greasableobject, sqlite3.schema, objects.greasebag, 
  objects.period, objects.shedule;

type
  TNode = class(TGreasableObject)
  private
    const
      NODE_TABLE_NAME = 'node';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Load object from database. }
    function Load : Boolean; override;

    { Object deep copy. }
    procedure Assign (ANode : TNode);
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

    { Delete all dependent objects. }
    function DeleteDepentObjects : Boolean; override;
  protected
    FPeriod : TPeriod;
    FShedule : TShedule;
  public
    property Period : TPeriod read FPeriod write FPeriod;
    property Shedule : TShedule read FShedule write FShedule;
  end;

implementation

{ TPeriod }

constructor TNode.Create (AID : Int64);
begin
  inherited Create (AID);
  FPeriod := TPeriod.Create(-1);
  FShedule := TShedule.Create(-1);
end;

destructor TNode.Destroy;
begin
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

function TNode.LoadCurrentObject : Boolean;
begin
  Result := inherited LoadCurrentObject;

  FName := GetStringProperty('name');
end;

function TNode.LoadDepentObjects : Boolean;
begin
  Result := FPeriod.Reload(GetIntegerProperty('period_id')) and
    FShedule.Reload(GetIntegerProperty('shedule_id'));
end;

function TNode.Load : Boolean;
begin
  Result := inherited Load and FGreaseBag.Reload(-1);
end;

function TNode.SaveDepentObjects : Boolean;
begin
  Result := FPeriod.Save and FShedule.Save;
end;

procedure TNode.SaveCurrentObject;
begin
  SetStringProperty('name', FName);
  SetIntegerProperty('period_id', FPeriod.ID);
  SetIntegerProperty('shedule_id', FShedule.ID);
end;

function TNode.Save : Boolean;
begin
  Result := inherited Save and FGreaseBag.Save;
end;

function TNode.DeleteDepentObjects : Boolean;
begin
  Result := FPeriod.Delete and FShedule.Delete and FGreaseBag.Delete;
end;

procedure TNode.Assign (ANode : TNode);
begin
  FName := ANode.Name;
  FGreaseBag.Assign(ANode.GreaseBag);
  FPeriod.Assign(ANode.Period);
  FShedule.Assign(ANode.Shedule);
end;

end.
