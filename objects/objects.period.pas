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
    
    { Get object database table name. }
    function Table : String; override;

    { Delete object from database. }
    function Delete : Boolean; override;

    { Object deep copy. }
    procedure Assign (APeriod : TPeriod);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;

    { Store current object to database. }
    procedure SaveCurrentObject; override;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; override;
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

procedure TPeriod.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Integer('quantity_id');
end;

function TPeriod.CheckDepentSchemes : Boolean; 
begin
  Result := FQuantity.CheckSchema;
end;

function TPeriod.Table : String;
begin
  Result := PERIOD_TABLE_NAME;
end;

function TPeriod.LoadDepentObjects : Boolean;
begin
  Result := FQuantity.Reload(GetIntegerProperty('quantity_id'));
end;

function TPeriod.SaveDepentObjects : Boolean;
begin
  Result := FQuantity.Save;
end;

procedure TPeriod.SaveCurrentObject;
begin
  SetIntegerProperty('quantity_id', FQuantity.ID);
end;

function TPeriod.Delete : Boolean;
begin
  Result := FQuantity.Delete and inherited Delete;
end;

procedure TPeriod.Assign (APeriod : TPeriod);
begin
  Quantity.Assign(APeriod.Quantity);
end;

end.
