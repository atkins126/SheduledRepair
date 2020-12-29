(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(* This is a software for creating schedules  for repair work, accounting and *)
(* monitoring  their  implementation, accounting for the  necessary materials *)
(* and spare parts.                                                           *)
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
unit objects.greasebundle;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, objects.grease, objects.quantity;

type
  TGreaseBundle = class(TCommonObject)
  private
    const
      GREASE_BUNDLE_TABLE_NAME = 'greasebundle';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Delete object from database. }
    function Delete : Boolean; override;

    { Object deep copy. }
    procedure Assign (AGreaseBundle : TGreaseBundle);
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
    FGrease : TGrease;
    FQuantity : TQuantity;
  public
    property Grease : TGrease read FGrease write FGrease;
    property Quantity : TQuantity read FQuantity write FQuantity;
  end;

implementation

{ TCommonObject }

constructor TGreaseBundle.Create (AID : Int64);
begin
  inherited Create (AID);
  FGrease := TGrease.Create(-1);
  FQuantity := TQuantity.Create(-1);
end;

destructor TGreaseBundle.Destroy;
begin
  FreeAndNil(FGrease);
  FreeAndNil(FQuantity);
  inherited Destroy;
end;

procedure TGreaseBundle.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Integer('grease_id').NotNull
    .Integer('quantity_id');
end;

function TGreaseBundle.CheckDepentSchemes : Boolean;
begin
  Result := FGrease.CheckSchema and FQuantity.CheckSchema;
end;

function TGreaseBundle.Table : String;
begin
  Result := GREASE_BUNDLE_TABLE_NAME;
end;

function TGreaseBundle.LoadDepentObjects : Boolean;
begin
  Result := FQuantity.Reload(GetIntegerProperty('quantity_id')) and
    FGrease.Reload(GetIntegerProperty('grease_id'));
end;

function TGreaseBundle.SaveDepentObjects : Boolean;
begin
  Result := FGrease.Save and FQuantity.Save;
end;

procedure TGreaseBundle.SaveCurrentObject;
begin
  SetIntegerProperty('quantity_id', FQuantity.ID);
  SetIntegerProperty('grease_id', FGrease.ID);
end;

function TGreaseBundle.Delete : Boolean;
begin
  Result := FQuantity.Delete and inherited Delete;
end;

procedure TGreaseBundle.Assign (AGreaseBundle : TGreaseBundle);
begin
  Grease := AGreaseBundle.Grease;
  Quantity.Assign(AGreaseBundle.Quantity);
end;

end.
