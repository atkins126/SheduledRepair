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
unit objects.greasebundle;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  objects.grease, objects.quantity;

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

    { Load object from database. }
    function Load : Boolean; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Delete object from database. }
    function Delete : Boolean; override;

    { Object deep copy. }
    procedure Assign (AGreaseBundle : TGreaseBundle);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;
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

function TGreaseBundle.Load : Boolean;
begin
  if not LoadCurrentObject then
    Exit(False);

  FQuantity.Reload(GetIntegerProperty('quantity_id'));
  Result := FGrease.Reload(GetIntegerProperty('grease_id'));
end;

function TGreaseBundle.Save : Boolean;
begin
  if not FGrease.Save then
    Exit(False);

  if not FQuantity.Save then
    Exit(False);

  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('quantity_id', FQuantity.ID)
      .Update('grease_id', FGrease.ID).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('quantity_id', FQuantity.ID)
      .Value('grease_id', FGrease.ID).Get > 0);
    UpdateObjectID;
  end;
end;

function TGreaseBundle.Delete : Boolean;
begin
  Result := FGrease.Delete and FQuantity.Delete and DeleteCurrentObject;
end;

procedure TGreaseBundle.Assign (AGreaseBundle : TGreaseBundle);
begin
  Grease := AGreaseBundle.Grease;
  Quantity.Assign(AGreaseBundle.Quantity);
end;

end.
