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
unit objects.grease;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  objects.supplier, objects.grade;

type
  TGrease = class(TCommonObject)
  private
    const
      GREASE_TABLE_NAME = 'grease';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Delete object from database. }
    function Delete : Boolean; override;

    { Object deep copy. }
    procedure Assign (AGrease : TGrease);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;
  protected
    FSupplier : TSupplier;
    FGrade : TGrade;
  public
    property Supplier : TSupplier read FSupplier write FSupplier;
    property Grade : TGrade read FGrade write FGrade;
  end;

implementation

{ TCommonObject }

constructor TGrease.Create (AID : Int64);
begin
  inherited Create (AID);
  FSupplier := TSupplier.Create(-1);
  FGrade := TGrade.Create(-1);
end;

destructor TGrease.Destroy;
begin
  FreeAndNil(FSupplier);
  FreeAndNil(FGrade);
  inherited Destroy;
end;

procedure TGrease.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Integer('supplier_id').NotNull
    .Integer('grade_id').NotNull;
end;

function TGrease.CheckDepentSchemes : Boolean;
begin
  Result := FSupplier.CheckSchema and FGrade.CheckSchema;
end;

function TGrease.Table : String;
begin
  Result := GREASE_TABLE_NAME;
end;

function TGrease.LoadDepentObjects : Boolean;
begin
  Result := FSupplier.Reload(GetIntegerProperty('supplier_id')) and
    FGrade.Reload(GetIntegerProperty('grade_id'));
end;

function TGrease.Save : Boolean;
begin
  if not FSupplier.Save then
    Exit(False);

  if not FGrade.Save then
    Exit(False);

  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('supplier_id', FSupplier.ID)
      .Update('grade_id', FGrade.ID).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('supplier_id', FSupplier.ID)
      .Value('grade_id', FGrade.ID).Get > 0);
    UpdateObjectID;
  end;
end;

function TGrease.Delete : Boolean;
begin
  Result := FSupplier.Delete and FGrade.Delete and DeleteCurrentObject;
end;

procedure TGrease.Assign (AGrease : TGrease);
begin
  Supplier := AGrease.Supplier;
  Grade := AGrease.Grade;
end;

end.
