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

    { Object deep copy. }
    procedure Assign (AGrease : TGrease);
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

function TGrease.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;
  
  Schema
    .Id
    .Integer('supplier_id').NotNull
    .Integer('grade_id').NotNull;

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema) and FSupplier.CheckSchema and 
    FGrade.CheckSchema;  

  FreeAndNil(Schema);
end;

function TGrease.Table : String;
begin
  Result := GREASE_TABLE_NAME;
end;

function TGrease.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
begin
  if ID = -1 then
    Exit(False);

  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  Result := FSupplier.Reload(row.Row.GetIntegerValue('supplier_id')) and
    FGrade.Reload(row.Row.GetIntegerValue('grade_id'));
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
  if ID <> -1 then
    Result := FSupplier.Delete and FGrade.Delete and (DeleteRow.Get > 0)
  else
    Result := False;
end;

procedure TGrease.Assign (AGrease : TGrease);
begin
  Supplier := AGrease.Supplier;
  Grade := AGrease.Grade;
end;

end.
