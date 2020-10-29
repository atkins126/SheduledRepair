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
unit objects.supplier;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row;

type
  TSupplier = class(TCommonObject)
  private
    const
      SUPPLIER_TABLE_NAME = 'supplier';
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
    procedure Assign (ASupplier : TSupplier);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;
  protected
    FName : String;
  public
    property Name : String read FName write FName;
  end;

implementation

{ TSupplier }

constructor TSupplier.Create (AID : Int64);
begin
  inherited Create (AID);
  FName := '';
end;

destructor TSupplier.Destroy;
begin
  inherited Destroy;
end;

procedure TSupplier.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Text('name').NotNull;
end;

function TSupplier.Table : String;
begin
  Result := SUPPLIER_TABLE_NAME;
end;

function TSupplier.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
begin
  if ID = -1 then
    Exit(False);

  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  FName := row.Row.GetStringValue('name');
  Result := True;
end;

function TSupplier.Save : Boolean;
begin
  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('name', FName).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('name', FName).Get > 0);
    UpdateObjectID;
  end;
end;

function TSupplier.Delete : Boolean;
begin
  if ID <> -1 then
    Result := (DeleteRow.Get > 0)
  else
    Result := False;
end;

procedure TSupplier.Assign (ASupplier : TSupplier);
begin
  Name := ASupplier.Name;
end;

end.
