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
unit objects.grade;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row;

type
  TGrade = class(TCommonObject)
  private
    const
      GRADE_TABLE_NAME = 'grade';
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
  public
    property Name : String read FName write FName;
  end;

implementation

{ TCommonObject }

constructor TGrade.Create (AID : Int64);
begin
  inherited Create (AID);
  FName := '';
end;

destructor TGrade.Destroy;
begin
  inherited Destroy;
end;

function TGrade.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;
  
  Schema
    .Id
    .Text('name').NotNull;

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema);  

  FreeAndNil(Schema);
end;

function TGrade.Table : String;
begin
  Result := GRADE_TABLE_NAME;
end;

function TGrade.Load : Boolean;
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

function TGrade.Save : Boolean;
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

function TGrade.Delete : Boolean;
begin
  if ID <> -1 then
    Result := (DeleteRow.Get > 0)
  else
    Result := False;
end;

end.
