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
    
    { Get object database table name. }
    function Table : String; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Object deep copy. }
    procedure Assign (AGrade : TGrade);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;
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

procedure TGrade.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Text('name').NotNull;
end;

function TGrade.Table : String;
begin
  Result := GRADE_TABLE_NAME;
end;

function TGrade.LoadCurrentObject : Boolean;
begin
  Result := inherited LoadCurrentObject;

  FName := GetStringProperty('name');
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

procedure TGrade.Assign (AGrade : TGrade);
begin
  Name := AGrade.Name;
end;

end.
