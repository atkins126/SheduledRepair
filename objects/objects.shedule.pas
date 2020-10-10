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
unit objects.shedule;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row;

type
  TShedule = class(TCommonObject)
  private
    const
      SHEDULE_TABLE_NAME = 'shedule';
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
    FPrevDate : TDate;
    FNextDate : TDate;
  public
    property PrevDate : TDate read FPrevDate write FPrevDate;
    property NextDate : TDate read FPrevDate write FPrevDate;
  end;

implementation

{ TPeriod }

constructor TShedule.Create (AID : Int64);
begin
  inherited Create (AID);
  FPrevDate := Now;
  FNextDate := Now;
end;

destructor TShedule.Destroy;
begin
  inherited Destroy;
end;

function TShedule.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;
  
  Schema
    .Id
    .Text('prev_date')
    .Text('next_date');

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema);  

  FreeAndNil(Schema);
end;

function TShedule.Table : String;
begin
  Result := SHEDULE_TABLE_NAME;
end;

function TShedule.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
begin
  if ID = -1 then
    Exit(False);

  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  FPrevDate := StrToDateDef(row.Row.GetStringValue('prev_date'), Now);
  FNextDate := StrToDateDef(row.Row.GetStringValue('next_date'), Now);
  Result := True;
end;

function TShedule.Save : Boolean;
begin
  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('prev_date', DateToStr(FPrevDate))
      .Update('next_date', DateToStr(FNextDate)).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('prev_date', DateToStr(FPrevDate))
      .Value('next_date', DateToStr(FNextDate)).Get > 0);
    UpdateObjectID;
  end;
end;

function TShedule.Delete : Boolean;
begin
  if ID <> -1 then
    Result := (DeleteRow.Get > 0)
  else
    Result := False;
end;

end.
