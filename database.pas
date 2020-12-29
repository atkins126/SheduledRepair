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
unit database;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpassqlite, sqlite3.database, sqlite3.errors_stack;

type
  { Config }
  TDatabase = class
  private
    const
      DATABASE_FILE_NAME = 'database.db';
  public
    constructor Create;
    destructor Destroy; override;

    { Get last inserted row id. }
    function GetLastInsertID : Int64;

    { Get database handle. }
    function Handle : ppsqlite3;

    { Get errors. }
    function Errors : PSQL3LiteErrorsStack;
  private
    FDatabase : TSQLite3Database;
  end;

var
  DB : TDatabase = nil;

implementation

{ TDatabase }

constructor TDatabase.Create;
begin
  if not Assigned(DB) then
  begin
    inherited Create;
    FDatabase := TSQLite3Database.Create(DATABASE_FILE_NAME,
      [TSQLite3Database.TConnectFlag.SQLITE_OPEN_CREATE,
       TSQLite3Database.TConnectFlag.SQLITE_OPEN_READWRITE]);
    DB := self;
  end else
    self := DB;
end;

destructor TDatabase.Destroy;
begin
  inherited Destroy;
end;

function TDatabase.GetLastInsertID : Int64;
begin
  Result := FDatabase.LastInsertID;
end;

function TDatabase.Handle : ppsqlite3;
begin
  Result := @FDatabase.Handle;
end;

function TDatabase.Errors : PSQL3LiteErrorsStack;
begin
  Result := @FDatabase.Errors;
end;

initialization
  DB := TDatabase.Create;
finalization
  FreeAndNil(DB);
end.
