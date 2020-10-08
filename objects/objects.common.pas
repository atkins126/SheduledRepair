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
unit objects.common;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, database, sqlite3.table;

type
  TCommonObject = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    
    { Check database table scheme. }
    function CheckSchema : Boolean; virtual; abstract;

    { Get object database table name. }
    function Table : String; virtual; abstract;

    { Load object from database. }
    function Load (AID : Int64) : Boolean; virtual; abstract;

    { Save object to database. }
    function Save : Boolean; virtual; abstract;
  protected
    FID : Int64;
    FTable : TSQLite3Table;  
  end;

implementation

{ TCommonObject }

constructor TCommonObject.Create;
begin
  FTable := TSQLite3Table.Create(DB.Errors, DB.Handle, Table);
end;

destructor TCommonObject.Destroy;
begin
  FreeAndNil(FTable);
  inherited Destroy;
end;

end.
