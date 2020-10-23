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
unit dataproviders.equipment;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, dataproviders.common, objects.equipment, sqlite3.table, 
  sqlite3.select, sqlite3.result, sqlite3.result_row, database;

type
  TEquipmentDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
    function CreateObject : TCommonObject;
  end;

implementation

{ TEquipmentDataProvider }

function TEquipmentDataProvider.Load : Boolean;
var
  EquipmentItem : TEquipment;
  Table : TSQLite3Table;
  ResultRows : TSQLite3Result;
  Row : TSQLite3ResultRow;
begin
  EquipmentItem := TMeasure.Create(-1);

  if not EquipmentItem.CheckSchema then
    Exit(False);

  Table := TSQLite3Table.Create(DB.Errors, DB.Handle, EquipmentItem.Table);
  ResultRows := Table.Select.All;

  if not ResultRows.FirstRow.HasRow then
    Exit(False);

  FObjectsList.Clear;
  for Row in ResultRows do
  begin
    EquipmentItem.Reload(Row.GetIntegerValue('id'));
    FObjectsList.Append(EquipmentItem);
  end;

  FreeAndNil(ResultRows);
  FreeAndNil(Table);
  Result := True;  
end;

function TEquipmentDataProvider.CreateObject : TCommonObject;
begin
  Result := TEquipment.Create(-1);
end;

end.