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
unit dataproviders.measure;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, dataproviders.common, objects.measure, sqlite3.table, 
  sqlite3.select, sqlite3.result, sqlite3.result_row, database;

type
  TMeasureDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
    function CreateObject : TCommonObject;
  end;

implementation

{ TMeasureDataProvider }

function TMeasureDataProvider.Load : Boolean;
var
  MeasureItem : TMeasure;
  Table : TSQLite3Table;
  ResultRows : TSQLite3Result;
  Row : TSQLite3ResultRow;
begin
  MeasureItem := TMeasure.Create(-1);

  if not MeasureItem.CheckSchema then
    Exit(False);

  Table := TSQLite3Table.Create(DB.Errors, DB.Handle, MeasureItem.Table);
  ResultRows := Table.Select.All;

  if not ResultRows.FirstRow.HasRow then
    Exit(False);

  FObjectsList.Clear;
  for Row in ResultRows do
  begin
    MeasureItem.Reload(Row.GetIntegerValue('id'));
    FObjectsList.Append(MeasureItem);
  end;

  FreeAndNil(ResultRows);
  FreeAndNil(Table);
  Result := True;  
end;

function TMeasureDataProvider.CreateObject : TCommonObject;
begin
  Result := TMeasure.Create(-1);
end;

end.
