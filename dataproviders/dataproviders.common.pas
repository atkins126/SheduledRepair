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
unit dataproviders.common;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, database, sqlite3.table, container.list, utils.functor,
  sqlite3.result, sqlite3.result_row, objects.common;

type
  generic TCommonDataProvider<T> = class
  public
    constructor Create;
    destructor Destroy; override;
    
    function Load : Boolean; virtual; abstract;
    function Count : Integer;

    function CreateObject : Boolean;
    function EditObject (AIndex : Integer) : Boolean;
    function RemoveObject (AIndex : Integer) : Boolean;
  public
    type
      TObjectCompareFunctor = class(specialize TUnsortableFunctor<T>);
      TObjectsList = class(specialize TList<T, TObjectCompareFunctor>);
  public
    function GetEnumerator : TObjectsList.TIterator;
  protected
    function LoadObjects (ATableName : String) : Boolean;
    function OpenEditor (AObject : T) : Boolean; virtual; 
      abstract;
  protected
    FObjectsList : TObjectsList;
  end;

implementation

{ TCommonDataProvider }

constructor TCommonDataProvider.Create;
begin
  FObjectsList := TObjectsList.Create;
end;

destructor TCommonDataProvider.Destroy;
begin
  FreeAndNil(FObjectsList);
  inherited Destroy;
end;

function TCommonDataProvider.GetEnumerator : TObjectsList.TIterator;
begin
  Result := FObjectsList.GetEnumerator;
end;

function TCommonDataProvider.LoadObjects (ATableName : String) : Boolean;
var
  Table : TSQLite3Table;
  ResultRows : TSQLite3Result;
  Row : TSQLite3ResultRow;
  ObjectItem : T;
begin
  FObjectsList.Clear;

  Table := TSQLite3Table.Create(DB.Errors, DB.Handle, ATableName);
  ResultRows := Table.Select.Field('id').Get;
  
  if not ResultRows.FirstRow.HasRow then
    Exit(False);

  for Row in ResultRows do
  begin
    ObjectItem := T.Create(Row.GetIntegerValue('id'));
    if not ObjectItem.Load then
      continue;

    FObjectsList.Append(ObjectItem);
  end;

  Result := True;  
end;

function TCommonDataProvider.Count : Integer;
begin
  Result := FObjectsList.Length;
end;

function TCommonDataProvider.CreateObject : Boolean;
var
  ObjectItem : T;
begin
  ObjectItem := T.Create(-1);

  if not OpenEditor(ObjectItem) then
    Exit(False);
  
  if ObjectItem.Save then
  begin
    FObjectsList.Append(ObjectItem);
    Exit(True);
  end;

  Result := False;
end;

function TCommonDataProvider.EditObject (AIndex : Integer) : Boolean;
var
  ObjectItem : TObjectsList.TIterator;
begin
  ObjectItem := FObjectsList.NthEntry(AIndex);

  if (not ObjectItem.HasValue) and (not OpenEditor(ObjectItem.Value)) then
    Exit(False);

  Result := ObjectItem.Value.Save;
end;

function TCommonDataProvider.RemoveObject (AIndex : Integer) : Boolean;
var
  ObjectItem : TObjectsList.TIterator;
begin
  ObjectItem := FObjectsList.NthEntry(AIndex);

  if not ObjectItem.HasValue then
    Exit(False);

  if ObjectItem.Value.Delete then
  begin
    ObjectItem.Remove;
    Exit(True);
  end;

  Result := False;
end;

end.
