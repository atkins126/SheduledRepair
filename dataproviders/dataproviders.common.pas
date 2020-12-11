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
unit dataproviders.common;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, database, sqlite3.table, container.arraylist, utils.functor,
  sqlite3.result, sqlite3.result_row, objects.common;

type
  TCommonDataProvider = class
  public
    type
      TObjectClickEvent = procedure (AObject : TCommonObject) of object;
      TObjectDoubleClickEvent = procedure (AObject : TCommonObject) of object;
  public
    constructor Create;
    destructor Destroy; override;
    
    { Load objects. }
    function Load : Boolean; virtual;

    { Clear all items. }
    procedure Clear;

    { Append new object to list. }
    procedure Append (AObject : TCommonObject);

    { Remove object from list by index. }
    procedure Remove (AObjectIndex : Cardinal);

    { Get object by index. }
    function GetObject (AObjectIndex : Cardinal) : TCommonObject;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; virtual; abstract;

    { Load concrete object. }
    function LoadConcreteObject (AID : Int64) : TCommonObject; virtual; 
      abstract;
  protected
    type
      TObjectsCompareFunctor = class
        (specialize TBinaryFunctor<TCommonObject, Integer>)
      public
        function Call (AValue1, AValue2 : TCommonObject) : Integer; override;
      end;

      TObjectsList = class
        (specialize TArrayList<TCommonObject, TObjectsCompareFunctor>);
  protected
    FObjectsList : TObjectsList;
    FObjectClick : TObjectClickEvent;
    FObjectDoubleClick : TObjectDoubleClickEvent;
  public
    { Get enumerator for in operator. }
    function GetEnumerator : TObjectsList.TIterator;

    property OnObjectClick : TObjectClickEvent read FObjectClick
      write FObjectClick;
    property OnObjectDoubleClick : TObjectDoubleClickEvent 
      read FObjectDoubleClick write FObjectDoubleClick;
  end;

implementation

{ TCommonDataProvider.TObjectsCompareFunctor }

function TCommonDataProvider.TObjectsCompareFunctor.Call (AValue1, AValue2 :
  TCommonObject) : Integer;
begin
  if AValue1.ID < AValue2.ID then
    Result := -1
  else if AValue2.ID < AValue1.ID then
    Result := 1
  else 
    Result := 0;
end;

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

procedure TCommonDataProvider.Clear;
begin
  FObjectsList.Clear;
end;

procedure TCommonDataProvider.Append (AObject : TCommonObject);
begin
  FObjectsList.Append(AObject);
end;

procedure TCommonDataProvider.Remove (AObjectIndex : Cardinal);
begin
  if AObjectIndex < FObjectsList.Length then
    FObjectsList.Remove(AObjectIndex);
end;

function TCommonDataProvider.GetObject (AObjectIndex : Cardinal) : 
  TCommonObject;
begin
  if AObjectIndex < FObjectsList.Length then
    Exit(FObjectsList.Value[AObjectIndex]);
  
  Result := nil;
end;

function TCommonDataProvider.Load : Boolean;
var
  Table : TSQLite3Table;
  ResultRows : TSQLite3Result;
  Row : TSQLite3ResultRow;
  Item : TCommonObject;
begin
  Clear;
  
  Table := TSQLite3Table.Create(DB.Errors, DB.Handle, LoadObjectsTableName);
  ResultRows := Table.Select.Field('id').Get;

  for Row in ResultRows do
  begin
    Item := LoadConcreteObject(Row.GetIntegerValue('id'));

    if Item = nil then
      Continue;
    
    FObjectsList.Append(Item);
  end;

  Result := True;
end;

function TCommonDataProvider.GetEnumerator : TObjectsList.TIterator;
begin
  Result := FObjectsList.GetEnumerator;
end;

end.
