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
  SysUtils, database, sqlite3.table, container.arraylist, utils.functor,
  sqlite3.result, sqlite3.result_row, Classes, rules.chain, 
  renderer.objectprofile;

type
  generic TCommonDataProvider<T> = class
  public
    constructor Create (AEditorParent : TComponent);
    destructor Destroy; override;
    
    function Load : Boolean; virtual; abstract;
    function Count : Integer;

    function GetObject (AIndex : Integer) : T;
    function GetObjectProfile (AIndex : Integer) : TRendererObjectProfile;

    function CreateObject : Integer;
    function EditObject (AIndex : Integer) : Boolean;
    function RemoveObject (AIndex : Integer) : Boolean;
  public
    type
      TObjectCompareFunctor = class(specialize TBinaryFunctor<T, Integer>)
      public
        function Call (AValue1, AValue2 : T) : Integer; override;
      end;

      TObjectsList = class(specialize TArrayList<T, TObjectCompareFunctor>);
  public
    function GetEnumerator : TObjectsList.TIterator;
  protected
    function LoadObjects (ATableName : String) : Boolean;
    function OpenEditor (AObject : T) : Boolean; virtual; 
      abstract;
  protected
    FObjectsList : TObjectsList;
    FEditorParent : TComponent;
  end;

implementation

{ TCommonDataProvider.TObjectCompareFunctor }

function TCommonDataProvider.TObjectCompareFunctor.Call (AValue1, AValue2 : T) :
  Integer;
begin
  if AValue1.ID < AValue2.ID then
    Result := -1
  else if AValue2.ID < AValue1.ID then
    Result := 1
  else 
    Result := 0;
end;

{ TCommonDataProvider }

constructor TCommonDataProvider.Create (AEditorParent : TComponent);
begin
  FObjectsList := TObjectsList.Create;
  FEditorParent := AEditorParent;
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

    ObjectItem.Profile := TRendererObjectProfile(
      TRulesChain.CalculateProfile(@ObjectItem)
    );
    FObjectsList.Append(ObjectItem);
  end;

  Result := True;  
end;

function TCommonDataProvider.Count : Integer;
begin
  Result := FObjectsList.Length;
end;

function TCommonDataProvider.GetObject (AIndex : Integer) : T;
begin
  if (AIndex < 0) or (AIndex > FObjectsList.Length) then
    Exit(nil);

  Result := FObjectsList.Value[AIndex - 1];
end;

function TCommonDataProvider.GetObjectProfile (AIndex : Integer) :
  TRendererObjectProfile;
begin
  if (AIndex < 0) or (AIndex > FObjectsList.Length) then
    Exit(nil);

  Result := TRendererObjectProfile(GetObject(AIndex).Profile);
end;

function TCommonDataProvider.CreateObject : Integer;
var
  ObjectItem : T;
begin
  ObjectItem := T.Create(-1);

  if not OpenEditor(ObjectItem) then
    Exit(-1);
  
  if ObjectItem.Save then
  begin
    ObjectItem.Profile := TRendererObjectProfile(
      TRulesChain.CalculateProfile(@ObjectItem)
    );
    FObjectsList.Append(ObjectItem);
    Exit(FObjectsList.Length - 1);
  end;

  Result := -1;
end;

function TCommonDataProvider.EditObject (AIndex : Integer) : Boolean;
var
  item : T;
begin
  if (AIndex < 0) or (AIndex > FObjectsList.Length) then
    Exit(False);

  if not OpenEditor(FObjectsList.Value[AIndex - 1]) then
    Exit(False);

  item := FObjectsList.Value[AIndex - 1];
  Result := item.Save;
  item.Profile := TRendererObjectProfile(TRulesChain.CalculateProfile(@item));
end;

function TCommonDataProvider.RemoveObject (AIndex : Integer) : Boolean;
var
  item : T;
begin
  if (AIndex < 0) or (AIndex > FObjectsList.Length) then
    Exit(False);

  item := FObjectsList.Value[AIndex - 1];
  if item.Delete then
  begin
    FObjectsList.Remove(AIndex - 1);
    Exit(True);
  end;

  Result := False;
end;

end.
