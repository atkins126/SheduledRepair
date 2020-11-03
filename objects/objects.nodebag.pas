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
unit objects.nodebag;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  container.arraylist, utils.functor, objects.node;

type
  TNodeBag = class(TCommonObject)
  private
    const
      NODE_BAG_TABLE_NAME = 'nodebag';
  public
    constructor Create (AID : Int64; AObject : TCommonObject);
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Add new grease bundle to current bag. }
    procedure Append (ANode : TNode);

    { Remove grease bundle from current bag. }
    procedure Remove (ANode : TNode);

    { Object deep copy. }
    procedure Assign (ANodeBag : TNodeBag);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; override;

    { Save all grease bundles associated with current object. }
    function SaveNodes : Boolean;

    { Load all grease bundles associated with current object. }
    function LoadNodes : Boolean;

    { Delete current object from database. }
    function DeleteCurrentObject : Boolean; override;

    { Delete all grease bundles associated with current object. }
    function DeleteNodes : Boolean;

    { Delete all dependent objects. }
    function DeleteDepentObjects : Boolean; override;
  public
    type
      TNodeCompareFunctor = class (specialize TUnsortableFunctor<TNode>);
      
      TNodesList = class
        (specialize TArrayList<TNode, TNodeCompareFunctor>);  
  public
    { Get enumerator for in operator. }
    function GetEnumerator : TNodesList.TIterator;
  private
    FObject : TCommonObject;
    FNodesList : TNodesList;
  end;

implementation

{ TNodeBag }

constructor TNodeBag.Create (AID : Int64; AObject : TCommonObject);
begin
  inherited Create (AID);
  FObject := AObject;
  FNodesList := TNodesList.Create;
end;

destructor TNodeBag.Destroy;
begin
  FreeAndNil(FNodesList);
  inherited Destroy;
end;

procedure TNodeBag.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Integer('node_id').NotNull
    .Text('object_name').NotNull
    .Integer('object_id').NotNull;
end;

function TNodeBag.CheckDepentSchemes : Boolean;
var
  Node : TNode;
begin
  Node := TNode.Create(-1);
  Result := Node.CheckSchema;
  FreeAndNil(Node);
end;

function TNodeBag.Table : String;
begin
  Result := NODE_BAG_TABLE_NAME;
end;

function TNodeBag.LoadCurrentObject : Boolean;
begin
  Result := True;
end;

function TNodeBag.LoadDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := LoadNodes;
end;

function TNodeBag.SaveDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := SaveNodes;
end;

function TNodeBag.SaveNodes : Boolean;
var
  Node : TNode;
begin
  if not FNodesList.FirstEntry.HasValue then
    Exit(True);

  Result := True;
  for Node in FNodesList do
  begin
    if not Node.Save then
      Continue;

    { Check if current node stored in database. }
    if FTable.Update
      .Update('node_id', Node.ID)
      .Where('node_id', Node.ID)
      .Where('object_name', FObject.Table)
      .Where('object_id', FObject.ID)
      .Get > 0 then
      Continue;

    { Save current node in database. }
    Result := Result and (FTable.Insert
      .Value('node_id', Node.ID)
      .Value('object_name', FObject.Table)
      .Value('object_id', FObject.ID)
      .Get > 0);
  end;
end;

function TNodeBag.LoadNodes : Boolean;
var
  ResultRows : TSQLite3Result;
  Row : TSQLite3ResultRow;
  Node : TNode;
begin
  ResultRows := FTable.Select.All
    .Where('object_name', FObject.Table)
    .Where('object_id', FObject.ID)
    .Get;

  for Row in ResultRows do
  begin
    Node := TNode.Create(Row.GetIntegerValue('node_id'));

    if Node.Load then
      FNodesList.Append(Node);
  end;

  Result := True;
end;

function TNodeBag.DeleteCurrentObject : Boolean;
begin
  Result := True;
end;

function TNodeBag.DeleteNodes : Boolean;
var
  Node : TNode;
begin
  if not FNodesList.FirstEntry.HasValue then
    Exit(True);

  for Node in FNodesList do
  begin
    Node.Delete;
  end;

  FTable.Delete
    .Where('object_name', FObject.Table)
    .Where('object_id', FObject.ID)
    .Get;

  FNodesList.Clear;
  Result := True;
end;

function TNodeBag.DeleteDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := DeleteNodes;
end;

procedure TNodeBag.Append (ANode : TNode);
begin
  FNodesList.Append(ANode);
end;

procedure TNodeBag.Remove (ANode : TNode);
var
  Index : Integer;
begin
  Index := FNodesList.IndexOf(ANode);

  if Index <> -1 then
    FNodesList.Remove(Index);
end;

function TNodeBag.GetEnumerator : TNodesList.TIterator;
begin
  Result := FNodesList.GetEnumerator;
end;

procedure TNodeBag.Assign (ANodeBag : TNodeBag);
var
  node_item, node : TNode;
begin
  if not ANodeBag.FNodesList.FirstEntry.HasValue then
    Exit;

  for node_item in ANodeBag.FNodesList do
  begin
    node := TNode.Create(-1);
    node.Assign(node_item);
    FNodesList.Append(node);
  end;
end;

end.
