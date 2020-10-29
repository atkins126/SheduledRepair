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

    { Load object from database. }
    function Load : Boolean; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Delete object from database. }
    function Delete : Boolean; override;

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
  public
    type
      TNodeCompareFunctor = class
        (specialize TBinaryFunctor<TNode, Integer>)
      public
        function Call (AValue1, AValue2 : TNode) : Integer; override;
      end;

      TNodeList = class
        (specialize TArrayList<TNode, TNodeCompareFunctor>);  
  public
    { Get enumerator for in operator. }
    function GetEnumerator : TNodeList.TIterator;
  private
    FObject : TCommonObject;
    FNodeList : TNodeList;
  end;

implementation

{ TGreaseBag.TGreaseBundleCompareFunctor }

function TNodeBag.TNodeCompareFunctor.Call (AValue1, AValue2 : TNode) : Integer;
begin
  if AValue1.ID < AValue2.ID then
    Result := -1
  else if AValue2.ID < AValue1.ID then
    Result := 1
  else
    Result := 0;
end;

{ TNodeBag }

constructor TNodeBag.Create (AID : Int64; AObject : TCommonObject);
begin
  inherited Create (AID);
  FObject := AObject;
  FNodeList := TNodeList.Create;
end;

destructor TNodeBag.Destroy;
begin
  FreeAndNil(FNodeList);
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

function TNodeBag.Load : Boolean;
var
  result_rows : TSQLite3Result;
  row : TSQLite3ResultRow;
  node : TNode;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  result_rows := FTable.Select.All.Where('object_id', FObject.ID)
    .Where('object_name', FObject.Table).Get;
  FNodeList.Clear;

  for row in result_rows do
  begin
    node := TNode.Create(row.GetIntegerValue('node_id'));

    if node.Load then
      FNodeList.Append(node);
  end;

  Result := True;
end;

function TNodeBag.Save : Boolean;
var
  node : TNode;
  updated_rows : Integer;
begin
  if (FObject = nil) then
    Exit(False);

  if FObject.ID = -1 then
    FObject.Save;

  if not FNodeList.FirstEntry.HasValue then
    Exit(False);

  for node in FNodeList do
  begin
    node.Save;
    
    updated_rows := UpdateRow.Update('node_id', node.ID)
      .Where('object_id', FObject.ID).Where('object_name', FObject.Table)
      .Where('node_id', node.ID).Get;

    if updated_rows > 0 then
      continue;
    
    InsertRow.Value('node_id', node.ID)
      .Value('object_id', FObject.ID).Value('object_name', FObject.Table).Get;
    UpdateObjectID;
  end;

  Result := True;
end;

function TNodeBag.Delete : Boolean;
var
  node : TNode;
begin
  if (FObject = nil) or (ID = -1) then
    Exit(False);

  for node in FNodeList do
  begin
    node.Delete;
  end;  

  Result := (DeleteRow.Get > 0);
end;

procedure TNodeBag.Append (ANode : TNode);
var
  updated_rows : Integer;
begin
  FNodeList.Append(ANode);

  if (FObject <> nil) and (FObject.ID <> -1) then
  begin
    if not ANode.Save then
      Exit;

    updated_rows := UpdateRow.Update('node_id', ANode.ID)
      .Where('object_id', FObject.ID).Where('object_name', FObject.Table)
      .Where('node_id', ANode.ID).Get;

    if updated_rows > 0 then
      Exit;

    InsertRow.Value('node_id', ANode.ID)
      .Value('object_id', FObject.ID).Value('object_name', FObject.Table).Get;
  end;
end;

procedure TNodeBag.Remove (ANode : TNode);
var
  Index : Integer;
begin
  Index := FNodeList.IndexOf(ANode);

  if Index <> -1 then
  begin
    FNodeList.Remove(Index);
    
    if (FObject <> nil) and (FObject.ID <> -1) then
    begin
      FTable.Delete.Where('node_id', ANode.ID)
        .Where('object_id', FObject.ID).Where('object_name', FObject.Table)
        .Get;
    end;
  end;
end;

function TNodeBag.GetEnumerator : TNodeList.TIterator;
begin
  Result := FNodeList.GetEnumerator;
end;

procedure TNodeBag.Assign (ANodeBag : TNodeBag);
var
  node_item, node : TNode;
begin
  if not ANodeBag.FNodeList.FirstEntry.HasValue then
    Exit;

  for node_item in ANodeBag.FNodeList do
  begin
    node := TNode.Create(-1);
    node.Assign(node_item);
    FNodeList.Append(node);
  end;
end;

end.
