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
unit dataproviders.node;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, dataproviders.common, objects.common, objects.entity, 
  objects.nodebag, objects.node;

type
  TNodeDataProvider = class(TCommonDataProvider)
  public
    constructor Create (AEntity : TEntity); reintroduce;

    { Load objects. }
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  private
    FEntity : TEntity;
  end;

implementation

{ TNodeDataProvider }

constructor TNodeDataProvider.Create (AEntity : TEntity);
begin
  inherited Create;
  FEntity := AEntity;
end;

function TNodeDataProvider.Load : Boolean;
var
  Node : TNode;
begin
  if not Assigned(FEntity) then
    Exit(inherited Load);

  Clear;

  for Node in FEntity.NodeBag do
    Append(Node);

  Result := True;
end;

function TNodeDataProvider.LoadObjectsTableName : String;
var
  Node : TNode;
begin
  Node := TNode.Create(-1);
  Result := Node.Table;
  FreeAndNil(Node);
end;

function TNodeDataProvider.LoadConcreteObject (AID : Int64) : TCommonObject;
var
  Node : TNode;
begin
  Node := TNode.Create(AID);
  if not Node.Load then
  begin
    FreeAndNil(Node);
    Exit(nil);
  end;

  Result := Node;
end;

end.
