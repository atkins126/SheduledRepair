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
unit renderers.virtualtreeview.mainmenu;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Graphics, Types, VirtualTrees;

type
  generic TVirtualTreeViewRenderer<T> = class
  public
    constructor Create (ATreeView : TVirtualDrawTree);
    destructor Destroy; override;

    procedure Append (AData : T);

    procedure Draw (ACanvas : TCanvas; ARect : TRect; AData : T); virtual;
      abstract;
  private
    type
      PItem = ^TItem;
      TItem = record
        Data : T;
      end;
  private
    FTreeView : TVirtualDrawTree;

    procedure NodeDataSizeEvent (ASender: TBaseVirtualTree; var ANodeDataSize:
      Integer);
    procedure NodeInit (ASender: TBaseVirtualTree; AParentNode, ANode:
      PVirtualNode; var AInitialStates: TVirtualNodeInitStates);
    procedure NodeDraw (ASender: TBaseVirtualTree; const APaintInfo:
      TVTPaintInfo);
    procedure NodeMeasure (ASender: TBaseVirtualTree; ATargetCanvas: TCanvas;
      ANode: PVirtualNode; var ANodeHeight: Integer);
    procedure NodeWidth (ASender: TBaseVirtualTree; AHintCanvas: TCanvas; ANode:
      PVirtualNode; AColumn: TColumnIndex; var ANodeWidth: Integer);
  end;

implementation

{ TVirtualTreeViewRenderer }

constructor TVirtualTreeViewRenderer.Create (ATreeView : 
  TVirtualDrawTree);
var
  Column : TVirtualTreeColumn;
begin
  FTreeView := ATreeView;
  FTreeView.NodeDataSize := Sizeof(TItem);
  FTreeView.TreeOptions.PaintOptions:= [toShowRoot, toHideFocusRect,
    toAlwaysHideSelection, toHideSelection, toUseBlendedImages];
  FTreeView.TreeOptions.MiscOptions := [toFullRepaintOnResize,
    toVariableNodeHeight, toWheelPanning];
  FTreeView.TreeOptions.SelectionOptions:= [toFullRowSelect];

  with FTreeView do
  begin
    OnMeasureItem := @NodeMeasure;
    OnDrawNode := @NodeDraw;
    OnGetNodeWidth := @NodeWidth;

    //Header.Columns.Clear;
    //Column := Header.Columns.Add;

    //with Column do
    begin
      //AutoSize := False;
      //Style := vsOwnerDraw;
    end;
  end;
end;

destructor TVirtualTreeViewRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TVirtualTreeViewRenderer.Append (AData : T);
var
  Item : PItem;
  Node : PVirtualNode;
begin
  FTreeView.BeginUpdate;
  Node := FTreeView.AddChild(nil);
  Item := PItem(FTreeView.GetNodeData(Node));
  if Assigned(Item) then
    Item^.Data := AData;
  FTreeView.EndUpdate;
end;

procedure TVirtualTreeViewRenderer.NodeDataSizeEvent (ASender :
  TBaseVirtualTree; var ANodeDataSize : Integer);
begin
  ANodeDataSize := Sizeof(TItem);
end;

procedure TVirtualTreeViewRenderer.NodeInit (ASender : TBaseVirtualTree;
  AParentNode, ANode : PVirtualNode; var AInitialStates :
  TVirtualNodeInitStates);
begin

end;

procedure TVirtualTreeViewRenderer.NodeMeasure (ASender: TBaseVirtualTree;
  ATargetCanvas: TCanvas; ANode: PVirtualNode; var ANodeHeight: Integer);
begin
  ANodeHeight := 60;
end;

procedure TVirtualTreeViewRenderer.NodeDraw (ASender : TBaseVirtualTree;
  const APaintInfo : TVTPaintInfo);
var
  Item : PItem;
begin
  Item := PItem(ASender.GetNodeData(APaintInfo.Node));
  if Assigned(Item) then
  begin
    Draw(APaintInfo.Canvas, APaintInfo.CellRect, Item^.Data);
  end;
end;

procedure TVirtualTreeViewRenderer.NodeWidth (ASender : TBaseVirtualTree;
  AHintCanvas : TCanvas; ANode : PVirtualNode; AColumn : TColumnIndex;
  var ANodeWidth : Integer);
begin
  ANodeWidth := ASender.Width;
end;

end.
