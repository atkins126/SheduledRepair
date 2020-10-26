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
unit renderers.virtualtreeview;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Types, VirtualTrees;

type
  generic TVirtualTreeViewRenderer<T> = class
  public
    type
      TDrawOption = (
        ITEM_DRAW_FOCUS,
        ITEM_DRAW_BUTTONS,
        ITEM_DRAW_TREE_LINES
      );
      TDrawOptions = set of TDrawOption;
  public
    constructor Create (ATreeView : TVirtualDrawTree; ADrawOptions :
      TDrawOptions = [ITEM_DRAW_BUTTONS]);
    destructor Destroy; override;
  protected
    type
      TItemState = (
        ITEM_SELECTED,
        ITEM_HOVER
      );
      TItemStates = set of TItemState;
  protected
    procedure AppendColumn (AWidth : Cardinal);
    procedure AppendData (AItemType : Integer; AData : T);

    function ItemHeight (AIndex : Cardinal; AData : T) : Cardinal; virtual;
      abstract;
    procedure ItemDraw (AItemType : Integer; ACanvas : TCanvas; ACellRect :
      TRect; AContentRect : TRect; AState : TItemStates; AData : T); virtual;
      abstract;
    function ItemEditor (ANode : PVirtualNode; AColumn : TColumnIndex) :
      IVTEditLink; virtual;
  private
    type
      PItem = ^TItem;
      TItem = record
        Data : T;
        MenuItemType : Integer;
      end;
  private
    FTreeView : TVirtualDrawTree;

    procedure NodeInit (ASender: TBaseVirtualTree; AParentNode, ANode:
      PVirtualNode; var AInitialStates: TVirtualNodeInitStates);
    procedure NodeDraw (ASender: TBaseVirtualTree; const APaintInfo:
      TVTPaintInfo);
    procedure NodeMeasure (ASender: TBaseVirtualTree; ATargetCanvas: TCanvas;
      ANode: PVirtualNode; var ANodeHeight: Integer);
    procedure NodeWidth (ASender: TBaseVirtualTree; AHintCanvas: TCanvas; ANode:
      PVirtualNode; AColumn: TColumnIndex; var ANodeWidth: Integer);
    procedure NodeHotChange (ASender: TBaseVirtualTree; AOldNode, ANewNode:
      PVirtualNode);
    procedure NodeCreateEditor (ASender: TBaseVirtualTree; ANode: PVirtualNode;
      AColumn: TColumnIndex; out AEditLink: IVTEditLink);
    procedure NodeAllowEdit (ASender: TBaseVirtualTree; ANode: PVirtualNode;
      AColumn: TColumnIndex; var Allowed: Boolean);
  end;

implementation

{ TVirtualTreeViewRenderer }

constructor TVirtualTreeViewRenderer.Create (ATreeView : 
  TVirtualDrawTree; ADrawOptions : TDrawOptions);
begin
  FTreeView := ATreeView;
  FTreeView.NodeDataSize := Sizeof(TItem);
  FTreeView.TreeOptions.PaintOptions:= [toShowRoot, toUseBlendedImages,
    toHotTrack];
  FTreeView.TreeOptions.MiscOptions := [toFullRepaintOnResize,
    toVariableNodeHeight, toWheelPanning, toEditable];
  FTreeView.TreeOptions.SelectionOptions:= [toFullRowSelect];

  if ITEM_DRAW_BUTTONS in ADrawOptions then
  begin
    FTreeView.TreeOptions.PaintOptions := FTreeView.TreeOptions.PaintOptions +
      [toShowButtons];
    FTreeView.TreeOptions.AutoOptions:= [toAutoHideButtons];
  end;

  if ITEM_DRAW_TREE_LINES in ADrawOptions then
  begin
    FTreeView.TreeOptions.PaintOptions := FTreeView.TreeOptions.PaintOptions +
      [toShowTreeLines];
  end;

  if not (ITEM_DRAW_FOCUS in ADrawOptions) then
  begin
    FTreeView.TreeOptions.PaintOptions := FTreeView.TreeOptions.PaintOptions +
      [toHideFocusRect, toAlwaysHideSelection, toHideSelection];
  end;

  FTreeView.Header.Columns.Clear;

  with FTreeView do
  begin
    OnMeasureItem := @NodeMeasure;
    OnDrawNode := @NodeDraw;
    OnCreateEditor := @NodeCreateEditor;
    OnEditing := @NodeAllowEdit;
    //OnGetNodeWidth := @NodeWidth;
  end;
end;

destructor TVirtualTreeViewRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TVirtualTreeViewRenderer.AppendColumn (AWidth : Cardinal);
var
  Column : TVirtualTreeColumn;
begin
  Column := FTreeView.Header.Columns.Add;

  with Column do
  begin
    Width := AWidth;
    Style := vsOwnerDraw;
  end;
end;

procedure TVirtualTreeViewRenderer.AppendData (AItemType : Integer; AData : T);
var
  Item : PItem;
  Node : PVirtualNode;
begin
  FTreeView.BeginUpdate;
  Node := FTreeView.AddChild(nil);
  if Node^.Index = 0 then
    FTreeView.AddChild(Node);
  Item := PItem(FTreeView.GetNodeData(Node));
  if Assigned(Item) then
  begin
    Item^.Data := AData;
    Item^.MenuItemType := AItemType;
  end;
  FTreeView.EndUpdate;
end;

procedure TVirtualTreeViewRenderer.NodeInit (ASender : TBaseVirtualTree;
  AParentNode, ANode : PVirtualNode; var AInitialStates :
  TVirtualNodeInitStates);
begin

end;

procedure TVirtualTreeViewRenderer.NodeMeasure (ASender: TBaseVirtualTree;
  ATargetCanvas: TCanvas; ANode: PVirtualNode; var ANodeHeight: Integer);
begin
  ANodeHeight := ItemHeight(ANode^.Index,
    PItem(ASender.GetNodeData(ANode))^.Data);
end;

procedure TVirtualTreeViewRenderer.NodeDraw (ASender : TBaseVirtualTree;
  const APaintInfo : TVTPaintInfo);
var
  Item : PItem;
  State : TItemStates;
begin
  Item := PItem(ASender.GetNodeData(APaintInfo.Node));
  if Assigned(Item) then
  begin
    State := [];

    if ASender.Selected[APaintInfo.Node] then
    begin
      State := State + [ITEM_SELECTED];
    end;

    if APaintInfo.Node = ASender.HotNode then
    begin
      State := State + [ITEM_HOVER];
    end;

    ItemDraw(Item^.MenuItemType, APaintInfo.Canvas, APaintInfo.CellRect,
      APaintInfo.ContentRect, State, Item^.Data);
  end;
end;

procedure TVirtualTreeViewRenderer.NodeWidth (ASender : TBaseVirtualTree;
  AHintCanvas : TCanvas; ANode : PVirtualNode; AColumn : TColumnIndex;
  var ANodeWidth : Integer);
begin
  ANodeWidth := ASender.Width;
end;

procedure TVirtualTreeViewRenderer.NodeHotChange (ASender : TBaseVirtualTree;
  AOldNode, ANewNode : PVirtualNode);
begin
  FTreeView.InvalidateNode(AOldNode);
  FTreeView.InvalidateNode(ANewNode);
end;

procedure TVirtualTreeViewRenderer.NodeCreateEditor (ASender : TBaseVirtualTree;
  ANode : PVirtualNode; AColumn : TColumnIndex; out AEditLink : IVTEditLink);
begin
  AEditLink := ItemEditor(ANode, AColumn);
end;

procedure TVirtualTreeViewRenderer.NodeAllowEdit (ASender : TBaseVirtualTree;
  ANode : PVirtualNode; AColumn : TColumnIndex; var Allowed : Boolean);
begin
  Allowed := ItemEditor(ANode, AColumn) <> nil;
end;

function TVirtualTreeViewRenderer.ItemEditor (ANode : PVirtualNode; AColumn :
  TColumnIndex) : IVTEditLink;
begin
  Result := nil;
end;

end.
