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
unit renderers.datarenderer;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, VirtualTrees, objects.common,
  dataproviders.common, profilesprovider.common, renderers.common,
  renderer.profile.objectprofile, renderer.profile.profile, 
  objects.mainmenu.item;

type
  TDataRenderer = class
  public
    constructor Create (ATreeView : TVirtualDrawTree; ADataProvider : 
      TCommonDataProvider; AProfileProvider : TCommonProfilesProvider;
      ARenderer : TCommonRenderer);
    destructor Destroy; override;

    { Update data. }
    procedure UpdateData;
  protected
    FTreeView : TVirtualDrawTree;
    FDataProvider : TCommonDataProvider;
    FProfileProvider : TCommonProfilesProvider;
    FRenderer : TCommonRenderer;

    FSelectedNode : PVirtualNode;
  private
    type
      PNodeData = ^TNodeData;
      TNodeData = record
        CommonObject : TCommonObject;
        Profile : TRendererObjectProfile;
      end;
  private
    { Setup VirtualTreeView parameters. }
    procedure SetTreeViewParams;
      {$IFNDEF DEBUG}inline;{$ENDIF}
      
    { Setup VirtualTreeView callbacks. }
    procedure SetTreeViewCallbacks;
      {$IFNDEF DEBUG}inline;{$ENDIF}
      
    { Setup VirtualTreeView columns. }
    procedure SetTreeViewColumns;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Get node renderer hover profile. }
    function GetHoverProfile (ANode : PVirtualNode) : TRendererProfile;
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    { Get node renderer selected profile. }
    function GetSelectedProfile (ANode : PVirtualNode) : TRendererProfile;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Get node renderer default profile. }
    function GetDefaultProfile (ANode : PVirtualNode) : TRendererProfile;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Get common object associated with node. }
    function GetObject (ANode : PVirtualNode) : TCommonObject;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return true if hover profile enable for node. }
    function IsHoverEnable (ANode : PVirtualNode) : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return true if selected profile enable for node. }
    function IsSelectEnable (ANode : PVirtualNode) : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Restore previous node height. }
    procedure RestorePrevSelectedNodeHeight;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    procedure NodeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

    { Node double click. }
    procedure NodeDoubleClick ({%H-}ASender : TObject);

    { Draw node. }
    procedure NodeDraw ({%H-}ASender : TBaseVirtualTree; const APaintInfo :
      TVTPaintInfo);

    { Resize VirtualTreeNode. }
    procedure TreeResize ({%H-}ASender : TObject);
  end;

  { Main menu data renderer decorator. }
  TMainMenuDataRenderer = class
  public
    constructor Create (ADataRenderer : TDataRenderer);

    { Full update data. }
    procedure UpdateData;

    procedure RedrawSelection;
  protected
    FDataRenderer : TDataRenderer;

    FSelectedNode : PVirtualNode;
  private
    function GetItemType (ANode : PVirtualNode) : TMainMenuItem.TItemType;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    procedure NodeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  end;

implementation

uses
  mainmenuprovider;

{ TDataRenderer }

constructor TDataRenderer.Create (ATreeView : TVirtualDrawTree; ADataProvider : 
  TCommonDataProvider; AProfileProvider : TCommonProfilesProvider;
  ARenderer : TCommonRenderer);
begin
  FTreeView := ATreeView;
  FDataProvider := ADataProvider;
  FProfileProvider := AProfileProvider;
  FRenderer := ARenderer;
  FSelectedNode := nil;

  SetTreeViewParams;
  SetTreeViewCallbacks;
  SetTreeViewColumns;
end;

destructor TDataRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TDataRenderer.SetTreeViewParams;
begin
  with FTreeView do
  begin
    NodeDataSize := SizeOf(TNodeData);
    TreeOptions.PaintOptions := [toShowRoot, toUseBlendedImages, toHotTrack, 
      toHideFocusRect, toAlwaysHideSelection, toHideSelection];
    TreeOptions.MiscOptions := [toFullRepaintOnResize, toVariableNodeHeight,
      toWheelPanning];
    TreeOptions.SelectionOptions := [toFullRowSelect];
  end;
end;

procedure TDataRenderer.SetTreeViewCallbacks;
begin
  with FTreeView do
  begin
    OnDrawNode := @NodeDraw;
    OnResize := @TreeResize;
    OnChange := @NodeChange;
    OnDblClick := @NodeDoubleClick;
  end;
end;

procedure TDataRenderer.SetTreeViewColumns;
var
  Width : Cardinal;
  Column : TVirtualTreeColumn;
begin
  FTreeView.Header.Columns.Clear;
  FRenderer.CalculateColumns(FTreeView.ClientWidth);

  for Width in FRenderer do
  begin
    Column := FTreeView.Header.Columns.Add;
    Column.Width := Width;
    Column.Style := vsOwnerDraw;
  end;
end;

function TDataRenderer.GetHoverProfile (ANode : PVirtualNode) : 
  TRendererProfile;
begin
  Result := PNodeData(FTreeView.GetNodeData(ANode))^.Profile.HoverProfile;
end;
      
function TDataRenderer.GetSelectedProfile (ANode : PVirtualNode) : 
  TRendererProfile;
begin
  Result := PNodeData(FTreeView.GetNodeData(ANode))^.Profile.SelectedProfile;
end;
    
function TDataRenderer.GetDefaultProfile (ANode : PVirtualNode) : 
  TRendererProfile;
begin
  Result := PNodeData(FTreeView.GetNodeData(ANode))^.Profile.DefaultProfile;
end;

function TDataRenderer.GetObject (ANode : PVirtualNode) : TCommonObject;
begin
  Result := PNodeData(FTreeView.GetNodeData(ANode))^.CommonObject;
end;

function TDataRenderer.IsHoverEnable (ANode : PVirtualNode) : Boolean;
begin
  Result := (FTreeView.HotNode = ANode) and (GetHoverProfile(ANode).Enable);
end;

function TDataRenderer.IsSelectEnable (ANode : PVirtualNode) : Boolean;
begin
  Result := (FTreeView.Selected[ANode]) and (GetSelectedProfile(ANode).Enable);
end;

procedure TDataRenderer.RestorePrevSelectedNodeHeight;
begin
  if Assigned(FSelectedNode) and (not FTreeView.Selected[FSelectedNode]) then
  begin
    FSelectedNode^.NodeHeight := GetDefaultProfile(FSelectedNode).Height;
    FTreeView.InvalidateNode(FSelectedNode);
  end;
end;

procedure TDataRenderer.NodeChange(Sender: TBaseVirtualTree; Node: 
  PVirtualNode);
begin
  if Node = nil then
    Exit;

  if IsSelectEnable(Node) then
  begin
    RestorePrevSelectedNodeHeight;
    Node^.NodeHeight := GetSelectedProfile(Node).Height;
    FSelectedNode := Node;
    FTreeView.InvalidateNode(Node);
  end;
end;

procedure TDataRenderer.NodeDoubleClick (ASender : TObject);
begin
  if not Assigned(FSelectedNode) then
    Exit;

  FDataProvider.ObjectDoubleClick(FSelectedNode^.Index);
end;

procedure TDataRenderer.NodeDraw (ASender : TBaseVirtualTree; const APaintInfo :
  TVTPaintInfo);
var
  BackgroundColor : TColor;
begin
  if IsHoverEnable(APaintInfo.Node) then
  begin
    if (FTreeView.Selected[APaintInfo.Node]) then
    begin
      BackgroundColor := GetSelectedProfile(APaintInfo.Node).Background;
      GetSelectedProfile(APaintInfo.Node).Background :=
        GetHoverProfile(APaintInfo.Node).Background;

      FRenderer.Draw(GetObject(APaintInfo.Node), 
        GetSelectedProfile(APaintInfo.Node), APaintInfo.Canvas, 
        APaintInfo.CellRect);  

      GetSelectedProfile(APaintInfo.Node).Background := BackgroundColor;
      Exit;  
    end;

    FRenderer.Draw(GetObject(APaintInfo.Node), 
      GetHoverProfile(APaintInfo.Node), 
      APaintInfo.Canvas, APaintInfo.CellRect);  
    Exit;
  end;

  if IsSelectEnable(APaintInfo.Node) then
  begin
    FRenderer.Draw(GetObject(APaintInfo.Node),
      GetSelectedProfile(APaintInfo.Node),
      APaintInfo.Canvas, APaintInfo.CellRect);
    Exit;
  end;

  FRenderer.Draw(GetObject(APaintInfo.Node),
    GetDefaultProfile(APaintInfo.Node),
    APaintInfo.Canvas, APaintInfo.CellRect);
end;

procedure TDataRenderer.TreeResize (ASender : TObject);
begin
  SetTreeViewColumns;
end;

procedure TDataRenderer.UpdateData;
var
  ObjectItem : TCommonObject;
  NodeData : PNodeData;
  Node : PVirtualNode;
  Index : Integer;
begin
  if (not FDataProvider.Load) or (not FProfileProvider.Load) then
    Exit;

  FTreeView.Clear;
  FTreeView.BeginUpdate;

  Index := 0;
  for ObjectItem in FDataProvider do
  begin
    Node := FTreeView.AddChild(nil);
    NodeData := PNodeData(FTreeView.GetNodeData(Node));

    if Assigned(NodeData) then
    begin
      NodeData^.CommonObject := ObjectItem;
      NodeData^.Profile := FProfileProvider.GetProfile(Index);
    end;

    Node^.NodeHeight := NodeData^.Profile.DefaultProfile.Height;

    Inc(Index);
  end;

  FTreeView.EndUpdate;
end;

constructor TMainMenuDataRenderer.Create (ADataRenderer : TDataRenderer);
begin
  FDataRenderer := ADataRenderer;
  FDataRenderer.FTreeView.OnChange := @NodeChange;
end;

procedure TMainMenuDataRenderer.UpdateData;
begin
  FDataRenderer.UpdateData;
end;
{
procedure TMainMenuDataRenderer.UpdateDynamicData;
var
  Node, SubitemNode : PVirtualNode;
  SubitemNodeData : TDataRenderer.PNodeData;
  Item : TMainMenu.TIterator;
  Index : Integer;
begin
  FDataRenderer.FTreeView.BeginUpdate;

  Node := FDataRenderer.FTreeView.GetFirst;
  repeat
    Index := 0;
    for Item in MainMenu.ItemData(FDataRenderer.GetObject(Node).ID) do
    begin
      FDataRenderer.FTreeView.DeleteChildren(Node, True);

      SubitemNode := FDataRenderer.FTreeView.AddChild(Node);
      SubitemNodeData := TDataRenderer.PNodeData(
        FDataRenderer.FTreeView.GetNodeData(SubitemNode));

      if Assigned(SubitemNodeData) then
      begin
        SubitemNodeData^.CommonObject := Item.DataProvider.GetObject(Index);
        SubitemNodeData^.Profile := Item.ProfilesProvider.GetProfile(Index);
      end;
      SubitemNode^.NodeHeight := SubitemNodeData^.Profile.DefaultProfile.Height;

      Inc(Index);
    end;
    Node := FDataRenderer.FTreeView.GetNext(Node, False);
  until Node = FDataRenderer.FTreeView.GetLast;
  
  FDataRenderer.FTreeView.EndUpdate;
end;
}
function TMainMenuDataRenderer.GetItemType (ANode : PVirtualNode) : 
  TMainMenuItem.TItemType;
begin
  Result := TMainMenuItem(FDataRenderer.GetObject(ANode)).ItemType;
end;

procedure TMainMenuDataRenderer.NodeChange (Sender : TBaseVirtualTree; Node :
  PVirtualNode);
begin
  if Node = nil then
    Exit;

  if (GetItemType(Node) = MENU_ITEM_TYPE_LOGO) and (FSelectedNode <> nil) then
  begin
    FDataRenderer.FTreeView.Selected[FSelectedNode] := True;
    Exit;
  end;
  
  { Run menu item OnUnSelect event if asigned. }
  if (FSelectedNode <> nil) and (Assigned(TMainMenuItem(FDataRenderer.GetObject(
    FSelectedNode)).OnUnselect)) then
  begin
    TMainMenuItem(FDataRenderer.GetObject(FSelectedNode)).OnUnselect(
      TMainMenuItem(FDataRenderer.GetObject(FSelectedNode)));
  end;

  FSelectedNode := Node;

  { Run menu item OnSelect event if asigned. }
  if Assigned(TMainMenuItem(FDataRenderer.GetObject(Node)).OnSelect) then
  begin
    TMainMenuItem(FDataRenderer.GetObject(Node)).OnSelect(
      TMainMenuItem(FDataRenderer.GetObject(Node)));
  end;
end;

procedure TMainMenuDataRenderer.RedrawSelection;
begin
  FDataRenderer.FTreeView.InvalidateNode(FSelectedNode);
end;

end.
