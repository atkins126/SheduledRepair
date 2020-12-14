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
  objects.mainmenu.item, eventproviders.common;

type
  TDataRenderer = class
  public
    constructor Create (ATreeView : TVirtualDrawTree; ADataProvider : 
      TCommonDataProvider; AProfileProvider : TCommonProfilesProvider;
      ARenderer : TCommonRenderer; AEventProvider : TCommonEventProvider);
    destructor Destroy; override;

    { Reload renderer data. }
    procedure ReloadData;

    { Get selected object. }
    function GetSelectedObject : TCommonObject;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  protected
    FTreeView : TVirtualDrawTree;
    FDataProvider : TCommonDataProvider;
    FProfileProvider : TCommonProfilesProvider;
    FRenderer : TCommonRenderer;
    FEventProvider : TCommonEventProvider;

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

    { Fire node event. }
    procedure FireNodeEvent (AEventID : Integer; ANode : PVirtualNode);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { On change node event. }
    procedure NodeChangeEvent (Sender: TBaseVirtualTree; Node: PVirtualNode);

    { Node OnClick event. }
    procedure NodeClickEvent (Sender : TBaseVirtualTree; const HitInfo : 
      THitInfo);

    { Node OnDblClick event. }
    procedure NodeDoubleClickEvent (Sender : TBaseVirtualTree; const HitInfo : 
      THitInfo);

    { On node draw event. }
    procedure NodeDrawEvent ({%H-}ASender : TBaseVirtualTree; const APaintInfo :
      TVTPaintInfo);

    { On VirtualTree resize event. }
    procedure TreeResizeEvent ({%H-}ASender : TObject);
  end;

  { Main menu data renderer decorator. }
  TMainMenuDataRenderer = class
  public
    constructor Create (ADataRenderer : TDataRenderer);

    { Reload renderer data. }
    procedure ReloadData;

    { Update dynamic menu items. }
    procedure UpdateDynamicMenu;
  protected
    FDataRenderer : TDataRenderer;
    FCurrentSelectedNode : PVirtualNode;
    FOpenDynamicMenuNode : PVirtualNode;
  private
    { Get menu item type. }
    function GetItemType (ANode : PVirtualNode) : TMainMenuItem.TItemType;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return true if menu item can be selected. }
    function CanSelected (ANode : PVirtualNode) : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Fire node event. }
    procedure FireNodeEvent (AEventID : Integer; ANode : PVirtualNode);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Append menu item dynamic menu items. }
    procedure CreateDynamicMenu (ANode : PVirtualNode);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Node OnChage event. }
    procedure NodeChangeEvent(Sender: TBaseVirtualTree; Node: PVirtualNode);

    { Node OnClick event. }
    procedure NodeClickEvent (Sender : TBaseVirtualTree; const HitInfo : 
      THitInfo);
  end;

implementation

uses
  mainmenuprovider;

{ TDataRenderer }

constructor TDataRenderer.Create (ATreeView : TVirtualDrawTree; ADataProvider : 
  TCommonDataProvider; AProfileProvider : TCommonProfilesProvider;
  ARenderer : TCommonRenderer; AEventProvider : TCommonEventProvider);
begin
  FTreeView := ATreeView;
  FDataProvider := ADataProvider;
  FProfileProvider := AProfileProvider;
  FRenderer := ARenderer;
  FEventProvider := AEventProvider;
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
    OnDrawNode := @NodeDrawEvent;
    OnResize := @TreeResizeEvent;
    OnChange := @NodeChangeEvent;
    OnNodeClick := @NodeClickEvent;
    OnNodeDblClick := @NodeDoubleClickEvent;
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
    FireNodeEvent(FEventProvider.EVENT_OBJECT_UNSELECT, FSelectedNode);

    FSelectedNode^.NodeHeight := GetDefaultProfile(FSelectedNode).Height;
    FTreeView.InvalidateNode(FSelectedNode);
  end;
end;

procedure TDataRenderer.FireNodeEvent (AEventID : Integer; ANode : 
  PVirtualNode);
begin
  if Assigned(FEventProvider) then
    FEventProvider.Fire(AEventID, GetObject(ANode));
end;

procedure TDataRenderer.NodeChangeEvent(Sender: TBaseVirtualTree; Node: 
  PVirtualNode);
begin
  if Node = nil then
    Exit;

  if IsSelectEnable(Node) then
  begin
    RestorePrevSelectedNodeHeight;
    Node^.NodeHeight := GetSelectedProfile(Node).Height;
    FSelectedNode := Node;

    FireNodeEvent(FEventProvider.EVENT_OBJECT_SELECT, Node);

    FTreeView.InvalidateNode(Node);
  end;
end;

procedure TDataRenderer.NodeClickEvent (Sender : TBaseVirtualTree; 
  const HitInfo : THitInfo);
begin
  FireNodeEvent(FEventProvider.EVENT_OBJECT_CLICK, HitInfo.HitNode);
end;

procedure TDataRenderer.NodeDoubleClickEvent (Sender : TBaseVirtualTree; 
  const HitInfo : THitInfo);
begin
  FireNodeEvent(FEventProvider.EVENT_OBJECT_DOUBLE_CLICK, HitInfo.HitNode);
end;

procedure TDataRenderer.NodeDrawEvent (ASender : TBaseVirtualTree; 
  const APaintInfo : TVTPaintInfo);
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

procedure TDataRenderer.TreeResizeEvent (ASender : TObject);
begin
  SetTreeViewColumns;
end;

procedure TDataRenderer.ReloadData;
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

function TDataRenderer.GetSelectedObject : TCommonObject;
begin
  if not Assigned(FSelectedNode) then
    Exit(nil);

  Result := GetObject(FSelectedNode);
end;

constructor TMainMenuDataRenderer.Create (ADataRenderer : TDataRenderer);
begin
  FDataRenderer := ADataRenderer;
  FDataRenderer.FTreeView.OnChange := @NodeChangeEvent;
  FDataRenderer.FTreeView.OnNodeClick := @NodeClickEvent;

  FCurrentSelectedNode := nil;
  FOpenDynamicMenuNode := nil;
end;

procedure TMainMenuDataRenderer.ReloadData;
begin
  FDataRenderer.ReloadData;
end;

function TMainMenuDataRenderer.GetItemType (ANode : PVirtualNode) : 
  TMainMenuItem.TItemType;
begin
  Result := TMainMenuItem(FDataRenderer.GetObject(ANode)).ItemType;
end;

function TMainMenuDataRenderer.CanSelected (ANode : PVirtualNode) : Boolean;
begin
  Result := TMainMenuItem(FDataRenderer.GetObject(ANode)).CanSelected;
end;

procedure TMainMenuDataRenderer.NodeChangeEvent (Sender : TBaseVirtualTree; 
  Node : PVirtualNode);
begin
  if Node = nil then
    Exit;
  
  if CanSelected(Node) then
  begin
    if (Assigned(FCurrentSelectedNode)) and (FCurrentSelectedNode <> Node) then
    begin
      FireNodeEvent(FDataRenderer.FEventProvider.EVENT_OBJECT_UNSELECT, 
        FCurrentSelectedNode);
    end;

    FCurrentSelectedNode := Node;

    FireNodeEvent(FDataRenderer.FEventProvider.EVENT_OBJECT_SELECT, Node);
    Exit;
  end;

  if Assigned(FCurrentSelectedNode) then
    FDataRenderer.FTreeView.Selected[FCurrentSelectedNode] := True;
end;

procedure TMainMenuDataRenderer.NodeClickEvent (Sender : TBaseVirtualTree;
  const HitInfo : THitInfo);
begin
  FireNodeEvent(FDataRenderer.FEventProvider.EVENT_OBJECT_CLICK, 
    HitInfo.HitNode);
  
  if GetItemType(HitInfo.HitNode) = MENU_ITEM_TYPE_ITEM then
  begin
    if Assigned(FOpenDynamicMenuNode) then
    begin
      FDataRenderer.FTreeView.DeleteChildren(FOpenDynamicMenuNode, True);
      FireNodeEvent(FDataRenderer.FEventProvider
        .EVENT_OBJECT_DETACH_DYNAMIC_MENU, HitInfo.HitNode);
    end;

    FOpenDynamicMenuNode := HitInfo.HitNode;

    FireNodeEvent(FDataRenderer.FEventProvider.EVENT_OBJECT_ATTACH_DYNAMIC_MENU,
      HitInfo.HitNode);
    CreateDynamicMenu(HitInfo.HitNode);  
  end;
end;

procedure TMainMenuDataRenderer.FireNodeEvent (AEventID : Integer; ANode :
  PVirtualNode);
begin
  TMainMenuItem(FDataRenderer.GetObject(ANode)).Fire(AEventID,
    TMainMenuItem(FDataRenderer.GetObject(ANode)));
end;

procedure TMainMenuDataRenderer.CreateDynamicMenu (ANode : PVirtualNode);
var
  DynamicMenu : TMainMenu.TIterator;
  DynamicMenuItem : TCommonObject;
  DynamicItemNode : PVirtualNode;
  DynamicNodeData : TDataRenderer.PNodeData;
  Index : Integer;
begin
  for DynamicMenu in MainMenu.GetAttachedMenus(
    FDataRenderer.GetObject(ANode).ID) do
  begin
    Index := 0;
    for DynamicMenuItem in DynamicMenu.DataProvider do
    begin
      DynamicItemNode := FDataRenderer.FTreeView.AddChild(ANode);
      DynamicNodeData := TDataRenderer.PNodeData(FDataRenderer.FTreeView
        .GetNodeData(DynamicItemNode));

      if Assigned(DynamicNodeData) then
      begin
        DynamicNodeData^.CommonObject := DynamicMenuItem;
        DynamicNodeData^.Profile := DynamicMenu.ProfilesProvider
          .GetProfile(Index);
      end;
      DynamicItemNode^.NodeHeight := DynamicNodeData^.Profile.DefaultProfile
        .Height;

      Inc(Index);  
    end;
    FDataRenderer.FTreeView.Expanded[ANode] := True;
  end;
  FDataRenderer.FTreeView.Refresh;
end;

procedure TMainMenuDataRenderer.UpdateDynamicMenu;
begin
  if Assigned(FOpenDynamicMenuNode) then
  begin
    FDataRenderer.FTreeView.DeleteChildren(FOpenDynamicMenuNode, True);
    CreateDynamicMenu(FOpenDynamicMenuNode);
  end;
end;

end.
