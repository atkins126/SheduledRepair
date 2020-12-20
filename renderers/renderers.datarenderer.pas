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

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, VirtualTrees, objects.common, 
  objects.namedobject, dataproviders.common, profilesprovider.common, 
  renderers.common, renderer.profile.objectprofile, renderer.profile.profile, 
  eventproviders.common;

type
  TDataRenderer = class
  public
    type
      { Renderer item handle. }
      TItemHandle = type Pointer;

      { Renderer item create event. }
      TItemCreateEvent = procedure (AObject : TCommonObject; AItemHandle :
        TItemHandle) of object;
  public
    constructor Create (ATreeView : TVirtualDrawTree; ADataProvider : 
      TCommonDataProvider; AProfileProvider : TCommonProfilesProvider;
      ARenderer : TCommonRenderer; AEventProvider : TCommonEventProvider);
    destructor Destroy; override;

    { Reload renderer data. }
    procedure ReloadData (AItemCreateEvent : TItemCreateEvent = nil);

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
    function FireNodeEvent (AEventID : Integer; ANode : PVirtualNode) : Boolean;
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
    type
      { Renderer item destroy event. }
      TItemDestroyEvent = procedure (AObject : TCommonObject) of object;
  public
    constructor Create (ADataRenderer : TDataRenderer);

    { Reload renderer data. }
    procedure ReloadData (AItemCreateEvent :
      TDataRenderer.TItemCreateEvent = nil);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Create dynamic menu. }
    procedure AttachDynamicMenu (AItemHandle : TDataRenderer.TItemHandle;
      ADataProvider : TCommonDataProvider; AProfilesProvider : 
      TCommonProfilesProvider; AItemCreateEvent : 
      TDataRenderer.TItemCreateEvent = nil);

    { Remove dynamic menu. }
    procedure DetachAllDynamicMenus (AItemHandle : TDataRenderer.TItemHandle;
      AItemDestroyEvent : TItemDestroyEvent = nil);

    { Select menu item by handle. }
    procedure SelectMenuItem (AItemHandle : TDataRenderer.TItemHandle);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Attach object to menu item element. }
    procedure AttachObject (AItemHandle : TDataRenderer.TItemHandle;
      AObject : TNamedObject);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Detach menu item element object. }
    procedure DetachObject (AItemHandle : TDataRenderer.TItemHandle);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Get attached object. }
    function GetAttachedObject (AItemHandle : TDataRenderer.TItemHandle) :
      TNamedObject;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  protected
    FDataRenderer : TDataRenderer;
    FCurrentSelectedNode : PVirtualNode;
    FItemDestroyEvent : TItemDestroyEvent;
  private
    { Fire node event. }
    function FireNodeEvent (AEventID : Integer; ANode : PVirtualNode) : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Node OnChage event. }
    procedure NodeChangeEvent(Sender: TBaseVirtualTree; Node: PVirtualNode);

    { Node OnClick event. }
    procedure NodeClickEvent (Sender : TBaseVirtualTree; const HitInfo : 
      THitInfo);

    { Node OnFree event. }
    procedure NodeFreeEvent (Sender : TBaseVirtualTree; Node : PVirtualNode);
  end;

implementation

uses
  mainmenuprovider, objects.mainmenu.item;

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
    OnDrawNode := {$IFDEF FPC}@{$ENDIF}NodeDrawEvent;
    OnResize := {$IFDEF FPC}@{$ENDIF}TreeResizeEvent;
    OnChange := {$IFDEF FPC}@{$ENDIF}NodeChangeEvent;
    OnNodeClick := {$IFDEF FPC}@{$ENDIF}NodeClickEvent;
    OnNodeDblClick := {$IFDEF FPC}@{$ENDIF}NodeDoubleClickEvent;
  end;
end;

procedure TDataRenderer.SetTreeViewColumns;
var
  Width : Cardinal;
  Column : TVirtualTreeColumn;
begin
  //FTreeView.Header.Columns.Clear;
  //Column := FTreeView.Header.Columns.Add;
  //Column.Width := FTreeView.ClientWidth;
  //Column.Style := vsOwnerDraw;

  {
  FRenderer.CalculateColumns(FTreeView.ClientWidth);

  for Width in FRenderer do
  begin
    Column := FTreeView.Header.Columns.Add;
    Column.Width := Width;
    Column.Style := vsOwnerDraw;
  end;
  }
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
    FireNodeEvent(TCommonEventProvider.EVENT_OBJECT_UNSELECT, FSelectedNode);

    FSelectedNode^.NodeHeight := GetDefaultProfile(FSelectedNode).Height;
    FTreeView.InvalidateNode(FSelectedNode);
  end;
end;

function TDataRenderer.FireNodeEvent (AEventID : Integer; ANode : 
  PVirtualNode) : Boolean;
begin
  if Assigned(FEventProvider) then
    Exit(FEventProvider.Fire(AEventID, GetObject(ANode)));
  Result := False;
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

    FireNodeEvent(TCommonEventProvider.EVENT_OBJECT_SELECT, Node);

    FTreeView.InvalidateNode(Node);
  end;
end;

procedure TDataRenderer.NodeClickEvent (Sender : TBaseVirtualTree; 
  const HitInfo : THitInfo);
begin
  FireNodeEvent(TCommonEventProvider.EVENT_OBJECT_CLICK, HitInfo.HitNode);
end;

procedure TDataRenderer.NodeDoubleClickEvent (Sender : TBaseVirtualTree; 
  const HitInfo : THitInfo);
begin
  FireNodeEvent(TCommonEventProvider.EVENT_OBJECT_DOUBLE_CLICK, 
    HitInfo.HitNode);
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

procedure TDataRenderer.ReloadData (AItemCreateEvent : TItemCreateEvent);
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

    if Assigned(AItemCreateEvent) then
      AItemCreateEvent(ObjectItem, TItemHandle(Node));

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
  FDataRenderer.FTreeView.OnChange := {$IFDEF FPC}@{$ENDIF}NodeChangeEvent;
  FDataRenderer.FTreeView.OnNodeClick := {$IFDEF FPC}@{$ENDIF}NodeClickEvent;
  FDataRenderer.FTreeView.OnFreeNode := {$IFDEF FPC}@{$ENDIF}NodeFreeEvent;

  FCurrentSelectedNode := nil;
  FItemDestroyEvent := nil;
end;

procedure TMainMenuDataRenderer.ReloadData(AItemCreateEvent : 
  TDataRenderer.TItemCreateEvent);
begin
  FDataRenderer.ReloadData(AItemCreateEvent);
end;

procedure TMainMenuDataRenderer.NodeChangeEvent (Sender : TBaseVirtualTree; 
  Node : PVirtualNode);
begin
  if Node = nil then
    Exit;
  
  if FireNodeEvent(TCommonEventProvider.EVENT_OBJECT_SELECT, Node) then
  begin
    if (Assigned(FCurrentSelectedNode)) and (FCurrentSelectedNode <> Node) then
    begin
      FireNodeEvent(TCommonEventProvider.EVENT_OBJECT_UNSELECT, 
        FCurrentSelectedNode);
    end;

    FCurrentSelectedNode := Node;
    FDataRenderer.FTreeView.Selected[FCurrentSelectedNode] := True;
    Exit;
  end;
  
  if Assigned(FCurrentSelectedNode) then
  begin
    FDataRenderer.FTreeView.Selected[FCurrentSelectedNode] := True;
    Exit;
  end;
  
  FDataRenderer.FTreeView.Selected[Node] := False;
end;

procedure TMainMenuDataRenderer.NodeClickEvent (Sender : TBaseVirtualTree;
  const HitInfo : THitInfo);
begin
  if HitInfo.HitNode = nil then
    Exit;

  FireNodeEvent(TCommonEventProvider.EVENT_OBJECT_CLICK,
    HitInfo.HitNode);
  
  if (HitInfo.HitNode <> nil) and (FDataRenderer.GetObject(HitInfo.HitNode).ID
    <> -1) then
  begin
    FireNodeEvent(TCommonEventProvider.EVENT_OBJECT_DETACH_DYNAMIC_MENU, 
      HitInfo.HitNode);
    FireNodeEvent(TCommonEventProvider.EVENT_OBJECT_ATTACH_DYNAMIC_MENU,
      HitInfo.HitNode);
  end;
end;

procedure TMainMenuDataRenderer.NodeFreeEvent (Sender : TBaseVirtualTree; Node : 
  PVirtualNode);
begin
  if Assigned(FItemDestroyEvent) then
    FItemDestroyEvent(FDataRenderer.GetObject(Node));
end;

function TMainMenuDataRenderer.FireNodeEvent (AEventID : Integer; ANode :
  PVirtualNode) : Boolean;
begin
  Result := TMainMenuItem(FDataRenderer.GetObject(ANode)).Fire(AEventID,
    TMainMenuItem(FDataRenderer.GetObject(ANode)));
end;

procedure TMainMenuDataRenderer.AttachDynamicMenu (AItemHandle : TDataRenderer
  .TItemHandle; ADataProvider : TCommonDataProvider; AProfilesProvider :
  TCommonProfilesProvider; AItemCreateEvent : TDataRenderer.TItemCreateEvent);
var
  DynamicMenuItem : TCommonObject;
  DynamicItemNode : PVirtualNode;
  DynamicNodeData : TDataRenderer.PNodeData;
  Index : Integer;
begin
  if (not Assigned(AItemHandle)) or (not ADataProvider.Load) or 
     (not AProfilesProvider.Load) then
    Exit;

  Index := 0;
  for DynamicMenuItem in ADataProvider do
  begin
    DynamicItemNode := FDataRenderer.FTreeView.AddChild(PVirtualNode(
      AItemHandle));
    DynamicNodeData := TDataRenderer.PNodeData(FDataRenderer.FTreeView
      .GetNodeData(DynamicItemNode));
    
    if Assigned(DynamicNodeData) then
    begin
      DynamicNodeData^.CommonObject := DynamicMenuItem;
      DynamicNodeData^.Profile := AProfilesProvider.GetProfile(Index);

      DynamicItemNode^.NodeHeight := DynamicNodeData^.Profile.DefaultProfile
        .Height;
    end;

    if Assigned(AItemCreateEvent) then
      AItemCreateEvent(DynamicMenuItem, TDataRenderer.TItemHandle(
        DynamicItemNode));

    Inc(Index);
  end;
  FDataRenderer.FTreeView.Expanded[PVirtualNode(AItemHandle)] := True;
end;

procedure TMainMenuDataRenderer.DetachAllDynamicMenus (AItemHandle :
  TDataRenderer.TItemHandle; AItemDestroyEvent : TItemDestroyEvent);
begin
  if (not Assigned(AItemHandle)) then
    Exit;

  FItemDestroyEvent := AItemDestroyEvent;
  FDataRenderer.FTreeView.DeleteChildren(PVirtualNode(AItemHandle), True);
end;

procedure TMainMenuDataRenderer.SelectMenuItem (AItemHandle : TDataRenderer
  .TItemHandle);
begin
  if not Assigned(AItemHandle) then
    Exit;

  FDataRenderer.FTreeView.Selected[PVirtualNode(AItemHandle)] := True;
end;

procedure TMainMenuDataRenderer.AttachObject (AItemHandle :
  TDataRenderer.TItemHandle; AObject : TNamedObject);
begin
  if not Assigned(AItemHandle) then
    Exit;

  TMainMenuItem(FDataRenderer.GetObject(PVirtualNode(AItemHandle)))
    .AttachedObject := AObject;
  FDataRenderer.FTreeView.InvalidateNode(PVirtualNode(AItemHandle));
end;

procedure TMainMenuDataRenderer.DetachObject (AItemHandle :
  TDataRenderer.TItemHandle);
begin
  if not Assigned(AItemHandle) then
    Exit;

  TMainMenuItem(FDataRenderer.GetObject(PVirtualNode(AItemHandle)))
    .AttachedObject := nil;
  FDataRenderer.FTreeView.InvalidateNode(PVirtualNode(AItemHandle));
end;

function TMainMenuDataRenderer.GetAttachedObject (AItemHandle :
  TDataRenderer.TItemHandle) : TNamedObject;
begin
  if not Assigned(AItemHandle) then
    Exit;

  Result := TMainMenuItem(FDataRenderer.GetObject(PVirtualNode(AItemHandle)))
    .AttachedObject;
end;

end.
