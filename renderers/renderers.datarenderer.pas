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
  renderer.profile.profile, objects.mainmenu.item;

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

    { Return true if hover profile enable for node. }
    function IsHoverEnable (ANode : PVirtualNode) : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return true if selected profile enable for node. }
    function IsSelectEnable (ANode : PVirtualNode) : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}






    procedure NodeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

    { Get ANode height. }
    procedure NodeMeasure ({%H-}ASender : TBaseVirtualTree; {%H-}ATargetCanvas : 
      TCanvas; ANode : PVirtualNode; var ANodeHeight : Integer);

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

    { Update data. }
    procedure UpdateData;

    procedure RedrawSelection;
  protected
    FDataRenderer : TDataRenderer;

    FSelectedNode : PVirtualNode;
  private
    procedure NodeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  end;

implementation

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
    NodeDataSize := SizeOf(Int64);
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
    OnMeasureItem := @NodeMeasure;
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
  Result := FProfileProvider.GetProfile(ANode^.Index).HoverProfile;
end;
      
function TDataRenderer.GetSelectedProfile (ANode : PVirtualNode) : 
  TRendererProfile;
begin
  Result := FProfileProvider.GetProfile(ANode^.Index).SelectedProfile;
end;
    
function TDataRenderer.GetDefaultProfile (ANode : PVirtualNode) : 
  TRendererProfile;
begin
  Result := FProfileProvider.GetProfile(ANode^.Index).DefaultProfile;
end;

function TDataRenderer.IsHoverEnable (ANode : PVirtualNode) : Boolean;
begin
  Result := (FTreeView.HotNode = ANode) and (GetHoverProfile(ANode).Enable);
end;

function TDataRenderer.IsSelectEnable (ANode : PVirtualNode) : Boolean;
begin
  Result := (FTreeView.Selected[ANode]) and (GetSelectedProfile(ANode).Enable);
end;

procedure TDataRenderer.NodeMeasure (ASender : TBaseVirtualTree; ATargetCanvas : 
  TCanvas; ANode : PVirtualNode; var ANodeHeight : Integer);
begin
  if IsHoverEnable(ANode) then
  begin
    ANodeHeight := GetHoverProfile(ANode).Height;
    Exit;
  end;

  if IsSelectEnable(ANode) then
  begin
    ANodeHeight := GetSelectedProfile(ANode).Height;
    Exit;
  end;

  ANodeHeight := GetDefaultProfile(ANode).Height;
end;

procedure TDataRenderer.NodeChange(Sender: TBaseVirtualTree; Node: 
  PVirtualNode);
begin
  if Node = nil then
    Exit;

  { Restore previous node height. }
  if (FSelectedNode <> nil) and (not FTreeView.Selected[FSelectedNode]) then
  begin
    FSelectedNode^.NodeHeight := GetDefaultProfile(FSelectedNode).Height;
    FTreeView.InvalidateNode(FSelectedNode);
  end;

  { Set selected node height. }
  if IsSelectEnable(Node) then
  begin
    Node^.NodeHeight := GetSelectedProfile(Node).Height;
    FSelectedNode := Node;
    FTreeView.InvalidateNode(Node);

    Exit;
  end;

  Node^.NodeHeight := GetDefaultProfile(Node).Height;
  FTreeView.InvalidateNode(Node);
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
  Profile : TRendererProfile;
begin
  if (FTreeView.HotNode = APaintInfo.Node) and 
     (GetHoverProfile(APaintInfo.Node).Enable)
     then
  begin
    if (FTreeView.Selected[APaintInfo.Node]) then
    begin
      Profile := GetSelectedProfile(APaintInfo.Node);
      Profile.Background := GetHoverProfile(APaintInfo.Node).Background;

      FRenderer.Draw(FDataProvider.GetObject(APaintInfo.Node^.Index), 
        Profile, APaintInfo.Canvas, APaintInfo.CellRect);  
      Exit;  
    end;

    FRenderer.Draw(FDataProvider.GetObject(APaintInfo.Node^.Index), 
      GetHoverProfile(APaintInfo.Node), 
      APaintInfo.Canvas, APaintInfo.CellRect);  
    Exit;
  end;

  if (FTreeView.Selected[APaintInfo.Node]) and (GetSelectedProfile(
     APaintInfo.Node).Enable) then
  begin
    FRenderer.Draw(FDataProvider.GetObject(APaintInfo.Node^.Index),
      GetSelectedProfile(APaintInfo.Node),
      APaintInfo.Canvas, APaintInfo.CellRect);
    Exit;
  end;

  FRenderer.Draw(FDataProvider.GetObject(APaintInfo.Node^.Index),
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
  PNodeID : PInt64;
  Node : PVirtualNode;
begin
  if (not FDataProvider.Load) or (not FProfileProvider.Load) then
    Exit;

  FTreeView.Clear;
  FTreeView.BeginUpdate;

  for ObjectItem in FDataProvider do
  begin
    Node := FTreeView.AddChild(nil);
    PNodeID := PInt64(FTreeView.GetNodeData(Node));
    if Assigned(PNodeID) then
    begin
      PNodeID^ := ObjectItem.ID;
    end;
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

procedure TMainMenuDataRenderer.NodeChange (Sender : TBaseVirtualTree; Node :
  PVirtualNode);
begin
  if Node = nil then
    Exit;

  if (TMainMenuItem(FDataRenderer.FDataProvider.GetObject(Node^.Index)).ItemType
    = MENU_ITEM_LOGO) and (FSelectedNode <> nil) then
  begin
    FDataRenderer.FTreeView.Selected[FSelectedNode] := True;
    Exit;
  end;
  
  FSelectedNode := Node;

  { Run menu item callback if asigned. }
  if Assigned(TMainMenuItem(FDataRenderer.FDataProvider.GetObject(Node^.Index))
    .Callback) then
  begin
    TMainMenuItem(FDataRenderer.FDataProvider.GetObject(Node^.Index)).Callback;
  end;
end;

procedure TMainMenuDataRenderer.RedrawSelection;
begin
  FDataRenderer.FTreeView.InvalidateNode(FSelectedNode);
end;

end.
