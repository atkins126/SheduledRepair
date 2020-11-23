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
  SysUtils, Classes, Graphics, Types, VirtualTrees, objects.common,
  dataproviders.common, profilesprovider.common, renderers.common,
  renderer.profile.profile;

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
    procedure TreeViewParams;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Create VirtualTreeView columns. }
    procedure TreeViewCreateColumns;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return true if ANode is selected. }
    function SelectedNode (ANode : PVirtualNode) : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return true if ANode is hover. }
    function HoverNode (ANode : PVirtualNode) : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    procedure NodeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

    { Get ANode height. }
    procedure NodeMeasure (ASender : TBaseVirtualTree; ATargetCanvas : TCanvas;
      ANode : PVirtualNode; var ANodeHeight : Integer);

    { Select node. }
    procedure NodeClick(Sender : TBaseVirtualTree; const HitInfo: THitInfo);

    { Draw node. }
    procedure NodeDraw (ASender : TBaseVirtualTree; const APaintInfo :
      TVTPaintInfo);

    { Resize VirtualTreeNode. }
    procedure TreeResize (ASender : TObject);

    { On show scrollbars. }
    procedure ShowScrollBar (ASender : TBaseVirtualTree; ABar : Integer; AShow : 
      Boolean);
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

  TreeViewParams;
end;

destructor TDataRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TDataRenderer.TreeViewParams;
begin
  with FTreeView do
  begin
    NodeDataSize := SizeOf(Cardinal);
    TreeOptions.PaintOptions := [toShowRoot, toUseBlendedImages, toHotTrack, 
      toHideFocusRect, toAlwaysHideSelection, toHideSelection];
    TreeOptions.MiscOptions := [toFullRepaintOnResize, toVariableNodeHeight,
      toWheelPanning];
    TreeOptions.SelectionOptions := [toFullRowSelect];

    OnMeasureItem := @NodeMeasure;
    OnDrawNode := @NodeDraw;
    OnResize := @TreeResize;
    //OnShowScrollBar := @ShowScrollBar;
    //OnNodeClick := @NodeClick;
    OnChange := @NodeChange;

    Header.Columns.Clear;
    TreeViewCreateColumns;
  end;
end;

procedure TDataRenderer.TreeViewCreateColumns;
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

function TDataRenderer.SelectedNode (ANode : PVirtualNode) : Boolean;
begin
  Result := FTreeView.Selected[ANode];
end;

function TDataRenderer.HoverNode (ANode : PVirtualNode) : Boolean;
begin
  Result := (FTreeView.HotNode = ANode);
end;

procedure TDataRenderer.NodeMeasure (ASender : TBaseVirtualTree; ATargetCanvas : 
  TCanvas; ANode : PVirtualNode; var ANodeHeight : Integer);
begin
  if (HoverNode (ANode)) and
     (FProfileProvider.GetProfile(ANode^.Index).HoverProfile.Enable) then
  begin
    ANodeHeight := 
      FProfileProvider.GetProfile(ANode^.Index).HoverProfile.Height;
    Exit;
  end;

  if (SelectedNode(ANode)) and
     (FProfileProvider.GetProfile(ANode^.Index).SelectedProfile.Enable) then
  begin
    ANodeHeight := 
      FProfileProvider.GetProfile(ANode^.Index).SelectedProfile.Height;
    Exit;
  end;

  ANodeHeight := 
    FProfileProvider.GetProfile(ANode^.Index).DefaultProfile.Height;
end;

procedure TDataRenderer.NodeChange(Sender: TBaseVirtualTree; Node: 
  PVirtualNode);
begin
  if Node = nil then
    Exit;

  { Restore previous node height. }
  if (FSelectedNode <> nil) and (not SelectedNode(FSelectedNode)) then
  begin
    FSelectedNode^.NodeHeight :=
      FProfileProvider.GetProfile(FSelectedNode^.Index).DefaultProfile.Height;
    FTreeView.InvalidateNode(FSelectedNode);
  end;

  { Set selected node height. }
  if (SelectedNode(Node)) and
     (FProfileProvider.GetProfile(Node^.Index).SelectedProfile.Enable) then
  begin
    Node^.NodeHeight :=
      FProfileProvider.GetProfile(Node^.Index).SelectedProfile.Height;
    FSelectedNode := Node;
    FTreeView.InvalidateNode(Node);

    Exit;
  end;

  Node^.NodeHeight :=
    FProfileProvider.GetProfile(Node^.Index).DefaultProfile.Height;
  FTreeView.InvalidateNode(Node);
end;

procedure TDataRenderer.NodeClick(Sender : TBaseVirtualTree; const HitInfo: 
  THitInfo);
begin


  HitInfo.HitNode^.NodeHeight := FProfileProvider.GetProfile
    (HitInfo.HitNode^.Index).SelectedProfile.Height;
  Sender.InvalidateNode(HitInfo.HitNode);
end;

procedure TDataRenderer.NodeDraw (ASender : TBaseVirtualTree; const APaintInfo :
  TVTPaintInfo);
var
  Profile : TRendererProfile;
begin
  if (HoverNode(APaintInfo.Node)) and
     (FProfileProvider.GetProfile(APaintInfo.Node^.Index).HoverProfile.Enable)
     then
  begin
    if (SelectedNode(APaintInfo.Node)) then
    begin
      Profile :=
        FProfileProvider.GetProfile(APaintInfo.Node^.Index).SelectedProfile;
      Profile.Background := 
        FProfileProvider.GetProfile(APaintInfo.Node^.Index).HoverProfile.Background;

      FRenderer.Draw(FDataProvider.GetObject(APaintInfo.Node^.Index), 
        Profile, APaintInfo.Canvas, APaintInfo.CellRect);  
      Exit;  
    end;

    FRenderer.Draw(FDataProvider.GetObject(APaintInfo.Node^.Index), 
      FProfileProvider.GetProfile(APaintInfo.Node^.Index).HoverProfile, 
      APaintInfo.Canvas, APaintInfo.CellRect);  
    Exit;
  end;

  if (SelectedNode(APaintInfo.Node)) and
    (FProfileProvider.GetProfile(APaintInfo.Node^.Index).SelectedProfile.Enable)
     then
  begin
    FRenderer.Draw(FDataProvider.GetObject(APaintInfo.Node^.Index),
      FProfileProvider.GetProfile(APaintInfo.Node^.Index).SelectedProfile,
      APaintInfo.Canvas, APaintInfo.CellRect);
    Exit;
  end;

  FRenderer.Draw(FDataProvider.GetObject(APaintInfo.Node^.Index),
    FProfileProvider.GetProfile(APaintInfo.Node^.Index).DefaultProfile,
    APaintInfo.Canvas, APaintInfo.CellRect);
end;

procedure TDataRenderer.TreeResize (ASender : TObject);
begin
  TreeViewCreateColumns;
end;

procedure TDataRenderer.ShowScrollBar (ASender : TBaseVirtualTree; ABar : 
  Integer; AShow : Boolean);
begin
  TreeResize(ASender);
end;

procedure TDataRenderer.UpdateData;
var
  Node : PVirtualNode;
  ObjectItem : TCommonObject;
begin
  FTreeView.BeginUpdate;

  for ObjectItem in FDataProvider do
  begin
    Node := FTreeView.AddChild(nil);
  end;

  FTreeView.EndUpdate;
end;

end.
