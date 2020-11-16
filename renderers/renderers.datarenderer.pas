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
  SysUtils, Graphics, Types, VirtualTrees, objects.common,
  dataproviders.common, profilesprovider.common, renderers.common;

type
  TDataRenderer = class
  public
    constructor Create (ATreeView : TVirtualDrawTree; ADataProvider : 
      TCommonDataProvider; AProfileProvider : TCommonProfilesProvider;
      ARenderer : TCommonRenderer);
    destructor Destroy; override;

    
  protected
    FTreeView : TVirtualDrawTree;
    FDataProvider : TCommonDataProvider;
    FProfileProvider : TCommonProfilesProvider;
    FRenderer : TCommonRenderer;
  private
    { Setup VirtualTreeView parameters. }
    procedure TreeViewParams;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return true if ANode is selected. }
    function SelectedNode (ANode : PVirtualNode) : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return true if ANode is hover. }
    function HoverNode (ANode : PVirtualNode) : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Get ANode height. }
    procedure NodeMeasure (ASender : TBaseVirtualTree; ATargetCanvas : TCanvas;
      ANode : PVirtualNode; var ANodeHeight : Integer);

    { Redraw hover node. }
    procedure NodeHotChange (ASender : TBaseVirtualTree; AOldNode, ANewNode :
      PVirtualNode);

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
    NodeDataSize := SizeOf(Pointer);
    TreeOptions.PaintOptions := [toShowRoot, toUseBlendedImages, toHotTrack, 
      toHideFocusRect, toAlwaysHideSelection, toHideSelection];
    TreeOptions.MiscOptions := [toFullRepaintResize, toVariableNodeHeight, 
      toWheelPanning];
    TreeOptions.SelectionOptions := [];

    OnMeasureItem := @NodeMeasure;
    OnDrawNode := @NodeDraw;
    OnResize := @TreeResize;
    OnShowScrollBar := @ShowScrollBar;

    Header.Columns.Clear;
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
     (FProfileProvider.GetProfile[ANode^.Index].HoverProfile.Enable) then
  begin
    ANodeHeight := 
      FProfileProvider.GetProfile[ANode^.Index].HoverProfile.Height;
    Exit;
  end;

  if (SelectedNode(ANode)) and
     (FProfileProvider.GetProfile[ANode^.Index].SelectedProfile.Enable) then
  begin
    ANodeHeight := 
      FProfileProvider.GetProfile[ANode^.Index].SelectedProfile.Height;
    Exit;
  end;

  ANodeHeight := 
    FProfileProvider.GetProfile[ANode^.Index].DefaultProfile.Height;
end;

procedure TDataRenderer.NodeHotChange (ASender : TBaseVirtualTree; AOldNode, 
  ANewNode : PVirtualNode);
begin
  FTreeView.InvalidateNode(AOldNode);
  FTreeView.InvalidateNode(ANewNode);
end;

procedure TDataRenderer.NodeDraw (ASender : TBaseVirtualTree; const APaintInfo :
  TVTPaintInfo);
begin
  if (HoverNode(APaintInfo.Node)) and
     (FProfileProvider.GetProfile(APaintInfo.Node^.Index).HoverProfile.Enable)
     then
  begin
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
    FProfileProvider.GetProfile(APaintInfo.Node^.Index).HoverProfile,
    APaintInfo.Canvas, APaintInfo.CellRect);
end;

procedure TDataRenderer.TreeResize (ASender : TObject);
begin
  // ??
end;

procedure TDataRenderer.ShowScrollBar (ASender : TBaseVirtualTree; ABar : 
  Integer; AShow : Boolean);
begin
  TreeResize(ASender);
end;

end.
