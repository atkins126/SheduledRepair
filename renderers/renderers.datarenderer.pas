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
    procedure NodeHotChange (ASender: TBaseVirtualTree; AOldNode, ANewNode:
      PVirtualNode);
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
end;

destructor TDataRenderer.Destroy;
begin
  inherited Destroy;
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

end.
