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
unit renderers.profile.inspector;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, renderers.virtualtreeview, renderer.profile.objectprofile,
  VirtualTrees, Classes, Graphics, Types;

type
  TObjectInspectorData = class
  public
    constructor Create;
    destructor Destroy; override;
  
  end;  

  TProfileInspectorRenderer = class
    (specialize TVirtualTreeViewRenderer<TObjectInspectorData>)
  public
    constructor Create (ATreeView : TVirtualDrawTree; ADrawOptions : 
      TDrawOptions = [ITEM_DRAW_BUTTONS]);
    destructor Destroy; override;
  protected
    function ItemHeight (ANode : PVirtualNode; AIndex : Cardinal; AData : 
      TObjectInspectorData) : Cardinal; override;
    procedure ItemDraw (ANode : PVirtualNode; AColumn : TColumnIndex; 
      AItemType : Integer; ACanvas : TCanvas; ACellRect : TRect; AContentRect : 
      TRect; AState : TItemStates; AData : TObjectInspectorData); override;
    function ItemEditor (ANode : PVirtualNode; AColumn : TColumnIndex; 
      AItemType : Integer; AData : TObjectInspectorData) : IVTEditLink; 
      override;
  private
    FProfile : TRendererObjectProfile;

    procedure NodeClick (ASender : TBaseVirtualTree; const AHitInfo : THitInfo);
    procedure TreeResize (ASender : TObject);
  end;

implementation

{ TObjectInpectorData }

constructor TObjectInspectorData.Create;
begin
  
end;

destructor TObjectInspectorData.Destroy;
begin
  
  inherited Destroy;
end;

{ TObjectsInspectorRenderer }

constructor TProfileInspectorRenderer.Create (ATreeView : TVirtualDrawTree;
  ADrawOptions : TDrawOptions);
begin
  inherited Create(ATreeView, ADrawOptions);
  FProfile := TRendererObjectProfile.Create(-1);
  FProfile.SelectedProfile.Background := clYellow;
  FProfile.HoverProfile.Background := clSilver;

  FTreeView.OnNodeClick := @NodeClick;
  FTreeView.OnResize := @TreeResize;

  AppendColumn((FTreeView.Width div 2) - 2);
  AppendColumn(FTreeView.Width div 2);

  AppendData(0, TObjectInspectorData.Create);
  AppendData(0, TObjectInspectorData.Create);
end;

destructor TProfileInspectorRenderer.Destroy;
begin
  FreeAndNil(FProfile);
  inherited Destroy;
end;

function TProfileInspectorRenderer.ItemHeight (ANode : PVirtualNode; AIndex : 
  Cardinal; AData : TObjectInspectorData) : Cardinal;
begin
  Result := 29;
end;

procedure TProfileInspectorRenderer.ItemDraw (ANode : PVirtualNode; AColumn : 
  TColumnIndex; AItemType : Integer; ACanvas : TCanvas; ACellRect : TRect; 
  AContentRect : TRect; AState : TItemStates; AData : TObjectInspectorData);
var
  OldBrush : TBrush;
begin
  OldBrush := ACanvas.Brush;

  if ITEM_HOVER in AState then
    ACanvas.Brush.Color := FProfile.HoverProfile.Background
  else if ITEM_SELECTED in AState then
    ACanvas.Brush.Color := FProfile.SelectedProfile.Background
  else
    ACanvas.Brush.Color := FProfile.DefaultProfile.Background;

  case AColumn of
    0 : begin
      ACanvas.FillRect(AContentRect.Left, ACellRect.Top, ACellRect.Right,
        ACellRect.Bottom);
    end;
    1 : begin
      ACanvas.FillRect(ACellRect);
    end;
  end;


  if AColumn > 0 then
  begin
    ACanvas.TextOut(AContentRect.Left + 5, AContentRect.Top + 5, '32');
    Exit;
  end;

  if AColumn = 0 then
  begin
    ACanvas.TextOut(AContentRect.Left + 5, AContentRect.Top + 5, 'test');
  end;

  ACanvas.Brush := OldBrush;
end;

function TProfileInspectorRenderer.ItemEditor (ANode : PVirtualNode; AColumn : 
  TColumnIndex; AItemType : Integer; AData : TObjectInspectorData) : 
  IVTEditLink;
begin
  if (AColumn = 0) or (not FTreeView.Selected[ANode]) then
    Exit(nil);

  Result := TEditEditor.Create;
end;

procedure TProfileInspectorRenderer.NodeClick (ASender : TBaseVirtualTree; 
  const AHitInfo : THitInfo);
begin
  ASender.EditNode(AHitInfo.HitNode, 1);
end;

procedure TProfileInspectorRenderer.TreeResize (ASender : TObject);
begin
  FTreeView.Header.Columns[0].Width := (FTreeView.Width div 2) - 2;
  FTreeView.Header.Columns[1].Width := FTreeView.Width div 2;
end;

end.
