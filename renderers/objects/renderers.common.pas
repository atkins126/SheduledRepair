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
unit renderers.common;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Types, renderers.virtualtreeview, 
  renderer.profile.profile, renderer.profile.profileitem, objects.common, 
  objects.mainmenu.item, dataproviders.common;

type
  TCommonRenderer = class
    (specialize TVirtualTreeViewRenderer<TCommonObject>)
  public
    constructor Create (ATreeView : TVirtualDrawTree; ADataProvider : 
      TCommonDataProvider);
    destructor Destroy; override;
  protected
    { Draw item. }
    procedure Draw (AItemType : Integer; ACanvas : TCanvas; ARect : TRect;
      AState : TItemState; AObject : TCommonObject; AProfile :
      TRendererProfile); virtual; abstract;
  protected
    { Get tree item height. }
    function ItemHeight (ANode : PVirtualNode; ACanvas : TCanvas; AIndex : 
      Cardinal; AItemType : Integer; AData : TCommonObject) : Cardinal; 
      override;

    { Draw tree item. }
    procedure ItemDraw (ANode : PVirtualNode; AColumn : TColumnIndex; 
      AItemType : Integer; ACanvas : TCanvas; ACellRect : TRect; AContentRect : 
      TRect; AState : TItemStates; AData : TCommonObject); override;

    { Resize virtual tree view event. }
    procedure TreeResize (ASender : TObject);

    { Scroll virtual tree view event. }
    procedure ShowScrollBar (ASender: TBaseVirtualTree; ABar: Integer; AShow: 
      Boolean);
  protected
    FDataProvider : TCommonDataProvider;
  end;

implementation

{ TMainMenuRenderer }

constructor TMainMenuRenderer.Create (ATreeView : TVirtualDrawTree;
  ADataProvider : TCommonDataProvider);
begin
  inherited Create(ATreeView, []);
  FDataProvider := ADataProvider;

  FTreeView.OnResize := @TreeResize;
  FTreeView.OnShowScrollBar := @ShowScrollBar;

  { Create one column. }
  AppendColumn (FTreeView.ClientWidth);
end;

destructor TMainMenuRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TCommonRenderer.TreeResize (ASender : TObject);
begin
  FTreeView.Header.Columns[0].Width := FTreeView.ClientWidth;
end;

procedure TCommonRenderer.ShowScrollBar (ASender : TBaseVirtualTree; 
  ABar : Integer; AShow : Boolean);
begin
  TreeResize(FTreeView);
end;

function TCommonRenderer.ItemHeight (ANode : PVirtualNode; ACanvas : TCanvas;
  AIndex : Cardinal; AItemType : Integer; AData : TCommonObject) : Cardinal;
begin
  { Get hover item profile item height. }
  if (FDataProvider.GetObjectProfile(AIndex).HoverProfile.Enable) and
     (FTreeView.HotNode = ANode) then
    Exit(FDataProvider.GetObjectProfile(AIndex).HoverProfile.Height);

  { Get selected item profile item height. }
  if (FDataProvider.GetObjectProfile(AIndex).SelectedProfile.Enable) and
     (FTreeView.Selected[ANode]) then
    Exit(FDataProvider.GetObjectProfile(AIndex).SelectedProfile.Height;

  { Get default item profile item height. }
  Result := FDataProvider.GetObjectProfile(AIndex).DefaultProfile.Height;
end;

procedure TCommonRenderer.ItemDraw (ANode : PVirtualNode; AColumn : 
  TColumnIndex; AItemType : Integer; ACanvas : TCanvas; ACellRect : TRect; 
  AContentRect :  TRect; AState : TItemStates; AData : TCommonObject);
begin
  { Draw a row. }
  Draw(AItemType, ACanvas, ACellRect, AState, AData, 
    FDataProvider.GetObjectProfile(ANode^.Index));
end;

end.
