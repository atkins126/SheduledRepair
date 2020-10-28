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
  SysUtils, renderers.virtualtreeview, renderer.profile.profile,
  VirtualTrees, Classes, Graphics, Types;

type
  TObjectInspectorData = record
    Text : String;
    Editable : Boolean;
  end;  

  TProfileInspectorRenderer = class
    (specialize TVirtualTreeViewRenderer<TObjectInspectorData>)
  public
    constructor Create (ATreeView : TVirtualDrawTree; ADrawOptions : 
      TDrawOptions = [ITEM_DRAW_BUTTONS]);
    destructor Destroy; override;

    procedure UpdateProfile (AProfile : TRendererProfile);
  protected
    type
      TItemType = (
        TYPE_TITLE,
        TYPE_TEXT_VALUE,
        TYPE_INTEGER_VALUE,
        TYPE_COLOR_VALUE
      );
  protected
    function ItemHeight (ANode : PVirtualNode; ACanvas : TCanvas; AIndex : 
      Cardinal; AItemType : Integer; AData : TObjectInspectorData) : Cardinal; 
      override;
    procedure ItemDraw (ANode : PVirtualNode; AColumn : TColumnIndex; 
      AItemType : Integer; ACanvas : TCanvas; ACellRect : TRect; AContentRect : 
      TRect; AState : TItemStates; AData : TObjectInspectorData); override;
    function ItemEditor (ANode : PVirtualNode; AColumn : TColumnIndex; 
      AItemType : Integer; AData : TObjectInspectorData) : IVTEditLink; 
      override;
  private
    FProfile : TRendererProfile;

    procedure NodeClick (ASender : TBaseVirtualTree; const AHitInfo : THitInfo);
    procedure TreeResize (ASender : TObject);

    procedure DrawBackground (AItemType : Integer; AState : TItemStates; ARect : 
      TRect; ACanvas : TCanvas);
  end;

implementation

{ TObjectsInspectorRenderer }

constructor TProfileInspectorRenderer.Create (ATreeView : TVirtualDrawTree;
  ADrawOptions : TDrawOptions);
begin
  inherited Create(ATreeView, ADrawOptions);
  FProfile := nil;

  FTreeView.OnNodeClick := @NodeClick;
  FTreeView.OnResize := @TreeResize;

  AppendColumn((FTreeView.Width div 2) - 2);
  AppendColumn(FTreeView.Width div 2);
end;

destructor TProfileInspectorRenderer.Destroy;
begin
  FreeAndNil(FProfile);
  inherited Destroy;
end;

function TProfileInspectorRenderer.ItemHeight (ANode : PVirtualNode; ACanvas :
  TCanvas; AIndex : Cardinal; AItemType : Integer; AData : 
  TObjectInspectorData) : Cardinal;
begin
  case AItemType of
    Integer(TYPE_TITLE) : begin
      ACanvas.Font.Style := [fsBold, fsItalic];
      Result := ACanvas.TextHeight(AData.Text) + 2;
    end
    else
      Result := 29;
  end;
end;

procedure TProfileInspectorRenderer.DrawBackground (AItemType : Integer;
  AState : TItemStates; ARect : TRect; ACanvas : TCanvas);
var
  OldBrush : TBrush;
begin
  OldBrush := ACanvas.Brush;

  if AItemType <> Integer(TYPE_TITLE) then
  begin
    if ITEM_HOVER in AState then
      ACanvas.Brush.Color := clSilver
    else if ITEM_SELECTED in AState then
      ACanvas.Brush.Color := clYellow
    else
      ACanvas.Brush.Color := clWhite;
  end else
    ACanvas.Brush.Color := clWhite;

  ACanvas.FillRect(ARect);

  ACanvas.Brush := OldBrush;
end;

procedure TProfileInspectorRenderer.ItemDraw (ANode : PVirtualNode; AColumn : 
  TColumnIndex; AItemType : Integer; ACanvas : TCanvas; ACellRect : TRect; 
  AContentRect : TRect; AState : TItemStates; AData : TObjectInspectorData);
var
  OldBrush : TBrush;
begin
  OldBrush := ACanvas.Brush;

  if AColumn = 0 then
    DrawBackground(AItemType, AState, Rect(AContentRect.Left, ACellRect.Top,
      ACellRect.Right, ACellRect.Bottom), ACanvas)
  else
    DrawBackground(AItemType, AState, ACellRect, ACanvas);

  case AItemType of
    Integer(TYPE_TITLE) : begin
      if AColumn = 0 then
      begin
        ACanvas.Font.Style := [fsBold, fsItalic];
        ACanvas.TextOut(AContentRect.Left + 1, AContentRect.Top + 1,
          AData.Text);
      end;
    end else begin
      ACanvas.Font.Style := [];

      if AColumn > 0 then
      begin
        ACanvas.TextOut(AContentRect.Left + 5, AContentRect.Top + 5,
          '110');
        Exit;
      end;

      if AColumn = 0 then
      begin
        ACanvas.TextOut(AContentRect.Left + 5, AContentRect.Top + 5,
          AData.Text);
      end;

    end;
  end;

  ACanvas.Brush := OldBrush;
end;

function TProfileInspectorRenderer.ItemEditor (ANode : PVirtualNode; AColumn : 
  TColumnIndex; AItemType : Integer; AData : TObjectInspectorData) : 
  IVTEditLink;
begin
  if (AColumn = 0) or (not FTreeView.Selected[ANode]) or not AData.Editable then
    Exit(nil);

  case AItemType of
    Integer(TYPE_TITLE) : begin
      Result := nil;
    end;
    Integer(TYPE_TEXT_VALUE) : begin
      Result := TEditEditor.Create;
    end;
    Integer(TYPE_INTEGER_VALUE) : begin
      Result := TSpinEditEditor.Create;
    end;
    Integer(TYPE_COLOR_VALUE) : begin
      Result := TColorEditor.Create;
    end;
  end;
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

procedure TProfileInspectorRenderer.UpdateProfile (AProfile : TRendererProfile);
var
  Data : TObjectInspectorData;
begin
  if AProfile = nil then
    Exit;

  FTreeView.Clear;

  Data.Text := 'Settings';
  Data.Editable := False;  
  AppendData(nil, Integer(TYPE_TITLE), Data);

  Data.Text := 'Background';
  Data.Editable := True;
  AppendData(nil, Integer(TYPE_COLOR_VALUE), Data);

  Data.Text := 'Height';
  Data.Editable := True;
  AppendData(nil, Integer(TYPE_INTEGER_VALUE), Data);
end;

end.
