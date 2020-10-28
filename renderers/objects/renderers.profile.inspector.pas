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
    const
      KEY_COLUMN = 0;
      VALUE_COLUMN = 1;

      DEFAULT_ITEM_HEIGHT = 27;
      ITEM_LEFT_PADDING = 5;
      ITEM_RIGHT_PADDING = 5;
      ITEM_TOP_PADDING = 2;
      ITEM_BOTTOM_PADDING = 2;
      ITEM_TEXT_TOP_PADDING = 1;
      ITEM_TEXT_BOTTOM_PADDING = 1;

    type
      TItemType = (
        TYPE_ITEM_TITLE,
        TYPE_ITEM_BACKGROUND,
        TYPE_ITEM_HEIGHT
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
    procedure DrawColorValue (AValue : TColor; ARect : TRect; ACanvas : 
      TCanvas);
    procedure DrawTextValue (AValue : String; ARect : TRect; ACanvas :
      TCanvas);
    procedure ValueChange (ANode : PVirtualNode; AColumn : TColumnIndex;
      AItemType : Integer; AValue : Pointer);
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
    Integer(TYPE_ITEM_TITLE) : begin
      ACanvas.Font.Style := [fsBold, fsItalic];
      Result := ACanvas.TextHeight(AData.Text) + ITEM_TEXT_TOP_PADDING +
        ITEM_TEXT_BOTTOM_PADDING;
    end
    else
      Result := DEFAULT_ITEM_HEIGHT;
  end;
end;

procedure TProfileInspectorRenderer.DrawBackground (AItemType : Integer;
  AState : TItemStates; ARect : TRect; ACanvas : TCanvas);
var
  OldBrush : TBrush;
begin
  OldBrush := ACanvas.Brush;
  
  ACanvas.Brush.Color := clWhite;

  if AItemType <> Integer(TYPE_ITEM_TITLE) then
  begin
    if ITEM_HOVER in AState then
      ACanvas.Brush.Color := clSilver;
  end;

  ACanvas.FillRect(ARect);

  ACanvas.Brush := OldBrush;
end;

procedure TProfileInspectorRenderer.DrawColorValue (AValue : TColor; ARect : 
  TRect; ACanvas : TCanvas);
begin
  ACanvas.Brush.Color := AValue;
  ACanvas.FillRect(ARect.Left + ITEM_LEFT_PADDING, 
    ARect.Top + ITEM_TOP_PADDING, 
    ARect.Right - ITEM_RIGHT_PADDING, 
    ARect.Bottom - ITEM_BOTTOM_PADDING);
  ACanvas.Pen.Style := psDot;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Rectangle(ARect.Left + ITEM_LEFT_PADDING, 
    ARect.Top + ITEM_TOP_PADDING, 
    ARect.Right - ITEM_RIGHT_PADDING, 
    ARect.Bottom - ITEM_BOTTOM_PADDING);
end;

procedure TProfileInspectorRenderer.DrawTextValue (AValue : String; ARect : 
  TRect; ACanvas : TCanvas);
var
  TextHeight : Integer;
begin
  TextHeight := ACanvas.TextHeight(AValue);
  ACanvas.TextOut(ARect.Left + ITEM_LEFT_PADDING, 
    ARect.Top + (DEFAULT_ITEM_HEIGHT - TextHeight) div 2,
    AValue);
end;

procedure TProfileInspectorRenderer.ItemDraw (ANode : PVirtualNode; AColumn : 
  TColumnIndex; AItemType : Integer; ACanvas : TCanvas; ACellRect : TRect; 
  AContentRect : TRect; AState : TItemStates; AData : TObjectInspectorData);
var
  OldBrush : TBrush;
begin
  OldBrush := ACanvas.Brush;

  if AColumn = KEY_COLUMN then
    DrawBackground(AItemType, AState, Rect(AContentRect.Left, ACellRect.Top,
      ACellRect.Right, ACellRect.Bottom), ACanvas)
  else
    DrawBackground(AItemType, AState, ACellRect, ACanvas);

  case AItemType of
    Integer(TYPE_ITEM_TITLE) : begin
      if AColumn = KEY_COLUMN then
      begin
        ACanvas.Font.Style := [fsBold, fsItalic];
        ACanvas.TextOut(ACellRect.Left + ITEM_LEFT_PADDING, ACellRect.Top + 
          ITEM_TEXT_TOP_PADDING, AData.Text);
      end;
    end else begin
      ACanvas.Font.Style := [];
      
      if AColumn = KEY_COLUMN then
      begin
        DrawTextValue(AData.Text, AContentRect, ACanvas);
      end else
      begin
        case AItemType of
          Integer(TYPE_ITEM_BACKGROUND) : 
            DrawColorValue(FProfile.Background, ACellRect, ACanvas);
          Integer(TYPE_ITEM_HEIGHT) : 
            DrawTextValue(IntToStr(FProfile.Height), ACellRect, ACanvas);
        end;
      end;
    end;
  end;

  ACanvas.Brush := OldBrush;
end;

function TProfileInspectorRenderer.ItemEditor (ANode : PVirtualNode; AColumn : 
  TColumnIndex; AItemType : Integer; AData : TObjectInspectorData) : 
  IVTEditLink;
begin
  if (AColumn = KEY_COLUMN) or (not FTreeView.Selected[ANode]) or 
    not AData.Editable then
    Exit(nil);

  case AItemType of
    Integer(TYPE_ITEM_TITLE) : begin
      Result := nil;
    end;
    Integer(TYPE_ITEM_BACKGROUND) : begin
      Result := TColorEditor.Create(FProfile.Background, AItemType,
        @ValueChange);
    end;
    Integer(TYPE_ITEM_HEIGHT) : begin
      Result := TSpinEditEditor.Create(FProfile.Height, AItemType,
        @ValueChange);
    end;
  end;
end;

procedure TProfileInspectorRenderer.NodeClick (ASender : TBaseVirtualTree; 
  const AHitInfo : THitInfo);
begin
  ASender.EditNode(AHitInfo.HitNode, VALUE_COLUMN);
end;

procedure TProfileInspectorRenderer.TreeResize (ASender : TObject);
begin
  FTreeView.Header.Columns[KEY_COLUMN].Width := (FTreeView.Width div 2) - 2;
  FTreeView.Header.Columns[VALUE_COLUMN].Width := FTreeView.Width div 2;
end;

procedure TProfileInspectorRenderer.ValueChange (ANode : PVirtualNode; AColumn :
  TColumnIndex; AItemType : Integer; AValue : Pointer);
begin
  case AItemType of
    Integer(TYPE_ITEM_BACKGROUND) : begin
      FProfile.Background := PColor(AValue)^;
    end;
    Integer(TYPE_ITEM_HEIGHT) : begin
      FProfile.Height := PInteger(AValue)^;
    end;
  end;

  FTreeView.InvalidateNode(ANode);
end;

procedure TProfileInspectorRenderer.UpdateProfile (AProfile : TRendererProfile);
var
  Data : TObjectInspectorData;
begin
  if AProfile = nil then
    Exit;

  FProfile := AProfile;
  FTreeView.Clear;

  Data.Text := 'Properties';
  Data.Editable := False;  
  AppendData(nil, Integer(TYPE_ITEM_TITLE), Data);

  Data.Text := 'Background';
  Data.Editable := True;
  AppendData(nil, Integer(TYPE_ITEM_BACKGROUND), Data);

  Data.Text := 'Height';
  Data.Editable := True;
  AppendData(nil, Integer(TYPE_ITEM_HEIGHT), Data);
end;

end.
