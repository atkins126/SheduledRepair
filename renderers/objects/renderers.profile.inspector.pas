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
unit renderers.profile.inspector;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Types, Dialogs, VirtualTrees, 
  renderers.virtualtreeview, renderer.profile.profile, 
  renderer.profile.profileitem;

type
  { Tree view connected item data. }
  TObjectInspectorData = record
    Text : String;
    Element : String;
    Editable : Boolean;
  end;  

  { Profile inspector. }
  TProfileInspectorRenderer = class
    (specialize TVirtualTreeViewRenderer<TObjectInspectorData>)
  public
    constructor Create (ACanDisable : Boolean; ATreeView : TVirtualDrawTree);
    destructor Destroy; override;

    { Reread profile data and redraw inspector. }
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
        TYPE_ITEM_ENABLE,
        TYPE_ITEM_BACKGROUND,
        TYPE_ITEM_HEIGHT,

        TYPE_ELEMENT_ENABLE,
        TYPE_ELEMENT_BACKGROUND,
        TYPE_ELEMENT_BACKGROUND_TYPE,
        TYPE_ELEMENT_BACKGROUND_RADIUS,
        TYPE_ELEMENT_FONT,
        TYPE_ELEMENT_FONT_NAME,
        TYPE_ELEMENT_FONT_SIZE,
        TYPE_ELEMENT_FONT_COLOR,
        TYPE_ELEMENT_PADDING,
        TYPE_ELEMENT_PADDING_TOP,
        TYPE_ELEMENT_PADDING_LEFT,
        TYPE_ELEMENT_PADDING_BOTTOM,
        TYPE_ELEMENT_PADDING_RIGHT,
        TYPE_ELEMENT_POSITION_TYPE,
        TYPE_ELEMENT_POSITION,
        TYPE_ELEMENT_POSITION_X,
        TYPE_ELEMENT_POSITION_Y
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

    { Create inspector view columns. }
    procedure CreateColumns; 
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    FProfile : TRendererProfile;
    FCanDisable : Boolean;

    procedure NodeClick (ASender : TBaseVirtualTree; const AHitInfo : THitInfo);
    procedure TreeResize (ASender : TObject);
    procedure ShowScrollBar (ASender: TBaseVirtualTree; ABar: Integer; AShow: 
      Boolean);

    procedure ValueChange (ANode : PVirtualNode; AColumn : TColumnIndex;
      AItemType : Integer; AValue : Pointer);
    procedure ValueEditorClick (ANode : PVirtualNode; AColumn : TColumnIndex;
      AItemType : Integer; var AValue : Pointer);

    procedure DrawBackground (AItemType : Integer; AState : TItemStates; ARect : 
      TRect; ACanvas : TCanvas);
      {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure DrawColorValue (AValue : TColor; ARect : TRect; ACanvas : 
      TCanvas);
      {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure DrawTextValue (AValue : String; ARect : TRect; ACanvas :
      TCanvas);
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
  end;

implementation

{ TObjectsInspectorRenderer }

constructor TProfileInspectorRenderer.Create (ACanDisable : Boolean; 
  ATreeView : TVirtualDrawTree);
begin
  inherited Create(ATreeView, [ITEM_DRAW_BUTTONS]);
  FProfile := nil;
  FCanDisable := ACanDisable;

  FTreeView.OnNodeClick := @NodeClick;
  FTreeView.OnResize := @TreeResize;
  FTreeView.OnShowScrollBar := @ShowScrollBar;

  { Create inspector view columns. }
  CreateColumns;
end;

destructor TProfileInspectorRenderer.Destroy;
begin
  FreeAndNil(FProfile);
  inherited Destroy;
end;

procedure TProfileInspectorRenderer.CreateColumns;
begin
  AppendColumn((FTreeView.ClientWidth div 2) - 2); { Key column. }
  AppendColumn(FTreeView.ClientWidth div 2);       { Data column. }
end;

function TProfileInspectorRenderer.ItemHeight (ANode : PVirtualNode; ACanvas :
  TCanvas; AIndex : Cardinal; AItemType : Integer; AData : 
  TObjectInspectorData) : Cardinal;
begin
  case TItemType(AItemType) of
    TYPE_ITEM_TITLE : begin
      { Calculate title item height. }
      ACanvas.Font.Style := [fsBold];
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
  Value : String;
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
        ACanvas.Font.Style := [fsBold];
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
        case TItemType(AItemType) of
          TYPE_ITEM_ENABLE : begin
            if FProfile.Enable then
              DrawTextValue('[Enable]', ACellRect, ACanvas)
            else
              DrawTextValue('[Disable]', ACellRect, ACanvas);
            end;
          TYPE_ITEM_BACKGROUND : 
            DrawColorValue(FProfile.Background, ACellRect, ACanvas);
          TYPE_ITEM_HEIGHT : 
            DrawTextValue(IntToStr(FProfile.Height), ACellRect, ACanvas);
          TYPE_ELEMENT_ENABLE : begin
            if FProfile.Items[AData.Element].Enable then
              DrawTextValue('[Enable]', ACellRect, ACanvas)
            else
              DrawTextValue('[Disable]', ACellRect, ACanvas);
            end;  
          TYPE_ELEMENT_BACKGROUND :
            DrawColorValue(FProfile.Items[AData.Element].Background, ACellRect,
              ACanvas);
          TYPE_ELEMENT_BACKGROUND_TYPE : begin
            case FProfile.Items[AData.Element].BackgroundFillType of
              FILL_NONE : Value := '[Transparent]';
              FILL_SQUARE : Value := '[Rectangle]';
              FILL_SQUARE_ROUND_CORNER : Value := '[Rounded rectangle]';
            end;
            DrawTextValue(Value, ACellRect, ACanvas);
          end;
          TYPE_ELEMENT_BACKGROUND_RADIUS : 
            DrawTextValue(
              IntToStr(FProfile.Items[AData.Element].BackgroundRoundRadius),
              ACellRect, ACanvas);
          TYPE_ELEMENT_FONT : 
            DrawTextValue('[Font]', ACellRect, ACanvas);
          TYPE_ELEMENT_FONT_NAME :
            DrawTextValue(FProfile.Items[AData.Element].FontName, ACellRect, 
              ACanvas);
          TYPE_ELEMENT_FONT_SIZE :
            DrawTextValue(IntToStr(FProfile.Items[AData.Element].FontSize),
              ACellRect, ACanvas);
          TYPE_ELEMENT_FONT_COLOR :
            DrawColorValue(FProfile.Items[AData.Element].FontColor, ACellRect,
              ACanvas);
          TYPE_ELEMENT_PADDING :
            DrawTextValue('[' +
              IntToStr(FProfile.Items[AData.Element].Padding.Top) +
              ', ' + IntToStr(FProfile.Items[AData.Element].Padding.Left) +
              ', ' + IntToStr(FProfile.Items[AData.Element].Padding.Bottom) +
              ', ' + IntToStr(FProfile.Items[AData.Element].Padding.Right)
              + ']', ACellRect, ACanvas);
          TYPE_ELEMENT_PADDING_TOP :
            DrawTextValue(IntToStr(FProfile.Items[AData.Element].Padding.Top), 
              ACellRect, ACanvas);
          TYPE_ELEMENT_PADDING_LEFT : 
            DrawTextValue(IntToStr(FProfile.Items[AData.Element].Padding.Left),
              ACellRect, ACanvas);
          TYPE_ELEMENT_PADDING_BOTTOM :
            DrawTextValue(IntToStr(FProfile.Items[AData.Element].Padding.Bottom),
              ACellRect, ACanvas);
          TYPE_ELEMENT_PADDING_RIGHT :
            DrawTextValue(IntToStr(FProfile.Items[AData.Element].Padding.Right),
              ACellRect, ACanvas);
          TYPE_ELEMENT_POSITION_TYPE : begin
            case FProfile.Items[AData.Element].PositionType of
              POSITION_FIXED : Value := '[Fixed]';
              POSITION_FLOAT : Value := '[Float]';
            end;
            DrawTextValue(Value, ACellRect, ACanvas);
          end;
          TYPE_ELEMENT_POSITION :
            DrawTextValue('[' + 
              IntToStr(FProfile.Items[AData.Element].Position.X) +
              ', ' + IntToStr(FProfile.Items[AData.Element].Position.Y)
              + ']', ACellRect, ACanvas);
          TYPE_ELEMENT_POSITION_X :
            DrawTextValue(IntToStr(FProfile.Items[AData.Element].Position.X),
              ACellRect, ACanvas);
          TYPE_ELEMENT_POSITION_Y :
            DrawTextValue(IntToStr(FProfile.Items[AData.Element].Position.Y),
              ACellRect, ACanvas);
        end;
      end;
    end;
  end;

  ACanvas.Brush := OldBrush;
end;

function TProfileInspectorRenderer.ItemEditor (ANode : PVirtualNode; AColumn : 
  TColumnIndex; AItemType : Integer; AData : TObjectInspectorData) : 
  IVTEditLink;
var
  Editor : TListEditor;
begin
  if (AColumn = KEY_COLUMN) or (not FTreeView.Selected[ANode]) or 
    not AData.Editable then
    Exit(nil);

  case AItemType of
    Integer(TYPE_ITEM_TITLE) : begin
      Result := nil;
    end;
    Integer(TYPE_ITEM_ENABLE) : begin
      Result := TCheckEditor.Create(FProfile.Enable, 'Enable', AItemType, 
        @ValueChange);
    end;
    Integer(TYPE_ITEM_BACKGROUND) : begin
      Result := TColorEditor.Create(FProfile.Background, AItemType,
        @ValueChange);
    end;
    Integer(TYPE_ITEM_HEIGHT) : begin
      Result := TSpinEditEditor.Create(FProfile.Height, AItemType,
        @ValueChange);
    end;
    Integer(TYPE_ELEMENT_ENABLE) : begin
      Result := TCheckEditor.Create(FProfile.Items[AData.Element].Enable,
        'Enable', AItemType, @ValueChange);
    end;
    Integer(TYPE_ELEMENT_BACKGROUND) : begin
      Result := TColorEditor.Create(FProfile.Items[AData.Element].Background, 
        AItemType, @ValueChange);
    end;
    Integer(TYPE_ELEMENT_BACKGROUND_TYPE) : begin
      Editor := TListEditor.Create(
      Integer(FProfile.Items[AData.Element].BackgroundFillType), AItemType,
        @ValueChange);
      Editor.AppendItem('Transparent');
      Editor.AppendItem('Rectangle');
      Editor.AppendItem('Rounded rectangle');

      Result := Editor;
    end;
    Integer(TYPE_ELEMENT_BACKGROUND_RADIUS) : begin
      Result := TSpinEditEditor.Create(
        FProfile.Items[AData.Element].BackgroundRoundRadius, AItemType,
        @ValueChange);
    end;
    Integer(TYPE_ELEMENT_FONT) : begin
      Result := TEditButtonEditor.Create('[Font]', AItemType, @ValueEditorClick,
        @ValueChange);
    end;
    Integer(TYPE_ELEMENT_FONT_NAME) : 
      Result := TEditEditor.Create(FProfile.Items[AData.Element].FontName,
        AItemType, @ValueChange);
    Integer(TYPE_ELEMENT_FONT_SIZE) :
      Result := TSpinEditEditor.Create(FProfile.Items[AData.Element].FontSize,
        AItemType, @ValueChange);
    Integer(TYPE_ELEMENT_FONT_COLOR) :
      Result := TColorEditor.Create(FProfile.Items[AData.Element].FontColor,
        AItemType, @ValueChange);
    Integer(TYPE_ELEMENT_PADDING_TOP) :
      Result := TSpinEditEditor.Create(
        FProfile.Items[AData.Element].Padding.Top, AItemType, @ValueChange);
    Integer(TYPE_ELEMENT_PADDING_LEFT) : 
      Result := TSpinEditEditor.Create(
        FProfile.Items[AData.Element].Padding.Left, AItemType, @ValueChange);
    Integer(TYPE_ELEMENT_PADDING_BOTTOM) : 
      Result := TSpinEditEditor.Create(
        FProfile.Items[AData.Element].Padding.Bottom, AItemType, @ValueChange);
    Integer(TYPE_ELEMENT_PADDING_RIGHT) : 
      Result := TSpinEditEditor.Create(
        FProfile.Items[AData.Element].Padding.Right, AItemType, @ValueChange);
    Integer(TYPE_ELEMENT_POSITION_TYPE) : begin
      Editor := TListEditor.Create(
        Integer(FProfile.Items[AData.Element].PositionType), AItemType,
        @ValueChange);
      Editor.AppendItem('Fixed');
      Editor.AppendItem('Float');

      Result := Editor; 
    end;
    Integer(TYPE_ELEMENT_POSITION_X) :
      Result := TSpinEditEditor.Create(
        FProfile.Items[AData.Element].Position.X, AItemType, @ValueChange);
    Integer(TYPE_ELEMENT_POSITION_Y) :
      Result := TSpinEditEditor.Create(
        FProfile.Items[AData.Element].Position.Y, AItemType, @ValueChange);
  end;
end;

procedure TProfileInspectorRenderer.ValueEditorClick (ANode : PVirtualNode; 
  AColumn : TColumnIndex; AItemType : Integer; var AValue : Pointer);
var
  FontDialog : TFontDialog;
begin
  case AItemType of
    Integer(TYPE_ELEMENT_FONT) : begin
      FontDialog := TFontDialog.Create(nil);
      FontDialog.Font.Name := FProfile.Items[GetData(ANode).Element].FontName;
      FontDialog.Font.Size := FProfile.Items[GetData(ANode).Element].FontSize;
      FontDialog.Font.Color := FProfile.Items[GetData(ANode).Element].FontColor;

      if not FontDialog.Execute then
      begin
        AValue := nil;
        Exit;
      end;

      AValue := FontDialog.Font;
      Exit;
    end;
  end;

  AValue := nil;
end;

procedure TProfileInspectorRenderer.NodeClick (ASender : TBaseVirtualTree; 
  const AHitInfo : THitInfo);
begin
  ASender.EditNode(AHitInfo.HitNode, VALUE_COLUMN);
end;

procedure TProfileInspectorRenderer.TreeResize (ASender : TObject);
begin
  FTreeView.Header.Columns[KEY_COLUMN].Width := (FTreeView.ClientWidth div 2)
    - 2;
  FTreeView.Header.Columns[VALUE_COLUMN].Width := FTreeView.ClientWidth div 2;
end;

procedure TProfileInspectorRenderer.ShowScrollBar (ASender : TBaseVirtualTree; 
  ABar : Integer; AShow : Boolean);
begin
  TreeResize(FTreeView);
end;

procedure TProfileInspectorRenderer.ValueChange (ANode : PVirtualNode; AColumn :
  TColumnIndex; AItemType : Integer; AValue : Pointer);
begin
  case AItemType of
    Integer(TYPE_ITEM_ENABLE) : begin
      FProfile.Enable := PBoolean(AValue)^;
    end;
    Integer(TYPE_ITEM_BACKGROUND) : begin
      FProfile.Background := PColor(AValue)^;
    end;
    Integer(TYPE_ITEM_HEIGHT) : begin
      FProfile.Height := PInteger(AValue)^;
    end;
    Integer(TYPE_ELEMENT_ENABLE) : begin
      FProfile.Items[GetData(ANode).Element].Enable := PBoolean(AValue)^;
    end;
    Integer(TYPE_ELEMENT_BACKGROUND) : begin
      FProfile.Items[GetData(ANode).Element].Background := PColor(AValue)^;
    end;
    Integer(TYPE_ELEMENT_BACKGROUND_TYPE) : begin
      FProfile.Items[GetData(ANode).Element].BackgroundFillType := 
        TRendererProfileItem.TBackgroundFillType(PInteger(AValue)^);
    end;
    Integer(TYPE_ELEMENT_BACKGROUND_RADIUS) : begin
      FProfile.Items[GetData(ANode).Element].BackgroundRoundRadius :=
        PInteger(AValue)^;
    end;
    Integer(TYPE_ELEMENT_FONT) : begin
      if AValue <> nil then
      begin
        FProfile.Items[GetData(ANode).Element].FontName := TFont(AValue).Name;
        FProfile.Items[GetData(ANode).Element].FontSize := TFont(AValue).Size;
        FProfile.Items[GetData(ANode).Element].FontColor := TFont(AValue).Color;
      end;
    end;
    Integer(TYPE_ELEMENT_FONT_NAME) : 
      FProfile.Items[GetData(ANode).Element].FontName := PString(AValue)^;
    Integer(TYPE_ELEMENT_FONT_SIZE) :
      FProfile.Items[GetData(ANode).Element].FontSize := PInteger(AValue)^;
    Integer(TYPE_ELEMENT_FONT_COLOR) :
      FProfile.Items[GetData(ANode).Element].FontColor := PColor(AValue)^;
    Integer(TYPE_ELEMENT_PADDING_TOP) :
      FProfile.Items[GetData(ANode).Element].Padding.Top := PInteger(AValue)^;
    Integer(TYPE_ELEMENT_PADDING_LEFT) :
      FProfile.Items[GetData(ANode).Element].Padding.Left := PInteger(AValue)^;
    Integer(TYPE_ELEMENT_PADDING_BOTTOM) :
      FProfile.Items[GetData(ANode).Element].Padding.Bottom := 
      PInteger(AValue)^;  
    Integer(TYPE_ELEMENT_PADDING_RIGHT) :
      FProfile.Items[GetData(ANode).Element].Padding.Right := PInteger(AValue)^;
    Integer(TYPE_ELEMENT_POSITION_TYPE) : begin
      FProfile.Items[GetData(ANode).Element].PositionType := 
        TRendererProfileItem.TPositionType(PInteger(AValue)^);
    end;
    Integer(TYPE_ELEMENT_POSITION_X) : 
      FProfile.Items[GetData(ANode).Element].Position.X := PInteger(AValue)^;
    Integer(TYPE_ELEMENT_POSITION_Y) :
      FProfile.Items[GetData(ANode).Element].Position.Y := PInteger(AValue)^;
  end;

  FTreeView.InvalidateNode(ANode);
end;

procedure TProfileInspectorRenderer.UpdateProfile (AProfile : TRendererProfile);
var
  Data : TObjectInspectorData;
  ProfileItem : TRendererProfileItem;
  Parent : PVirtualNode;
begin
  if AProfile = nil then
    Exit;

  FProfile := AProfile;
  FTreeView.Clear;

  Data.Text := 'Properties';
  Data.Element := '';
  Data.Editable := False;  
  AppendData(nil, Integer(TYPE_ITEM_TITLE), Data);

  if FCanDisable then
  begin
    Data.Text := 'Enable';
    Data.Element := '';
    Data.Editable := True;
    AppendData(nil, Integer(TYPE_ITEM_ENABLE), Data);
  end;

  Data.Text := 'Background';
  Data.Element := '';
  Data.Editable := True;
  AppendData(nil, Integer(TYPE_ITEM_BACKGROUND), Data);

  Data.Text := 'Height';
  Data.Element := '';
  Data.Editable := True;
  AppendData(nil, Integer(TYPE_ITEM_HEIGHT), Data);

  for ProfileItem in FProfile.GetEnumerator do
  begin
    Data.Text := ProfileItem.Name;
    Data.Element := ProfileItem.Name;
    Data.Editable := False;
    AppendData(nil, Integer(TYPE_ITEM_TITLE), Data);

    Data.Text := 'Enable';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(nil, Integer(TYPE_ELEMENT_ENABLE), Data);

    Data.Text := 'Background';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(nil, Integer(TYPE_ELEMENT_BACKGROUND), Data);

    Data.Text := 'Background type';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(nil, Integer(TYPE_ELEMENT_BACKGROUND_TYPE), Data);

    Data.Text := 'Background corner radius';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(nil, Integer(TYPE_ELEMENT_BACKGROUND_RADIUS), Data);

    Data.Text := 'Font';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    Parent := AppendData(nil, Integer(TYPE_ELEMENT_FONT), Data);

    Data.Text := 'Font name';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(Parent, Integer(TYPE_ELEMENT_FONT_NAME), Data);

    Data.Text := 'Font size';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(Parent, Integer(TYPE_ELEMENT_FONT_SIZE), Data);

    Data.Text := 'Font color';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(Parent, Integer(TYPE_ELEMENT_FONT_COLOR), Data);

    Data.Text := 'Padding';
    Data.Element := ProfileItem.Name;
    Data.Editable := False;
    Parent := AppendData(nil, Integer(TYPE_ELEMENT_PADDING), Data);

    Data.Text := 'Top';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(Parent, Integer(TYPE_ELEMENT_PADDING_TOP), Data);

    Data.Text := 'Left';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(Parent, Integer(TYPE_ELEMENT_PADDING_LEFT), Data);

    Data.Text := 'Bottom';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(Parent, Integer(TYPE_ELEMENT_PADDING_BOTTOM), Data);

    Data.Text := 'Right';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(Parent, Integer(TYPE_ELEMENT_PADDING_RIGHT), Data);

    Data.Text := 'Position type';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(nil, Integer(TYPE_ELEMENT_POSITION_TYPE), Data);

    Data.Text := 'Position';
    Data.Element := ProfileItem.Name;
    Data.Editable := False;
    Parent := AppendData(nil, Integer(TYPE_ELEMENT_POSITION), Data);

    Data.Text := 'X';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(Parent, Integer(TYPE_ELEMENT_POSITION_X), Data);

    Data.Text := 'Y';
    Data.Element := ProfileItem.Name;
    Data.Editable := True;
    AppendData(Parent, Integer(TYPE_ELEMENT_POSITION_Y), Data);
  end;
end;

end.
