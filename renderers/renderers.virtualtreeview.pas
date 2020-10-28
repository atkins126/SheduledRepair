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
unit renderers.virtualtreeview;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Types, StdCtrls, Spin, LMessages, LCLType,
  ColorBox, VirtualTrees;

type
  generic TVirtualTreeViewRenderer<T> = class
  public
    type
      TDrawOption = (
        ITEM_DRAW_FOCUS,
        ITEM_DRAW_BUTTONS,
        ITEM_DRAW_TREE_LINES
      );
      TDrawOptions = set of TDrawOption;
  public
    constructor Create (ATreeView : TVirtualDrawTree; ADrawOptions :
      TDrawOptions = [ITEM_DRAW_BUTTONS]);
    destructor Destroy; override;
  protected
    type
      TItemState = (
        ITEM_SELECTED,
        ITEM_HOVER
      );
      TItemStates = set of TItemState;
  protected
    procedure AppendColumn (AWidth : Cardinal);
    function AppendData (AParentItem : PVirtualNode; AItemType : Integer; 
      AData : T) : PVirtualNode;

    function GetData (ANode : PVirtualNode) : T;

    function ItemHeight (ANode : PVirtualNode; ACanvas : TCanvas; AIndex : 
      Cardinal; AItemType : Integer; AData : T) : Cardinal; virtual; abstract;
    procedure ItemDraw (ANode : PVirtualNode; AColumn : TColumnIndex; 
      AItemType : Integer; ACanvas : TCanvas; ACellRect : TRect; AContentRect : 
      TRect; AState : TItemStates; AData : T); virtual; abstract;
    function ItemEditor (ANode : PVirtualNode; AColumn : TColumnIndex; 
      AItemType : Integer; AData : T) : IVTEditLink; virtual;
  private
    type
      PItem = ^TItem;
      TItem = record
        Data : T;
        MenuItemType : Integer;
      end;
  private
    procedure NodeDraw (ASender: TBaseVirtualTree; const APaintInfo:
      TVTPaintInfo);
    procedure NodeMeasure (ASender: TBaseVirtualTree; ATargetCanvas: TCanvas;
      ANode: PVirtualNode; var ANodeHeight: Integer);
    procedure NodeHotChange (ASender: TBaseVirtualTree; AOldNode, ANewNode:
      PVirtualNode);
    procedure NodeCreateEditor (ASender: TBaseVirtualTree; ANode: PVirtualNode;
      AColumn: TColumnIndex; out AEditLink: IVTEditLink);
    procedure NodeAllowEdit (ASender: TBaseVirtualTree; ANode: PVirtualNode;
      AColumn: TColumnIndex; var Allowed: Boolean);
  protected
    FTreeView : TVirtualDrawTree;
  end;

  TEditEditor = class(TInterfacedObject, IVTEditLink)
  public
    type
      TValueChangeCallback = procedure (ANode : PVirtualNode; AColumn :
        TColumnIndex; AItemType : Integer; AValue : Pointer) of object;
  public
    constructor Create (AValue : String; AItemType : Integer; ACallback : 
      TValueChangeCallback);
    destructor Destroy; override;
    function BeginEdit : Boolean; stdcall;
    function CancelEdit : Boolean; stdcall;
    function EndEdit : Boolean; stdcall;
    function GetBounds : TRect; stdcall;
    function PrepareEdit (ATree : TBaseVirtualTree; ANode : PVirtualNode;
      AColumn : TColumnIndex) : Boolean; stdcall;
    procedure ProcessMessage (var AMessage : TLMessage); stdcall;
    procedure SetBounds (ARect : TRect); stdcall;
  protected
    procedure EditKeyDown (ASender : TObject; var AKey : Word; AShift :
      TShiftState);
  private
    FEdit : TEdit;
    FTree : TVirtualDrawTree;
    FNode : PVirtualNode;
    FColumn : Integer;
    FValue : String;
    FItemType : Integer;
    FCallback : TValueChangeCallback;
  end;

  TSpinEditEditor = class(TInterfacedObject, IVTEditLink)
  public
    type
      TValueChangeCallback = procedure (ANode : PVirtualNode; AColumn :
        TColumnIndex; AItemType : Integer; AValue : Pointer) of object;
  public
    constructor Create (AValue : Integer; AItemType : Integer; ACallback : 
      TValueChangeCallback);
    destructor Destroy; override;
    function BeginEdit : Boolean; stdcall;
    function CancelEdit : Boolean; stdcall;
    function EndEdit : Boolean; stdcall;
    function GetBounds : TRect; stdcall;
    function PrepareEdit (ATree : TBaseVirtualTree; ANode : PVirtualNode;
      AColumn : TColumnIndex) : Boolean; stdcall;
    procedure ProcessMessage (var AMessage : TLMessage); stdcall;
    procedure SetBounds (ARect : TRect); stdcall;
  protected
    procedure EditKeyDown (ASender : TObject; var AKey : Word; AShift :
      TShiftState);
  private
    FEdit : TSpinEdit;
    FTree : TVirtualDrawTree;
    FNode : PVirtualNode;
    FColumn : Integer;
    FValue : Integer;
    FItemType : Integer;
    FCallback : TValueChangeCallback;
  end;

  TColorEditor = class(TInterfacedObject, IVTEditLink)
  public
    type
      TValueChangeCallback = procedure (ANode : PVirtualNode; AColumn :
        TColumnIndex; AItemType : Integer; AValue : Pointer) of object;
  public
    constructor Create (AValue : TColor; AItemType : Integer; ACallback : 
      TValueChangeCallback);
    destructor Destroy; override;
    function BeginEdit : Boolean; stdcall;
    function CancelEdit : Boolean; stdcall;
    function EndEdit : Boolean; stdcall;
    function GetBounds : TRect; stdcall;
    function PrepareEdit (ATree : TBaseVirtualTree; ANode : PVirtualNode;
      AColumn : TColumnIndex) : Boolean; stdcall;
    procedure ProcessMessage (var AMessage : TLMessage); stdcall;
    procedure SetBounds (ARect : TRect); stdcall;
  protected
    procedure EditKeyDown (ASender : TObject; var AKey : Word; AShift :
      TShiftState);
  private
    FEdit : TColorBox;
    FTree : TVirtualDrawTree;
    FNode : PVirtualNode;
    FColumn : Integer;
    FValue : TColor;
    FItemType : Integer;
    FCallback : TValueChangeCallback;
  end;

  TListEditor = class(TInterfacedObject, IVTEditLink)
  public
    type
      TValueChangeCallback = procedure (ANode : PVirtualNode; AColumn :
        TColumnIndex; AItemType : Integer; AValue : Pointer) of object;
  public
    constructor Create (ASelected : Integer; AItemType : Integer; ACallback : 
      TValueChangeCallback);
    destructor Destroy; override;

    procedure AppendItem (AItem : String);

    function BeginEdit : Boolean; stdcall;
    function CancelEdit : Boolean; stdcall;
    function EndEdit : Boolean; stdcall;
    function GetBounds : TRect; stdcall;
    function PrepareEdit (ATree : TBaseVirtualTree; ANode : PVirtualNode;
      AColumn : TColumnIndex) : Boolean; stdcall;
    procedure ProcessMessage (var AMessage : TLMessage); stdcall;
    procedure SetBounds (ARect : TRect); stdcall;
  protected
    procedure EditKeyDown (ASender : TObject; var AKey : Word; AShift :
      TShiftState);
  private
    FEdit : TComboBox;
    FTree : TVirtualDrawTree;
    FNode : PVirtualNode;
    FColumn : Integer;
    FItems : TStringList;
    FSelected : Integer;
    FItemType : Integer;
    FCallback : TValueChangeCallback;
  end;

implementation

{ TVirtualTreeViewRenderer }

constructor TVirtualTreeViewRenderer.Create (ATreeView : 
  TVirtualDrawTree; ADrawOptions : TDrawOptions);
begin
  FTreeView := ATreeView;
  FTreeView.NodeDataSize := Sizeof(TItem);
  FTreeView.TreeOptions.PaintOptions:= [toShowRoot, toUseBlendedImages,
    toHotTrack];
  FTreeView.TreeOptions.MiscOptions := [toFullRepaintOnResize,
    toVariableNodeHeight, toWheelPanning, toEditable];
  FTreeView.TreeOptions.SelectionOptions:= [toFullRowSelect];

  if ITEM_DRAW_BUTTONS in ADrawOptions then
  begin
    FTreeView.TreeOptions.PaintOptions := FTreeView.TreeOptions.PaintOptions +
      [toShowButtons];
    FTreeView.TreeOptions.AutoOptions:= [toAutoHideButtons];
  end;

  if ITEM_DRAW_TREE_LINES in ADrawOptions then
  begin
    FTreeView.TreeOptions.PaintOptions := FTreeView.TreeOptions.PaintOptions +
      [toShowTreeLines];
  end;

  if not (ITEM_DRAW_FOCUS in ADrawOptions) then
  begin
    FTreeView.TreeOptions.PaintOptions := FTreeView.TreeOptions.PaintOptions +
      [toHideFocusRect, toAlwaysHideSelection, toHideSelection];
  end;

  FTreeView.Header.Columns.Clear;

  with FTreeView do
  begin
    OnMeasureItem := @NodeMeasure;
    OnDrawNode := @NodeDraw;
    OnCreateEditor := @NodeCreateEditor;
    OnEditing := @NodeAllowEdit;
  end;
end;

destructor TVirtualTreeViewRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TVirtualTreeViewRenderer.AppendColumn (AWidth : Cardinal);
var
  Column : TVirtualTreeColumn;
begin
  Column := FTreeView.Header.Columns.Add;

  with Column do
  begin
    Width := AWidth;
    Style := vsOwnerDraw;
  end;
end;

function TVirtualTreeViewRenderer.AppendData (AParentItem : PVirtualNode; 
  AItemType : Integer; AData : T) : PVirtualNode;
var
  Item : PItem;
  Node : PVirtualNode;
begin
  FTreeView.BeginUpdate;
  Node := FTreeView.AddChild(AParentItem);
  Item := PItem(FTreeView.GetNodeData(Node));
  if Assigned(Item) then
  begin
    Item^.Data := AData;
    Item^.MenuItemType := AItemType;
  end;
  Result := Node;
  FTreeView.EndUpdate;
end;

function TVirtualTreeViewRenderer.GetData (ANode : PVirtualNode) : T;
begin
  Result := PItem(FTreeView.GetNodeData(ANode))^.Data;
end;

procedure TVirtualTreeViewRenderer.NodeMeasure (ASender: TBaseVirtualTree;
  ATargetCanvas: TCanvas; ANode: PVirtualNode; var ANodeHeight: Integer);
begin
  ANodeHeight := ItemHeight(ANode, ATargetCanvas, ANode^.Index, 
    PItem(ASender.GetNodeData(ANode))^.MenuItemType, 
    PItem(ASender.GetNodeData(ANode))^.Data);
end;

procedure TVirtualTreeViewRenderer.NodeDraw (ASender : TBaseVirtualTree;
  const APaintInfo : TVTPaintInfo);
var
  Item : PItem;
  State : TItemStates;
begin
  Item := PItem(ASender.GetNodeData(APaintInfo.Node));
  if Assigned(Item) then
  begin
    State := [];

    if ASender.Selected[APaintInfo.Node] then
    begin
      State := State + [ITEM_SELECTED];
    end;

    if APaintInfo.Node = ASender.HotNode then
    begin
      State := State + [ITEM_HOVER];
    end;

    ItemDraw(APaintInfo.Node, APaintInfo.Column, Item^.MenuItemType,
      APaintInfo.Canvas, APaintInfo.CellRect, APaintInfo.ContentRect, State,
      Item^.Data);
  end;
end;

procedure TVirtualTreeViewRenderer.NodeHotChange (ASender : TBaseVirtualTree;
  AOldNode, ANewNode : PVirtualNode);
begin
  FTreeView.InvalidateNode(AOldNode);
  FTreeView.InvalidateNode(ANewNode);
end;

procedure TVirtualTreeViewRenderer.NodeCreateEditor (ASender : TBaseVirtualTree;
  ANode : PVirtualNode; AColumn : TColumnIndex; out AEditLink : IVTEditLink);
var
  Item : PItem;
begin
  Item := PItem(ASender.GetNodeData(ANode));
  AEditLink := ItemEditor(ANode, AColumn, Item^.MenuItemType, Item^.Data);
end;

procedure TVirtualTreeViewRenderer.NodeAllowEdit (ASender : TBaseVirtualTree;
  ANode : PVirtualNode; AColumn : TColumnIndex; var Allowed : Boolean);
var
  Item : PItem;
begin
  Item := PItem(ASender.GetNodeData(ANode));
  Allowed := ItemEditor(ANode, AColumn, Item^.MenuItemType, Item^.Data) <> nil;
end;

function TVirtualTreeViewRenderer.ItemEditor (ANode : PVirtualNode; AColumn : 
  TColumnIndex; AItemType : Integer; AData : T) : IVTEditLink;
begin
  Result := nil;
end;

{ TEditEditor }

constructor TEditEditor.Create (AValue : String; AItemType : Integer;
  ACallback : TValueChangeCallback);
begin
  FValue := AValue;
  FItemType := AItemType;
  FCallback := ACallback;
end;

destructor TEditEditor.Destroy;
begin
  FreeAndNil(FEdit);
  inherited Destroy;
end;

procedure TEditEditor.EditKeyDown (ASender : TObject; var AKey : Word; AShift :
  TShiftState);
begin
  case AKey of
    VK_ESCAPE : begin
      FTree.CancelEditNode;
      AKey := 0;
    end;
    VK_RETURN : begin
      FTree.EndEditNode;
      AKey := 0;
    end;
  end;
end;

function TEditEditor.BeginEdit : Boolean; stdcall;
begin
  FEdit.Show;
  FEdit.SetFocus;
  Result := True;
end;

function TEditEditor.CancelEdit : Boolean; stdcall;
begin
  FEdit.Hide;
  Result := True;
end;

function TEditEditor.EndEdit : Boolean; stdcall;
var
  Text : String;
begin
  if Assigned(FCallback) then
  begin
    Text := FEdit.Text;
    FCallback(FNode, FColumn, FItemType, @Text);
  end;
  FEdit.Hide;
  FTree.SetFocus;
  Result := True;
end;

function TEditEditor.GetBounds : TRect; stdcall;
begin
  Result := FEdit.BoundsRect;
end;

function TEditEditor.PrepareEdit (ATree : TBaseVirtualTree; ANode :
  PVirtualNode; AColumn : TColumnIndex) : Boolean; stdcall;
begin
  FTree := TVirtualDrawTree(ATree);
  FNode := ANode;
  FColumn := AColumn;

  FreeAndNil(FEdit);
  FEdit := TEdit.Create(nil);
  FEdit.AutoSize := False;
  FEdit.Visible := False;
  FEdit.Parent := ATree;
  FEdit.Text := FValue;
  FEdit.OnKeyDown := @EditKeyDown;

  Result := True;
end;

procedure TEditEditor.ProcessMessage (var AMessage : TLMessage); stdcall;
begin
  FEdit.WindowProc(AMessage);
end;

procedure TEditEditor.SetBounds (ARect : TRect); stdcall;
var
  Dummy : Integer;
begin
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, ARect.Right);
  FEdit.BoundsRect := ARect;
end;

{ TSpinEditEditor }

constructor TSpinEditEditor.Create (AValue : Integer; AItemType : Integer;
  ACallback : TValueChangeCallback);
begin
  FValue := AValue;
  FItemType := AItemType;
  FCallback := ACallback;
end;

destructor TSpinEditEditor.Destroy;
begin
  FreeAndNil(FEdit);
  inherited Destroy;
end;

procedure TSpinEditEditor.EditKeyDown (ASender : TObject; var AKey : Word; AShift :
  TShiftState);
begin
  case AKey of
    VK_ESCAPE : begin
      FTree.CancelEditNode;
      AKey := 0;
    end;
    VK_RETURN : begin
      FTree.EndEditNode;
      AKey := 0;
    end;
  end;
end;

function TSpinEditEditor.BeginEdit : Boolean; stdcall;
begin
  FEdit.Show;
  FEdit.SetFocus;
  Result := True;
end;

function TSpinEditEditor.CancelEdit : Boolean; stdcall;
begin
  FEdit.Hide;
  Result := True;
end;

function TSpinEditEditor.EndEdit : Boolean; stdcall;
var
  Value : Integer;
begin
  if Assigned(FCallback) then
  begin
    Value := FEdit.Value;
    FCallback(FNode, FColumn, FItemType, @Value);
  end;
  FEdit.Hide;
  FTree.SetFocus;
  Result := True;
end;

function TSpinEditEditor.GetBounds : TRect; stdcall;
begin
  Result := FEdit.BoundsRect;
end;

function TSpinEditEditor.PrepareEdit (ATree : TBaseVirtualTree; ANode :
  PVirtualNode; AColumn : TColumnIndex) : Boolean; stdcall;
begin
  FTree := TVirtualDrawTree(ATree);
  FNode := ANode;
  FColumn := AColumn;

  FreeAndNil(FEdit);
  FEdit := TSpinEdit.Create(nil);
  FEdit.AutoSize := False;
  FEdit.Visible := False;
  FEdit.Parent := ATree;
  FEdit.Value := FValue;
  FEdit.MinValue := 0;
  FEdit.MaxValue := MAXINT - 1;
  FEdit.OnKeyDown := @EditKeyDown;

  Result := True;
end;

procedure TSpinEditEditor.ProcessMessage (var AMessage : TLMessage); stdcall;
begin
  FEdit.WindowProc(AMessage);
end;

procedure TSpinEditEditor.SetBounds (ARect : TRect); stdcall;
var
  Dummy : Integer;
begin
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, ARect.Right);
  FEdit.BoundsRect := ARect;
end;

{ TColorEdit }

constructor TColorEditor.Create (AValue : TColor; AItemType : Integer;
  ACallback : TValueChangeCallback);
begin
  FValue := AValue;
  FItemType := AItemType;
  FCallback := ACallback;
end;

destructor TColorEditor.Destroy;
begin
  FreeAndNil(FEdit);
  inherited Destroy;
end;

procedure TColorEditor.EditKeyDown (ASender : TObject; var AKey : Word; AShift :
  TShiftState);
begin
  case AKey of
    VK_ESCAPE : begin
      FTree.CancelEditNode;
      AKey := 0;
    end;
    VK_RETURN : begin
      FTree.EndEditNode;
      AKey := 0;
    end;
  end;
end;

function TColorEditor.BeginEdit : Boolean; stdcall;
begin
  FEdit.Show;
  FEdit.SetFocus;
  Result := True;
end;

function TColorEditor.CancelEdit : Boolean; stdcall;
begin
  FEdit.Hide;
  Result := True;
end;

function TColorEditor.EndEdit : Boolean; stdcall;
var
  Color : TColor;
begin
  if Assigned(FCallback) then
  begin
    Color := FEdit.Selected;
    FCallback(FNode, FColumn, FItemType, @Color);
  end;
  FEdit.Hide;
  FTree.SetFocus;
  Result := True;
end;

function TColorEditor.GetBounds : TRect; stdcall;
begin
  Result := FEdit.BoundsRect;
end;

function TColorEditor.PrepareEdit (ATree : TBaseVirtualTree; ANode :
  PVirtualNode; AColumn : TColumnIndex) : Boolean; stdcall;
begin
  FTree := TVirtualDrawTree(ATree);
  FNode := ANode;
  FColumn := AColumn;

  FreeAndNil(FEdit);
  FEdit := TColorBox.Create(nil);
  FEdit.AutoSize := False;
  FEdit.Visible := False;
  FEdit.Parent := ATree;
  FEdit.Selected := FValue;
  FEdit.Style := [cbCustomColor, cbPrettyNames, cbStandardColors];
  FEdit.OnKeyDown := @EditKeyDown;

  Result := True;
end;

procedure TColorEditor.ProcessMessage (var AMessage : TLMessage); stdcall;
begin
  FEdit.WindowProc(AMessage);
end;

procedure TColorEditor.SetBounds (ARect : TRect); stdcall;
var
  Dummy : Integer;
begin
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, ARect.Right);
  FEdit.BoundsRect := ARect;
end;

{ TListEditor }

constructor TListEditor.Create (ASelected : Integer; AItemType : Integer;
  ACallback : TValueChangeCallback);
begin
  FSelected := ASelected;
  FItemType := AItemType;
  FCallback := ACallback;
  FItems := TStringList.Create;
end;

destructor TListEditor.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FEdit);
  inherited Destroy;
end;

procedure TListEditor.AppendItem (AItem : String);
begin
  FItems.Add(AItem);
end;

procedure TListEditor.EditKeyDown (ASender : TObject; var AKey : Word; AShift :
  TShiftState);
begin
  case AKey of
    VK_ESCAPE : begin
      FTree.CancelEditNode;
      AKey := 0;
    end;
    VK_RETURN : begin
      FTree.EndEditNode;
      AKey := 0;
    end;
  end;
end;

function TListEditor.BeginEdit : Boolean; stdcall;
begin
  FEdit.Show;
  FEdit.SetFocus;
  Result := True;
end;

function TListEditor.CancelEdit : Boolean; stdcall;
begin
  FEdit.Hide;
  Result := True;
end;

function TListEditor.EndEdit : Boolean; stdcall;
var
  Selected : Integer;
begin
  if Assigned(FCallback) then
  begin
    Selected := FEdit.ItemIndex;
    FCallback(FNode, FColumn, FItemType, @Selected);
  end;
  FEdit.Hide;
  FTree.SetFocus;
  Result := True;
end;

function TListEditor.GetBounds : TRect; stdcall;
begin
  Result := FEdit.BoundsRect;
end;

function TListEditor.PrepareEdit (ATree : TBaseVirtualTree; ANode :
  PVirtualNode; AColumn : TColumnIndex) : Boolean; stdcall;
begin
  FTree := TVirtualDrawTree(ATree);
  FNode := ANode;
  FColumn := AColumn;

  FreeAndNil(FEdit);
  FEdit := TComboBox.Create(nil);
  FEdit.AutoSize := False;
  FEdit.Visible := False;
  FEdit.Parent := ATree;
  FEdit.Items := FItems;
  FEdit.ItemIndex := FSelected;
  FEdit.Style := csDropDownList;
  FEdit.OnKeyDown := @EditKeyDown;

  Result := True;
end;

procedure TListEditor.ProcessMessage (var AMessage : TLMessage); stdcall;
begin
  FEdit.WindowProc(AMessage);
end;

procedure TListEditor.SetBounds (ARect : TRect); stdcall;
var
  Dummy : Integer;
begin
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, ARect.Right);
  FEdit.BoundsRect := ARect;
end;

end.
