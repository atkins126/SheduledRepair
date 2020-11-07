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
unit renderers.virtualtreeview;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Types, StdCtrls, Spin, LMessages, LCLType,
  ColorBox, EditBtn, VirtualTrees;

type
  { Custom renderer based on TVirtualTreeView.  }
  generic TVirtualTreeViewRenderer<T> = class
  public
    type
      { Draw item options. }
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
      { List item state. }
      TItemState = (
        ITEM_SELECTED,
        ITEM_HOVER
      );
      TItemStates = set of TItemState;
  protected
    { Add new column to tree view. }
    procedure AppendColumn (AWidth : Cardinal);
     
    { Add item node to virtual tree. }
    function AppendData (AParentItem : PVirtualNode; AItemType : Integer; 
      AData : T) : PVirtualNode;

    { Get item node connected data. }
    function GetData (ANode : PVirtualNode) : T;

    { Get tree item height. }
    function ItemHeight (ANode : PVirtualNode; ACanvas : TCanvas; AIndex : 
      Cardinal; AItemType : Integer; AData : T) : Cardinal; virtual; abstract;
    
    { Draw tree item. }
    procedure ItemDraw (ANode : PVirtualNode; AColumn : TColumnIndex; 
      AItemType : Integer; ACanvas : TCanvas; ACellRect : TRect; AContentRect : 
      TRect; AState : TItemStates; AData : T); virtual; abstract;
    
    { Connect editor to the tree item. }
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

  { Custom tree view editor. }
  generic TRendererCustomEditor<T> = class(TInterfacedObject, IVTEditLink)
  public
    type
      TValueChangeCallback = procedure (ANode : PVirtualNode; AColumn :
        TColumnIndex; AItemType : Integer; AValue : Pointer) of object;
  public
    constructor Create (AItemType : Integer; ACallback : 
      TValueChangeCallback);
    destructor Destroy; override;

    function GetEditorValue : Pointer; virtual; abstract;
    procedure CreateEditor; virtual; abstract;

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
  protected
    FEdit : T;
    FTree : TVirtualDrawTree;
    FNode : PVirtualNode;
    FColumn : Integer;
    FItemType : Integer;
    FCallback : TValueChangeCallback;
  end;

  TEditEditor = class(specialize TRendererCustomEditor<TEdit>)
  public
    constructor Create (AValue : String; AItemType : Integer; ACallback : 
      TValueChangeCallback);
    
    function GetEditorValue : Pointer; override;
    procedure CreateEditor; override;
  protected
    FValue : String;
  end;

  TSpinEditEditor = class(specialize TRendererCustomEditor<TSpinEdit>)
  public
    constructor Create (AValue : Integer; AItemType : Integer; ACallback : 
      TValueChangeCallback);
    
    function GetEditorValue : Pointer; override;
    procedure CreateEditor; override;
  protected
    FValue : Integer;
  end;

  TColorEditor = class(specialize TRendererCustomEditor<TColorBox>)
  public
    constructor Create (AValue : TColor; AItemType : Integer; ACallback : 
      TValueChangeCallback);
    
    function GetEditorValue : Pointer; override;
    procedure CreateEditor; override;
  protected
    FValue : TColor;
  end;

  TListEditor = class(specialize TRendererCustomEditor<TComboBox>)
  public
    constructor Create (ASelected : Integer; AItemType : Integer; ACallback : 
      TValueChangeCallback);
    destructor Destroy; override;

    procedure AppendItem (AItem : String);

    function GetEditorValue : Pointer; override;
    procedure CreateEditor; override;
  protected
    FItems : TStringList;
    FSelected : Integer;
  end;

  TCheckEditor = class(specialize TRendererCustomEditor<TCheckBox>)
  public
    constructor Create (AChecked : Boolean; ACaption : String; 
      AItemType : Integer; ACallback : TValueChangeCallback);
    
    function GetEditorValue : Pointer; override;
    procedure CreateEditor; override;
  protected
    FCaption : String;
    FChecked : Boolean;
  end;

  TEditButtonEditor = class(specialize TRendererCustomEditor<TEditButton>)
  public
    type
      TButtonClickCallback = procedure (ANode : PVirtualNode; AColumn :
        TColumnIndex; AItemType : Integer; var AValue : Pointer) of object;  
  public
    constructor Create (AText : String; AItemType : Integer; 
      AButtonClickCallback : TButtonClickCallback; ACallback : 
      TValueChangeCallback);

    function GetEditorValue : Pointer; override;
    procedure CreateEditor; override;
  protected
    FText : String;
    FValue : Pointer;
    FButtonCallback : TButtonClickCallback;
    
    procedure ButtonClick (ASender : TObject);
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

{ TRendererCustomEditor }

constructor TRendererCustomEditor.Create (AItemType : Integer; ACallback : 
  TValueChangeCallback);
begin
  FItemType := AItemType;
  FCallback := ACallback;
end;

destructor TRendererCustomEditor.Destroy;
begin
  FreeAndNil(FEdit);
  inherited Destroy;
end;

procedure TRendererCustomEditor.EditKeyDown (ASender : TObject; var AKey : Word; 
  AShift : TShiftState);
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

function TRendererCustomEditor.BeginEdit : Boolean; stdcall;
begin
  FEdit.Show;
  FEdit.SetFocus;
  Result := True;
end;

function TRendererCustomEditor.CancelEdit : Boolean; stdcall;
begin
  FEdit.Hide;
  Result := True;
end;

function TRendererCustomEditor.EndEdit : Boolean; stdcall;
begin
  if Assigned(FCallback) then
  begin
    FCallback(FNode, FColumn, FItemType, GetEditorValue);
  end;
  FEdit.Hide;
  FTree.SetFocus;
  Result := True;
end;

function TRendererCustomEditor.GetBounds : TRect; stdcall;
begin
  Result := FEdit.BoundsRect;
end;

function TRendererCustomEditor.PrepareEdit (ATree : TBaseVirtualTree; ANode :
  PVirtualNode; AColumn : TColumnIndex) : Boolean; stdcall;
begin
  FTree := TVirtualDrawTree(ATree);
  FNode := ANode;
  FColumn := AColumn;

  FreeAndNil(FEdit);
  CreateEditor;
  FEdit.OnKeyDown := @EditKeyDown;

  Result := True;
end;

procedure TRendererCustomEditor.ProcessMessage (var AMessage : TLMessage); stdcall;
begin
  FEdit.WindowProc(AMessage);
end;

procedure TRendererCustomEditor.SetBounds (ARect : TRect); stdcall;
var
  Dummy : Integer;
begin
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, ARect.Right);
  FEdit.BoundsRect := ARect;
end;

{ TEditEditor }

constructor TEditEditor.Create (AValue : String; AItemType : Integer;
  ACallback : TValueChangeCallback);
begin
  inherited Create(AItemType, ACallback);
  FValue := AValue;
end;

procedure TEditEditor.CreateEditor;
begin
  FEdit := TEdit.Create(nil);
  FEdit.AutoSize := False; 
  FEdit.Visible := False;
  FEdit.Parent := FTree;
  FEdit.Text := FValue;
end;

function TEditEditor.GetEditorValue : Pointer;
begin
  FValue := FEdit.Text;
  Result := @FValue;
end;

{ TSpinEditEditor }

constructor TSpinEditEditor.Create (AValue : Integer; AItemType : Integer;
  ACallback : TValueChangeCallback);
begin
  inherited Create(AItemType, ACallback);
  FValue := AValue;
end;

procedure TSpinEditEditor.CreateEditor;
begin
  FEdit := TSpinEdit.Create(nil);
  FEdit.AutoSize := False;
  FEdit.Visible := False;
  FEdit.Parent := FTree;
  FEdit.Value := FValue;
  FEdit.MinValue := 0;
  FEdit.MaxValue := MAXINT - 1;
end;

function TSpinEditEditor.GetEditorValue : Pointer;
begin
  FValue := FEdit.Value;
  Result := @FValue;
end;

{ TColorEdit }

constructor TColorEditor.Create (AValue : TColor; AItemType : Integer;
  ACallback : TValueChangeCallback);
begin
  inherited Create(AItemType, ACallback);
  FValue := AValue;
end;


procedure TColorEditor.CreateEditor;
begin
  FEdit := TColorBox.Create(nil);
  FEdit.AutoSize := False;
  FEdit.Visible := False;
  FEdit.Parent := FTree;
  FEdit.Selected := FValue;
  FEdit.Style := [cbCustomColor, cbPrettyNames, cbStandardColors];
end;

function TColorEditor.GetEditorValue : Pointer;
begin
  FValue := FEdit.Selected;
  Result := @FValue;
end;

{ TListEditor }

constructor TListEditor.Create (ASelected : Integer; AItemType : Integer;
  ACallback : TValueChangeCallback);
begin
  inherited Create(AItemType, ACallback);
  FSelected := ASelected;
  FItems := TStringList.Create;
end;

destructor TListEditor.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TListEditor.AppendItem (AItem : String);
begin
  FItems.Add(AItem);
end;


function TListEditor.GetEditorValue : Pointer;
begin
  FSelected := FEdit.ItemIndex;
  Result := @FSelected;
end;

procedure TListEditor.CreateEditor;
begin
  FEdit := TComboBox.Create(nil);
  FEdit.AutoSize := False;
  FEdit.Visible := False;
  FEdit.Parent := FTree;
  FEdit.Items := FItems;
  FEdit.ItemIndex := FSelected;
  FEdit.Style := csDropDownList;
end;

{ TCheckEditor }

constructor TCheckEditor.Create (AChecked : Boolean; ACaption : String; 
  AItemType : Integer; ACallback : TValueChangeCallback);
begin
  inherited Create(AItemType, ACallback);
  FChecked := AChecked;
  FCaption := ACaption;
end;

function TCheckEditor.GetEditorValue : Pointer;
begin
  FChecked := FEdit.Checked;
  Result := @FChecked;
end;

procedure TCheckEditor.CreateEditor;
begin
  FEdit := TCheckBox.Create(nil);
  FEdit.AutoSize := False;
  FEdit.Visible := False;
  FEdit.Parent := FTree;
  FEdit.Caption := FCaption;
  FEdit.Checked := FChecked;
end;

{ TEditButtonEditor }

constructor TEditButtonEditor.Create (AText : String; AItemType : Integer; 
  AButtonClickCallback : TButtonClickCallback; ACallback : 
  TValueChangeCallback);
begin
  inherited Create(AItemType, ACallback);
  FText := AText;
  FButtonCallback := AButtonClickCallback;
end;

procedure TEditButtonEditor.ButtonClick (ASender : TObject);
begin
  FButtonCallback(FNode, FColumn, FItemType, FValue);
end;

function TEditButtonEditor.GetEditorValue : Pointer;
begin
  Result := FValue;
end;

procedure TEditButtonEditor.CreateEditor;
begin
  FEdit := TEditButton.Create(nil);
  FEdit.AutoSize := False;
  FEdit.Visible := False;
  FEdit.Parent := FTree;
  FEdit.Text := FText;
  FEdit.ButtonCaption := '...';
  FEdit.OnButtonClick := @ButtonClick;
end;

end.
