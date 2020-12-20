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
unit datahandlers;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, VirtualTrees, Forms, Controls, objects.common, objects.equipment,
  objects.job, objects.entity, objects.node, renderers.datarenderer, 
  renderers.equipment, dataproviders.node, profilesprovider.node, 
  dataproviders.equipment, profilesprovider.equipment, eventproviders.equipment,
  renderers.entity, dataproviders.entity, profilesprovider.entity, 
  renderers.job, dataproviders.job, profilesprovider.job, eventproviders.job, 
  eventproviders.entity, renderers.node,
  eventproviders.node, dataproviders.grease,
  profilesprovider.greasebundle, renderers.greasebundle,
  eventproviders.entitygrease, objects.greasebundle,
  eventproviders.nodegrease{$IFDEF FPC},jobform, equipmentform, entityform,
  nodeform, greasebundleform{$ENDIF};

type
  TDataHandler = class
  public
    { Create data renderer for current data type. }
    function CreateDataRenderer (ADataView : TVirtualDrawTree) : TDataRenderer; 
      virtual; abstract;
    
    { Show current data type editor. }
    procedure ShowEditor (AParent : TCustomForm; AObject : TCommonObject); 
      virtual; abstract;
  end;

  TJobDataHandler = class(TDataHandler)
  public
    function CreateDataRenderer (ADataView : TVirtualDrawTree) : TDataRenderer; 
      override;
    procedure ShowEditor (AParent : TCustomForm; AObject : TCommonObject); 
      override;  
  end;

  TEquipmentDataHandler = class(TDataHandler)
  public
    function CreateDataRenderer (ADataView : TVirtualDrawTree) : TDataRenderer; 
      override;
    procedure ShowEditor (AParent : TCustomForm; AObject : TCommonObject); 
      override;
  end;  

  TEquipmentEntityDataHandler = class(TDataHandler)
  public
    constructor Create (AEquipment : TEquipment);
    function CreateDataRenderer (ADataView : TVirtualDrawTree) : TDataRenderer;
      override;
    procedure ShowEditor (AParent : TCustomForm; {%H-}AObject : TCommonObject); 
      override;
  private
    FEquipment : TEquipment;
  end;

  TEntityGreaseDataHandler = class(TDataHandler)
  public
    constructor Create (AEntity : TEntity);
    function CreateDataRenderer (ADataView : TVirtualDrawTree) : TDataRenderer;
      override;
    procedure ShowEditor (AParent : TCustomForm; {%H-}AObject : TCommonObject); 
      override;
  private
    FEntity : TEntity;
  end;

  TEntityNodeDataHandler = class(TDataHandler)
  public
    constructor Create (AEntity : TEntity);
    function CreateDataRenderer (ADataView : TVirtualDrawTree) : TDataRenderer;
      override;
    procedure ShowEditor (AParent : TCustomForm; {%H-}AObject : TCommonObject); 
      override;
  private
    FEntity : TEntity;
  end;

  TNodeGreaseDataHandler = class(TDataHandler)
  public
    constructor Create (ANode : TNode);
    function CreateDataRenderer (ADataView : TVirtualDrawTree) : TDataRenderer;
      override;
    procedure ShowEditor (AParent : TCustomForm; {%H-}AObject : TCommonObject); 
      override;
  private
    FNode : TNode;
  end;

implementation

uses
  dataprovider;

{ TJobDataHandler }

function TJobDataHandler.CreateDataRenderer (ADataView : TVirtualDrawTree) : 
  TDataRenderer;
begin
  Result := TDataRenderer.Create(ADataView, TJobDataProvider.Create,
    TJobProfilesProvider.Create, TJobRenderer.Create, TJobEventProvider.Create);
end;

procedure TJobDataHandler.ShowEditor (AParent : TCustomForm; AObject : 
  TCommonObject);
var
  {$IFDEF FPC}JobEditor : TJobWindow;{$ENDIF}
  ModalResult : Integer;
begin
  { JobEditor window is temporaryly, onlu for work testing and it will been 
    changed after refactor. }
  {$IFDEF FPC}
  JobEditor := TJobWindow.Create(AParent, TJob(AObject));
  ModalResult := JobEditor.ShowModal;

  if ModalResult = mrOk then
    JobEditor.GetObject.Save;

  if ModalResult <> mrCancel then
    Provider.ReloadData;

  FreeAndNil(JobEditor);
  {$ENDIF}
end;

{ TEquipmentDataHandler }

function TEquipmentDataHandler.CreateDataRenderer (ADataView : TVirtualDrawTree) 
  : TDataRenderer;
begin
  Result := TDataRenderer.Create(ADataView, TEquipmentDataProvider.Create,
    TEquipmentProfilesProvider.Create, TEquipmentRenderer.Create,
    TEquipmentEventProvider.Create);
end;

procedure TEquipmentDataHandler.ShowEditor (AParent : TCustomForm; AObject : 
  TCommonObject);
var
  {$IFDEF FPC}EquipmentEditor : TEquipmentWindow;{$ENDIF}
  ModalResult : Integer;
begin
  { EquipmentEditor window is temporaryly, onlu for work testing and it will
    been changed after refactor. }
  {$IFDEF FPC}
  EquipmentEditor := TEquipmentWindow.Create(AParent, TEquipment(AObject));
  ModalResult := EquipmentEditor.ShowModal;

  if ModalResult = mrOk then
    EquipmentEditor.GetObject.Save;

  if ModalResult <> mrCancel then
    Provider.ReloadData;

  FreeAndNil(EquipmentEditor);
  {$ENDIF}
end;

{ TEquipmentEntityDataHandler }

constructor TEquipmentEntityDataHandler.Create (AEquipment : TEquipment);
begin
  FEquipment := AEquipment;
end;

function TEquipmentEntityDataHandler.CreateDataRenderer (ADataView :
  TVirtualDrawTree) : TDataRenderer;
begin
  Result := TDataRenderer.Create(ADataView, 
    TEntityDataProvider.Create(FEquipment), TEntityProfilesProvider.Create,
    TEntityRenderer.Create, TEntityEventProvider.Create);
end;

procedure TEquipmentEntityDataHandler.ShowEditor (AParent : TCustomForm; 
  AObject : TCommonObject);
var
  {$IFDEF FPC}EntityEditor : TEntityWindow;{$ENDIF}
  Entity : TEntity;
  ModalResult : Integer;
begin
  { EntityEditor window is temporaryly, onlu for work testing and it will
    been changed after refactor. }
  {$IFDEF FPC}
  EntityEditor := TEntityWindow.Create(AParent, FEquipment, TEntity(AObject));
  ModalResult := EntityEditor.ShowModal;

  if ModalResult = mrOk then
  begin
    Entity := EntityEditor.GetObject;

    if Entity.ID = -1 then
    begin
      Entity.Save;
      FEquipment.EntityBag.Append(Entity);
    end else
    begin
      Entity.Save;
    end;

    FEquipment.Save;
  end;

  if ModalResult <> mrCancel then
    Provider.ReloadData;

  FreeAndNil(EntityEditor);
  {$ENDIF}
end;

{ TEntityNodeDataHandler }

constructor TEntityNodeDataHandler.Create (AEntity : TEntity);
begin
  FEntity := AEntity;
end;

function TEntityNodeDataHandler.CreateDataRenderer (ADataView :
  TVirtualDrawTree) : TDataRenderer;
begin
  Result := TDataRenderer.Create(ADataView, 
    TNodeDataProvider.Create(FEntity), TNodeProfilesProvider.Create,
    TNodeRenderer.Create, TNodeEventProvider.Create);
end;

procedure TEntityNodeDataHandler.ShowEditor (AParent : TCustomForm; 
  AObject : TCommonObject);
var
  {$IFDEF FPC}NodeEditor : TNodeWindow;{$ENDIF}
  Node : TNode;
  ModalResult : Integer;
begin
  { NodeEditor window is temporaryly, onlu for work testing and it will
    been changed after refactor. }
  {$IFDEF FPC}
  NodeEditor := TNodeWindow.Create(AParent, FEntity, TNode(AObject));
  ModalResult := NodeEditor.ShowModal;

  if ModalResult = mrOk then
  begin
    Node := NodeEditor.GetObject;

    if Node.ID = -1 then
    begin
      Node.Save;
      FEntity.NodeBag.Append(Node);
    end else
    begin
      Node.Save;
    end;

    FEntity.Save;
  end;

  if ModalResult <> mrCancel then
    Provider.ReloadData;

  FreeAndNil(NodeEditor);
  {$ENDIF}
end;

{ TEntityGreaseDataHandler }

constructor TEntityGreaseDataHandler.Create (AEntity : TEntity);
begin
  FEntity := AEntity;
end;

function TEntityGreaseDataHandler.CreateDataRenderer (ADataView :
  TVirtualDrawTree) : TDataRenderer;
begin
  Result := TDataRenderer.Create(ADataView, 
    TGreaseDataProvider.Create(FEntity), TGreaseBundleProfilesProvider.Create,
    TGreaseBundleRenderer.Create, TEntityGreaseEventProvider.Create);
end;

procedure TEntityGreaseDataHandler.ShowEditor (AParent : TCustomForm; 
  AObject : TCommonObject);
var
  {$IFDEF FPC}GreaseBundleEditor : TGreaseBundleWindow;{$ENDIF}
  GreaseBundle : TGreaseBundle;
  ModalResult : Integer;
begin
  { GreaseBundleEditor window is temporaryly, onlu for work testing and it will
    been changed after refactor. }
  {$IFDEF FPC}
  GreaseBundleEditor := TGreaseBundleWindow.Create(AParent, FEntity,
    TGreaseBundle(AObject));
  ModalResult := GreaseBundleEditor.ShowModal;

  if ModalResult = mrOk then
  begin
    GreaseBundle := GreaseBundleEditor.GetObject;

    if GreaseBundle.ID = -1 then
    begin
      GreaseBundle.Save;
      FEntity.GreaseBag.Append(GreaseBundle);
    end else
    begin
      GreaseBundle.Save;
    end;

    FEntity.Save;
  end;

  if ModalResult <> mrCancel then
    Provider.ReloadData;

  FreeAndNil(GreaseBundleEditor);
  {$ENDIF}
end;

{ TNodeGreaseDataHandler }

constructor TNodeGreaseDataHandler.Create (ANode : TNode);
begin
  FNode := ANode;
end;

function TNodeGreaseDataHandler.CreateDataRenderer (ADataView :
  TVirtualDrawTree) : TDataRenderer;
begin
  Result := TDataRenderer.Create(ADataView, 
    TGreaseDataProvider.Create(FNode), TGreaseBundleProfilesProvider.Create,
    TGreaseBundleRenderer.Create, TNodeGreaseEventProvider.Create);
end;

procedure TNodeGreaseDataHandler.ShowEditor (AParent : TCustomForm; 
  AObject : TCommonObject);
var
  {$IFDEF FPC}GreaseBundleEditor : TGreaseBundleWindow;{$ENDIF}
  GreaseBundle : TGreaseBundle;
  ModalResult : Integer;
begin
  { GreaseBundleEditor window is temporaryly, onlu for work testing and it will
    been changed after refactor. }
  {$IFDEF FPC}
  GreaseBundleEditor := TGreaseBundleWindow.Create(AParent, FNode,
    TGreaseBundle(AObject));
  ModalResult := GreaseBundleEditor.ShowModal;

  if ModalResult = mrOk then
  begin
    GreaseBundle := GreaseBundleEditor.GetObject;

    if GreaseBundle.ID = -1 then
    begin
      GreaseBundle.Save;
      FNode.GreaseBag.Append(GreaseBundle);
    end else
    begin
      GreaseBundle.Save;
    end;

    FNode.Save;
  end;

  if ModalResult <> mrCancel then
    Provider.ReloadData;

  FreeAndNil(GreaseBundleEditor);
  {$ENDIF}
end;

end.
