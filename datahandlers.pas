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

{$mode objfpc}{$H+}
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
  eventproviders.entity, jobform, equipmentform, entityform, renderers.node,
  eventproviders.node;

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
  JobEditor : TJobWindow;
  ModalResult : Integer;
begin
  { JobEditor window is temporaryly, onlu for work testing and it will been 
    changed after refactor. }
  JobEditor := TJobWindow.Create(AParent, TJob(AObject));
  ModalResult := JobEditor.ShowModal;
  
  if ModalResult = mrOk then
    JobEditor.GetObject.Save;

  if ModalResult <> mrCancel then
    Provider.ReloadData;

  FreeAndNil(JobEditor);
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
  EquipmentEditor : TEquipmentWindow;
  ModalResult : Integer;
begin
  { EquipmentEditor window is temporaryly, onlu for work testing and it will
    been changed after refactor. }
  EquipmentEditor := TEquipmentWindow.Create(AParent, TEquipment(AObject));
  ModalResult := EquipmentEditor.ShowModal;

  if ModalResult = mrOk then
    EquipmentEditor.GetObject.Save;

  if ModalResult <> mrCancel then
    Provider.ReloadData;

  FreeAndNil(EquipmentEditor);
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
  EntityEditor : TEntityWindow;
  Entity : TEntity;
  ModalResult : Integer;
begin
  { EquipmentEditor window is temporaryly, onlu for work testing and it will
    been changed after refactor. }
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
  EntityEditor : TEntityWindow;
  Entity : TEntity;
  ModalResult : Integer;
begin
  { EquipmentEditor window is temporaryly, onlu for work testing and it will
    been changed after refactor. }
  {
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
  }
end;

end.
