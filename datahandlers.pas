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
  SysUtils, VirtualTrees, objects.equipment, 
  renderers.datarenderer, 
  renderers.equipment, dataproviders.equipment, profilesprovider.equipment,
  renderers.entity, dataproviders.entity, profilesprovider.entity, 
  renderers.job, dataproviders.job, profilesprovider.job;

type
  TDataHandler = class
  public
    function CreateDataRenderer (ADataView : TVirtualDrawTree) : TDataRenderer; 
      virtual; abstract;
  end;

  TJobDataHandler = class(TDataHandler)
  public
    function CreateDataRenderer (ADataView : TVirtualDrawTree) : TDataRenderer; 
      override;  
  end;

  TEquipmentDataHandler = class(TDataHandler)
  public
    function CreateDataRenderer (ADataView : TVirtualDrawTree) : TDataRenderer; 
      override;
  end;  

  TEquipmentEntityDataHandler = class(TDataHandler)
  public
    constructor Create (AEquipment : TEquipment);
    function CreateDataRenderer (ADataView : TVirtualDrawTree) : TDataRenderer;
      override;
  private
    FEquipment : TEquipment;
  end;

implementation

{ TJobDataHandler }

function TJobDataHandler.CreateDataRenderer (ADataView : TVirtualDrawTree) : 
  TDataRenderer;
begin
  Result := TDataRenderer.Create(ADataView, TJobDataProvider.Create,
    TJobProfilesProvider.Create, TJobRenderer.Create);
end;

{ TEquipmentDataHandler }

function TEquipmentDataHandler.CreateDataRenderer (ADataView : TVirtualDrawTree) 
  : TDataRenderer;
begin
  Result := TDataRenderer.Create(ADataView, TEquipmentDataProvider.Create,
    TEquipmentProfilesProvider.Create, TEquipmentRenderer.Create);
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
    TEntityRenderer.Create);
end;

end.
