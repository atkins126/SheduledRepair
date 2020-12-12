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
unit dataproviders.entity;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, dataproviders.common, objects.common, objects.equipment, 
  objects.entitybag, objects.entity;

type
  TEntityDataProvider = class(TCommonDataProvider)
  public
    constructor Create (AEquipment : TEquipment); reintroduce;

    { Load objects. }
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  private
    FEquipment : TEquipment;
  end;

implementation

{ TEntityDataProvider }

constructor TEntityDataProvider.Create (AEquipment : TEquipment);
begin
  inherited Create;
  FEquipment := AEquipment;
end;

function TEntityDataProvider.Load : Boolean;
var
  Entity : TEntity;
begin
  if not Assigned(FEquipment) then
    Exit(inherited Load);

  Clear;

  for Entity in FEquipment.EntityBag do
    Append(Entity);

  Result := True;
end;

function TEntityDataProvider.LoadObjectsTableName : String;
var
  Entity : TEntity;
begin
  Entity := TEntity.Create(-1);
  Result := Entity.Table;
  FreeAndNil(Entity);
end;

function TEntityDataProvider.LoadConcreteObject (AID : Int64) : TCommonObject;
var
  Entity : TEntity;
begin
  Entity := TEntity.Create(AID);
  if not Entity.Load then
  begin
    FreeAndNil(Entity);
    Exit(nil);
  end;

  Result := Entity;
end;

end.
