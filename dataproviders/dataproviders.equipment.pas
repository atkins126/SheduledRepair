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
unit dataproviders.equipment;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, dataproviders.common, objects.common, objects.equipment;

type
  TEquipmentDataProvider = class(TCommonDataProvider)
  public
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject (AID : Int64) : TCommonObject; override;

    procedure ObjectDoubleClick (AObjectIndex : Cardinal); override;
  end;

implementation

uses
  dataprovider, datahandlers, mainmenuprovider;

{ TEquipmentDataProvider }

function TEquipmentDataProvider.LoadObjectsTableName : String;
var
  Equipment : TEquipment;
begin
  Equipment := TEquipment.Create(-1);
  Result := Equipment.Table;
  FreeAndNil(Equipment);
end;

function TEquipmentDataProvider.LoadConcreteObject (AID : Int64) :
  TCommonObject;
var
  Equipment : TEquipment;
begin
  Equipment := TEquipment.Create(AID);
  if not Equipment.Load then
  begin
    FreeAndNil(Equipment);
    Exit(nil);
  end;

  Result := Equipment;
end;

procedure TEquipmentDataProvider.ObjectDoubleClick (AObjectIndex : Cardinal);
begin
  MainMenu.Clear;
  

  Provider.ChangeData(TEquipmentEntityDataHandler.Create(
    TEquipment(GetObject(AObjectIndex))));
end;

end.
