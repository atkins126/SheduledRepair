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
unit dataproviders.supplier;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, dataproviders.common, objects.common, objects.supplier;

type
  TSupplierDataProvider = class(TCommonDataProvider)
  public
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject (AID : Int64) : TCommonObject; override;
  end;

implementation

{ TSupplierDataProvider }

function TSupplierDataProvider.LoadObjectsTableName : String;
var
  Supplier : TSupplier;
begin
  Supplier := TSupplier.Create(-1);
  Result := Supplier.Table;
  FreeAndNil(Supplier);
end;

function TSupplierDataProvider.LoadConcreteObject (AID : Int64) :
  TCommonObject;
var
  Supplier : TSupplier;
begin
  Supplier := TSupplier.Create(AID);
  if not Supplier.Load then
  begin
    FreeAndNil(Supplier);
    Exit(nil);
  end;

  Result := Supplier;
end;

end.
