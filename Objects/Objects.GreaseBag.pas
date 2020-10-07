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
unit Objects.GreaseBag;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, sqlite3.builder, container.list, utils.functor, 
  Objects.Grease, Objects.Quantity;

type
  TGreaseBag = class
  public
    type
      TGreaseBagItem = record
        Grease : TGrease;
        Quantity : TQuantity;
      end;

      TGreaseBagItemCompareFunctor = class
        (specialize TBinaryFunctor<TGreaseBagItem, Integer>)
      public
        function Call (AValue1, AValue2 : TGreaseBagItem) : Integer; override;
      end;

      TGreaseBagItemList = class
        (specialize TList<TGreaseBagItem, TGreaseBagItemCompareFunctor>);
  public
    constructor Create;
    destructor Destroy; override;
  private
    FGreasesList : TGreaseBagItemList;
  public
    property GreasesList : TGreaseBagItemList read FGreasesList;
  end;

implementation

{ TGreaseBag.TGreaseBagItemCompareFunctor }

function TGreaseBag.TGreaseBagItemCompareFunctor.Call (AValue1, AValue2 :
  TGreaseBagItem) : Integer;
begin
  if AValue1.Quantity.Measure.Name = AValue2.Quantity.Measure.Name then
  begin
    if AValue1.Quantity.Measure.Count < AValue2.Quantity.Measure.Count then
      Result := -1
    else if AValue2.Quantity.Measure.Count < AValue1.Quantity.Measure.Count then
      Result := 1
    else
      Result := 0;
  end else
    Result := 0;
end;

{ TGreaseBag }

constructor TGreaseBag.Create;
begin
  FGreasesList := TGreaseBagItemList.Create;
end;

destructor TGreaseBag.Destroy;
begin
  FreeAndNil(FGreasesList);
  inherited Destroy;
end;

end.