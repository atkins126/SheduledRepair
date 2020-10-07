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
unit Objects.Entity;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  sqlite3.builder, Objects.GreaseBag, Objects.Quantity, Objects.Period;

type
  TEntity = class
  public
    type
      TNodeBagItem = record
        Node : TNode;
      end;

      TNodeBagItemCompareFunctor = class
        (specialize TBinaryFunctor<TNodeBagItem, Integer>)
      public
        function Call (AValue1, AValue2 : TNodeBagItem) : Integer; override;
      end;

      TNodeBagItemList = class
        (specialize TList<TNodeBagItem, TNodeBagItemCompareFunctor>);
  public
    constructor Create;
    destructor Destroy; override;  
  private
    FName : String;
    FGreaseBag : TGreaseBag;
    FQuantity : TQuantity;
    FPeriod : TPeriod;
    FNodesList : TNodeBagItemList;
  public
    property Name : String read FName;  
    property GreaseBag : TGreaseBag read FGreaseBag;
    property Quantity : TQuantity read FQuantity;
    property Period : TPeriod read FPeriod;
    property NodesList : TNodeBagItemList read FNodesList;
  end;

implementation

{ TEntity.TNodeBagItemCompareFunctor }

function TEntity.TNodeBagItemCompareFunctor.Call (AValue1, AValue2 :
  TNodeBagItem) : Integer;
begin
  if AValue1.Node.Name < AValue2.Node.Name then
    Result := -1
  else if AValue2.Node.Name < AValue1.Node.Name then
    Result := 1
  else
    Result := 0;
end;

{ TGreaseBag }

constructor TEntity.Create;
begin
  FNodesList := TNodeBagItemList.Create;
end;

destructor TEntity.Destroy;
begin
  FreeAndNil(FNodesList);
  inherited Destroy;
end;

end.