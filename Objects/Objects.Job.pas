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
unit Objects.Job;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  sqlite3.builder, Objects.Entity, Objects.Period, Objects.Shedule;

type
  TJob = class
  public
    type
      TEntityBagItem = record
        Entity : TEntity;
      end;

      TEntityBagItemCompareFunctor = class
        (specialize TBinaryFunctor<TEntityBagItem, Integer>)
      public
        function Call (AValue1, AValue2 : TEntityBagItem) : Integer; override;
      end;

      TEntityBagItemList = class
        (specialize TList<TEntityBagItem, TEntityBagItemCompareFunctor>);
  public
    constructor Create;
    destructor Destroy; override;
  private
    FName : String;
    FPeriod : TPeriod;
    FShedule : TShedule;
    FEntitiesList : TEntityBagItemList;
  public
    property Name : String read FName;
    property Period : TPeriod read FPeriod;
    property Shedule : TShedule read FShedule;
    property EntitiesList : TEntityBagItemList read FEntitiesList;   
  end;

implementation

{ TJob.TNodeBagItemCompareFunctor }

function TJob.TEntityBagItemCompareFunctor.Call (AValue1, AValue2 :
  TNodeBagItem) : Integer;
begin
  if AValue1.Entity.Name < AValue2.Entity.Name then
    Result := -1
  else if AValue2.Entity.Name < AValue1.Entity.Name then
    Result := 1
  else
    Result := 0;
end;

{ TJob }

constructor TJob.Create;
begin
  FEntitiesList := TEntityBagItemList.Create;
end;

destructor TJob.Destroy;
begin
  FreeAndNil(FEntitiesList);
  inherited Destroy;
end;

end.