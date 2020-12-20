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
unit dataproviders.grease;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, dataproviders.common, objects.common, objects.greasebag,
  objects.greasebundle, objects.greasableobject, objects.grease;

type
  TGreaseDataProvider = class(TCommonDataProvider)
  public
    constructor Create (AGreasableObject : TGreasableObject); reintroduce;

    { Load objects. }
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  private
    FGreasableObject : TGreasableObject;
  end;

implementation

{ TGreaseDataProvider }

constructor TGreaseDataProvider.Create (AGreasableObject : TGreasableObject);
begin
  inherited Create;
  FGreasableObject := AGreasableObject;
end;

function TGreaseDataProvider.Load : Boolean;
var
  GreaseBundle : TGreaseBundle;
begin
  if not Assigned(FGreasableObject) then
    Exit(inherited Load);

  Clear;

  for GreaseBundle in FGreasableObject.GreaseBag do
    Append(GreaseBundle);

  Result := True;
end;

function TGreaseDataProvider.LoadObjectsTableName : String;
var
  Grease : TGrease;
begin
  Grease := TGrease.Create(-1);
  Result := Grease.Table;
  FreeAndNil(Grease);
end;

function TGreaseDataProvider.LoadConcreteObject (AID : Int64) : TCommonObject;
var
  Grease : TGrease;
begin
  Grease := TGrease.Create(AID);
  if not Grease.Load then
  begin
    FreeAndNil(Grease);
    Exit(nil);
  end;

  Result := Grease;
end;

end.
