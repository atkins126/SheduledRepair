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
unit dataproviders.grade;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, dataproviders.common, objects.common, objects.grade;

type
  TGradeDataProvider = class(TCommonDataProvider)
  public
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject (AID : Int64) : TCommonObject; override;
  end;

implementation

{ TGradeDataProvider }

function TGradeDataProvider.LoadObjectsTableName : String;
var
  Grade : TGrade;
begin
  Grade := TGrade.Create(-1);
  Result := Grade.Table;
  FreeAndNil(Grade);
end;

function TGradeDataProvider.LoadConcreteObject (AID : Int64) :
  TCommonObject;
var
  Grade : TGrade;
begin
  Grade := TGrade.Create(AID);
  if not Grade.Load then
  begin
    FreeAndNil(Grade);
    Exit(nil);
  end;

  Result := Grade;
end;

end.
