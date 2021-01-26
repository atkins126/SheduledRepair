(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(* This is a software for creating schedules  for repair work, accounting and *)
(* monitoring  their  implementation, accounting for the  necessary materials *)
(* and spare parts.                                                           *)
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
unit renderers.common;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Types, renderer.profile.profile, 
  objects.common, container.arraylist, utils.functor;

type
  TCommonRenderer = class
  public
    constructor Create;
    destructor Destroy; override;

    { Draw object using renderer profile. }
    procedure Draw (AObject : TCommonObject; AProfile : TRendererProfile;
      ACanvas : TCanvas; ARect : TRect); virtual; abstract;

    { Calculate columns. }
    procedure CalculateColumns (AFullWidth : Cardinal); virtual; abstract;
  protected
    type
      TColumnsList = {$IFDEF FPC}type specialize{$ENDIF} TArrayList<Cardinal,
        TCompareFunctorCardinal>;
  protected
    { Add new column. }
    procedure AppendColumn (AWidth : Cardinal);
  
    { Clear columns. }
    procedure Clear;
  protected
    FColumns : TColumnsList;
  public
    function GetEnumerator : TColumnsList.TIterator;
  end;

implementation

{ TCommonRenderer }

constructor TCommonRenderer.Create;
begin
  FColumns := TColumnsList.Create;
end;

destructor TCommonRenderer.Destroy;
begin
  FreeAndNil(FColumns);
end;

procedure TCommonRenderer.AppendColumn (AWidth : Cardinal);
begin
  FColumns.Append(AWidth);
end;

procedure TCommonRenderer.Clear;
begin
  FColumns.Clear;
end;

function TCommonRenderer.GetEnumerator : TColumnsList.TIterator;
begin
  Result := FColumns.GetEnumerator;
end;

end.
