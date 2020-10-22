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
unit dataprovider;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, dataproviders.measure;

type
  TDataProvider = class
  public
    constructor Create;
    destructor Destroy; override;
  private
    FMeasure : TMeasureDataProvider;

    procedure LoadDataProviders;
  public
    property Measure : TMeasureDataProvider read FMeasure;
  end;

var
  Provider : TDataProvider = nil;

implementation

{ TDataProvider }

constructor TDataProvider.Create;
begin
  if not Assigned(Provider) then
  begin
    FMeasure := TMeasureDataProvider.Create;

    LoadDataProviders;
    Provider := self;
  end else
    self := Provider;
end;

destructor TDataProvider.Destroy;
begin
  inherited Destroy;
end;

procedure TDataProvider.LoadDataProviders;
begin
  FMeasure.Load;
end;

initialization
  Provider := TDataProvider.Create;
finalization
  FreeAndNil(Provider);
end.
