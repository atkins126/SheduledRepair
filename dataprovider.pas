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
unit dataprovider;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, VirtualTrees, Forms, objects.common, 
  renderers.datarenderer, datahandlers;

type
  TDataProvider = class
  public
    constructor Create;
    destructor Destroy; override;
  private
    FParent : TCustomForm;
    FDataHandler : TDataHandler;

    FDataView : TVirtualDrawTree;
    FDataRenderer : TDataRenderer;
  public
    { Change data types. }
    procedure ChangeData (ADataHandler : TDataHandler);

    { Update renderer data. }
    procedure UpdateData;

    function GetSelectedObject : TCommonObject;

    { Show editor. }
    procedure ShowEditor (AObject : TCommonObject);

    property Parent : TCustomForm read FParent write FParent;
    property DataView : TVirtualDrawTree read FDataView write FDataView;
  end;

var
  Provider : TDataProvider = nil;

implementation

{ TDataProvider }

constructor TDataProvider.Create;
begin
  if not Assigned(Provider) then
  begin
    FDataView := nil;
    FDataRenderer := nil;

    Provider := self;
  end else
    self := Provider;
end;

destructor TDataProvider.Destroy;
begin
  FreeAndNil(FDataRenderer);
  inherited Destroy;
end;

procedure TDataProvider.ChangeData (ADataHandler : TDataHandler);
begin
  if not Assigned(FDataView) then
    Exit;

  FDataHandler := ADataHandler;

  FreeAndNil(FDataRenderer);
  FDataRenderer := ADataHandler.CreateDataRenderer(FDataView);
  FDataRenderer.UpdateData;
end;

procedure TDataProvider.UpdateData;
begin
  FDataRenderer.UpdateData;
end;

procedure TDataProvider.ShowEditor (AObject : TCommonObject);
begin
  if (not Assigned(FDataHandler)) or (not Assigned(FParent)) then
    Exit;

  FDataHandler.ShowEditor(FParent, AObject);
end;

function TDataProvider.GetSelectedObject : TCommonObject;
begin
  Result := FDataRenderer.GetSelectedObject;
end;

initialization
  Provider := TDataProvider.Create;
finalization
  FreeAndNil(Provider);
end.
