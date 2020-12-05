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
  SysUtils, Classes, VirtualTrees, renderers.mainmenu, dataproviders.mainmenu,
  profilesprovider.mainmenu, renderers.datarenderer, datahandlers;

type
  TDataProvider = class
  public
    constructor Create;
    destructor Destroy; override;
  private
    FMainMenuView : TVirtualDrawTree;
    FMainMenuRenderer : TMainMenuDataRenderer;

    FDataView : TVirtualDrawTree;
    FDataRenderer : TDataRenderer;
    
    procedure SetMainMenuView (AMainMenuView : TVirtualDrawTree);
  public
    { Change data types. }
    procedure ChangeData (ADataHandler : TDataHandler);

    property MainMenuView : TVirtualDrawTree read FMainMenuView 
      write SetMainMenuView;
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
    FMainMenuView := nil;
    FMainMenuRenderer := nil;

    FDataView := nil;
    FDataRenderer := nil;

    Provider := self;
  end else
    self := Provider;
end;

destructor TDataProvider.Destroy;
begin
  FreeAndNil(FMainMenuRenderer);
  FreeAndNil(FDataRenderer);
  inherited Destroy;
end;

procedure TDataProvider.SetMainMenuView (AMainMenuView : TVirtualDrawTree);
begin
  if AMainMenuView = nil then
    Exit;

  FMainMenuView := AMainMenuView;
  FMainMenuRenderer := TMainMenuDataRenderer.Create(
    TDataRenderer.Create(FMainMenuView, TMainMenuDataProvider.Create, 
    TMainMenuProfilesProvider.Create, TMainMenuRenderer.Create)
  );
  FMainMenuRenderer.UpdateData;
end;

procedure TDataProvider.ChangeData (ADataHandler : TDataHandler);
begin
  if not Assigned(FDataView) then
    Exit;

  FreeAndNil(FDataRenderer);
  FDataRenderer := ADataHandler.CreateDataRenderer(FDataView);
  FDataRenderer.UpdateData;
end;

initialization
  Provider := TDataProvider.Create;
finalization
  FreeAndNil(Provider);
end.
