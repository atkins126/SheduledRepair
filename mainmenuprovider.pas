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
unit mainmenuprovider;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, VirtualTrees, renderers.mainmenu, dataproviders.mainmenu,
  profilesprovider.mainmenu, renderers.datarenderer;

type
  TMainMenu = class
  public
    constructor Create;
    destructor Destroy; override;
  private
    FMainMenuView : TVirtualDrawTree;  
    FMainMenuRenderer : TMainMenuDataRenderer;

    procedure SetMainMenuView (AMainMenuView : TVirtualDrawTree);
  public
    property View : TVirtualDrawTree read FMainMenuView 
      write SetMainMenuView;
  end;

var
  MainMenu : TMainMenu = nil;

implementation

{ TMainMenu }

constructor TMainMenu.Create;
begin
  if not Assigned(MainMenu) then
  begin
    FMainMenuView := nil;
    FMainMenuRenderer := nil;

    MainMenu := self;
  end else
  begin
    self := MainMenu;
  end;
end;

destructor TMainMenu.Destroy;
begin
  FreeAndNil(FMainMenuRenderer);
  inherited Destroy;
end;

procedure TMainMenu.SetMainMenuView (AMainMenuView : TVirtualDrawTree);
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

initialization
  MainMenu := TMainMenu.Create;
finalization
  FreeAndNil(MainMenu);
end.
