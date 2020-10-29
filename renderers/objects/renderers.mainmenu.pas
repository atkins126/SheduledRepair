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
unit renderers.profile.inspector;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, renderers.virtualtreeview, renderer.profile.profile,
  renderer.profile.profileitem, objects.mainmenu.item, dataproviders.mainmenu;

type
  TMainMenuRenderer = class
    (specialize TVirtualTreeViewRenderer<Cardinal>)
  public
    constructor Create (ATreeView : TVirtualDrawTree; ADataProvider : 
      TMainMenuDataProvider);
    destructor Destroy; override;

  private
    FDataProvider : TMainMenuDataProvider;
  end;

implementation

{ TMainMenuRenderer }

constructor TMainMenuRenderer.Create (ATreeView : TVirtualDrawTree;
  ADataProvider : TMainMenuDataProvider);
begin
  inherited Create(ATreeView, []);
  FDataProvider := ADataProvider;
end;

destructor TMainMenuRenderer.Destroy;
begin
  inherited Destroy;
end;

end.