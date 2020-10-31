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
unit dataproviders.mainmenu;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.mainmenu.item;

type
  TMainMenuDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
    function CreateObject : TCommonObject; override;
  end;

implementation

{ TMainMenuDataProvider }

function TMainMenuDataProvider.Load : Boolean;
var
  MenuItem : TMainMenuItem;
begin
  MenuItem := TMainMenuItem.Create(0);  
  MenuItem.ItemType := MENU_ITEM_LOGO;  
  FObjectsList.Append(MenuItem);

  MenuItem := TMainMenuItem.Create(1);
  MenuItem.ItemType := MENU_ITEM;
  FObjectsList.Append(MenuItem);
end;

function TMainMenuDataProvider.CreateObject : TCommonObject;
begin
  Result := nil;
end;

end.