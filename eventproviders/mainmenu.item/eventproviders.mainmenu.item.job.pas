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
unit eventproviders.mainmenu.item.job;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, eventproviders.common, objects.common;

type
  TMainMenuItemJobEventProvider = class(TCommonEventProvider)
  public
    constructor Create; override;
  private
    procedure JobClickEvent ({%H-}AObject : TCommonObject);
    procedure JobAttachDynamicMenuEvent ({%H-}AObject : TCommonObject);
    procedure JobDetachDynamicMenuEvent ({%H-}AObject : TCommonObject);
  end;

implementation

uses
  dataprovider, datahandlers, mainmenuprovider, profilesprovider.mainmenu,
  dataproviders.mainmenu;

{ TMainMenuItemJobEventProvider }

constructor TMainMenuItemJobEventProvider.Create;
begin
  inherited Create;
  
  Register(EVENT_OBJECT_CLICK, @JobClickEvent);
  Register(EVENT_OBJECT_ATTACH_DYNAMIC_MENU, @JobAttachDynamicMenuEvent);
  Register(EVENT_OBJECT_DETACH_DYNAMIC_MENU, @JobDetachDynamicMenuEvent);
end;

procedure TMainMenuItemJobEventProvider.JobClickEvent (AObject : 
  TCommonObject);
begin
  Provider.ChangeData(TJobDataHandler.Create);
end;

procedure TMainMenuItemJobEventProvider.JobAttachDynamicMenuEvent (AObject :
  TCommonObject);
begin
  MainMenu.AttachDynamicMenu(TMainMenu.MAIN_MENU_ITEM_JOB,
    TMenuSubitemJobDataProvider.Create, TMenuSubitemJobProfilesProvider.Create);
end;

procedure TMainMenuItemJobEventProvider.JobDetachDynamicMenuEvent (AObject :
  TCommonObject);
begin
  MainMenu.DetachAllDynamicMenus(TMainMenu.MAIN_MENU_ITEM_JOB);
end;

end.
