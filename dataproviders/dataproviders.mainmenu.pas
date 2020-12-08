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
  SysUtils, dataproviders.common, objects.common, objects.mainmenu.item;

type
  TMainMenuDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  private
    procedure EquipmentCallback;
    procedure JobCallback;
  end;

implementation

uses
  dataprovider, datahandlers, mainmenuprovider;

{ TMainMenuDataProvider }

function TMainMenuDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(TMainMenu.MENU_ITEM_LOGO, MENU_ITEM_LOGO, 
    'SheduledRepair', nil));
  Append(TMainMenuItem.Create(TMainMenu.MENU_ITEM_JOB, MENU_ITEM, 
    'Job', @JobCallback));
  Append(TMainMenuItem.Create(TMainMenu.MENU_ITEM_EQUIPMENT, MENU_ITEM, 
    'Equipment', @EquipmentCallback));

  Result := True;
end;

function TMainMenuDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMainMenuDataProvider.LoadConcreteObject (AID : Int64) : TCommonObject;
begin
  Result := nil;
end;

procedure TMainMenuDataProvider.EquipmentCallback;
begin
  MainMenu.Clear;
  
  Provider.ChangeData(TEquipmentDataHandler.Create);
end;

procedure TMainMenuDataProvider.JobCallback;
begin
  MainMenu.Clear;

  Provider.ChangeData(TJobDataHandler.Create);
end;

end.
