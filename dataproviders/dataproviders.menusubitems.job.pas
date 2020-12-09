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
unit dataproviders.menusubitems.job;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, dataproviders.common, objects.common, objects.mainmenu.item;

type
  TMenuSubitemJobDataProvider = class(TCommonDataProvider)
  public
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Load concrete object. }
    function LoadConcreteObject ({%H-}AID : Int64) : TCommonObject; override;
  private
    
  end;

implementation

uses
  dataprovider, mainmenuprovider;

{ TMainMenuDataProvider }

function TMenuSubitemJobDataProvider.Load : Boolean;
begin
  Clear;
  
  Append(TMainMenuItem.Create(-1, MENU_SUBITEM, 'Create', nil));
  Append(TMainMenuItem.Create(-1, MENU_SUBITEM, 'Edit', nil));
  
  Result := True;
end;

function TMenuSubitemJobDataProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemJobDataProvider.LoadConcreteObject (AID : Int64) : 
  TCommonObject;
begin
  Result := nil;
end;

end.
