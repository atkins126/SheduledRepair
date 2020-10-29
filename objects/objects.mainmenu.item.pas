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
unit objects.mainmenu.item;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common;

type
  TMainMenuItem = class(TCommonObject)
  public
    type
      TItemType = (
        MENU_ITEM_LOGO,
        MENU_ITEM
      );
  public
    constructor Create (AID : Int64; AItemType : TItemType); override;
    destructor Destroy; override; 

    function CheckSchema : Boolean; override;
    function Table : String; override;
    function Load : Boolean; override;
    function Save : Boolean; override;
    function Delete : Boolean; override; 
  protected
    FItemType : TItemType;
  public
    property ItemType : TItemType read FItemType;
  end;

implementation

{ TMainMenuItem }

constructor TMainMenuItem.Create (AID : Int64; AItemType : TItemType);
begin
  inherited Create(AID);
  FItemType := AItemType;
end;

destructor TMainMenuItem.Destroy;
begin
  inherited Destroy;
end;

function TMainMenuItem.CheckSchema : Boolean;
begin
  Result := True;
end;

function TMainMenuItem.Table : String;
begin
  Result := 'main_menu';
end;

function TMainMenuItem.Load : Boolean;
begin
  Result := True;
end;

function TMainMenuItem.Save : Boolean;
begin
  Result := True;
end;

function TMainMenuItem.Delete : Boolean;
begin
  Result := True;
end;

end.