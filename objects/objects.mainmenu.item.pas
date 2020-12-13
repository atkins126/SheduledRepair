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
  SysUtils, objects.common, sqlite3.schema, eventproviders.common;

type
  TMainMenuItem = class(TCommonObject)
  private
    const
      MAIN_MENU_TABLE_NAME = 'mainmenu';
  public
    type
      TItemType = (
        MENU_ITEM_TYPE_LOGO,
        MENU_ITEM_TYPE_ITEM,
        MENU_ITEM_TYPE_SUBITEM
      );
  public
    constructor Create (AID : Int64; AItemType : TItemType; ATitle : String;
      ACanSelected : Boolean = True; AEventProvider : TCommonEventProvider = nil);
      reintroduce;
    destructor Destroy; override; 

    { Get object database table name. }
    function Table : String; override;

    { Object deep copy. }
    procedure Assign (AMainMenuItem : TMainMenuItem);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var {%H-}ASchema : TSQLite3Schema); override; 

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;

    { Store current object to database. }
    procedure SaveCurrentObject; override;
  protected
    FItemType : TItemType;
    FTitle : String;
    FAttachedObject : TCommonObject;
    FAttachedObjectName : String;
    FCanSelected : Boolean;
    FEventProvider : TCommonEventProvider;
  public
    property ItemType : TItemType read FItemType;
    property Title : String read FTitle;
    property CanSelected : Boolean read FCanSelected;
    
    property AttachedObject : TCommonObject read FAttachedObject 
      write FAttachedObject;
    property AttachedObjectName : String read FAttachedObjectName
      write FAttachedObjectName;

    property EventProvider : TCommonEventProvider read FEventProvider;
  end;

implementation

{ TMainMenuItem }

constructor TMainMenuItem.Create (AID : Int64; AItemType : TItemType; ATitle : 
  String; ACanSelected : Boolean; AEventProvider : TCommonEventProvider);
begin
  inherited Create(AID);
  FItemType := AItemType;
  FTitle := ATitle;
  FAttachedObject := nil;
  FAttachedObjectName := '';
  FCanSelected := ACanSelected;
  FEventProvider := AEventProvider;
end;

destructor TMainMenuItem.Destroy;
begin
  inherited Destroy;
end;

function TMainMenuItem.Table : String;
begin
  Result := MAIN_MENU_TABLE_NAME;
end;

procedure TMainMenuItem.Assign (AMainMenuItem : TMainMenuItem);
begin
  FItemType := AMainMenuItem.ItemType;
end;

procedure TMainMenuItem.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  { Do nothing. }
end;

function TMainMenuItem.LoadCurrentObject : Boolean;
begin
  Result := True;  
end;

procedure TMainMenuItem.SaveCurrentObject;
begin
  { Do nothing. }
end;

end.
