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
  SysUtils, objects.common, objects.namedobject, sqlite3.schema,
  eventproviders.common;

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
      ACanSelected : Boolean = True; AEventProvider : 
      TCommonEventProvider = nil); reintroduce;
    destructor Destroy; override; 

    { Get object database table name. }
    function Table : String; override;

    { Fire OnObjectClick event. }
    procedure OnObjectClick (AMenuItem : TMainMenuItem);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Fire OnObjectDoubleClick event. }
    procedure OnObjectDoubleClick (AMenuItem : TMainMenuItem);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Fire OnObjectSelect event. }
    procedure OnObjectSelect (AMenuItem : TMainMenuItem);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Fire OnObjectUnselect event. }
    procedure OnObjectUnselect (AMenuItem : TMainMenuItem);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Fire OnObjectAttachDynamicMenu event. }
    procedure OnObjectAttachDynamicMenu (AMenuItem : TMainMenuItem);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Fire OnObjectDetachDynamicMenu event. }
    procedure OnObjectDetachDynamicMenu (AMenuItem : TMainMenuItem);
      {$IFNDEF DEBUG}inline;{$ENDIF}
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
    FAttachedObject : TNamedObject;
    FCanSelected : Boolean;
    FEventProvider : TCommonEventProvider;
  public
    property ItemType : TItemType read FItemType;
    property Title : String read FTitle;
    property CanSelected : Boolean read FCanSelected;
    
    property AttachedObject : TNamedObject read FAttachedObject 
      write FAttachedObject;
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

procedure TMainMenuItem.OnObjectClick (AMenuItem : TMainMenuItem);
begin
  if (Assigned(FEventProvider)) and (Assigned(FEventProvider.OnObjectClick)) 
     then
    FEventProvider.OnObjectClick(AMenuItem);
end;
    
procedure TMainMenuItem.OnObjectDoubleClick (AMenuItem : TMainMenuItem);
begin
  if (Assigned(FEventProvider)) and 
     (Assigned(FEventProvider.OnObjectDoubleClick)) then
    FEventProvider.OnObjectDoubleClick(AMenuItem);
end;      
    
procedure TMainMenuItem.OnObjectSelect (AMenuItem : TMainMenuItem);
begin
  if (Assigned(FEventProvider)) and (Assigned(FEventProvider.OnObjectSelect))
     then
    FEventProvider.OnObjectSelect(AMenuItem);
end;

procedure TMainMenuItem.OnObjectUnselect (AMenuItem : TMainMenuItem);
begin
  if (Assigned(FEventProvider)) and (Assigned(FEventProvider.OnObjectUnselect))
     then
    FEventProvider.OnObjectUnselect(AMenuItem);
end;

procedure TMainMenuItem.OnObjectAttachDynamicMenu (AMenuItem : TMainMenuItem);
begin
  if (Assigned(FEventProvider)) and 
     (Assigned(FEventProvider.OnObjectAttachDynamicMenu)) then
    FEventProvider.OnObjectAttachDynamicMenu(AMenuItem);
end;

procedure TMainMenuItem.OnObjectDetachDynamicMenu (AMenuItem : TMainMenuItem);
begin
  if (Assigned(FEventProvider)) and 
     (Assigned(FEventProvider.OnObjectDetachDynamicMenu)) then
    FEventProvider.OnObjectDetachDynamicMenu(AMenuItem);
end;

end.
