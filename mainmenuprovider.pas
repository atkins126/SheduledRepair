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

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, VirtualTrees, objects.common, objects.namedobject,
  dataproviders.common, renderers.mainmenu, dataproviders.mainmenu,
  profilesprovider.common, profilesprovider.mainmenu, renderers.datarenderer,
  eventproviders.mainmenu, container.arraylist, utils.functor;

type
  TMainMenu = class
  public
    const
      MAIN_MENU_ITEM_LOGO                                             = 0;
      MAIN_MENU_ITEM_JOB                                              = 1;
      MAIN_MENU_ITEM_EQUIPMENT                                        = 2;
      MAIN_MENU_ITEM_ENTITY                                           = 3;
      MAIN_MENU_ITEM_NODE                                             = 4;
      MAIN_MENU_ITEM_ENTITY_GREASE                                    = 5;
      MAIN_MENU_ITEM_NODE_GREASE                                      = 6;
  public
    constructor Create;
    destructor Destroy; override;

    { Attach additional dynamic menu to element. }
    procedure AttachDynamicMenu (AMenuItemID : Int64; ADataProvider : 
      TCommonDataProvider; AProfilesProvider : TCommonProfilesProvider);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Detach all additional dynamic menus from menu element. }
    procedure DetachAllDynamicMenus (AMenuItemID : Int64);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Select menu item by ID. }
    procedure SelectMenuItem (AMenuItemID : Int64);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return selected menu item ID or -1. }
    function GetSelectedMenuItem : Int64;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Attach object to menu item element. }
    procedure AttachObject (AMenuItemID : Int64; AObject : TNamedObject);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Detach menu item element object. }
    procedure DetachObject (AMenuItemID : Int64);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Get attached object. }
    function GetAttachedObject (AMenuItemID : Int64) : TNamedObject;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    type
      { Menu items handle list. }
      TMenuItemListCompareFunctor = class
        ({$IFDEF FPC}specialize{$ENDIF}
        TUnsortableFunctor<TDataRenderer.TItemHandle>);
      TMenuItemList = {$IFDEF FPC}type specialize{$ENDIF}
        TArrayList<TDataRenderer.TItemHandle, TMenuItemListCompareFunctor>;
  private
    FMainMenuView : TVirtualDrawTree;  
    FMainMenuRenderer : TMainMenuDataRenderer;
    FMainMenuDataProvider : TMainMenuDataProvider;
    FMenuItems : TMenuItemList;

    procedure SetMainMenuView (AMainMenuView : TVirtualDrawTree);
    
    procedure MenuItemCreateEvent (AObject : TCommonObject; AItemHandle :
      TDataRenderer.TItemHandle);
    procedure MenuItemDestroyEvent (AObject : TCommonObject);
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
    FMainMenuDataProvider := nil;
    FMenuItems := TMenuItemList.Create;

    MainMenu := self;
  end else
  begin
    self := MainMenu;
  end;
end;

destructor TMainMenu.Destroy;
begin
  FreeAndNil(FMainMenuRenderer);
  FreeAndNil(FMenuItems);
  inherited Destroy;
end;

procedure TMainMenu.SetMainMenuView (AMainMenuView : TVirtualDrawTree);
begin
  if AMainMenuView = nil then
    Exit;

  FMainMenuView := AMainMenuView;
  FMainMenuDataProvider := TMainMenuDataProvider.Create;
  FMainMenuDataProvider.Load;

  FMainMenuRenderer := TMainMenuDataRenderer.Create(
    TDataRenderer.Create(FMainMenuView, FMainMenuDataProvider, 
    TMainMenuProfilesProvider.Create, TMainMenuRenderer.Create,
    TMainMenuEventProvider.Create));
  FMainMenuRenderer.ReloadData({$IFDEF FPC}@{$ENDIF}MenuItemCreateEvent);
end;

procedure TMainMenu.MenuItemCreateEvent (AObject : TCommonObject; AItemHandle :
  TDataRenderer.TItemHandle);
var
  Index : Integer;
begin
  if AObject.ID = -1 then
    Exit;

  if AObject.ID > (FMenuItems.Length - 1) then
  begin
    for index := FMenuItems.Length to AObject.ID do
    begin
      FMenuItems.Append(nil);
    end;
  end;

  FMenuItems.Value[AObject.ID] := AItemHandle;
end;

procedure TMainMenu.MenuItemDestroyEvent (AObject : TCommonObject);
begin
  if (AObject.ID = -1) or (AObject.ID > (FMenuItems.Length - 1)) then
    Exit;

  FMenuItems.Value[AObject.ID] := nil;
end;

procedure TMainMenu.AttachDynamicMenu (AMenuItemID : Int64; ADataProvider : 
  TCommonDataProvider; AProfilesProvider : TCommonProfilesProvider);
begin
  if AMenuItemID > (FMenuItems.Length - 1) then
    Exit;
    
  FMainMenuRenderer.AttachDynamicMenu(FMenuItems.Value[AMenuItemID],
    ADataProvider, AProfilesProvider, {$IFDEF FPC}@{$ENDIF}MenuItemCreateEvent);
end;

procedure TMainMenu.DetachAllDynamicMenus (AMenuItemID : Int64);
begin
  if AMenuItemID > (FMenuItems.Length - 1) then
    Exit;

  FMainMenuRenderer.DetachAllDynamicMenus(FMenuItems.Value[AMenuItemID]);
end;

procedure TMainMenu.SelectMenuItem (AMenuItemID : Int64);
begin
  if AMenuItemID > (FMenuItems.Length - 1) then
    Exit;

  FMainMenuRenderer.SelectMenuItem(FMenuItems.Value[AMenuItemID]);
end;

function TMainMenu.GetSelectedMenuItem : Int64;
var
  MenuItemHandle : TDataRenderer.TItemHandle;
  MenuItemIterator : TMenuItemList.TIterator;
  Index : Integer;
begin
  MenuItemHandle := FMainMenuRenderer.GetSelectedMenuItem;

  if (not Assigned(MenuItemHandle)) or (not FMenuItems.FirstEntry.HasValue) then
  begin
    Exit(-1);  
  end;

  MenuItemIterator := FMenuItems.FirstEntry;
  Index := 0;

  while MenuItemIterator.HasValue do
  begin
    if MenuItemIterator.Value = MenuItemHandle then
      Exit(Index);
    
    Inc(index);
    MenuItemIterator := MenuItemIterator.Next;
  end;

  Result := -1;
end;

procedure TMainMenu.AttachObject (AMenuItemID : Int64; AObject : TNamedObject);
begin
  if AMenuItemID > (FMenuItems.Length - 1) then
    Exit;

  FMainMenuRenderer.AttachObject(FMenuItems.Value[AMenuItemID], AObject);
end;

procedure TMainMenu.DetachObject (AMenuItemID : Int64);
begin
  if AMenuItemID > (FMenuItems.Length - 1) then
    Exit;

  FMainMenuRenderer.DetachObject(FMenuItems.Value[AMenuItemID]);
end;

function TMainMenu.GetAttachedObject (AMenuItemID : Int64) : TNamedObject;
begin
  if AMenuItemID > (FMenuItems.Length - 1) then
    Exit;

  Result := FMainMenuRenderer.GetAttachedObject(FMenuItems.Value[AMenuItemID]);
end;

initialization
  MainMenu := TMainMenu.Create;
finalization
  FreeAndNil(MainMenu);
end.
