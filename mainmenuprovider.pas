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
  SysUtils, Classes, VirtualTrees, objects.common, objects.namedobject,
  dataproviders.common, renderers.mainmenu, dataproviders.mainmenu,
  profilesprovider.common, profilesprovider.mainmenu, renderers.datarenderer,
  eventproviders.mainmenu, container.arraylist, container.list, utils.functor,
  utils.pair;

type
  TMainMenu = class
  public
    const
      MAIN_MENU_ITEM_LOGO                                             = 0;
      MAIN_MENU_ITEM_JOB                                              = 1;
      MAIN_MENU_ITEM_EQUIPMENT                                        = 2;
      MAIN_MENU_ITEM_ENTITY                                           = 3;
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

    { Attach object to menu item element. }
    procedure AttachObject (AMenuItemID : Int64; AObject : TNamedObject);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Detach menu item element object. }
    procedure DetachObject (AMenuItemID : Int64);
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    type
      { Dynamic menu data provider and profiles provider. }
      TDynamicMenuData = class(specialize TPair<TCommonDataProvider, 
        TCommonProfilesProvider>);
      
      { Dynamic menus list container. }  
      TDynamicMenuDataCompareFunctor = class
        (specialize TUnsortableFunctor<TDynamicMenuData>);
      TDynamicMenusList = class(specialize TList<TDynamicMenuData, 
        TDynamicMenuDataCompareFunctor>);
      
      { Dynamic menus array lists. }
      TDynamicMenusListCompareFunctor = class 
        (specialize TUnsortableFunctor<TDynamicMenusList>);
      TDynamicMenus = class(specialize TArrayList<TDynamicMenusList,
        TDynamicMenusListCompareFunctor>);

      { Menu items handle list. }
      TMenuItemListCompareFunctor = class
        (specialize TUnsortableFunctor<TDataRenderer.TItemHandle>);
      TMenuItemList = class(specialize TArrayList<TDataRenderer.TItemHandle,
        TMenuItemListCompareFunctor>);
  public
    type
      TIterator = class
      protected
        {%H-}constructor Create (AIterator : TDynamicMenusList.TIterator);

        { Get DataProvider. }
        function GetDataProvider : TCommonDataProvider;

        { Get ProfilesProfilesProvider. }
        function GetProfilesProvider : TCommonProfilesProvider;

        { Return current item iterator and move it to next element. }
        function GetCurrent : TIterator;
      protected
        FIterator : TDynamicMenusList.TIterator;
      public
        { Return true if iterator has correct value. }
        function HasValue : Boolean;

        { Retrive the next entry. }
        function Next : TIterator;

        { Return true if we can move to next element. }
        function MoveNext : Boolean;

        { Return enumerator for in operator. }
        function GetEnumerator : TIterator;
      public
        property DataProvider : TCommonDataProvider read GetDataProvider;
        property ProfilesProvider : TCommonProfilesProvider 
          read GetProfilesProvider;

        property Current : TIterator read GetCurrent;
      end;
  private
    FMainMenuView : TVirtualDrawTree;  
    FMainMenuRenderer : TMainMenuDataRenderer;
    FMainMenuDataProvider : TMainMenuDataProvider;
    FMenuItems : TMenuItemList;
    FDynamicMenus : TDynamicMenus;

    procedure SetMainMenuView (AMainMenuView : TVirtualDrawTree);
    procedure MenuItemCreateEvent (AObject : TCommonObject; AItemHandle :
      TDataRenderer.TItemHandle);
  public
    property View : TVirtualDrawTree read FMainMenuView 
      write SetMainMenuView;
  end;

var
  MainMenu : TMainMenu = nil;

implementation

uses 
  objects.mainmenu.item;

{ TMainMenu.TIterator }

constructor TMainMenu.TIterator.Create(AIterator : TDynamicMenusList.TIterator);
begin
  FIterator := AIterator;
end;

function TMainMenu.TIterator.GetDataProvider : TCommonDataProvider;
begin
  Result := FIterator.Value.First;
end;

function TMainMenu.TIterator.GetProfilesProvider : TCommonProfilesProvider;
begin
  Result := FIterator.Value.Second;
end;

function TMainMenu.TIterator.GetCurrent : TIterator;
begin
  Result := TIterator.Create(FIterator);
  FIterator := FIterator.Next;
end;

function TMainMenu.TIterator.HasValue : Boolean;
begin
  Result := (FIterator <> nil) and (FIterator.HasValue);
end;

function TMainMenu.TIterator.Next : TIterator;
begin
  Result := TIterator.Create(FIterator.Next);
end;

function TMainMenu.TIterator.MoveNext : Boolean;
begin
  Result := (FIterator <> nil) and FIterator.MoveNext;
end;

function TMainMenu.TIterator.GetEnumerator : TIterator;
begin
  Result := TIterator.Create(FIterator);
end;

{ TMainMenu }

constructor TMainMenu.Create;
begin
  if not Assigned(MainMenu) then
  begin
    FMainMenuView := nil;
    FMainMenuRenderer := nil;
    FMainMenuDataProvider := nil;
    FMenuItems := TMenuItemList.Create;
    FDynamicMenus := TDynamicMenus.Create;

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
  FreeAndNil(FDynamicMenus);
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
  FMainMenuRenderer.ReloadData(@MenuItemCreateEvent);
end;

procedure TMainMenu.MenuItemCreateEvent (AObject : TCommonObject; AItemHandle :
  TDataRenderer.TItemHandle);
var
  Index : Integer;
begin
  if AObject.ID > (FMenuItems.Length - 1) then
  begin
    for index := FMenuItems.Length to AObject.ID do
    begin
      FMenuItems.Append(nil);
    end;
  end;

  FMenuItems.Value[AObject.ID] := AItemHandle;
end;

procedure TMainMenu.AttachDynamicMenu (AMenuItemID : Int64; ADataProvider : 
  TCommonDataProvider; AProfilesProvider : TCommonProfilesProvider);
begin
  if AMenuItemID > (FMenuItems.Length - 1) then
    Exit;
    
  FMainMenuRenderer.AttachDynamicMenu(FMenuItems.Value[AMenuItemID],
    ADataProvider, AProfilesProvider);
end;

procedure TMainMenu.DetachAllDynamicMenus (AMenuItemID : Int64);
begin
  if AMenuItemID > (FMenuItems.Length - 1) then
    Exit;

  FMainMenuRenderer.DetachAllDynamicMenus(FMenuItems.Value[AMenuItemID]);
end;
}
procedure TMainMenu.AttachObject (AMenuItemID : Int64; AObject : TNamedObject);
begin
  with TMainMenuItem(FMainMenuDataProvider.GetObject(AMenuItemID)) do
  begin
    AttachedObject := AObject;
  end;
  FMainMenuView.Refresh;
end;

procedure TMainMenu.DetachObject (AMenuItemID : Int64);
begin
  with TMainMenuItem(FMainMenuDataProvider.GetObject(AMenuItemID)) do
  begin
    AttachedObject := nil;
  end;
  FMainMenuView.Refresh;
end;

initialization
  MainMenu := TMainMenu.Create;
finalization
  FreeAndNil(MainMenu);
end.
