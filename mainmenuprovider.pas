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
  SysUtils, Classes, VirtualTrees, objects.common, objects.mainmenu.item,
  dataproviders.common, renderers.mainmenu, dataproviders.mainmenu, 
  profilesprovider.common, profilesprovider.mainmenu, renderers.datarenderer, 
  container.arraylist, container.list, utils.functor, utils.pair;

type
  TMainMenu = class
  public
    const
      MAIN_MENU_ITEM_LOGO                                             = 0;
      MAIN_MENU_ITEM_JOB                                              = 1;
      MAIN_MENU_ITEM_EQUIPMENT                                        = 2;
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
    procedure AttachObject (AMenuItemID : Int64; AName : String; AObject : 
      TCommonObject);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Detach menu item element object. }
    procedure DetachObject (AMenuItemID : Int64);
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    type
      { Dynamic menu data provider and profiles provider. }
      TDynamicMenuData = class(specialize TPair<TCommonDataProvider, 
        TCommonProfilesProvider>);
      TDynamicMenuDataCompareFunctor = class
        (specialize TUnsortableFunctor<TDynamicMenuData>);

      { Dynamic menus list container. }  
      TDynamicMenusList = class(specialize TList<TDynamicMenuData, 
        TDynamicMenuDataCompareFunctor>);
      TDynamicMenusListCompareFunctor = class 
        (specialize TUnsortableFunctor<TDynamicMenusList>);

      { Dynamic menus array lists. }
      TDynamicMenus = class(specialize TArrayList<TDynamicMenusList,
        TDynamicMenusListCompareFunctor>);
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
    FDynamicMenus : TDynamicMenus;

    procedure SetMainMenuView (AMainMenuView : TVirtualDrawTree);
  public
    { Get dynamic menus list iterator for menu element. }
    function GetAttachedMenus (AMenuItemID : Int64) : TIterator;
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    property View : TVirtualDrawTree read FMainMenuView 
      write SetMainMenuView;
  end;

var
  MainMenu : TMainMenu = nil;

implementation

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
  FreeAndNil(FDynamicMenus);
  inherited Destroy;
end;

procedure TMainMenu.SetMainMenuView (AMainMenuView : TVirtualDrawTree);
var
  MenuItem : TCommonObject;
begin
  if AMainMenuView = nil then
    Exit;

  FMainMenuView := AMainMenuView;
  FMainMenuDataProvider := TMainMenuDataProvider.Create;
  FMainMenuDataProvider.Load;

  FDynamicMenus.Clear;
  for MenuItem in FMainMenuDataProvider do
  begin
    FDynamicMenus.Append(TDynamicMenusList.Create);
  end;

  FMainMenuRenderer := TMainMenuDataRenderer.Create(
    TDataRenderer.Create(FMainMenuView, FMainMenuDataProvider, 
    TMainMenuProfilesProvider.Create, TMainMenuRenderer.Create)
  );
  FMainMenuRenderer.UpdateData;
end;

procedure TMainMenu.AttachDynamicMenu (AMenuItemID : Int64; ADataProvider : 
  TCommonDataProvider; AProfilesProvider : TCommonProfilesProvider);
begin
  if (not ADataProvider.Load) or (not AProfilesProvider.Load) then
    Exit;

  FDynamicMenus.Value[AMenuItemID].Append(TDynamicMenuData.Create(
    ADataProvider, AProfilesProvider));
end;

procedure TMainMenu.DetachAllDynamicMenus (AMenuItemID : Int64);
begin
  FDynamicMenus.Value[AMenuItemID].Clear;
end;

function TMainMenu.GetAttachedMenus (AMenuItemID : Int64) : TIterator;
begin
  if AMenuItemID = -1 then
    Exit(TIterator.Create(nil));

  Result := TIterator.Create(FDynamicMenus.Value[AMenuItemID].FirstEntry);
end;

procedure TMainMenu.AttachObject (AMenuItemID : Int64; AName : String; AObject : 
  TCommonObject);
begin
  with TMainMenuItem(FMainMenuDataProvider.GetObject(AMenuItemID)) do
  begin
    AttachedObjectName := AName;
    AttachedObject := AObject;
  end;  
  FMainMenuView.Refresh;
end;

procedure TMainMenu.DetachObject (AMenuItemID : Int64);
begin
  with TMainMenuItem(FMainMenuDataProvider.GetObject(AMenuItemID)) do
  begin
    AttachedObjectName := '';
    AttachedObject := nil;
  end;
  FMainMenuView.Refresh;
end;

initialization
  MainMenu := TMainMenu.Create;
finalization
  FreeAndNil(MainMenu);
end.
