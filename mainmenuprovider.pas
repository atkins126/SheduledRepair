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
  profilesprovider.mainmenu, renderers.datarenderer, container.arraylist, 
  utils.functor, utils.pair;

type
  TMainMenu = class
  public
    const
      MENU_ITEM_LOGO                                                  = 0;
      MENU_ITEM_JOB                                                   = 1;
      MENU_ITEM_EQUIPMENT                                             = 2;
  public
    constructor Create;
    destructor Destroy; override;
  private
    type
      TMenuItemProvider = class(specialize TPair<Int64, TCommonDataProvider>);

      TMenuItemCompareFunctor = class
        (specialize TBinaryFunctor<TMenuItemProvider, Integer>)
      public
        function Call (AValue1, AValue2 : TMenuItemProvider) : Integer;
          override;
      end;

      TMenuItemProvidersList = class
        (specialize TArrayList<TMenuItemProvider, TMenuItemCompareFunctor>);
  public
    type
      { Menu item providers list iterator (array list filter iterator 
        decorator). }
      TIterator = class
      private
        {%H-}constructor Create (AIterator : TMenuItemProvidersList.TIterator;
          AItemID : Int64);
      public
        { Return true if iterator has value. }
        function HasValue : Boolean;

        { Retrive the next entry in a list. }
        function Next : TIterator;

        { Return true if we can move to next element. }
        function MoveNext : Boolean;

        { Return enumerator for in operator. }
        function GetEnumerator : TIterator;  
      protected  
        { Get item value. }
        function GetValue : TCommonDataProvider;

        { Return current item iterator and move it to next. }
        function GetCurrent : TCommonDataProvider;
      public
        property Value : TCommonDataProvider read GetValue;
        property Current : TCommonDataProvider read GetCurrent;
      private
        FItemID : Int64;
        FIterator : TMenuItemProvidersList.TIterator;    
      end;
  private
    FMainMenuView : TVirtualDrawTree;  
    FMainMenuRenderer : TMainMenuDataRenderer;
    FMainMenuDataProvider : TMainMenuDataProvider;
    FMenuItems : TMenuItemProvidersList;

    procedure SetMainMenuView (AMainMenuView : TVirtualDrawTree);
  public
    { Append additional menu elements. }
    procedure Append (AItemID : Int64; ADataProvider : TCommonDataProvider);

    { Remove menu elements. }
    procedure Remove (AItemID : Int64);

    { Clear menu item elements. }
    procedure Clear;

    { Get menu element sub items. }
    function ItemData (AItemID : Int64) : TIterator;

    procedure SelectObject (AObjectID : Int64; AObjectName : String; 
      AObject : TCommonObject);
    
    property View : TVirtualDrawTree read FMainMenuView 
      write SetMainMenuView;
  end;

var
  MainMenu : TMainMenu = nil;

implementation

{ TMainMenu.TMenuItemCompareFunctor }

function TMainMenu.TMenuItemCompareFunctor.Call (AValue1, AValue2 : 
  TMenuItemProvider) : Integer;
begin
  if AValue1.First < AValue2.First then
    Result := -1
  else if AValue2.First < AValue1.First then
    Result := 1
  else
    Result := 0;
end;

{ TMainMenu.TIterator }

constructor TMainMenu.TIterator.Create (AIterator : 
  TMenuItemProvidersList.TIterator; AItemID : Int64);
begin
  FItemID := AItemID;
  FIterator := AIterator;
end;

function TMainMenu.TIterator.HasValue : Boolean;
begin
  Result := FIterator.HasValue;
end;

function TMainMenu.TIterator.Next : TIterator;
var
  Iterator : TMenuItemProvidersList.TIterator;
begin
  Iterator := FIterator;
  while Iterator.HasValue do
  begin
    if Iterator.Value.First = FItemID then
      Exit(TIterator.Create(Iterator, FItemID));

    Iterator := Iterator.Next;
  end;

  Result := TIterator.Create(Iterator, FItemID);
end;

function TMainMenu.TIterator.MoveNext : Boolean;
var
  Iterator : TMenuItemProvidersList.TIterator;
begin
  Iterator := FIterator;
  while Iterator.HasValue do
  begin
    if Iterator.Value.First = FItemID then
      Exit(True);

    Iterator := Iterator.Next;
  end;

  Result := False;
end;

function TMainMenu.TIterator.GetEnumerator : TIterator;
begin
  Result := TIterator.Create(FIterator, FItemID);
end;

function TMainMenu.TIterator.GetValue : TCommonDataProvider;
begin
  Result := FIterator.Value.Second;
end;

function TMainMenu.TIterator.GetCurrent : TCommonDataProvider;
begin
  Result := GetValue;
  FIterator := FIterator.Next;
end;

{ TMainMenu }

constructor TMainMenu.Create;
begin
  if not Assigned(MainMenu) then
  begin
    FMainMenuView := nil;
    FMainMenuRenderer := nil;
    FMainMenuDataProvider := nil;
    FMenuItems := TMenuItemProvidersList.Create;

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
  FMainMenuRenderer := TMainMenuDataRenderer.Create(
    TDataRenderer.Create(FMainMenuView, FMainMenuDataProvider, 
    TMainMenuProfilesProvider.Create, TMainMenuRenderer.Create)
  );
  FMainMenuRenderer.UpdateData;
end;

procedure TMainMenu.Append (AItemID : Int64; ADataProvider : 
  TCommonDataProvider);
begin
  FMenuItems.Append(TMenuItemProvider.Create(AItemID, ADataProvider));
end;

procedure TMainMenu.Remove (AItemID : Int64);
begin
  FMenuItems.Remove(FMenuItems.IndexOf(TMenuItemProvider.Create(AItemID, nil)));
end;

procedure TMainMenu.Clear;
begin
  FMenuItems.Clear;
end;

function TMainMenu.ItemData (AItemID : int64) : TIterator;
var
  Iterator : TMenuItemProvidersList.TIterator;
begin
  Iterator := FMenuItems.FirstEntry;
  while Iterator.HasValue do
  begin
    if Iterator.Value.First = AItemID then
      Exit(TIterator.Create(Iterator, AItemID));
  end;

  Result := TIterator.Create(Iterator, AItemID);
end;

procedure TMainMenu.SelectObject (AObjectID : Int64; AObjectName : String;
  AObject : TCommonObject);
begin
  TMainMenuItem(FMainMenuDataProvider.GetObject(AObjectID)).SelectedObjectName 
    := AObjectName;
  TMainMenuItem(FMainMenuDataProvider.GetObject(AObjectID)).SelectedObject :=
    AObject;
  FMainMenuRenderer.RedrawSelection;
end;

initialization
  MainMenu := TMainMenu.Create;
finalization
  FreeAndNil(MainMenu);
end.
