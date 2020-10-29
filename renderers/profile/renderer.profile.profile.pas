(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(*                                                                            *)
(* Copyright (c) 2020                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpassqlite                ivan@semenkov.pro *)
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
unit renderer.profile.profile;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, Graphics, container.arraylist, utils.functor, 
  sqlite3.schema, sqlite3.result, sqlite3.result_row, 
  renderer.profile.profileitem, sqlite3.table, database;

type
  TRendererProfile = class(TCommonObject)
  private
    const
      RENDERER_PROFILE_TABLE_NAME = 'renderer_profile';
  public
    type
      TItemCompareFunctor = class
        (specialize TBinaryFunctor<TRendererProfileItem, Integer>)
      public
        function Call (AValue1, AValue2 : TRendererProfileItem) : Integer; 
          override;
      end;

      TItemsList = class
        (specialize TArrayList<TRendererProfileItem, TItemCompareFunctor>);
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Check database table scheme. }
    function CheckSchema : Boolean; override;

    { Get object database table name. }
    function Table : String; override;

    { Load object from database. }
    function Load : Boolean; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Delete object from database. }
    function Delete : Boolean; override;
  protected
    FEnable : Boolean;
    FHeight : Integer;
    FBackground : TColor;
    FItemsList : TItemsList;

    function GetItem (AName : String) : TRendererProfileItem;
    procedure SetItem (AName : String; AValue : TRendererProfileItem);
  public
    { Get enumerator for in operator. }
    function GetEnumerator : TItemsList.TIterator;
  public
    property Enable : Boolean read FEnable write FEnable;
    property Height : Integer read FHeight write FHeight;
    property Background : TColor read FBackground write FBackground;
    property Items[Name : String] : TRendererProfileItem read GetItem
      write SetItem;
  end;

implementation

{ TRendererProfile.TItemCompareFunctor }

function TRendererProfile.TItemCompareFunctor.Call (AValue1, AValue2 :
  TRendererProfileItem) : Integer;
begin
  if AValue1.Name < AValue2.Name then
    Result := -1
  else if AValue2.Name < AValue1.Name then
    Result := 1
  else
    Result := 1;
end;

constructor TRendererProfile.Create (AID : Int64);
begin
  inherited Create(AID);
  FEnable := False;
  FHeight := 20;
  FBackground := clWhite;
  FItemsList := TItemsList.Create;
end;

destructor TRendererProfile.Destroy;
begin
  FreeAndNil(FItemsList);
  inherited Destroy;
end;

function TRendererProfile.GetItem (AName : String) : TRendererProfileItem;
var
  item : TRendererProfileItem;
begin
  for item in FItemsList do
  begin
    if item.Name = AName then
      Exit(item);
  end;  

  FItemsList.Append(TRendererProfileItem.Create(-1));
  FItemsList.LastEntry.Value.Name := AName;
  Result := FItemsList.LastEntry.Value;
end;

procedure TRendererProfile.SetItem (AName : String; AValue :
  TRendererProfileItem);
var
  item : TRendererProfileItem;
begin
  for item in FItemsList do
  begin
    if item.Name = AName then
    begin
      item.Background := AValue.Background;
      item.BackgroundFillType := AValue.BackgroundFillType;
      item.BackgroundRoundRadius := AValue.BackgroundRoundRadius;
      item.FontName := AValue.FontName;
      item.FontSize := AValue.FontSize;
      item.FontColor := AValue.FontColor;
      item.Padding := AValue.Padding;
      item.PositionType := AValue.PositionType;
      item.Position := AValue.Position;
      Exit;
    end;  
  end;

  FItemsList.Append(AValue);
  FItemsList.LastEntry.Value.Name := AName;
end;

function TRendererProfile.GetEnumerator : TItemsList.TIterator;
begin
  Result := FItemsList.GetEnumerator;
end;

function TRendererProfile.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
  Item : TRendererProfileItem;
begin
  Schema := TSQLite3Schema.Create;
  Item := TRendererProfileItem.Create(-1);
  
  Schema
    .Id
    .Integer('enable').NotNull
    .Integer('height').NotNull
    .Integer('background').NotNull;

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema) and Item.CheckSchema;  

  FreeAndNil(Item);
  FreeAndNil(Schema);
end;

function TRendererProfile.Table : String;
begin
  Result := RENDERER_PROFILE_TABLE_NAME;
end;

function TRendererProfile.Load : Boolean;
var
  result_row : TSQLite3Result;
  item_row : TSQLite3ResultRow;
  row : TSQLite3Result.TRowIterator;
  item : TRendererProfileItem;
  items_table : TSQLite3Table;
begin
  if ID = -1 then
    Exit(False);

  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  FEnable := Boolean(row.Row.GetIntegerValue('enable'));
  FHeight := row.Row.GetIntegerValue('height');
  FBackground := TColor(row.Row.GetIntegerValue('backgorund'));

  item := TRendererProfileItem.Create(-1);
  items_table := TSQLite3Table.Create(DB.Errors, DB.Handle, item.Table);
  result_row := items_table.Select.Field('id').Where('profile_id', ID).Get;
  FItemsList.Clear;

  for item_row in result_row do
  begin
    item := TRendererProfileItem.Create(item_row.GetIntegerValue('id'));

    if item.Load then
      FItemsList.Append(item);    
  end;

  Result := True;
end;

function TRendererProfile.Save : Boolean;
var
  item : TRendererProfileItem;
begin
  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('enable', Integer(FEnable))
      .Update('height', FHeight)
      .Update('background', FBackground).Get > 0);
  end else
  begin
    Result := (InsertRow.Value('height', FHeight)
      .Value('background', FBackground).Get > 0);
      UpdateObjectID;
  end;

  if not FItemsList.FirstEntry.HasValue then
    Exit;

  for item in FItemsList do
  begin
    item.RendererProfile := Self;
    item.Save;
  end;
end;

function TRendererProfile.Delete : Boolean;
begin
  if ID <> -1 then
    Result := (DeleteRow.Get > 0)
  else
    Result := False;
end;

end.
