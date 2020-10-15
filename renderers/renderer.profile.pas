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
unit renderer.profile;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, Graphics, BGRABitmap, BGRABitmapTypes,
  container.arraylist, utils.functor, sqlite3.schema, sqlite3.result,
  sqlite3.result_row, renderer.profileitem, sqlite3.table, database;

type
  TRendererProfile = class(TCommonObject)
  private
    const
      RENDERER_PROFILE_TABLE_NAME = 'rendererprofile';
  public
    type
      TBorderType = (
        BORDER_NONE,
        BORDER_SQUARE,
        BORDER_SQUARE_ROUND_CORNER
      );

      TMargin = class
        Top, Left, Bottom, Right : Integer;
      end;

      TPadding = class
        Top, Left, Bottom, Right : Integer;
      end;

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
    FBorderType : TBorderType;
    FBorder : Integer;
    FBorderRadius : Integer;
    FBorderColor : TBGRAPixel;
    FBorderMargin : TMargin;
    FBackground : TBGRAPixel;
    FItemsList : TItemsList;

    function GetItem (AName : String) : TRendererProfileItem;
    procedure SetItem (AName : String; AValue : TRendererProfileItem);
  public
    property BorderType : TBorderType read FBorderType write FBorderType;
    property Border : Integer read FBorder write FBorder;
    property BorderRadius : Integer read FBorderRadius write FBorderRadius;
    property BorderColor : TBGRAPixel read FBorderColor write FBorderColor;
    property BorderMargin : TMargin read FBorderMargin write FBorderMargin;
    property Background : TBGRAPixel read FBackground write FBackground;
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
  FBorderType := BORDER_NONE;
  FBorder := 0;
  FBorderRadius := 0;
  FBorderColor := BGRA(0, 0, 0, 255);
  FBorderMargin := TMargin.Create;
  FBorderMargin.Top := 0;
  FBorderMargin.Left := 0;
  FBorderMargin.Bottom := 0;
  FBorderMargin.Right := 0;
  FBackground := BGRA(255, 255, 255, 255);
  FItemsList := TItemsList.Create;
end;

destructor TRendererProfile.Destroy;
begin
  FreeAndNil(FItemsList);
  FreeAndNil(FBorderMargin);
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

  FItemsList.Append(TRendererProfileItem.Create(-1));
  FItemsList.LastEntry.Value.Name := AName;
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
    .Integer('border_type').NotNull
    .Integer('border').NotNull
    .Integer('border_radius').NotNull
    .Text('border_color').NotNull
    .Integer('border_margin_top').NotNull
    .Integer('border_margin_left').NotNull
    .Integer('border_margin_bottom').NotNull
    .Integer('border_margin_right').NotNull
    .Text('background').NotNull;

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

  FBorderType := TBorderType(row.Row.GetIntegerValue('border_type'));
  FBorder := row.Row.GetIntegerValue('border');
  FBorderRadius := row.Row.GetIntegerValue('border_radius');
  FBorderColor := StrToBGRA(row.Row.GetStringValue('border_color'));
  FBorderMargin.Top := row.Row.GetIntegerValue('border_margin_top');
  FBorderMargin.Left := row.Row.GetIntegerValue('border_margin_left');
  FBorderMargin.Bottom := row.Row.GetIntegerValue('border_margin_bottom');
  FBorderMargin.Right := row.Row.GetIntegerValue('border_margin_right');
  FBackground := StrToBGRA(row.Row.GetStringValue('background'));

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
    Result := (UpdateRow.Update('border_type', Integer(FBorderType))
      .Update('border', FBorder).Update('border_radius', FBorderRadius)
      .Update('border_color', BGRAToStr(FBorderColor))
      .Update('border_margin_top', FBorderMargin.Top)
      .Update('border_margin_left', FBorderMargin.Left)
      .Update('border_margin_bottom', FBorderMargin.Bottom)
      .Update('border_margin_right', FBorderMargin.Right)
      .Update('background', BGRAToStr(FBackground)).Get > 0);
  end else
  begin
    Result := (InsertRow.Value('border_type', Integer(FBorderType))
      .Value('border', FBorder).Value('border_radius', FBorderRadius)
      .Value('border_color', BGRAToStr(FBorderColor))
      .Value('border_margin_top', FBorderMargin.Top)
      .Value('border_margin_left', FBorderMargin.Left)
      .Value('border_margin_bottom', FBorderMargin.Bottom)
      .Value('border_margin_right', FBorderMargin.Right)
      .Value('background', BGRAToStr(FBackground)).Get > 0);
      UpdateObjectID;
  end;

  if not FItemsList.FirstEntry.HasValue then
    Exit;

  for item in FItemsList do
  begin
    item.Profile := @Self;
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
