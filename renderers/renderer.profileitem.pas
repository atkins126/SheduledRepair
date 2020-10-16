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
unit renderer.profileitem;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, Graphics, BGRABitmap, BGRABitmapTypes,
  sqlite3.schema, sqlite3.result, sqlite3.result_row;

type
  TRendererProfileItem = class(TCommonObject)
  private
    const
      RENDERER_PROFILE_ITEM_TABLE_NAME = 'renderer_profile_item';
  public
    type
      
      TPadding = class
        Top, Left, Bottom, Right : Integer;
      end;

      TBackgroundFillType = (
        FILL_NONE,
        FILL_SQUARE,
        FILL_SQUARE_ROUND_CORNER
      );

      TPositionType = (
        POSITION_FIXED,
        POSITION_FLOAT  
      );  
  
      TPosition = class
        Top, Left, Bottom, Right : Integer;
      end;
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
    FName : String;
    FRenderProfile : PCommonObject;
    FBackground : TBGRAPixel;
    FBackgroundFill : TBackgroundFillType;
    FBackgroundRoundRadius : Integer;
    FFontName : String;
    FFontSize : Integer;
    FFontColor : TBGRAPixel;
    FPadding : TPadding;
    FPositionType : TPositionType;
    FPosition : TPosition;
  public
    property Name : String read FName write FName;
    property Background : TBGRAPixel read FBackground write FBackground;
    property BackgroundFillType : TBackgroundFillType read FBackgroundFill
      write FBackgroundFill;
    property BackgroundRoundRadius : Integer read FBackgroundRoundRadius
      write FBackgroundRoundRadius;
    property FontName : String read FFontName write FFontName;
    property FontSize : Integer read FFontSize write FFontSize;
    property FontColor : TBGRAPixel read FFontColor write FFontColor;
    property Padding : TPadding read FPadding write FPadding;
    property PositionType : TPositionType read FPositionType 
      write FPositionType;
    property Position : TPosition read FPosition write FPosition;

    property RendererProfile : PCommonObject read FRenderProfile
      write FRenderProfile;
  end;

implementation

{ TRendererProfileItem }

constructor TRendererProfileItem.Create (AID : Int64);
begin
  inherited Create(AID);
  FRenderProfile := nil;

  FName := '';
  FBackground := BGRA(255, 255, 255, 255);
  FBackgroundFill := FILL_NONE;
  FBackgroundRoundRadius := 0;
  FFontName := 'Default';
  FFontSize := 12;
  FFontColor := BGRA(0, 0, 0, 255);
  FPadding := TPadding.Create;
  FPadding.Top := 0;
  FPadding.Left := 0;
  FPadding.Bottom := 0;
  FPadding.Right := 0;
  FPositionType := POSITION_FIXED;
  FPosition := TPosition.Create;
  FPosition.Top := 0;
  FPosition.Left := 0;
  FPosition.Bottom := 0;
  FPosition.Right := 0;
end;

destructor TRendererProfileItem.Destroy;
begin
  FreeAndNil(FPosition);
  FreeAndNil(FPadding);
  inherited Destroy;
end;

function TRendererProfileItem.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;
  
  Schema
    .Id
    .Integer('profile_id').NotNull
    .Text('name').NotNull
    .Text('background').NotNull
    .Integer('background_fill_type').NotNull
    .Integer('background_round_radius').NotNull
    .Text('font_name').NotNull
    .Integer('font_size').NotNull
    .Text('font_color').NotNull
    .Integer('padding_top').NotNull
    .Integer('padding_left').NotNull
    .Integer('padding_bottom').NotNull
    .Integer('padding_right').NotNull
    .Integer('position_type').NotNull
    .Integer('position_top').NotNull
    .Integer('position_left').NotNull
    .Integer('position_bottom').NotNull
    .Integer('position_right').NotNull;

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema);  

  FreeAndNil(Schema);
end;

function TRendererProfileItem.Table : String;
begin
  Result := RENDERER_PROFILE_ITEM_TABLE_NAME;
end;

function TRendererProfileItem.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
begin
  if ID = -1 then
    Exit(False);

  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  FName := row.Row.GetStringValue('name');  
  FBackground := StrToBGRA(row.Row.GetStringValue('background'));
  FBackgroundFill := 
    TBackgroundFillType(row.Row.GetIntegerValue('background_fill_type'));  
  FBackgroundRoundRadius := row.Row.GetIntegerValue('background_round_radius');
  FFontName := row.Row.GetStringValue('font_name');
  FFontSize := row.Row.GetIntegerValue('font_size');
  FFontColor := StrToBGRA(row.Row.GetStringValue('font_color'));
  FPadding.Top := row.Row.GetIntegerValue('padding_top');
  FPadding.Left := row.Row.GetIntegerValue('padding_left');
  FPadding.Bottom := row.Row.GetIntegerValue('padding_bottom');
  FPadding.Right := row.Row.GetIntegerValue('padding_right'); 
  FPositionType := TPositionType(row.Row.GetIntegerValue('position_type'));
  FPosition.Top := row.Row.GetIntegerValue('position_top');
  FPosition.Left := row.Row.GetIntegerValue('position_left');
  FPosition.Bottom := row.Row.GetIntegerValue('position_bottom');
  FPosition.Right := row.Row.GetIntegerValue('position_right');  
  Result := True;
end;

function TRendererProfileItem.Save : Boolean;
begin
  if (FRenderProfile = nil) or (FRenderProfile^.ID = -1) then
    Exit(False);  

  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('profile_id', FRenderProfile^.ID)
      .Update('name', FName)
      .Update('background', BGRAToStr(FBackground))
      .Update('background_fill_type', Integer(FBackgroundFill))
      .Update('background_round_radius', FBackgroundRoundRadius)
      .Update('font_name', FFontName)
      .Update('font_size', FFontSize)
      .Update('font_color', BGRAToStr(FFontColor))
      .Update('padding_top', FPadding.Top)
      .Update('padding_left', FPadding.Left)
      .Update('padding_bottom', FPadding.Bottom)
      .Update('padding_right', FPadding.Right)
      .Update('position_type', Integer(FPositionType))
      .Update('position_top', FPosition.Top)
      .Update('position_left', FPosition.Left)
      .Update('position_bottom', FPosition.Bottom)
      .Update('position_right', FPosition.Right).Get > 0);
  end else
  begin
    Result := (InsertRow.Value('profile_id', FRenderProfile^.ID)
      .Value('name', FName)
      .Value('background', BGRAToStr(FBackground))
      .Value('background_fill_type', Integer(FBackgroundFill))
      .Value('background_round_radius', FBackgroundRoundRadius)
      .Value('font_name', FFontName)
      .Value('font_size', FFontSize)
      .Value('font_color', BGRAToStr(FFontColor))
      .Value('padding_top', FPadding.Top)
      .Value('padding_left', FPadding.Left)
      .Value('padding_bottom', FPadding.Bottom)
      .Value('padding_right', FPadding.Right)
      .Value('position_type', Integer(FPositionType))
      .Value('position_top', FPosition.Top)
      .Value('position_left', FPosition.Left)
      .Value('position_bottom', FPosition.Bottom)
      .Value('position_right', FPosition.Right).Get > 0);
      UpdateObjectID;
  end;
end;

function TRendererProfileItem.Delete : Boolean;
begin
  if ID <> -1 then
    Result := (DeleteRow.Get > 0)
  else
    Result := False;
end;

end.
