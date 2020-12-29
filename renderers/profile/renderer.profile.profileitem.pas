(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(* This is a software for creating schedules  for repair work, accounting and *)
(* monitoring  their  implementation, accounting for the  necessary materials *)
(* and spare parts.                                                           *)
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
unit renderer.profile.profileitem;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, Graphics, sqlite3.schema;

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
        X, Y : Integer;
      end;
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Object deep copy. }
    procedure Assign (AProfileItem : TRendererProfileItem);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;

    { Store current object to database. }
    procedure SaveCurrentObject; override;
  protected
    FName : String;
    FEnable : Boolean;
    FBackground : TColor;
    FBackgroundFill : TBackgroundFillType;
    FBackgroundRoundRadius : Integer;
    FFontName : String;
    FFontSize : Integer;
    FFontColor : TColor;
    FPadding : TPadding;
    FPositionType : TPositionType;
    FPosition : TPosition;
  public
    property Name : String read FName write FName;
    property Enable : Boolean read FEnable write FEnable;
    property Background : TColor read FBackground write FBackground;
    property BackgroundFillType : TBackgroundFillType read FBackgroundFill
      write FBackgroundFill;
    property BackgroundRoundRadius : Integer read FBackgroundRoundRadius
      write FBackgroundRoundRadius;
    property FontName : String read FFontName write FFontName;
    property FontSize : Integer read FFontSize write FFontSize;
    property FontColor : TColor read FFontColor write FFontColor;
    property Padding : TPadding read FPadding write FPadding;
    property PositionType : TPositionType read FPositionType 
      write FPositionType;
    property Position : TPosition read FPosition write FPosition;
  end;

implementation

{ TRendererProfileItem }

constructor TRendererProfileItem.Create (AID : Int64);
begin
  inherited Create(AID);

  FName := '';
  FEnable := False;
  FBackground := clDefault;
  FBackgroundFill := FILL_NONE;
  FBackgroundRoundRadius := 0;
  FFontName := 'Default';
  FFontSize := 12;
  FFontColor := clBlack;
  FPadding := TPadding.Create;
  FPadding.Top := 0;
  FPadding.Left := 0;
  FPadding.Bottom := 0;
  FPadding.Right := 0;
  FPositionType := POSITION_FIXED;
  FPosition := TPosition.Create;
  FPosition.X := 0;
  FPosition.Y := 0;
end;

destructor TRendererProfileItem.Destroy;
begin
  FreeAndNil(FPosition);
  FreeAndNil(FPadding);
  inherited Destroy;
end;

procedure TRendererProfileItem.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Text('name').NotNull
    .Integer('enable').NotNull
    .Integer('background').NotNull
    .Integer('background_fill_type').NotNull
    .Integer('background_round_radius').NotNull
    .Text('font_name').NotNull
    .Integer('font_size').NotNull
    .Integer('font_color').NotNull
    .Integer('padding_top').NotNull
    .Integer('padding_left').NotNull
    .Integer('padding_bottom').NotNull
    .Integer('padding_right').NotNull
    .Integer('position_type').NotNull
    .Integer('position_x').NotNull
    .Integer('position_y').NotNull;
end;

function TRendererProfileItem.Table : String;
begin
  Result := RENDERER_PROFILE_ITEM_TABLE_NAME;
end;

function TRendererProfileItem.LoadCurrentObject : Boolean;
begin
  Result := inherited LoadCurrentObject;

  FName := GetStringProperty('name');
  FEnable := Boolean(GetIntegerProperty('enable'));
  FBackground := TColor(GetIntegerProperty('background'));
  FBackgroundFill :=
    TBackgroundFillType(GetIntegerProperty('background_fill_type'));
  FBackgroundRoundRadius := GetIntegerProperty('background_round_radius');
  FFontName := GetStringProperty('font_name');
  FFontSize := GetIntegerProperty('font_size');
  FFontColor := TColor(GetIntegerProperty('font_color'));
  FPadding.Top := GetIntegerProperty('padding_top');
  FPadding.Left := GetIntegerProperty('padding_left');
  FPadding.Bottom := GetIntegerProperty('padding_bottom');
  FPadding.Right := GetIntegerProperty('padding_right');
  FPositionType := TPositionType(GetIntegerProperty('position_type'));
  FPosition.X := GetIntegerProperty('position_x');
  FPosition.Y := GetIntegerProperty('position_y');
end;

procedure TRendererProfileItem.SaveCurrentObject;
begin
  SetStringProperty('name', FName);
  SetIntegerProperty('enable', Integer(FEnable));
  SetIntegerProperty('background', FBackground);
  SetIntegerProperty('background_fill_type', Integer(FBackgroundFill));
  SetIntegerProperty('background_round_radius', FBackgroundRoundRadius);
  SetStringProperty('font_name', FFontName);
  SetIntegerProperty('font_size', FFontSize);
  SetIntegerProperty('font_color', FFontColor);
  SetIntegerProperty('padding_top', FPadding.Top);
  SetIntegerProperty('padding_left', FPadding.Left);
  SetIntegerProperty('padding_bottom', FPadding.Bottom);
  SetIntegerProperty('padding_right', FPadding.Right);
  SetIntegerProperty('position_type', Integer(FPositionType));
  SetIntegerProperty('position_x', FPosition.X);
  SetIntegerProperty('position_y', FPosition.Y);
end;

procedure TRendererProfileItem.Assign (AProfileItem : TRendererProfileItem);
begin
  FName := AProfileItem.Name;
  FEnable := AProfileItem.Enable;
  FBackground := AProfileItem.Background;
  FBackgroundFill := AProfileItem.BackgroundFillType;
  FBackgroundRoundRadius := AProfileItem.BackgroundRoundRadius;
  FFontName := AProfileItem.FontName;
  FFontSize := AProfileItem.FontSize;
  FFontColor := AProfileItem.FontColor;
  FPadding.Top := AProfileItem.Padding.Top;
  FPadding.Left := AProfileItem.Padding.Left;
  FPadding.Bottom := AProfileItem.Padding.Bottom;
  FPadding.Right := AProfileItem.Padding.Right;
  FPositionType := AProfileItem.PositionType;
  FPosition.X := AProfileItem.Position.X;
  FPosition.Y := AProfileItem.Position.Y;
end;

end.
