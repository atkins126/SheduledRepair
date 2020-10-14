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
  objects.common, Graphics, BGRABitmap, BGRABitmapTypes, container.arraylist;

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

      TMargin = record
        Top, Left, Bottom, Right : Integer;
      end;

      TPadding = record
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
  
      TPosition = record
        Top, Left, Botom, Right : Integer;
      end;

      TItem = record
        Name : String;
        Background : TBGRAPixel;
        BackgroundFill : TBackgroundFillType;
        BackgroundRoundRadius : Integer;
        FontName : String;
        FontSize : Integer;
        FontColor : TBGRAPixel;
        Padding : TPadding;
        PositionType : TPositionType;
        Position : TPosition;
      end; 

      TItemCompareFunctor = class(specialize TBinaryFunctor<TItem, Integer)
      public
        function Call (AValue1, AValue2 : TItem) : Integer; override;
      end;

      TItemsList = class(specialize TArrayList<TItem, TItemCompareFunctor>);
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

    function GetItem (AName : String) : TItem;
    procedure SetItem (AName : String; AValue : TItem);
  public
    property BorderType : TBorderType read FBorderType write FBorderType;
    property Border : Integer read FBorder write FBorder;
    property BorderRadius : Integer read FBorderRadius write FBorderRadius;
    property BorderColor : TBGRAPixel read FBorderColor write FBorderColor;
    property BorderMargin : TMargin read FBorderMargin write FBorderMargin;
    property Background : TBGRAPixel read FBackground write FBackground;
    property Items[Name : String] : TItem read GetItem write SetItem;
  end;

implementation

{ TRendererProfile.TItemCompareFunctor }

function TRendererProfile.TItemCompareFunctor.Call (AValue1, AValue2 :
  TItem) : Integer;
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
  inherited Destroy;
end;

function TRendererProfile.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;
  
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

  Result := FTable.CheckSchema(Schema);  

  FreeAndNil(Schema);
end;

function TRendererProfile.Table : String;
begin
  Result := RENDERER_PROFILE_TABLE_NAME;
end;

function TRendererProfile.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
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
  Result := True;
end;



end.
