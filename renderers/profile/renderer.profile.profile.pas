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
unit renderer.profile.profile;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, Graphics, sqlite3.schema,
  renderer.profile.profileitem, renderer.profile.itembag;

type
  TRendererProfile = class(TCommonObject)
  private
    const
      RENDERER_PROFILE_TABLE_NAME = 'renderer_profile';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Load object from database. }
    function Load : Boolean; override;

    { Object deep copy. }
    procedure Assign (AProfile : TRendererProfile);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;

    { Store current object to database. }
    procedure SaveCurrentObject; override;

    { Delete all dependent objects. }
    function DeleteDepentObjects : Boolean; override;
  protected
    FEnable : Boolean;
    FHeight : Integer;
    FBackground : TColor;
    FItems : TProfileItemBag;

    function GetItem (AName : String) : TRendererProfileItem;
    procedure SetItem (AName : String; AValue : TRendererProfileItem);
  public
    { Get enumerator for in operator. }
    function GetEnumerator : TProfileItemBag.TIterator;
  public
    property Enable : Boolean read FEnable write FEnable;
    property Height : Integer read FHeight write FHeight;
    property Background : TColor read FBackground write FBackground;
    property Items[Name : String] : TRendererProfileItem read GetItem
      write SetItem;
  end;

implementation

constructor TRendererProfile.Create (AID : Int64);
begin
  inherited Create(AID);
  FEnable := False;
  FHeight := 20;
  FBackground := clWhite;
  FItems := TProfileItemBag.Create(-1, Self);
end;

destructor TRendererProfile.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TRendererProfile.GetItem (AName : String) : TRendererProfileItem;
var
  ProfileItem : TRendererProfileItem;
begin
  ProfileItem := FItems.Search(AName);

  if ProfileItem = nil then
  begin
    ProfileItem := TRendererProfileItem.Create(-1);
    ProfileItem.Name := AName;
    FItems.Append(ProfileItem);
    Exit(ProfileItem);
  end;

  Result := ProfileItem;
end;

procedure TRendererProfile.SetItem (AName : String; AValue :
  TRendererProfileItem);
var
  ProfileItem : TRendererProfileItem;
begin
  ProfileItem := FItems.Search(AName);

  if ProfileItem = nil then
  begin
    AValue.Name := AName;
    FItems.Append(AValue);
    Exit;
  end;

  ProfileItem := TRendererProfileItem.Create(-1);
  ProfileItem.Assign(AValue);
  ProfileItem.Name := AName;
  FItems.Append(ProfileItem);
end;

function TRendererProfile.GetEnumerator : TProfileItemBag.TIterator;
begin
  Result := FItems.GetEnumerator;
end;

procedure TRendererProfile.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Integer('enable').NotNull
    .Integer('height').NotNull
    .Integer('background').NotNull;
end;

function TRendererProfile.CheckDepentSchemes : Boolean;
begin
  Result := FItems.CheckSchema;
end;

function TRendererProfile.Table : String;
begin
  Result := RENDERER_PROFILE_TABLE_NAME;
end;

function TRendererProfile.LoadCurrentObject : Boolean;
begin
  Result := inherited LoadCurrentObject;

  FEnable := Boolean(GetIntegerProperty('enable'));
  FHeight := GetIntegerProperty('height');
  FBackground := TColor(GetIntegerProperty('background'));

  Result := True;
end;

function TRendererProfile.Load : Boolean;
begin
  Result := inherited Load and FItems.Reload(-1);
end;

procedure TRendererProfile.SaveCurrentObject;
begin
  SetIntegerProperty('enable', Integer(FEnable));
  SetIntegerProperty('height', FHeight);
  SetIntegerProperty('background', FBackground);
end;

function TRendererProfile.Save : Boolean;
begin
  Result := inherited Save and FItems.Save;
end;

function TRendererProfile.DeleteDepentObjects : Boolean;
begin
  Result := FItems.Delete;
end;

procedure TRendererProfile.Assign (AProfile : TRendererProfile);
begin
  FEnable := AProfile.Enable;
  FHeight := AProfile.Height;
  FBackground := AProfile.Background;
  FItems.Assign(AProfile.FItems);
end;

end.
