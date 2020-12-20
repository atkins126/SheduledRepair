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
unit renderer.profile.objectprofile;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, renderer.profile.profile;

type
  TRendererObjectProfile = class(TCommonObject)
  private
    const
      RENDERER_OBJECT_PROFILE_TABLE_NAME = 'renderer_object_profile';
  public
    constructor Create (AID : Int64; AObject : TCommonObject); reintroduce;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Object deep copy. }
    procedure Assign (AObjectProfile : TRendererObjectProfile);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;

    { Store current object to database. }
    procedure SaveCurrentObject; override;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; override;

    { Delete all dependent objects. }
    function DeleteDepentObjects : Boolean; override;
  protected
    FObject : TCommonObject;
    FDefaultProfile : TRendererProfile;
    FSelectedProfile : TRendererProfile;
    FHoverProfile : TRendererProfile;
  public
    property DefaultProfile : TRendererProfile read FDefaultProfile;
    property SelectedProfile : TRendererProfile read FSelectedProfile;
    property HoverProfile : TRendererProfile read FHoverProfile;
  end;

implementation

{ TRendererObjectProfile }

constructor TRendererObjectProfile.Create (AID : Int64; AObject : 
  TCommonObject);
begin
  inherited Create (AID);
  FObject := AObject;
  FDefaultProfile := TRendererProfile.Create(-1);
  FSelectedProfile := TRendererProfile.Create(-1);
  FHoverProfile := TRendererProfile.Create(-1);
end;

destructor TRendererObjectProfile.Destroy;
begin
  FreeAndNil(FDefaultProfile);
  FreeAndNil(FSelectedProfile);
  FreeAndNil(FHoverProfile);
  inherited Destroy;
end;

procedure TRendererObjectProfile.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Text('object_name').NotNull
    .Integer('default_profile_id').NotNull
    .Integer('selected_profile_id').NotNull
    .Integer('hover_profile_id').NotNull;
end;

function TRendererObjectProfile.CheckDepentSchemes : Boolean;
begin
  Result := FDefaultProfile.CheckSchema;
end;

function TRendererObjectProfile.Table : String;
begin
  Result := RENDERER_OBJECT_PROFILE_TABLE_NAME;
end;

procedure TRendererObjectProfile.SaveCurrentObject;
begin
  if FObject <> nil then
    SetStringProperty('object_name', FObject.Table);
  
  SetIntegerProperty('default_profile_id', FDefaultProfile.ID);
  SetIntegerProperty('selected_profile_id', FSelectedProfile.ID);
  SetIntegerProperty('hover_profile_id', FHoverProfile.ID);
end;

function TRendererObjectProfile.SaveDepentObjects : Boolean;
begin
  Result := FDefaultProfile.Save and FSelectedProfile.Save and
    FHoverProfile.Save;
end;

function TRendererObjectProfile.LoadDepentObjects : Boolean;
begin
  Result := FDefaultProfile.Reload(GetIntegerProperty('default_profile_id')) and
    FSelectedProfile.Reload(GetIntegerProperty('selected_profile_id')) and
    FHoverProfile.Reload(GetIntegerProperty('hover_profile_id'));
end;

function TRendererObjectProfile.DeleteDepentObjects : Boolean;
begin
  Result := FDefaultProfile.Delete and FSelectedProfile.Delete and
    FHoverProfile.Delete;
end;

procedure TRendererObjectProfile.Assign (AObjectProfile :
  TRendererObjectProfile);
begin
  FDefaultProfile.Assign(AObjectProfile.DefaultProfile);
  FSelectedProfile.Assign(AObjectProfile.SelectedProfile);
  FHoverProfile.Assign(AObjectProfile.HoverProfile);
end;

end.
