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

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  renderer.profile.profile;

type
  TRendererObjectProfile = class(TCommonObject)
  private
    const
      RENDERER_OBJECT_PROFILE_TABLE_NAME = 'renderer_object_profile';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Load object from database. }
    function Load : Boolean; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Delete object from database. }
    function Delete : Boolean; override;
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;
  protected
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

constructor TRendererObjectProfile.Create (AID : Int64);
begin
  inherited Create (AID);
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

function TRendererObjectProfile.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
  default_profile_id, selected_profile_id, hover_profile_id : Int64;
begin
  if ID = -1 then
    Exit(False);

  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  default_profile_id := row.Row.GetIntegerValue('default_profile_id');
  selected_profile_id := row.Row.GetIntegerValue('selected_profile_id');
  hover_profile_id := row.Row.GetIntegerValue('hover_profile_id');

  Result := FDefaultProfile.Reload(default_profile_id) and
    FSelectedProfile.Reload(selected_profile_id) and 
    FHoverProfile.Reload(hover_profile_id);
end;

function TRendererObjectProfile.Save : Boolean;
begin
  if not FDefaultProfile.Save then
    Exit(False);  

  if not FSelectedProfile.Save then
    Exit(False);

  if not FHoverProfile.Save then
    Exit(False);

  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('default_profile_id', FDefaultProfile.ID)
      .Update('selected_profile_id', FSelectedProfile.ID)
      .Update('hover_profile_id', FHoverProfile.ID).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('default_profile_id', FDefaultProfile.ID)
      .Value('selected_profile_id', FSelectedProfile.ID)
      .Value('hover_profile_id', FHoverProfile.ID).Get > 0);
    UpdateObjectID;
  end;
end;

function TRendererObjectProfile.Delete : Boolean;
begin
  if ID <> -1 then
    Result := (DeleteRow.Get > 0)
  else 
    Result := False;
end;

end.
