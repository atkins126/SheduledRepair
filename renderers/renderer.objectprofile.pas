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
unit renderer.objectprofile;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  renderer.profile, rules.chain;

type
  TRendererObjectProfile = class(TCommonObject)
  private
    const
      RENDERER_OBJECT_PROFILE_TABLE_NAME = 'renderer_object_profile';
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
    FProfile : TRendererProfile;
    FFocusedProfile : TRendererProfile;
  public
    property Profile : TRendererProfile read FProfile;
    property FocusedProfile : TRendererProfile read FFocusedProfile;
  end;

implementation

{ TRendererObjectProfile }

constructor TRendererObjectProfile.Create (AID : Int64);
begin
  inherited Create (AID);
  FProfile := TRendererProfile.Create(-1);
  FFocusedProfile := TRendererProfile.Create(-1);
end;

destructor TRendererObjectProfile.Destroy;
begin
  FreeAndNil(FProfile);
  FreeAndNil(FFocusedProfile);
  inherited Destroy;
end;

function TRendererObjectProfile.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;
  
  Schema
    .Id
    .Integer('profile_id').NotNull
    .Integer('focused_profile_id').NotNull;

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema) and FProfile.CheckSchema;  

  FreeAndNil(Schema);
end;

function TRendererObjectProfile.Table : String;
begin
  Result := RENDERER_OBJECT_PROFILE_TABLE_NAME;
end;

function TRendererObjectProfile.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
  profile_id, focused_profile_id : Int64;
begin
  if ID = -1 then
    Exit(False);

  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  profile_id := row.Row.GetIntegerValue('profile_id');
  focused_profile_id := row.Row.GetIntegerValue('focused_profile_id');

  Result := FProfile.Reload(profile_id) and 
    FFocusedProfile.Reload(focused_profile_id);
end;

function TRendererObjectProfile.Save : Boolean;
begin
  if not FProfile.Save then
    Exit(False);  

  if not FFocusedProfile.Save then  
    Exit(False);

  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('profile_id', FProfile.ID)
      .Update('focused_profile_id', FFocusedProfile.ID).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('profile_id', FProfile.ID)
      .Value('focused_profile_id', FFocusedProfile.ID).Get > 0);
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
