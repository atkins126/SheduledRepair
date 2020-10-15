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
unit rules.chain;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  rules.rule, renderer.objectprofile, sqlite3.table, database;

type
  TRulesChain = class(TCommonObject)
  private
    const
      RULES_CHAIN_TABLE_NAME = 'rules_chain';
  public
    class function CalculateProfile (AObject : PCommonObject) : 
      TRendererObjectProfile;    

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
    FObject : PCommonObject;
    FRendererProfile : TRendererObjectProfile;
  public
    property Entity : PCommonObject read FObject write FObject;
    property RendererProfile : TRendererObjectProfile read FRendererProfile;
  end;

implementation

{ TRulesChain }

class function TRulesChain.CalculateProfile (AObject : PCommonObject) : 
  TRendererObjectProfile;
var
  rules : TSQLite3Table;
  chain : TSQLite3Result.TRowIterator;
  profile_id : Int64; 
begin
  if (AObject = nil) or (AObject^.ID = -1) then
    Exit(TRendererObjectProfile.Create(-1));

  rules := TSQLite3Table.Create(DB.Errors, DB.Handle, RULES_CHAIN_TABLE_NAME);
  chain := rules.Select.Field('object_profile_id')
    .Where('object_name', AObject^.Table).Where('object_id', AObject^.ID)
    .Limit(1).Get.FirstRow;
  
  if not chain.HasRow then
    Exit(TRendererObjectProfile.Create(-1));

  Result := TRendererObjectProfile.Create(
    chain.Row.GetIntegerValue('object_profile_id')
  );
end;

constructor TRulesChain.Create (AID : Int64);
begin
  inherited Create (AID);
  FRendererProfile := TRendererObjectProfile.Create(-1);
end;

destructor TRulesChain.Destroy;
begin
  inherited Destroy;
end;

function TRulesChain.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
  rule : TRule;
  obj_profile : TRendererObjectProfile;
begin
  Schema := TSQLite3Schema.Create;
  rule := TRule.Create(-1);
  obj_profile := TRendererObjectProfile.Create(-1);
  
  Schema
    .Id
    .Text('object_name').NotNull
    .Integer('object_id').NotNull
    .Integer('object_profile_id').NotNull;

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema) and rule.CheckSchema and
    obj_profile.CheckSchema;  

  FreeAndNil(rule);
  FreeAndNil(obj_profile);
  FreeAndNil(Schema);
end;

function TRulesChain.Table : String;
begin
  Result := RULES_CHAIN_TABLE_NAME;
end;

function TRulesChain.Load : Boolean;
var
  row : TSQLite3Result.TRowIterator;
begin
  if (FObject = nil) or (FObject^.ID = -1) then
    Exit(False);  

  if ID = -1 then
    Exit(False);

  row := GetRowIterator;

  if not row.HasRow then
    Exit(False);

  Result := FRendererProfile.Reload(
    row.Row.GetIntegerValue('object_profile_id')
  );  
end;

function TRulesChain.Save : Boolean;
begin
  if (FObject = nil) or (FObject^.ID = -1) then
    Exit(False);

  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('object_profile_id', FRendererProfile.ID)
      .Update('object_name', FObject^.Table)
      .Update('object_id', FObject^.ID).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('object_profile_id', FRendererProfile.ID)
      .Value('object_name', FObject^.Table).Value('object_id', FObject^.ID)  
      .Get > 0);
    UpdateObjectID;
  end;
end;

function TRulesChain.Delete : Boolean;
begin
  if ID <> -1 then
    Result := (DeleteRow.Get > 0)
  else 
    Result := False;
end;

end.
