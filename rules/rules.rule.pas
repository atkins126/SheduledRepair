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
unit rules.rule;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, renderer.profile.objectprofile;

type
  TRule = class(TCommonObject)
  private
    const
      RULE_TABLE_NAME = 'rule';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Object deep copy. }
    procedure Assign (ARule : TRule);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;

    { Store current object to database. }
    procedure SaveCurrentObject; override;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; override;
  protected
    FProfile : TRendererObjectProfile;
  public
    property Profile : TRendererObjectProfile read FProfile write FProfile;
  end;

implementation

{ TRule }

constructor TRule.Create (AID : Int64);
begin
  inherited Create (AID);
  FProfile := TRendererObjectProfile.Create(-1, nil);
end;

destructor TRule.Destroy;
begin
  FreeAndNil(FProfile);
  inherited Destroy;
end;

procedure TRule.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Integer('object_profile_id').NotNull;
end;

function TRule.Table : String;
begin
  Result := RULE_TABLE_NAME;
end;

function TRule.LoadDepentObjects : Boolean;
begin
  Result := FProfile.Reload(GetIntegerProperty('object_profile_id'));
end;

procedure TRule.SaveCurrentObject;
begin
  SetIntegerProperty('object_profile_id', FProfile.ID);
end;

function TRule.SaveDepentObjects : Boolean;
begin
  Result := FProfile.Save;
end;

procedure TRule.Assign (ARule : TRule);
begin
  FProfile.Assign(ARule.Profile);
end;

end.
