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
unit rules.rulesbag;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row,
  container.arraylist, utils.functor, rules.rule;

type
  TRulesBag = class(TCommonObject)
  private
    const
      RULES_BAG_TABLE_NAME = 'rulesbag';
  public
    constructor Create (AID : Int64; AObject : TCommonObject);
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Add new rule to current bag. }
    procedure Append (ARule : TRule);

    { Remove rule from current bag. }
    procedure Remove (ARule : TRule);

    { Object deep copy. }
    procedure Assign (ARulesBag : TRulesBag);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; override;

    { Save all rules associated with current object. }
    function SaveRules : Boolean;

    { Load all rules associated with current object. }
    function LoadRules : Boolean;

    { Delete current object from database. }
    function DeleteCurrentObject : Boolean; override;

    { Delete all rules associated with current object. }
    function DeleteRules : Boolean;

    { Delete all dependent objects. }
    function DeleteDepentObjects : Boolean; override;
  protected
    type
      TRulesCompareFunctor = class
        (specialize TBinaryFunctor<TRule, Integer>)
      public
        function Call (AValue1, AValue2 : TRule) : Integer; override;
      end;
      
      TRulesList = class
        (specialize TArrayList<TRule, TRulesCompareFunctor>);  
  public
    { Get enumerator for in operator. }
    function GetEnumerator : TRulesList.TIterator;
  private
    FObject : TCommonObject;
    FRulesList : TRulesList;
  end;

implementation

{ TRulesBag.TRulesCompareFunctor }

function TRulesBag.TRulesCompareFunctor.Call (AValue1, AValue2 : 
  TGreaseBundle) : Integer;
begin
  if AValue1.ID < AValue2.ID then
    Result := -1
  else if AValue2.ID < AValue1.ID then
    Result := 1
  else
    Result := 0;
end;

{ TGreaseBag }

constructor TRulesBag.Create (AID : Int64; AObject : TCommonObject);
begin
  inherited Create (AID);
  FObject := AObject;
  FRulesList := TRulesList.Create;
end;

destructor TRulesBag.Destroy;
begin
  FreeAndNil(FRulesList);
  inherited Destroy;
end;

procedure TRulesBag.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Integer('rule_id').NotNull
    .Text('object_name').NotNull
    .Integer('object_id').NotNull;
end;

function TRulesBag.CheckDepentSchemes : Boolean;
var
  Rule : TRule;
begin
  Rule := TRule.Create(-1);
  Result := Rule.CheckSchema;
  FreeAndNil(Rule);
end;

function TRulesBag.Table : String;
begin
  Result := RULES_BAG_TABLE_NAME;
end;

function TRulesBag.LoadCurrentObject : Boolean;
begin
  Result := True;
end;

function TRulesBag.LoadDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := LoadRules;
end;

function TRulesBag.SaveDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := SaveRules;
end;

function TRulesBag.SaveRules : Boolean;
var
  Rule : TRule;
begin
  if not FRulesList.FirstEntry.HasValue then
    Exit(True);

  Result := True;
  for Rule in FRulesList do
  begin
    if not Rule.Save then
      Continue;

    { Check if current rule stored in database. }  
    if FTable.Update
      .Update('rule_id', Rule.ID)
      .Where('rule_id', Rule.ID)
      .Where('object_name', FObject.Table)
      .Where('object_id', FObject.ID)
      .Get > 0 then
      Continue;

    { Save current rule in database. }
    Result := Result and (FTable.Insert
      .Value('rule_id', Rule.ID)
      .Value('object_name', FObject.Table)
      .Value('object_id', FObject.ID)
      .Get > 0);
  end;
end;

function TRulesBag.LoadRules : Boolean;
var
  ResultRows : TSQLite3Result;
  Row : TSQLite3ResultRow;
  Rule : TRules;
begin
  ResultRows := FTable.Select.All
    .Where('object_name', FObject.Table)
    .Where('object_id', FObject.ID)
    .Get;

  for Row in ResultRows do
  begin
    Rule := TRule.Create(Row.GetIntegerValue('rule_id'));

    if Rule.Load then
      FRulesList.Append(Rule);
  end;

  Result := True;
end;

function TRulesBag.DeleteCurrentObject : Boolean;
begin
  Result := True;
end;

function TRulesBag.DeleteRules : Boolean;
var
  Rule : TRules;
begin
  if not FRulesList.FirstEntry.HasValue then
    Exit(True);

  for Rule in FRulesList do
  begin
    Rule.Delete;
  end;

  FTable.Delete
    .Where('object_name', FObject.Table)
    .Where('object_id', FObject.ID)
    .Get;

  FRulesList.Clear;
  Result := True;
end;

function TRulesBag.DeleteDepentObjects : Boolean;
begin
  if (FObject = nil) or (FObject.ID = -1) then
    Exit(False);

  Result := DeleteRules;
end;

procedure TRulesBag.Append (ARule : TRule);
begin
  FRulesList.Append(ARule);
end;

procedure TRulesBag.Remove (ARule : TRule);
var
  Index : Integer;
begin
  Index := FRulesList.IndexOf(ARule);

  if Index <> -1 then
    FRulesList.Remove(Index);
end;

function TRulesBag.GetEnumerator : TRulesList.TIterator;
begin
  Result := FRulesList.GetEnumerator;
end;

procedure TRulesBag.Assign (ARulesBag : TRulesBag);
var
  Rule, CopyRule : TRule;
begin
  if not ARulesBag.FRulesList.FirstEntry.HasValue then
    Exit;

  for Rule in ARulesBag.FRulesList do
  begin
    CopyRule := TRule.Create(-1);
    CopyRule.Assign(Rule);
    FRulesList.Append(CopyRule);
  end;
end;

end.
