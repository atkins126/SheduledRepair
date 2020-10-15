unit rule_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, rules.rule;

type
  TRuleTestCase = class(TTestCase)
  published
    procedure Test_Rule_CheckSchema;
    procedure Test_Rule_SaveAndLoad;
    procedure Test_Rule_Delete;
  end;

implementation

procedure TRuleTestCase.Test_Rule_CheckSchema;
var
  rule : TRule;
begin
  rule := TRule.Create(-1);
  AssertTrue('Database table schema is not correct', rule.CheckSchema);
  FreeAndNil(rule);
end;

procedure TRuleTestCase.Test_Rule_SaveAndLoad;
var
  rule : TRule;
  id : Int64;
begin
  rule := TRule.Create(-1);
  AssertTrue('Database table schema is not correct', rule.CheckSchema);

  AssertTrue('Object save error', rule.Save);
  
  id := rule.ID;
  FreeAndNil(rule);

  rule := TRule.Create(id);
  AssertTrue('Rule object load error', rule.Load);
  AssertTrue('Rule object ''ID'' is not correct error', rule.ID = id);

  FreeAndNil(rule);
end;

procedure TRuleTestCase.Test_Rule_Delete;
var
  rule : TRule;
  id : Int64;
begin
  rule := TRule.Create(-1);
  AssertTrue('Database table schema is not correct', rule.CheckSchema);

  AssertTrue('Object save error', rule.Save);
  
  id := rule.ID;
  FreeAndNil(rule);

  rule := TRule.Create(id);
  AssertTrue('Rule object load error', rule.Load);
  AssertTrue('Rule object ''ID'' is not correct error', rule.ID = id);

  AssertTrue('Rule object delete error', rule.Delete);
  AssertTrue('Rule object impossible load', not rule.Load);

  FreeAndNil(rule);
end;

initialization
  RegisterTest(TRuleTestCase);
end.

