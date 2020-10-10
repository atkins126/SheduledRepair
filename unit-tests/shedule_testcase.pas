unit shedule_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.shedule, DateUtils;

type
  TSheduleTestCase = class(TTestCase)
  published
    procedure Test_Shedule_CheckSchema;
    procedure Test_Shedule_SaveAndLoad;
    procedure Test_Shedule_Delete;
  end;

implementation

procedure TSheduleTestCase.Test_Shedule_CheckSchema;
var
  shedule : TShedule;
begin
  shedule := TShedule.Create(-1);
  AssertTrue('Database table schema is not correct', shedule.CheckSchema);
  FreeAndNil(shedule);
end;

procedure TSheduleTestCase.Test_Shedule_SaveAndLoad;
var
  shedule : TShedule;
  id : Int64;
begin
  shedule := TShedule.Create(-1);
  AssertTrue('Database table schema is not correct', shedule.CheckSchema);

  shedule.PrevDate := Now;
  shedule.NextDate := Now;
  AssertTrue('Object save error', shedule.Save);
  
  id := shedule.ID;
  FreeAndNil(shedule);

  shedule := TShedule.Create(id);
  AssertTrue('Shedule object load error', shedule.Load);
  AssertTrue('Shedule object ''PrevDate'' is not correct error', 
    CompareDate(Now, shedule.PrevDate) = 0);
  AssertTrue('Shedule object ''Next'' is not correct error', 
    CompareDate(Now, shedule.NextDate) = 0);

  FreeAndNil(shedule);
end;

procedure TSheduleTestCase.Test_Shedule_Delete;
var
  shedule : TShedule;
  id : Int64;
begin
  shedule := TShedule.Create(-1);
  AssertTrue('Database table schema is not correct', shedule.CheckSchema);

  shedule.PrevDate := Now;
  shedule.NextDate := Now;
  AssertTrue('Object save error', shedule.Save);
  
  id := shedule.ID;
  FreeAndNil(shedule);

  shedule := TShedule.Create(id);
  AssertTrue('Shedule object load error', shedule.Load);
  AssertTrue('Shedule object ''PrevDate'' is not correct error', 
    CompareDate(Now, shedule.PrevDate) = 0);
  AssertTrue('Shedule object ''Next'' is not correct error', 
    CompareDate(Now, shedule.NextDate) = 0);

  AssertTrue('Shedule object delete error', shedule.Delete);
  AssertTrue('Shedule object impossible load', not shedule.Load);

  FreeAndNil(shedule);
end;

initialization
  RegisterTest(TSheduleTestCase);
end.

