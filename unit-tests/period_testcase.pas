unit period_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.period, objects.quantity,
  objects.measure;

type
  TPeriodTestCase = class(TTestCase)
  published
    procedure Test_Period_CheckSchema;
    procedure Test_Period_SaveAndLoad;
    procedure Test_Period_Delete;
  end;

implementation

procedure TPeriodTestCase.Test_Period_CheckSchema;
var
  period : TPeriod;
begin
  period := TPeriod.Create(-1);
  AssertTrue('Database table schema is not correct', period.CheckSchema);
  FreeAndNil(period);
end;

procedure TPeriodTestCase.Test_Period_SaveAndLoad;
var
  period : TPeriod;
  id : Int64;
begin
  period := TPeriod.Create(-1);
  AssertTrue('Database table schema is not correct', period.CheckSchema);

  period.Quantity.Measure.Name := 'year';
  period.Quantity.Count := 2;
  AssertTrue('Object save error', period.Save);
  
  id := period.ID;
  FreeAndNil(period);

  period := TPeriod.Create(id);
  AssertTrue('Period object load error', period.Load);
  AssertTrue('Period object ''ID'' is not correct error', period.ID = id);
  AssertTrue('Period object ''Quantity.Measure.Name'' is not correct error', 
    period.Quantity.Measure.Name = 'year');
  AssertEquals('Period object ''Quantity.Count'' is not correct error', 
    period.Quantity.Count, 2, 0.01);
  
  FreeAndNil(period);
end;

procedure TPeriodTestCase.Test_Period_Delete;
var
  period : TPeriod;
  id : Int64;
begin
  period := TPeriod.Create(-1);
  AssertTrue('Database table schema is not correct', period.CheckSchema);

  period.Quantity.Measure.Name := 'year';
  period.Quantity.Count := 2;
  AssertTrue('Object save error', period.Save);
  
  id := period.ID;
  FreeAndNil(period);

  period := TPeriod.Create(id);
  AssertTrue('Period object load error', period.Load);
  AssertTrue('Period object ''ID'' is not correct error', period.ID = id);
  AssertTrue('Period object ''Quantity.Measure.Name'' is not correct error', 
    period.Quantity.Measure.Name = 'year');
  AssertEquals('Period object ''Quantity.Count'' is not correct error', 
    period.Quantity.Count, 2, 0.01);
  
  AssertTrue('Period object delete error', period.Delete);
  AssertTrue('Period object impossible load', not period.Load);

  FreeAndNil(period);
end;

initialization
  RegisterTest(TPeriodTestCase);
end.

