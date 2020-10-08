unit measure_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.measure;

type
  TMeasureTestCase = class(TTestCase)
  published
    procedure Test_Measure_CheckSchema;
    procedure Test_Measure_SaveAndLoad;
  end;

implementation

procedure TMeasureTestCase.Test_Measure_CheckSchema;
var
  measure : TMeasure;
begin
  measure := TMeasure.Create(-1);

  AssertTrue('Database table schema is not correct', measure.CheckSchema);
  AssertTrue('Database file not exists', FileExists('database.db'));

  FreeAndNil(measure);
end;

procedure TMeasureTestCase.Test_Measure_SaveAndLoad;
var
  measure : TMeasure;
  id : Int64;
begin
  measure := TMeasure.Create(-1);

  AssertTrue('Database table schema is not correct', measure.CheckSchema);
  AssertTrue('Database file not exists', FileExists('database.db'));

  measure.Name := 'pcs';
  AssertTrue('Object save error', measure.Save);
  
  id := measure.ID;
  FreeAndNil(measure);

  measure := TMeasure.Create(id);
  AssertTrue('Measure object load error', measure.Load);
  AssertTrue('Measure object ''ID'' is not correct error', measure.ID = id);
  AssertTrue('Measure object ''Name'' is not correct', measure.Name = 'pcs');

  FreeAndNil(measure);
end;

initialization
  RegisterTest(TMeasureTestCase);
end.

