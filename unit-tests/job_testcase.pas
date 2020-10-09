unit job_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.job, dateutils;

type
  TJobTestCase = class(TTestCase)
  published
    procedure Test_Job_CheckSchema;
    procedure Test_Job_SaveAndLoad;
  end;

implementation

procedure TJobTestCase.Test_Job_CheckSchema;
var
  job : TJob;
begin
  job := TJob.Create(-1);
  AssertTrue('Database table schema is not correct', job.CheckSchema);
  FreeAndNil(job);
end;

procedure TJobTestCase.Test_Job_SaveAndLoad;
var
  job : TJob;
  id : Int64;
begin
  job := TJob.Create(-1);
  AssertTrue('Database table schema is not correct', job.CheckSchema);

  job.Name := 'job 1';
  job.Entity.Name := 'Equipment 1';

  job.Period.Quantity.Measure.Name := 'hours';
  job.Period.Quantity.Count := 250;
  AssertTrue('Object save error', job.Save);

  id := job.ID;
  FreeAndNil(job);

  job := TJOB.Create(id);
  AssertTrue('Job object load error', job.Load);
  AssertTrue('Job object ''ID'' is not correct error', job.ID = id);
  AssertTrue('Job object ''Name'' is not correct error', 
    job.Name = 'job 1');
  AssertTrue('Job object ''Entity.Name'' is not correct error', 
    job.Entity.Name = 'Equipment 1');
  AssertTrue('Job object ''Period.Quantity.Measure.Name'' is not correct error', 
    job.Period.Quantity.Measure.Name = 'hours');
  AssertEquals('Job object ''Period.Quantity.Count'' is not correct error', 
    job.Period.Quantity.Count, 250, 0.01);
  
  FreeAndNil(job);
end;

initialization
  RegisterTest(TJobTestCase);
end.

