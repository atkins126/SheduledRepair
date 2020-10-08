unit grade_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.grade;

type
  TGradeTestCase = class(TTestCase)
  published
    procedure Test_Grade_CheckSchema;
    procedure Test_Grade_SaveAndLoad;
  end;

implementation

procedure TGradeTestCase.Test_Grade_CheckSchema;
var
  grade : TGrade;
begin
  grade := TGrade.Create(-1);

  AssertTrue('Database table schema is not correct', grade.CheckSchema);
  AssertTrue('Database file not exists', FileExists('database.db'));

  FreeAndNil(grade);
end;

procedure TGradeTestCase.Test_Grade_SaveAndLoad;
var
  grade : TGrade;
  id : Int64;
begin
  grade := TGrade.Create(-1);

  AssertTrue('Database table schema is not correct', grade.CheckSchema);
  AssertTrue('Database file not exists', FileExists('database.db'));

  grade.Name := 'E10';
  AssertTrue('Object save error', grade.Save);
  
  id := grade.ID;
  FreeAndNil(grade);

  grade := TGrade.Create(id);
  AssertTrue('Grade object load error', grade.Load);
  AssertTrue('Grade object ''ID'' is not correct error', grade.ID = id);
  AssertTrue('Grade object ''Name'' is not correct', grade.Name = 'E10');

  FreeAndNil(grade);
end;

initialization
  RegisterTest(TGradeTestCase);
end.

