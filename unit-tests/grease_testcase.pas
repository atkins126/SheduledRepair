unit grease_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.grease, objects.grade,
  objects.supplier;

type
  TGreaseTestCase = class(TTestCase)
  published
    procedure Test_Grease_CheckSchema;
    procedure Test_Grease_SaveAndLoad;
  end;

implementation

procedure TGreaseTestCase.Test_Grease_CheckSchema;
var
  grease : TGrease;
begin
  grease := TGrease.Create(-1);

  AssertTrue('Database table schema is not correct', grease.CheckSchema);
  AssertTrue('Database file not exists', FileExists('database.db'));

  FreeAndNil(grease);
end;

procedure TGreaseTestCase.Test_Grease_SaveAndLoad;
var
  grease : TGrease;
  supplier : TSupplier;
  grade : TGrade;
  id : Int64;
begin
  grease := TGrease.Create(-1);

  AssertTrue('Database table schema is not correct', grease.CheckSchema);
  AssertTrue('Database file not exists', FileExists('database.db'));

  supplier := TSupplier.Create(-1);
  supplier.Name := 'MOBI';

  grade := TGrade.Create(-1);
  grade.Name := 'GW700';

  grease.Supplier := supplier;
  grease.Grade := grade;
  AssertTrue('Object save error', grease.Save);

  id := grease.ID;
  FreeAndNil(grease);

  grease := TGrease.Create(id);
  AssertTrue('Grease object load error', grease.Load);
  AssertTrue('Grease object ''Supplier.Name'' is not correct error', 
    grease.Supplier.Name = 'MOBI');
  AssertTrue('Grease object ''Grade.Name'' is not correct', 
    grease.Grade.Name = 'GW700');

  FreeAndNil(grease);
end;

initialization
  RegisterTest(TGreaseTestCase);
end.

