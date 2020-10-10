unit greasebundle_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.greasebundle,
  objects.grease, objects.quantity, objects.supplier, objects.grade,
  objects.measure;

type
  TGreaseBundleTestCase = class(TTestCase)
  published
    procedure Test_GreaseBundle_CheckSchema;
    procedure Test_GreaseBundle_SaveAndLoad;
    procedure Test_GreaseBundle_Delete;
  end;

implementation

procedure TGreaseBundleTestCase.Test_GreaseBundle_CheckSchema;
var
  grease_bundle : TGreaseBundle;
begin
  grease_bundle := TGreaseBundle.Create(-1);
  AssertTrue('Database table schema is not correct', grease_bundle.CheckSchema);
  FreeAndNil(grease_bundle);
end;

procedure TGreaseBundleTestCase.Test_GreaseBundle_SaveAndLoad;
var
  grease_bundle : TGreaseBundle;
  id : Int64;
begin
  grease_bundle := TGreaseBundle.Create(-1);
  AssertTrue('Database table schema is not correct', grease_bundle.CheckSchema);

  grease_bundle := TGreaseBundle.Create(-1);
  grease_bundle.Grease.Supplier.Name := 'New oil INC.';
  grease_bundle.Grease.Grade.Name := 'O-65/2';
  grease_bundle.Quantity.Measure.Name := 'barrel';
  grease_bundle.Quantity.Count := 2;
  AssertTrue('Object save error', grease_bundle.Save);

  id := grease_bundle.ID;
  FreeAndNil(grease_bundle);

  grease_bundle := TGreaseBundle.Create(id);
  AssertTrue('GreaseBundle object load error', grease_bundle.Load);
  AssertTrue('GreaseBundle object ''Grease.Supplier.Name'' is not correct error', 
    grease_bundle.Grease.Supplier.Name = 'New oil INC.');
  AssertTrue('GreaseBundle object ''Grease.Grade.Name'' is not correct error', 
    grease_bundle.Grease.Grade.Name = 'O-65/2');
  AssertTrue('GreaseBundle object ''Quantity.Measure.Name'' is not correct error', 
    grease_bundle.Quantity.Measure.Name = 'barrel');
  AssertTrue('GreaseBundle object ''Quantity.Count'' is not correct error', 
    grease_bundle.Quantity.Count = 2);

  FreeAndNil(grease_bundle);
end;

procedure TGreaseBundleTestCase.Test_GreaseBundle_Delete;
var
  grease_bundle : TGreaseBundle;
  id : Int64;
begin
  grease_bundle := TGreaseBundle.Create(-1);
  AssertTrue('Database table schema is not correct', grease_bundle.CheckSchema);

  grease_bundle := TGreaseBundle.Create(-1);
  grease_bundle.Grease.Supplier.Name := 'New oil INC.';
  grease_bundle.Grease.Grade.Name := 'O-65/2';
  grease_bundle.Quantity.Measure.Name := 'barrel';
  grease_bundle.Quantity.Count := 2;
  AssertTrue('Object save error', grease_bundle.Save);

  id := grease_bundle.ID;
  FreeAndNil(grease_bundle);

  grease_bundle := TGreaseBundle.Create(id);
  AssertTrue('GreaseBundle object load error', grease_bundle.Load);
  AssertTrue('GreaseBundle object ''Grease.Supplier.Name'' is not correct error', 
    grease_bundle.Grease.Supplier.Name = 'New oil INC.');
  AssertTrue('GreaseBundle object ''Grease.Grade.Name'' is not correct error', 
    grease_bundle.Grease.Grade.Name = 'O-65/2');
  AssertTrue('GreaseBundle object ''Quantity.Measure.Name'' is not correct error', 
    grease_bundle.Quantity.Measure.Name = 'barrel');
  AssertTrue('GreaseBundle object ''Quantity.Count'' is not correct error', 
    grease_bundle.Quantity.Count = 2);

  AssertTrue('GreaseBundle object delete error', grease_bundle.Delete);
  AssertTrue('GreaseBundle object impossible load', not grease_bundle.Load);

  FreeAndNil(grease_bundle);
end;

initialization
  RegisterTest(TGreaseBundleTestCase);
end.

