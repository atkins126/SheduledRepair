unit quantity_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.quantity, objects.measure;

type
  TQuantityTestCase = class(TTestCase)
  published
    procedure Test_Quantity_CheckSchema;
    procedure Test_Quantity_SaveAndLoad;
    procedure Test_Quantity_Delete;
  end;

implementation

procedure TQuantityTestCase.Test_Quantity_CheckSchema;
var
  quantity : TQuantity;
begin
  quantity := TQuantity.Create(-1);
  AssertTrue('Database table schema is not correct', quantity.CheckSchema);
  FreeAndNil(quantity);
end;

procedure TQuantityTestCase.Test_Quantity_SaveAndLoad;
var
  quantity : TQuantity;
  id : Int64;
begin
  quantity := TQuantity.Create(-1);
  AssertTrue('Database table schema is not correct', quantity.CheckSchema);

  quantity.Count := 1;
  quantity.Measure.Name := 'piece';
  AssertTrue('Object save error', quantity.Save);

  id := quantity.ID;
  FreeAndNil(quantity);

  quantity := TQuantity.Create(id);
  AssertTrue('Quantity object load error', quantity.Load);
  AssertEquals('Quantity object ''Count'' is not correct', quantity.Count, 1, 
    0.01);
  AssertTrue('Quantity object ''Measure'' is not correct', 
    quantity.Measure.Name = 'piece');
  
  FreeAndNil(quantity);
end;

procedure TQuantityTestCase.Test_Quantity_Delete;
var
  quantity : TQuantity;
  id : Int64;
begin
  quantity := TQuantity.Create(-1);
  AssertTrue('Database table schema is not correct', quantity.CheckSchema);

  quantity.Count := 1;
  quantity.Measure.Name := 'piece';
  AssertTrue('Object save error', quantity.Save);

  id := quantity.ID;
  FreeAndNil(quantity);

  quantity := TQuantity.Create(id);
  AssertTrue('Quantity object load error', quantity.Load);
  AssertEquals('Quantity object ''Count'' is not correct', quantity.Count, 1, 
    0.01);
  AssertTrue('Quantity object ''Measure'' is not correct', 
    quantity.Measure.Name = 'piece');
  
  AssertTrue('Quantity object delete error', quantity.Delete);
  AssertTrue('Quantity object impossible load', not quantity.Load);

  FreeAndNil(quantity);
end;

initialization
  RegisterTest(TQuantityTestCase);
end.

