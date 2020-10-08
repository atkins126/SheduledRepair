unit quantity_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.quantity, objects.measure;

type
  TQuantityTestCase = class(TTestCase)
  published
    procedure Test_Quanitity_CheckSchema;
    procedure Test_Quantity_SaveAndLoad;
  end;

implementation

procedure TQuantityTestCase.Test_Quanitity_CheckSchema;
var
  quantity : TQuantity;
begin
  quantity := TQuantity.Create(-1);

  AssertTrue('Database table schema is not correct', quantity.CheckSchema);
  AssertTrue('Database file not exists', FileExists('database.db'));
end;

procedure TQuantityTestCase.Test_Quantity_SaveAndLoad;
var
  quantity : TQuantity;
  measure : TMeasure;
  id : Int64;
begin
  quantity := TQuantity.Create(-1);

  AssertTrue('Database table schema is not correct', quantity.CheckSchema);
  AssertTrue('Database file not exists', FileExists('database.db'));

  quantity.Count := 1;
  
  measure := TMeasure.Create(-1);
  measure.Name := 'piece';
  quantity.Measure := measure;

  AssertTrue('Object save error', quantity.Save);

  id := quantity.ID;
  FreeAndNil(quantity);

  quantity := TQuantity.Create(id);
  AssertTrue('Quantity object load error', quantity.Load);
  AssertTrue('Quantity object ''Count'' is not correct', quantity.Count = 1);
  AssertTrue('Quantity object ''Measure'' is not correct', 
    quantity.Measure.Name = 'piece');
  
  FreeAndNil(quantity);
end;

initialization
  RegisterTest(TQuantityTestCase);
end.

