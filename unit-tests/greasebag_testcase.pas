unit greasebag_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.greasebag, 
  objects.greasebundle, objects.quantity, objects.measure, objects.grease,
  objects.supplier, objects.grade;

type
  TGreaseBagTestCase = class(TTestCase)
  published
    procedure Test_GreaseBag_CheckSchema;
    procedure Test_GreaseBag_SaveAndLoad;
  end;

implementation

procedure TGreaseBagTestCase.Test_GreaseBag_CheckSchema;
var
  greasebag : TGreaseBag;
begin
  greasebag := TGreaseBag.Create(-1);
  AssertTrue('Database table schema is not correct', greasebag.CheckSchema);
  FreeAndNil(greasebag);
end;

procedure TGreaseBagTestCase.Test_GreaseBag_SaveAndLoad;
var
  greasebag : TGreaseBag;  
begin
  greasebag := TGreaseBag.Create(-1);
  AssertTrue('Database table schema is not correct', greasebag.CheckSchema);

  greasebundle := TGreaseBundle.Create(-1);
  greasebundle.Grease.Supplier.Name := 'Clamps & Pipe Inc.';
  greasebundle.Grease.Grade.Name := 'L134-4PK';
  greasebundle.Quantity.Measure := 'Gal';
  greasebundle.Quantity.Count := 0.5;

  

end;

initialization
  AssertTrue('Database file not exists', FileExists('database.db'));
  RegisterTest(TGreaseBagTestCase);
end.

