unit node_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.greasebag, 
  objects.greasebundle, objects.quantity, objects.measure, objects.grease,
  objects.supplier, objects.grade, objects.node, objects.period, dateutils,
  objects.shedule;

type
  TNodeTestCase = class(TTestCase)
  published
    procedure Test_Node_CheckSchema;
    procedure Test_Node_SaveAndLoad;
    procedure Test_Node_Delete;
  end;

implementation

procedure TNodeTestCase.Test_Node_CheckSchema;
var
  node : TNode;
begin
  node := TNode.Create(-1);
  AssertTrue('Database table schema is not correct', node.CheckSchema);
  FreeAndNil(node);
end;

procedure TNodeTestCase.Test_Node_SaveAndLoad;
var
  node : TNode;
  greasebundle : TGreaseBundle;
  counter : Integer;
  id : Int64;
begin
  node := TNode.Create(-1);
  AssertTrue('Database table schema is not correct', node.CheckSchema);

  node.Name := 'equipment';

  greasebundle := TGreaseBundle.Create(-1);
  greasebundle.Grease.Supplier.Name := 'Clamps & Pipe Inc.';
  greasebundle.Grease.Grade.Name := 'L134-4PK';
  greasebundle.Quantity.Measure.Name := 'Gal';
  greasebundle.Quantity.Count := 0.5;

  node.GreaseBag.Append(greasebundle);

  greasebundle := TGreaseBundle.Create(-1);
  greasebundle.Grease.Supplier.Name := 'Interflon';
  greasebundle.Grease.Grade.Name := 'MicPol';
  greasebundle.Quantity.Measure.Name := 'kg';
  greasebundle.Quantity.Count := 5.5;

  node.GreaseBag.Append(greasebundle);

  node.Period.Quantity.Measure.Name := 'month';
  node.Period.Quantity.Count := 6;
  AssertTrue('Object save error', node.Save);
  
  id := node.ID;
  FreeAndNil(node);

  node := TNode.Create(id);
  AssertTrue('Node object load error', node.Load);
  AssertTrue('Node object ''ID'' is not correct error', node.ID = id);
  AssertTrue('Node object ''Name'' is not correct error', 
    node.Name = 'equipment');
  AssertTrue('Node object ''Shedule.PrevDate'' is not correct error', 
    CompareDate(now, node.Shedule.PrevDate) = 0);
  AssertTrue('Node object ''Shedule.NextDate'' is not correct error', 
    CompareDate(now, node.Shedule.NextDate) = 0);
  AssertTrue('Node object ''Period.Quantity.Measure.Name'' is not correct error', 
    node.Period.Quantity.Measure.Name = 'month');
  AssertTrue('Node object ''Period.Quantity.Count'' is not correct error', 
    node.Period.Quantity.Count = 6);
  
  counter := 0;
  for greasebundle in node.GreaseBag do
  begin
    case counter of
    0 : begin
      AssertTrue('Node object ''GreaseBundle.Grease.Supplier.Name'' is not correct error', 
        greasebundle.Grease.Supplier.Name = 'Clamps & Pipe Inc.');
      AssertTrue('Node object ''GreaseBundle.Grease.Grade.Name'' is not correct error', 
        greasebundle.Grease.Grade.Name = 'L134-4PK');
      AssertTrue('Node object ''GreaseBundle.Quantity.Measure.Name'' is not correct error', 
        greasebundle.Quantity.Measure.Name = 'Gal');
      AssertTrue('Node object ''GreaseBundle.Quantity.Count'' is not correct error', 
        greasebundle.Quantity.Count = 0.5);
    end;
    1 : begin
      AssertTrue('Node object ''GreaseBundle.Grease.Supplier.Name'' is not correct error',
        greasebundle.Grease.Supplier.Name = 'Interflon');
      AssertTrue('Node object ''GreaseBundle.Grease.Grade.Name'' is not correct error',
        greasebundle.Grease.Grade.Name = 'MicPol');
      AssertTrue('Node object ''GreaseBundle.Quantity.Measure.Name'' is not correct error',
        greasebundle.Quantity.Measure.Name = 'kg');
      AssertTrue('Node object ''GreaseBundle.Quantity.Count'' is not correct error',
        greasebundle.Quantity.Count = 5.5);
    end;
    2 : begin
      Fail('Impossible Node.GreaseBag item.');
    end;
    end;
    Inc(counter);
  end;

  AssertTrue('GreaseBag items count is not correct', counter = 2);

  FreeAndNil(node);
end;

procedure TNodeTestCase.Test_Node_Delete;
var
  node : TNode;
  greasebundle : TGreaseBundle;
  counter : Integer;
  id : Int64;
begin
  node := TNode.Create(-1);
  AssertTrue('Database table schema is not correct', node.CheckSchema);

  node.Name := 'equipment';

  greasebundle := TGreaseBundle.Create(-1);
  greasebundle.Grease.Supplier.Name := 'Clamps & Pipe Inc.';
  greasebundle.Grease.Grade.Name := 'L134-4PK';
  greasebundle.Quantity.Measure.Name := 'Gal';
  greasebundle.Quantity.Count := 0.5;

  node.GreaseBag.Append(greasebundle);

  greasebundle := TGreaseBundle.Create(-1);
  greasebundle.Grease.Supplier.Name := 'Interflon';
  greasebundle.Grease.Grade.Name := 'MicPol';
  greasebundle.Quantity.Measure.Name := 'kg';
  greasebundle.Quantity.Count := 5.5;

  node.GreaseBag.Append(greasebundle);

  node.Period.Quantity.Measure.Name := 'month';
  node.Period.Quantity.Count := 6;
  AssertTrue('Object save error', node.Save);
  
  id := node.ID;
  FreeAndNil(node);

  node := TNode.Create(id);
  AssertTrue('Node object load error', node.Load);
  AssertTrue('Node object ''ID'' is not correct error', node.ID = id);
  AssertTrue('Node object ''Name'' is not correct error', 
    node.Name = 'equipment');
  AssertTrue('Node object ''Shedule.PrevDate'' is not correct error', 
    CompareDate(now, node.Shedule.PrevDate) = 0);
  AssertTrue('Node object ''Shedule.NextDate'' is not correct error', 
    CompareDate(now, node.Shedule.NextDate) = 0);
  AssertTrue('Node object ''Period.Quantity.Measure.Name'' is not correct error', 
    node.Period.Quantity.Measure.Name = 'month');
  AssertTrue('Node object ''Period.Quantity.Count'' is not correct error', 
    node.Period.Quantity.Count = 6);
  
  counter := 0;
  for greasebundle in node.GreaseBag do
  begin
    case counter of
    0 : begin
      AssertTrue('Node object ''GreaseBundle.Grease.Supplier.Name'' is not correct error', 
        greasebundle.Grease.Supplier.Name = 'Clamps & Pipe Inc.');
      AssertTrue('Node object ''GreaseBundle.Grease.Grade.Name'' is not correct error', 
        greasebundle.Grease.Grade.Name = 'L134-4PK');
      AssertTrue('Node object ''GreaseBundle.Quantity.Measure.Name'' is not correct error', 
        greasebundle.Quantity.Measure.Name = 'Gal');
      AssertTrue('Node object ''GreaseBundle.Quantity.Count'' is not correct error', 
        greasebundle.Quantity.Count = 0.5);
    end;
    1 : begin
      AssertTrue('Node object ''GreaseBundle.Grease.Supplier.Name'' is not correct error',
        greasebundle.Grease.Supplier.Name = 'Interflon');
      AssertTrue('Node object ''GreaseBundle.Grease.Grade.Name'' is not correct error',
        greasebundle.Grease.Grade.Name = 'MicPol');
      AssertTrue('Node object ''GreaseBundle.Quantity.Measure.Name'' is not correct error',
        greasebundle.Quantity.Measure.Name = 'kg');
      AssertTrue('Node object ''GreaseBundle.Quantity.Count'' is not correct error',
        greasebundle.Quantity.Count = 5.5);
    end;
    2 : begin
      Fail('Impossible Node.GreaseBag item.');
    end;
    end;
    Inc(counter);
  end;

  AssertTrue('GreaseBag items count is not correct', counter = 2);

  AssertTrue('Node object delete error', node.Delete);
  AssertTrue('Node object impossible load', not node.Load);

  FreeAndNil(node);
end;

initialization
  RegisterTest(TNodeTestCase);
end.

