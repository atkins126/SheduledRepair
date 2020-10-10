unit entity_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, objects.entity, 
  objects.greasebundle, objects.node, dateutils;

type
  TEntityTestCase = class(TTestCase)
  published
    procedure Test_Entity_CheckSchema;
    procedure Test_Entity_SaveAndLoad;
    procedure Test_Entity_Delete;
  end;

implementation

procedure TEntityTestCase.Test_Entity_CheckSchema;
var
  entity : TEntity;
begin
  entity := TEntity.Create(-1);
  AssertTrue('Database table schema is not correct', entity.CheckSchema);
  FreeAndNil(entity);
end;

procedure TEntityTestCase.Test_Entity_SaveAndLoad;
var
  entity : TEntity;
  greasebundle : TGreaseBundle;
  node : TNode;
  counter : Integer;
  id : Int64;
begin
  entity := TEntity.Create(-1);
  AssertTrue('Database table schema is not correct', entity.CheckSchema);

  entity.Name := 'some_entity';

  greasebundle := TGreaseBundle.Create(-1);
  greasebundle.Grease.Supplier.Name := 'Key Cooperative';
  greasebundle.Grease.Grade.Name := 'HTB';
  greasebundle.Quantity.Measure.Name := 'l';
  greasebundle.Quantity.Count := 0.9;

  entity.GreaseBag.Append(greasebundle);

  greasebundle := TGreaseBundle.Create(-1);
  greasebundle.Grease.Supplier.Name := 'Key Cooperative';
  greasebundle.Grease.Grade.Name := 'EH ISO 46';
  greasebundle.Quantity.Measure.Name := 'can';
  greasebundle.Quantity.Count := 12;

  entity.GreaseBag.Append(greasebundle);

  node := TNode.Create(-1);
  node.Name := 'node 1';
  node.Period.Quantity.Measure.Name := 'days';
  node.Period.Quantity.Count := 180;
  
  entity.NodeBag.Append(node);

  node := TNode.Create(-1);
  node.Name := 'node 2';
  node.Period.Quantity.Measure.Name := 'week';
  node.Period.Quantity.Count := 4;

  entity.NodeBag.Append(node);

  entity.Quantity.Measure.Name := 'pcs';
  entity.Quantity.Count := 1;
  entity.Period.Quantity.Measure.Name := 'half-year';
  entity.Period.Quantity.Count := 1.0;
  AssertTrue('Object save error', entity.Save);

  id := entity.ID;
  FreeAndNil(entity);

  entity := TEntity.Create(id);
  AssertTrue('Entity object load error', entity.Load);
  AssertTrue('Entity object ''ID'' is not correct error', entity.ID = id);
  AssertTrue('Entity object ''Name'' is not correct error', 
    entity.Name = 'some_entity');
  AssertTrue('Entity object ''Quantity.Measure.Name'' is not correct error', 
    entity.Quantity.Measure.Name = 'pcs');
  AssertTrue('Entity object ''Quantity.Measure.Count'' is not correct error', 
    entity.Quantity.Count = 1);
  AssertTrue('Entity object ''Period.Quantity.Measure.Name'' is not correct error', 
    entity.Period.Quantity.Measure.Name = 'half-year');
  AssertTrue('Entity object ''Period.Quantity.Count'' is not correct error', 
    entity.Period.Quantity.Count = 1);
  AssertTrue('Entity object ''Shedule.PrevDate'' is not correct error', 
    CompareDate(now, entity.Shedule.PrevDate) = 0);
  AssertTrue('Entity object ''Shedule.NextDate'' is not correct error', 
    CompareDate(now, entity.Shedule.NextDate) = 0);
  
  counter := 0;
  for greasebundle in entity.GreaseBag do
  begin
    case counter of
    0 : begin
      AssertTrue('Node object ''GreaseBundle.Grease.Supplier.Name'' is not correct error', 
        greasebundle.Grease.Supplier.Name = 'Key Cooperative');
      AssertTrue('Node object ''GreaseBundle.Grease.Grade.Name'' is not correct error', 
        greasebundle.Grease.Grade.Name = 'HTB');
      AssertTrue('Node object ''GreaseBundle.Quantity.Measure.Name'' is not correct error', 
        greasebundle.Quantity.Measure.Name = 'l');
      AssertEquals('Node object ''GreaseBundle.Quantity.Count'' is not correct error',
        greasebundle.Quantity.Count, 0.9, 0.01);
    end;
    1 : begin
      AssertTrue('Node object ''GreaseBundle.Grease.Supplier.Name'' is not correct error', 
        greasebundle.Grease.Supplier.Name = 'Key Cooperative');
      AssertTrue('Node object ''GreaseBundle.Grease.Grade.Name'' is not correct error', 
        greasebundle.Grease.Grade.Name = 'EH ISO 46');
      AssertTrue('Node object ''GreaseBundle.Quantity.Measure.Name'' is not correct error', 
        greasebundle.Quantity.Measure.Name = 'can');
      AssertEquals('Node object ''GreaseBundle.Quantity.Count'' is not correct error',
        greasebundle.Quantity.Count, 12, 0.01);
    end;
    2 : begin
      Fail('Impossible Node.GreaseBag item.');
    end;
    end;
    Inc(counter);
  end;

  AssertTrue('GreaseBag items count is not correct', counter = 2);

  counter := 0;
  for node in entity.NodeBag do
  begin
    case counter of
    0 : begin
      AssertTrue('Node object ''Node.Name'' is not correct error', 
        node.Name = 'node 1');
      AssertTrue('Node object ''Node.Period.Quantity.Measure.Name'' is not correct error', 
        node.Period.Quantity.Measure.Name = 'days');
      AssertTrue('Node object ''Node.Period.Quantity.Count'' is not correct error', 
        node.Period.Quantity.COunt = 180);
      AssertTrue('Entity object ''Node.Shedule.PrevDate'' is not correct error', 
        CompareDate(now, node.Shedule.PrevDate) = 0);
      AssertTrue('Entity object ''Node.Shedule.NextDate'' is not correct error', 
        CompareDate(now, node.Shedule.NextDate) = 0);
    end;
    1 : begin
      AssertTrue('Node object ''Node.Name'' is not correct error', 
        node.Name = 'node 2');
      AssertTrue('Node object ''Node.Period.Quantity.Measure.Name'' is not correct error', 
        node.Period.Quantity.Measure.Name = 'week');
      AssertTrue('Node object ''Node.Period.Quantity.Count'' is not correct error', 
        node.Period.Quantity.COunt = 4);
      AssertTrue('Entity object ''Node.Shedule.PrevDate'' is not correct error', 
        CompareDate(now, node.Shedule.PrevDate) = 0);
      AssertTrue('Entity object ''Node.Shedule.NextDate'' is not correct error', 
        CompareDate(now, node.Shedule.NextDate) = 0);
    end;
    2 : begin
      Fail('Impossible Entity.NodeBag item.');
    end;
    end;
    Inc(counter);
  end;

  AssertTrue('NodeBag items count is not correct', counter = 2);

  FreeAndNil(entity);
end;

procedure TEntityTestCase.Test_Entity_Delete;
var
  entity : TEntity;
  greasebundle : TGreaseBundle;
  node : TNode;
  counter : Integer;
  id : Int64;
begin
  entity := TEntity.Create(-1);
  AssertTrue('Database table schema is not correct', entity.CheckSchema);

  entity.Name := 'some_entity';

  greasebundle := TGreaseBundle.Create(-1);
  greasebundle.Grease.Supplier.Name := 'Key Cooperative';
  greasebundle.Grease.Grade.Name := 'HTB';
  greasebundle.Quantity.Measure.Name := 'l';
  greasebundle.Quantity.Count := 0.9;

  entity.GreaseBag.Append(greasebundle);

  greasebundle := TGreaseBundle.Create(-1);
  greasebundle.Grease.Supplier.Name := 'Key Cooperative';
  greasebundle.Grease.Grade.Name := 'EH ISO 46';
  greasebundle.Quantity.Measure.Name := 'can';
  greasebundle.Quantity.Count := 12;

  entity.GreaseBag.Append(greasebundle);

  node := TNode.Create(-1);
  node.Name := 'node 1';
  node.Period.Quantity.Measure.Name := 'days';
  node.Period.Quantity.Count := 180;
  
  entity.NodeBag.Append(node);

  node := TNode.Create(-1);
  node.Name := 'node 2';
  node.Period.Quantity.Measure.Name := 'week';
  node.Period.Quantity.Count := 4;

  entity.NodeBag.Append(node);

  entity.Quantity.Measure.Name := 'pcs';
  entity.Quantity.Count := 1;
  entity.Period.Quantity.Measure.Name := 'half-year';
  entity.Period.Quantity.Count := 1.0;
  AssertTrue('Object save error', entity.Save);

  id := entity.ID;
  FreeAndNil(entity);

  entity := TEntity.Create(id);
  AssertTrue('Entity object load error', entity.Load);
  AssertTrue('Entity object ''ID'' is not correct error', entity.ID = id);
  AssertTrue('Entity object ''Name'' is not correct error', 
    entity.Name = 'some_entity');
  AssertTrue('Entity object ''Quantity.Measure.Name'' is not correct error', 
    entity.Quantity.Measure.Name = 'pcs');
  AssertTrue('Entity object ''Quantity.Measure.Count'' is not correct error', 
    entity.Quantity.Count = 1);
  AssertTrue('Entity object ''Period.Quantity.Measure.Name'' is not correct error', 
    entity.Period.Quantity.Measure.Name = 'half-year');
  AssertTrue('Entity object ''Period.Quantity.Count'' is not correct error', 
    entity.Period.Quantity.Count = 1);
  AssertTrue('Entity object ''Shedule.PrevDate'' is not correct error', 
    CompareDate(now, entity.Shedule.PrevDate) = 0);
  AssertTrue('Entity object ''Shedule.NextDate'' is not correct error', 
    CompareDate(now, entity.Shedule.NextDate) = 0);
  
  counter := 0;
  for greasebundle in entity.GreaseBag do
  begin
    case counter of
    0 : begin
      AssertTrue('Node object ''GreaseBundle.Grease.Supplier.Name'' is not correct error', 
        greasebundle.Grease.Supplier.Name = 'Key Cooperative');
      AssertTrue('Node object ''GreaseBundle.Grease.Grade.Name'' is not correct error', 
        greasebundle.Grease.Grade.Name = 'HTB');
      AssertTrue('Node object ''GreaseBundle.Quantity.Measure.Name'' is not correct error', 
        greasebundle.Quantity.Measure.Name = 'l');
      AssertEquals('Node object ''GreaseBundle.Quantity.Count'' is not correct error',
        greasebundle.Quantity.Count, 0.9, 0.01);
    end;
    1 : begin
      AssertTrue('Node object ''GreaseBundle.Grease.Supplier.Name'' is not correct error', 
        greasebundle.Grease.Supplier.Name = 'Key Cooperative');
      AssertTrue('Node object ''GreaseBundle.Grease.Grade.Name'' is not correct error', 
        greasebundle.Grease.Grade.Name = 'EH ISO 46');
      AssertTrue('Node object ''GreaseBundle.Quantity.Measure.Name'' is not correct error', 
        greasebundle.Quantity.Measure.Name = 'can');
      AssertEquals('Node object ''GreaseBundle.Quantity.Count'' is not correct error',
        greasebundle.Quantity.Count, 12, 0.01);
    end;
    2 : begin
      Fail('Impossible Node.GreaseBag item.');
    end;
    end;
    Inc(counter);
  end;

  AssertTrue('GreaseBag items count is not correct', counter = 2);

  counter := 0;
  for node in entity.NodeBag do
  begin
    case counter of
    0 : begin
      AssertTrue('Node object ''Node.Name'' is not correct error', 
        node.Name = 'node 1');
      AssertTrue('Node object ''Node.Period.Quantity.Measure.Name'' is not correct error', 
        node.Period.Quantity.Measure.Name = 'days');
      AssertTrue('Node object ''Node.Period.Quantity.Count'' is not correct error', 
        node.Period.Quantity.COunt = 180);
      AssertTrue('Entity object ''Node.Shedule.PrevDate'' is not correct error', 
        CompareDate(now, node.Shedule.PrevDate) = 0);
      AssertTrue('Entity object ''Node.Shedule.NextDate'' is not correct error', 
        CompareDate(now, node.Shedule.NextDate) = 0);
    end;
    1 : begin
      AssertTrue('Node object ''Node.Name'' is not correct error', 
        node.Name = 'node 2');
      AssertTrue('Node object ''Node.Period.Quantity.Measure.Name'' is not correct error', 
        node.Period.Quantity.Measure.Name = 'week');
      AssertTrue('Node object ''Node.Period.Quantity.Count'' is not correct error', 
        node.Period.Quantity.COunt = 4);
      AssertTrue('Entity object ''Node.Shedule.PrevDate'' is not correct error', 
        CompareDate(now, node.Shedule.PrevDate) = 0);
      AssertTrue('Entity object ''Node.Shedule.NextDate'' is not correct error', 
        CompareDate(now, node.Shedule.NextDate) = 0);
    end;
    2 : begin
      Fail('Impossible Entity.NodeBag item.');
    end;
    end;
    Inc(counter);
  end;

  AssertTrue('NodeBag items count is not correct', counter = 2);

  AssertTrue('Entity object delete error', entity.Delete);
  AssertTrue('Entity object impossible load', not entity.Load);

  FreeAndNil(entity);
end;

initialization

  RegisterTest(TEntityTestCase);
end.

