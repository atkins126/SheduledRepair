program test;

{$mode objfpc}{$H+}

uses
  SysUtils, Interfaces, Forms, GuiTestRunner, fpcunittestrunner, database,
  sqlite3.database, quantity_testcase, objects.greasebundle, objects.quantity,
  objects.period, objects.node, objects.entity, objects.shedule,
  config_testcase, measure_testcase, grade_testcase, supplier_testcase,
  grease_testcase, greasebundle_testcase, period_testcase, shedule_testcase,
  node_testcase, entity_testcase, job_testcase, equipment_testcase,
  rendererprofile_testcase, utils.functor, rendererobjectprofile_testcase,
  rule_testcase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

