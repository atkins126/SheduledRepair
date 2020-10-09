program test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, fpcunittestrunner, SysUtils,
  sqlite3.database, quantity_testcase, objects.greasebundle, objects.quantity,
  objects.period, objects.node, config_testcase, measure_testcase,
  grade_testcase, supplier_testcase, grease_testcase, greasebundle_testcase,
  period_testcase, shedule_testcase, node_testcase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

