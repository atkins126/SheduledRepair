program test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, fpcunittestrunner, SysUtils,
  config_testcase, measure_testcase, sqlite3.database, quantity_testcase,
  grade_testcase, supplier_testcase, grease_testcase, greasebundle_testcase;

{$R *.res}

begin
  //DeleteFile('database.db');

  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

