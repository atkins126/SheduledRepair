unit config_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, configuration;

type
  TConfigTestCase = class(TTestCase)
  published
    procedure Test_Config_CheckSchema;
    procedure Test_Config_GetValue;
  end;

implementation

procedure TConfigTestCase.Test_Config_CheckSchema;
begin
  AssertTrue('Database table schema is not correct', Config.CheckSchema);
  AssertTrue('Database file not exists', FileExists('database.db'));
end;

procedure TConfigTestCase.Test_Config_GetValue;
begin
  AssertTrue('Database table schema is not correct', Config.CheckSchema);
  AssertTrue('Database file not exists', FileExists('database.db'));

  Config.SetValue('config.test', 123);
  Config.SetValue('config.string', 'some text value');
  Config.SetValue('config.float_value', 14.558);

  AssertTrue('Database save data error', Config.Save);
  Config.Load(-1);

  AssertTrue('Config integer key not exists or not correct', 
    Config.GetValue('config.test', 0) = 123);
  AssertTrue('Config string key not exists or not correct', 
    Config.GetValue('config.string', '') = 'some text value');
  AssertEquals('Config double key not exists or not correct', 
    Config.GetValue('config.float_value', 0.00), 14.558, 0.001);
end;

initialization
  RegisterTest(TConfigTestCase);
end.

