unit config_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, configuration;

type
  TConfigTestCase = class(TTestCase)
  published
    procedure Test_Config_CheckSchema;
    procedure Test_Config_SaveAndLoad;
    procedure Test_Config_RemoveValue;
  end;

implementation

procedure TConfigTestCase.Test_Config_CheckSchema;
begin
  AssertTrue('Database table schema is not correct', Config.CheckSchema);
end;

procedure TConfigTestCase.Test_Config_SaveAndLoad;
begin
  AssertTrue('Database table schema is not correct', Config.CheckSchema);
  Config.Load;

  Config.SetValue('config.integer', 123);
  Config.SetValue('config.string', 'Secret text value');
  Config.SetValue('config.float', 14.558);

  AssertTrue('Database save data error', Config.Save);
  Config.Load;

  AssertTrue('Config integer key not exists or not correct', 
    Config.GetValue('config.integer', 0) = 123);
  AssertTrue('Config string key not exists or not correct', 
    Config.GetValue('config.string', '') = 'Secret text value');
  AssertEquals('Config double key not exists or not correct', 
    Config.GetValue('config.float', 0.00), 14.558, 0.001);
end;

procedure TConfigTestCase.Test_Config_RemoveValue;
begin
  AssertTrue('Database table schema is not correct', Config.CheckSchema);
  Config.Load;

  AssertTrue('Config integer key not exists or not correct', 
    Config.GetValue('config.integer', 0) = 123);
  AssertTrue('Config string key not exists or not correct', 
    Config.GetValue('config.string', '') = 'Secret text value');
  AssertEquals('Config double key not exists or not correct', 
    Config.GetValue('config.float', 0.00), 14.558, 0.001);

  AssertTrue('Config float value doesn''t remove', 
    Config.RemoveValue('config.float'));
  AssertTrue('Config remove not exists value', 
    not Config.RemoveValue('config.not_exists'));
end;

initialization
  RegisterTest(TConfigTestCase);
end.

