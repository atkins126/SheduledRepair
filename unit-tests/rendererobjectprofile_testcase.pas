unit rendererobjectprofile_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, renderer.profile.profile, 
  renderer.profile.objectprofile, BGRABitmap, BGRABitmapTypes;

type
  TRendererObjectProfileTestCase = class(TTestCase)
  published
    procedure Test_RendererObjectProfile_CheckSchema;
    procedure Test_RendererObjectProfile_SaveAndLoad;
    procedure Test_RendererObjectProfile_Delete;
  end;

implementation

procedure TRendererObjectProfileTestCase.Test_RendererObjectProfile_CheckSchema;
var
  obj_profile : TRendererObjectProfile;
begin
  obj_profile := TRendererObjectProfile.Create(-1);
  AssertTrue('Database table schema is not correct', obj_profile.CheckSchema);
  FreeAndNil(obj_profile);
end;

procedure TRendererObjectProfileTestCase.Test_RendererObjectProfile_SaveAndLoad;
var
  obj_profile : TRendererObjectProfile;
  id : Int64;
begin
  obj_profile := TRendererObjectProfile.Create(-1);
  AssertTrue('Database table schema is not correct', obj_profile.CheckSchema);

  obj_profile.DefaultProfile.BorderRadius := 10;
  obj_profile.DefaultProfile.Background := BGRA(10, 10, 10, 128);
  obj_profile.DefaultProfile.Height := 25;

  obj_profile.SelectedProfile.BorderRadius := 10;
  obj_profile.SelectedProfile.Background := BGRA(128, 128, 128, 192);
  obj_profile.SelectedProfile.Height := 40;

  AssertTrue('Object save error', obj_profile.Save);

  id := obj_profile.ID;
  FreeAndNil(obj_profile);

  obj_profile := TRendererObjectProfile.Create(id);
  AssertTrue('RendererObjectProfile object load error', obj_profile.Load);
  AssertTrue('RendererObjectProfile object ''ID'' is not correct error', 
    obj_profile.ID = id);
  AssertTrue('RendererObjectProfile object ''DefaultProfile.Height'' is not correct error',
    obj_profile.DefaultProfile.Height = 25);
  AssertTrue('RendererObjectProfile object ''SelectedProfile.Height'' is not correct error',
    obj_profile.SelectedProfile.Height = 40);

  FreeAndNil(obj_profile);  
end;

procedure TRendererObjectProfileTestCase.Test_RendererObjectProfile_Delete;
begin
  AssertTrue(True);
end;

initialization
  RegisterTest(TRendererObjectProfileTestCase);
end.

