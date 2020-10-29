unit rendererprofile_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpcunit, testregistry, renderer.profile.profile,
  renderer.profile.profileitem;

type
  TRendererProfileTestCase = class(TTestCase)
  published
    procedure Test_RendererProfile_CheckSchema;
    procedure Test_RendererProfile_SaveAndLoad;
    procedure Test_RendererProfile_Delete;
  end;

implementation

procedure TRendererProfileTestCase.Test_RendererProfile_CheckSchema;
var
  profile : TRendererProfile;
begin
  profile := TRendererProfile.Create(-1);
  AssertTrue('Database table schema is not correct', profile.CheckSchema);
  FreeAndNil(profile);
end;

procedure TRendererProfileTestCase.Test_RendererProfile_SaveAndLoad;
var
  profile : TRendererProfile;
  item : TRendererProfileItem;
  id : Int64;
begin
  profile := TRendererProfile.Create(-1);
  AssertTrue('Database table schema is not correct', profile.CheckSchema);

  profile.Background := clDefault;

  item := profile.Items['name'];
  item.Background := clDefault;
  item.BackgroundFillType := FILL_NONE;
  item.BackgroundRoundRadius := 10;
  item.FontName := 'Default';
  item.FontSize := 13;
  item.FontColor := clBlack;
  item.Padding.Top := 2;
  item.Padding.Left := 10;
  item.Padding.Bottom := 2;
  item.Padding.Right := 10;
  item.PositionType := POSITION_FIXED;
  item.Position.X := 15;
  item.Position.Y := 0;

  AssertTrue('Object save error', profile.Save);

  id := profile.ID;
  FreeAndNil(profile);

  profile := TRendererProfile.Create(id);
  AssertTrue('RendererProfile object load error', profile.Load);
  AssertTrue('RendererProfile object ''ID'' is not correct error', 
    profile.ID = id);
  AssertTrue('RendererProfile object ''Background'' is not correct error', 
    profile.Background = clDefault);

  AssertTrue('RendererProfile object ''name'' item ''Name'' is not correct error', 
    profile.Items['name'].Name = 'name');
  AssertTrue('RendererProfile object ''name'' item ''Background'' is not correct error', 
    profile.Items['name'].Background = clDefault);
  AssertTrue('RendererProfile object ''name'' item ''BackgroundFillType'' is not correct error', 
    profile.Items['name'].BackgroundFillType = FILL_NONE);
  AssertTrue('RendererProfile object ''name'' item ''BackgroundRoundRadius'' is not correct error', 
    profile.Items['name'].BackgroundRoundRadius = 10);
  AssertTrue('RendererProfile object ''name'' item ''FontName'' is not correct error', 
    profile.Items['name'].FontName = 'Default');
  AssertTrue('RendererProfile object ''name'' item ''FontSize'' is not correct error', 
    profile.Items['name'].FontSize = 13);
  AssertTrue('RendererProfile object ''name'' item ''FontColor'' is not correct error', 
    profile.Items['name'].FontColor = clBlack);
  AssertTrue('RendererProfile object ''name'' item ''Padding.Top'' is not correct error', 
    profile.Items['name'].Padding.Top = 2);
  AssertTrue('RendererProfile object ''name'' item ''Padding.Left'' is not correct error', 
    profile.Items['name'].Padding.Left = 10);
  AssertTrue('RendererProfile object ''name'' item ''Padding.Bottom'' is not correct error', 
    profile.Items['name'].Padding.Bottom = 2);
  AssertTrue('RendererProfile object ''name'' item ''Padding.Right'' is not correct error', 
    profile.Items['name'].Padding.Right = 10);
  AssertTrue('RendererProfile object ''name'' item ''PositionType'' is not correct error', 
    profile.Items['name'].PositionType = POSITION_FIXED);
  AssertTrue('RendererProfile object ''name'' item ''Posititon.X'' is not correct error',
    profile.Items['name'].Position.X = 15);
  AssertTrue('RendererProfile object ''name'' item ''Posititon.Y'' is not correct error',
    profile.Items['name'].Position.Y = 0);

  FreeAndNil(profile);
end;

procedure TRendererProfileTestCase.Test_RendererProfile_Delete;
var
  profile : TRendererProfile;
  id : Int64;
begin
  profile := TRendererProfile.Create(-1);
  AssertTrue('Database table schema is not correct', profile.CheckSchema);

  profile.Background := clDefault;

  AssertTrue('Object save error', profile.Save);

  id := profile.ID;
  FreeAndNil(profile);

  profile := TRendererProfile.Create(id);
  AssertTrue('RendererProfile object load error', profile.Load);
  AssertTrue('RendererProfile object ''ID'' is not correct error', 
    profile.ID = id);
  AssertTrue('RendererProfile object ''Background'' is not correct error', 
    profile.Background = clDefault);

  AssertTrue('RendererProfile object delete error', profile.Delete);
  AssertTrue('RendererProfile object impossible load', not profile.Load);

  FreeAndNil(profile);
end;

initialization
  RegisterTest(TRendererProfileTestCase);
end.

