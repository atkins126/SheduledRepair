unit rendererprofile_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, renderer.profile.profile,
  BGRABitmap, BGRABitmapTypes, renderer.profile.profileitem;

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

  profile.BorderType := BORDER_NONE;
  profile.Border := 2;
  profile.BorderRadius := 5;
  profile.BorderColor := BGRA(128, 128, 128, 128);
  profile.BorderMargin.Top := 5;
  profile.BorderMargin.Left := 5;
  profile.BorderMargin.Bottom := 5;
  profile.BorderMargin.Right := 5;
  profile.Background := BGRA(25, 25, 25, 255);

  item := profile.Items['name'];
  item.Background := BGRA(255, 255, 128, 255);
  item.BackgroundFillType := FILL_NONE;
  item.BackgroundRoundRadius := 10;
  item.FontName := 'Default';
  item.FontSize := 13;
  item.FontColor := BGRA(12, 12, 12, 255);
  item.Padding.Top := 2;
  item.Padding.Left := 10;
  item.Padding.Bottom := 2;
  item.Padding.Right := 10;
  item.PositionType := POSITION_FIXED;
  item.Position.Top := 15;
  item.Position.Left := 0;
  item.Position.Bottom := 0;
  item.Position.Right := 25;

  AssertTrue('Object save error', profile.Save);

  id := profile.ID;
  FreeAndNil(profile);

  profile := TRendererProfile.Create(id);
  AssertTrue('RendererProfile object load error', profile.Load);
  AssertTrue('RendererProfile object ''ID'' is not correct error', 
    profile.ID = id);
  AssertTrue('RendererProfile object ''BorderType'' is not correct error', 
    profile.BorderType = BORDER_NONE);
  AssertTrue('RendererProfile object ''Border'' is not correct error', 
    profile.Border = 2);
  AssertTrue('RendererProfile object ''BorderRadius'' is not correct error', 
    profile.BorderRadius = 5);
  AssertTrue('RendererProfile object ''BorderColor'' is not correct error', 
    profile.BorderColor = BGRA(128, 128, 128, 128));
  AssertTrue('RendererProfile object ''BorderMargin.Top'' is not correct error', 
    profile.BorderMargin.Top = 5);
  AssertTrue('RendererProfile object ''BorderMargin.Left'' is not correct error', 
    profile.BorderMargin.Left = 5);
  AssertTrue('RendererProfile object ''BorderMargin.Bottom'' is not correct error', 
    profile.BorderMargin.Bottom = 5);
  AssertTrue('RendererProfile object ''BorderMargin.Right'' is not correct error', 
    profile.BorderMargin.Right = 5);
  AssertTrue('RendererProfile object ''Background'' is not correct error', 
    profile.Background = BGRA(25, 25, 25, 255));

  AssertTrue('RendererProfile object ''name'' item ''Name'' is not correct error', 
    profile.Items['name'].Name = 'name');
  AssertTrue('RendererProfile object ''name'' item ''Background'' is not correct error', 
    profile.Items['name'].Background = BGRA(255, 255, 128, 255));
  AssertTrue('RendererProfile object ''name'' item ''BackgroundFillType'' is not correct error', 
    profile.Items['name'].BackgroundFillType = FILL_NONE);
  AssertTrue('RendererProfile object ''name'' item ''BackgroundRoundRadius'' is not correct error', 
    profile.Items['name'].BackgroundRoundRadius = 10);
  AssertTrue('RendererProfile object ''name'' item ''FontName'' is not correct error', 
    profile.Items['name'].FontName = 'Default');
  AssertTrue('RendererProfile object ''name'' item ''FontSize'' is not correct error', 
    profile.Items['name'].FontSize = 13);
  AssertTrue('RendererProfile object ''name'' item ''FontColor'' is not correct error', 
    profile.Items['name'].FontColor = BGRA(12, 12, 12, 255));
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
  AssertTrue('RendererProfile object ''name'' item ''Posititon.Top'' is not correct error', 
    profile.Items['name'].Position.Top = 15);
  AssertTrue('RendererProfile object ''name'' item ''Posititon.Left'' is not correct error', 
    profile.Items['name'].Position.Left = 0);
  AssertTrue('RendererProfile object ''name'' item ''Posititon.Bottom'' is not correct error', 
    profile.Items['name'].Position.Bottom = 0);
  AssertTrue('RendererProfile object ''name'' item ''Posititon.Right'' is not correct error', 
    profile.Items['name'].Position.Right = 25);

  FreeAndNil(profile);
end;

procedure TRendererProfileTestCase.Test_RendererProfile_Delete;
var
  profile : TRendererProfile;
  id : Int64;
begin
  profile := TRendererProfile.Create(-1);
  AssertTrue('Database table schema is not correct', profile.CheckSchema);

  profile.BorderType := BORDER_NONE;
  profile.Border := 2;
  profile.BorderRadius := 5;
  profile.BorderColor := BGRA(128, 128, 128, 128);
  profile.BorderMargin.Top := 5;
  profile.BorderMargin.Left := 5;
  profile.BorderMargin.Bottom := 5;
  profile.BorderMargin.Right := 5;
  profile.Background := BGRA(25, 25, 25, 255);

  AssertTrue('Object save error', profile.Save);

  id := profile.ID;
  FreeAndNil(profile);

  profile := TRendererProfile.Create(id);
  AssertTrue('RendererProfile object load error', profile.Load);
  AssertTrue('RendererProfile object ''ID'' is not correct error', 
    profile.ID = id);
  AssertTrue('RendererProfile object ''BorderType'' is not correct error', 
    profile.BorderType = BORDER_NONE);
  AssertTrue('RendererProfile object ''Border'' is not correct error', 
    profile.Border = 2);
  AssertTrue('RendererProfile object ''BorderRadius'' is not correct error', 
    profile.BorderRadius = 5);
  AssertTrue('RendererProfile object ''BorderColor'' is not correct error', 
    profile.BorderColor = BGRA(128, 128, 128, 128));
  AssertTrue('RendererProfile object ''BorderMargin.Top'' is not correct error', 
    profile.BorderMargin.Top = 5);
  AssertTrue('RendererProfile object ''BorderMargin.Left'' is not correct error', 
    profile.BorderMargin.Left = 5);
  AssertTrue('RendererProfile object ''BorderMargin.Bottom'' is not correct error', 
    profile.BorderMargin.Bottom = 5);
  AssertTrue('RendererProfile object ''BorderMargin.Right'' is not correct error', 
    profile.BorderMargin.Right = 5);
  AssertTrue('RendererProfile object ''Background'' is not correct error', 
    profile.Background = BGRA(25, 25, 25, 255));

  AssertTrue('RendererProfile object delete error', profile.Delete);
  AssertTrue('RendererProfile object impossible load', not profile.Load);

  FreeAndNil(profile);
end;

initialization
  RegisterTest(TRendererProfileTestCase);
end.

