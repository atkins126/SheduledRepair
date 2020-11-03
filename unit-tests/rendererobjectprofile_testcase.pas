unit rendererobjectprofile_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpcunit, testregistry, renderer.profile.profile,
  renderer.profile.objectprofile, renderer.profile.profileitem;

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
  item : TRendererProfileItem;
  id : Int64;
begin
  obj_profile := TRendererObjectProfile.Create(-1);
  AssertTrue('Database table schema is not correct', obj_profile.CheckSchema);

  obj_profile.DefaultProfile.Background := clDefault;
  obj_profile.DefaultProfile.Height := 25;

  item := obj_profile.DefaultProfile.Items['test'];
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

  item := obj_profile.DefaultProfile.Items['some_item'];
  item.Background := clRed;
  item.BackgroundFillType := FILL_SQUARE;
  item.BackgroundRoundRadius := 8;
  item.FontName := 'Default';
  item.FontSize := 11;
  item.FontColor := clBlack;
  item.Padding.Top := 1;
  item.Padding.Left := 9;
  item.Padding.Bottom := 2;
  item.Padding.Right := 15;
  item.PositionType := POSITION_FLOAT;
  item.Position.X := 5;
  item.Position.Y := 4;

  obj_profile.SelectedProfile.Background := clDefault;
  obj_profile.SelectedProfile.Height := 40;

  item := obj_profile.SelectedProfile.Items['test'];
  item.Background := clCream;
  item.BackgroundFillType := FILL_SQUARE_ROUND_CORNER;
  item.BackgroundRoundRadius := 2;
  item.FontName := 'Default';
  item.FontSize := 8;
  item.FontColor := clYellow;
  item.Padding.Top := 0;
  item.Padding.Left := 11;
  item.Padding.Bottom := 4;
  item.Padding.Right := 42;
  item.PositionType := POSITION_FIXED;
  item.Position.X := 12;
  item.Position.Y := 1;

  item := obj_profile.SelectedProfile.Items['item'];
  item.Background := clSkyBlue;
  item.BackgroundFillType := FILL_NONE;
  item.BackgroundRoundRadius := 1;
  item.FontName := 'Default';
  item.FontSize := 45;
  item.FontColor := clYellow;
  item.Padding.Top := 1;
  item.Padding.Left := 13;
  item.Padding.Bottom := 3;
  item.Padding.Right := 42;
  item.PositionType := POSITION_FIXED;
  item.Position.X := 121;
  item.Position.Y := 2;

  obj_profile.HoverProfile.Background := clWhite;
  obj_profile.HoverProfile.Height := 82;

  item := obj_profile.HoverProfile.Items['text'];
  item.Background := clTeal;
  item.BackgroundFillType := FILL_NONE;
  item.BackgroundRoundRadius := 1;
  item.FontName := 'Default';
  item.FontSize := 12;
  item.FontColor := clBlue;
  item.Padding.Top := 1;
  item.Padding.Left := 12;
  item.Padding.Bottom := 1;
  item.Padding.Right := 0;
  item.PositionType := POSITION_FIXED;
  item.Position.X := 0;
  item.Position.Y := 0;

  item := obj_profile.HoverProfile.Items['value'];
  item.Background := clDkGray;
  item.BackgroundFillType := FILL_NONE;
  item.BackgroundRoundRadius := 1;
  item.FontName := 'Default';
  item.FontSize := 12;
  item.FontColor := clBlue;
  item.Padding.Top := 1;
  item.Padding.Left := 12;
  item.Padding.Bottom := 1;
  item.Padding.Right := 0;
  item.PositionType := POSITION_FIXED;
  item.Position.X := 0;
  item.Position.Y := 0;

  AssertTrue('Object save error', obj_profile.Save);

  id := obj_profile.ID;
  FreeAndNil(obj_profile);

  obj_profile := TRendererObjectProfile.Create(id);
  AssertTrue('RendererObjectProfile object load error', obj_profile.Load);
  AssertTrue('RendererObjectProfile object ''ID'' is not correct error', 
    obj_profile.ID = id);
  AssertTrue('RendererObjectProfile object ''DefaultProfile.Background'' is not correct error',
    obj_profile.DefaultProfile.Background = clDefault);
  AssertTrue('RendererObjectProfile object ''DefaultProfile.Height'' is not correct error',
    obj_profile.DefaultProfile.Height = 25);

  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Background'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Background = clDefault);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.BackgroundFillType'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].BackgroundFillType = FILL_NONE);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.BackgroundRoundRadius'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].BackgroundRoundRadius = 10);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.FontName'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.FontSize'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].FontSize = 13);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.FontColor'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].FontColor = clBlack);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Padding.Top'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Padding.Top = 2);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Padding.Left'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Padding.Left = 10);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Padding.Bottom'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Padding.Bottom = 2);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Padding.Right'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Padding.Right = 10);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.PositionType'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].PositionType = POSITION_FIXED);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Position.X'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Position.X = 15);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Position.Y'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Position.Y = 0);
  
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Background'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Background = clRed);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.BackgroundFillType'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].BackgroundFillType = FILL_SQUARE);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.BackgroundRoundRadius'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].BackgroundRoundRadius = 8);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.FontName'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.FontSize'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].FontSize = 11);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.FontColor'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].FontColor = clBlack);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Padding.Top'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Padding.Top = 1);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Padding.Left'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Padding.Left = 9);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Padding.Bottom'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Padding.Bottom = 2);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Padding.Right'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Padding.Right = 15);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.PositionType'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].PositionType = POSITION_FLOAT);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Position.X'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Position.X = 5);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Position.Y'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Position.Y = 4);

  AssertTrue('RendererObjectProfile object ''SelectedProfile.Background'' is not correct error',
    obj_profile.SelectedProfile.Background = clDefault);
  AssertTrue('RendererObjectProfile object ''SelectedProfile.Height'' is not correct error',
    obj_profile.SelectedProfile.Height = 40);

  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Background'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Background = clCream);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.BackgroundFillType'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].BackgroundFillType = FILL_SQUARE_ROUND_CORNER);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.BackgroundRoundRadius'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].BackgroundRoundRadius = 2);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.FontName'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.FontSize'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].FontSize = 8);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.FontColor'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].FontColor = clYellow);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Padding.Top'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Padding.Top = 0);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Padding.Left'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Padding.Left = 11);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Padding.Bottom'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Padding.Bottom = 4);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Padding.Right'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Padding.Right = 42);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.PositionType'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].PositionType = POSITION_FIXED);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Position.X'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Position.X = 12);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Position.Y'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Position.Y = 1);

  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Background'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Background = clSkyBlue);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.BackgroundFillType'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].BackgroundFillType = FILL_NONE);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.BackgroundRoundRadius'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].BackgroundRoundRadius = 1);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.FontName'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.FontSize'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].FontSize = 45);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.FontColor'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].FontColor = clYellow);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Padding.Top'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Padding.Top = 1);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Padding.Left'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Padding.Left = 13);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Padding.Bottom'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Padding.Bottom = 3);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Padding.Right'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Padding.Right = 42);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.PositionType'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].PositionType = POSITION_FIXED);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Position.X'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Position.X = 121);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Position.Y'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Position.Y = 2);

  AssertTrue('RendererObjectProfile object ''HoverProfile.Background'' is not correct error',
    obj_profile.HoverProfile.Background = clWhite);
  AssertTrue('RendererObjectProfile object ''HoverProfile.Height'' is not correct error',
    obj_profile.HoverProfile.Height = 82);

  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Background'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Background = clTeal);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.BackgroundFillType'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].BackgroundFillType = FILL_NONE);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.BackgroundRoundRadius'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].BackgroundRoundRadius = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.FontName'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.FontSize'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].FontSize = 12);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.FontColor'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].FontColor = clBlue);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Padding.Top'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Padding.Top = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Padding.Left'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Padding.Left = 12);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Padding.Bottom'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Padding.Bottom = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Padding.Right'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Padding.Right = 0);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.PositionType'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].PositionType = POSITION_FIXED);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Position.X'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Position.X = 0);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Position.Y'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Position.Y = 0);

  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Background'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Background = clDkGray);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.BackgroundFillType'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].BackgroundFillType = FILL_NONE);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.BackgroundRoundRadius'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].BackgroundRoundRadius = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.FontName'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.FontSize'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].FontSize = 12);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.FontColor'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].FontColor = clBlue);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Padding.Top'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Padding.Top = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Padding.Left'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Padding.Left = 12);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Padding.Bottom'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Padding.Bottom = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Padding.Right'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Padding.Right = 0);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.PositionType'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].PositionType = POSITION_FIXED);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Position.X'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Position.X = 0);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Position.Y'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Position.Y = 0); 

  FreeAndNil(obj_profile);  
end;

procedure TRendererObjectProfileTestCase.Test_RendererObjectProfile_Delete;
var
  obj_profile : TRendererObjectProfile;
  item : TRendererProfileItem;
  id : Int64;
begin
  obj_profile := TRendererObjectProfile.Create(-1);
  AssertTrue('Database table schema is not correct', obj_profile.CheckSchema);

  obj_profile.DefaultProfile.Background := clDefault;
  obj_profile.DefaultProfile.Height := 25;

  item := obj_profile.DefaultProfile.Items['test'];
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

  item := obj_profile.DefaultProfile.Items['some_item'];
  item.Background := clRed;
  item.BackgroundFillType := FILL_SQUARE;
  item.BackgroundRoundRadius := 8;
  item.FontName := 'Default';
  item.FontSize := 11;
  item.FontColor := clBlack;
  item.Padding.Top := 1;
  item.Padding.Left := 9;
  item.Padding.Bottom := 2;
  item.Padding.Right := 15;
  item.PositionType := POSITION_FLOAT;
  item.Position.X := 5;
  item.Position.Y := 4;

  obj_profile.SelectedProfile.Background := clDefault;
  obj_profile.SelectedProfile.Height := 40;

  item := obj_profile.SelectedProfile.Items['test'];
  item.Background := clCream;
  item.BackgroundFillType := FILL_SQUARE_ROUND_CORNER;
  item.BackgroundRoundRadius := 2;
  item.FontName := 'Default';
  item.FontSize := 8;
  item.FontColor := clYellow;
  item.Padding.Top := 0;
  item.Padding.Left := 11;
  item.Padding.Bottom := 4;
  item.Padding.Right := 42;
  item.PositionType := POSITION_FIXED;
  item.Position.X := 12;
  item.Position.Y := 1;

  item := obj_profile.SelectedProfile.Items['item'];
  item.Background := clSkyBlue;
  item.BackgroundFillType := FILL_NONE;
  item.BackgroundRoundRadius := 1;
  item.FontName := 'Default';
  item.FontSize := 45;
  item.FontColor := clYellow;
  item.Padding.Top := 1;
  item.Padding.Left := 13;
  item.Padding.Bottom := 3;
  item.Padding.Right := 42;
  item.PositionType := POSITION_FIXED;
  item.Position.X := 121;
  item.Position.Y := 2;

  obj_profile.HoverProfile.Background := clWhite;
  obj_profile.HoverProfile.Height := 82;

  item := obj_profile.HoverProfile.Items['text'];
  item.Background := clTeal;
  item.BackgroundFillType := FILL_NONE;
  item.BackgroundRoundRadius := 1;
  item.FontName := 'Default';
  item.FontSize := 12;
  item.FontColor := clBlue;
  item.Padding.Top := 1;
  item.Padding.Left := 12;
  item.Padding.Bottom := 1;
  item.Padding.Right := 0;
  item.PositionType := POSITION_FIXED;
  item.Position.X := 0;
  item.Position.Y := 0;

  item := obj_profile.HoverProfile.Items['value'];
  item.Background := clDkGray;
  item.BackgroundFillType := FILL_NONE;
  item.BackgroundRoundRadius := 1;
  item.FontName := 'Default';
  item.FontSize := 12;
  item.FontColor := clBlue;
  item.Padding.Top := 1;
  item.Padding.Left := 12;
  item.Padding.Bottom := 1;
  item.Padding.Right := 0;
  item.PositionType := POSITION_FIXED;
  item.Position.X := 0;
  item.Position.Y := 0;

  AssertTrue('Object save error', obj_profile.Save);

  id := obj_profile.ID;
  FreeAndNil(obj_profile);

  obj_profile := TRendererObjectProfile.Create(id);
  AssertTrue('RendererObjectProfile object load error', obj_profile.Load);
  AssertTrue('RendererObjectProfile object ''ID'' is not correct error', 
    obj_profile.ID = id);
  AssertTrue('RendererObjectProfile object ''DefaultProfile.Background'' is not correct error',
    obj_profile.DefaultProfile.Background = clDefault);
  AssertTrue('RendererObjectProfile object ''DefaultProfile.Height'' is not correct error',
    obj_profile.DefaultProfile.Height = 25);

  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Background'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Background = clDefault);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.BackgroundFillType'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].BackgroundFillType = FILL_NONE);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.BackgroundRoundRadius'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].BackgroundRoundRadius = 10);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.FontName'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.FontSize'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].FontSize = 13);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.FontColor'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].FontColor = clBlack);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Padding.Top'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Padding.Top = 2);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Padding.Left'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Padding.Left = 10);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Padding.Bottom'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Padding.Bottom = 2);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Padding.Right'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Padding.Right = 10);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.PositionType'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].PositionType = POSITION_FIXED);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Position.X'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Position.X = 15);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''test.Position.Y'' is not correct error', 
    obj_profile.DefaultProfile.Items['test'].Position.Y = 0);
  
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Background'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Background = clRed);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.BackgroundFillType'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].BackgroundFillType = FILL_SQUARE);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.BackgroundRoundRadius'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].BackgroundRoundRadius = 8);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.FontName'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.FontSize'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].FontSize = 11);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.FontColor'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].FontColor = clBlack);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Padding.Top'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Padding.Top = 1);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Padding.Left'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Padding.Left = 9);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Padding.Bottom'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Padding.Bottom = 2);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Padding.Right'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Padding.Right = 15);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.PositionType'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].PositionType = POSITION_FLOAT);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Position.X'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Position.X = 5);
  AssertTrue('RendererObjectProfile object ''DefaultProfile'' item ''some_item.Position.Y'' is not correct error', 
    obj_profile.DefaultProfile.Items['some_item'].Position.Y = 4);

  AssertTrue('RendererObjectProfile object ''SelectedProfile.Background'' is not correct error',
    obj_profile.SelectedProfile.Background = clDefault);
  AssertTrue('RendererObjectProfile object ''SelectedProfile.Height'' is not correct error',
    obj_profile.SelectedProfile.Height = 40);

  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Background'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Background = clCream);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.BackgroundFillType'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].BackgroundFillType = FILL_SQUARE_ROUND_CORNER);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.BackgroundRoundRadius'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].BackgroundRoundRadius = 2);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.FontName'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.FontSize'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].FontSize = 8);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.FontColor'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].FontColor = clYellow);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Padding.Top'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Padding.Top = 0);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Padding.Left'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Padding.Left = 11);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Padding.Bottom'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Padding.Bottom = 4);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Padding.Right'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Padding.Right = 42);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.PositionType'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].PositionType = POSITION_FIXED);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Position.X'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Position.X = 12);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''test.Position.Y'' is not correct error', 
    obj_profile.SelectedProfile.Items['test'].Position.Y = 1);

  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Background'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Background = clSkyBlue);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.BackgroundFillType'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].BackgroundFillType = FILL_NONE);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.BackgroundRoundRadius'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].BackgroundRoundRadius = 1);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.FontName'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.FontSize'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].FontSize = 45);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.FontColor'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].FontColor = clYellow);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Padding.Top'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Padding.Top = 1);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Padding.Left'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Padding.Left = 13);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Padding.Bottom'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Padding.Bottom = 3);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Padding.Right'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Padding.Right = 42);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.PositionType'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].PositionType = POSITION_FIXED);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Position.X'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Position.X = 121);
  AssertTrue('RendererObjectProfile object ''SelectedProfile'' item ''item.Position.Y'' is not correct error', 
    obj_profile.SelectedProfile.Items['item'].Position.Y = 2);

  AssertTrue('RendererObjectProfile object ''HoverProfile.Background'' is not correct error',
    obj_profile.HoverProfile.Background = clWhite);
  AssertTrue('RendererObjectProfile object ''HoverProfile.Height'' is not correct error',
    obj_profile.HoverProfile.Height = 82);

  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Background'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Background = clTeal);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.BackgroundFillType'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].BackgroundFillType = FILL_NONE);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.BackgroundRoundRadius'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].BackgroundRoundRadius = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.FontName'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.FontSize'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].FontSize = 12);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.FontColor'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].FontColor = clBlue);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Padding.Top'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Padding.Top = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Padding.Left'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Padding.Left = 12);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Padding.Bottom'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Padding.Bottom = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Padding.Right'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Padding.Right = 0);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.PositionType'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].PositionType = POSITION_FIXED);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Position.X'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Position.X = 0);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''text.Position.Y'' is not correct error', 
    obj_profile.HoverProfile.Items['text'].Position.Y = 0);

  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Background'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Background = clDkGray);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.BackgroundFillType'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].BackgroundFillType = FILL_NONE);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.BackgroundRoundRadius'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].BackgroundRoundRadius = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.FontName'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].FontName = 'Default');
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.FontSize'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].FontSize = 12);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.FontColor'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].FontColor = clBlue);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Padding.Top'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Padding.Top = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Padding.Left'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Padding.Left = 12);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Padding.Bottom'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Padding.Bottom = 1);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Padding.Right'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Padding.Right = 0);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.PositionType'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].PositionType = POSITION_FIXED);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Position.X'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Position.X = 0);
  AssertTrue('RendererObjectProfile object ''HoverProfile'' item ''value.Position.Y'' is not correct error', 
    obj_profile.HoverProfile.Items['value'].Position.Y = 0); 

  AssertTrue('RendererObjectProfile object delete error', obj_profile.Delete);
  AssertTrue('RendererObjectProfile object impossible load', 
    not obj_profile.Load);

  FreeAndNil(obj_profile); 
end;

initialization
  RegisterTest(TRendererObjectProfileTestCase);
end.

