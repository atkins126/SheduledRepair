(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(*                                                                            *)
(* Copyright (c) 2020                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/SheduledRepair              ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* This source  is free software;  you can redistribute  it and/or modify  it *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation; either version 3 of the License.                      *)
(*                                                                            *)
(* This code is distributed in the  hope that it will  be useful, but WITHOUT *)
(* ANY  WARRANTY;  without even  the implied  warranty of MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License for *)
(* more details.                                                              *)
(*                                                                            *)
(* A copy  of the  GNU General Public License is available  on the World Wide *)
(* Web at <http://www.gnu.org/copyleft/gpl.html>. You  can also obtain  it by *)
(* writing to the Free Software Foundation, Inc., 51  Franklin Street - Fifth *)
(* Floor, Boston, MA 02110-1335, USA.                                         *)
(*                                                                            *)
(******************************************************************************)
unit profilesprovider.mainmenu;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Graphics, profilesprovider.common, renderer.profile.objectprofile,
  renderer.profile.profile, renderer.profile.profileitem;

type
  TMainMenuProfilesProvider = class (TCommonProfilesProvider)
  public
    { Load profiles. }
    function Load : Boolean; override;
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Get default object profile. }
    function GetDefaultProfile : TRendererObjectProfile; override;
  end;

  TMenuSubitemJobProfilesProvider = class (TCommonProfilesProvider)
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Get default object profile. }
    function GetDefaultProfile : TRendererObjectProfile; override;
  end;

implementation

{ TMainMenuProfilesProvider }

function TMainMenuProfilesProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMainMenuProfilesProvider.GetDefaultProfile : TRendererObjectProfile;
begin
  Result := TRendererObjectProfile.Create(-1, nil);

  { Create main menu item default profile. }
  with Result.DefaultProfile do
  begin
    Enable := True;
    Height := 35;
    Background := clWhite;
  end;

  with Result.DefaultProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 12;
    FontColor := clBlack;
    Padding.Top := 5;
    Padding.Left := 0;
    Padding.Bottom := 5;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 2;
  end;

  with Result.DefaultProfile.Items['SelectionTitle'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 10;
    FontColor := clBlack;
    Padding.Top := 1;
    Padding.Left := 0;
    Padding.Bottom := 1;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 0;
  end;

  with Result.DefaultProfile.Items['SelectionName'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 8;
    FontColor := $777777;
    Padding.Top := 1;
    Padding.Left := 0;
    Padding.Bottom := 1;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 18;
  end;

  { Create main menu item selected profile. }
  with Result.SelectedProfile do
  begin
    Enable := True;
    Height := 35;
    Background := clYellow;
  end;

  with Result.SelectedProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 12;
    FontColor := clBlack;
    Padding.Top := 5;
    Padding.Left := 0;
    Padding.Bottom := 5;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 2;
  end;

  with Result.SelectedProfile.Items['SelectionTitle'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 10;
    FontColor := clBlack;
    Padding.Top := 1;
    Padding.Left := 0;
    Padding.Bottom := 1;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 0;
  end;

  with Result.SelectedProfile.Items['SelectionName'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 8;
    FontColor := $777777;
    Padding.Top := 1;
    Padding.Left := 0;
    Padding.Bottom := 1;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 18;
  end;

  { Create main menu item hover profile. }
  with Result.HoverProfile do
  begin
    Enable := True;
    Height := 35;
    Background := clSilver;
  end;  

  with Result.HoverProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 12;
    FontColor := clBlack;
    Padding.Top := 5;
    Padding.Left := 0;
    Padding.Bottom := 5;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 2;
  end;

  with Result.HoverProfile.Items['SelectionTitle'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 10;
    FontColor := clBlack;
    Padding.Top := 1;
    Padding.Left := 0;
    Padding.Bottom := 1;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 0;
  end;

  with Result.HoverProfile.Items['SelectionName'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 8;
    FontColor := $777777;
    Padding.Top := 1;
    Padding.Left := 0;
    Padding.Bottom := 1;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 18;
  end;
end;

function TMainMenuProfilesProvider.Load : Boolean;
var
  ProfileObject : TRendererObjectProfile;
begin
  Clear;

  ProfileObject := TRendererObjectProfile.Create(-1, nil);

  { Create item logo default profile. }
  with ProfileObject.DefaultProfile do
  begin
    Enable := True;
    Height := 60;
    Background := clWhite;
  end;
  
  with ProfileObject.DefaultProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 18;
    FontColor := clBlack;
    Padding.Top := 0;
    Padding.Left := 0;
    Padding.Bottom := 0;
    Padding.Right := 0;
    PositionType := POSITION_FLOAT;
    Position.X := 20;
    Position.Y := 30;
  end;

  { Create item logo selected profile. }
  with ProfileObject.SelectedProfile do
  begin
    Enable := False;
    Height := 60;
    Background := clWhite;
  end;

  with ProfileObject.SelectedProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 18;
    FontColor := clBlack;
    Padding.Top := 0;
    Padding.Left := 0;
    Padding.Bottom := 0;
    Padding.Right := 0;
    PositionType := POSITION_FLOAT;
    Position.X := 20;
    Position.Y := 30;
  end;

  { Create item logo hover profile. }
  with ProfileObject.HoverProfile do
  begin
    Enable := False;
    Height := 60;
    Background := clWhite;
  end;

  with ProfileObject.HoverProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 18;
    FontColor := clBlack;
    Padding.Top := 0;
    Padding.Left := 0;
    Padding.Bottom := 0;
    Padding.Right := 0;
    PositionType := POSITION_FLOAT;
    Position.X := 20;
    Position.Y := 30;
  end;  

  Append(ProfileObject);  

  Result := True;
end;

{ TMenuSubitemJobProfilesProvider }

function TMenuSubitemJobProfilesProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TMenuSubitemJobProfilesProvider.GetDefaultProfile : 
  TRendererObjectProfile;
begin
  Result := TRendererObjectProfile.Create(-1, nil);

  { Create menu subitem job default profile. }
  with Result.DefaultProfile do
  begin
    Enable := True;
    Height := 25;
    Background := clWhite;
  end;

  with Result.DefaultProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 10;
    FontColor := clBlack;
    Padding.Top := 2;
    Padding.Left := 15;
    Padding.Bottom := 5;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 2;
  end;

  { Create menu subitem job selected profile. }
  with Result.SelectedProfile do
  begin
    Enable := True;
    Height := 25;
    Background := clYellow;
  end;

  with Result.SelectedProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 10;
    FontColor := clBlack;
    Padding.Top := 2;
    Padding.Left := 15;
    Padding.Bottom := 5;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 2;
  end;

  { Create menu subitem job hover profile. }
  with Result.HoverProfile do
  begin
    Enable := True;
    Height := 25;
    Background := clSilver;
  end;  

  with Result.HoverProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 10;
    FontColor := clBlack;
    Padding.Top := 2;
    Padding.Left := 15;
    Padding.Bottom := 5;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 2;
  end;
end;

end.
