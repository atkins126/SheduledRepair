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
  SysUtils, Graphics, renderer.profile.objectprofile, renderer.profile.profile,
  renderer.profile.profileitem, objects.mainmenu.item;

type
  TMainMenuProfilesProvider = class 
    (specialize TCommonProfilesProvider<TMainMenuItem>)
  public
    { Load profiles. }
    function Load : Boolean; override;
  protected
    { Get default object profile. }
    function GetDefaultProfile : TRendererObjectProfile; override;
  end;

implementation

{ TMainMenuProfilesProvider }

function TMainMenuProfilesProvider.GetDefaultProfile : TRendererObjectProfile;
begin
  Result := TRendererObjectProfile.Create(-1);

  { Create main menu item default profile. }
  with Result.DefaultProfile do
  begin
    Enable := True;
    Height := 25;
    Background := clWhite;
  end;

  { Create main menu item default profile items. }
  Result.DefaultProfile.Items['Title'] := TRendererProfileItem.Create(-1);
  
  with Result.DefaultProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundBorderRadius := 0;
    FontName := 'default';
    FontSize := 14;
    FontColor := clBlack;
    Padding.Top := 0;
    Padding.Left := 0;
    Padding.Bottom := 0;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Positioin.Y := 14;    
  end;

  { Create main menu item selected profile. }
  with Result.SelectedProfile do
  begin
    Enable := True;
    Height := 25;
    Background := clYellow;
  end;

  { Create main menu item selected profile items. }
  Result.SelectedProfile.Items['Title'] := TRendererProfileItem.Create(-1);
  
  with Result.SelectedProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundBorderRadius := 0;
    FontName := 'default';
    FontSize := 14;
    FontColor := clBlack;
    Padding.Top := 0;
    Padding.Left := 0;
    Padding.Bottom := 0;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Positioin.Y := 14;    
  end;

  { Create main menu item hover profile. }
  with Result.HoverProfile do
  begin
    Enable := True;
    Height := 25;
    Background := clSilver;
  end;  

  { Create main menu item hover profile items. }
  Result.HoverProfile.Items['Title'] := TRendererProfileItem.Create(-1);
  
  with Result.HoverProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundBorderRadius := 0;
    FontName := 'default';
    FontSize := 14;
    FontColor := clBlack;
    Padding.Top := 0;
    Padding.Left := 0;
    Padding.Bottom := 0;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Positioin.Y := 14;    
  end;
end;

function TMainMenuProfilesProvider.Load : Boolean;
var
  ProfileObject : TRendererObjectProfile;
begin
  ProfileObject := TRendererObjectProfile.Create(-1);  

  { Create item logo default profile. }
  with ProfileObject.DefaultProfile do
  begin
    Enable := True;
    Height := 60;
    Background := clWhite;
  end;
  
  { Create item logo default profile items. }
  ProfileObject.DefaultProfile.Items['Title'] := 
    TRendererProfileItem.Create(-1);
  
  with ProfileObject.DefaultProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundBorderRadius := 0;
    FontName := 'default';
    FontSize := 18;
    FontColor := clBlack;
    Padding.Top := 0;
    Padding.Left := 0;
    Padding.Bottom := 0;
    Padding.Right := 0;
    PositionType := POSITION_FLOAT;
    Position.X := 15;
    Positioin.Y := 45;    
  end;

  { Create item logo selected profile. }
  with ProfileObject.SelectedProfile do
  begin
    Enable := False;
    Height := 60;
    Backgorund := clWhite;
  end;

  { Create item logo selected profile items. }
  ProfileObject.SelectedProfile.Items['Title'] := 
    TRendererProfileItem.Create(-1);
  
  with ProfileObject.SelectedProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundBorderRadius := 0;
    FontName := 'default';
    FontSize := 18;
    FontColor := clBlack;
    Padding.Top := 0;
    Padding.Left := 0;
    Padding.Bottom := 0;
    Padding.Right := 0;
    PositionType := POSITION_FLOAT;
    Position.X := 15;
    Positioin.Y := 45;    
  end;

  { Create item logo hover profile. }
  with ProfileObject.HoverProfile do
  begin
    Enable := False;
    Height := 60;
    Backgorund := clWhite;
  end;

  { Create item logo hover profile items. }
  ProfileObject.HoverProfile.Items['Title'] := 
    TRendererProfileItem.Create(-1);
  
  with ProfileObject.HoverProfile.Items['Title'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundBorderRadius := 0;
    FontName := 'default';
    FontSize := 18;
    FontColor := clBlack;
    Padding.Top := 0;
    Padding.Left := 0;
    Padding.Bottom := 0;
    Padding.Right := 0;
    PositionType := POSITION_FLOAT;
    Position.X := 15;
    Positioin.Y := 45;    
  end;  

  Append(ProfileObject);  

  Result := True;
end;

end.
