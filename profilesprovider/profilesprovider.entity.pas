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
unit profilesprovider.entity;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Graphics, profilesprovider.common, renderer.profile.objectprofile,
  renderer.profile.profile, renderer.profile.profileitem;

type
  TEntityProfilesProvider = class (TCommonProfilesProvider)
  protected
    { Get current loaded objects table name. }
    function LoadObjectsTableName : String; override;

    { Get default object profile. }
    function GetDefaultProfile : TRendererObjectProfile; override;
  end;

implementation

{ TEntityProfilesProvider }

function TEntityProfilesProvider.LoadObjectsTableName : String;
begin
  Result := '';
end;

function TEntityProfilesProvider.GetDefaultProfile : TRendererObjectProfile;
begin
  Result := TRendererObjectProfile.Create(-1, nil);

  { Create entity default profile. }
  with Result.DefaultProfile do
  begin
    Enable := True;
    Height := 20;
    Background := clWhite;
  end;

  { Create entity default profile items. }
  Result.DefaultProfile.Items['Name'] := TRendererProfileItem.Create(-1);
  
  with Result.DefaultProfile.Items['Name'] do
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

  with Result.DefaultProfile.Items['Quantity'] do
  begin
    Enable := False;
  end;

  { Create entity selected profile. }
  with Result.SelectedProfile do
  begin
    Enable := True;
    Height := 60;
    Background := clYellow;
  end;

  { Create entity selected profile items. }
  Result.SelectedProfile.Items['Name'] := TRendererProfileItem.Create(-1);
  
  with Result.SelectedProfile.Items['Name'] do
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

  with Result.SelectedProfile.Items['Quantity'] do
  begin
    Enable := True;
    Background := clWhite;
    BackgroundFillType := FILL_NONE;
    BackgroundRoundRadius := 0;
    FontName := 'default';
    FontSize := 10;
    FontColor := clBlack;
    Padding.Top := 28;
    Padding.Left := 0;
    Padding.Bottom := 0;
    Padding.Right := 0;
    PositionType := POSITION_FIXED;
    Position.X := 15;
    Position.Y := 2;
  end;

  { Create entity hover profile. }
  with Result.HoverProfile do
  begin
    Enable := True;
    Height := 20;
    Background := clSilver;
  end;  

  { Create job hover profile items. }
  Result.HoverProfile.Items['Name'] := TRendererProfileItem.Create(-1);
  
  with Result.HoverProfile.Items['Name'] do
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

  with Result.HoverProfile.Items['Quantity'] do
  begin
    Enable := False;
  end;
end;

end.