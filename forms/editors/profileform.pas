(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(* This is a software for creating schedules  for repair work, accounting and *)
(* monitoring  their  implementation, accounting for the  necessary materials *)
(* and spare parts.                                                           *)
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
unit profileform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, VirtualTrees, renderer.profile.profile,
  renderer.profile.profileitem, renderers.inspector;

type

  { TProfileWindow }

  TProfileWindow = class(TForm)
    PageControl: TPageControl;
    DefaultProfileTabSheet: TTabSheet;
    HoverProfileTabSheet: TTabSheet;
    SelectedProfileTabSheet: TTabSheet;
    DefaultProfileTree: TVirtualDrawTree;
    HoverProfileTree: TVirtualDrawTree;
    SelectedProfileTree: TVirtualDrawTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDefaultRenderer : TProfileInspectorRenderer;
    FHoverRenderer : TProfileInspectorRenderer;
    FSelectedRenderer : TProfileInspectorRenderer;
  public

  end;

var
  ProfileWindow: TProfileWindow;

implementation

{$R *.lfm}

{ TProfileWindow }

procedure TProfileWindow.FormCreate(Sender: TObject);
var
  Profile : TRendererProfile;
begin
  FDefaultRenderer := TProfileInspectorRenderer.Create(False,
    DefaultProfileTree);
  FHoverRenderer := TProfileInspectorRenderer.Create(True, HoverProfileTree);
  FSelectedRenderer := TProfileInspectorRenderer.Create(True,
    SelectedProfileTree);

  Profile := TRendererProfile.Create(-1);
  Profile.Items['Name'] := TRendererProfileItem.Create(-1);
  Profile.Items['Measure'] := TRendererProfileItem.Create(-1);
  FDefaultRenderer.UpdateProfile(Profile);
  FHoverRenderer.UpdateProfile(TRendererProfile.Create(-1));
  FSelectedRenderer.UpdateProfile(TRendererProfile.Create(-1));
end;

procedure TProfileWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDefaultRenderer);
  FreeAndNil(FHoverRenderer);
  FreeAndNil(FSelectedRenderer);
end;

end.

