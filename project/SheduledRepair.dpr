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
program SheduledRepair;

uses
  Vcl.Forms,
  mainform_delphi in '..\forms\mainform_delphi.pas' {MainWindow},
  jobform_delphi in '..\forms\editors\jobform_delphi.pas' {JobWindow},
  equipmentform_delphi in '..\forms\editors\equipmentform_delphi.pas' {EquipmentWindow},
  entityform_delphi in '..\forms\editors\entityform_delphi.pas' {EntityWindow},
  nodeform_delphi in '..\forms\editors\nodeform_delphi.pas' {NodeWindow},
  greasebundleform_delphi in '..\forms\editors\greasebundleform_delphi.pas' {GreaseBundleWindow};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TJobWindow, JobWindow);
  Application.CreateForm(TEquipmentWindow, EquipmentWindow);
  Application.CreateForm(TEntityWindow, EntityWindow);
  Application.CreateForm(TNodeWindow, NodeWindow);
  Application.CreateForm(TGreaseBundleWindow, GreaseBundleWindow);
  Application.Run;
end.
