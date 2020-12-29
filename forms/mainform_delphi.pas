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
unit mainform_delphi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, 
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, 
  VirtualTrees, Vcl.ExtCtrls, dataprovider, mainmenuprovider, objects.equipment, 
  objects.job, Vcl.ComCtrls;

type
  TMainWindow = class(TForm)
    MainMenuView: TVirtualDrawTree;
    MainMenuSplitter: TSplitter;
    SettingsView: TVirtualDrawTree;
    SettingsSplitter: TSplitter;
    ContentView: TVirtualDrawTree;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainWindow: TMainWindow;

implementation

uses
  configuration;

{$R *.dfm}

procedure TMainWindow.FormCreate(Sender: TObject);
var
  Equipment : TEquipment;
  Job : TJob;
begin
  Equipment := TEquipment.Create(-1);
  Equipment.CheckSchema;
  FreeAndNil(Equipment);
  Job := TJob.Create(-1);
  Job.CheckSchema;
  FreeAndNil(Job);
  Config.CheckSchema;
  Config.Load;

  MainMenu.View := MainMenuView;

  Provider.Parent := Self;
  Provider.DataView := ContentView;
end;

end.
