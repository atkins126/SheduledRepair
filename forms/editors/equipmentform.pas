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
unit equipmentform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, Buttons, objects.equipment;

type

  { TEquipmentWindow }

  TEquipmentWindow = class(TForm)
    CancelButton: TBitBtn;
    DeleteButton: TBitBtn;
    NameEditor: TEdit;
    NameEditorGroup: TPanel;
    NameGroup: TPanel;
    NameLabel: TLabel;
    NameLabelGroup: TPanel;
    ObjectGroup: TGroupBox;
    SaveButton: TBitBtn;
    procedure DeleteButtonClick(Sender: TObject);
    procedure NameEditorChange(Sender: TObject);
  private
    FObject : TEquipment;
  public
    constructor Create(TheOwner: TComponent; AObject : TEquipment); reintroduce;
    function GetObject : TEquipment;
  end;

var
  EquipmentWindow: TEquipmentWindow;

implementation

uses
  configuration;

{$R *.lfm}

{ TEquipmentWindow }

procedure TEquipmentWindow.DeleteButtonClick(Sender: TObject);
begin
  if FObject.ID <> -1 then
    FObject.Delete;
end;

procedure TEquipmentWindow.NameEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := NameEditor.Text <> '';
end;

constructor TEquipmentWindow.Create(TheOwner: TComponent; AObject : TEquipment);
begin
  inherited Create(TheOwner);
  FObject := AObject;

  ObjectGroup.Caption := Config.GetValue('Object', 'Object');
  NameLabel.Caption := Config.GetValue('Name', 'Name');

  SaveButton.Caption := Config.GetValue('Save', 'Save');
  CancelButton.Caption := Config.GetValue('Cancel', 'Cancel');
  DeleteButton.Caption := Config.GetValue('Delete', 'Delete');

  NameEditor.Text := FObject.Name;

  SaveButton.Enabled := NameEditor.Text <> '';
  DeleteButton.Visible := (FObject <> nil) and (FObject.ID <> -1);
end;

function TEquipmentWindow.GetObject : TEquipment;
begin
  if FObject.ID <> -1 then
    Result := FObject
  else
    Result := TEquipment.Create(-1);

  Result.Name := NameEditor.Text;
end;

end.

