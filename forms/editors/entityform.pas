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
unit entityform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, EditBtn, Buttons, objects.entity, objects.quantity, objects.period,
  objects.measure, objects.equipment, dataproviders.measure, objects.common;

type

  { TEntityWindow }

  TEntityWindow = class(TForm)
    CancelButton: TBitBtn;
    DeleteButton: TBitBtn;
    NameEditor: TEdit;
    NameEditorGroup: TPanel;
    NameGroup: TPanel;
    NameLabel: TLabel;
    NameLabelGroup: TPanel;
    ObjectGroup: TGroupBox;
    PeriodCountEditor: TFloatSpinEdit;
    PeriodEditorGroup: TPanel;
    PeriodGroup: TPanel;
    PeriodLabel: TLabel;
    PeriodLabelGroup: TPanel;
    PeriodMeasureEditor: TComboBox;
    QuantityCountEditor: TFloatSpinEdit;
    QuantityEditorGroup: TPanel;
    QuantityGroup: TPanel;
    QuantityLabel: TLabel;
    QuantityLabelGroup: TPanel;
    QuantityMeasureEditor: TComboBox;
    SaveButton: TBitBtn;
    SheduleGroup: TGroupBox;
    SheduleNextEditor: TDateEdit;
    SheduleNextEditorGroup: TPanel;
    SheduleNextGroup: TPanel;
    SheduleNextNameLabel: TLabel;
    SheduleNextNameLabelGroup: TPanel;
    ShedulePrevEditor: TDateEdit;
    ShedulePrevEditorGroup: TPanel;
    ShedulePrevGroup: TPanel;
    ShedulePrevNameLabel: TLabel;
    ShedulePrevNameLabelGroup: TPanel;
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NameEditorChange(Sender: TObject);
  private
    FEquipment : TEquipment;
    FObject : TEntity;
    MeasureDataProvider : TMeasureDataProvider;
  public
    constructor Create(TheOwner: TComponent; AEquipment : TEquipment; AObject :
      TEntity); reintroduce;
    function GetObject : TEntity;
  end;

var
  EntityWindow: TEntityWindow;

implementation

uses
  configuration;

{$R *.lfm}

procedure TEntityWindow.DeleteButtonClick(Sender: TObject);
begin
  if FEquipment.ID <> -1 then
    FEquipment.EntityBag.Remove(FObject);

  if FObject.ID <> -1 then
    FObject.Delete;
end;

procedure TEntityWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MeasureDataProvider);
end;

procedure TEntityWindow.NameEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := NameEditor.Text <> '';
end;

constructor TEntityWindow.Create (TheOwner : TComponent; AEquipment :
  TEquipment; AObject : TEntity);
var
  Measure : TCommonObject;
  MeasureIndex : Integer;
  Index : Integer;
  MeasureLoad : Boolean;
begin
  inherited Create(TheOwner);
  FEquipment := AEquipment;
  FObject := AObject;

  ObjectGroup.Caption := Config.GetValue('Object', 'Object');
  NameLabel.Caption := Config.GetValue('Name', 'Name');
  QuantityLabel.Caption := Config.GetValue('Quantity', 'Quantity');
  PeriodLabel.Caption := Config.GetValue('Period', 'Period');

  SheduleGroup.Caption := Config.GetValue('Shedule', 'Shedule');
  ShedulePrevNameLabel.Caption := Config.GetValue('Previous date',
    'Previous date');
  SheduleNextNameLabel.Caption := Config.GetValue('Next date',
    'Next date');

  SaveButton.Caption := Config.GetValue('Save', 'Save');
  CancelButton.Caption := Config.GetValue('Cancel', 'Cancel');
  DeleteButton.Caption := Config.GetValue('Delete', 'Delete');

  MeasureDataProvider := TMeasureDataProvider.Create;
  MeasureLoad := MeasureDataProvider.Load;

  NameEditor.Text := FObject.Name;
  QuantityCountEditor.Value := FObject.Quantity.Count;

  if MeasureLoad then
  begin
    QuantityMeasureEditor.Items.Clear;

    Index := 0;
    MeasureIndex := -1;
    for Measure in MeasureDataProvider do
    begin
      QuantityMeasureEditor.Items.Add(TMeasure(Measure).Name);

      if (FObject <> nil) and (TMeasure(Measure).ID =
        FObject.Quantity.Measure.ID) then
        MeasureIndex := Index;

      Inc(Index);
    end;

    QuantityMeasureEditor.ItemIndex := MeasureIndex;
  end;

  PeriodCountEditor.Value := FObject.Period.Quantity.Count;

  if MeasureLoad then
  begin
    PeriodMeasureEditor.Items.Clear;

    Index := 0;
    MeasureIndex := -1;
    for Measure in MeasureDataProvider do
    begin
      PeriodMeasureEditor.Items.Add(TMeasure(Measure).Name);

      if (FObject <> nil) and (TMeasure(Measure).ID =
        FObject.Period.Quantity.Measure.ID) then
        MeasureIndex := Index;

      Inc(Index);
    end;

    PeriodMeasureEditor.ItemIndex := MeasureIndex;
  end;

  ShedulePrevEditor.Date := FObject.Shedule.PrevDate;
  SheduleNextEditor.Date := FObject.Shedule.NextDate;

  SaveButton.Enabled := (NameEditor.Text <> '');
  DeleteButton.Visible := (FObject <> nil) and (FObject.ID <> -1);
end;

function TEntityWindow.GetObject : TEntity;
begin
  Result := FObject;

  Result.Name := NameEditor.Text;
  Result.Quantity.Count := QuantityCountEditor.Value;

  if QuantityMeasureEditor.ItemIndex <> -1 then
  begin
    Result.Quantity.Measure := TMeasure(MeasureDataProvider.GetObject(
      QuantityMeasureEditor.ItemIndex));
  end else
  begin
    Result.Quantity.Measure.Name := QuantityMeasureEditor.Text;
  end;

  Result.Period.Quantity.Count := PeriodCountEditor.Value;

  if PeriodMeasureEditor.ItemIndex <> -1 then
  begin
    Result.Period.Quantity.Measure := TMeasure(MeasureDataProvider.GetObject(
      PeriodMeasureEditor.ItemIndex));
  end else
  begin
    Result.Period.Quantity.Measure.Name := PeriodMeasureEditor.Text;
  end;

  Result.Shedule.PrevDate := ShedulePrevEditor.Date;
  Result.Shedule.NextDate := SheduleNextEditor.Date;
end;

end.

