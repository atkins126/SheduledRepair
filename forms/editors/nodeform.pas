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
unit nodeform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, EditBtn, Buttons, objects.entity, objects.node, objects.common,
  objects.measure, dataproviders.measure;

type

  { TNodeWindow }

  TNodeWindow = class(TForm)
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
    FEntity : TEntity;
    FObject : TNode;
    FMeasureDataProvider : TMeasureDataProvider;
  public
    constructor Create(TheOwner: TComponent; AEntity : TEntity; AObject :
      TNode); reintroduce;
    function GetObject : TNode;
  end;

var
  NodeWindow: TNodeWindow;

implementation

uses
  configuration;

{$R *.lfm}

procedure TNodeWindow.NameEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := NameEditor.Text <> '';
end;

procedure TNodeWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMeasureDataProvider);
end;

procedure TNodeWindow.DeleteButtonClick(Sender: TObject);
begin
  if FEntity.ID <> -1 then
    FEntity.NodeBag.Remove(FObject);

  if FObject.ID <> -1 then
    FObject.Delete;
end;

constructor TNodeWindow.Create (TheOwner : TComponent; AEntity : TEntity;
  AObject : TNode);
var
  Measure : TCommonObject;
  MeasureIndex : Integer;
  Index : Integer;
begin
  inherited Create(TheOwner);
  FEntity := AEntity;
  FObject := AObject;

  ObjectGroup.Caption := Config.GetValue('Object', 'Object');
  NameLabel.Caption := Config.GetValue('Name', 'Name');
  PeriodLabel.Caption := Config.GetValue('Period', 'Period');

  SheduleGroup.Caption := Config.GetValue('Shedule', 'Shedule');
  ShedulePrevNameLabel.Caption := Config.GetValue('Previous date',
    'Previous date');
  SheduleNextNameLabel.Caption := Config.GetValue('Next date',
    'Next date');

  SaveButton.Caption := Config.GetValue('Save', 'Save');
  CancelButton.Caption := Config.GetValue('Cancel', 'Cancel');
  DeleteButton.Caption := Config.GetValue('Delete', 'Delete');

  NameEditor.Text := FObject.Name;
  PeriodCountEditor.Value := FObject.Period.Quantity.Count;

  FMeasureDataProvider := TMeasureDataProvider.Create;
  if FMeasureDataProvider.Load then
  begin
    PeriodMeasureEditor.Items.Clear;

    Index := 0;
    MeasureIndex := -1;
    for Measure in FMeasureDataProvider do
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

  SaveButton.Enabled := NameEditor.Text <> '';
  DeleteButton.Visible := (FObject <> nil) and (FObject.ID <> -1);
end;

function TNodeWindow.GetObject : TNode;
begin
  Result := FObject;

  Result.Name := NameEditor.Text;
  Result.Period.Quantity.Count := PeriodCountEditor.Value;

  if PeriodMeasureEditor.ItemIndex <> -1 then
  begin
    Result.Period.Quantity.Measure := TMeasure(FMeasureDataProvider.GetObject(
      PeriodMeasureEditor.ItemIndex));
  end else
  begin
    Result.Period.Quantity.Measure.Name := PeriodMeasureEditor.Text;
  end;

  Result.Shedule.PrevDate := ShedulePrevEditor.Date;
  Result.Shedule.NextDate := SheduleNextEditor.Date;
end;

end.

