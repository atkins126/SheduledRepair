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
unit greasebundleform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, Buttons, objects.greasableobject, objects.greasebundle, objects.grease,
  objects.supplier, objects.grade, objects.quantity, objects.measure,
  dataproviders.supplier, objects.common, dataproviders.grade,
  dataproviders.measure;

type

  { TGreaseBundleWindow }

  TGreaseBundleWindow = class(TForm)
    CancelButton: TBitBtn;
    DeleteButton: TBitBtn;
    QuantityCountEditor: TFloatSpinEdit;
    QuantityEditorGroup: TPanel;
    QuantityGroup: TPanel;
    QuantityLabel: TLabel;
    QuantityLabelGroup: TPanel;
    QuantityMeasureEditor: TComboBox;
    SaveButton: TBitBtn;
    SupplierEditor: TComboBox;
    GradeEditor: TComboBox;
    SupplierEditorGroup: TPanel;
    GradeEditorGroup: TPanel;
    SupplierGroup: TPanel;
    GradeGroup: TPanel;
    SupplierLabel: TLabel;
    GradeLabel: TLabel;
    SupplierLabelGroup: TPanel;
    ObjectGroup: TGroupBox;
    GradeLabelGroup: TPanel;
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GradeEditorChange(Sender: TObject);
    procedure QuantityMeasureEditorChange(Sender: TObject);
    procedure SupplierEditorChange(Sender: TObject);
  private
    FObject : TGreaseBundle;
    FGreasableObject : TGreasableObject;
    FSupplierDataProvider : TSupplierDataProvider;
    FGradeDataProvider : TGradeDataProvider;
    FMeasureDataProvider : TMeasureDataProvider;
  public
    constructor Create(TheOwner : TComponent; AGreasableObject :
      TGreasableObject; AObject : TGreaseBundle); reintroduce;
    function GetObject : TGreaseBundle;
  end;

var
  GreaseBundleWindow: TGreaseBundleWindow;

implementation

uses
  configuration;

{$R *.lfm}

procedure TGreaseBundleWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSupplierDataProvider);
  FreeAndNil(FGradeDataProvider);
  FreeAndNil(FMeasureDataProvider);
end;

procedure TGreaseBundleWindow.DeleteButtonClick(Sender: TObject);
begin
  if FGreasableObject.ID <> -1 then
    FGreasableObject.GreaseBag.Remove(FObject);

  if FObject.ID <> -1 then
    FObject.Delete;
end;

procedure TGreaseBundleWindow.GradeEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := ((SupplierEditor.ItemIndex <> -1) or
    (SupplierEditor.Text <> '')) and ((GradeEditor.ItemIndex <> -1) or
    (GradeEditor.Text <> '')) and ((QuantityMeasureEditor.ItemIndex <> -1) or
    (QuantityCountEditor.Text <> ''));
end;

procedure TGreaseBundleWindow.QuantityMeasureEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := ((SupplierEditor.ItemIndex <> -1) or
    (SupplierEditor.Text <> '')) and ((GradeEditor.ItemIndex <> -1) or
    (GradeEditor.Text <> '')) and ((QuantityMeasureEditor.ItemIndex <> -1) or
    (QuantityCountEditor.Text <> ''));
end;

procedure TGreaseBundleWindow.SupplierEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := ((SupplierEditor.ItemIndex <> -1) or
    (SupplierEditor.Text <> '')) and ((GradeEditor.ItemIndex <> -1) or
    (GradeEditor.Text <> '')) and ((QuantityMeasureEditor.ItemIndex <> -1) or
    (QuantityCountEditor.Text <> ''));
end;

constructor TGreaseBundleWindow.Create(TheOwner : TComponent; AGreasableObject :
  TGreasableObject; AObject : TGreaseBundle);
var
  Supplier, Grade, Measure : TCommonObject;
  Index, SupplierIndex, GradeIndex, MeasureIndex : Integer;
begin
  inherited Create(TheOwner);
  FGreasableObject := AGreasableObject;
  FObject := AObject;

  ObjectGroup.Caption := Config.GetValue('Object', 'Object');
  SupplierLabel.Caption := Config.GetValue('Supplier', 'Supplier');
  GradeLabel.Caption := Config.GetValue('Grade', 'Grade');
  QuantityLabel.Caption := Config.GetValue('Quantity', 'Quantity');

  SaveButton.Caption := Config.GetValue('Save', 'Save');
  CancelButton.Caption := Config.GetValue('Cancel', 'Cancel');
  DeleteButton.Caption := Config.GetValue('Delete', 'Delete');

  FSupplierDataProvider := TSupplierDataProvider.Create;
  if FSupplierDataProvider.Load then
  begin
    SupplierEditor.Items.Clear;

    Index := 0;
    SupplierIndex := -1;
    for Supplier in FSupplierDataProvider do
    begin
      SupplierEditor.Items.Add(TSupplier(Supplier).Name);

      if (AObject <> nil) and (TSupplier(Supplier).ID =
        AObject.Grease.Supplier.ID) then
        SupplierIndex := Index;
      
      Inc(Index);
    end;

    SupplierEditor.ItemIndex := SupplierIndex;
  end;

  FGradeDataProvider := TGradeDataProvider.Create;
  if FGradeDataProvider.Load then
  begin
    GradeEditor.Items.Clear;

    Index := 0;
    GradeIndex := -1;
    for Grade in FGradeDataProvider do
    begin
      GradeEditor.Items.Add(TGrade(Grade).Name);

      if (AObject <> nil) and (TGrade(Grade).ID =
        AObject.Grease.Grade.ID) then
        GradeIndex := Index;
      
      Inc(Index);
    end;

    GradeEditor.ItemIndex := GradeIndex;
  end;

  QuantityCountEditor.Value := FObject.Quantity.Count;

  FMeasureDataProvider := TMeasureDataProvider.Create;
  if FMeasureDataProvider.Load then
  begin
    QuantityMeasureEditor.Items.Clear;

    Index := 0;
    MeasureIndex := -1;
    for Measure in FMeasureDataProvider do
    begin
      QuantityMeasureEditor.Items.Add(TMeasure(Measure).Name);

      if (AObject <> nil) and (TMeasure(Measure).ID =
        AObject.Quantity.Measure.ID) then
        MeasureIndex := Index;
      
      Inc(Index);
    end;

    QuantityMeasureEditor.ItemIndex := MeasureIndex;
  end;

  SaveButton.Enabled := ((SupplierEditor.ItemIndex <> -1) or
    (SupplierEditor.Text <> '')) and ((GradeEditor.ItemIndex <> -1) or
    (GradeEditor.Text <> '')) and ((QuantityMeasureEditor.ItemIndex <> -1) or
    (QuantityCountEditor.Text <> ''));
  DeleteButton.Visible := (FObject <> nil) and (FObject.ID <> -1);
end;

function TGreaseBundleWindow.GetObject : TGreaseBundle;
begin
  Result := FObject;

  if SupplierEditor.ItemIndex <> -1 then
  begin
    Result.Grease.Supplier := TSupplier(FSupplierDataProvider.GetObject(
      SupplierEditor.ItemIndex));
  end else
  begin
    Result.Grease.Supplier.Name := SupplierEditor.Text;
  end;

  if GradeEditor.ItemIndex <> -1 then
  begin
    Result.Grease.Grade := TGrade(FGradeDataProvider.GetObject(
      GradeEditor.ItemIndex));
  end else
  begin
    Result.Grease.Grade.Name := GradeEditor.Text;
  end;

  Result.Quantity.Count := QuantityCountEditor.Value;

  if QuantityMeasureEditor.ItemIndex <> -1 then
  begin
  Result.Quantity.Measure := TMeasure(FMeasureDataProvider.GetObject(
      QuantityMeasureEditor.ItemIndex));
  end else
  begin
    Result.Quantity.Measure.Name := QuantityMeasureEditor.Text;
  end;
end;

end.

