unit entityform_delphi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls, Vcl.Buttons, Vcl.ComCtrls,
  objects.entity, objects.quantity, objects.period, objects.measure,
  objects.equipment, dataproviders.measure, objects.common, math;

type
  TEntityWindow = class(TForm)
    ObjectGroup: TGroupBox;
    NameGroup: TPanel;
    NameLabelGroup: TPanel;
    NameLabel: TLabel;
    NameEditorGroup: TPanel;
    NameEditor: TEdit;
    QuantityGroup: TPanel;
    QuantityLabelGroup: TPanel;
    QuantityLabel: TLabel;
    QuantityEditorGroup: TPanel;
    QuantityCountEditor: TSpinEdit;
    QuantityMeasureEditor: TComboBox;
    PeriodGroup: TPanel;
    PeriodLabelGroup: TPanel;
    PeriodLabel: TLabel;
    PeriodEditorGroup: TPanel;
    PeriodCountEditor: TSpinEdit;
    PeriodMeasureEditor: TComboBox;
    SheduleGroup: TGroupBox;
    ShedulePrevGroup: TPanel;
    ShedulePrevNameLabelGroup: TPanel;
    ShedulePrevLabel: TLabel;
    ShedulePrevEditorGroup: TPanel;
    ShedulePrevEditor: TDateTimePicker;
    SheduleNextGroup: TPanel;
    SheduleNextNameLabelGroup: TPanel;
    SheduleNextLabel: TLabel;
    SheduleNextEditorGroup: TPanel;
    SheduleNextEditor: TDateTimePicker;
    DeleteButton: TBitBtn;
    SaveButton: TBitBtn;
    CancelButton: TBitBtn;
    procedure NameEditorChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
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

{$R *.dfm}

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

  MeasureDataProvider := TMeasureDataProvider.Create;
  MeasureLoad := MeasureDataProvider.Load;

  NameEditor.Text := FObject.Name;
  QuantityCountEditor.Value := Ceil(FObject.Quantity.Count);

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

  PeriodCountEditor.Value := Ceil(FObject.Period.Quantity.Count);

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

procedure TEntityWindow.NameEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := NameEditor.Text <> '';
end;

end.
