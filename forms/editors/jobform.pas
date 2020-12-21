unit jobform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, Buttons, Spin, objects.job, objects.entity, dataproviders.entity,
  objects.common, objects.measure, dataproviders.measure;

type

  { TJobWindow }

  TJobWindow = class(TForm)
    PeriodMeasureEditor: TComboBox;
    PeriodCountEditor: TFloatSpinEdit;
    PeriodEditorGroup: TPanel;
    PeriodGroup: TPanel;
    PeriodLabel: TLabel;
    PeriodLabelGroup: TPanel;
    SaveButton: TBitBtn;
    CancelButton: TBitBtn;
    DeleteButton: TBitBtn;
    ShedulePrevEditor: TDateEdit;
    SheduleNextEditor: TDateEdit;
    ShedulePrevEditorGroup: TPanel;
    SheduleNextEditorGroup: TPanel;
    ShedulePrevGroup: TPanel;
    SheduleNextGroup: TPanel;
    ShedulePrevNameLabel: TLabel;
    SheduleNextNameLabel: TLabel;
    ShedulePrevNameLabelGroup: TPanel;
    EntityNameSelector: TComboBox;
    EntityCountEditor: TPanel;
    EntityPeriodEditor: TPanel;
    EntityCountEditorGroup: TPanel;
    EntityPeriodEditorGroup: TPanel;
    EntityCountGroup: TPanel;
    EntityPeriodGroup: TPanel;
    EntityCountLabel: TLabel;
    EntityPeriodLabel: TLabel;
    EntityCountLabelGroup: TPanel;
    EntityPeriodLabelGroup: TPanel;
    EntityNameLabelGroup: TPanel;
    EntityNameEditorGroup: TPanel;
    EntityNameGroup: TPanel;
    EntityNameLabel: TLabel;
    SheduleGroup: TGroupBox;
    ObjectGroup: TGroupBox;
    EntityGroup: TGroupBox;
    NameLabelGroup: TPanel;
    NameEditor: TEdit;
    NameEditorGroup: TPanel;
    NameGroup: TPanel;
    NameLabel: TLabel;
    SheduleNextNameLabelGroup: TPanel;
    procedure DeleteButtonClick(Sender: TObject);
    procedure EntityNameSelectorChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NameEditorChange(Sender: TObject);
  private
    FObject : TJob;
    EntityDataProvider : TEntityDataProvider;
    MeasureDataProvider : TMeasureDataProvider;
  public
    constructor Create(TheOwner: TComponent; AObject : TJob); reintroduce;
    function GetObject : TJob;
  end;

var
  JobWindow: TJobWindow;

implementation

{$R *.lfm}

procedure TJobWindow.EntityNameSelectorChange(Sender: TObject);
begin
  if EntityNameSelector.ItemIndex = -1 then
    Exit;

  EntityCountEditor.Caption := FormatFloat('0.00',
    TEntity(EntityDataProvider.GetObject(EntityNameSelector.ItemIndex)).Quantity
    .Count) + ' ' + TEntity(EntityDataProvider.GetObject(
    EntityNameSelector.ItemIndex)).Quantity.Measure.Name;

  EntityPeriodEditor.Caption := FormatFloat('0.00',
    TEntity(EntityDataProvider.GetObject(EntityNameSelector.ItemIndex)).Period
    .Quantity.Count) + ' ' + TEntity(EntityDataProvider.GetObject(
    EntityNameSelector.ItemIndex)).Period.Quantity.Measure.Name;

  SaveButton.Enabled := (NameEditor.Text <> '') and
    (EntityNameSelector.ItemIndex <> -1);
end;

procedure TJobWindow.DeleteButtonClick(Sender: TObject);
begin
  if FObject.ID <> -1 then
    FObject.Delete;
end;

procedure TJobWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(EntityDataProvider);
  FreeAndNil(MeasureDataProvider);
end;

procedure TJobWindow.NameEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := (NameEditor.Text <> '') and
    (EntityNameSelector.ItemIndex <> -1);
end;

constructor TJobWindow.Create (TheOwner : TComponent; AObject : TJob);
var
  Entity, Measure : TCommonObject;
  Index, EntityIndex, MeasureIndex : Integer;
begin
  inherited Create(TheOwner);
  FObject := AObject;

  NameEditor.Text := FObject.Name;
  PeriodCountEditor.Value := FObject.Period.Quantity.Count;

  MeasureDataProvider := TMeasureDataProvider.Create;
  if MeasureDataProvider.Load then
  begin
    PeriodMeasureEditor.Items.Clear;

    Index := 0;
    MeasureIndex := -1;
    for Measure in MeasureDataProvider do
    begin
      PeriodMeasureEditor.Items.Add(TMeasure(Measure).Name);

      if (AObject <> nil) and (TMeasure(Measure).ID =
        AObject.Period.Quantity.Measure.ID) then
        MeasureIndex := Index;
      Inc(Index);
    end;

    PeriodMeasureEditor.ItemIndex := MeasureIndex;
  end;

  ShedulePrevEditor.Date := FObject.Shedule.PrevDate;
  SheduleNextEditor.Date := FObject.Shedule.NextDate;

  EntityDataProvider := TEntityDataProvider.Create(nil);
  if EntityDataProvider.Load then
  begin
    EntityNameSelector.Items.Clear;

    Index := 0;
    EntityIndex := -1;
    for Entity in EntityDataProvider do
    begin
      EntityNameSelector.Items.Add(TEntity(Entity).Name);

      if (AObject <> nil) and (TEntity(Entity).ID = AObject.Entity.ID) then
        EntityIndex := Index;
      Inc(Index);
    end;

    EntityNameSelector.ItemIndex := EntityIndex;

    if EntityNameSelector.ItemIndex <> -1 then
      EntityNameSelector.OnChange(EntityNameSelector);
  end;

  SaveButton.Enabled := (NameEditor.Text <> '') and
    (EntityNameSelector.ItemIndex <> -1);
  DeleteButton.Visible := (FObject <> nil) and (FObject.ID <> -1);
end;

function TJobWindow.GetObject : TJob;
begin
  if FObject.ID <> -1 then
    Result := FObject
  else
    Result := TJob.Create(-1);

  Result.Name := NameEditor.Text;
  Result.Entity := TEntity(EntityDataProvider.GetObject(
    EntityNameSelector.ItemIndex));

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

