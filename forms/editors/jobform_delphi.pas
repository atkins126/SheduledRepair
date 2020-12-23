unit jobform_delphi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Samples.Spin, Vcl.ComCtrls, Vcl.Buttons,
  objects.job, objects.entity, dataproviders.entity, objects.common,
  objects.measure, dataproviders.measure, math;

type
  TJobWindow = class(TForm)
    ObjectGroup: TGroupBox;
    NameGroup: TPanel;
    NameLabelGroup: TPanel;
    NameLabel: TLabel;
    NameEditorGroup: TPanel;
    NameEditor: TEdit;
    PeriodGroup: TPanel;
    PeriodLabelGroup: TPanel;
    PeriodLabel: TLabel;
    PeriodEditorGroup: TPanel;
    PeriodCountEditor: TSpinEdit;
    PeriodMeasureEditor: TComboBox;
    EntityGroup: TGroupBox;
    EntityNameGroup: TPanel;
    EntityNameLabelGroup: TPanel;
    EntityNameLabel: TLabel;
    EntityNameEditorGroup: TPanel;
    EntityNameSelector: TComboBox;
    EntityCountGroup: TPanel;
    EntityCountLabelGroup: TPanel;
    EntityCountLabel: TLabel;
    EntityCountEditorGroup: TPanel;
    EntityCountEditor: TEdit;
    EntityPeriodGroup: TPanel;
    EntityPeriodLabelGroup: TPanel;
    EntityPeriodLabel: TLabel;
    EntityPeriodEditorGroup: TPanel;
    EntityPeriodEditor: TEdit;
    SheduleGroup: TGroupBox;
    ShedulePrevGroup: TPanel;
    ShedulePrevNameLabelGroup: TPanel;
    ShedulePrevNameLabel: TLabel;
    ShedulePrevEditorGroup: TPanel;
    ShedulePrevEditor: TDateTimePicker;
    SheduleNextGroup: TPanel;
    SheduleNextNameLabelGroup: TPanel;
    SheduleNextNameLabel: TLabel;
    SheduleNextEditorGroup: TPanel;
    SheduleNextEditor: TDateTimePicker;
    DeleteButton: TBitBtn;
    CancelButton: TBitBtn;
    SaveButton: TBitBtn;
    procedure FormDestroy(Sender: TObject);
    procedure NameEditorChange(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure EntityNameSelectorChange(Sender: TObject);
  private
    FObject : TJob;
    FEntityDataProvider : TEntityDataProvider;
    FMeasureDataProvider : TMeasureDataProvider;
  public
    constructor Create (TheOwner : TComponent; AObject : TJob); reintroduce;
    function GetObject : TJob;
  end;

var
  JobWindow: TJobWindow;

implementation

uses
  configuration;

{$R *.dfm}

constructor TJobWindow.Create (TheOwner : TComponent; AObject : TJob);
var
  Entity, Measure : TCommonObject;
  Index, EntityIndex, MeasureIndex : Integer;
begin
  inherited Create(TheOwner);
  FObject := AObject;

  ObjectGroup.Caption := Config.GetValue('Object', 'Object');
  NameLabel.Caption := Config.GetValue('Name', 'Name');
  PeriodLabel.Caption := Config.GetValue('Period', 'Period');

  EntityGroup.Caption := Config.GetValue('Entity', 'Entity');
  EntityNameLabel.Caption := Config.GetValue('Name', 'Name');
  EntityCountLabel.Caption := Config.GetValue('Quantity', 'Quantity');
  EntityPeriodLabel.Caption := Config.GetValue('Period', 'Period');

  SheduleGroup.Caption := Config.GetValue('Shedule', 'Shedule');
  ShedulePrevNameLabel.Caption := Config.GetValue('Previous date',
    'Previous date');
  SheduleNextNameLabel.Caption := Config.GetValue('Next date',
    'Next date');

  SaveButton.Caption := Config.GetValue('Save', 'Save');
  CancelButton.Caption := Config.GetValue('Cancel', 'Cancel');
  DeleteButton.Caption := Config.GetValue('Delete', 'Delete');

  NameEditor.Text := FObject.Name;
  PeriodCountEditor.Value := Ceil(FObject.Period.Quantity.Count);

  FMeasureDataProvider := TMeasureDataProvider.Create;
  if FMeasureDataProvider.Load then
  begin
    PeriodMeasureEditor.Items.Clear;

    Index := 0;
    MeasureIndex := -1;
    for Measure in FMeasureDataProvider do
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

  FEntityDataProvider := TEntityDataProvider.Create(nil);
  if FEntityDataProvider.Load then
  begin
    EntityNameSelector.Items.Clear;

    Index := 0;
    EntityIndex := -1;
    for Entity in FEntityDataProvider do
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

procedure TJobWindow.DeleteButtonClick(Sender: TObject);
begin
  if FObject.ID <> -1 then
    FObject.Delete;
end;

procedure TJobWindow.EntityNameSelectorChange(Sender: TObject);
begin
  if EntityNameSelector.ItemIndex = -1 then
    Exit;

  EntityCountEditor.Text := FormatFloat('0.00',
    TEntity(FEntityDataProvider.GetObject(EntityNameSelector.ItemIndex)).Quantity
    .Count) + ' ' + TEntity(FEntityDataProvider.GetObject(
    EntityNameSelector.ItemIndex)).Quantity.Measure.Name;

  EntityPeriodEditor.Text := FormatFloat('0.00',
    TEntity(FEntityDataProvider.GetObject(EntityNameSelector.ItemIndex)).Period
    .Quantity.Count) + ' ' + TEntity(FEntityDataProvider.GetObject(
    EntityNameSelector.ItemIndex)).Period.Quantity.Measure.Name;

  SaveButton.Enabled := (NameEditor.Text <> '') and
    (EntityNameSelector.ItemIndex <> -1);
end;

procedure TJobWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEntityDataProvider);
  FreeAndNil(FMeasureDataProvider);
end;

function TJobWindow.GetObject : TJob;
begin
  if FObject.ID <> -1 then
    Result := FObject
  else
    Result := TJob.Create(-1);

  Result.Name := NameEditor.Text;
  Result.Entity := TEntity(FEntityDataProvider.GetObject(
    EntityNameSelector.ItemIndex));

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

procedure TJobWindow.NameEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := (NameEditor.Text <> '') and
    (EntityNameSelector.ItemIndex <> -1);
end;

end.
