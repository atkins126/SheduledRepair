unit nodeform_delphi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls,
  objects.entity, objects.node, objects.common, objects.measure,
  dataproviders.measure, math;

type
  TNodeWindow = class(TForm)
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

{$R *.dfm}

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

procedure TNodeWindow.DeleteButtonClick(Sender: TObject);
begin
  if FEntity.ID <> -1 then
    FEntity.NodeBag.Remove(FObject);

  if FObject.ID <> -1 then
    FObject.Delete;
end;

procedure TNodeWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMeasureDataProvider);
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

procedure TNodeWindow.NameEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := NameEditor.Text <> '';
end;

end.
