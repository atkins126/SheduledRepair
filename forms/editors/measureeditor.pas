unit MeasureEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  objects.measure;

type

  { TMeasureEditorForm }

  TMeasureEditorForm = class(TForm)
    CancelButton: TButton;
    OkButton: TButton;
    NameEdit: TEdit;
    NameGroupBox: TGroupBox;
    procedure OkButtonClick(Sender: TObject);
  private
    FMeasure : TMeasure;

    procedure SetMeasure (AMeasure : TMeasure);
  public
    property Measure : TMeasure read FMeasure write SetMeasure;
  end;

var
  MeasureEditorForm: TMeasureEditorForm;

implementation

{$R *.lfm}

{ TMeasureEditorForm }

procedure TMeasureEditorForm.SetMeasure (AMeasure : TMeasure);
begin
  FMeasure := AMeasure;
  NameEdit.Text := FMeasure.Name;
end;

procedure TMeasureEditorForm.OkButtonClick(Sender: TObject);
begin
  FMeasure.Name := NameEdit.Text;
end;

end.

