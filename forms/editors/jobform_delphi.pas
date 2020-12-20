unit jobform_delphi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Spin;

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
    PeriodMeasureEditor: TComboBox;
    PeriodCountEditor: TSpinEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  JobWindow: TJobWindow;

implementation

{$R *.dfm}

end.
