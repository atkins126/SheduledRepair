program SheduledRepair;

uses
  Vcl.Forms,
  mainform_delphi in '..\forms\mainform_delphi.pas' {MainWindow},
  jobform_delphi in '..\forms\editors\jobform_delphi.pas' {JobWindow},
  equipmentform_delphi in '..\forms\editors\equipmentform_delphi.pas' {EquipmentWindow},
  entityform_delphi in '..\forms\editors\entityform_delphi.pas' {EntityWindow},
  nodeform_delphi in '..\forms\editors\nodeform_delphi.pas' {NodeWindow},
  greasebundleform_delphi in '..\forms\editors\greasebundleform_delphi.pas' {GreaseBundleWindow};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TJobWindow, JobWindow);
  Application.CreateForm(TEquipmentWindow, EquipmentWindow);
  Application.CreateForm(TEntityWindow, EntityWindow);
  Application.CreateForm(TNodeWindow, NodeWindow);
  Application.CreateForm(TGreaseBundleWindow, GreaseBundleWindow);
  Application.Run;
end.
