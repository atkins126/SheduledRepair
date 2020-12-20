program SheduledRepair;

uses
  Vcl.Forms,
  mainform_delphi in '..\forms\mainform_delphi.pas' {MainWindow},
  jobform_delphi in '..\forms\editors\jobform_delphi.pas' {JobWindow};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TJobWindow, JobWindow);
  Application.Run;
end.
