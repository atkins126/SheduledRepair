program SheduledRepair;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, renderers.datarenderer, profilesprovider.mainmenu,
  dataproviders.mainmenu, dataproviders.equipment, dataproviders.measure,
  datahandlers, objects.common, objects.entitybag, objects.greasebag,
  objects.mainmenu.item, objects.nodebag, equipmentform, profileform, jobform
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TJobWindow, JobWindow);
  Application.Run;
end.

