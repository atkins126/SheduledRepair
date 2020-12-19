program SheduledRepair;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, renderers.datarenderer, profilesprovider.mainmenu,
  dataproviders.mainmenu, dataproviders.equipment, dataproviders.measure,
  dataproviders.node, datahandlers, eventproviders.mainmenu.item.entity,
  objects.common, objects.entitybag, objects.greasebag, objects.mainmenu.item,
  objects.nodebag, equipmentform, profileform, jobform, entityform, nodeform,
  greasebundleform
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TEntityWindow, EntityWindow);
  Application.CreateForm(TNodeWindow, NodeWindow);
  Application.CreateForm(TGreaseBundleWindow, GreaseBundleWindow);
  Application.Run;
end.

