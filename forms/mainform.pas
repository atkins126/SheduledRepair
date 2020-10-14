unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType, ExtCtrls, Graphics,
  Grids, Menus, renderer.objects.measure, configuration;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MenuGrid: TStringGrid;
    ContentGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
  public

  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
var
  r : TMeasureRenderer;
begin
  Config.CheckSchema;
  r := TMeasureRenderer.Create;
  r.LoadSettings;
  r.SaveSettings;
end;

end.

