unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType, ExtCtrls, Graphics,
  Grids, Menus, dataprovider, renderer.objectprofile;

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
  //profile : TRendererObjectProfile;
  //load : Boolean;
begin
  //load := Provider.Measure.EditObject(2);
  //profile := Provider.Measure.GetObjectProfile(2);
end;

end.

