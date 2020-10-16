unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType, ExtCtrls, Graphics,
  Grids, Menus, dataproviders.measure;

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
  provider : TMeasureDataProvider;
begin
  provider := TMeasureDataProvider.Create(Self);
  provider.Load;
  provider.CreateObject;
end;

end.

