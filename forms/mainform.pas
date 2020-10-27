unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, VirtualTrees,
  renderers.virtualtreeview, renderers.profile.inspector;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MainMenu: TVirtualDrawTree;
    BottomPanel: TPanel;
    TopPanel: TPanel;
    Settings: TVirtualDrawTree;
    MainMenuSplitter: TSplitter;
    SettingsSplitter: TSplitter;
    Content: TVirtualDrawTree;
    procedure FormCreate(Sender: TObject);

  public
    FRenderer : TProfileInspectorRenderer;
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  FRenderer := TProfileInspectorRenderer.Create(MainMenu);
end;

end.

