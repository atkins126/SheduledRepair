unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  VirtualTrees, profileform, renderers.mainmenu, dataproviders.mainmenu,
  profilesprovider.mainmenu, renderers.renderer;

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
    procedure FormDestroy(Sender: TObject);
  private
    FProfileEditor : TProfileWindow;

    MenuDataProvider : TMainMenuDataProvider;
    MenuProfileProvider : TMainMenuProfilesProvider;
    MenuRenderer : TMainMenuRenderer;
    Renderer : TRenderer;
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  FProfileEditor := TProfileWindow.Create(Self);
  FProfileEditor.Left := Self.Left - FProfileEditor.Width - 1;
  FProfileEditor.Top := Self.Top;
  FProfileEditor.Height := Self.Height;
  FProfileEditor.Show;

  MenuDataProvider := TMainMenuDataProvider.Create;
  MenuProfileProvider := TMainMenuProfilesProvider.Create;
  MenuRenderer := TMainMenuRenderer.Create;

  MenuDataProvider.Load;
  MenuProfileProvider.Load;

  Renderer := TRenderer.Create(MainMenu);
  Renderer.Update(MenuDataProvider, MenuProfileProvider, MenuRenderer);
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Renderer);
  FreeAndNil(MenuDataProvider);
  FreeAndNil(MenuProfileProvider);
  FreeAndNil(MenuRenderer);

  FreeAndNil(FProfileEditor);
end;

end.

