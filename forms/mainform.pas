unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  VirtualTrees, profileform, renderers.mainmenu, dataproviders.mainmenu,
  profilesprovider.mainmenu, renderers.datarenderer;

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
    DataRenderer : TDataRenderer;
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

  DataRenderer := TDataRenderer.Create(MainMenu, MenuDataProvider,
    MenuProfileProvider, MenuRenderer);
  DataRenderer.UpdateData;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  {
  FreeAndNil(DataRenderer);
  FreeAndNil(MenuDataProvider);
  FreeAndNil(MenuProfileProvider);
  FreeAndNil(MenuRenderer);
  }
  FreeAndNil(FProfileEditor);
end;

end.

