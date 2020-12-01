unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  VirtualTrees, profileform, renderers.mainmenu, dataproviders.mainmenu,
  profilesprovider.mainmenu, renderers.datarenderer, renderers.equipment,
  dataproviders.equipment, profilesprovider.equipment;

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

    MainMenuRenderer : TDataRenderer;
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

  { Render main menu. }
  MainMenuRenderer := TDataRenderer.Create(MainMenu,
    TMainMenuDataProvider.Create, TMainMenuProfilesProvider.Create,
    TMainMenuRenderer.Create);
  MainMenuRenderer.UpdateData;

  { Render data. }
  DataRenderer := TDataRenderer.Create(Content,
    TEquipmentDataProvider.Create, TEquipmentProfilesProvider.Create,
    TEquipmentRenderer.Create);
  DataRenderer.UpdateData;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MainMenuRenderer);
  FreeAndNil(DataRenderer);

  FreeAndNil(FProfileEditor);
end;

end.

