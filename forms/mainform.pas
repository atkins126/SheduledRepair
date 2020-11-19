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

    MainMenuDataProvider : TMainMenuDataProvider;
    MainMenuProfileProvider : TMainMenuProfilesProvider;
    MainMenuItemRenderer : TMainMenuRenderer;
    MainMenuRenderer : TDataRenderer;

    EquipmentDataProvider : TEquipmentDataProvider;
    EquipmentProfileProvider : TEquipmentProfilesProvider;
    EquipmentRenderer : TEquipmentRenderer;
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
  MainMenuDataProvider := TMainMenuDataProvider.Create;
  MainMenuProfileProvider := TMainMenuProfilesProvider.Create;
  MainMenuItemRenderer := TMainMenuRenderer.Create;

  MainMenuDataProvider.Load;
  MainMenuProfileProvider.Load;

  MainMenuRenderer := TDataRenderer.Create(MainMenu, MainMenuDataProvider,
    MainMenuProfileProvider, MainMenuItemRenderer);
  MainMenuRenderer.UpdateData;

  { Render data. }
  EquipmentDataProvider := TEquipmentDataProvider.Create;
  EquipmentProfileProvider := TEquipmentProfilesProvider.Create;
  EquipmentRenderer := TEquipmentRenderer.Create;

  EquipmentDataProvider.Load;
  EquipmentProfileProvider.Load;

  DataRenderer := TDataRenderer.Create(Content, EquipmentDataProvider,
    EquipmentProfileProvider, EquipmentRenderer);
  DataRenderer.UpdateData;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MainMenuRenderer);
  FreeAndNil(MainMenuDataProvider);
  FreeAndNil(MainMenuProfileProvider);
  FreeAndNil(MainMenuItemRenderer);

  FreeAndNil(DataRenderer);
  FreeAndNil(EquipmentDataProvider);
  FreeAndNil(EquipmentProfileProvider);
  FreeAndNil(EquipmentRenderer);

  FreeAndNil(FProfileEditor);
end;

end.

