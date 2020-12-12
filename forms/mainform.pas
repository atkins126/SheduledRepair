unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, VirtualTrees,
  profileform, dataprovider, mainmenuprovider;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MainMenuView: TVirtualDrawTree;
    BottomPanel: TPanel;
    TopPanel: TPanel;
    SettingsView: TVirtualDrawTree;
    MainMenuSplitter: TSplitter;
    SettingsSplitter: TSplitter;
    ContentView: TVirtualDrawTree;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FProfileEditor : TProfileWindow;
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  {
  FProfileEditor := TProfileWindow.Create(Self);
  FProfileEditor.Left := Self.Left - FProfileEditor.Width - 1;
  FProfileEditor.Top := Self.Top;
  FProfileEditor.Height := Self.Height;
  FProfileEditor.Show;
  }
  MainMenu.View := MainMenuView;

  Provider.Parent := Self;
  Provider.DataView := ContentView;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProfileEditor);
end;

end.

