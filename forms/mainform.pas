unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, VirtualTrees,
  profileform, dataprovider, mainmenuprovider, objects.equipment, objects.job;

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
var
  Equipment : TEquipment;
  Job : TJob;
begin
  {
  FProfileEditor := TProfileWindow.Create(Self);
  FProfileEditor.Left := Self.Left - FProfileEditor.Width - 1;
  FProfileEditor.Top := Self.Top;
  FProfileEditor.Height := Self.Height;
  FProfileEditor.Show;
  }
  Equipment := TEquipment.Create(-1);
  Equipment.CheckSchema;
  Job := TJob.Create(-1);
  Job.CheckSchema;

  MainMenu.View := MainMenuView;

  Provider.Parent := Self;
  Provider.DataView := ContentView;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProfileEditor);
end;

end.

