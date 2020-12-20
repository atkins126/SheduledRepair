unit mainform_delphi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ExtCtrls, dataprovider,
  mainmenuprovider, objects.equipment, objects.job, Vcl.ComCtrls;

type
  TMainWindow = class(TForm)
    MainMenuView: TVirtualDrawTree;
    MainMenuSplitter: TSplitter;
    SettingsView: TVirtualDrawTree;
    SettingsSplitter: TSplitter;
    ContentView: TVirtualDrawTree;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.dfm}

procedure TMainWindow.FormCreate(Sender: TObject);
var
  Equipment : TEquipment;
  Job : TJob;
begin
  Equipment := TEquipment.Create(-1);
  Equipment.CheckSchema;
  FreeAndNil(Equipment);
  Job := TJob.Create(-1);
  Job.CheckSchema;
  FreeAndNil(Job);

  MainMenu.View := MainMenuView;

  Provider.Parent := Self;
  Provider.DataView := ContentView;
end;

end.
