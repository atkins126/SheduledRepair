unit MainWindowUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  Vcl.StdCtrls, VirtualTrees, Vcl.ExtCtrls;

type
  TMainWindow = class(TForm)
    MainMenu: TMainMenu;
    Equipment: TMenuItem;
    EquipmentCreate: TMenuItem;
    Shedule: TMenuItem;
    EquipmentList: TMenuItem;
    SheduleList: TMenuItem;
    SheduleCreate: TMenuItem;
    Help: TMenuItem;
    About: TMenuItem;
    Delimiter1: TMenuItem;
    GreaseList: TMenuItem;
    ListBox: TListBox;
    Splitter1: TSplitter;
    ContentListBox: TVirtualStringTree;
    HelpPanel: TPanel;
    ShortcutPanel1: TPanel;
    ShortcutLabel1: TLabel;
    Label1: TLabel;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.dfm}

end.
