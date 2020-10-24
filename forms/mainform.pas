unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, VirtualTrees,
  renderers.virtualtreeview.mainmenu, Graphics;

type
  TRenderer = class(specialize TVirtualTreeViewRenderer<String>)
  public
    procedure Draw (ACanvas : TCanvas; ARect : TRect; AData : String);
      override;
  end;

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
    FRenderer : TRenderer;
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

procedure TRenderer.Draw (ACanvas : TCanvas; ARect : TRect; AData : String);
begin
  ACanvas.TextOut(5, 5, AData);
end;

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  FRenderer := TRenderer.Create(MainMenu);
  FRenderer.Append('test string');
  FRenderer.Append('some another string');
end;

end.

