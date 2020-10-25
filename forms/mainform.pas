unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, VirtualTrees,
  renderers.virtualtreeview.mainmenu, Graphics, Controls;

type
  TRenderer = class(specialize TVirtualTreeViewRenderer<String>)
  public
    function ItemHeight (AIndex : Cardinal; AData : String) : Cardinal;
      override;
    procedure Draw (AItemType : Integer; ACanvas : TCanvas; ARect : TRect;
      AData : String); override;
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

function TRenderer.ItemHeight (AIndex : Cardinal; AData : String) : Cardinal;
begin
  case AIndex of
  0 : begin
    Result := 50;
  end;
  1 : begin
    Result := 20;
  end;
  end;
end;

procedure TRenderer.Draw (AItemType : Integer; ACanvas : TCanvas; ARect : TRect;
  AData : String);
begin
  case AItemType of
  1 : begin
    ACanvas.Brush.Color := clSilver;
  end;
  2 : begin
    //ACanvas.Brush.Color := clBtnFace;
  end;
  end;
  ACanvas.FillRect(ARect);
  ACanvas.TextOut(5, 5, AData);
end;

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  FRenderer := TRenderer.Create(MainMenu);
  FRenderer.Append(1, 'test string');
  FRenderer.Append(2, 'some another string');
end;

end.

