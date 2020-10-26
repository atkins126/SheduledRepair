unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, VirtualTrees,
  renderers.virtualtreeview, Graphics, Controls;

type
  TRenderer = class(specialize TVirtualTreeViewRenderer<String>)
  public
    function ItemHeight (AIndex : Cardinal; AData : String) : Cardinal;
      override;
    procedure ItemDraw (AItemType : Integer; ACanvas : TCanvas; ACellRect :
      TRect; AContentRect : TRect; AState : TItemStates; AData : String);
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

function TRenderer.ItemHeight (AIndex : Cardinal; AData : String) : Cardinal;
begin
  Result := 25;
end;

procedure TRenderer.ItemDraw (AItemType : Integer; ACanvas : TCanvas; ACellRect:
  TRect; AContentRect : TRect; AState : TItemStates; AData : String);
begin
  if ITEM_SELECTED in AState then
    ACanvas.Brush.Color := clYellow;

  if ITEM_HOVER in AState then
    ACanvas.Brush.Color := clSilver;

  ACanvas.FillRect(AContentRect.Left, ACellRect.Top, ACellRect.Right,
    ACellRect.Bottom);
  ACanvas.Pen.Style := psDot;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Rectangle(AContentRect.Left, ACellRect.Top, ACellRect.Right,
    ACellRect.Bottom);
  ACanvas.TextOut(AContentRect.Left + 1, AContentRect.Top + 5, AData);
end;

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  FRenderer := TRenderer.Create(MainMenu);
  FRenderer.AppendColumn(200);
  FRenderer.AppendColumn(100);
  FRenderer.AppendData(1, 'test string');
  FRenderer.AppendData(2, 'some another string');
end;

end.

