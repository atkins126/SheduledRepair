unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType, Grids, ExtCtrls,
  renderer.menu.listbox, Types, Graphics, renderer.toolbar.stringgrid;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MenuList: TListBox;
    MenuIcons: TImageList;
    ToolBarGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private
    FMenuListBoxRenderer : TMenuListBoxRenderer;
    FToolbarRenderer : TToolbarStringGridRenderer;
  public

  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  FMenuListBoxRenderer := TMenuListBoxRenderer.Create(@MenuList,
    @MenuIcons);
  FToolbarRenderer := TToolbarStringGridRenderer.Create(@ToolBarGrid);
  FToolbarRenderer.Append(TMenuHeaderItem.Create);
end;

end.

