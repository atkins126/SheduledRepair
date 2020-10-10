unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Types, LCLType,
  renderer.menu.listbox;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    ActiveMenuIcons: TImageList;
    GreyMenuIcons: TImageList;
    ListBox1: TListBox;
    MenuList: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    FMenuListBoxRenderer : TMenuListBoxRenderer;

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
    @ActiveMenuIcons, @GreyMenuIcons);
end;


end.

