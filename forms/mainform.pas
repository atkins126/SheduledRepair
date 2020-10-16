unit MainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType, ExtCtrls, Graphics,
  Grids, Menus, dataprovider, renderer.profile.objectprofile,
  renderer.objects.measure, BGRABitmap, BGRABitmapTypes, Types, objects.measure;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MenuGrid: TStringGrid;
    ContentGrid: TStringGrid;
    procedure ContentGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
  public

  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);

begin
  //load := Provider.Measure.EditObject(2);
  //profile := Provider.Measure.GetObjectProfile(2);
  ContentGrid.Rows[0].Add('test');
  ContentGrid.RowHeights[0] := 50;
  ContentGrid.ColWidths[0] := ContentGrid.Width;
end;

procedure TMainWindow.ContentGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  bitmap : TBGRABitmap;
  measure : TMeasure;
  profile : TRendererObjectProfile;
  renderer : TMeasureRenderer;
begin
  bitmap := TBGRABitmap.Create(ContentGrid.Width, 50);

  measure := Provider.Measure.GetObject(1);
  profile := Provider.Measure.GetObjectProfile(1);
  renderer := TMeasureRenderer.Create(measure, profile);

  renderer.Draw(@bitmap);
  bitmap.Draw((Sender as TCustomStringGrid).Canvas, aRect);
end;

end.

