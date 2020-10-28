unit profileform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, VirtualTrees, renderer.profile.profile,
  renderers.profile.inspector;

type

  { TProfileWindow }

  TProfileWindow = class(TForm)
    PageControl: TPageControl;
    DefaultProfileTabSheet: TTabSheet;
    HoverProfileTabSheet: TTabSheet;
    SelectedProfileTabSheet: TTabSheet;
    DefaultProfileTree: TVirtualDrawTree;
    HoverProfileTree: TVirtualDrawTree;
    SelectedProfileTree: TVirtualDrawTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDefaultRenderer : TProfileInspectorRenderer;
    FHoverRenderer : TProfileInspectorRenderer;
    FSelectedRenderer : TProfileInspectorRenderer;
  public

  end;

var
  ProfileWindow: TProfileWindow;

implementation

{$R *.lfm}

{ TProfileWindow }

procedure TProfileWindow.FormCreate(Sender: TObject);
begin
  FDefaultRenderer := TProfileInspectorRenderer.Create(DefaultProfileTree);
  FHoverRenderer := TProfileInspectorRenderer.Create(HoverProfileTree);
  FSelectedRenderer := TProfileInspectorRenderer.Create(SelectedProfileTree);

  FDefaultRenderer.UpdateProfile(TRendererProfile.Create(-1));
  FHoverRenderer.UpdateProfile(TRendererProfile.Create(-1));
  FSelectedRenderer.UpdateProfile(TRendererProfile.Create(-1));
end;

procedure TProfileWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDefaultRenderer);
  FreeAndNil(FHoverRenderer);
  FreeAndNil(FSelectedRenderer);
end;

end.

