unit profileform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, VirtualTrees, renderer.profile.profile,
  renderer.profile.profileitem, renderers.profile.inspector;

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
var
  Profile : TRendererProfile;
begin
  FDefaultRenderer := TProfileInspectorRenderer.Create(False,
    DefaultProfileTree);
  FHoverRenderer := TProfileInspectorRenderer.Create(True, HoverProfileTree);
  FSelectedRenderer := TProfileInspectorRenderer.Create(True,
    SelectedProfileTree);

  Profile := TRendererProfile.Create(-1);
  Profile.Items['Name'] := TRendererProfileItem.Create(-1);
  Profile.Items['Measure'] := TRendererProfileItem.Create(-1);
  FDefaultRenderer.UpdateProfile(Profile);
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

