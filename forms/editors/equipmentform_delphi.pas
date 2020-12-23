unit equipmentform_delphi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, objects.equipment;

type
  TEquipmentWindow = class(TForm)
    ObjectGroup: TGroupBox;
    NameGroup: TPanel;
    NameLabelGroup: TPanel;
    NameLabel: TLabel;
    NameEditorGroup: TPanel;
    NameEditor: TEdit;
    DeleteButton: TBitBtn;
    SaveButton: TBitBtn;
    CancelButton: TBitBtn;
    procedure NameEditorChange(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
  private
    FObject : TEquipment;
  public
    constructor Create(TheOwner: TComponent; AObject : TEquipment); reintroduce;
    function GetObject : TEquipment;
  end;

var
  EquipmentWindow: TEquipmentWindow;

implementation

uses
  configuration;

{$R *.dfm}

constructor TEquipmentWindow.Create(TheOwner: TComponent; AObject : TEquipment);
begin
  inherited Create(TheOwner);
  FObject := AObject;

  ObjectGroup.Caption := Config.GetValue('Object', 'Object');
  NameLabel.Caption := Config.GetValue('Name', 'Name');

  SaveButton.Caption := Config.GetValue('Save', 'Save');
  CancelButton.Caption := Config.GetValue('Cancel', 'Cancel');
  DeleteButton.Caption := Config.GetValue('Delete', 'Delete');

  NameEditor.Text := FObject.Name;

  SaveButton.Enabled := NameEditor.Text <> '';
  DeleteButton.Visible := (FObject <> nil) and (FObject.ID <> -1);
end;

procedure TEquipmentWindow.DeleteButtonClick(Sender: TObject);
begin
  if FObject.ID <> -1 then
    FObject.Delete;
end;

function TEquipmentWindow.GetObject : TEquipment;
begin
  if FObject.ID <> -1 then
    Result := FObject
  else
    Result := TEquipment.Create(-1);

  Result.Name := NameEditor.Text;
end;

procedure TEquipmentWindow.NameEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := NameEditor.Text <> '';
end;

end.
