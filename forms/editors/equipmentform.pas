unit equipmentform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, Buttons, objects.equipment;

type

  { TEquipmentWindow }

  TEquipmentWindow = class(TForm)
    CancelButton: TBitBtn;
    DeleteButton: TBitBtn;
    NameEditor: TEdit;
    NameEditorGroup: TPanel;
    NameGroup: TPanel;
    NameLabel: TLabel;
    NameLabelGroup: TPanel;
    ObjectGroup: TGroupBox;
    SaveButton: TBitBtn;
    procedure DeleteButtonClick(Sender: TObject);
    procedure NameEditorChange(Sender: TObject);
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

{$R *.lfm}

{ TEquipmentWindow }

procedure TEquipmentWindow.DeleteButtonClick(Sender: TObject);
begin
  if FObject.ID <> -1 then
    FObject.Delete;
end;

procedure TEquipmentWindow.NameEditorChange(Sender: TObject);
begin
  SaveButton.Enabled := NameEditor.Text <> '';
end;

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

function TEquipmentWindow.GetObject : TEquipment;
begin
  if FObject.ID <> -1 then
    Result := FObject
  else
    Result := TEquipment.Create(-1);

  Result.Name := NameEditor.Text;
end;

end.

