object EquipmentWindow: TEquipmentWindow
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Equipment editor'
  ClientHeight = 109
  ClientWidth = 658
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ObjectGroup: TGroupBox
    Left = 0
    Top = 0
    Width = 645
    Height = 50
    Align = alCustom
    Caption = 'Object'
    TabOrder = 0
    object NameGroup: TPanel
      Left = 10
      Top = 17
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 0
      object NameLabelGroup: TPanel
        Left = 0
        Top = 0
        Width = 305
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object NameLabel: TLabel
          Left = 0
          Top = 0
          Width = 305
          Height = 21
          Align = alClient
          Caption = 'Name'
          Layout = tlCenter
          ExplicitWidth = 27
          ExplicitHeight = 13
        end
      end
      object NameEditorGroup: TPanel
        Left = 305
        Top = 0
        Width = 320
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object NameEditor: TEdit
          Left = 0
          Top = 0
          Width = 320
          Height = 21
          Align = alClient
          TabOrder = 0
          OnChange = NameEditorChange
        end
      end
    end
  end
  object DeleteButton: TBitBtn
    Left = 0
    Top = 64
    Width = 97
    Height = 33
    Caption = 'Delete'
    ModalResult = 8
    TabOrder = 1
    OnClick = DeleteButtonClick
  end
  object SaveButton: TBitBtn
    Left = 445
    Top = 64
    Width = 97
    Height = 33
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TBitBtn
    Left = 548
    Top = 64
    Width = 97
    Height = 33
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
