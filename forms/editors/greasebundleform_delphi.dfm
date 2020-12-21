object GreaseBundleWindow: TGreaseBundleWindow
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Grease editor'
  ClientHeight = 160
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ObjectGroup: TGroupBox
    Left = 0
    Top = 0
    Width = 645
    Height = 100
    Align = alCustom
    Caption = 'Object'
    TabOrder = 0
    object SupplierGroup: TPanel
      Left = 10
      Top = 17
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 0
      object SupplierLabelGroup: TPanel
        Left = 0
        Top = 0
        Width = 305
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object SupplierLabel: TLabel
          Left = 0
          Top = 0
          Width = 305
          Height = 21
          Align = alClient
          Caption = 'Supplier'
          Layout = tlCenter
          ExplicitWidth = 38
          ExplicitHeight = 13
        end
      end
      object SupplierEditorGroup: TPanel
        Left = 305
        Top = 0
        Width = 320
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object SupplierEditor: TComboBox
          Left = 0
          Top = 0
          Width = 320
          Height = 21
          Align = alClient
          TabOrder = 0
          OnChange = SupplierEditorChange
        end
      end
    end
    object GradeGroup: TPanel
      Left = 10
      Top = 40
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 1
      object GradeLabelGroup: TPanel
        Left = 0
        Top = 0
        Width = 305
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object GradeLabel: TLabel
          Left = 0
          Top = 0
          Width = 305
          Height = 21
          Align = alClient
          Caption = 'Grade'
          Layout = tlCenter
          ExplicitWidth = 29
          ExplicitHeight = 13
        end
      end
      object GradeEditorGroup: TPanel
        Left = 305
        Top = 0
        Width = 320
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object GradeEditor: TComboBox
          Left = 0
          Top = 0
          Width = 320
          Height = 21
          Align = alClient
          TabOrder = 0
          OnChange = GradeEditorChange
        end
      end
    end
    object QuantityGroup: TPanel
      Left = 10
      Top = 65
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 2
      object QuantityLabelGroup: TPanel
        Left = 0
        Top = 0
        Width = 305
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object QuantityLabel: TLabel
          Left = 0
          Top = 0
          Width = 305
          Height = 21
          Align = alClient
          Caption = 'Quantity'
          Layout = tlCenter
          ExplicitWidth = 42
          ExplicitHeight = 13
        end
      end
      object QuantityEditorGroup: TPanel
        Left = 305
        Top = 0
        Width = 320
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object QuantityCountEditor: TSpinEdit
          Left = 0
          Top = 0
          Width = 160
          Height = 22
          Align = alLeft
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object QuantityMeasureEditor: TComboBox
          Left = 160
          Top = 0
          Width = 160
          Height = 21
          Align = alClient
          TabOrder = 1
          OnChange = QuantityMeasureEditorChange
        end
      end
    end
  end
  object DeleteButton: TBitBtn
    Left = 0
    Top = 120
    Width = 97
    Height = 33
    Caption = 'Delete'
    ModalResult = 8
    TabOrder = 1
    OnClick = DeleteButtonClick
  end
  object SaveButton: TBitBtn
    Left = 445
    Top = 120
    Width = 97
    Height = 33
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TBitBtn
    Left = 548
    Top = 120
    Width = 97
    Height = 33
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
