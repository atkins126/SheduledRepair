object EntityWindow: TEntityWindow
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Entity editor'
  ClientHeight = 241
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
    object QuantityGroup: TPanel
      Left = 10
      Top = 40
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 1
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
        end
      end
    end
    object PeriodGroup: TPanel
      Left = 10
      Top = 65
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 2
      object PeriodLabelGroup: TPanel
        Left = 0
        Top = 0
        Width = 305
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object PeriodLabel: TLabel
          Left = 0
          Top = 0
          Width = 305
          Height = 21
          Align = alClient
          Caption = 'Period'
          Layout = tlCenter
          ExplicitWidth = 30
          ExplicitHeight = 13
        end
      end
      object PeriodEditorGroup: TPanel
        Left = 305
        Top = 0
        Width = 320
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object PeriodCountEditor: TSpinEdit
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
        object PeriodMeasureEditor: TComboBox
          Left = 160
          Top = 0
          Width = 160
          Height = 21
          Align = alClient
          TabOrder = 1
        end
      end
    end
  end
  object SheduleGroup: TGroupBox
    Left = 0
    Top = 103
    Width = 645
    Height = 75
    Align = alCustom
    Caption = 'Shedule'
    TabOrder = 1
    object ShedulePrevGroup: TPanel
      Left = 10
      Top = 17
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 0
      object ShedulePrevNameLabelGroup: TPanel
        Left = 0
        Top = 0
        Width = 305
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object ShedulePrevNameLabel: TLabel
          Left = 0
          Top = 0
          Width = 305
          Height = 21
          Align = alClient
          Caption = 'Previous date'
          Layout = tlCenter
          ExplicitWidth = 66
          ExplicitHeight = 13
        end
      end
      object ShedulePrevEditorGroup: TPanel
        Left = 305
        Top = 0
        Width = 320
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object ShedulePrevEditor: TDateTimePicker
          Left = 0
          Top = 0
          Width = 320
          Height = 21
          Align = alClient
          Date = 44186.000000000000000000
          Time = 0.431694849539781000
          TabOrder = 0
        end
      end
    end
    object SheduleNextGroup: TPanel
      Left = 10
      Top = 40
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 1
      object SheduleNextNameLabelGroup: TPanel
        Left = 0
        Top = 0
        Width = 305
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object SheduleNextNameLabel: TLabel
          Left = 0
          Top = 0
          Width = 305
          Height = 21
          Align = alClient
          Caption = 'Next date'
          Layout = tlCenter
          ExplicitWidth = 48
          ExplicitHeight = 13
        end
      end
      object SheduleNextEditorGroup: TPanel
        Left = 305
        Top = 0
        Width = 320
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object SheduleNextEditor: TDateTimePicker
          Left = 0
          Top = 0
          Width = 320
          Height = 21
          Align = alClient
          Date = 44186.000000000000000000
          Time = 0.431694849539781000
          TabOrder = 0
        end
      end
    end
  end
  object DeleteButton: TBitBtn
    Left = 0
    Top = 200
    Width = 97
    Height = 33
    Caption = 'Delete'
    ModalResult = 8
    TabOrder = 2
    OnClick = DeleteButtonClick
  end
  object SaveButton: TBitBtn
    Left = 445
    Top = 200
    Width = 97
    Height = 33
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TBitBtn
    Left = 548
    Top = 200
    Width = 97
    Height = 33
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
