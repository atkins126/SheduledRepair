object JobWindow: TJobWindow
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Job Editor'
  ClientHeight = 316
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
    Height = 75
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
    object PeriodGroup: TPanel
      Left = 10
      Top = 42
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 1
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
  object EntityGroup: TGroupBox
    Left = 0
    Top = 78
    Width = 645
    Height = 99
    Align = alCustom
    Caption = 'Entity'
    TabOrder = 1
    object EntityNameGroup: TPanel
      Left = 10
      Top = 17
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 0
      object EntityNameLabelGroup: TPanel
        Left = 0
        Top = 0
        Width = 305
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object EntityNameLabel: TLabel
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
      object EntityNameEditorGroup: TPanel
        Left = 305
        Top = 0
        Width = 320
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object EntityNameSelector: TComboBox
          Left = 0
          Top = 0
          Width = 320
          Height = 21
          Align = alClient
          Style = csDropDownList
          TabOrder = 0
          OnChange = EntityNameSelectorChange
        end
      end
    end
    object EntityCountGroup: TPanel
      Left = 10
      Top = 41
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 1
      object EntityCountLabelGroup: TPanel
        Left = 0
        Top = 0
        Width = 305
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object EntityCountLabel: TLabel
          Left = 0
          Top = 0
          Width = 305
          Height = 21
          Align = alClient
          Caption = 'Count'
          Layout = tlCenter
          ExplicitWidth = 29
          ExplicitHeight = 13
        end
      end
      object EntityCountEditorGroup: TPanel
        Left = 305
        Top = 0
        Width = 320
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object EntityCountEditor: TEdit
          Left = 0
          Top = 0
          Width = 320
          Height = 21
          Align = alClient
          Enabled = False
          TabOrder = 0
        end
      end
    end
    object EntityPeriodGroup: TPanel
      Left = 10
      Top = 65
      Width = 625
      Height = 21
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 2
      object EntityPeriodLabelGroup: TPanel
        Left = 0
        Top = 0
        Width = 305
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object EntityPeriodLabel: TLabel
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
      object EntityPeriodEditorGroup: TPanel
        Left = 305
        Top = 0
        Width = 320
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object EntityPeriodEditor: TEdit
          Left = 0
          Top = 0
          Width = 320
          Height = 21
          Align = alClient
          Enabled = False
          TabOrder = 0
        end
      end
    end
  end
  object SheduleGroup: TGroupBox
    Left = 0
    Top = 180
    Width = 645
    Height = 75
    Align = alCustom
    Caption = 'Shedule'
    TabOrder = 2
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
    Top = 272
    Width = 97
    Height = 33
    Caption = 'Delete'
    ModalResult = 8
    TabOrder = 3
    OnClick = DeleteButtonClick
  end
  object CancelButton: TBitBtn
    Left = 548
    Top = 272
    Width = 97
    Height = 33
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object SaveButton: TBitBtn
    Left = 445
    Top = 272
    Width = 97
    Height = 33
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 5
  end
end
