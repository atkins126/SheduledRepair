object JobWindow: TJobWindow
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Job Editor'
  ClientHeight = 444
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ObjectGroup: TGroupBox
    Left = 0
    Top = 0
    Width = 645
    Height = 75
    Align = alTop
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
        ExplicitHeight = 41
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
        ExplicitLeft = 480
        ExplicitTop = 24
        ExplicitWidth = 185
        ExplicitHeight = 41
        object NameEditor: TEdit
          Left = 0
          Top = 0
          Width = 320
          Height = 21
          Align = alClient
          TabOrder = 0
          ExplicitLeft = 64
          ExplicitTop = 16
          ExplicitWidth = 121
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
end
