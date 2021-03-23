object MainWindow: TMainWindow
  Left = 0
  Top = 0
  Caption = 'MainWindow'
  ClientHeight = 497
  ClientWidth = 811
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 233
    Top = 0
    Height = 456
    ExplicitLeft = 304
    ExplicitTop = 232
    ExplicitHeight = 100
  end
  object ListBox: TListBox
    Left = 0
    Top = 0
    Width = 233
    Height = 456
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    ExplicitHeight = 497
  end
  object ContentListBox: TVirtualStringTree
    Left = 236
    Top = 0
    Width = 575
    Height = 456
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    TabOrder = 1
    ExplicitLeft = 480
    ExplicitTop = 176
    ExplicitWidth = 200
    ExplicitHeight = 100
    Columns = <>
  end
  object HelpPanel: TPanel
    Left = 0
    Top = 456
    Width = 811
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 462
    object ShortcutPanel1: TPanel
      Left = 106
      Top = 1
      Width = 125
      Height = 39
      Align = alLeft
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object ShortcutLabel1: TLabel
        Left = 5
        Top = 5
        Width = 115
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = 'Ctrl + Alt + E'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 72
      end
      object Label1: TLabel
        Left = 5
        Top = 18
        Width = 115
        Height = 16
        Align = alClient
        Caption = 'Create new equipment.'
        ExplicitWidth = 113
        ExplicitHeight = 13
      end
    end
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 105
      Height = 39
      Align = alLeft
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 1
      object Label2: TLabel
        Left = 5
        Top = 5
        Width = 95
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = 'Ctrl + E'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 41
      end
      object Label3: TLabel
        Left = 5
        Top = 18
        Width = 95
        Height = 16
        Align = alClient
        Caption = 'Load equipment list.'
        ExplicitWidth = 96
        ExplicitHeight = 13
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 744
    Top = 400
    object Equipment: TMenuItem
      Caption = '&Equipment'
      object EquipmentList: TMenuItem
        Caption = 'E&quiplent list'
      end
      object EquipmentCreate: TMenuItem
        Caption = '&Create new'
      end
      object Delimiter1: TMenuItem
        Caption = '-'
      end
      object GreaseList: TMenuItem
        Caption = '&Grease list'
      end
    end
    object Shedule: TMenuItem
      Caption = '&Shedule'
      object SheduleList: TMenuItem
        Caption = 'S&hedule list'
      end
      object SheduleCreate: TMenuItem
        Caption = 'Shedule &new'
      end
    end
    object Help: TMenuItem
      Caption = '&Help'
      object About: TMenuItem
        Caption = '&About'
      end
    end
  end
end
