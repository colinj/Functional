object FrmDemo: TFrmDemo
  Left = 0
  Top = 0
  Caption = 'Sequence Demo'
  ClientHeight = 528
  ClientWidth = 806
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 198
    Height = 522
    ActivePage = TabSheet1
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 415
    object TabSheet1: TTabSheet
      Caption = 'TSequence'
      ExplicitLeft = 5
      ExplicitTop = 25
      object Button1: TButton
        Left = 32
        Top = 24
        Width = 120
        Height = 25
        Action = actDemo01
        TabOrder = 0
      end
      object Button2: TButton
        Left = 32
        Top = 55
        Width = 120
        Height = 25
        Action = actDemo02
        TabOrder = 1
      end
      object Button3: TButton
        Left = 32
        Top = 86
        Width = 120
        Height = 25
        Action = actDemo03A
        TabOrder = 2
      end
      object Button4: TButton
        Left = 32
        Top = 117
        Width = 120
        Height = 25
        Action = actDemo03B
        TabOrder = 3
      end
      object Button5: TButton
        Left = 32
        Top = 148
        Width = 120
        Height = 25
        Action = actDemo04
        TabOrder = 4
      end
      object Button9: TButton
        Left = 32
        Top = 179
        Width = 120
        Height = 25
        Action = actDemo05
        TabOrder = 5
      end
      object Button6: TButton
        Left = 32
        Top = 210
        Width = 120
        Height = 25
        Action = actDemo06
        TabOrder = 6
      end
      object Button7: TButton
        Left = 32
        Top = 241
        Width = 120
        Height = 25
        Action = actDemo07
        TabOrder = 7
      end
      object Button8: TButton
        Left = 32
        Top = 272
        Width = 120
        Height = 25
        Action = actDemo08
        TabOrder = 8
      end
      object Button11: TButton
        Left = 32
        Top = 303
        Width = 120
        Height = 25
        Action = actDemo09
        TabOrder = 9
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'DataSet'
      ImageIndex = 2
      ExplicitHeight = 387
      object Button10: TButton
        Left = 24
        Top = 20
        Width = 75
        Height = 25
        Caption = 'Button5'
        TabOrder = 0
        OnClick = Button5Click
      end
      object btnDsLoop: TButton
        Left = 24
        Top = 64
        Width = 75
        Height = 25
        Caption = 'Loop'
        TabOrder = 1
        OnClick = btnDsLoopClick
      end
    end
  end
  object Panel1: TPanel
    Left = 204
    Top = 0
    Width = 602
    Height = 528
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitWidth = 458
    ExplicitHeight = 421
    object PageControl2: TPageControl
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 594
      Height = 520
      ActivePage = TabSheet3
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 450
      ExplicitHeight = 374
      object TabSheet3: TTabSheet
        Caption = 'Log'
        ExplicitWidth = 442
        ExplicitHeight = 346
        object Memo1: TMemo
          Left = 0
          Top = 0
          Width = 586
          Height = 492
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
          ExplicitWidth = 442
          ExplicitHeight = 346
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'DataSet'
        ImageIndex = 1
        ExplicitWidth = 442
        ExplicitHeight = 346
        object DBGrid1: TDBGrid
          Left = 0
          Top = 0
          Width = 586
          Height = 492
          Align = alClient
          DataSource = DataSource1
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
        end
      end
    end
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    FileName = 'data\employee.cds'
    Params = <>
    Left = 640
    Top = 144
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 640
    Top = 200
  end
  object atlDemo: TActionList
    Left = 640
    Top = 72
    object actDemo01: TAction
      Caption = 'ForEach'
      OnExecute = actDemo01Execute
    end
    object actDemo02: TAction
      Caption = 'Where / Filter'
      OnExecute = actDemo02Execute
    end
    object actDemo03A: TAction
      Caption = 'Select / Map'
      OnExecute = actDemo03AExecute
    end
    object actDemo03B: TAction
      Caption = 'Select / Map (2)'
      OnExecute = actDemo03BExecute
    end
    object actDemo04: TAction
      Caption = 'Take && Skip'
      OnExecute = actDemo04Execute
    end
    object actDemo05: TAction
      Caption = 'TakeWhile'
      OnExecute = actDemo05Execute
    end
    object actDemo06: TAction
      Caption = 'SkipWhile'
      OnExecute = actDemo06Execute
    end
    object actDemo07: TAction
      Caption = 'Fold'
      OnExecute = actDemo07Execute
    end
    object actDemo08: TAction
      Caption = 'String Demo'
      OnExecute = actDemo08Execute
    end
    object actDemo09: TAction
      Caption = 'TStrings Demo'
      OnExecute = actDemo09Execute
    end
  end
end
