object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'Form6'
  ClientHeight = 421
  ClientWidth = 662
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 198
    Height = 415
    ActivePage = TabSheet1
    Align = alLeft
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = 'Counter'
      ImageIndex = 1
      ExplicitWidth = 159
      object Button6: TButton
        Left = 16
        Top = 16
        Width = 129
        Height = 25
        Caption = 'Static Counter'
        TabOrder = 0
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 16
        Top = 72
        Width = 129
        Height = 25
        Caption = 'Anonymous Counter'
        TabOrder = 1
        OnClick = Button7Click
      end
      object Button11: TButton
        Left = 56
        Top = 160
        Width = 75
        Height = 25
        Caption = 'Button11'
        TabOrder = 2
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'TSequence'
      ExplicitWidth = 159
      object Button1: TButton
        Left = 32
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 32
        Top = 55
        Width = 75
        Height = 25
        Caption = 'Button2'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 32
        Top = 86
        Width = 75
        Height = 25
        Caption = 'Button3'
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 32
        Top = 117
        Width = 75
        Height = 25
        Caption = 'Button4'
        TabOrder = 3
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 32
        Top = 148
        Width = 75
        Height = 25
        Caption = 'Button5'
        TabOrder = 4
        OnClick = Button5Click
      end
      object Button9: TButton
        Left = 32
        Top = 200
        Width = 75
        Height = 25
        Caption = 'Button9'
        TabOrder = 5
        OnClick = Button9Click
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'DataSet'
      ImageIndex = 2
      ExplicitWidth = 159
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
    Width = 458
    Height = 421
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitLeft = 173
    ExplicitWidth = 489
    object Panel2: TPanel
      Left = 1
      Top = 381
      Width = 456
      Height = 39
      Align = alBottom
      TabOrder = 0
      ExplicitWidth = 487
      DesignSize = (
        456
        39)
      object Button8: TButton
        Left = 348
        Top = 3
        Width = 95
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Clear'
        TabOrder = 0
        OnClick = Button8Click
        ExplicitLeft = 379
      end
    end
    object PageControl2: TPageControl
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 450
      Height = 374
      ActivePage = TabSheet3
      Align = alClient
      TabOrder = 1
      ExplicitWidth = 481
      object TabSheet3: TTabSheet
        Caption = 'Log'
        ExplicitWidth = 473
        object Memo1: TMemo
          Left = 0
          Top = 0
          Width = 442
          Height = 346
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
          ExplicitWidth = 473
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'DataSet'
        ImageIndex = 1
        ExplicitWidth = 473
        object DBGrid1: TDBGrid
          Left = 0
          Top = 0
          Width = 442
          Height = 346
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
    Active = True
    Aggregates = <>
    FileName = 
      'C:\Users\Public\Documents\RAD Studio\9.0\Samples\Data\employee.c' +
      'ds'
    Params = <>
    Left = 40
    Top = 272
    Data = {
      890700009619E0BD010000001800000006002A00000003000000DB0005456D70
      4E6F0400010000000000084C6173744E616D6501004900000001000557494454
      480200020014000946697273744E616D65010049000000010005574944544802
      0002000F000850686F6E65457874010049000000010005574944544802000200
      040008486972654461746508000800000000000653616C617279080004000000
      000003000D44454641554C545F4F52444552020082000100000001000B505249
      4D4152595F4B455902008200010000000100044C434944040001000904000000
      000002000000064E656C736F6E07526F626572746F033235300000EA4F4F87CC
      42000000000088E3400000000400000005596F756E6705427275636503323333
      0000EA4F4F87CC42000000008019EB4000000005000000074C616D6265727403
      4B696D02323200001A4FB687CC4200000000006AD84000000008000000074A6F
      686E736F6E064C65736C696503343130000086A74B88CC42000000008076D840
      0000000900000006466F72657374045068696C033232390000AE8D6A88CC4200
      0000008076D8400000000B00000006576573746F6E054B2E204A2E0233340000
      18A82E8BCC42000000009E41E0400000000C000000034C656505546572726903
      3235360000C8723A8CCC42000000008022E6400000000E0000000448616C6C07
      53746577617274033232370000E4FE918CCC420000000054D6E0400000000F00
      000005596F756E67094B6174686572696E65033233310000B0BEAB8CCC420000
      000000D4D740000000140000000C50617061646F706F756C6F73054368726973
      0338383700003875058BCC42000000008076D840000000180000000646697368
      65720450657465033838380000DC7C938DCC42000000000080D6400000001C00
      00000642656E6E657403416E6E013500006020018FCC429A99999959D6E04000
      00001D00000008446520536F757A6105526F6765720332383800006EE62C8FCC
      420000000000E7D840000000220000000742616C6477696E054A616E65740132
      000000B97C8FCC420000000000C1D64000000024000000065265657665730552
      6F676572013600004AD8D68FCC4200000000806AE04000000025000000095374
      616E73627572790657696C6C6965013700004AD8D68FCC42000000000027E340
      0000002C0000000550686F6E67064C65736C69650332313600004C443B90CC42
      00000000C0B3E3400000002D0000000A52616D616E617468616E054173686F6B
      033230390000E62FD390CC4248E17A149E41E0400000002E0000000853746561
      646D616E0657616C74657203323130000056C9E790CC4200000000C023D34000
      000034000000094E6F72647374726F6D054361726F6C0334323000000AD57291
      CC42000000000094B1400000003D000000054C65756E67044C756B6501330000
      04BFD892CC420000000080D8E04000000041000000074F27427269656E085375
      6520416E6E65033837370000204B3093CC4200000000C08ADE40000000470000
      000742757262616E6B0B4A656E6E69666572204D2E03323839000042846B93CC
      42000000008022E640004000480000000A5375746865726C616E6407436C6175
      646961000028647893CC4200000000606EE1400000005300000006426973686F
      700444616E61033239300000B489E493CC420000000000F9E540000000550000
      00094D6163446F6E616C64074D61727920532E033437370000B489E493CC4200
      000000606EE1400000005E0000000857696C6C69616D730552616E6479033839
      320000ECA19394CC42000000000039DC40000000690000000642656E64657209
      4F6C6976657220482E033235350000E2B33095CC4200000000E0F7E140000000
      6B00000004436F6F6B054B6576696E033839340000BA645B96CC420000000080
      55E1400000006D0000000542726F776E054B656C6C79033230320000441E6396
      CC4200000000005EDA400000006E000000064963686964610459756B69023232
      0000441E6396CC42000000004016D940000000710000000450616765044D6172
      790338343500004EA30F97CC42000000000070E7400000007200000006506172
      6B65720442696C6C0332343700004A629097CC42000000000017E14000000076
      0000000859616D616D6F746F0754616B617368690232330000AEA1DD97CC4200
      00000000BDDF4000000079000000074665727261726907526F626572746F0131
      0000A8F4F997CC420000000080C6E3400000007F0000000859616E6F77736B69
      074D69636861656C033439320000B00D4298CC4200000000007CE54000400086
      00000004476C6F6E074A6163717565730000341A6698CC4200000000C045D840
      00000088000000074A6F686E736F6E0553636F7474033236350000FA2C9C98CC
      42C3F5285C3FDFDD400000008A00000005477265656E04542E4A2E0332313800
      00C8581A99CC42000000000094E1400040008D000000074F73626F726E650650
      696572726500001A91BC99CC42000000000062E140000000900000000A4D6F6E
      74676F6D657279044A6F686E0338323000008E029A9ACC4200000000606EE140
      000000910000000C4775636B656E6865696D6572044D61726B0332323100007C
      FBEE9ACC42000000000040DF40}
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 64
    Top = 352
  end
end
