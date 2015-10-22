inherited UnkFrame: TUnkFrame
  object GroupBox2: TGroupBox
    Left = 0
    Top = 0
    Width = 320
    Height = 41
    Align = alTop
    Caption = ' '#1056#1077#1078#1080#1084' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '
    Padding.Left = 6
    TabOrder = 0
    object FlowPanel1: TFlowPanel
      Left = 8
      Top = 15
      Width = 310
      Height = 24
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object chkModeText: TRadioButton
        Left = 0
        Top = 0
        Width = 65
        Height = 17
        Caption = 'Text'
        TabOrder = 0
        OnClick = chkModeTextClick
      end
      object chkModeBinary: TRadioButton
        Tag = 1
        Left = 65
        Top = 0
        Width = 73
        Height = 17
        Caption = 'Binary'
        TabOrder = 1
        OnClick = chkModeTextClick
      end
      object chkModeHex: TRadioButton
        Tag = 2
        Left = 138
        Top = 0
        Width = 65
        Height = 17
        Caption = 'Hex'
        TabOrder = 2
        OnClick = chkModeTextClick
      end
      object chkModeUnicode: TRadioButton
        Tag = 3
        Left = 203
        Top = 0
        Width = 81
        Height = 17
        Caption = 'Unicode'
        TabOrder = 3
        OnClick = chkModeTextClick
      end
    end
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 41
    Width = 320
    Height = 199
    Align = alClient
    Caption = ' '#1057#1086#1076#1077#1088#1078#1080#1084#1086#1077' '#1092#1072#1081#1083#1072' '
    TabOrder = 1
    object Viewer: TATBinHex
      Left = 2
      Top = 15
      Width = 316
      Height = 182
      Cursor = crIBeam
      Align = alClient
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Caption = 'No file loaded'
      Color = clWindow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      TabStop = True
      FontOEM.Charset = OEM_CHARSET
      FontOEM.Color = clWindowText
      FontOEM.Height = -12
      FontOEM.Name = 'Terminal'
      FontOEM.Style = []
      FontFooter.Charset = DEFAULT_CHARSET
      FontFooter.Color = clBlack
      FontFooter.Height = -12
      FontFooter.Name = 'Arial'
      FontFooter.Style = []
      FontGutter.Charset = DEFAULT_CHARSET
      FontGutter.Color = clBlack
      FontGutter.Height = -12
      FontGutter.Name = 'Courier New'
      FontGutter.Style = []
      TextWidth = 65
      TextWidthHex = 12
      TextWidthFit = True
      TextWidthFitHex = True
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 16
    Top = 200
    LangData = {
      0800556E6B4672616D65000108000000090047726F7570426F78320101000000
      01000000070043617074696F6E000A00466C6F7750616E656C3100000B006368
      6B4D6F646554657874010100000002000000070043617074696F6E000D006368
      6B4D6F646542696E617279010100000003000000070043617074696F6E000A00
      63686B4D6F6465486578010100000004000000070043617074696F6E000E0063
      686B4D6F6465556E69636F6465010100000005000000070043617074696F6E00
      090047726F7570426F7831010100000006000000070043617074696F6E000600
      566965776572010100000007000000070043617074696F6E00}
  end
end
