inherited TxtFrame: TTxtFrame
  Width = 402
  Height = 284
  ParentFont = False
  ExplicitWidth = 402
  ExplicitHeight = 284
  object synedtText: TSynEdit
    Left = 0
    Top = 22
    Width = 402
    Height = 262
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    RightEdge = -1
    TabWidth = 2
    WordWrap = True
    OnChange = synedtTextChange
    OnCommandProcessed = synedtTextCommandProcessed
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 402
    Height = 22
    AutoSize = True
    ButtonWidth = 184
    EdgeInner = esNone
    EdgeOuter = esNone
    Images = CommonDM.imlActions
    List = True
    ShowCaptions = True
    TabOrder = 1
    Transparent = True
    object ToolButton2: TToolButton
      Left = 0
      Top = 0
      Action = acWordWrap
      AutoSize = True
      Style = tbsCheck
    end
    object ToolButton4: TToolButton
      Left = 105
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 67
      Style = tbsDivider
    end
    object ToolButton1: TToolButton
      Left = 113
      Top = 0
      Action = acTextReload
      AutoSize = True
    end
    object ToolButton3: TToolButton
      Left = 301
      Top = 0
      Action = acSave
      AutoSize = True
    end
  end
  object ActionList1: TActionList
    Images = CommonDM.imlActions
    Left = 192
    Top = 56
    object acTextReload: TAction
      Caption = #1042#1077#1088#1085#1091#1090#1100' '#1074' '#1080#1089#1093#1086#1076#1085#1086#1077' '#1089#1086#1089#1090#1086#1103#1085#1080#1077
      Hint = #1042#1077#1088#1085#1091#1090#1100' '#1074' '#1080#1089#1093#1086#1076#1085#1086#1077' '#1089#1086#1089#1090#1086#1103#1085#1080#1077
      ImageIndex = 68
      OnExecute = acTextReloadExecute
    end
    object acSave: TAction
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      ImageIndex = 66
      OnExecute = acSaveExecute
    end
    object acWordWrap: TAction
      AutoCheck = True
      Caption = #1055#1077#1088#1077#1085#1086#1089' '#1089#1090#1088#1086#1082
      Checked = True
      Hint = #1055#1077#1088#1077#1085#1086#1089' '#1089#1090#1088#1086#1082
      ImageIndex = 74
      OnExecute = acWordWrapExecute
    end
    object acAddToDictionary: TAction
      Caption = '&Add Word To Dictionary'
      OnExecute = acAddToDictionaryExecute
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 224
    Top = 56
    LangData = {
      08005478744672616D6500010E0000000A0073796E6564745465787400000800
      546F6F6C4261723100000B00546F6F6C427574746F6E3200000B00546F6F6C42
      7574746F6E34010100000001000000070043617074696F6E000B00546F6F6C42
      7574746F6E3100000B00546F6F6C427574746F6E3300000B00416374696F6E4C
      6973743100000C0061635465787452656C6F6164010200000002000000070043
      617074696F6E03000000040048696E7400060061635361766501020000000400
      0000070043617074696F6E05000000040048696E74000A006163576F72645772
      6170010200000006000000070043617074696F6E07000000040048696E740010
      0070706D6E7553756767657374696F6E73000011007069416464546F44696374
      696F6E617279000002004E31000011006163416464546F44696374696F6E6172
      7901010000000B000000070043617074696F6E00}
  end
  object ppmnuSuggestions: TPopupMenu
    Left = 136
    Top = 56
    object piAddToDictionary: TMenuItem
      Action = acAddToDictionary
    end
    object N1: TMenuItem
      Caption = '-'
    end
  end
end
