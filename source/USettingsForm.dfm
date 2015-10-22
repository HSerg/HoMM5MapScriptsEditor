object SettingsForm: TSettingsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 363
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 129
    Top = 0
    Width = 5
    Height = 319
    ExplicitHeight = 266
  end
  object JvPageListTreeView1: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 129
    Height = 319
    AutoExpand = False
    ShowButtons = True
    ShowLines = True
    PageDefault = 0
    PageList = JvPageList1
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 0
    Items.Links = {00000000}
  end
  object JvPageList1: TJvPageList
    Left = 134
    Top = 0
    Width = 401
    Height = 319
    ActivePage = jvspLua
    PropagateEnable = False
    Align = alClient
    ParentBackground = True
    object jvspCommons: TJvStandardPage
      Left = 0
      Top = 0
      Width = 401
      Height = 319
      object GroupBox1: TGroupBox
        Left = 6
        Top = 3
        Width = 387
        Height = 308
        Caption = #1055#1088#1080' '#1086#1090#1082#1088#1099#1090#1080#1080' '#1082#1072#1088#1090#1099
        TabOrder = 0
        object CheckBox1: TCheckBox
          Left = 11
          Top = 21
          Width = 374
          Height = 17
          Caption = #1054#1090#1082#1088#1099#1074#1072#1090#1100' map.xdb ('#1090#1088#1077#1073#1091#1077#1090' '#1084#1085#1086#1075#1086' '#1088#1077#1089#1091#1088#1089#1086#1074', '#1079#1072#1084#1077#1076#1083#1103#1077#1090' '#1079#1072#1075#1088#1091#1079#1082#1091')'
          TabOrder = 0
        end
        object CheckBox2: TCheckBox
          Left = 11
          Top = 44
          Width = 339
          Height = 16
          Caption = #1054#1090#1082#1088#1099#1074#1072#1090#1100' MapScript.lua'
          TabOrder = 1
        end
        object RadioGroup1: TRadioGroup
          Left = 8
          Top = 68
          Width = 369
          Height = 108
          Caption = #1054#1090#1082#1088#1099#1074#1072#1090#1100' '#1087#1072#1087#1082#1080
          Items.Strings = (
            #1053#1077' '#1086#1090#1082#1088#1099#1074#1072#1090#1100
            #1058#1086#1083#1100#1082#1086' '#1087#1072#1087#1082#1091' '#1082#1072#1088#1090#1099
            #1055#1072#1087#1082#1091' '#1082#1072#1088#1090#1099' + '#1074#1089#1077' '#1074#1083#1086#1078#1077#1085#1085#1099#1077
            #1042#1089#1077)
          TabOrder = 2
        end
      end
    end
    object jvspEmpty: TJvStandardPage
      Left = 0
      Top = 0
      Width = 401
      Height = 319
      Caption = 'jvspEmpty'
    end
    object jvspLangs: TJvStandardPage
      Left = 0
      Top = 0
      Width = 401
      Height = 319
      Caption = 'jvspLangs'
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 401
        Height = 319
        Align = alClient
        Caption = #1071#1079#1099#1082
        TabOrder = 0
        object tntlstbxLang: TTntListBox
          Left = 2
          Top = 15
          Width = 397
          Height = 302
          Align = alClient
          ItemHeight = 13
          Items.Strings = (
            #1056#1091#1089#1089#1082#1080#1081' [Russian]')
          TabOrder = 0
        end
      end
    end
    object jvspLua: TJvStandardPage
      Left = 0
      Top = 0
      Width = 401
      Height = 319
      Caption = 'jvspLua'
      object GroupBox3: TGroupBox
        Left = 6
        Top = 3
        Width = 387
        Height = 308
        Caption = 'LUA-'#1088#1077#1076#1072#1082#1090#1086#1088
        TabOrder = 0
        object CheckBox4: TCheckBox
          Left = 11
          Top = 20
          Width = 339
          Height = 16
          Caption = #1055#1088#1080' '#1074#1089#1090#1072#1074#1082#1077' '#1080#1084#1077#1085#1080' '#1092#1072#1081#1083#1072' '#1080#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' "GetMapDataPath().."'
          TabOrder = 0
        end
        object CheckBox3: TCheckBox
          Left = 11
          Top = 42
          Width = 339
          Height = 16
          Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1087#1086#1083#1085#1086#1077' '#1086#1087#1080#1089#1072#1085#1080#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1086#1074' '#1092#1091#1085#1082#1094#1080#1080
          TabOrder = 1
        end
      end
    end
    object jvspPath: TJvStandardPage
      Left = 0
      Top = 0
      Width = 401
      Height = 319
      Caption = 'jvspPath'
      object GroupBox4: TGroupBox
        Left = 6
        Top = 3
        Width = 387
        Height = 308
        Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1091#1090#1077#1081
        TabOrder = 0
        object lblHMM5Path: TLabel
          Left = 10
          Top = 24
          Width = 165
          Height = 13
          Caption = #1055#1072#1087#1082#1072' '#1089' '#1091#1089#1090#1072#1085#1086#1074#1083#1077#1085#1085#1099#1084#1080' HoMM5'
        end
        object lblHMMVersion: TLabel
          Left = 24
          Top = 69
          Width = 345
          Height = 13
          AutoSize = False
          Caption = ' * '#1042#1077#1088#1089#1080#1103': -'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object JvDirectoryEdit1: TJvDirectoryEdit
          Left = 10
          Top = 43
          Width = 361
          Height = 21
          AcceptFiles = False
          DialogKind = dkWin32
          DialogOptions = []
          TabOrder = 0
          OnChange = JvDirectoryEdit1Change
        end
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 319
    Width = 535
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      535
      44)
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 535
      Height = 50
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = 104
      ExplicitWidth = 50
    end
    object Button1: TButton
      Left = 370
      Top = 10
      Width = 75
      Height = 25
      Action = acOK
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
    object Button2: TButton
      Left = 451
      Top = 10
      Width = 75
      Height = 25
      Action = acCancel
      Anchors = [akTop, akRight]
      TabOrder = 1
    end
  end
  object ActionList1: TActionList
    Left = 16
    Top = 272
    object acOK: TAction
      Caption = 'OK'
      Hint = 'OK'
      OnExecute = acOKExecute
    end
    object acCancel: TAction
      Caption = #1054#1090#1084#1077#1085#1072
      Hint = #1054#1090#1084#1077#1085#1072
      OnExecute = acCancelExecute
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      'jvspEmpty.Caption'
      'jvspLangs.Caption'
      'jvspLua.Caption'
      'jvspPath.Caption'
      'lblHMMVersion.Caption')
    Left = 16
    Top = 128
    LangData = {
      0C0053657474696E6773466F726D010100000001000000070043617074696F6E
      011C000000090053706C697474657231000013004A76506167654C6973745472
      6565566965773100000B004A76506167654C6973743100000B006A767370436F
      6D6D6F6E730000090047726F7570426F78310101000000030000000700436170
      74696F6E000900436865636B426F783101010000000400000007004361707469
      6F6E000900436865636B426F7832010100000005000000070043617074696F6E
      000B00526164696F47726F757031010200000006000000070043617074696F6E
      0700000005004974656D730009006A767370456D707479000009006A7673704C
      616E67730000090047726F7570426F783201010000000A000000070043617074
      696F6E000C00746E746C737462784C616E6701010000000B0000000500497465
      6D73000A00706E6C427574746F6E7300000600426576656C3100000700427574
      746F6E3100000700427574746F6E3200000B00416374696F6E4C697374310000
      040061634F4B01020000000C000000070043617074696F6E0D00000004004869
      6E74000800616343616E63656C01020000000E000000070043617074696F6E0F
      000000040048696E740007006A7673704C75610000090047726F7570426F7833
      010100000011000000070043617074696F6E000900436865636B426F78340101
      00000014000000070043617074696F6E0008006A767370506174680000090047
      726F7570426F7834010100000016000000070043617074696F6E0010004A7644
      69726563746F7279456469743100000B006C626C484D4D355061746801010000
      0017000000070043617074696F6E000D006C626C484D4D56657273696F6E0000
      0900436865636B426F7833010100000018000000070043617074696F6E00}
  end
end
