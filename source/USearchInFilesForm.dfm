object SearchInFilesForm: TSearchInFilesForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #1055#1086#1080#1089#1082' '#1074' '#1092#1072#1081#1083#1072#1093' '#1082#1072#1088#1090#1099'...'
  ClientHeight = 535
  ClientWidth = 470
  Color = clBtnFace
  Constraints.MinHeight = 330
  Constraints.MinWidth = 432
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 516
    Width = 470
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object pnlSettings: TPanel
    Left = 0
    Top = 0
    Width = 470
    Height = 162
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 61
      Height = 13
      Caption = #1063#1090#1086' '#1080#1089#1082#1072#1090#1100':'
    end
    object cboSearchText: TComboBox
      Left = 74
      Top = 8
      Width = 335
      Height = 21
      ItemHeight = 13
      TabOrder = 0
    end
    object gbSearchOptions: TGroupBox
      Left = 8
      Top = 40
      Width = 154
      Height = 89
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1086#1080#1089#1082#1072
      TabOrder = 1
      object chkSearchCaseSensitive: TCheckBox
        Left = 8
        Top = 17
        Width = 140
        Height = 17
        Caption = #1059#1095#1080#1090#1099#1074#1072#1090#1100' &'#1088#1077#1075#1080#1089#1090#1088
        TabOrder = 0
      end
      object chkSearchWholeWords: TCheckBox
        Left = 8
        Top = 39
        Width = 140
        Height = 17
        Caption = #1058#1086#1083#1100#1082#1086' &'#1094#1077#1083#1099#1077' '#1089#1083#1086#1074#1072
        TabOrder = 1
      end
      object chkRegularExpression: TCheckBox
        Left = 8
        Top = 62
        Width = 140
        Height = 17
        Caption = #1056#1077#1075#1091#1083#1103#1088#1085#1086#1077' '#1074#1099#1088#1072#1078#1077#1085#1080#1077
        TabOrder = 2
      end
    end
    object btnFind: TButton
      Left = 253
      Top = 135
      Width = 75
      Height = 23
      Action = acFind
      Default = True
      TabOrder = 2
    end
    object gbFilesFilter: TGroupBox
      Left = 168
      Top = 40
      Width = 241
      Height = 89
      Caption = #1058#1080#1087#1099' '#1092#1072#1081#1083#1086#1074
      Padding.Left = 3
      Padding.Right = 3
      Padding.Bottom = 3
      TabOrder = 3
      object GridPanel1: TGridPanel
        Left = 5
        Top = 15
        Width = 231
        Height = 69
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = chkLUA
            Row = 0
          end
          item
            Column = 1
            Control = chkXDB
            Row = 0
          end
          item
            Column = 0
            Control = chkTXT
            Row = 1
          end
          item
            Column = 1
            Control = chkOthers
            Row = 1
          end>
        Padding.Left = 2
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        TabOrder = 0
        object chkLUA: TCheckBox
          Left = 4
          Top = 0
          Width = 112
          Height = 34
          Align = alClient
          Caption = #1057#1082#1088#1080#1087#1090#1099' (*.lua)'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object chkXDB: TCheckBox
          Left = 118
          Top = 0
          Width = 113
          Height = 34
          Align = alClient
          Caption = 'XDB-'#1092#1072#1081#1083#1099' (*.xdb)'
          TabOrder = 1
        end
        object chkTXT: TCheckBox
          Left = 4
          Top = 34
          Width = 112
          Height = 35
          Align = alClient
          Caption = #1058#1077#1082#1089#1090#1099' (*.txt)'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object chkOthers: TCheckBox
          Left = 118
          Top = 34
          Width = 113
          Height = 35
          Align = alClient
          Caption = #1055#1088#1086#1095#1080#1077' '#1092#1072#1081#1083#1099
          TabOrder = 3
        end
      end
    end
    object btnClose: TButton
      Left = 334
      Top = 135
      Width = 75
      Height = 23
      Action = acClose
      Cancel = True
      TabOrder = 4
    end
  end
  object vstData: TVirtualDrawTree
    Left = 0
    Top = 162
    Width = 470
    Height = 354
    Align = alClient
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
    HintMode = hmHint
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
    OnDblClick = vstDataDblClick
    OnDrawNode = vstDataDrawNode
    OnGetNodeWidth = vstDataGetNodeWidth
    Columns = <
      item
        Position = 0
        Width = 470
        WideText = #1048#1084#1103' '#1092#1072#1081#1083#1072
      end>
  end
  object ActionList1: TActionList
    Left = 9
    Top = 207
    object acFind: TAction
      Caption = #1053#1072#1081#1090#1080
      OnExecute = acFindExecute
    end
    object acClose: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100
      OnExecute = acCloseExecute
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 40
    Top = 207
    LangData = {
      1100536561726368496E46696C6573466F726D01010000000100000007004361
      7074696F6E01160000000B00416374696F6E4C6973743100000600616346696E
      6401010000000E000000070043617074696F6E000A0053746174757342617231
      00000B00706E6C53657474696E6773000006004C6162656C3101010000000F00
      0000070043617074696F6E000D0063626F5365617263685465787400000F0067
      625365617263684F7074696F6E73010100000010000000070043617074696F6E
      00160063686B5365617263684361736553656E73697469766501010000001100
      0000070043617074696F6E00130063686B53656172636857686F6C65576F7264
      73010100000012000000070043617074696F6E00140063686B526567756C6172
      45787072657373696F6E010100000013000000070043617074696F6E00070062
      746E46696E6400000D00676246696C657346696C746572010100000015000000
      070043617074696F6E00080062746E436C6F73650000090073796E5365617263
      6800000E0073796E536561726368526567657800000A004772696450616E656C
      310000060063686B4C554101010000001A000000070043617074696F6E000600
      63686B58444201010000001B000000070043617074696F6E00060063686B5458
      5401010000001C000000070043617074696F6E00090063686B4F746865727301
      010000001D000000070043617074696F6E0007006163436C6F73650101000000
      1E000000070043617074696F6E000700767374446174610000}
  end
  object synSearch: TSynEditSearch
    Left = 160
    Top = 216
  end
  object synSearchRegex: TSynEditRegexSearch
    Left = 192
    Top = 216
  end
end
