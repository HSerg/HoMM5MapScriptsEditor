object ReplaceQueryForm: TReplaceQueryForm
  Left = 413
  Top = 261
  BorderStyle = bsDialog
  Caption = #1047#1072#1084#1077#1085#1080#1090#1100'?'
  ClientHeight = 95
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblConfirmation: TLabel
    Left = 60
    Top = 12
    Width = 261
    Height = 44
    AutoSize = False
    WordWrap = True
  end
  object imgIcon: TImage
    Left = 16
    Top = 16
    Width = 32
    Height = 32
  end
  object btnReplace: TButton
    Left = 8
    Top = 67
    Width = 75
    Height = 23
    Caption = '&'#1044#1072
    Default = True
    ModalResult = 6
    TabOrder = 0
  end
  object btnSkip: TButton
    Left = 87
    Top = 67
    Width = 75
    Height = 23
    Caption = '&'#1053#1077#1090
    ModalResult = 7
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 166
    Top = 67
    Width = 75
    Height = 23
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 2
  end
  object btnReplaceAll: TButton
    Left = 245
    Top = 67
    Width = 75
    Height = 23
    Caption = #1044#1072' '#1076#1083#1103' &'#1074#1089#1077#1093
    ModalResult = 10
    TabOrder = 3
  end
  object DKLanguageController1: TDKLanguageController
    Left = 296
    Top = 8
    LangData = {
      10005265706C6163655175657279466F726D0101000000010000000700436170
      74696F6E01060000000F006C626C436F6E6669726D6174696F6E00000700696D
      6749636F6E00000A0062746E5265706C61636501010000000200000007004361
      7074696F6E00070062746E536B6970010100000003000000070043617074696F
      6E00090062746E43616E63656C010100000004000000070043617074696F6E00
      0D0062746E5265706C616365416C6C010100000005000000070043617074696F
      6E00}
  end
end
