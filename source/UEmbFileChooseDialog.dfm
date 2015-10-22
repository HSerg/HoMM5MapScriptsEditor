object EmbFileChooseDialog: TEmbFileChooseDialog
  Left = 0
  Top = 0
  ActiveControl = H5MFrame1.vstH5MContent
  BorderIcons = [biSystemMenu]
  Caption = #1042#1099#1073#1086#1088' '#1092#1072#1081#1083#1072
  ClientHeight = 357
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 273
    Top = 0
    Width = 5
    Height = 317
    ExplicitLeft = 354
    ExplicitHeight = 226
  end
  object FlowPanel1: TFlowPanel
    Left = 0
    Top = 317
    Width = 554
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    FlowStyle = fsRightLeftBottomTop
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 1
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 471
      Top = 7
      Width = 75
      Height = 25
      Caption = #1054#1090#1084#1077#1085#1072
      TabOrder = 1
      OnClick = btnCancelClick
    end
    object btnChoose: TButton
      AlignWithMargins = True
      Left = 390
      Top = 7
      Width = 75
      Height = 25
      Caption = #1042#1099#1073#1088#1072#1090#1100
      TabOrder = 0
      OnClick = btnChooseClick
    end
  end
  inline H5MFrame1: TH5MFrame
    Left = 0
    Top = 0
    Width = 273
    Height = 317
    Align = alLeft
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 273
    ExplicitHeight = 317
    inherited vstH5MContent: TVirtualStringTree
      Width = 273
      Height = 317
      ExplicitWidth = 273
      ExplicitHeight = 317
    end
    inherited DKLanguageController1: TDKLanguageController
      LangData = {
        090048354D4672616D65310001010000000D0076737448354D436F6E74656E74
        0000}
    end
  end
  object Panel1: TPanel
    Left = 278
    Top = 0
    Width = 276
    Height = 317
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object lblPreview: TLabel
      Left = 0
      Top = 0
      Width = 276
      Height = 25
      Margins.Top = 10
      Margins.Bottom = 5
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = #1055#1088#1077#1076#1087#1088#1086#1089#1084#1086#1090#1088' '#1092#1072#1081#1083#1072
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
      Layout = tlCenter
      ExplicitLeft = 1
      ExplicitTop = -8
    end
    object synedtPreview: TSynEdit
      Left = 0
      Top = 25
      Width = 276
      Height = 292
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      TabOrder = 0
      TabStop = False
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Gutter.Visible = False
      ReadOnly = True
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 16
    Top = 8
    LangData = {
      1300456D6246696C6543686F6F73654469616C6F670101000000010000000700
      43617074696F6E0108000000090053706C69747465723100000A00466C6F7750
      616E656C310000090062746E43616E63656C0101000000020000000700436170
      74696F6E00090062746E43686F6F736501010000000300000007004361707469
      6F6E00090048354D4672616D65310001010000000D0076737448354D436F6E74
      656E740000060050616E656C3100000A006C626C507265766965770101000000
      04000000070043617074696F6E000D0073796E656474507265766965770000}
  end
end
