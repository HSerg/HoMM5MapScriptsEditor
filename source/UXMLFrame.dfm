inherited XMLFrame: TXMLFrame
  Width = 369
  Height = 333
  OnResize = FrameResize
  ExplicitWidth = 369
  ExplicitHeight = 333
  object JvTabBar1: TJvTabBar
    Left = 0
    Top = 310
    Width = 369
    Align = alBottom
    Orientation = toBottom
    CloseButton = False
    AutoFreeClosed = False
    Margin = 4
    PageListTabLink = True
    PageList = JvPageList1
    Painter = JvModernTabBarPainter1
    Tabs = <
      item
        Caption = #1044#1077#1088#1077#1074#1086
        Selected = True
      end
      item
        Caption = #1058#1077#1082#1089#1090
      end>
  end
  object JvPageList1: TJvPageList
    Left = 0
    Top = 0
    Width = 369
    Height = 310
    ActivePage = jvstdpgXMLTree
    PropagateEnable = False
    Align = alClient
    ParentBackground = True
    object jvstdpgXMLTree: TJvStandardPage
      Left = 0
      Top = 0
      Width = 369
      Height = 310
      Caption = #1044#1077#1088#1077#1074#1086
      ParentBackground = True
      object GridPanel1: TGridPanel
        Left = 0
        Top = 0
        Width = 369
        Height = 310
        Align = alClient
        ColumnCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 50.000000000000000000
          end
          item
            Value = 100.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 30.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = Label1
            Row = 0
          end
          item
            Column = 1
            Control = edtFilter
            Row = 0
          end
          item
            Column = 0
            ColumnSpan = 3
            Control = vstData
            Row = 1
          end
          item
            Column = 2
            Control = JvSpeedButton1
            Row = 0
          end>
        RowCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 27.000000000000000000
          end
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 0
        DesignSize = (
          369
          310)
        object Label1: TLabel
          Left = 7
          Top = 8
          Width = 38
          Height = 13
          Anchors = []
          Caption = #1060#1080#1083#1100#1090#1088
          ExplicitLeft = 34
          ExplicitTop = 30
        end
        object edtFilter: TEdit
          AlignWithMargins = True
          Left = 54
          Top = 4
          Width = 281
          Height = 21
          Align = alClient
          TabOrder = 0
          OnKeyDown = edtFilterKeyDown
        end
        object vstData: TVirtualStringTree
          Left = 1
          Top = 28
          Width = 367
          Height = 281
          Align = alClient
          BevelEdges = [beLeft, beTop, beRight]
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'Tahoma'
          Header.Font.Style = []
          Header.Options = [hoColumnResize, hoDrag, hoVisible]
          Images = CommonDM.imglstXML
          TabOrder = 1
          OnFocusChanged = vstDataFocusChanged
          OnFreeNode = vstDataFreeNode
          OnGetText = vstDataGetText
          OnPaintText = vstDataPaintText
          OnGetImageIndex = vstDataGetImageIndex
          Columns = <
            item
              Position = 0
              Width = 200
              WideText = #1069#1083#1077#1084#1077#1085#1090'/'#1040#1090#1088#1080#1073#1091#1090
            end
            item
              Position = 1
              Width = 150
              WideText = #1047#1085#1072#1095#1077#1085#1080#1077
            end>
        end
        object JvSpeedButton1: TJvSpeedButton
          Left = 340
          Top = 2
          Width = 25
          Height = 25
          Anchors = []
          DropDownMenu = ppmnuFilterOptions
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -11
          HotTrackFont.Name = 'Tahoma'
          HotTrackFont.Style = []
          Transparent = True
        end
      end
    end
    object jvstdpgXMLText: TJvStandardPage
      Left = 0
      Top = 0
      Width = 369
      Height = 310
      Caption = #1058#1077#1082#1089#1090
      object synedtXMLText: TSynEdit
        Left = 0
        Top = 0
        Width = 369
        Height = 310
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        TabStop = False
        BorderStyle = bsNone
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Visible = False
        Highlighter = SynXMLSyn1
        ReadOnly = True
        RightEdge = 8000
      end
    end
  end
  object SynXMLSyn1: TSynXMLSyn
    ElementAttri.Foreground = 8355647
    ElementAttri.Style = []
    AttributeAttri.Foreground = 8323199
    AttributeValueAttri.Foreground = 16711722
    AttributeValueAttri.Style = []
    NamespaceAttributeValueAttri.Style = []
    TextAttri.Style = []
    EntityRefAttri.Style = []
    CommentAttri.Style = [fsItalic]
    SymbolAttri.Foreground = 8355647
    WantBracesParsed = False
    Left = 16
    Top = 16
  end
  object JvModernTabBarPainter1: TJvModernTabBarPainter
    TabColor = clWindow
    Color = clBtnFace
    BorderColor = clBtnFace
    ControlDivideColor = clBtnShadow
    CloseColor = clWindow
    CloseCrossColorSelected = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    DisabledFont.Charset = DEFAULT_CHARSET
    DisabledFont.Color = clGrayText
    DisabledFont.Height = -11
    DisabledFont.Name = 'Tahoma'
    DisabledFont.Style = []
    SelectedFont.Charset = DEFAULT_CHARSET
    SelectedFont.Color = clWindowText
    SelectedFont.Height = -11
    SelectedFont.Name = 'Tahoma'
    SelectedFont.Style = []
    Left = 48
    Top = 16
  end
  object ppmnuFilterOptions: TPopupMenu
    Alignment = paRight
    Left = 80
    Top = 16
    object N1231: TMenuItem
      Tag = 1
      Caption = #1055#1086' '#1101#1083#1077#1084#1077#1085#1090#1072#1084'/'#1072#1090#1088#1080#1073#1091#1090#1072#1084
      Checked = True
      GroupIndex = 1
      RadioItem = True
      OnClick = FilterOptionsClick
    end
    object N2341: TMenuItem
      Tag = 2
      Caption = #1055#1086' '#1079#1085#1072#1095#1077#1085#1080#1103#1084
      GroupIndex = 1
      RadioItem = True
      OnClick = FilterOptionsClick
    end
    object N3451: TMenuItem
      Tag = 3
      Caption = #1055#1086' '#1101#1083#1077#1084#1077#1085#1090#1072#1084'/'#1072#1090#1088#1080#1073#1091#1090#1072#1084'/'#1079#1085#1072#1095#1077#1085#1080#1103#1084
      GroupIndex = 1
      RadioItem = True
      OnClick = FilterOptionsClick
    end
    object N5671: TMenuItem
      Tag = 4
      Caption = 'XPath-'#1074#1099#1088#1072#1078#1077#1085#1080#1077
      Enabled = False
      GroupIndex = 1
      RadioItem = True
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 112
    Top = 16
    LangData = {
      0800584D4C4672616D6500011100000009004A76546162426172310102000000
      010000000F00546162735B305D2E43617074696F6E020000000F00546162735B
      315D2E43617074696F6E000B004A76506167654C6973743100000E006A767374
      647067584D4C54726565010100000003000000070043617074696F6E000A0047
      72696450616E656C31000006004C6162656C3101010000000400000007004361
      7074696F6E00090065647446696C746572000007007673744461746100000E00
      4A765370656564427574746F6E3100000E006A767374647067584D4C54657874
      010100000005000000070043617074696F6E000D0073796E656474584D4C5465
      787400000A0053796E584D4C53796E31000016004A764D6F6465726E54616242
      61725061696E746572310000120070706D6E7546696C7465724F7074696F6E73
      000005004E31323331010100000006000000070043617074696F6E0005004E32
      333431010100000007000000070043617074696F6E0005004E33343531010100
      000008000000070043617074696F6E0005004E35363731010100000009000000
      070043617074696F6E00}
  end
end
