object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Heroes of Might and Magic V Map Scripts Editor'
  ClientHeight = 613
  ClientWidth = 683
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlClientArea: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 25
    Width = 677
    Height = 566
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 185
      Top = 0
      Width = 7
      Height = 566
      AutoSnap = False
      ExplicitLeft = 232
      ExplicitTop = 1
      ExplicitHeight = 612
    end
    object jvpgctrlEditors: TJvPageControl
      Left = 192
      Top = 0
      Width = 485
      Height = 566
      Align = alClient
      Images = CommonDM.imglstFileIcons
      MultiLine = True
      PopupMenu = ppmnuPages
      TabOrder = 0
      OnChange = jvpgctrlEditorsChange
      OnContextPopup = jvpgctrlEditorsContextPopup
      RightClickSelect = True
    end
    object pnlFilesTree: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 566
      Align = alLeft
      TabOrder = 1
      inline H5MFrame1: TH5MFrame
        Left = 1
        Top = 23
        Width = 183
        Height = 542
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 1
        ExplicitTop = 23
        ExplicitWidth = 183
        ExplicitHeight = 542
        inherited vstH5MContent: TVirtualStringTree
          Width = 183
          Height = 542
          OnIncrementalSearch = H5MFrame1vstH5MContentIncrementalSearch
          OnStateChange = H5MFrame1vstH5MContentStateChange
          ExplicitWidth = 183
          ExplicitHeight = 542
        end
        inherited DKLanguageController1: TDKLanguageController
          IgnoreList.Strings = (
            'lblFileName.Caption')
          LangData = {
            090048354D4672616D65310001010000000D0076737448354D436F6E74656E74
            0000}
        end
      end
      object pnlToolbar: TPanel
        Left = 1
        Top = 1
        Width = 183
        Height = 22
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object tlbrFileOperations: TToolBar
          Left = 0
          Top = 0
          Width = 131
          Height = 22
          Align = alLeft
          AutoSize = True
          EdgeInner = esNone
          EdgeOuter = esNone
          Images = CommonDM.imlActions
          TabOrder = 0
          object ToolButton1: TToolButton
            Left = 0
            Top = 0
            Action = acCreateFolder
          end
          object ToolButton2: TToolButton
            Left = 23
            Top = 0
            Action = acCreateTXTFile
          end
          object ToolButton3: TToolButton
            Left = 46
            Top = 0
            Action = acCreateLUAFile
          end
          object ToolButton4: TToolButton
            Left = 69
            Top = 0
            Width = 8
            Caption = 'ToolButton4'
            ImageIndex = 3
            Style = tbsSeparator
          end
          object ToolButton5: TToolButton
            Left = 77
            Top = 0
            Action = acDeleteFolderOrFile
          end
          object ToolButton7: TToolButton
            Left = 100
            Top = 0
            Width = 8
            Caption = 'ToolButton7'
            ImageIndex = 63
            Style = tbsSeparator
          end
          object ToolButton8: TToolButton
            Left = 108
            Top = 0
            Action = acRefresh
          end
        end
        object tlbrFilters: TToolBar
          Left = 160
          Top = 0
          Width = 23
          Height = 22
          Align = alRight
          AutoSize = True
          EdgeInner = esNone
          EdgeOuter = esNone
          Images = CommonDM.imlActions
          TabOrder = 1
          object ToolButton6: TToolButton
            Left = 0
            Top = 0
            Action = acFilterFiles
            DropdownMenu = ppmnuFilesFilter
          end
        end
      end
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 683
    Height = 22
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 2
    Padding.Right = 8
    TabOrder = 1
    object lblFileName: TLabel
      Left = 8
      Top = 2
      Width = 667
      Height = 20
      Align = alClient
      AutoSize = False
      Caption = #1060#1072#1081#1083' '#1082#1072#1088#1090#1099':'
      EllipsisPosition = epPathEllipsis
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = 3
      ExplicitTop = -2
      ExplicitWidth = 675
      ExplicitHeight = 25
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 594
    Width = 683
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object actListMain: TActionList
    Images = CommonDM.imlActions
    OnUpdate = actListMainUpdate
    Left = 384
    Top = 9
    object acCreateFolder: TAction
      Category = 'Files Operations'
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1087#1072#1087#1082#1091
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1087#1072#1087#1082#1091
      ImageIndex = 69
      OnExecute = acCreateFolderExecute
    end
    object acExit: TAction
      Category = 'Application Related'
      Caption = #1042#1099#1093#1086#1076
      Hint = #1042#1099#1093#1086#1076
      ImageIndex = 1
      OnExecute = acExitExecute
    end
    object acFindInFiles: TAction
      Caption = #1055#1086#1080#1089#1082' '#1074' '#1092#1072#1081#1083#1072#1093' '#1082#1072#1088#1090#1099'...'
      Hint = #1055#1086#1080#1089#1082' '#1074' '#1092#1072#1081#1083#1072#1093' '#1082#1072#1088#1090#1099'...'
      ImageIndex = 19
      ShortCut = 24646
      OnExecute = acFindInFilesExecute
    end
    object acPrint: TAction
      Category = 'Application Related'
      Caption = 'Print...'
      Hint = 'Print...'
      ImageIndex = 4
    end
    object acUndo: TAction
      Category = 'Code Manipulation'
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100' '#1074#1074#1086#1076
      Hint = #1054#1090#1084#1077#1085#1080#1090#1100' '#1074#1074#1086#1076
      ImageIndex = 14
      ShortCut = 16474
      OnExecute = acUndoExecute
    end
    object acRedo: TAction
      Category = 'Code Manipulation'
      Caption = #1042#1077#1088#1085#1091#1090#1100' '#1074#1074#1086#1076
      Hint = #1042#1077#1088#1085#1091#1090#1100' '#1074#1074#1086#1076
      ImageIndex = 13
      ShortCut = 24666
      OnExecute = acRedoExecute
    end
    object acSelectAll: TAction
      Category = 'Code Manipulation'
      Caption = 'Select All'
      Hint = 'Select All'
      ShortCut = 16449
      OnExecute = acSelectAllExecute
    end
    object acCut: TAction
      Category = 'Code Manipulation'
      Caption = #1042#1099#1088#1077#1079#1072#1090#1100
      Hint = #1042#1099#1088#1077#1079#1072#1090#1100
      ImageIndex = 16
      ShortCut = 16472
      OnExecute = acCutExecute
    end
    object acCopy: TAction
      Category = 'Code Manipulation'
      Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
      Hint = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
      ImageIndex = 15
      ShortCut = 16451
      OnExecute = acCopyExecute
    end
    object acPaste: TAction
      Category = 'Code Manipulation'
      Caption = #1042#1089#1090#1072#1074#1080#1090#1100
      Hint = #1042#1089#1090#1072#1074#1080#1090#1100
      ImageIndex = 12
      ShortCut = 16470
      OnExecute = acPasteExecute
    end
    object acFind: TAction
      Category = 'Code Manipulation'
      Caption = #1053#1072#1081#1090#1080'...'
      Hint = #1053#1072#1081#1090#1080'...'
      ImageIndex = 17
      ShortCut = 16454
      OnExecute = acFindExecute
    end
    object acFindAgain: TAction
      Category = 'Code Manipulation'
      Caption = #1053#1072#1081#1090#1080' '#1076#1072#1083#1077#1077
      Hint = #1053#1072#1081#1090#1080' '#1076#1072#1083#1077#1077
      ImageIndex = 18
      ShortCut = 114
      OnExecute = acFindAgainExecute
    end
    object acFindReplace: TAction
      Category = 'Code Manipulation'
      Caption = #1047#1072#1084#1077#1085#1080#1090#1100'...'
      Hint = #1047#1072#1084#1077#1085#1080#1090#1100'...'
      ImageIndex = 22
      ShortCut = 16466
      OnExecute = acFindReplaceExecute
    end
    object acGoToLine: TAction
      Category = 'Code Manipulation'
      Caption = #1055#1077#1088#1077#1081#1090#1080' '#1082' '#1089#1090#1088#1086#1082#1077'...'
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1082' '#1089#1090#1088#1086#1082#1077'...'
      ImageIndex = 20
      ShortCut = 16455
      OnExecute = acGoToLineExecute
    end
    object acEditorSettings: TAction
      Category = 'Application Related'
      Caption = 'Editor Settings...'
      Hint = 'Editor Settings...'
      ImageIndex = 41
    end
    object acBlockUnindent: TAction
      Category = 'Code Manipulation'
      Caption = #1059#1084#1077#1085#1100#1096#1080#1090#1100' '#1086#1090#1089#1090#1091#1087
      Hint = #1059#1084#1077#1085#1100#1096#1080#1090#1100' '#1086#1090#1089#1090#1091#1087
      ImageIndex = 39
      ShortCut = 24661
      OnExecute = acBlockUnindentExecute
    end
    object acBlockIndent: TAction
      Category = 'Code Manipulation'
      Caption = #1059#1074#1077#1083#1080#1095#1080#1090#1100' '#1086#1090#1089#1090#1091#1087
      Hint = #1059#1074#1077#1083#1080#1095#1080#1090#1100' '#1086#1090#1089#1090#1091#1087
      ImageIndex = 40
      ShortCut = 24649
      OnExecute = acBlockIndentExecute
    end
    object acBlockComment: TAction
      Category = 'Code Manipulation'
      Caption = #1047#1072#1082#1086#1084#1084#1077#1085#1090#1080#1088#1086#1074#1072#1090#1100' '#1073#1083#1086#1082
      Hint = #1047#1072#1082#1086#1084#1084#1077#1085#1090#1080#1088#1086#1074#1072#1090#1100' '#1073#1083#1086#1082
      ImageIndex = 57
      ShortCut = 24643
      OnExecute = acBlockCommentExecute
    end
    object acBlockUncomment: TAction
      Category = 'Code Manipulation'
      Caption = #1056#1072#1089#1082#1086#1084#1084#1077#1085#1090#1080#1088#1086#1074#1072#1090#1100' '#1073#1083#1086#1082
      Hint = #1056#1072#1089#1082#1086#1084#1084#1077#1085#1090#1080#1088#1086#1074#1072#1090#1100' '#1073#1083#1086#1082
      ImageIndex = 56
      ShortCut = 24662
      OnExecute = acBlockUncommentExecute
    end
    object acUpperCase: TAction
      Category = 'Code Manipulation'
      Caption = 'Upper Case Selection'
      Hint = 'Upper Case Selection'
      ShortCut = 16469
    end
    object acLowerCase: TAction
      Category = 'Code Manipulation'
      Caption = 'Lower Case Selection'
      Hint = 'Lower Case Selection'
      ShortCut = 16460
    end
    object acClosePage: TAction
      Category = 'Pages'
      Caption = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
      Hint = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
      ShortCut = 16471
      OnExecute = acClosePageExecute
    end
    object acOpenH5M: TAction
      Caption = #1054#1090#1082#1088#1099#1090#1100' '#1082#1072#1088#1090#1091' (*.h5m)'
      Hint = #1054#1090#1082#1088#1099#1090#1100' '#1082#1072#1088#1090#1091' (*.h5m)'
      ImageIndex = 2
      ShortCut = 16463
      OnExecute = acOpenH5MExecute
    end
    object acCreateTXTFile: TAction
      Category = 'Files Operations'
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1099#1081' '#1090#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1099#1081' '#1090#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083
      ImageIndex = 71
      OnExecute = acCreateTXTFileExecute
    end
    object acCloseOtherPages: TAction
      Category = 'Pages'
      Caption = #1047#1072#1082#1088#1099#1090#1100' '#1074#1089#1077' '#1086#1082#1085#1072', '#1082#1088#1086#1084#1077' '#1090#1077#1082#1091#1097#1077#1075#1086
      Hint = #1047#1072#1082#1088#1099#1090#1100' '#1074#1089#1077' '#1086#1082#1085#1072', '#1082#1088#1086#1084#1077' '#1090#1077#1082#1091#1097#1077#1075#1086
      OnExecute = acCloseOtherPagesExecute
    end
    object acOpenEmbFile: TAction
      Caption = 'acOpenEmbFile'
      ShortCut = 8304
      OnExecute = acOpenEmbFileExecute
    end
    object acCreateLUAFile: TAction
      Category = 'Files Operations'
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1099#1081' lua-'#1092#1072#1081#1083
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1099#1081' lua-'#1092#1072#1081#1083
      ImageIndex = 70
      OnExecute = acCreateLUAFileExecute
    end
    object acDeleteFolderOrFile: TAction
      Category = 'Files Operations'
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1092#1072#1081#1083'/'#1087#1072#1087#1082#1091
      Hint = #1059#1076#1072#1083#1080#1090#1100' '#1092#1072#1081#1083'/'#1087#1072#1087#1082#1091
      ImageIndex = 62
      OnExecute = acDeleteFolderOrFileExecute
    end
    object acNextPage: TAction
      Category = 'Pages'
      Caption = #1057#1083#1077#1076#1091#1102#1097#1077#1077' '#1086#1082#1085#1086
      Hint = #1057#1083#1077#1076#1091#1102#1097#1077#1077' '#1086#1082#1085#1086
      ShortCut = 16393
      OnExecute = acNextPageExecute
    end
    object acPrevPage: TAction
      Category = 'Pages'
      Caption = #1055#1088#1077#1076#1099#1076#1091#1097#1077#1077' '#1086#1082#1085#1086
      Hint = #1055#1088#1077#1076#1099#1076#1091#1097#1077#1077' '#1086#1082#1085#1086
      ShortCut = 24585
      OnExecute = acPrevPageExecute
    end
    object acReOpenH5M: TAction
      Caption = 'acReOpenH5M'
      OnExecute = acReOpenH5MExecute
    end
    object acAbout: TAction
      Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
      Hint = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
      OnExecute = acAboutExecute
    end
    object acCloseH5M: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100' '#1082#1072#1088#1090#1091
      Hint = #1047#1072#1082#1088#1099#1090#1100' '#1082#1072#1088#1090#1091
      OnExecute = acCloseH5MExecute
    end
    object acSettings: TAction
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      Hint = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      OnExecute = acSettingsExecute
    end
    object acCheckGrammar: TAction
      Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1086#1088#1092#1086#1075#1088#1072#1092#1080#1080' '#1074' '#1090#1077#1082#1089#1090#1086#1074#1099#1093' '#1092#1072#1081#1083#1072#1093' '#1082#1072#1088#1090#1099
      Hint = #1055#1088#1086#1074#1077#1088#1082#1072' '#1086#1088#1092#1086#1075#1088#1072#1092#1080#1080' '#1074' '#1090#1077#1082#1089#1090#1086#1074#1099#1093' '#1092#1072#1081#1083#1072#1093' '#1082#1072#1088#1090#1099
    end
    object acOpenHelp: TAction
      Caption = #1057#1087#1088#1072#1074#1082#1072
      Hint = #1057#1087#1088#1072#1074#1082#1072
      ShortCut = 112
    end
    object acFilterFiles: TAction
      Category = 'Files Operations'
      Caption = #1053#1072#1089#1090#1088#1086#1080#1090#1100' '#1092#1080#1083#1100#1090#1088
      Hint = #1053#1072#1089#1090#1088#1086#1080#1090#1100' '#1092#1080#1083#1100#1090#1088
      ImageIndex = 35
      OnExecute = acFilterFilesExecute
    end
    object acOpenExtFile: TAction
      Caption = #1054#1090#1082#1088#1099#1090#1100' '#1074#1085#1077#1096#1085#1080#1081' '#1092#1072#1081#1083
      Hint = #1054#1090#1082#1088#1099#1090#1100' '#1074#1085#1077#1096#1085#1080#1081' '#1092#1072#1081#1083
      OnExecute = acOpenExtFileExecute
    end
    object acRefresh: TAction
      Category = 'Files Operations'
      Caption = 'Refresh'
      Hint = 'Refresh'
      ImageIndex = 75
      OnExecute = acRefreshExecute
    end
    object acSavePageEditorContent: TAction
      Category = 'Code Manipulation'
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      ImageIndex = 5
      ShortCut = 16467
      OnExecute = acSavePageEditorContentExecute
    end
    object acOpenHomePage: TAction
      Caption = #1055#1086#1089#1077#1090#1080#1090#1100' '#1089#1072#1081#1090' '#1087#1088#1086#1075#1088#1072#1084#1084#1099
      Hint = #1055#1086#1089#1077#1090#1080#1090#1100' '#1089#1072#1081#1090' '#1087#1088#1086#1075#1088#1072#1084#1084#1099
      OnExecute = acOpenHomePageExecute
    end
  end
  object ppmnuEditor: TPopupMenu
    Images = CommonDM.imlActions
    Left = 320
    Top = 9
    object Undo2: TMenuItem
      Action = acUndo
    end
    object Redo2: TMenuItem
      Action = acRedo
    end
    object N17: TMenuItem
      Caption = '-'
    end
    object Cut2: TMenuItem
      Action = acCut
    end
    object Copy2: TMenuItem
      Action = acCopy
    end
    object Paste2: TMenuItem
      Action = acPaste
    end
    object N19: TMenuItem
      Caption = '-'
    end
    object IndentSelection2: TMenuItem
      Action = acBlockIndent
    end
    object UnindentSelection2: TMenuItem
      Action = acBlockUnindent
    end
    object CommentSelection2: TMenuItem
      Action = acBlockComment
    end
    object UncommentSelection2: TMenuItem
      Action = acBlockUncomment
    end
    object N16: TMenuItem
      Caption = '-'
    end
    object oggleBookmark1: TMenuItem
      Caption = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100'/'#1091#1073#1088#1072#1090#1100' '#1079#1072#1082#1083#1072#1076#1082#1091
      object GotoBookmark12: TMenuItem
        Tag = 1
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 1'
        GroupIndex = 1
        ShortCut = 24625
        OnClick = ToggleBookmarkClick
      end
      object GotoBookmark13: TMenuItem
        Tag = 2
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 2'
        GroupIndex = 1
        ShortCut = 24626
        OnClick = ToggleBookmarkClick
      end
      object GotoBookmark14: TMenuItem
        Tag = 3
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 3'
        GroupIndex = 1
        ShortCut = 24627
        OnClick = ToggleBookmarkClick
      end
      object GotoBookmark15: TMenuItem
        Tag = 4
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 4'
        GroupIndex = 1
        ShortCut = 24628
        OnClick = ToggleBookmarkClick
      end
      object GotoBookmark16: TMenuItem
        Tag = 5
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 5'
        GroupIndex = 1
        ShortCut = 24629
        OnClick = ToggleBookmarkClick
      end
      object GotoBookmark17: TMenuItem
        Tag = 6
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 6'
        GroupIndex = 1
        ShortCut = 24630
        OnClick = ToggleBookmarkClick
      end
      object GotoBookmark18: TMenuItem
        Tag = 7
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 7'
        GroupIndex = 1
        ShortCut = 24631
        OnClick = ToggleBookmarkClick
      end
      object GotoBookmark81: TMenuItem
        Tag = 8
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 8'
        GroupIndex = 1
        ShortCut = 24632
        OnClick = ToggleBookmarkClick
      end
      object GotoBookmark91: TMenuItem
        Tag = 9
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 9'
        GroupIndex = 1
        ShortCut = 24633
        OnClick = ToggleBookmarkClick
      end
      object GotoBookmark11: TMenuItem
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 0'
        GroupIndex = 1
        ShortCut = 24624
        OnClick = ToggleBookmarkClick
      end
    end
    object GotoBookmark1: TMenuItem
      Caption = #1055#1077#1088#1077#1081#1090#1080' '#1082' '#1079#1072#1082#1083#1072#1076#1082#1077
      object Bookmark11: TMenuItem
        Tag = 1
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 1'
        GroupIndex = 1
        ShortCut = 16433
        OnClick = GotoBookmarkClick
      end
      object Bookmark12: TMenuItem
        Tag = 2
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 2'
        GroupIndex = 1
        ShortCut = 16434
        OnClick = GotoBookmarkClick
      end
      object Bookmark13: TMenuItem
        Tag = 3
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 3'
        GroupIndex = 1
        ShortCut = 16435
        OnClick = GotoBookmarkClick
      end
      object Bookmark14: TMenuItem
        Tag = 4
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 4'
        GroupIndex = 1
        ShortCut = 16436
        OnClick = GotoBookmarkClick
      end
      object Bookmark15: TMenuItem
        Tag = 5
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 5'
        GroupIndex = 1
        ShortCut = 16437
        OnClick = GotoBookmarkClick
      end
      object Bookmark16: TMenuItem
        Tag = 6
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 6'
        GroupIndex = 1
        ShortCut = 16438
        OnClick = GotoBookmarkClick
      end
      object Bookmark17: TMenuItem
        Tag = 7
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 7'
        GroupIndex = 1
        ShortCut = 16439
        OnClick = GotoBookmarkClick
      end
      object Bookmark18: TMenuItem
        Tag = 8
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 8'
        GroupIndex = 1
        ShortCut = 16440
        OnClick = GotoBookmarkClick
      end
      object Bookmark19: TMenuItem
        Tag = 9
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 9'
        GroupIndex = 1
        ShortCut = 16441
        OnClick = GotoBookmarkClick
      end
      object Bookmark110: TMenuItem
        Caption = #1047#1072#1082#1083#1072#1076#1082#1072' 0'
        GroupIndex = 1
        ShortCut = 16432
        OnClick = GotoBookmarkClick
      end
    end
    object N24: TMenuItem
      Caption = '-'
    end
    object GotoLine2: TMenuItem
      Action = acGoToLine
    end
    object N27: TMenuItem
      Caption = '-'
    end
    object N28: TMenuItem
      Action = acClosePage
    end
  end
  object MainMenu1: TMainMenu
    Images = CommonDM.imlActions
    Left = 264
    Top = 8
    object miFiles: TMenuItem
      Caption = #1060#1072#1081#1083
      object h5m1: TMenuItem
        Action = acOpenH5M
      end
      object N36: TMenuItem
        Action = acCloseH5M
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object lualua1: TMenuItem
        Action = acOpenExtFile
      end
      object N44: TMenuItem
        Caption = '-'
      end
      object N26: TMenuItem
        Action = acCreateTXTFile
      end
      object acCreateLUAFile1: TMenuItem
        Action = acCreateLUAFile
      end
      object N30: TMenuItem
        Caption = '-'
      end
      object acDeleteFile1: TMenuItem
        Action = acDeleteFolderOrFile
      end
      object miMRUStart: TMenuItem
        Caption = '-'
      end
      object miMRUEnd: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = acExit
      end
    end
    object Ghfdrf1: TMenuItem
      Caption = #1055#1088#1072#1074#1082#1072
      object N3: TMenuItem
        Action = acUndo
      end
      object N4: TMenuItem
        Action = acRedo
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object N8: TMenuItem
        Action = acCut
      end
      object N9: TMenuItem
        Action = acCopy
      end
      object N10: TMenuItem
        Action = acPaste
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object N12: TMenuItem
        Action = acBlockIndent
      end
      object N13: TMenuItem
        Action = acBlockUnindent
      end
      object N14: TMenuItem
        Action = acBlockComment
      end
      object N15: TMenuItem
        Action = acBlockUncomment
      end
    end
    object N18: TMenuItem
      Caption = #1055#1086#1080#1089#1082
      object N20: TMenuItem
        Action = acFind
      end
      object Replace1: TMenuItem
        Action = acFindReplace
      end
      object N21: TMenuItem
        Action = acFindAgain
      end
      object N45: TMenuItem
        Caption = '-'
      end
      object FindinFiles1: TMenuItem
        Action = acFindInFiles
      end
      object N23: TMenuItem
        Caption = '-'
      end
      object N22: TMenuItem
        Action = acGoToLine
      end
    end
    object N25: TMenuItem
      Caption = #1048#1085#1089#1090#1088#1091#1084#1077#1085#1090#1099
      object N35: TMenuItem
        Action = acCheckGrammar
      end
      object N34: TMenuItem
        Caption = '-'
      end
      object N38: TMenuItem
        Action = acSettings
      end
    end
    object miWindows: TMenuItem
      Caption = #1054#1082#1085#1086
      OnClick = miWindowsClick
      object N31: TMenuItem
        Caption = '-'
      end
      object N32: TMenuItem
        Action = acNextPage
      end
      object N33: TMenuItem
        Action = acPrevPage
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object acClosePage1: TMenuItem
        Action = acClosePage
      end
      object N29: TMenuItem
        Action = acCloseOtherPages
      end
    end
    object N2: TMenuItem
      Caption = #1057#1087#1088#1072#1074#1082#1072
      object N37: TMenuItem
        Action = acOpenHomePage
      end
      object N39: TMenuItem
        Caption = '-'
      end
      object N1: TMenuItem
        Action = acAbout
      end
    end
  end
  object synSearch: TSynEditSearch
    Left = 232
    Top = 64
  end
  object synSearchRegex: TSynEditRegexSearch
    Left = 262
    Top = 64
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      'bin1.Caption'
      'dds1.Caption'
      'lblFileName.Caption'
      'lua1.Caption'
      'txt1.Caption'
      'xdb1.Caption')
    OnLanguageChanged = DKLanguageController1LanguageChanged
    Left = 472
    Top = 8
    LangData = {
      08004D61696E466F726D010100000001000000070043617074696F6E01A50000
      000D00706E6C436C69656E74417265610000090053706C69747465723100000F
      006A7670676374726C456469746F727300000B006163744C6973744D61696E00
      000600616345786974010300000004000000070043617074696F6E0300000008
      0043617465676F727905000000040048696E74000D00616346696E64496E4669
      6C6573010200000007000000070043617074696F6E08000000040048696E7400
      070061635072696E7401030000000A000000070043617074696F6E0900000008
      0043617465676F72790B000000040048696E740006006163556E646F01030000
      000D000000070043617074696F6E0C000000080043617465676F72790E000000
      040048696E7400060061635265646F010300000010000000070043617074696F
      6E0F000000080043617465676F727911000000040048696E74000B0061635365
      6C656374416C6C010300000013000000070043617074696F6E12000000080043
      617465676F727914000000040048696E74000500616343757401030000001600
      0000070043617074696F6E15000000080043617465676F727917000000040048
      696E740006006163436F7079010300000019000000070043617074696F6E1800
      0000080043617465676F72791A000000040048696E7400070061635061737465
      01030000001C000000070043617074696F6E1B000000080043617465676F7279
      1D000000040048696E74000600616346696E6401030000001F00000007004361
      7074696F6E1E000000080043617465676F727920000000040048696E74000B00
      616346696E64416761696E010300000022000000070043617074696F6E210000
      00080043617465676F727923000000040048696E74000D00616346696E645265
      706C616365010300000025000000070043617074696F6E240000000800436174
      65676F727926000000040048696E74000A006163476F546F4C696E6501030000
      0028000000070043617074696F6E27000000080043617465676F727929000000
      040048696E740010006163456469746F7253657474696E677301030000002B00
      0000070043617074696F6E2A000000080043617465676F72792C000000040048
      696E74000F006163426C6F636B556E696E64656E7401030000002E0000000700
      43617074696F6E2D000000080043617465676F72792F000000040048696E7400
      0D006163426C6F636B496E64656E74010300000031000000070043617074696F
      6E30000000080043617465676F727932000000040048696E74000E006163426C
      6F636B436F6D6D656E74010300000041000000070043617074696F6E40000000
      080043617465676F727942000000040048696E740010006163426C6F636B556E
      636F6D6D656E74010300000044000000070043617074696F6E43000000080043
      617465676F727945000000040048696E74000B00616355707065724361736501
      0300000047000000070043617074696F6E46000000080043617465676F727948
      000000040048696E74000B0061634C6F7765724361736501030000004A000000
      070043617074696F6E49000000080043617465676F72794B000000040048696E
      74000B006163436C6F73655061676501030000004C000000070043617074696F
      6E7D000000080043617465676F7279A7000000040048696E7400090061634F70
      656E48354D01020000004D000000070043617074696F6E89000000040048696E
      74000F00616343726561746554585446696C6501030000004E00000007004361
      7074696F6E6D000000080043617465676F727977000000040048696E74000B00
      70706D6E75456469746F7200000500556E646F32000005005265646F32000003
      004E3137000004004375743200000500436F7079320000060050617374653200
      0003004E313900001000496E64656E7453656C656374696F6E3200001200556E
      696E64656E7453656C656374696F6E3200001100436F6D6D656E7453656C6563
      74696F6E3200001300556E636F6D6D656E7453656C656374696F6E3200000300
      4E313600000E006F67676C65426F6F6B6D61726B3101010000004F0000000700
      43617074696F6E000E00476F746F426F6F6B6D61726B31320101000000500000
      00070043617074696F6E000E00476F746F426F6F6B6D61726B31330101000000
      51000000070043617074696F6E000E00476F746F426F6F6B6D61726B31340101
      00000052000000070043617074696F6E000E00476F746F426F6F6B6D61726B31
      35010100000053000000070043617074696F6E000E00476F746F426F6F6B6D61
      726B3136010100000054000000070043617074696F6E000E00476F746F426F6F
      6B6D61726B3137010100000055000000070043617074696F6E000E00476F746F
      426F6F6B6D61726B3138010100000056000000070043617074696F6E000E0047
      6F746F426F6F6B6D61726B3831010100000057000000070043617074696F6E00
      0E00476F746F426F6F6B6D61726B393101010000005800000007004361707469
      6F6E000E00476F746F426F6F6B6D61726B313101010000005900000007004361
      7074696F6E000D00476F746F426F6F6B6D61726B3101010000005A0000000700
      43617074696F6E000A00426F6F6B6D61726B313101010000005B000000070043
      617074696F6E000A00426F6F6B6D61726B313201010000005C00000007004361
      7074696F6E000A00426F6F6B6D61726B313301010000005D0000000700436170
      74696F6E000A00426F6F6B6D61726B313401010000005E000000070043617074
      696F6E000A00426F6F6B6D61726B313501010000005F00000007004361707469
      6F6E000A00426F6F6B6D61726B3136010100000060000000070043617074696F
      6E000A00426F6F6B6D61726B3137010100000061000000070043617074696F6E
      000A00426F6F6B6D61726B3138010100000062000000070043617074696F6E00
      0A00426F6F6B6D61726B3139010100000063000000070043617074696F6E000B
      00426F6F6B6D61726B313130010100000064000000070043617074696F6E0003
      004E323400000900476F746F4C696E6532000003004E3237000003004E323800
      0009004D61696E4D656E753100010100000003004E3337000007006D6946696C
      6573010100000065000000070043617074696F6E00040068356D31000002004E
      36000003004E323600000A006D694D5255537461727400000500457869743100
      00070047686664726631010100000066000000070043617074696F6E0002004E
      33000002004E34000002004E37000002004E38000002004E39000003004E3130
      000003004E3131000003004E3132000003004E3133000003004E313400000300
      4E3135000003004E3138010100000067000000070043617074696F6E0003004E
      3230000008005265706C61636531000003004E3231000003004E323300000300
      4E3232000009006D6957696E646F777301010000006800000007004361707469
      6F6E0002004E3500000C006163436C6F73655061676531000002004E32010100
      000069000000070043617074696F6E00090073796E53656172636800000E0073
      796E5365617263685265676578000011006163436C6F73654F74686572506167
      657301030000006B000000070043617074696F6E7E000000080043617465676F
      7279A8000000040048696E740003004E323900000D0061634F70656E456D6246
      696C6501010000006C000000070043617074696F6E000F006163437265617465
      4C554146696C6501030000006F000000070043617074696F6E6E000000080043
      617465676F727978000000040048696E74001400616344656C657465466F6C64
      65724F7246696C65010300000071000000070043617074696F6E700000000800
      43617465676F727979000000040048696E7400100061634372656174654C5541
      46696C653100000D00616344656C65746546696C6531000003004E333000000C
      00706E6C46696C6573547265650000090048354D4672616D6531000101000000
      0D0076737448354D436F6E74656E7400000E006163437265617465466F6C6465
      72010300000075000000070043617074696F6E74000000080043617465676F72
      797A000000040048696E74000F006F706E646C6748354D53656C656374010200
      00007B000000060046696C7465727C00000005005469746C65000A0061634E65
      787450616765010300000080000000070043617074696F6E7F00000008004361
      7465676F7279A9000000040048696E74000A0061635072657650616765010300
      000082000000070043617074696F6E81000000080043617465676F7279AA0000
      00040048696E740003004E3333000003004E3332000003004E3331000008006D
      694D5255456E6400000B00616352654F70656E48354D01010000008300000007
      0043617074696F6E000700616341626F75740102000000840000000700436170
      74696F6E85000000040048696E740002004E31000003004E3235010100000086
      000000070043617074696F6E000A006163436C6F736548354D01020000008A00
      0000070043617074696F6E8B000000040048696E740003004E3336000003004E
      333800000A00616353657474696E677301020000008C00000007004361707469
      6F6E8D000000040048696E74000E006163436865636B4772616D6D6172010200
      00008E000000070043617074696F6E8F000000040048696E740003004E333400
      0003004E333500000A0061634F70656E48656C70010200000090000000070043
      617074696F6E91000000040048696E740003004E333900000900706E6C486561
      64657200000B006C626C46696C654E616D6500000A00706E6C546F6F6C626172
      00001200746C627246696C654F7065726174696F6E7300000B00546F6F6C4275
      74746F6E3100000B00546F6F6C427574746F6E3200000B00546F6F6C42757474
      6F6E3300000B00546F6F6C427574746F6E340101000000930000000700436170
      74696F6E000B00546F6F6C427574746F6E350000100070706D6E7546696C6573
      46696C74657200000400747874310000040078646231000004006C7561310000
      0400646473310000040062696E31000003004E343200000B00746C627246696C
      7465727300000D00616346696C74657246696C657301030000009C0000000700
      43617074696F6E9B000000080043617465676F72799D000000040048696E7400
      0B00546F6F6C427574746F6E36000003004E343000000C006D694F7468657246
      696C657301010000009E000000070043617074696F6E000A0070706D6E755061
      676573000003004E3431000003004E343300000D0061634F70656E4578744669
      6C6501020000009F000000070043617074696F6EA0000000040048696E740013
      006F706E646C6745787446696C6553656C6563740102000000A1000000060046
      696C746572A200000005005469746C650003004E3434000007006C75616C7561
      3100000B00546F6F6C427574746F6E370101000000A300000007004361707469
      6F6E000B00546F6F6C427574746F6E3800000900616352656672657368010300
      0000A5000000070043617074696F6EA4000000080043617465676F7279A60000
      00040048696E74000A005374617475734261723100000C0046696E64696E4669
      6C657331000003004E34350000170061635361766550616765456469746F7243
      6F6E74656E740103000000AC000000070043617074696F6EAB00000008004361
      7465676F7279AD000000040048696E74000E0061634F70656E486F6D65506167
      650102000000AE000000070043617074696F6EAF000000040048696E74000300
      4E33370000}
  end
  object opndlgH5MSelect: TOpenDialog
    Filter = 'H5M Files (*.h5m)|*.h5m|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open H5M File'
    Left = 431
    Top = 9
  end
  object ppmnuFilesFilter: TPopupMenu
    AutoHotkeys = maManual
    AutoLineReduction = maManual
    Left = 224
    Top = 8
    object xdb1: TMenuItem
      Caption = '  *.xdb'
      Checked = True
      OnClick = FilterChangeClick
    end
    object txt1: TMenuItem
      Caption = '  *.txt'
      Checked = True
      OnClick = FilterChangeClick
    end
    object lua1: TMenuItem
      Caption = '  *.lua'
      Checked = True
      OnClick = FilterChangeClick
    end
    object N42: TMenuItem
      Caption = '-'
    end
    object dds1: TMenuItem
      Caption = '  *.dds'
      Checked = True
      OnClick = FilterChangeClick
    end
    object bin1: TMenuItem
      Caption = '  *.bin'
      Checked = True
      OnClick = FilterChangeClick
    end
    object N40: TMenuItem
      Caption = '-'
    end
    object miOtherFiles: TMenuItem
      Caption = ' '#1087#1088#1086#1095#1080#1077
      Checked = True
      OnClick = FilterChangeClick
    end
  end
  object ppmnuPages: TPopupMenu
    Images = CommonDM.imlActions
    Left = 320
    Top = 40
    object N41: TMenuItem
      Action = acClosePage
    end
    object N43: TMenuItem
      Action = acCloseOtherPages
    end
  end
  object opndlgExtFileSelect: TOpenDialog
    Filter = 'All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open external File'
    Left = 431
    Top = 41
  end
end
