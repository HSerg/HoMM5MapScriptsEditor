object H5MFrame: TH5MFrame
  Left = 0
  Top = 0
  Width = 292
  Height = 451
  TabOrder = 0
  TabStop = True
  object vstH5MContent: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 292
    Height = 451
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Images = CommonDM.imglstFileIcons
    IncrementalSearch = isAll
    IncrementalSearchTimeout = 2000
    Indent = 14
    TabOrder = 0
    OnChange = vstH5MContentChange
    OnCompareNodes = vstH5MContentCompareNodes
    OnDblClick = vstH5MContentDblClick
    OnFreeNode = vstH5MContentFreeNode
    OnGetText = vstH5MContentGetText
    OnPaintText = vstH5MContentPaintText
    OnGetImageIndexEx = vstH5MContentGetImageIndexEx
    OnIncrementalSearch = vstH5MContentIncrementalSearch
    OnKeyPress = vstH5MContentKeyPress
    Columns = <>
  end
  object DKLanguageController1: TDKLanguageController
    Left = 16
    Top = 16
    LangData = {
      080048354D4672616D650001010000000D0076737448354D436F6E74656E7400
      00}
  end
end
