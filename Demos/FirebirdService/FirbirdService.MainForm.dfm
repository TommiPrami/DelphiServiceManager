object FormFirebierdServiceMain: TFormFirebierdServiceMain
  Left = 0
  Top = 0
  Caption = 'FormFirebierdServiceMain'
  ClientHeight = 411
  ClientWidth = 918
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object MemoLog: TMemo
    Left = 0
    Top = 0
    Width = 777
    Height = 411
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object PanelRight: TPanel
    Left = 777
    Top = 0
    Width = 141
    Height = 411
    Align = alRight
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    object ButtonQueryFirebird: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 135
      Height = 25
      Align = alTop
      Caption = 'Query Firebird'
      TabOrder = 0
      OnClick = ButtonQueryFirebirdClick
    end
    object ButtonEnumerateServices: TButton
      AlignWithMargins = True
      Left = 3
      Top = 34
      Width = 135
      Height = 25
      Align = alTop
      Caption = 'Enumerate services'
      TabOrder = 1
      OnClick = ButtonEnumerateServicesClick
    end
    object ButtonStartFirebirdService: TButton
      AlignWithMargins = True
      Left = 3
      Top = 65
      Width = 135
      Height = 25
      Align = alTop
      Caption = 'Start Firebird service'
      TabOrder = 2
      OnClick = ButtonStartFirebirdServiceClick
    end
    object ButtonStopFirebirdService: TButton
      AlignWithMargins = True
      Left = 3
      Top = 96
      Width = 135
      Height = 25
      Align = alTop
      Caption = 'Stop Firebird service'
      TabOrder = 3
      OnClick = ButtonStopFirebirdServiceClick
    end
    object ButtonFixFirebirdService: TButton
      AlignWithMargins = True
      Left = 3
      Top = 127
      Width = 135
      Height = 25
      Align = alTop
      Caption = 'Fix Firebird service'
      TabOrder = 4
      OnClick = ButtonFixFirebirdServiceClick
    end
    object ButtonTestErrorHandler: TButton
      AlignWithMargins = True
      Left = 3
      Top = 158
      Width = 135
      Height = 25
      Align = alTop
      Caption = 'Test error handler'
      TabOrder = 5
      OnClick = ButtonTestErrorHandlerClick
    end
  end
end
