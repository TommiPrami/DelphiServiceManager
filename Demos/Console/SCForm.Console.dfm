object SCConsoleForm: TSCConsoleForm
  Left = 0
  Top = 0
  Caption = 'Delphi Windows Service Manager Console'
  ClientHeight = 584
  ClientWidth = 1151
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object ServicesGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 1151
    Height = 584
    Align = alClient
    DoubleBuffered = False
    ParentDoubleBuffered = False
    TabOrder = 0
    RowHeights = (
      24
      24
      24
      24
      24)
  end
end
