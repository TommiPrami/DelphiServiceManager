object SCConsoleForm: TSCConsoleForm
  Left = 0
  Top = 0
  Margins.Left = 6
  Margins.Top = 6
  Margins.Right = 6
  Margins.Bottom = 6
  Caption = 'Delphi Windows Service Manager Console'
  ClientHeight = 889
  ClientWidth = 1254
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -24
  Font.Name = 'Segoe UI'
  Font.Style = []
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 192
  TextHeight = 32
  object ServicesGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 1254
    Height = 889
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    DefaultColWidth = 128
    DefaultRowHeight = 48
    DoubleBuffered = False
    ParentDoubleBuffered = False
    TabOrder = 0
    RowHeights = (
      48
      48
      48
      48
      48)
  end
end
