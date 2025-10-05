unit SCForm.Console;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids;

type
  TSCConsoleForm = class(TForm)
    ServicesGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure PopulateTheGrid;
  end;

var
  SCConsoleForm: TSCConsoleForm;

implementation

{$R *.dfm}

uses
  Windows.ServiceManager;

procedure AutoSizeGridColumns(Grid: TStringGrid);
const
  MIN_COL_WIDTH = 15;
var
  LCol: NativeUInt;
  LColWidth, LCellWidth: NativeUInt;
  LRow: NativeUInt;
begin
  Grid.Canvas.Font.Assign(Grid.Font);

  for LCol := 0 to Grid.ColCount -1 do
  begin
    LColWidth := Grid.Canvas.TextWidth(Grid.Cells[LCol, 0]);

    for LRow := 0 to Grid.RowCount - 1 do
    begin
      LCellWidth := Grid.Canvas.TextWidth(Grid.Cells[LCol, LRow]);

      if LCellWidth > LColWidth then
        LColWidth := LCellWidth
    end;

    Grid.ColWidths[LCol] := LColWidth + MIN_COL_WIDTH;
  end;
end;

procedure TSCConsoleForm.FormCreate(Sender: TObject);
begin
  Cursor := crHourGlass;
  try
    PopulateTheGrid;
  finally
    Cursor := crDefault;
  end;
end;

procedure TSCConsoleForm.FormResize(Sender: TObject);
begin
  ServicesGrid.ColWidths[1] := Width - (ServicesGrid.ColWidths[0] + ServicesGrid.ColWidths[2] + ServicesGrid.ColWidths[3]) - 64;
end;

procedure TSCConsoleForm.FormShow(Sender: TObject);
begin
  AutoSizeGridColumns(ServicesGrid);
end;

procedure TSCConsoleForm.PopulateTheGrid;
var
  LRow: NativeUInt;
  LServiceManager: TDSMServiceManager;
begin
  LServiceManager := TDSMServiceManager.Create;
  try
    LServiceManager.Open;

    ServicesGrid.RowCount := LServiceManager.ServiceCount;

    for LRow := 0 to LServiceManager.ServiceCount -1 do
    with ServicesGrid do
    begin
      Cells[0, LRow] := LServiceManager.Services[LRow].Info.Name;
      Cells[1, LRow] := LServiceManager.Services[LRow].Info.Description;
      Cells[2, LRow] := LServiceManager.Services[LRow].Info.State.ToString;
      Cells[3, LRow] := LServiceManager.Services[LRow].Info.StartType.ToString;
    end;
  finally
    FreeAndNil(LServiceManager);
  end;
end;

end.
