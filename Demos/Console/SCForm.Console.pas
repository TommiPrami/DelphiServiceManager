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
    procedure ServicesGridDrawCell(Sender: TObject; ACol, ARow: LongInt; Rect: TRect; State: TGridDrawState);
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
  LCol: Integer;
  LColWidth, LCellWidth: Integer;
  LRow: Integer;
begin
  Grid.Canvas.Font.Assign(Grid.Font);

  for LCol := 0 to Grid.ColCount -1 do
  begin
    LColWidth := Grid.Canvas.TextWidth(Grid.Cells[LCol, 0]);

    for LRow := 0 to Grid.RowCount - 1 do
    begin
      LCellWidth := Grid.Canvas.TextWidth(Grid.Cells[LCol, LRow]);

      if LCellWidth > LColWidth then
        LColWidth := LCellWidth;
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

  procedure SetGridCellValues(const ARow: Integer; const ANameColumn, ADescriptionColumn, AStateColumn, AStartTypeColumn: string);
  begin
    ServicesGrid.Cells[0, ARow] := ANameColumn;
    ServicesGrid.Cells[1, ARow] := ADescriptionColumn;
    ServicesGrid.Cells[2, ARow] := AStateColumn;
    ServicesGrid.Cells[3, ARow] := AStartTypeColumn;
  end;
var
  LRow: Integer;
  LServiceManager: TDSMServiceManager;
  LCurrentService: TDSMService;
begin
  LServiceManager := TDSMServiceManager.Create;
  try
    LServiceManager.Open;

    ServicesGrid.RowCount := LServiceManager.ServiceCount + 1;

    SetGridCellValues(0, 'Name', 'Description', 'State', 'Start type');

    for LRow := 0 to LServiceManager.ServiceCount -1 do
    begin
      LCurrentService := LServiceManager.Services[LRow];

      SetGridCellValues(LRow + 1, LCurrentService.Info.Name, LCurrentService.Info.Description, LCurrentService.Info.State.ToString,
        LCurrentService.Info.StartType.ToString);
    end;
  finally
    FreeAndNil(LServiceManager);
  end;
end;

procedure TSCConsoleForm.ServicesGridDrawCell(Sender: TObject; ACol, ARow: LongInt; Rect: TRect; State: TGridDrawState);
begin
  // if ARow = 0 then
    // State := [gdFixed];
end;

end.
