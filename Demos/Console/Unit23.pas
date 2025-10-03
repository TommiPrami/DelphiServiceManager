unit Unit23;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids;

type
  TForm23 = class(TForm)
    ServicesGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form23: TForm23;

implementation

{$R *.dfm}

uses
  Windows.ServiceManager;

// https://stackoverflow.com/questions/7884248/how-do-i-make-a-stringgrids-columns-fit-the-grids-width
procedure AutoSizeGridColumns(Grid: TStringGrid);
const
  MIN_COL_WIDTH = 15;
var
  Col : Integer;
  ColWidth, CellWidth: Integer;
  Row: Integer;
begin
  Grid.Canvas.Font.Assign(Grid.Font);
  for Col := 0 to Grid.ColCount -1 do
  begin
    ColWidth := Grid.Canvas.TextWidth(Grid.Cells[Col, 0]);
    for Row := 0 to Grid.RowCount - 1 do
    begin
      CellWidth := Grid.Canvas.TextWidth(Grid.Cells[Col, Row]);
      if CellWidth > ColWidth then
        ColWidth := CellWidth
    end;
    Grid.ColWidths[Col] := ColWidth + MIN_COL_WIDTH;
  end;
end;

procedure TForm23.FormCreate(Sender: TObject);
var
  I: NativeUInt;
  vServiceManager: TServiceManager;
begin
  vServiceManager := TServiceManager.Create;
  try
    vServiceManager.Open;

    ServicesGrid.RowCount := vServiceManager.ServiceCount;

    for I := 0 to vServiceManager.ServiceCount -1 do
    with ServicesGrid do
    begin
      Cells[0, I] := vServiceManager.Services[I].Info.Name;
      Cells[1, I] := vServiceManager.Services[I].Info.Description;
      Cells[2, I] := ServiceStateToString(vServiceManager.Services[I].Info.State); // vServiceManager.Services[I].State
      Cells[3, I] := ServiceStartupToString(vServiceManager.Services[I].Info);
    end;
  finally
    FreeAndNil(vServiceManager);
  end;
end;

procedure TForm23.FormShow(Sender: TObject);
begin
  AutoSizeGridColumns(ServicesGrid);
  ServicesGrid.ColWidths[1] := 1000;
end;

end.
