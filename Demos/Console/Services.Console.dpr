program Services.Console;

uses
  Vcl.Forms,
  Unit23 in 'Unit23.pas' {Form23};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm23, Form23);
  Application.Run;
end.
