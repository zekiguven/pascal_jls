program sample_fmx;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_fmx in 'Unit_fmx.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
