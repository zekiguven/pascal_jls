program sample_vcl;

uses
  Vcl.Forms,
  Unit_vcl in 'Unit_vcl.pas' {Form1},
  JLSHelper in '..\source\JLSHelper.pas',
  JLSCodec in '..\source\JLSCodec.pas',
  Unitopt_vcl in 'Unitopt_vcl.pas' {frmOptions};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
