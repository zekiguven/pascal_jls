unit unit_fpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls,common, bsJLSGlobal,bsJLSDecoder;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnLoad: TButton;
    btnSave: TButton;
    Img1: TImage;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    ScrollBox1: TScrollBox;
    procedure btnLoadClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure DecodeFile(FileName:String);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute() then
  begin
    DecodeFile(OpenDialog1.FileName);
  end;
end;

procedure TForm1.DecodeFile(FileName: String);
var
  LInput,LOutput:TMemorystream;
  info:TbsJlsParameters;
  Abitmap:TBitmap;
begin
  LInput:=TMemorystream.Create;
  LOutput:=TMemorystream.Create;
  fillchar(info, SizeOf(info), 0);

  try
    LInput.LoadFromFile(Filename);

    if jpegls_decompress(LInput, LOutput, @info) then
    begin
      ABitmap:=RawToBitmap(LOutput,info);
      img1.Picture.Assign(ABitmap);
      ABitmap.Free;
    end;
  finally
    LInput.Free;
    LOutput.Free;
  end;

end;
end.

