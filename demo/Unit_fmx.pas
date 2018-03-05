unit Unit_fmx;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation, common;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    bOpen: TButton;
    bSave: TButton;
    ScrollBox1: TScrollBox;
    ImageControl1: TImageControl;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure bOpenClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.bOpenClick(Sender: TObject);
var
  ABitmap:TBitmap;
begin
  if OpenDialog1.Execute() then
  begin
    ABitmap:=DecodeFileToBitmap(OpenDialog1.FileName);
    ImageControl1.Bitmap.Assign(ABitmap);
    ABitmap.Free;
  end;
end;
end.
