unit Unit_vcl;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs
  ,Jpeg,PngImage, JlSCodec, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    scrlbx1: TScrollBox;
    img1: TImage;
    dlgOpen1: TOpenDialog;
    dlgSave1: TSaveDialog;
    pnl1: TPanel;
    btnOpen: TButton;
    btnSave: TButton;
    btnOptions: TButton;
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    info:TJlsParameters;
    procedure InitParameters;
  end;

var
  Form1: TForm1;

implementation
uses
  JLSHelper, Unitopt_vcl;

{$R *.dfm}



procedure TForm1.btnOpenClick(Sender: TObject);
var
  ABitmap:TBitmap;
begin
  if dlgOpen1.Execute() then
  begin
    ABitmap:=DecodeFileToBitmap(dlgOpen1.FileName,@info);
    img1.Picture.Assign(ABitmap);
    ABitmap.Free;
  end;
end;

procedure TForm1.btnOptionsClick(Sender: TObject);
begin
  ShowOptionsForm(Info);
end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  Bitmap:TBitmap;
begin

  if dlgSave1.Execute() then
  begin
    Bitmap:=TBitmap.Create;
    Bitmap.HandleType := bmDIB;
    Bitmap.Assign(img1.Picture.Graphic);
    EncodeBitmapToFile(dlgSave1.FileName, Bitmap, @info);
    Bitmap.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 InitParameters;
end;

procedure TForm1.InitParameters;
begin
  FillChar(info,SizeOf(Info),0);
  info.Custom.T1:=3;
  info.Custom.T2:=7;
  info.Custom.T3:=21;
  info.Custom.RESET:=64;
  info.AllowedLossyError:=0;
end;

end.
