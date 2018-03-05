unit Unitopt_vcl;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,JlSCodec;

type
  TfrmOptions = class(TForm)
    lblT1: TLabel;
    lblT2: TLabel;
    lblT3: TLabel;
    lblReset: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    edtT3: TEdit;
    edtT2: TEdit;
    edtT1: TEdit;
    chkDefaultT: TCheckBox;
    edtRESET: TEdit;
    chkR: TCheckBox;
    edtComp: TEdit;
    chkComp: TCheckBox;
    cbb1: TComboBox;
    bClose: TButton;
    procedure chkCompClick(Sender: TObject);
    procedure chkDefaultTClick(Sender: TObject);
    procedure chkRClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOptions: TfrmOptions;

procedure ShowOptionsForm(var Info:TJlsParameters);

implementation

{$R *.dfm}
procedure ShowOptionsForm(var Info:TJlsParameters);
begin
  Application.CreateForm(TfrmOptions,frmOptions);

  frmOptions.edtT1.Text:= IntToStr(Info.Custom.T1);
  frmOptions.edtT2.Text:= IntToStr(Info.Custom.T2);
  frmOptions.edtT3.Text:= IntToStr(Info.Custom.T3);

  frmOptions.edtRESET.Text:= IntToStr(Info.Custom.RESET);
  frmOptions.edtComp.Text:= IntToStr(Info.AllowedLossyError);
  frmOptions.cbb1.ItemIndex:=Info.InterleavedMode;
  frmOptions.ShowModal;
  if  frmOptions.ModalResult=mrOk then
  begin
    Info.Custom.T1:= StrToInt(frmOptions.edtT1.Text);
    Info.Custom.T2:= StrToInt(frmOptions.edtT2.Text);
    Info.Custom.T3:= StrToInt(frmOptions.edtT3.Text);

    Info.Custom.RESET:= StrToInt(frmOptions.edtRESET.Text);
    Info.AllowedLossyError:= StrToInt(frmOptions.edtComp.Text);
    Info.InterleavedMode:=frmOptions.cbb1.ItemIndex;
  end;

  frmOptions.Free;
end;

procedure TfrmOptions.chkCompClick(Sender: TObject);
begin
  if chkComp.Checked then edtComp.Text:='0';
  edtComp.Enabled:=not chkComp.Checked;
end;

procedure TfrmOptions.chkDefaultTClick(Sender: TObject);
begin
 if chkDefaultT.Checked then
 begin
   edtT1.Text:='3';
   edtT2.Text:='7';
   edtT3.Text:='31';
 end;
 edtT1.Enabled:=not chkDefaultT.Checked;
 edtT2.Enabled:=not chkDefaultT.Checked;
 edtT3.Enabled:=not chkDefaultT.Checked;

end;

procedure TfrmOptions.chkRClick(Sender: TObject);
begin
  if chkR.Checked then edtRESET.Text:='64';
  edtRESET.Enabled:=not chkR.Checked;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  edtComp.Text:='0';
  edtRESET.Text:='64';
  edtT1.Text:='3';
  edtT2.Text:='7';
  edtT3.Text:='31';
end;

end.
