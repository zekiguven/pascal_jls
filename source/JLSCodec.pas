{
   JPEG-LS Codec
   This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
   Converted from C to Pascal. 2017

   https://github.com/zekiguven/pascal_jls

   author : Zeki Guven
}
unit JLSCodec;

interface
uses
  Classes, JLSGlobal, JLSEncoder, JLSDecoder;

type
 PJlsParameters=JLSGlobal.PJlsParameters;
 TJlsParameters=JLSGlobal.TJlsParameters;
 TJlsCustomParameters=JLSGlobal.TJlsCustomParameters;


 function jpegls_decompress(SourceStream, OutputStream: TStream; info:PJlsParameters):Boolean;
 function jpegls_compress(SourceStream, OutputStream: TStream; info:PJlsParameters):Boolean;

implementation

function jpegls_decompress(SourceStream, OutputStream: TStream; info:PJlsParameters):Boolean;
var
  dec:TJLSDecoder;
begin
  Result:=False;
  dec:=TJLSDecoder.Create;
  try
    dec.InputStream:=SourceStream;
    dec.OutputStream:=OutputStream;

    if dec.Execute then
    begin
      if info<>NIL then
      begin
        info^.Width:=dec.Width;
        info^.Height:=dec.Height;
        info^.BitsPerSample:=dec.bpp;
        info^.Components:=dec.Components;
        info^.AllowedLossyError:=dec.NEAR;
        info^.InterleavedMode:=dec.InterleavedMode;
        info^.Custom.T1:=dec.T1;
        info^.Custom.T2:=dec.T2;
        info^.Custom.T3:=dec.T3;
        info^.Custom.RESET:=dec.RESET;
        info^.Custom.MAXVAL:=dec. MAXVAL;
      end;
      Result:=True;
    end;


  finally
    dec.Free;
  end;
end;


function jpegls_compress(SourceStream, OutputStream: TStream; info:PJlsParameters):Boolean;
var
  enc:TJLSEncoder;
begin
  Result:=False;
  enc:=TJLSEncoder.Create;
  try
    enc.InputStream:=SourceStream;
    enc.OutputStream:=OutputStream;

    enc.Width:=info^.Width;
    enc.Height:=info^.Height;
    enc.bpp:=info^.BitsPerSample;
    enc.Components:=info^.Components;
    enc.InterleavedMode:=info^.InterleavedMode;
    enc.T1:=info^.Custom.T1;
    enc.T2:=info^.Custom.T2;
    enc.T3:=info^.Custom.T3;
    enc.RESET:=info^.Custom.RESET;
    enc.NEAR:=info^.AllowedLossyError;
    enc.MAXVAL :=info^.Custom.MAXVAL;

    if enc.Execute then
    begin
      Result:=True;
    end;


  finally
    enc.Free;
  end;
end;
end.
