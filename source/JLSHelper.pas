{
   JPEG-LS Codec
   This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
   Converted from C to Pascal. 2017

   https://github.com/zekiguven/delphi_jpeg_jls

   author : Zeki Guven
}
unit JLSHelper;

interface
uses
  Windows, Graphics, Classes, SysUtils, JLSCodec;


  function BitsPerSample(APixelFormat: TPixelFormat):Integer;
  function BytesPerPixel(APixelFormat: TPixelFormat): Integer;
  function RawToBitmap(AStream:TMemoryStream;info:TJlsParameters): TBitmap;
  procedure BitmapToRaw(ABitmap:TBitmap; RawStream: TStream);
  function DecodeFileToBitmap(FileName: String; info:PJlsParameters):TBitmap;
  procedure EncodeBitmapToFile(FileName: String; Bitmap: TBitmap; info: PJlsParameters);

implementation
{$IFDEF VER150}
type
  TBytes = array of Byte;
{$ENDIF}
{$IFDEF VER160}
type
  TBytes = array of Byte;
{$ENDIF}
{$IFDEF VER170}
type
  TBytes = array of Byte;
{$ENDIF}
{$IFDEF VER180}
type
  TBytes = array of Byte;
{$ENDIF}



function BitsPerSample(APixelFormat: TPixelFormat):Integer;
begin
  Result:=-1;
  case APixelFormat of
    pf1Bit: Result:=1;
    pf4Bit: Result:=4;
    pf8Bit: Result:= 8;
    pf15Bit, pf16Bit: Result:= 5;
    pf24Bit: Result:= 8;
    pf32Bit: Result:= 8;
    //pfDevice: Result:= GetDeviceCaps(Bitmap.Canvas.Handle, BITSPIXEL) div 8
  end;
end;

function BytesPerPixel(APixelFormat: TPixelFormat): Integer;
begin
  Result := -1;
  case APixelFormat of
    pf8bit: Result := 1;
    pf16bit: Result := 2;
    pf24bit: Result := 3;
    pf32bit: Result := 4;
  end;
end;

function RawToBitmap(AStream:TMemoryStream;info:TJlsParameters): TBitmap;
var
  I:Integer;
  ABitmap:TBitmap;
  LogicalPalette   :  TMaxLogPalette;
  scanline,src:PByte;
  srcword:PWord;
  trip,tripsrc:PRGBTriple;
  J: Integer;
  r,g,b:byte;
begin
  try
    ABitmap :=TBitmap.Create;

    case info.Components  of
      1: case info.BitsPerSample of
            8:
              begin
                ABitmap.PixelFormat:=pf8bit;
                ABitmap.Width:=info.Width;
                ABitmap.Height:=info.Height;

                //ABitmap.SetSize(info.Width,info.Height);

                // Define 256 shades of gray palette
                LogicalPalette.palVersion := $0300;  // "Magic Number" for Windows LogPalette
                LogicalPalette.palNumEntries := 256;

                FOR i := 0 TO 255 DO
                BEGIN
                  LogicalPalette.palPalEntry[i].peRed   := i;
                  LogicalPalette.palPalEntry[i].peGreen := i;
                  LogicalPalette.palPalEntry[i].peBlue  := i;
                  LogicalPalette.palPalEntry[i].peFlags := PC_NOCOLLAPSE;
                END;
                ABitmap.Palette := CreatePalette(pLogPalette(@LogicalPalette)^);
                {$IFNDEF FPC}
                for I := 0 to ABitmap.Height-1 do
                begin
                  scanline:=ABitmap.ScanLine[i];
                  src:=AStream.Memory;
                  Inc(src,I* Info.Width);
                  Move(src^,scanline^,Info.Width);
                end;
                {$ELSE}
                for I := 0 to ABitmap.Height-1 do
                begin
                  trip:=PRGBTriple(ABitmap.ScanLine[i]);
                  src:=AStream.Memory;
                  Inc(src,I*Info.Width);

                  for j := 0 to Info.Width-1 do
                  begin
                    trip^.rgbtBlue:=src^;
                    trip^.rgbtGreen:=src^;
                    trip^.rgbtRed:=src^;
                    Inc(Trip);
                    Inc(src);
                  end;
                end;
                {$ENDIF}
              end;

           16:
              begin
              ABitmap.PixelFormat:=pf8bit;
              ABitmap.Width:=info.Width;
              ABitmap.Height:=info.Height;
              //ABitmap.SetSize(Info.Width, info.Height);



              for I := 0 to ABitmap.Height-1 do
              begin
                scanline:=ABitmap.ScanLine[i];
                srcword:=AStream.Memory;
                Inc(srcword,I*Info.Width);
                //Result.A := 1;
                for j := 0 to Info.Width-1 do
                begin
                  r := Word( ((srcword^ and $F800) shr 8)); //to rgb888
                  g := Word( ((srcword^ and $07E0) shr 3) );
                  b := Word( ((srcword^ and $001F) shl 3) );

                  scanline^ := (((r shl 1) + (g shl 2) + g + b) shr 3);

                  Inc(scanline);
                  Inc(srcword);
                end;
              end;
              end;

           10,12,15:
              begin
                ABitmap.PixelFormat:=pf16bit;
                ABitmap.Width:=info.Width;
                ABitmap.Height:=info.Height;
                //ABitmap.SetSize(info.Width ,info.Height);

                for I := 0 to ABitmap.Height-1 do
                begin
                  scanline:=ABitmap.ScanLine[i];
                  src:=AStream.Memory;
                  Inc(src,I*Info.Width*2);
                  for J := 0 to ABitmap.Width-1 do
                  begin
                    PWordArray(scanline)[J]:=PWordArray(src)[J]
                  end;

                end;

              end;
          end;
      3: case info.BitsPerSample of
            8:begin
            ABitmap.PixelFormat:=pf24bit;
            ABitmap.Width:=info.Width;
            ABitmap.Height:=info.Height;
            //  ABitmap.SetSize(Info.Width, info.Height);

              for I := 0 to ABitmap.Height-1 do
              begin
                trip:=PRGBTriple(ABitmap.ScanLine[i]);
                tripsrc:=AStream.Memory;
                Inc(tripsrc,I*Info.Width);

                for j := 0 to Info.Width-1 do
                begin
                  trip^.rgbtBlue:=tripsrc^.rgbtRed;
                  trip^.rgbtGreen:=tripsrc^.rgbtGreen;
                  trip^.rgbtRed:=tripsrc^.rgbtBlue;
                  Inc(Trip);
                  Inc(tripsrc);
                end;
              end;

            end;
         end;
    end;

  finally

  end;
  Result:=ABitmap;
end;

procedure BitmapToRaw(ABitmap:TBitmap; RawStream: TStream);
var
  I,j: Integer;
  P:PByte;
  buf:TBytes;
  tmp:byte;
  BPP: Integer;
begin
  BPP := BytesPerPixel(ABitmap.PixelFormat);

  if BPP=3 then
   SetLength(buf,ABitmap.Width*3);

  for I := 0 to ABitmap.Height-1 do
  begin
    P:=ABitmap.ScanLine[I];

    case BPP of
      1 : RawStream.Write(P^, ABitmap.Width);
      3 :
        begin
          Move(P^,buf[0],ABitmap.Width*3);

          for j := 0 to ABitmap.Width-1 do
          begin
            tmp:=buf[j*3];
            buf[j*3]:=buf[j*3+2];
            buf[j*3+2]:=tmp;
          end;
          RawStream.Write(buf[0], ABitmap.Width*3)
        end;

    end;

  end;
  RawStream.Position:=0;
end;

function DecodeFileToBitmap(FileName: String; info: PJlsParameters):TBitmap;
var
  LInput,LOutput:TMemorystream;
  ABitmap:TBitmap;
begin
  LInput:=TMemorystream.Create;
  LOutput:=TMemorystream.Create;
  fillchar(info^, SizeOf(info^), 0);

  try
    LInput.LoadFromFile(Filename);

    if jpegls_decompress(LInput, LOutput, info) then
    begin
      ABitmap:=RawToBitmap(LOutput,info^);
      Result:=ABitmap;
    end;
  finally
    LInput.Free;
    LOutput.Free;
  end;

end;

procedure EncodeBitmapToFile(FileName: String; Bitmap: TBitmap; info: PJlsParameters);
var
  LInput,LOutput:TMemorystream;

begin
  LInput:=TMemorystream.Create;
  LOutput:=TMemorystream.Create;

  try
    BitmapToRaw(Bitmap,LInput);
    info^.Width:=Bitmap.Width;
    info^.Height:=Bitmap.Height;
    info^.Components:=BytesPerPixel(Bitmap.PixelFormat);

    if jpegls_compress(LInput, LOutput, info) then
    begin
      LOutput.SaveToFile(FileName);
    end;
  finally
    LInput.Free;
    LOutput.Free;
  end;

end;



end.
