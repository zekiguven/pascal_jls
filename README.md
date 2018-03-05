Delphi / Free Pascal Compitable  JPEG-LS Codec
====
This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm  

Converted from C to Pascal by Zeki Guven.

Added sample demo project.(VCL & FMX & FPC)

![Demo](https://github.com/zekiguven/pascal_jls/blob/master/doc/jls_demo.jpg)

# How to use ?

## Parameter types

    /// Defines the JPEG-LS preset coding parameters as defined in ISO/IEC 14495-1, C.2.4.1.1.
    /// JPEG-LS defines a default set of parameters, but custom parameters can be used.
    /// When used these parameters are written into the encoded bit stream as they are needed for the decoding process.
    TJlsCustomParameters = packed record
      /// Maximum possible value for any image sample in a scan.
      /// This must be greater than or equal to the actual maximum value for the components in a scan.
      MAXVAL:integer;
      /// First quantization threshold value for the local gradients.
      T1:integer;
      /// Second quantization threshold value for the local gradients.
      T2:integer;
      /// Third quantization threshold value for the local gradients.
      T3:integer;
      /// Value at which the counters A, B, and N are halved.
      RESET:integer;
    end;

    PJlsParameters = ^TJlsParameters;


    TJlsParameters = packed record
      /// Width of the image in pixels.
      Width: integer;
      /// Height of the image in pixels.
      Height: integer;
      /// The number of valid bits per sample to encode.
      /// Valid range 2 - 16. When greater than 8, pixels are assumed to stored as two bytes per sample, otherwise one byte per sample is assumed.
      BitsPerSample: integer;
      /// The number of components.
      /// Typical 1 for monochrome images and 3 for color images.
      Components: integer;
      /// Defines the allowed lossy error. Value 0 defines lossless.
      AllowedLossyError:integer;
      /// Defines the interleave mode for multi-component (color) pixel data.
      /// PLANE_INT =  0;
      /// LINE_INT = 1;
      /// PIXEL_INT  = 2;
      InterleavedMode:Integer;
      Custom: TJlsCustomParameters;
    end;

## Decoding a JLS File

    function DecodeFileToBitmap(FileName: String):TBitmap;
    var
      LInput,LOutput:TMemorystream;
      info:TJlsParameters;
      ABitmap:TBitmap;
    begin
      LInput:=TMemorystream.Create;
      LOutput:=TMemorystream.Create;
      fillchar(info, SizeOf(info), 0);

      try
        LInput.LoadFromFile(Filename);

        if jpegls_decompress(LInput, LOutput, @info) then
        begin
          ABitmap:=RawToBitmap(LOutput,info);
          Result:=ABitmap;
        end;
      finally
      LInput.Free;
      LOutput.Free;
    end;

  
    //Load JLS...
    procedure TForm1.btnOpenClick(Sender: TObject);
    var
      ABitmap:TBitmap;
    begin
      if dlgOpen1.Execute() then
      begin
        ABitmap:=DecodeFileToBitmap(dlgOpen1.FileName);
        img1.Picture.Assign(ABitmap);
        ABitmap.Free;
      end;
    end;				
					

## Encoding as a JLS File

    procedure EncodeBitmapToFile(FileName: String; Bitmap: TBitmap);
    var
      LInput,LOutput:TMemorystream;
      info:TJlsParameters;
    begin
      LInput:=TMemorystream.Create;
      LOutput:=TMemorystream.Create;
      fillchar(info, SizeOf(info), 0);
      
      try
        BitmapToRaw(Bitmap,LInput);
        info.Width:=Bitmap.Width;
        info.Height:=Bitmap.Height;
        info.Components:=BytesPerPixel(Bitmap.PixelFormat);
        info.BitsPerSample:=BytesPerPixel(Bitmap.PixelFormat)*8;

        if jpegls_compress(LInput, LOutput, @info) then
        begin
          LOutput.SaveToFile(FileName);
        end;
      finally
        LInput.Free;
        LOutput.Free;
      end;

    end;
      
     //Save JLS...
    procedure TForm1.btnSaveClick(Sender: TObject);
    var
      Bitmap:TBitmap;
    begin
      if dlgSave1.Execute() then
      begin
        Bitmap:=TBitmap.Create;
        Bitmap.HandleType := bmDIB;
        Bitmap.Assign(img1.Picture.Graphic);
        EncodeBitmapToFile(dlgSave1.FileName,Bitmap);
        Bitmap.Free;
      end;
    end;
					






