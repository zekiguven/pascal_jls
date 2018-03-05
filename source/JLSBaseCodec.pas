{
   JPEG-LS Codec
   This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
   Converted from C to Pascal. 2017

   https://github.com/zekiguven/delphi_jpeg_jls

   author : Zeki Guven
}
unit JLSBaseCodec;

interface
uses
  JLSGlobal, JLSJpegmark, JLSBitIO, JLSMelcode, Classes, SysUtils,
  JLSLossless, JLSLossy, math;

type

  { TbsJLSBaseCodec }

  TJLSBaseCodec=class
  private

    function GetComponents: Int;
    function GetHeight: Int;
    function GetWidth: Int;
    procedure SetComponents(AValue: Int);
    procedure SetHeight(AValue: Int);
    procedure SetWidth(AValue: Int);
    function GetRESET: Int;
    procedure SetRESET(const Value: Int);
    function GetNEAR: Int;
    procedure SetNEAR(const Value: Int);
    procedure SetT1(const Value: int);
    procedure SetT2(const Value: int);
    procedure SetT3(const Value: int);
    procedure SetMAXVAL(const Value: int);
    procedure SetInterleaveMode(const Value: Integer);
    procedure SetRestartInterval(const Value: Integer);
    function GetLimit: Integer;
    procedure SetLimit(const Value: Integer);
    function GetAlpha: Integer;
    procedure SetAlpha(const Value: Integer);
  protected
    FBitIO: TJLSBitIO;
    FJpeg: TJLSJpegMark;
    FMelcode: TJLSMelcode;
    FLossless: TJLSLossless;
    FLossy : TJLSLossy;
    //initialization
    FImageInfo:TImageInfo;
    FInputStream: TStream;
    FOutputStream: TStream;
    FEnableLog : Boolean;
    FLog : TStringList;
    pscanline, cscanline : ppixel;
    pscanl0, cscanl0:ppixel;

    head_frame: pjpeg_ls_header;
    head_scan: array [0..MAX_SCANS-1] of pjpeg_ls_header;

    samplingx : array[0..MAX_COMPONENTS-1] of int;
    samplingy : array[0..MAX_COMPONENTS-1] of int;

    c_columns : array[0..MAX_COMPONENTS-1] of int;
    c_rows:array[0..MAX_COMPONENTS-1] of int;

    whose_max_size_rows, whose_max_size_columns: int;
    number_of_scans: int;
    color_mode: int;
    alpha0 : int;
    restart_interval:int;   { indicates the restart interval }

        //encode...
    need_lse:int;           { if we need an LSE marker (non-default params) }
    need_table:int;         { if we need an LSE marker (mapping table) }
    need_restart:int;       { if we need to add restart markers }

    function prepareLUTs:int;
    function prepare_qtables(absize:int; AnNEAR:int):int;
    procedure init_stats(absize:int);
    function GetInputStream: TStream;
    function GetOutputStream: TStream;
    procedure SetInputStream(const Value: TStream);
    procedure SetOutputStream(const Value: TStream);
    { Set thresholds to default unless specified by header: }
    procedure set_thresholds(alfa,AnNEAR:int; T1p,T2p,T3p:pint);
  public
    sampling: array [0..MAX_COMPONENTS-1] of Integer;
    disclaimer:PChar;
    ceil_half_qbeta:Integer;

    bpp:Integer;      { bits per sample }
    { for LOSSY mode }
    quant:Integer;

    lutmax: int;
    lossy :Boolean;
    nopause:boolean;   { whether to pause the legal notice or not }
    nolegal :boolean;   { whether to print the legal notice or not }

    {Context quantization thresholds  - initially unset }
    FT3 :int;
    FT2 :int;
    FT1 :int;
    FTa :int;
    constructor Create; virtual;
    destructor Destroy;override;
    function Execute:Boolean;virtual;
    property Height:Int read GetHeight write SetHeight;
    property Width:Int read GetWidth write SetWidth;
    property RESET: Int read GetRESET write SetRESET;
    property NEAR : Int read GetNEAR write SetNEAR;
    property Components:Int read GetComponents write SetComponents;
    property T3 :int read FT3 write SetT3;
    property T2 :int read FT2 write SetT2;
    property T1 :int read FT1 write SetT1;
    property MAXVAL : int read alpha0 write SetMAXVAL;
    property Limit : Integer read GetLimit write SetLimit;
    property Alpha : Integer read GetAlpha write SetAlpha;
    property InterleavedMode:Integer read color_mode write SetInterleaveMode;
    property RestartInterval:Integer read restart_interval write SetRestartInterval;
    property InputStream:TStream read GetInputStream write SetInputStream;
    property OutputStream:TStream read GetOutputStream write SetOutputStream;
    property EnableLog :Boolean read FEnableLog write FEnableLog;

  end;

implementation
{$IFDEF TRIAL}
 {$I BesaTrial.inc}
{$ENDIF}

{ TbsJLSBaseCoder }

constructor TJLSBaseCodec.Create;
begin
  FInputStream:=NIL;
  FOutputStream:=NIL;
  FBitIO:=TJLSBitIO.Create(FInputStream, FOutputStream);
  FJpeg:=TJLSJpegMark.Create(FBitIO, @FImageInfo);
  FMelcode:=TJLSMelcode.Create(FBitIO, @FImageInfo);
  FLossless:= TJLSLossless.Create(FBitIO, FMelcode, @FImageInfo);
  FLossy := TJLSLossy.Create(FBitIO, FMelcode, @FImageInfo);
  FLog:=TStringList.Create;
  FT3 := -1;
  FT2 := -1;
  FT1 := -1;
  FTa := -1;
  FImageInfo.RESET:=64;
  FEnableLog:=False;
  nopause:= true;
  nolegal := true;
end;

destructor TJLSBaseCodec.Destroy;
begin
  FLossless.Free;
  FLossy.Free;
  FMelcode.Free;
  FJpeg.Free;
  FBitIO.Free;
  FLog.Free;
  inherited;
end;


function TJLSBaseCodec.Execute: Boolean;
begin
  Result:=False;
end;

function TJLSBaseCodec.GetInputStream: TStream;
begin
  Result:=FInputStream;
end;

function TJLSBaseCodec.GetLimit: Integer;
begin
  Result:=FImageInfo.limit;
end;

function TJLSBaseCodec.GetNEAR: Int;
begin
  Result:=FImageInfo.NEAR;
end;

function TJLSBaseCodec.GetOutputStream: TStream;
begin
  Result:= FOutputStream;
end;

function TJLSBaseCodec.GetRESET: Int;
begin
  Result:=FImageInfo.RESET;
end;

{ Initialize A[], B[], C[], and N[] arrays }
procedure TJLSBaseCodec.init_stats(absize: int);
var
  i, initabstat, slack : int;
begin
  slack := 1 shl INITABSLACK;
  initabstat := (absize + slack div 2) div slack;
  if ( initabstat < MIN_INITABSTAT ) then initabstat := MIN_INITABSTAT;

  { do the statistics initialization }
  for i := 0 to pred(TOT_CONTEXTS) do
  begin
   FImageInfo.C[i] := 0;
   FImageInfo.B[i] := 0;
   FImageInfo.N[i] := INITNSTAT;
   FImageInfo.A[i] := initabstat;
  end;
end;

function TJLSBaseCodec.GetAlpha: Integer;
begin
  Result:=FImageInfo.alpha;
end;

function TJLSBaseCodec.GetComponents: Int;
begin
  Result:= FImageInfo.components;
end;

function TJLSBaseCodec.GetHeight: Int;
begin
  Result:=FImageInfo.height;
end;

function TJLSBaseCodec.GetWidth: Int;
begin
  Result:=FImageInfo.width;
end;

procedure TJLSBaseCodec.SetAlpha(const Value: Integer);
begin
  FImageInfo.alpha:=Value;
end;

procedure TJLSBaseCodec.SetComponents(AValue: Int);
begin
  FImageInfo.components:=AValue;
end;

procedure TJLSBaseCodec.SetHeight(AValue: Int);
begin
  FImageInfo.height:=AValue;
end;

procedure TJLSBaseCodec.SetWidth(AValue: Int);
begin
  FImageInfo.width:=AValue;
end;

procedure TJLSBaseCodec.set_thresholds(alfa, AnNEAR: int; T1p, T2p,
  T3p: pint);
var
  lambda, ilambda, quant_ : int;
begin
  ilambda := 256 div alfa;
  quant_ := 2*AnNEAR+1;
  FT1 := T1p^;
  FT2 := T2p^;
  FT3 := T3p^;

  if (alfa<4096) then
     lambda := (alfa+127) div 256
  else
     lambda := (4096+127) div 256;

  if ( FT1 <= 0 )  then
  begin
    { compute lossless default }
    if ( IsTrue(lambda) ) then
      FT1 := lambda*(BASIC_T1 - 2) + 2
    else begin  { alphabet < 8 bits }
      FT1 := BASIC_T1 div ilambda;
      if ( FT1 < 2 ) then FT1 := 2;
    end;
    { adjust for lossy }
    FT1 :=FT1+3*AnNEAR;

    { check that the default threshold is in bounds }
    if ( FT1 < AnNEAR+1) or (FT1 > (alfa-1) ) then
         FT1 := AnNEAR+1;         { eliminates the threshold }
  end;

  if ( FT2 <= 0 )  then
  begin
    { compute lossless default }
    if (IsTrue(lambda) ) then
      FT2 := lambda*(BASIC_T2 - 3) + 3
    else begin
      FT2 := BASIC_T2 div ilambda;
      if ( FT2 < 3 ) then FT2 := 3;
    end;
    { adjust for lossy }
    FT2 := FT2+5*AnNEAR;

    { check that the default threshold is in bounds }
    if ( FT2 < FT1) or (FT2 > (alfa-1) ) then
         FT2 := FT1;         { eliminates the threshold }
  end;

  if ( FT3 <= 0 ) then
  begin
    { compute lossless default }
    if (IsTrue(lambda) ) then
      FT3 := lambda*(BASIC_T3 - 4) + 4
    else begin
      FT3 := BASIC_T3 div ilambda;
      if ( FT3 < 4 ) then FT3 := 4;
    end;
    { adjust for lossy }
    FT3 :=FT3+ 7*AnNEAR;

    { check that the default threshold is in bounds }
    if ( FT3 < FT2 ) or (FT3 > (alfa-1) ) then
         FT3 := FT2;         { eliminates the threshold }
  end;

  T1p^ := FT1;
  T2p^ := FT2;
  T3p^ := FT3;
end;

{ Setup Look Up Tables for quantized gradient merging }
function TJLSBaseCodec.prepareLUTs:int;
var
 i, j, idx, lmax : int;
 k : byte;
 q1, q2, q3, n1, n2, n3, ineg, sgn:int;

begin
  Result:=0;

  lmax := min(FImageInfo.alpha, lutmax);

  { implementation limitation: }
  if ( FT3 > lmax-1 ) then begin
    FLog.Append(Format('ERROR : Sorry, current implementation does not support threshold T3 > %d, got %d',[lmax-1,FT3]));
    Result:=10;
    exit;
  end;


  { Build classification tables (lossless or lossy) }

  if (lossy=FALSE) then
  begin

    for i:= -lmax + 1 to pred(lmax) do
    begin

      if  ( i <= -FT3 ) then       { ...... -T3  }
        idx := 7
      else if ( i <= -FT2 ) then   { -(T3-1) ... -T2 }
        idx := 5
      else if ( i <= -FT1 ) then   { -(T2-1) ... -T1 }
        idx := 3

      else if ( i <= -1 ) then    { -(T1-1) ...  -1 }
        idx := 1
      else if ( i = 0 ) then      {  just 0 }
        idx := 0

      else if ( i < FT1 ) then     { 1 ... T1-1 }
        idx := 2
      else if ( i < FT2 ) then     { T1 ... T2-1 }
        idx := 4
      else if ( i < FT3 ) then     { T2 ... T3-1 }
        idx := 6
      else                        { T3 ... }
        idx := 8;

      FImageInfo.vLUT[0][i + lutmax] := CREGIONS * CREGIONS * idx;
      FImageInfo.vLUT[1][i + lutmax] := CREGIONS * idx;
      FImageInfo.vLUT[2][i + lutmax] := idx;
    end;

  end else begin

    for i := -lmax + 1 to pred(lmax) do
    begin

      if ( FImageInfo.NEAR >= (FImageInfo.alpha-1) ) then
        idx := 0   { degenerate case, regardless of thresholds }
      else

        if  ( i <= -FT3 ) then       { ...... -T3  }
          idx := 7
        else if ( i <= -FT2 ) then   { -(T3-1) ... -T2 }
          idx := 5
        else if ( i <= -FT1 ) then   { -(T2-1) ... -T1 }
          idx := 3

        else if ( i <= -FImageInfo.NEAR-1 ) then    { -(T1-1) ...  -NEAR-1 }
          idx := 1
        else if ( i <= FImageInfo.NEAR ) then     {  within NEAR of 0 }
          idx := 0

        else if ( i < FT1 ) then     { 1 ... T1-1 }
          idx := 2
        else if ( i < FT2 ) then     { T1 ... T2-1 }
          idx := 4
        else if ( i < FT3 ) then     { T2 ... T3-1 }
          idx := 6
        else                    { T3 ... }
          idx := 8;

      FImageInfo.vLUT[0][i + lutmax] := CREGIONS * CREGIONS * idx;
      FImageInfo.vLUT[1][i + lutmax] := CREGIONS * idx;
      FImageInfo.vLUT[2][i + lutmax] := idx;
    end;

  end;


  {  prepare context mapping table (symmetric context merging) }
  FImageInfo.classmap[0] := 0;
  j:=0;

  for i:=1 to pred(CONTEXTS1) do
  begin
    n1:=0;
    n2:=0;
    n3:=0;

    if (IsTrue(FImageInfo.classmap[i])) then continue;

    q1 := i div(CREGIONS*CREGIONS);    { first digit }
    q2 := (i div CREGIONS) mod CREGIONS;    { second digit }
    q3 := i mod CREGIONS;          { third digit }

    if(IsTrue(q1 mod 2)) or ((q1=0) and IsTrue(q2 mod 2) ) or ((q1=0) and (q2=0) and IsTrue(q3 mod 2)) then
      sgn := -1
    else
      sgn := 1;

    { compute negative context }
    if IsTrue(q1) then
    begin
      if IsTrue(q1 mod 2) then n1 := q1 + 1 else n1 := q1 -1;
    end;

    if IsTrue(q2) then
    begin
      if IsTrue(q2 mod 2) then n2 := q2 + 1 else n2 := q2 -1;
    end;

    if IsTrue(q3) then
    begin
      if IsTrue(q3 mod 2) then  n3 := q3 + 1 else n3 := q3 -1;
    end;

    ineg := (n1*CREGIONS+n2)*CREGIONS+n3;
    Inc(j);    { next class number }
    FImageInfo.classmap[i] := sgn*j;
    FImageInfo.classmap[ineg] := -sgn*j;
  end;

end;

{ prepare quantization tables for near-lossless quantization }
function TJLSBaseCodec.prepare_qtables(absize: int; AnNEAR: int):int;
var
  diff, qdiff:int;
  beta_, quant_:int;
  arrpos:int;
begin
  Result:=0;

  quant_ := 2*AnNEAR+1;
  beta_ := absize;


  GetMem(FImageInfo.qdiv0, (2*absize-1)*sizeof(int) );
  if (FImageInfo.qdiv0 = NIL ) then
  begin
    FLog.Append('ERROR : qdiv  table');
    Result:=10;
    exit;
  end;

  FImageInfo.qdiv := Pointer(FImageInfo.qdiv0);
  Inc(FImageInfo.qdiv,absize-1);

  GetMem(FImageInfo.qmul0,(2*beta_-1)*SizeOf(int));

  if ( FImageInfo.qmul0 = NIL ) then
  begin
    FLog.Append('ERROR : qmul  table');
    Result:=10;
    exit;
  end;

  FImageInfo.qmul := Pointer(FImageInfo.qmul0);
  Inc(FImageInfo.qmul,beta_-1);

  arrpos:=beta_-1;

  for diff := -(absize-1) to pred(absize) do
  begin
    if ( diff<0 ) then
      qdiff := - ( (AnNEAR-diff) div quant_ )
    else
      qdiff := ( AnNEAR + diff ) div quant_;

    FImageInfo.qdiv0^[diff+arrpos]:=qdiff;
  end;

  for qdiff := -(beta_-1) to pred(beta_) do
  begin
    diff := quant*qdiff;
    FImageInfo.qmul0^[qdiff+arrpos]:=diff;
  end;

end;


procedure TJLSBaseCodec.SetInputStream(const Value: TStream);
begin
  FInputStream:=Value;
  FBitIO.FInputStream:=FInputStream;
end;

procedure TJLSBaseCodec.SetInterleaveMode(const Value: Integer);
begin
  color_mode:= Value;
end;

procedure TJLSBaseCodec.SetLimit(const Value: Integer);
begin
  FImageInfo.limit:= Value;
end;

procedure TJLSBaseCodec.SetMAXVAL(const Value: int);
begin
   if Value=0 then
     Alpha0:=DEF_ALPHA
   else
     Alpha0:=Value;
end;

procedure TJLSBaseCodec.SetNEAR(const Value: Int);
begin
   FImageInfo.NEAR:=Value;
end;

procedure TJLSBaseCodec.SetOutputStream(const Value: TStream);
begin
  FOutputStream:=Value;
  FBitIO.FOutputStream:=FOutputStream;
end;

procedure TJLSBaseCodec.SetRESET(const Value: Int);
begin
  { Reset value }
  if (FImageInfo.RESET<>DEFAULT_RESET) and (Value > 0) then
  begin
    FImageInfo.RESET:=Value;
    need_lse:=1;
  end;
end;

procedure TJLSBaseCodec.SetRestartInterval(const Value: Integer);
begin
  { Enable use of Restart Markers }
  need_restart := 1;
end;

procedure TJLSBaseCodec.SetT1(const Value: int);
begin
  if Value<>0 then
  begin
    FT1 := Value;
    need_lse:=1;
  end;
end;

procedure TJLSBaseCodec.SetT2(const Value: int);
begin
  if Value<>0 then
  begin
    FT2 := Value;
    need_lse:=1;
  end;
end;

procedure TJLSBaseCodec.SetT3(const Value: int);
begin
  if Value<>0 then
  begin
    FT3 := Value;
    need_lse:=1;
  end;
end;

end.
