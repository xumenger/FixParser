unit FlexFix;

interface
uses
  StrUtils, SysUtils, Classes;

const
  SOH = #1;
  SEQUAL = '='; 

type
  TFixValue = class;

  TBaseFix = class
  private
    FixList: TList;
  public
    constructor Create; overload;
    constructor Create(fixPkg: string); overload;
    destructor Destroy; override;
    function GetFixValue(key: Integer; index: Integer = 1): TFixValue; 
  protected
    //获取子串subStr出现在Str中第n次对应的位置，未找到时返回0
    function NPos(subStr: string; Str: string; n: Integer): Integer;
    //sText是Key=Value<SOH>格式的字符串，根据该字符串获取键、值
    procedure GetNameValue(sText: string; var iKey, sValue: string);
  protected
    sBuffer: string;
  end;

  TFixValue = class(TBaseFix)
  public    
    procedure SetKey(key: Integer);
    procedure SetValue(value: string);
    function GetKey: Integer;
    function AsString: string; 
    function AsInteger: Integer;
    function AsFloat: Double; 
  private
    iKey: Integer;
    sValue: string;
  end;

  TFlexFix = class(TBaseFix)
  public
    procedure AddValue(key: Integer; value: string; iLen: Integer); overload;
    procedure AddValue(key: Integer; value: Integer; iLen: Integer); overload;
    procedure AddValue(key: Integer; value: Double; iLen: Integer); overload;
    function GetBuffer: string;
  private
    function GetSpaceText(value: string; iLen: Integer): string; overload;
    function GetSpaceText(value: Integer; iLen: Integer): string; overload;
    function GetSpaceText(value: Double; iLen: Integer): string; overload;
  end;

implementation

{TBaseFix}
constructor TBaseFix.Create;
begin
  inherited;
  FixList := TList.Create();
  sBuffer := '';
end;

constructor TBaseFix.Create(fixPkg: string);
begin
  inherited Create;
  FixList := TList.Create();
  sBuffer := SOH + fixPkg;
  sBuffer := StringReplace(sBuffer, ' ', '', [rfReplaceAll]);
end;

destructor TBaseFix.Destroy;
var
  aFixPkg: TBaseFix;
  i: Integer;
begin
  sBuffer := '';
  for i:=0 to FixList.Count-1 do
  begin
    aFixPkg := FixList.Items[i];
    if nil <> aFixPkg then
    begin
      aFixPkg.Free();
    end;
  end;
  if nil <> FixList then
  begin
    FixList.Clear();
    FixList.Free();
  end;
  inherited;
end;

function TBaseFix.GetFixValue(key: Integer; index: Integer = 1): TFixValue;
var
  searchKey, tmpBuffer: string;
  pos, posSOH, posNext: Integer;
  sNewBuffer, sKeyValue: string;
  sKey, sValue: string;
begin
  searchKey := SOH + IntToStr(key) + SEQUAL;
  pos := NPos(searchKey, sBuffer, index);
  tmpBuffer := Copy(sBuffer, pos, Length(sBuffer)-pos+1);
  if (0 <> pos) then
  begin
    posSOH := NPos(SOH, tmpBuffer, 2);
    sKeyValue := Copy(tmpBuffer, 1, posSOH);
    GetNameValue(sKeyValue, sKey, sValue);

    posNext := NPos(searchKey, tmpBuffer, 2);
    if (0 <> posNext) then
    begin
      //如果有下一个key，则拷贝第index 个key-value 之后到第index+1 个key 之间的字符串
      sNewBuffer := Copy(tmpBuffer, posSOH+1, posNext-posSOH+1);
    end
    else
    begin
      //如果没有下一个key，则拷贝第index 个key-value 之后到sBuffer 最后字符之间的字符串
      sNewBuffer := Copy(tmpBuffer, posSOH+1, Length(tmpBuffer)-posSOH);
    end;
  end
  else
  begin
    sNewBuffer := '';
    sValue := '';
  end;
  
  if '' <> sNewBuffer then
  begin
    Result := TFixValue.Create(sNewBuffer);
    Result.SetKey(key);
    Result.SetValue(sValue);
    FixList.Add(Result);
  end
  else
  begin
    Result := nil;
  end;
end;

//返回值0表示没有搜索到
function TBaseFix.NPos(subStr: string; Str: string; n: Integer): Integer;
var
  i, position: Integer;
  x, y: Integer;
begin
  Result := 0;
  for i:=1 to n do
  begin
    position := Pos(subStr, Str);

    x := position + Length(subStr);
    y := Length(Str) - position - Length(subStr)+1;
    Str := Copy(Str, x, y);
    if (0 = position) then
    begin
      Result := 0;
      Break;
    end;
    Result := Result + position;
  end;
  if (0 <> Result) then
  begin
    Result := Result + (n-1) * (Length(subStr)-1);
  end;
end;

procedure TBaseFix.GetNameValue(sText: string; var iKey, sValue: string);
var
  sTemp: string;
  index: Integer;
begin
  sTemp := Trim(sText);
  iKey := '';
  sValue := '';
  index := Pos(SEQUAL, sTemp);
  if (0 = index) then
  begin
    Exit;
  end;
  iKey := Trim(LeftStr(sTemp, index-1));
  sValue := Trim(RightStr(sTemp, Length(sTemp)-index));
end;

{TFixValue}
procedure TFixValue.SetKey(key: Integer);
begin
  iKey := key;  
end;

procedure TFixValue.SetValue(value: string);
begin
  sValue := value;
end;

function TFixValue.GetKey: Integer;
begin
  Result := iKey;
end;

function TFixValue.AsString: string;
begin
  Result := sValue;
end;

function TFixValue.AsInteger: Integer;
begin
  try
    Result := StrToInt(sValue);
  except
    Result := 0;
  end;
end;

function TFixValue.AsFloat: Double;
begin
  try
    Result := StrToFloat(sValue);
  except
    Result := 0.0;
  end;
end;

{TEasyFix}
procedure TFlexFix.AddValue(key: Integer; value: string; iLen: Integer);
begin
  sBuffer := sBuffer + IntToStr(key) + SEQUAL + GetSpaceText(value, iLen) + SOH;
end;

procedure TFlexFix.AddValue(key: Integer; value: Integer; iLen: Integer);
begin
  sBuffer := sBuffer + IntToStr(key) + SEQUAL + GetSpaceText(value, iLen) + SOH;
end;

procedure TFlexFix.AddValue(key: Integer; value: Double; iLen: Integer);
begin
  sBuffer := sBuffer + IntToStr(key) + SEQUAL + GetSpaceText(value, iLen) + SOH;
end;

function TFlexFix.GetBuffer: string;
begin
  //打包包头，第一个键值对是FIX包的长度，实际要不要打包也是需要看双方约定
  sBuffer := '9' + SEQUAL + GetSpaceText((Length(sBuffer)), 5) + SOH + sBuffer;

  //打包包尾
  //这个看通信双方的约定，有些约定要在包尾用专门的字段打包校验值等信息，有些则没有

  //获取最终FIX包
  Result := sBuffer;
end;

function TFlexFix.GetSpaceText(value: string; iLen: Integer): string;
begin
  //string类型的要在后补空格
  //Result := value + StringOfChar(' ', iLen - Length(value));
  Result := value;
end;

function TFlexFix.GetSpaceText(value: Integer; iLen: Integer): string;
begin
  //number类型要在前补空格
  //Result := StringOfChar(' ', iLen - Length(IntToStr(value))) + IntToStr(value);
  Result := IntToStr(value);
end;

function TFlexFix.GetSpaceText(value: Double; iLen: Integer): string;
begin
  //number类型要在前补空格
  //Result := StringOfChar(' ', iLen - Length(FloatToStr(value))) + FloatToStr(value);
  Result := FloatToStr(value);
end;

end.
