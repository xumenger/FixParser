unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, EasyFix;

type
  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  fix: TEasyFix;
begin
  fix := TEasyFix.Create;
  fix.AddValue(10, 100, 4);
  fix.AddValue(100, '600570', 8);

  ShowMessage(fix.GetBuffer);
  fix.Free;
end;

procedure TForm1.btn2Click(Sender: TObject);
var
  fix: TEasyFix;
  fixPkg: string;
  stockCode: string;
  stockName: string;
begin
  fixPkg := '1=2' + SOH
          + '20=S' + SOH + '10=2' + SOH
          + '101=600000' + SOH + '102=浦发银行' + SOH + '103=16.28' + SOH
          + '101=600005' + SOH + '102=武钢股份' + SOH + '103=3.37' + SOH
          + '20=B' + SOH + '10=2' + SOH 
          + '101=600006' + SOH + '102=东风汽车' + SOH + '103=6.98' + SOH
          + '101=600007' + SOH + '102=中国国贸' + SOH + '103=3.37' + SOH;

  fix := TEasyFix.Create(fixPkg);
  stockCode := fix.GetFixValue(1).GetFixValue(10, 2).GetFixValue(101, 2).AsString;
  stockName := fix.GetFixValue(1).GetFixValue(10, 2).GetFixValue(102, 2).AsString;
  ShowMessage('证券代码:' + stockCode + #13#10 + '证券名称:' + stockName);
  fix.Free;
end;

procedure TForm1.btn3Click(Sender: TObject);
var
  BeginTime: Cardinal;
  Round: Integer;
  fixPkg: string;
  fix: TEasyFix;
  stockCode, stockName: string;
begin
  BeginTime := GetTickCount;
  Round := 0;
  fixPkg := '1=2' + SOH
          + '20=S' + SOH + '10=2' + SOH
          + '101=600000' + SOH + '102=浦发银行' + SOH + '103=16.28' + SOH
          + '101=600005' + SOH + '102=武钢股份' + SOH + '103=3.37' + SOH
          + '20=B' + SOH + '10=2' + SOH 
          + '101=600006' + SOH + '102=东风汽车' + SOH + '103=6.98' + SOH
          + '101=600007' + SOH + '102=中国国贸' + SOH + '103=3.37' + SOH;
  while True do
  begin
    fix := TEasyFix.Create(fixPkg);
    stockCode := fix.GetFixValue(1).GetFixValue(10, 2).GetFixValue(101, 2).AsString;
    stockName := fix.GetFixValue(1).GetFixValue(10, 2).GetFixValue(102, 2).AsString;

    fix.Free();

    Inc(Round);
    if (GetTickCount - BeginTime > 6000) then
    begin
      Break;
    end;
  end;
  ShowMessage('6s解析' + IntToStr(Round) + '条FIX包');
end;

end.

