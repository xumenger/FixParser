unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FlexFix;

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
  fix: TFlexFix;
begin
  fix := TFlexFix.Create;
  fix.AddValue(10, 100, 4);
  fix.AddValue(100, '600570', 8);

  ShowMessage(fix.GetBuffer);
  fix.Free;
end;

procedure TForm1.btn2Click(Sender: TObject);
var
  fix: TFlexFix;
  fixPkg: string;
  stockCode: string;
  stockName: string;
  ShowMsg: string;
begin
  fixPkg := '9=62635=U0261346=716=3146=3'
          +    '6133=1279=040=F44=10.003226=18847=164=20170104541=20170105193=2017010554=1'
          +       '711=148=125201308=38=10231=1.128504=100000.89879=0159=100.23119=100010.3432=100000'
          +       '529=1125=19=0'
          +       '453=3448=004452=12448=融通基金1  452=103448=Z00401452=10258=test1'
          +    '6133=1279=240=F44=0226=08847=064=541=193=54= 529=1125=19=058='
          +    '6133=2279=040=F44=15.000226=28847=264=20170104541=20170106193=2017010654=1'
          +       '711=148=125201308=38=5231=0.988504=48000.89879=0159=89.23119=48089.3432=50000'
          +       '529=1125=19=0'
          +       '453=3448=004452=12448=融通基金3  452=103448=Z00403452=10258=test2';

  fix := TFlexFix.Create(fixPkg);
  
  stockName := fix.GetFixValue(6133, 3).GetFixValue(448, 2).AsString;
  stockCode := fix.GetFixValue(6133, 3).GetFixValue(448, 3).AsString;
  ShowMsg := '名称3：' + stockName + #13#10 + '代码3：' + stockCode + #13#10;
  if (nil = fix.GetFixValue(6133, 2).GetFixValue(448, 2)) then
  begin
    ShowMsg := ShowMsg + '名称2为NIL'
  end
  else
  begin
    ShowMsg := ShowMsg + '名称2不为NIL，其值为：' + fix.GetFixValue(6133, 2).GetFixValue(448, 2).AsString;
  end;
  
  ShowMessage(ShowMsg);
  fix.Free;
end;

procedure TForm1.btn3Click(Sender: TObject);
var
  BeginTime: Cardinal;
  Round: Integer;
  fixPkg: string;
  fix: TFlexFix;
  stockCode, stockName: string;
begin
  BeginTime := GetTickCount;
  Round := 0;
    fixPkg := '9=62435=U0261346=716=3146=3'
          +    '6133=1279=040=F44=10.003226=18847=164=20170104541=20170105193=2017010554=1'
          +       '711=148=125201308=38=10231=1.128504=100000.89879=0159=100.23119=100010.3432=100000'
          +       '529=1125=19=0'
          +       '453=3448=004452=12448=融通基金  452=103448=Z00401452=10258=test1'
          +    '6133=1279=240=F44=0226=08847=064=541=193=54= 529=1125=19=058='
          +    '6133=2279=040=F44=15.000226=28847=264=20170104541=20170106193=2017010654=1'
          +       '711=148=125201308=38=5231=0.988504=48000.89879=0159=89.23119=48089.3432=50000'
          +       '529=1125=19=0'
          +       '453=3448=004452=12448=融通基金  452=103448=Z00401452=10258=test2';
          
  while True do
  begin
    fix := TFlexFix.Create(fixPkg);
    stockName := fix.GetFixValue(6133, 3).GetFixValue(448, 2).AsString;
    stockCode := fix.GetFixValue(6133, 3).GetFixValue(448, 3).AsString;

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

