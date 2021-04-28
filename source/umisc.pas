unit uMisc;
(*
 Various helper functions.
 20210412 rwbl
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Dialogs,
  ComCtrls,            // Statusbar
  Forms,               // Application.MessageBox
  LCLType              // MB_nnnnn
  ;

Procedure SetStatusBarSimpleText(AStatusBar: TStatusBar; AText: String);

procedure ShowInfo(ACaption, AMessage: String);
procedure ShowWarning(ACaption, AMessage: String);
procedure ShowError(ACaption, AMessage: String);

function ByteStringToHex(ABin:String):String;
function ByteStringToInt(ABin:String):Integer;

function HexToInt(AHex:String): Integer;
function IntStrToHex(AIntStr: String): String;

function CharToHexStr(AChar:String): String;
function CharToIntStr(AChar:String): String;
function CharValueToStr(ACharValue:String):String;
function HexToChar(AHex: String):String;

function BoolToStr(ABool: Boolean;ATrue,AFalse:String): String;

procedure Delay(const ms: double);

function StringListDeleteEmptyLines(AList: TStringList): TStringList;

implementation

//
// STATUSBAR
//
Procedure SetStatusBarSimpleText(AStatusBar: TStatusBar; AText: String);
begin
  AStatusBar.SimpleText := AText;
end;

//
// MESSAGES
//

procedure ShowInfo(ACaption, AMessage: String);
begin
  Application.MessageBox(PChar(AMessage), PChar(ACaption), MB_ICONINFORMATION)
end;

procedure ShowWarning(ACaption, AMessage: String);
begin
  Application.MessageBox(PChar(AMessage), PChar(ACaption), MB_ICONWARNING)
end;

procedure ShowError(ACaption, AMessage: String);
begin
  Application.MessageBox(PChar(AMessage), PChar(ACaption), MB_ICONERROR)
end;

//
// CONVERSION
//

(*
  Convert a byte string with 8 bits into a hex string
  Example: 11011111 converted to DF
*)
function ByteStringToHex(ABin:String):String;
var
  IntValue: Integer;
begin
  IntValue := ByteStringToInt(ABin);
  ByteStringToHex := IntToHex(IntValue, 1);
end;

(*
  Convert a byte string with 8 bits into an integer
  Example: 11011111 converted to 223
*)
function ByteStringToInt(ABin:String):Integer;
begin
  ByteStringToInt := Numb2Dec(ABin, 2);
end;


(*
  Convert HEX string to Integer using radix 16.
  Example: "A" converted to 10
*)
function HexToInt(AHex:String): Integer;
begin
  HexToInt := Hex2Dec(AHex);
end;

(*
  Convert an integer defined as string to hex value.
*)
function IntStrToHex(AIntStr: String): String;
var
  IntValue: Integer;
begin
  IntValue := StrToInt(AIntStr);
  IntStrToHex := IntToHex(IntValue, 1);
end;

(*
  Convert char defined as string to hex string with minimal length.
  Be aware: the first char of the string is converted only.
  Example: CharValue := CharToHexStr(CharString);
*)
function CharToHexStr(AChar:String): String;
begin
  CharToHexStr := IntToHex(Ord(AChar[1]), 1);
end;

(*
  Convert char defined as string to int string according ascii table
  Be aware: the first char of the string is converted only.
  Example:
  CharValue := CharToIntStr(CharString);
*)
function CharToIntStr(AChar:String): String;
begin
  CharToIntStr := IntToStr(Ord(AChar[1]));
end;

(*
  Convert a charvalue to a its ascii string repesentation.
  Examples:
  Char value "34" = double quote ".
  Char value "76" = character L.
*)
function CharValueToStr(ACharValue:String):String;
begin
  CharValueToStr := Chr(StrToInt(ACharValue));
end;

(*
  Convert HEX string to a char.
  Example:
  Char = HexToChar("22")  // DoubleQuote "
*)
function HexToChar(AHex: String):String;
begin
  try
    HexToChar := AnsiToUtf8(Chr(HexToInt(AHex)));
  except on E: Exception do
    HexToChar := '*';
  end;
  // HexToChar := Chr(HexToInt(AChar));
end;

(*
  Convert a boolean true|false to string
*)
function BoolToStr(ABool: Boolean;ATrue,AFalse:String): String;
begin
  BoolToStr := ATrue;
  if ABool = False then BoolToStr := AFalse;
end;

(*
  Delay function to handle application process messages
*)
procedure Delay(const ms: double);
begin
  repeat
    application.ProcessMessages;
  until now > ms;
end;

(*
  StringLists
*)
function StringListDeleteEmptyLines(AList: TStringList): TStringList;
var
  i: Integer;
begin
  for i := AList.Count - 1 downto 0 do begin
    if Trim(AList[i]) = '' then AList.Delete(i);
    end;
  StringListDeleteEmptyLines := AList;
end;

end.

