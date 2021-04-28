unit uLCDTableChars;
(*

*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  ActnList, ClipBrd,
  uMisc;

type

  { TFormLCDTableChars }

  TFormLCDTableChars = class(TForm)
    ActionConvertBits: TAction;
    ActionList1: TActionList;
    ImageTableChars: TImage;
    ToolBarForm: TToolBar;
    ToolButtonConvertBits: TToolButton;
    procedure ActionConvertBitsExecute(Sender: TObject);
  private

  public

  end;

var
  FormLCDTableChars: TFormLCDTableChars;

implementation

{$R *.lfm}

{ TFormLCDTableChars }

(*
  Convert Table Char Bits to HEX,DEC,AscII values.
  Examples:
  Degree:
  col 1101 (higher 4 bit), row 1111 (lower 4 bit)
  bits 1101111 converted to HEX DF, DEC 223, AscII no value because DEC value > 127.
  DoubleQuotes:
  col 0010 (higher 4 bit), row 0010 (lower 4 bit)
  bits 00100010 converted to HEX 22, DEC 4, AscII "
  ArrowLeft:
  col 0111 (higher 4 bit), row 1111 (lower 4 bit)7
  bits 01111111 converted to HEX 7F, DEC 127, AscII "
*)
procedure TFormLCDTableChars.ActionConvertBitsExecute(Sender: TObject);
var
  BitStr: String;
  HexValue: String;
  IntValue: Integer;
  AscIIValue: String;
  Result: String;
begin
  BitStr := '11011111'; // Degree
  if InputQuery('Convert Table Char Bits', 'Enter Bit String (Higher 4 bit Lower 4 bit)', False, BitStr) then begin
   if Length(BitStr) = 8 then begin
    HexValue := ByteStringToHex(BitStr);
    IntValue := ByteStringToInt(BitStr);
    AscIIValue := HexToChar(HexValue);
    Result := Format(
      'Bits: %s%sDEC: %d%sHEX: %s%sASCII: %s',
      [BitStr,LineEnding,IntValue,LineEnding,HexValue,LineEnding,AscIIValue]);
   ClipBoard.AsText := Result;
   ShowInfo(
     Format('Convert Table Char Bits',[]),
     Format('%s',[Result]));
   end
   else begin
     ShowError(
       Format('Convert Table Char Bits',[]),
       Format('The Bit string "%s" must contain 8 Bits.',[BitStr]));
   end;
  end;
end;

end.

