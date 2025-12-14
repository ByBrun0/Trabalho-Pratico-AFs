unit io_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, af_utils;

function LerAutomatoJson(const Caminho: string): TAutomato;
procedure SalvarAutomatoJson(const Automato: TAutomato; const Caminho: string);
procedure ImprimirAutomato(const Automato: TAutomato);

implementation

function JSONToStringArray(Data: TJSONData): TStringArray;
var
  Arr: TJSONArray;
  I: Integer;
begin
  Result := nil;
  SetLength(Result, 0);
  if Data = nil then
    Exit;
  Arr := TJSONArray(Data);
  SetLength(Result, Arr.Count);
  for I := 0 to Arr.Count - 1 do
    Result[I] := Arr.Strings[I];
end;

function StringArrayToJSON(const Arr: TStringArray): TJSONArray;
var
  JArr: TJSONArray;
  S: string;
begin
  JArr := TJSONArray.Create;
  for S in Arr do
    JArr.Add(S);
  Result := JArr;
end;

function JSONToTransicoes(Data: TJSONData): TTransicoes;
var
  Arr: TJSONArray;
  I: Integer;
  T: TJSONArray;
begin
  Result := nil;
  SetLength(Result, 0);
  if Data = nil then
    Exit;
  Arr := TJSONArray(Data);
  SetLength(Result, Arr.Count);
  for I := 0 to Arr.Count - 1 do
  begin
    T := Arr.Arrays[I];
    Result[I].Origem := T.Strings[0];
    Result[I].Destino := T.Strings[1];
    Result[I].Simbolo := T.Strings[2];
  end;
end;

function TransicoesToJSON(const Transicoes: TTransicoes): TJSONArray;
var
  JArr: TJSONArray;
  T: TTransicao;
  Item: TJSONArray;
begin
  JArr := TJSONArray.Create;
  for T in Transicoes do
  begin
    Item := TJSONArray.Create;
    Item.Add(T.Origem);
    Item.Add(T.Destino);
    Item.Add(T.Simbolo);
    JArr.Add(Item);
  end;
  Result := JArr;
end;

function LerAutomatoJson(const Caminho: string): TAutomato;
var
  Parser: TJSONParser;
  Data: TJSONData;
  FS: TFileStream;
begin
  FS := TFileStream.Create(Caminho, fmOpenRead or fmShareDenyWrite);
  try
    Parser := TJSONParser.Create(FS, []);
    try
      Data := Parser.Parse;
      try
        Result.Alfabeto := JSONToStringArray(Data.FindPath('alfabeto'));
        Result.Estados := JSONToStringArray(Data.FindPath('estados'));
        Result.EstadosIniciais := JSONToStringArray(Data.FindPath('estados_iniciais'));
        Result.EstadosFinais := JSONToStringArray(Data.FindPath('estados_finais'));
        Result.Transicoes := JSONToTransicoes(Data.FindPath('transicoes'));
      finally
        Data.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    FS.Free;
  end;
end;

procedure SalvarAutomatoJson(const Automato: TAutomato; const Caminho: string);
var
  Obj: TJSONObject;
  FS: TFileStream;
  S: string;
  Bytes: TBytes;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('alfabeto', StringArrayToJSON(Automato.Alfabeto));
    Obj.Add('estados', StringArrayToJSON(Automato.Estados));
    Obj.Add('estados_iniciais', StringArrayToJSON(Automato.EstadosIniciais));
    Obj.Add('estados_finais', StringArrayToJSON(Automato.EstadosFinais));
    Obj.Add('transicoes', TransicoesToJSON(Automato.Transicoes));

    S := Obj.FormatJSON([], 4);
    Bytes := BytesOf(S);

    FS := TFileStream.Create(Caminho, fmCreate);
    try
      FS.Write(Bytes[0], Length(Bytes));
    finally
      FS.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure ImprimirAutomato(const Automato: TAutomato);
var
  T: TTransicao;

  function ArrayToString(const Arr: TStringArray): string;
  var
    I: Integer;
  begin
    Result := '[';
    for I := 0 to Length(Arr) - 1 do
    begin
      Result := Result + Arr[I];
      if I < Length(Arr) - 1 then
        Result := Result + ', ';
    end;
    Result := Result + ']';
  end;

begin
  WriteLn('\n=== AUTÔMATO ===');
  WriteLn('Alfabeto: ', ArrayToString(Automato.Alfabeto));
  WriteLn('Estados: ', ArrayToString(Automato.Estados));
  WriteLn('Estados iniciais: ', ArrayToString(Automato.EstadosIniciais));
  WriteLn('Estados finais: ', ArrayToString(Automato.EstadosFinais));
  WriteLn('Transições:');
  for T in Automato.Transicoes do
    WriteLn('  δ(', T.Origem, ', ', T.Simbolo, ') -> ', T.Destino);
  WriteLn('=================');
end;

end.
