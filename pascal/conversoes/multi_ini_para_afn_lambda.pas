unit multi_ini_para_afn_lambda;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, af_utils;

function Converter(const Automato: TAutomato): TAutomato;

implementation

function Converter(const Automato: TAutomato): TAutomato;
var
  EstadosIniciais: TStringArray;
  Estados, Alfabeto, Finais: TStringArray;
  Transicoes: TTransicoes;
  NovoEstadoInicial: string;
  I, Count: Integer;
  L: TStringList;
begin
  EstadosIniciais := Automato.EstadosIniciais;
  if Length(EstadosIniciais) <= 1 then
    Exit(Automato);

  Estados := Automato.Estados;
  Alfabeto := Automato.Alfabeto;
  Finais := Automato.EstadosFinais;
  Transicoes := Automato.Transicoes;

  NovoEstadoInicial := 'q_ini';
  Count := 0;
  while True do
  begin
    Count := 0;
    for I := 0 to Length(Estados) - 1 do
      if Estados[I] = NovoEstadoInicial then
        Inc(Count);

    if Count = 0 then
      Break;

    NovoEstadoInicial := 'q_ini' + IntToStr(Count);
  end;

  SetLength(Estados, Length(Estados) + 1);
  Estados[High(Estados)] := NovoEstadoInicial;

  L := StringArrayToList(Alfabeto);
  try
    if L.IndexOf(LAMBDA) = -1 then
    begin
      SetLength(Alfabeto, Length(Alfabeto) + 1);
      Alfabeto[High(Alfabeto)] := LAMBDA;
    end;
  finally
    L.Free;
  end;

  Count := Length(Transicoes);
  SetLength(Transicoes, Count + Length(EstadosIniciais));
  for I := 0 to Length(EstadosIniciais) - 1 do
  begin
    Transicoes[Count + I].Origem := NovoEstadoInicial;
    Transicoes[Count + I].Destino := EstadosIniciais[I];
    Transicoes[Count + I].Simbolo := LAMBDA;
  end;

  Result.Alfabeto := Alfabeto;
  Result.Estados := Estados;
  SetLength(Result.EstadosIniciais, 1);
  Result.EstadosIniciais[0] := NovoEstadoInicial;
  Result.EstadosFinais := Finais;
  Result.Transicoes := Transicoes;
end;

end.
