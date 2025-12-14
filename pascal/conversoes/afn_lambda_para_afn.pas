unit afn_lambda_para_afn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, af_utils;

function Converter(const Automato: TAutomato): TAutomato;

implementation

function Converter(const Automato: TAutomato): TAutomato;
var
  NovoAlfabeto: TStringArray;
  Estados: TStringArray;
  EstadosFinaisOrig: TStringList;
  NovasTransicoes: TTransicoes;
  Estado: string;
  FechoQ, Destinos, MoveResult, FechoP, TempList: TStringList;
  Simbolo: string;
  I, J: Integer;
  NovosFinais: TStringList;
begin
  SetLength(NovoAlfabeto, 0);
  for Estado in Automato.Alfabeto do
    if Estado <> LAMBDA then
    begin
      SetLength(NovoAlfabeto, Length(NovoAlfabeto) + 1);
      NovoAlfabeto[High(NovoAlfabeto)] := Estado;
    end;

  Estados := Automato.Estados;
  EstadosFinaisOrig := StringArrayToList(Automato.EstadosFinais);
  NovosFinais := TStringList.Create;
  NovosFinais.Sorted := True;
  NovosFinais.Duplicates := dupIgnore;

  SetLength(NovasTransicoes, 0);

  for I := 0 to Length(Estados) - 1 do
  begin
    FechoQ := TStringList.Create;
    FechoQ.Sorted := True;
    FechoQ.Duplicates := dupIgnore;
    FechoQ.Add(Estados[I]);
    TempList := FechoLambda(Automato, FechoQ);
    FechoQ.Free;
    FechoQ := TempList;
    try
      for Simbolo in NovoAlfabeto do
      begin
        MoveResult := Mover(Automato, FechoQ, Simbolo);
        Destinos := TStringList.Create;
        Destinos.Sorted := True;
        Destinos.Duplicates := dupIgnore;
        try
          for J := 0 to MoveResult.Count - 1 do
          begin
            FechoP := TStringList.Create;
            FechoP.Sorted := True;
            FechoP.Duplicates := dupIgnore;
            FechoP.Add(MoveResult[J]);
            TempList := FechoLambda(Automato, FechoP);
            FechoP.Free;
            FechoP := TempList;
            try
              Destinos.AddStrings(FechoP);
            finally
              FechoP.Free;
            end;
          end;

          for J := 0 to Destinos.Count - 1 do
          begin
            SetLength(NovasTransicoes, Length(NovasTransicoes) + 1);
            NovasTransicoes[High(NovasTransicoes)].Origem := Estados[I];
            NovasTransicoes[High(NovasTransicoes)].Destino := Destinos[J];
            NovasTransicoes[High(NovasTransicoes)].Simbolo := Simbolo;
          end;
        finally
          Destinos.Free;
          MoveResult.Free;
        end;
      end;

      if EstadosFinaisCompostos(FechoQ, EstadosFinaisOrig) then
        NovosFinais.Add(Estados[I]);
    finally
      FechoQ.Free;
    end;
  end;

  Result.Alfabeto := NovoAlfabeto;
  Result.Estados := Estados;
  Result.EstadosIniciais := Automato.EstadosIniciais;
  Result.EstadosFinais := ListToStringArray(NovosFinais);
  Result.Transicoes := NovasTransicoes;
  NovosFinais.Free;
  EstadosFinaisOrig.Free;
end;

end.
