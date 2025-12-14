unit af_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStringArray = array of string;

  TTransicao = record
    Origem: string;
    Destino: string;
    Simbolo: string;
  end;

  TTransicoes = array of TTransicao;

  TAutomato = record
    Alfabeto: TStringArray;
    Estados: TStringArray;
    EstadosIniciais: TStringArray;
    EstadosFinais: TStringArray;
    Transicoes: TTransicoes;
  end;

const
  LAMBDA = '&';

function StringArrayToList(const Arr: TStringArray): TStringList;
function ListToStringArray(const L: TStringList): TStringArray;
function NormalizarNomeEstado(const Estados: TStringList): string;
function ObterTransicoes(const Automato: TAutomato; const Estado, Simbolo: string): TStringList;
function FechoLambda(const Automato: TAutomato; const Estados: TStringList): TStringList;
function Mover(const Automato: TAutomato; const Estados: TStringList; const Simbolo: string): TStringList;
function EhAFD(const Automato: TAutomato): boolean;
function EhAFNLambda(const Automato: TAutomato): boolean;
function EstadosFinaisCompostos(const EstadosCompostos: TStringList; const EstadosFinais: TStringList): boolean;

implementation

function StringArrayToList(const Arr: TStringArray): TStringList;
var
  L: TStringList;
  S: string;
begin
  Result := nil;
  L := TStringList.Create;
  L.Sorted := True;
  L.Duplicates := dupIgnore;
  for S in Arr do
    L.Add(S);
  Result := L;
end;

function ListToStringArray(const L: TStringList): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  SetLength(Result, L.Count);
  for I := 0 to L.Count - 1 do
    Result[I] := L[I];
end;

function CloneList(const L: TStringList): TStringList;
begin
  Result := nil;
  Result := TStringList.Create;
  Result.Sorted := L.Sorted;
  Result.Duplicates := L.Duplicates;
  Result.Assign(L);
end;

function NormalizarNomeEstado(const Estados: TStringList): string;
var
  Temp: TStringList;
  I: Integer;
begin
  Temp := CloneList(Estados);
  try
    Temp.Sort;
    Result := '{';
    for I := 0 to Temp.Count - 1 do
    begin
      Result := Result + Temp[I];
      if I < Temp.Count - 1 then
        Result := Result + ',';
    end;
    Result := Result + '}';
  finally
    Temp.Free;
  end;
end;

function ObterTransicoes(const Automato: TAutomato; const Estado, Simbolo: string): TStringList;
var
  Destinos: TStringList;
  T: TTransicao;
begin
  Result := nil;
  Destinos := TStringList.Create;
  Destinos.Sorted := True;
  Destinos.Duplicates := dupIgnore;
  for T in Automato.Transicoes do
    if (T.Origem = Estado) and (T.Simbolo = Simbolo) then
      Destinos.Add(T.Destino);
  Result := Destinos;
end;

function FechoLambda(const Automato: TAutomato; const Estados: TStringList): TStringList;
var
  Fecho, Pilha, Destinos: TStringList;
  EstadoAtual, Prox: string;
begin
  Result := nil;
  Fecho := CloneList(Estados);
  Pilha := CloneList(Estados);
  try
    while Pilha.Count > 0 do
    begin
      EstadoAtual := Pilha[Pilha.Count - 1];
      Pilha.Delete(Pilha.Count - 1);

      Destinos := ObterTransicoes(Automato, EstadoAtual, LAMBDA);
      try
        for Prox in Destinos do
          if Fecho.IndexOf(Prox) = -1 then
          begin
            Fecho.Add(Prox);
            Pilha.Add(Prox);
          end;
      finally
        Destinos.Free;
      end;
    end;
    Result := Fecho;
  finally
    Pilha.Free;
  end;
end;

function Mover(const Automato: TAutomato; const Estados: TStringList; const Simbolo: string): TStringList;
var
  Resultado, Destinos: TStringList;
  Estado, Prox: string;
begin
  Result := nil;
  Resultado := TStringList.Create;
  Resultado.Sorted := True;
  Resultado.Duplicates := dupIgnore;
  for Estado in Estados do
  begin
    Destinos := ObterTransicoes(Automato, Estado, Simbolo);
    try
      for Prox in Destinos do
        Resultado.Add(Prox);
    finally
      Destinos.Free;
    end;
  end;
  Result := Resultado;
end;

function EhAFD(const Automato: TAutomato): boolean;
var
  Estado, Simbolo: string;
  Destinos: TStringList;
begin
  for Simbolo in Automato.Alfabeto do
    if Simbolo = LAMBDA then
      Exit(False);

  for Estado in Automato.Estados do
  begin
    for Simbolo in Automato.Alfabeto do
    begin
      Destinos := ObterTransicoes(Automato, Estado, Simbolo);
      try
        if Destinos.Count <> 1 then
          Exit(False);
      finally
        Destinos.Free;
      end;
    end;
  end;
  Result := True;
end;

function EhAFNLambda(const Automato: TAutomato): boolean;
var
  T: TTransicao;
begin
  for T in Automato.Transicoes do
    if T.Simbolo = LAMBDA then
      Exit(True);
  Result := False;
end;

function EstadosFinaisCompostos(const EstadosCompostos: TStringList; const EstadosFinais: TStringList): boolean;
var
  Estado: string;
begin
  for Estado in EstadosCompostos do
    if EstadosFinais.IndexOf(Estado) <> -1 then
      Exit(True);
  Result := False;
end;

end.
