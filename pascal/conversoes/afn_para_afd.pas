unit afn_para_afd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, af_utils;

function Converter(const Automato: TAutomato): TAutomato;

implementation

function ConjuntoParaNome(const Conjunto: TStringList): string;
begin
  Result := NormalizarNomeEstado(Conjunto);
end;

function ClonarConjunto(const Fonte: TStringList): TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  Result.Assign(Fonte);
end;

function ConjuntoExiste(const Lista: array of TStringList; const Cmp: TStringList): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(Lista) - 1 do
    if Lista[I].Equals(Cmp) then
      Exit(I);
  Result := -1;
end;

function Converter(const Automato: TAutomato): TAutomato;
var
  Alfabeto: TStringArray;
  EstadosAFN, EstadosFinaisAFN: TStringList;
  EstadoInicialAFN: TStringList;
  EstadosBrutos: TStringList;
  Visitados: array of TStringList;
  Fila: array of Integer;
  FilaInicio, FilaFim: Integer;
  TransicoesBrutas: TTransicoes;
  EstadoAtualIdx: Integer;
  EstadoAtualNome, NomeDestino: string;
  Simbolo: string;
  Destino: TStringList;
  MapaNomes: TStringList;
  NovosEstados, NovosIniciais, NovosFinais: TStringList;
  I, Posicao: Integer;
  NovoNome: string;
  Prefixo: string;
  NovaTrans: TTransicao;
begin
  Alfabeto := Automato.Alfabeto;

  EstadosAFN := StringArrayToList(Automato.Estados);
  EstadosFinaisAFN := StringArrayToList(Automato.EstadosFinais);

  EstadoInicialAFN := StringArrayToList(Automato.EstadosIniciais);

  EstadosBrutos := TStringList.Create;
  EstadosBrutos.Sorted := False;
  EstadosBrutos.Duplicates := dupIgnore;

  SetLength(Visitados, 1);
  Visitados[0] := ClonarConjunto(EstadoInicialAFN);
  EstadosBrutos.Add(ConjuntoParaNome(Visitados[0]));

  SetLength(Fila, Length(Fila) + 1);
  Fila[0] := 0;
  FilaInicio := 0;
  FilaFim := 1;

  SetLength(TransicoesBrutas, 0);

  while FilaInicio < FilaFim do
  begin
    EstadoAtualIdx := Fila[FilaInicio];
    Inc(FilaInicio);
    EstadoAtualNome := ConjuntoParaNome(Visitados[EstadoAtualIdx]);

    for Simbolo in Alfabeto do
    begin
      Destino := Mover(Automato, Visitados[EstadoAtualIdx], Simbolo);
      NomeDestino := ConjuntoParaNome(Destino);

      SetLength(TransicoesBrutas, Length(TransicoesBrutas) + 1);
      TransicoesBrutas[High(TransicoesBrutas)].Origem := EstadoAtualNome;
      TransicoesBrutas[High(TransicoesBrutas)].Destino := NomeDestino;
      TransicoesBrutas[High(TransicoesBrutas)].Simbolo := Simbolo;

      Posicao := ConjuntoExiste(Visitados, Destino);
      if Posicao = -1 then
      begin
        SetLength(Visitados, Length(Visitados) + 1);
        Visitados[High(Visitados)] := Destino;
        EstadosBrutos.Add(NomeDestino);

        SetLength(Fila, Length(Fila) + 1);
        Fila[FilaFim] := High(Visitados);
        Inc(FilaFim);
      end
      else
        Destino.Free;
    end;
  end;

  MapaNomes := TStringList.Create;
  MapaNomes.Sorted := False;
  MapaNomes.Duplicates := dupIgnore;

  NovosEstados := TStringList.Create;
  NovosEstados.Sorted := False;
  NovosEstados.Duplicates := dupIgnore;

  NovosIniciais := TStringList.Create;
  NovosIniciais.Sorted := False;
  NovosIniciais.Duplicates := dupIgnore;

  NovosFinais := TStringList.Create;
  NovosFinais.Sorted := False;
  NovosFinais.Duplicates := dupIgnore;

  Prefixo := 'S';
  for I := 0 to EstadosBrutos.Count - 1 do
  begin
    if EstadosBrutos[I] = '{}' then
      NovoNome := 'q_erro'
    else
      NovoNome := Prefixo + IntToStr(I);

    MapaNomes.Add(EstadosBrutos[I] + '=' + NovoNome);
    NovosEstados.Add(NovoNome);
  end;

  for I := 0 to Length(TransicoesBrutas) - 1 do
  begin
    NovoNome := MapaNomes.Values[TransicoesBrutas[I].Origem];
    if NovoNome = '' then
      NovoNome := TransicoesBrutas[I].Origem;
    TransicoesBrutas[I].Origem := NovoNome;

    NovoNome := MapaNomes.Values[TransicoesBrutas[I].Destino];
    if NovoNome = '' then
      NovoNome := TransicoesBrutas[I].Destino;
    TransicoesBrutas[I].Destino := NovoNome;
  end;

  for I := 0 to EstadosBrutos.Count - 1 do
  begin
    if I = 0 then
      NovosIniciais.Add(MapaNomes.Values[EstadosBrutos[I]]);

    if EstadosFinaisCompostos(Visitados[I], EstadosFinaisAFN) then
      NovosFinais.Add(MapaNomes.Values[EstadosBrutos[I]]);
  end;

  Result.Alfabeto := Alfabeto;
  Result.Estados := ListToStringArray(NovosEstados);
  Result.EstadosIniciais := ListToStringArray(NovosIniciais);
  Result.EstadosFinais := ListToStringArray(NovosFinais);
  Result.Transicoes := TransicoesBrutas;

  EstadosAFN.Free;
  EstadosFinaisAFN.Free;
  EstadoInicialAFN.Free;
  EstadosBrutos.Free;
  for I := 0 to Length(Visitados) - 1 do
    Visitados[I].Free;
  MapaNomes.Free;
  NovosEstados.Free;
  NovosIniciais.Free;
  NovosFinais.Free;
end;

end.
