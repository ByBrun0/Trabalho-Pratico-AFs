unit minimizacao_afd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, af_utils;

type
  TStringListArray = array of TStringList;

function Minimizar(const Automato: TAutomato): TAutomato;

implementation

function Delta(const Automato: TAutomato; const Estado, Simbolo: string): string;
var
  T: TTransicao;
begin
  for T in Automato.Transicoes do
    if (T.Origem = Estado) and (T.Simbolo = Simbolo) then
      Exit(T.Destino);
  Result := '';
end;

function EncontrarGrupo(const Particoes: TStringListArray; const Estado: string): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(Particoes) - 1 do
    if Particoes[I].IndexOf(Estado) <> -1 then
      Exit(I);
  Result := -1;
end;

function CopiarParticoes(const Fonte: TStringListArray): TStringListArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Fonte));
  for I := 0 to Length(Fonte) - 1 do
  begin
    Result[I] := TStringList.Create;
    Result[I].Sorted := False;
    Result[I].Duplicates := dupIgnore;
    Result[I].Assign(Fonte[I]);
  end;
end;

function Minimizar(const Automato: TAutomato): TAutomato;
var
  Alfabeto: TStringArray;
  Estados, Finais, Iniciais: TStringList;
  Transicoes: TTransicoes;
  P: TStringListArray;
  Mudou: boolean;
  NovaP: TStringListArray;
  Grupo, SubGrupo: TStringList;
  I, J, K: Integer;
  Estado: string;
  Assinatura: string;
  Assinaturas: TStringList;
  Indice: Integer;
  MapaRep: TStringList;
  ListaReps: TStringList;
  NovoEstado: string;
  NovasTransicoes: TTransicoes;
  Destino: string;
  NovoNome: string;
  DestinoRep: string;
  ParticaoIdx: Integer;
  NovosFinais, NovosIniciais: TStringList;
begin
  Alfabeto := Automato.Alfabeto;
  Estados := StringArrayToList(Automato.Estados);
  Finais := StringArrayToList(Automato.EstadosFinais);
  Iniciais := StringArrayToList(Automato.EstadosIniciais);
  Transicoes := Automato.Transicoes;

  SetLength(P, 0);
  if Finais.Count > 0 then
  begin
    SetLength(P, Length(P) + 1);
    P[High(P)] := TStringList.Create;
    P[High(P)].Assign(Finais);
  end;

  if Estados.Count - Finais.Count > 0 then
  begin
    SetLength(P, Length(P) + 1);
    P[High(P)] := TStringList.Create;
    P[High(P)].Sorted := False;
    P[High(P)].Duplicates := dupIgnore;
    for Estado in Estados do
      if Finais.IndexOf(Estado) = -1 then
        P[High(P)].Add(Estado);
  end;

  Mudou := True;
  while Mudou do
  begin
    Mudou := False;
    SetLength(NovaP, 0);

    for Grupo in P do
    begin
      Assinaturas := TStringList.Create;
      Assinaturas.Sorted := False;
      Assinaturas.Duplicates := dupIgnore;

      for Estado in Grupo do
      begin
        Assinatura := '';
        for I := 0 to Length(Alfabeto) - 1 do
        begin
          Destino := Delta(Automato, Estado, Alfabeto[I]);
          ParticaoIdx := EncontrarGrupo(P, Destino);
          Assinatura := Assinatura + IntToStr(ParticaoIdx) + '|';
        end;

        Indice := Assinaturas.IndexOf(Assinatura);
        if Indice = -1 then
        begin
          SetLength(NovaP, Length(NovaP) + 1);
          NovaP[High(NovaP)] := TStringList.Create;
          NovaP[High(NovaP)].Sorted := False;
          NovaP[High(NovaP)].Duplicates := dupIgnore;
          NovaP[High(NovaP)].Add(Estado);
          Assinaturas.AddObject(Assinatura, NovaP[High(NovaP)]);
        end
        else
          TStringList(Assinaturas.Objects[Indice]).Add(Estado);
      end;

      if Assinaturas.Count = 1 then
      begin
        SetLength(NovaP, Length(NovaP) + 1);
        NovaP[High(NovaP)] := TStringList.Create;
        NovaP[High(NovaP)].Assign(Grupo);
      end;

      Assinaturas.Free;
    end;

    if Length(NovaP) <> Length(P) then
      Mudou := True
    else
    begin
      for I := 0 to Length(P) - 1 do
        if not P[I].Equals(NovaP[I]) then
        begin
          Mudou := True;
          Break;
        end;
    end;

    for Grupo in P do
      Grupo.Free;
    P := CopiarParticoes(NovaP);
    for Grupo in NovaP do
      Grupo.Free;
  end;

  MapaRep := TStringList.Create;
  MapaRep.Sorted := False;
  MapaRep.Duplicates := dupIgnore;

  ListaReps := TStringList.Create;
  ListaReps.Sorted := True;
  ListaReps.Duplicates := dupIgnore;

  for I := 0 to Length(P) - 1 do
  begin
    if P[I].Count > 0 then
    begin
      NovoNome := P[I][0];
      for Estado in P[I] do
        MapaRep.Add(Estado + '=' + NovoNome);
      ListaReps.Add(NovoNome);
    end;
  end;

  SetLength(NovasTransicoes, 0);
  for Estado in Estados do
  begin
    NovoEstado := MapaRep.Values[Estado];
    for I := 0 to Length(Alfabeto) - 1 do
    begin
      Destino := Delta(Automato, Estado, Alfabeto[I]);
      DestinoRep := MapaRep.Values[Destino];

      SetLength(NovasTransicoes, Length(NovasTransicoes) + 1);
      NovasTransicoes[High(NovasTransicoes)].Origem := NovoEstado;
      NovasTransicoes[High(NovasTransicoes)].Destino := DestinoRep;
      NovasTransicoes[High(NovasTransicoes)].Simbolo := Alfabeto[I];
    end;
  end;

  NovosFinais := TStringList.Create;
  NovosFinais.Sorted := True;
  NovosFinais.Duplicates := dupIgnore;
  for Estado in Automato.EstadosFinais do
    NovosFinais.Add(MapaRep.Values[Estado]);

  NovosIniciais := TStringList.Create;
  NovosIniciais.Sorted := False;
  NovosIniciais.Duplicates := dupIgnore;
  NovosIniciais.Add(MapaRep.Values[Iniciais[0]]);

  Result.Alfabeto := Alfabeto;
  Result.Estados := ListToStringArray(ListaReps);
  Result.EstadosIniciais := ListToStringArray(NovosIniciais);
  Result.EstadosFinais := ListToStringArray(NovosFinais);
  Result.Transicoes := NovasTransicoes;

  Estados.Free;
  Finais.Free;
  Iniciais.Free;
  MapaRep.Free;
  ListaReps.Free;
  NovosFinais.Free;
  NovosIniciais.Free;
  for Grupo in P do
    Grupo.Free;
end;

end.
