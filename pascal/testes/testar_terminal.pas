unit testar_terminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, af_utils;

function AceitaPalavra(const AF: TAutomato; const Palavra: string): boolean;
procedure TestarViaTerminal(const AF: TAutomato);

implementation

function AceitaPalavra(const AF: TAutomato; const Palavra: string): boolean;
var
  EstadosIniciais, EstadosAtuais, Proximos: TStringList;
  Simbolo: char;
  EstadosFinais: TStringList;
begin
  EstadosIniciais := StringArrayToList(AF.EstadosIniciais);
  EstadosAtuais := FechoLambda(AF, EstadosIniciais);
  EstadosIniciais.Free;
  EstadosFinais := StringArrayToList(AF.EstadosFinais);
  try
    for Simbolo in Palavra do
    begin
      Proximos := Mover(AF, EstadosAtuais, string(Simbolo));
      EstadosAtuais.Free;
      EstadosAtuais := FechoLambda(AF, Proximos);
      Proximos.Free;

      if EstadosAtuais.Count = 0 then
        Break;
    end;

    Result := EstadosFinaisCompostos(EstadosAtuais, EstadosFinais);
  finally
    EstadosAtuais.Free;
    EstadosFinais.Free;
  end;
end;

procedure TestarViaTerminal(const AF: TAutomato);
var
  Entrada, Display: string;
begin
  WriteLn('\n--- Teste Interativo ---');
  WriteLn('Digite a palavra para testar.');
  WriteLn('Para testar a Palavra Vazia (epsilon), apenas aperte ENTER.');
  WriteLn('Para encerrar, digite: SAIR');

  while True do
  begin
    Write('> ');
    ReadLn(Entrada);

    if UpperCase(Trim(Entrada)) = 'SAIR' then
      Break;

    if AceitaPalavra(AF, Entrada) then
      Display := 'ACEITA'
    else
      Display := 'REJEITA';

    if Entrada = '' then
      WriteLn('[vazia]: ', Display)
    else
      WriteLn('''', Entrada, '''', ': ', Display);
  end;
end;

end.
