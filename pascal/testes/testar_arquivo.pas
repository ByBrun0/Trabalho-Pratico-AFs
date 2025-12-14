unit testar_arquivo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, af_utils, testar_terminal;

function TestarViaArquivo(const AF: TAutomato; const CaminhoArquivo: string): TStringList;

implementation

function TestarViaArquivo(const AF: TAutomato; const CaminhoArquivo: string): TStringList;
var
  F: TextFile;
  Linha: string;
  Resultado: boolean;
  Saida: TStringList;
begin
  AssignFile(F, CaminhoArquivo);
  Reset(F);
  Saida := TStringList.Create;
  try
    while not Eof(F) do
    begin
      ReadLn(F, Linha);
      Linha := Trim(Linha);
      if Linha = '' then
        Continue;

      Resultado := AceitaPalavra(AF, Linha);
      Saida.Add(Linha + ': ' + IfThen(Resultado, 'ACEITA', 'REJEITA'));
      WriteLn(Linha, ': ', IfThen(Resultado, 'ACEITA', 'REJEITA'));
    end;
    Result := Saida;
  finally
    CloseFile(F);
  end;
end;

end.
