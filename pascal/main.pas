program trabalho_afs;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, StrUtils,
  af_utils, io_utils,
  multi_ini_para_afn_lambda,
  afn_lambda_para_afn,
  afn_para_afd,
  minimizacao_afd,
  testar_terminal,
  testar_arquivo;

const
  BASE_DIR = '..' + DirectorySeparator;
  EXEMPLOS_DIR = BASE_DIR + 'exemplos' + DirectorySeparator;
  RESULTADOS_DIR = BASE_DIR + 'resultados' + DirectorySeparator;
  PALAVRAS = EXEMPLOS_DIR + 'palavras.txt';
  AF_PADRAO = EXEMPLOS_DIR + 'af.json';
  AFN_LAMBDA_PADRAO = EXEMPLOS_DIR + 'afn_lambda.json';

procedure Menu;
begin
  WriteLn('\n===== TRABALHO LFA — AUTÔMATOS FINITOS =====');
  WriteLn('0 - Converter multiestado inicial em AFN-&');
  WriteLn('1 - Converter AFN-& em AFN');
  WriteLn('2 - Converter AFN em AFD');
  WriteLn('3 - Minimizar AFD');
  WriteLn('4 - Testar Palavras');
  WriteLn('5 - Carregar/Trocar Arquivo (Reiniciar)');
  WriteLn('6 - Sair');
end;

procedure SubmenuTestes;
begin
  WriteLn('\n--- Testar Palavras ---');
  WriteLn('1 - Testar palavras via terminal');
  WriteLn('2 - Testar palavras via arquivo');
  WriteLn('0 - Voltar');
end;

function CarregarAutomato(const Caminho: string): TAutomato;
begin
  Result := LerAutomatoJson(Caminho);
  WriteLn('\nAutômato carregado com sucesso (', ExtractFileName(Caminho), ')!');
  ImprimirAutomato(Result);
end;

function ListarEEscolherArquivo: TAutomato;
var
  SR: TSearchRec;
  Arquivos: TStringList;
  I, Escolha: Integer;
  Caminho: string;
begin
  Result.Alfabeto := nil;
  Arquivos := TStringList.Create;
  try
    if FindFirst(EXEMPLOS_DIR + '*.json', faAnyFile, SR) = 0 then
    begin
      repeat
        Arquivos.Add(SR.Name);
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;

    if Arquivos.Count = 0 then
    begin
      WriteLn('Nenhum arquivo JSON encontrado em ', EXEMPLOS_DIR);
      Exit;
    end;

    WriteLn('\n--- Arquivos Disponíveis ---');
    for I := 0 to Arquivos.Count - 1 do
      WriteLn('[', I, '] ', Arquivos[I]);

    Write('Escolha o número do arquivo: ');
    ReadLn(Escolha);
    if (Escolha >= 0) and (Escolha < Arquivos.Count) then
    begin
      Caminho := EXEMPLOS_DIR + Arquivos[Escolha];
      Result := CarregarAutomato(Caminho);
    end
    else
      WriteLn('Número inválido.');
  finally
    Arquivos.Free;
  end;
end;

procedure MainLoop;
var
  Automato: TAutomato;
  Opcao, Sub: string;
  ArquivoEscolhido: TAutomato;
  ArquivosTxt: TStringList;
  I, Idx: Integer;
  Caminho: string;
  ResultadosTeste: TStringList;
  SR: TSearchRec;
begin
  ForceDirectories(RESULTADOS_DIR);
  Automato.Alfabeto := nil;

  while True do
  begin
    Menu;
    Write('Escolha uma opção: ');
    ReadLn(Opcao);
    Opcao := Trim(Opcao);

    if Opcao = '6' then
    begin
      WriteLn('Encerrando o programa...');
      Break;
    end;

    if Opcao = '5' then
    begin
      WriteLn('\n[Trocar Arquivo]');
      ArquivoEscolhido := ListarEEscolherArquivo;
      if Length(ArquivoEscolhido.Alfabeto) > 0 then
        Automato := ArquivoEscolhido;
      Continue;
    end;

    if (Opcao <> '0') and (Opcao <> '1') and (Opcao <> '2') and (Opcao <> '3') and (Opcao <> '4') then
    begin
      WriteLn('Opção inválida!');
      Continue;
    end;

    if Length(Automato.Alfabeto) = 0 then
    begin
      WriteLn('\nAVISO: Você precisa carregar um autômato primeiro.');
      ArquivoEscolhido := ListarEEscolherArquivo;
      if Length(ArquivoEscolhido.Alfabeto) > 0 then
        Automato := ArquivoEscolhido
      else
        Continue;
    end;

    case Opcao of
      '0': begin
        Automato := multi_ini_para_afn_lambda.Converter(Automato);
        SalvarAutomatoJson(Automato, RESULTADOS_DIR + 'resultado_multi_ini.json');
      end;
      '1': begin
        Automato := afn_lambda_para_afn.Converter(Automato);
        SalvarAutomatoJson(Automato, RESULTADOS_DIR + 'resultado_afn.json');
      end;
      '2': begin
        Automato := afn_para_afd.Converter(Automato);
        SalvarAutomatoJson(Automato, RESULTADOS_DIR + 'resultado_afd.json');
      end;
      '3': begin
        WriteLn('Aviso: O algoritmo assume que o AFD é total (tem transições para todos símbolos).');
        Automato := minimizacao_afd.Minimizar(Automato);
        SalvarAutomatoJson(Automato, RESULTADOS_DIR + 'resultado_minimo.json');
      end;
      '4': begin
        while True do
        begin
          SubmenuTestes;
          Write('Escolha uma opção: ');
          ReadLn(Sub);
          Sub := Trim(Sub);

          if Sub = '0' then
            Break
          else if Sub = '1' then
            TestarViaTerminal(Automato)
          else if Sub = '2' then
          begin
            WriteLn('\n[Teste via arquivo]');
            ArquivosTxt := TStringList.Create;
            try
              if FindFirst(EXEMPLOS_DIR + '*.txt', faAnyFile, SR) = 0 then
              begin
                repeat
                  ArquivosTxt.Add(SR.Name);
                until FindNext(SR) <> 0;
                FindClose(SR);
              end;

              if ArquivosTxt.Count > 0 then
              begin
                WriteLn('Arquivos de texto encontrados:');
                for I := 0 to ArquivosTxt.Count - 1 do
                  WriteLn('  [', I, '] ', ArquivosTxt[I]);
                Write('Escolha o número ou digite o caminho: ');
                ReadLn(Idx);
                if (Idx >= 0) and (Idx < ArquivosTxt.Count) then
                  Caminho := EXEMPLOS_DIR + ArquivosTxt[Idx]
                else
                begin
                  Write('Caminho inválido, digite manualmente: ');
                  ReadLn(Caminho);
                end;
              end
              else
              begin
                Write('Informe o caminho do arquivo de palavras: ');
                ReadLn(Caminho);
              end;
            finally
              ArquivosTxt.Free;
            end;
            ResultadosTeste := TestarViaArquivo(Automato, Caminho);
            ResultadosTeste.Free;
          end
          else
            WriteLn('Opção inválida!');
        end;
      end;
    end;

    if (Opcao = '0') or (Opcao = '1') or (Opcao = '2') or (Opcao = '3') then
    begin
      WriteLn('\n--- Autômato Resultante ---');
      ImprimirAutomato(Automato);
    end;
  end;
end;

begin
  MainLoop;
end.
