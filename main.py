from io_utils import ler_automato_json, salvar_automato_json, imprimir_automato
import sys
from pathlib import Path

BASE_DIR = Path(__file__).resolve().parent
EXEMPLOS_DIR = BASE_DIR / "exemplos"

# Define a pasta de resultados e cria se não existir
RESULTADOS_DIR = BASE_DIR / "resultados"
RESULTADOS_DIR.mkdir(exist_ok=True)

PALAVRAS = EXEMPLOS_DIR / "palavras.txt"
AF_PADRAO = EXEMPLOS_DIR / "af.json"
AFN_LAMBDA_PADRAO = EXEMPLOS_DIR / "afn_lambda.json"


# Conversões (serão implementadas nos respectivos arquivos)
from conversoes.multi_ini_para_afn_lambda import converter as multi_ini_para_afn_lambda
from conversoes.afn_lambda_para_afn import converter as afn_lambda_para_afn
from conversoes.afn_para_afd import converter as afn_para_afd
from conversoes.minimizacao_afd import minimizar as minimizar_afd

# Testes de palavras
from testes.testar_terminal import testar_via_terminal
from testes.testar_arquivo import testar_via_arquivo


def menu() -> None:
    print("\n===== TRABALHO LFA — AUTÔMATOS FINITOS =====")
    print("0 - Converter multiestado inicial em AFN-&")
    print("1 - Converter AFN-& em AFN")
    print("2 - Converter AFN em AFD")
    print("3 - Minimizar AFD")
    print("4 - Testar Palavras")
    print("5 - Carregar/Trocar Arquivo (Reiniciar)")
    print("6 - Sair")


def submenu_testes() -> None:
    print("\n--- Testar Palavras ---")
    print("1 - Testar palavras via terminal")
    print("2 - Testar palavras via arquivo")
    print("0 - Voltar")


def carregar_automato(caminho: Path) -> dict:
    automato = ler_automato_json(caminho)
    print(f"\nAutômato carregado com sucesso ({caminho.name})!")
    imprimir_automato(automato)
    return automato

def listar_e_escolher_arquivo() -> dict:
    """Lista arquivos JSON na pasta exemplos e permite seleção numérica."""
    arquivos = list(EXEMPLOS_DIR.glob("*.json"))
    
    if not arquivos:
        print(f"Nenhum arquivo JSON encontrado em {EXEMPLOS_DIR}")
        return None

    print("\n--- Arquivos Disponíveis ---")
    for i, arq in enumerate(arquivos):
        print(f"[{i}] {arq.name}")

    try:
        escolha = int(input("Escolha o número do arquivo: "))
        if 0 <= escolha < len(arquivos):
            return carregar_automato(arquivos[escolha])
        else:
            print("Número inválido.")
    except ValueError:
        print("Entrada inválida.")
    return None

def main() -> None:
    automato = None

    while True:
        menu()
        opcao = input("Escolha uma opção: ").strip()

        if opcao == '6':
            print("Encerrando o programa...")
            break
        
        if opcao == '5':
            print("\n[Trocar Arquivo]")
            novo_automato = listar_e_escolher_arquivo()
            if novo_automato:
                automato = novo_automato
            continue

        if opcao not in {'0', '1', '2', '3', '4'}:
            print("Opção inválida!")
            continue

        if automato is None:
            print("\nAVISO: Você precisa carregar um autômato primeiro.")
            novo_automato = listar_e_escolher_arquivo()
            if novo_automato:
                automato = novo_automato
            else:
                continue
            
            continue

        # ================= CONVERSÕES =================
        if opcao == '0':
            automato = multi_ini_para_afn_lambda(automato)
            salvar_automato_json(automato, RESULTADOS_DIR / 'resultado_multi_ini.json')
            
        elif opcao == '1':
            automato = afn_lambda_para_afn(automato)
            salvar_automato_json(automato, RESULTADOS_DIR / 'resultado_afn.json')

        elif opcao == '2':
            automato = afn_para_afd(automato)
            salvar_automato_json(automato, RESULTADOS_DIR / 'resultado_afd.json')

        elif opcao == '3':
            print("Aviso: O algoritmo assume que o AFD é total (tem transições para todos símbolos).")
            automato = minimizar_afd(automato)
            salvar_automato_json(automato, RESULTADOS_DIR / 'resultado_minimo.json')

        # ================= TESTES =================
        elif opcao == '4':
            while True:
                submenu_testes()
                sub = input("Escolha uma opção: ").strip()

                if sub == '0':
                    break

                elif sub == '1':
                    testar_via_terminal(automato)

                elif sub == '2':
                    print("\n[Teste via arquivo]")
                    # Lista arquivos txt da pasta exemplos para facilitar
                    arquivos_txt = list(EXEMPLOS_DIR.glob("*.txt"))
                    if arquivos_txt:
                        print("Arquivos de texto encontrados:")
                        for i, txt in enumerate(arquivos_txt):
                            print(f"  [{i}] {txt.name}")
                        try:
                            idx = int(input("Escolha o número ou digite o caminho: "))
                            if 0 <= idx < len(arquivos_txt):
                                caminho = arquivos_txt[idx]
                            else:
                                caminho = input("Caminho inválido, digite manualmente: ")
                        except ValueError:
                             caminho = input("Caminho: ")
                    else:
                        caminho = input("Informe o caminho do arquivo de palavras: ")
                        
                    testar_via_arquivo(automato, caminho)

                else:
                    print("Opção inválida!")

        # ================= SAÍDA =================
        if opcao in {'0', '1', '2', '3'}:
            print("\n--- Autômato Resultante ---")
            imprimir_automato(automato)


if __name__ == '__main__':
    main()
