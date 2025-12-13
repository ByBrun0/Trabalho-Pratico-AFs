from io_utils import ler_automato_json, salvar_automato_json, imprimir_automato

# Conversões (serão implementadas nos respectivos arquivos)
from conversoes.multi_ini_para_afn_lambda import converter as multi_ini_para_afn_lambda
from conversoes.afn_lambda_para_afn import converter as afn_lambda_para_afn
from conversoes.afn_para_afd import converter as afn_para_afd
from conversoes.minimizacao_afd import minimizar as minimizar_afd

# Testes de palavras
from testes.testar_terminal import testar_terminal
from testes.testar_arquivo import testar_arquivo


def menu() -> None:
    print("\n===== TRABALHO LFA — AUTÔMATOS FINITOS =====")
    print("0 - Converter multiestado inicial em AFN-&")
    print("1 - Converter AFN-& em AFN")
    print("2 - Converter AFN em AFD")
    print("3 - Minimizar AFD")
    print("4 - Testar Palavras")
    print("5 - Sair")


def submenu_testes() -> None:
    print("\n--- Testar Palavras ---")
    print("1 - Testar palavras via terminal")
    print("2 - Testar palavras via arquivo")
    print("0 - Voltar")


def carregar_automato() -> dict:
    caminho = input("Informe o caminho do arquivo JSON do autômato: ")
    automato = ler_automato_json(caminho)
    print("\nAutômato carregado com sucesso!")
    imprimir_automato(automato)
    return automato


def main() -> None:
    automato = None

    while True:
        menu()
        opcao = input("Escolha uma opção: ").strip()

        if opcao == '5':
            print("Encerrando o programa...")
            break

        if opcao not in {'0', '1', '2', '3', '4'}:
            print("Opção inválida!")
            continue

        if automato is None:
            automato = carregar_automato()

        # ================= CONVERSÕES =================
        if opcao == '0':
            print("\n[0] Converter multiestado inicial em AFN-&")
            automato = multi_ini_para_afn_lambda(automato)

        elif opcao == '1':
            print("\n[1] Converter AFN-& em AFN")
            automato = afn_lambda_para_afn(automato)

        elif opcao == '2':
            print("\n[2] Converter AFN em AFD")
            automato = afn_para_afd(automato)

        elif opcao == '3':
            print("\n[3] Minimizar AFD")
            automato = minimizar_afd(automato)

        # ================= TESTES =================
        elif opcao == '4':
            while True:
                submenu_testes()
                sub = input("Escolha uma opção: ").strip()

                if sub == '0':
                    break

                elif sub == '1':
                    print("\n[Teste via terminal]")
                    testar_terminal(automato)

                elif sub == '2':
                    print("\n[Teste via arquivo]")
                    caminho = input("Informe o caminho do arquivo de palavras: ")
                    testar_arquivo(automato, caminho)

                else:
                    print("Opção inválida!")

        # ================= SAÍDA =================
        if opcao in {'0', '1', '2', '3'}:
            print("\n(Quando implementado, o novo autômato será exibido e salvo)")
            imprimir_automato(automato)
            salvar_automato_json(automato, 'saida.json')


if __name__ == '__main__':
    main()
