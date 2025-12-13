from af_utils import transicoes_por_estado

def aceita_palavra(af, palavra):
    estados_atuais = set(af["estados_iniciais"])
    mapa = transicoes_por_estado(af)

    for simbolo in palavra:
        novos_estados = set()
        for estado in estados_atuais:
            for destino in mapa.get(estado, {}).get(simbolo, []):
                novos_estados.add(destino)
        estados_atuais = novos_estados
        if not estados_atuais:
            break

    return any(e in af["estados_finais"] for e in estados_atuais)


def testar_via_terminal(af):
    print("Digite palavras para testar (vazio para sair):")
    while True:
        palavra = input("> ").strip()
        if palavra == "":
            break
        if aceita_palavra(af, palavra):
            print("ACEITA")
        else:
            print("REJEITA")
