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


def testar_via_arquivo(af, caminho_arquivo):
    with open(caminho_arquivo, 'r', encoding='utf-8') as f:
        palavras = [linha.strip() for linha in f if linha.strip()]

    resultados = []
    for palavra in palavras:
        resultado = aceita_palavra(af, palavra)
        resultados.append((palavra, resultado))
        print(f"{palavra}: {'ACEITA' if resultado else 'REJEITA'}")

    return resultados
