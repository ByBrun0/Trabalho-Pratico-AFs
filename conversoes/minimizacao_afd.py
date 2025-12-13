from typing import Dict, Any, List, Set


def minimizar(automato: Dict[str, Any]) -> Dict[str, Any]:
    """
    Minimiza um AFD usando o algoritmo de partições (Myhill–Nerode).
    Assume que o autômato de entrada é determinístico e completo.
    """

    alfabeto = automato.get("alfabeto", [])
    estados = set(automato.get("estados", []))
    estados_finais = set(automato.get("estados_finais", []))
    estados_iniciais = automato.get("estados_iniciais", [])
    transicoes = automato.get("transicoes", [])

    # Função de transição auxiliar
    def delta(estado: str, simbolo: str) -> str:
        for origem, destino, s in transicoes:
            if origem == estado and s == simbolo:
                return destino
        return None

    # Partição inicial: finais e não finais
    P: List[Set[str]] = []
    F = estados_finais
    NF = estados - estados_finais

    if F:
        P.append(set(F))
    if NF:
        P.append(set(NF))

    mudou = True
    while mudou:
        mudou = False
        nova_P: List[Set[str]] = []

        for grupo in P:
            # Agrupa estados por assinatura de transições
            classes = {}
            for estado in grupo:
                assinatura = []
                for simbolo in alfabeto:
                    destino = delta(estado, simbolo)
                    # Descobre em qual grupo o destino está
                    indice = None
                    for i, g in enumerate(P):
                        if destino in g:
                            indice = i
                            break
                    assinatura.append(indice)

                assinatura = tuple(assinatura)
                classes.setdefault(assinatura, set()).add(estado)

            if len(classes) == 1:
                nova_P.append(grupo)
            else:
                mudou = True
                for subgrupo in classes.values():
                    nova_P.append(subgrupo)

        P = nova_P

    # Construção do AFD minimizado
    # Mapeia cada estado antigo para seu representante
    representante = {}
    for grupo in P:
        rep = sorted(grupo)[0]
        for estado in grupo:
            representante[estado] = rep

    novos_estados = sorted(set(representante.values()))

    novo_estado_inicial = representante[estados_iniciais[0]]

    novos_estados_finais = sorted({representante[e] for e in estados_finais})

    novas_transicoes = []
    for estado in novos_estados:
        for simbolo in alfabeto:
            destino = delta(estado, simbolo)
            if destino is not None:
                novas_transicoes.append([
                    estado,
                    representante[destino],
                    simbolo
                ])

    novo_automato = {
        "alfabeto": alfabeto,
        "estados": novos_estados,
        "estados_iniciais": [novo_estado_inicial],
        "estados_finais": novos_estados_finais,
        "transicoes": novas_transicoes
    }

    return novo_automato