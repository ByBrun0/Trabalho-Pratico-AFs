from typing import Dict, Any, Set
from af_utils import fecho_lambda, mover, LAMBDA


def converter(automato: Dict[str, Any]) -> Dict[str, Any]:
    """
    Converte um AFN-λ (AFN com transições lambda) em um AFN equivalente
    sem transições lambda, usando fecho-λ.
    """

    # Remove lambda do alfabeto
    novo_alfabeto = [s for s in automato.get("alfabeto", []) if s != LAMBDA]

    estados = automato.get("estados", [])
    estados_finais_originais = set(automato.get("estados_finais", []))

    # Novo conjunto de transições
    novas_transicoes = []

    # Para cada estado e símbolo, calcula δ'(q, a)
    for estado in estados:
        fecho_q = fecho_lambda(automato, {estado})

        for simbolo in novo_alfabeto:
            destinos = set()

            # move(fecho(q), a)
            move_result = mover(automato, fecho_q, simbolo)

            # fecho_lambda(move(...))
            for p in move_result:
                destinos |= fecho_lambda(automato, {p})

            for destino in destinos:
                novas_transicoes.append([estado, destino, simbolo])

    # Estados iniciais permanecem os mesmos
    novos_estados_iniciais = automato.get("estados_iniciais", [])

    # Novo conjunto de estados finais
    novos_estados_finais = []
    for estado in estados:
        fecho_q = fecho_lambda(automato, {estado})
        if not fecho_q.isdisjoint(estados_finais_originais):
            novos_estados_finais.append(estado)

    novo_automato = {
        "alfabeto": novo_alfabeto,
        "estados": estados,
        "estados_iniciais": novos_estados_iniciais,
        "estados_finais": novos_estados_finais,
        "transicoes": novas_transicoes
    }

    return novo_automato
