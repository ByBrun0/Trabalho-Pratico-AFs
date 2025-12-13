from typing import Dict, Any, Set, List
from af_utils import mover, normalizar_nome_estado, estados_finais_compostos


def converter(automato: Dict[str, Any]) -> Dict[str, Any]:
    """
    Converte um AFN (sem transições lambda) em um AFD equivalente
    usando o método dos subconjuntos.
    """

    alfabeto = automato.get("alfabeto", [])
    estados_afn = set(automato.get("estados", []))
    estados_finais_afn = set(automato.get("estados_finais", []))

    # Estado inicial do AFD é o conjunto dos estados iniciais do AFN
    estado_inicial_afn = set(automato.get("estados_iniciais", []))
    nome_estado_inicial = normalizar_nome_estado(estado_inicial_afn)

    # Estruturas do AFD
    estados_afd: List[str] = []
    estados_finais_afd: List[str] = []
    transicoes_afd: List[List[str]] = []

    # Controle de estados não processados
    fila: List[Set[str]] = []
    visitados: List[Set[str]] = []

    fila.append(estado_inicial_afn)
    visitados.append(estado_inicial_afn)
    estados_afd.append(nome_estado_inicial)

    if estados_finais_compostos(estado_inicial_afn, estados_finais_afn):
        estados_finais_afd.append(nome_estado_inicial)

    # Processamento dos subconjuntos
    while fila:
        estado_atual = fila.pop(0)
        nome_estado_atual = normalizar_nome_estado(estado_atual)

        for simbolo in alfabeto:
            destino = mover(automato, estado_atual, simbolo)

            if not destino:
                continue

            nome_destino = normalizar_nome_estado(destino)
            transicoes_afd.append([nome_estado_atual, nome_destino, simbolo])

            if destino not in visitados:
                visitados.append(destino)
                fila.append(destino)
                estados_afd.append(nome_destino)

                if estados_finais_compostos(destino, estados_finais_afn):
                    estados_finais_afd.append(nome_destino)

    novo_automato = {
        "alfabeto": alfabeto,
        "estados": estados_afd,
        "estados_iniciais": [nome_estado_inicial],
        "estados_finais": estados_finais_afd,
        "transicoes": transicoes_afd
    }

    return novo_automato