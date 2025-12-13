from typing import Dict, Any, List, Set

LAMBDA = "&"


def obter_transicoes(automato: Dict[str, Any], estado: str, simbolo: str) -> List[str]:
    """
    Retorna a lista de estados de destino a partir de um estado
    consumindo um determinado símbolo.
    """
    destinos = []
    for origem, destino, s in automato.get("transicoes", []):
        if origem == estado and s == simbolo:
            destinos.append(destino)
    return destinos


def fecho_lambda(automato: Dict[str, Any], estados: Set[str]) -> Set[str]:
    """
    Calcula o fecho-λ (fecho-epsilon) de um conjunto de estados.
    """
    fecho = set(estados)
    pilha = list(estados)

    while pilha:
        estado_atual = pilha.pop()
        for prox in obter_transicoes(automato, estado_atual, LAMBDA):
            if prox not in fecho:
                fecho.add(prox)
                pilha.append(prox)

    return fecho


def mover(automato: Dict[str, Any], estados: Set[str], simbolo: str) -> Set[str]:
    """
    Retorna o conjunto de estados alcançáveis a partir de um conjunto
    de estados consumindo um símbolo.
    """
    resultado = set()
    for estado in estados:
        for prox in obter_transicoes(automato, estado, simbolo):
            resultado.add(prox)
    return resultado


def normalizar_nome_estado(estados: Set[str]) -> str:
    """
    Gera um nome canônico para um estado composto (ex: AFD).
    """
    return "{" + ",".join(sorted(estados)) + "}"


def eh_afd(automato: Dict[str, Any]) -> bool:
    """
    Verifica se o autômato é determinístico.
    """
    if LAMBDA in automato.get("alfabeto", []):
        return False

    for estado in automato.get("estados", []):
        for simbolo in automato.get("alfabeto", []):
            destinos = obter_transicoes(automato, estado, simbolo)
            if len(destinos) != 1:
                return False

    return True


def eh_afn_lambda(automato: Dict[str, Any]) -> bool:
    """
    Verifica se o autômato possui transições lambda.
    """
    for _, _, simbolo in automato.get("transicoes", []):
        if simbolo == LAMBDA:
            return True
    return False


def estados_finais_compostos(estados_compostos: Set[str], estados_finais: Set[str]) -> bool:
    """
    Verifica se um estado composto contém algum estado final.
    """
    return not estados_compostos.isdisjoint(estados_finais)
