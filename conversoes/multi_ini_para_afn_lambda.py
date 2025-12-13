from typing import Dict, Any
from af_utils import LAMBDA


def converter(automato: Dict[str, Any]) -> Dict[str, Any]:
    """
    Converte um autômato com múltiplos estados iniciais em um AFN-λ
    equivalente, criando um novo estado inicial com transições lambda
    para cada estado inicial original.
    """

    estados_iniciais = automato.get("estados_iniciais", [])

    # Se já houver apenas um estado inicial, retorna o autômato original
    if len(estados_iniciais) <= 1:
        return automato

    estados = list(automato.get("estados", []))
    alfabeto = list(automato.get("alfabeto", []))
    estados_finais = list(automato.get("estados_finais", []))
    transicoes = list(automato.get("transicoes", []))

    # Cria um novo estado inicial que não conflite com os existentes
    novo_estado_inicial = "q_ini"
    contador = 0
    while novo_estado_inicial in estados:
        contador += 1
        novo_estado_inicial = f"q_ini{contador}"

    estados.append(novo_estado_inicial)

    # Garante que lambda esteja no alfabeto
    if LAMBDA not in alfabeto:
        alfabeto.append(LAMBDA)

    # Adiciona transições lambda do novo estado inicial
    for estado in estados_iniciais:
        transicoes.append([novo_estado_inicial, estado, LAMBDA])

    novo_automato = {
        "alfabeto": alfabeto,
        "estados": estados,
        "estados_iniciais": [novo_estado_inicial],
        "estados_finais": estados_finais,
        "transicoes": transicoes
    }

    return novo_automato