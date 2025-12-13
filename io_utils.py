import json
from typing import Dict, Any


def ler_automato_json(caminho: str) -> Dict[str, Any]:
    """
    Lê um autômato a partir de um arquivo JSON.
    Espera o formato padrão definido no trabalho de LFA.
    """
    with open(caminho, 'r', encoding='utf-8') as f:
        automato = json.load(f)
    return automato


def salvar_automato_json(automato: Dict[str, Any], caminho: str) -> None:
    """
    Salva um autômato em um arquivo JSON, com indentação para facilitar leitura.
    """
    with open(caminho, 'w', encoding='utf-8') as f:
        json.dump(automato, f, indent=4, ensure_ascii=False)


def imprimir_automato(automato: Dict[str, Any]) -> None:
    """
    Imprime o autômato no terminal de forma simples e legível.
    """
    print("\n=== AUTÔMATO ===")
    print(f"Alfabeto: {automato.get('alfabeto', [])}")
    print(f"Estados: {automato.get('estados', [])}")
    print(f"Estados iniciais: {automato.get('estados_iniciais', [])}")
    print(f"Estados finais: {automato.get('estados_finais', [])}")
    print("Transições:")
    for origem, destino, simbolo in automato.get('transicoes', []):
        print(f"  δ({origem}, {simbolo}) -> {destino}")
    print("=================\n")
