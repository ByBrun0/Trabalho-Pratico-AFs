from typing import Dict, Any, Set, List
from af_utils import mover, normalizar_nome_estado, estados_finais_compostos

def converter(automato: Dict[str, Any]) -> Dict[str, Any]:
    """
    Converte um AFN em AFD usando subconjuntos e RENOMEIA os estados
    para um formato sequencial simples (q0, q1, q2...) ou amigável.
    """
    alfabeto = automato.get("alfabeto", [])
    estados_afn = set(automato.get("estados", []))
    estados_finais_afn = set(automato.get("estados_finais", []))

    estado_inicial_afn = set(automato.get("estados_iniciais", []))
    nome_inicial_bruto = normalizar_nome_estado(estado_inicial_afn)

    # Listas temporárias com os nomes "feios" (ex: "{q0,q1}")
    estados_brutos = [nome_inicial_bruto]
    transicoes_brutas = []
    
    fila = [estado_inicial_afn]
    visitados = [estado_inicial_afn]

    # --- 1. Lógica dos Subconjuntos (Gera o AFD Matemático) ---
    while fila:
        estado_atual = fila.pop(0)
        nome_atual = normalizar_nome_estado(estado_atual)

        for simbolo in alfabeto:
            # Calcula destino (mesmo vazio)
            destino = mover(automato, estado_atual, simbolo)
            nome_destino = normalizar_nome_estado(destino)
            
            transicoes_brutas.append([nome_atual, nome_destino, simbolo])

            # Se é um estado novo, adiciona na fila
            if destino not in visitados:
                visitados.append(destino)
                fila.append(destino)
                estados_brutos.append(nome_destino)

    # --- 2. Etapa de Renomeação (Estética) ---
    # Vamos criar um mapa: "{q0,q1}" -> "q1" (ou S1, etc)
    mapa_nomes = {}
    novos_estados = []
    novos_iniciais = []
    novos_finais = []
    
    # O estado inicial será sempre o primeiro da lista, vamos chamá-lo de S0 ou q0
    # Dica: Usar "S" (State) evita confusão com os "q" do original, mas você pode usar "q"
    PREFIXO = "S" 

    for i, nome_bruto in enumerate(estados_brutos):
        # Se for o estado de erro {}, podemos dar um nome especial
        if nome_bruto == "{}":
            novo_nome = "q_erro" # Ou "limbo", ou ""
        else:
            novo_nome = f"{PREFIXO}{i}"
        
        mapa_nomes[nome_bruto] = novo_nome
        novos_estados.append(novo_nome)

    # Recriar transições com os novos nomes
    novas_transicoes = []
    for origem, destino, simbolo in transicoes_brutas:
        novas_transicoes.append([
            mapa_nomes[origem], 
            mapa_nomes[destino], 
            simbolo
        ])

    # Definir iniciais e finais com os novos nomes
    novos_iniciais = [mapa_nomes[nome_inicial_bruto]]

    # Para finais, precisamos checar o conjunto original em 'visitados'
    # A lista 'visitados' está na mesma ordem de 'estados_brutos'
    for i, conjunto_original in enumerate(visitados):
        if estados_finais_compostos(conjunto_original, estados_finais_afn):
            nome_novo = mapa_nomes[estados_brutos[i]]
            novos_finais.append(nome_novo)

    # Monta o objeto final limpo
    novo_automato = {
        "alfabeto": alfabeto,
        "estados": novos_estados,
        "estados_iniciais": novos_iniciais,
        "estados_finais": novos_finais,
        "transicoes": novas_transicoes
    }

    return novo_automato