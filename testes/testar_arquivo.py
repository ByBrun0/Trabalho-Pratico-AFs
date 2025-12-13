from af_utils import mover, fecho_lambda, estados_finais_compostos


def aceita_palavra(af, palavra):
    """
    Simula a aceitação de uma palavra considerando AFN e Transições Lambda.
    """
    # 1. O estado atual começa como o Fecho-Lambda dos iniciais
    # (Ou seja, onde consigo chegar sem ler nada?)
    estados_iniciais = set(af["estados_iniciais"])
    estados_atuais = fecho_lambda(af, estados_iniciais)

    for simbolo in palavra:
        # 2. Verifica se o símbolo existe no alfabeto (opcional, mas bom pra debug)
        # Se quiser ser rigoroso: if simbolo not in af['alfabeto']: return False
        
        # 3. Move consumindo o símbolo (passo padrão)
        proximos_estados = mover(af, estados_atuais, simbolo)
        
        # 4. Imediatamente calcula o fecho-lambda dos destinos
        # (Onde consigo chegar via lambda a partir dos novos estados?)
        estados_atuais = fecho_lambda(af, proximos_estados)
        
        # Se morreu (conjunto vazio), já pode parar
        if not estados_atuais:
            break

    # 5. Verifica se sobrou algum estado final no conjunto atual
    estados_finais = set(af["estados_finais"])
    return estados_finais_compostos(estados_atuais, estados_finais)


def testar_via_arquivo(af, caminho_arquivo):
    with open(caminho_arquivo, 'r', encoding='utf-8') as f:
        palavras = [linha.strip() for linha in f if linha.strip()]

    resultados = []
    for palavra in palavras:
        resultado = aceita_palavra(af, palavra)
        resultados.append((palavra, resultado))
        print(f"{palavra}: {'ACEITA' if resultado else 'REJEITA'}")

    return resultados
