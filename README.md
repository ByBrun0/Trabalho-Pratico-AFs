# 🤖 Conversões e Testes de Autômatos Finitos (LFA)

[![Instituição][cefet-badge]][cefet-url]
[![IDE][vscode-badge]][vscode-url]
[![Linguagem][python-badge]][python-url]

Este repositório contém o código-fonte do **Trabalho da disciplina de Linguagens Formais e Autômatos (LFA)**, ministrada no **CEFET-MG – Campus V**, cujo objetivo é **implementar conversões clássicas entre autômatos finitos e realizar testes de aceitação de palavras**.

O projeto foi desenvolvido com foco em **organização modular**, **clareza teórica** e **aderência formal** aos conceitos estudados em LFA.

---

## 🎯 Objetivos do Projeto

O sistema implementa e integra as seguintes funcionalidades:

- Conversão de **AF com múltiplos estados iniciais** para **AFN-λ**;
- Conversão de **AFN-λ para AFN** (remoção de transições λ);
- Conversão de **AFN para AFD** (construção dos subconjuntos, com criação de estado de erro e renomeação de estados);
- **Minimização de AFD**;
- **Teste de palavras** em autômatos:
  - Via terminal;
  - Via arquivo de texto.

Todas as operações seguem rigorosamente a teoria apresentada em sala.

---

## 🧠 Visão Geral de Funcionamento

O arquivo principal do projeto é `main.py`. Ao executá-lo, o sistema:

1. **Lista automaticamente** os arquivos `.json` disponíveis na pasta `exemplos/`;
2. Solicita que o usuário selecione um autômato inicial por número;
3. Exibe um **menu interativo** de operações;
4. Após cada conversão, o autômato resultante é:
   - Exibido no terminal (com estados renomeados e organizados);
   - Salvo automaticamente na pasta `resultados/`;
   - Definido como o autômato atual para a próxima operação.

## 📥 Clone do Projeto

Clone o repositório para sua máquina local:

```bash
# Usando HTTPS
git clone https://github.com/seu-usuario/trabalho-lfa-automatos.git

# Usando SSH
git clone git@github.com:seu-usuario/trabalho-lfa-automatos.git
```

---

## 🚀 Requisitos

### Versão Python
- **Python 3.10** ou superior  
- Não há dependências externas além da biblioteca padrão do Python

### Versão Pascal (FPC)
- **Free Pascal Compiler 3.2.2** ou superior
  - Linux: `sudo apt install fpc` (Debian/Ubuntu) ou equivalente para sua distro
  - macOS: `brew install fpc`
  - Windows: Baixar em [https://www.freepascal.org](https://www.freepascal.org)

---

## 📂 Estrutura do Projeto

```text
Trabalho-Pratico-AFs/
├── main.py                           <-- Entrada principal (Python)
├── af_utils.py                       <-- Utilidades para autômatos
├── io_utils.py                       <-- I/O JSON
├── README.md
├── .gitignore
│
├── conversoes/                       <-- Conversões em Python
│   ├── __init__.py
│   ├── multi_ini_para_afn_lambda.py
│   ├── afn_lambda_para_afn.py
│   ├── afn_para_afd.py
│   └── minimizacao_afd.py
│
├── testes/                           <-- Testes em Python
│   ├── __init__.py
│   ├── testar_terminal.py
│   └── testar_arquivo.py
│
├── exemplos/                         <-- Entrada: JSONs de autômatos e palavras
│   ├── af.json
│   ├── afn_lambda.json
│   ├── teste1.json
│   ├── teste2.json
│   ├── teste3.json
│   ├── teste4.json
│   ├── teste_completo.json
│   └── palavras.txt
│
├── resultados/                       <-- Saída: JSONs gerados (ignorado por git)
│   ├── resultado_multi_ini.json
│   ├── resultado_afn.json
│   ├── resultado_afd.json
│   └── resultado_minimo.json
│
└── pascal/                           <-- Implementação em Free Pascal
    ├── main.pas                      <-- Entrada principal (FPC)
    ├── af_utils.pas                  <-- Utilidades para autômatos
    ├── io_utils.pas                  <-- I/O JSON (fpjson)
    │
    ├── conversoes/                   <-- Conversões em Pascal
    │   ├── multi_ini_para_afn_lambda.pas
    │   ├── afn_lambda_para_afn.pas
    │   ├── afn_para_afd.pas
    │   └── minimizacao_afd.pas
    │
    └── testes/                       <-- Testes em Pascal
        ├── testar_terminal.pas
        └── testar_arquivo.pas
```

---

## 📄 Formato do Autômato (JSON)

Os autômatos devem ser descritos em arquivos `.json` no seguinte formato:

```json
{
  "alfabeto": ["a", "b"],
  "estados": ["q0", "q1", "q2"],
  "estados_iniciais": ["q0"],
  "estados_finais": ["q2"],
  "transicoes": [
    ["q0", "q1", "a"],
    ["q1", "q2", "b"]
  ]
}
```

Para AFN-λ, utiliza-se o símbolo `"&"` para representar transições lambda.

---

## 🔬 Algoritmos Implementados

Esta seção detalha a lógica dos algoritmos de conversão e teste implementados no projeto.

### 0️⃣ Converter Multiestado Inicial em AFN-λ

**Problema:** Autômatos com múltiplos estados iniciais não são formalmente AFN-λ padrão.

**Solução:** Cria-se um novo estado inicial único (`q_ini`, `q_ini1`, etc.) que possui transições λ (`&`) para cada um dos estados iniciais originais.

**Implementação (`conversoes/multi_ini_para_afn_lambda.py`):**
```python
# 1. Verifica se há múltiplos estados iniciais
if len(estados_iniciais) <= 1:
    return automato  # Já está no formato correto

# 2. Cria novo estado inicial que não conflite com existentes
novo_estado_inicial = "q_ini"
contador = 0
while novo_estado_inicial in estados:
    contador += 1
    novo_estado_inicial = f"q_ini{contador}"

# 3. Adiciona transições lambda do novo estado para todos os iniciais originais
for estado in estados_iniciais:
    transicoes.append([novo_estado_inicial, estado, LAMBDA])

# 4. Retorna autômato com estado inicial único
```

**Resultado:** AFN-λ com um único estado inicial.

---

### 1️⃣ Converter AFN-λ em AFN (Remoção de λ)

**Problema:** Remover transições lambda mantendo a linguagem aceita equivalente.

**Solução:** Utiliza o **fecho-λ** (lambda-closure) para calcular novas transições que "absorvem" os caminhos via λ.

**Conceitos-chave:**
- **Fecho-λ(q)**: Conjunto de estados alcançáveis a partir de `q` usando apenas transições λ.
- **Nova transição δ'(q, a)**: Para cada estado `q` e símbolo `a`, calcula:
  1. `fecho-λ(q)` → Estados alcançáveis sem consumir símbolos
  2. `mover(fecho-λ(q), a)` → Consome o símbolo `a`
  3. `fecho-λ(mover(...))` → Expande novamente via λ

**Implementação (`conversoes/afn_lambda_para_afn.py`):**
```python
# 1. Remove λ do alfabeto
novo_alfabeto = [s for s in automato.get("alfabeto", []) if s != LAMBDA]

# 2. Para cada estado e símbolo, calcula nova transição
for estado in estados:
    fecho_q = fecho_lambda(automato, {estado})
    
    for simbolo in novo_alfabeto:
        # move(fecho(q), a)
        move_result = mover(automato, fecho_q, simbolo)
        
        # fecho_lambda(move(...))
        for p in move_result:
            destinos |= fecho_lambda(automato, {p})
        
        # Adiciona transições diretas
        for destino in destinos:
            novas_transicoes.append([estado, destino, simbolo])

# 3. Estados finais: qualquer estado cujo fecho-λ contenha um final original
for estado in estados:
    if fecho_lambda(automato, {estado}) ∩ estados_finais_originais ≠ ∅:
        novos_estados_finais.append(estado)
```

**Resultado:** AFN sem transições λ, linguagem preservada.

---

### 2️⃣ Converter AFN em AFD (Construção de Subconjuntos)

**Problema:** Tornar determinístico um autômato não-determinístico.

**Solução:** Algoritmo de **construção de subconjuntos** (powerset construction), onde cada estado do AFD é um conjunto de estados do AFN.

**Etapas:**
1. **Estado inicial do AFD:** `{q0}` (conjunto com estado inicial do AFN)
2. **Para cada conjunto e símbolo:** Calcula destino usando `mover(conjunto, símbolo)`
3. **Novos conjuntos:** Adicionados à fila para processamento (BFS)
4. **Estado final do AFD:** Qualquer conjunto que contenha ao menos um estado final do AFN
5. **Renomeação estética:** `{q0,q1}` → `S0`, `{q2}` → `S1`, `{}` → `q_erro`

**Implementação (`conversoes/afn_para_afd.py`):**
```python
# 1. Construção de subconjuntos (BFS)
fila = [estado_inicial_afn]
visitados = [estado_inicial_afn]

while fila:
    estado_atual = fila.pop(0)
    
    for simbolo in alfabeto:
        # Calcula destino (pode ser vazio {})
        destino = mover(automato, estado_atual, simbolo)
        transicoes_brutas.append([estado_atual, destino, simbolo])
        
        if destino not in visitados:
            visitados.append(destino)
            fila.append(destino)

# 2. Renomeação: {q0,q1} → S0, {} → q_erro
for i, conjunto in enumerate(visitados):
    if conjunto == set():
        novo_nome = "q_erro"
    else:
        novo_nome = f"S{i}"
    mapa_nomes[conjunto] = novo_nome

# 3. Estados finais: conjuntos que contêm pelo menos um estado final original
for conjunto in visitados:
    if conjunto ∩ estados_finais_afn ≠ ∅:
        novos_finais.append(mapa_nomes[conjunto])
```

**Resultado:** AFD completo com nomes limpos (`S0`, `S1`, `q_erro`).

---

### 3️⃣ Minimizar AFD (Algoritmo de Myhill-Nerode)

**Problema:** Reduzir o número de estados do AFD sem alterar a linguagem aceita.

**Solução:** Algoritmo de **refinamento de partições**, que agrupa estados equivalentes.

**Conceito:** Dois estados `p` e `q` são equivalentes se, para toda palavra `w`, `δ(p,w)` e `δ(q,w)` levam ambos a finais ou ambos a não-finais.

**Etapas:**
1. **Partição inicial:** `P = {F, Q\F}` (finais e não-finais)
2. **Refinamento:** Para cada grupo, verifica se estados têm "assinatura" idêntica (mesmo comportamento para todos os símbolos)
3. **Assinatura de um estado:** Tupla indicando para qual partição cada símbolo leva
4. **Repete até estabilizar:** Quando nenhum grupo se divide mais
5. **Representante:** Escolhe um estado de cada grupo como representante

**Implementação (`conversoes/minimizacao_afd.py`):**
```python
# 1. Partição inicial
P = [{estados_finais}, {estados_nao_finais}]

# 2. Refinamento iterativo
mudou = True
while mudou:
    mudou = False
    nova_P = []
    
    for grupo in P:
        # Agrupa por assinatura
        classes = {}
        for estado in grupo:
            assinatura = []
            for simbolo in alfabeto:
                destino = delta(estado, simbolo)
                # Descobre índice da partição de destino
                indice = encontrar_grupo(P, destino)
                assinatura.append(indice)
            
            assinatura = tuple(assinatura)
            classes[assinatura].add(estado)
        
        # Se dividiu, marca mudança
        if len(classes) > 1:
            mudou = True
        nova_P.extend(classes.values())
    
    P = nova_P

# 3. Escolhe representante de cada grupo
for grupo in P:
    representante[grupo] = sorted(grupo)[0]
```

**Resultado:** AFD mínimo equivalente.

---

### 4️⃣ Testar Palavras (Simulação de Aceitação)

**Problema:** Verificar se uma palavra é aceita pelo autômato (AFN ou AFD).

**Solução:** Simula a execução do autômato, mantendo conjunto de estados ativos.

**Suporta:** AFN, AFD e AFN-λ (usa fecho-λ automaticamente).

**Implementação (`testes/testar_terminal.py`):**
```python
def aceita_palavra(af, palavra):
    # 1. Estado inicial: fecho-λ dos estados iniciais
    estados_atuais = fecho_lambda(af, {af["estados_iniciais"]})
    
    # 2. Para cada símbolo da palavra
    for simbolo in palavra:
        # Move consumindo o símbolo
        proximos = mover(af, estados_atuais, simbolo)
        
        # Expande via fecho-λ
        estados_atuais = fecho_lambda(af, proximos)
        
        # Se morreu (conjunto vazio), rejeita
        if not estados_atuais:
            return False
    
    # 3. Aceita se algum estado atual é final
    return estados_atuais ∩ estados_finais ≠ ∅
```

**Modos:**
- **Terminal:** Digita palavras interativamente
- **Arquivo:** Lê palavras de `.txt` (uma por linha)

**Suporte a palavra vazia:** Pressionar ENTER sem digitar testa ε (epsilon).

---

## ⚠️ Guia de Uso (Fluxo de Conversão)

Este sistema funciona como um **pipeline (funil) de conversão**. O usuário deve selecionar a opção condizente com o **estado atual** do autômato carregado.

Siga a ordem lógica abaixo para evitar inconsistências:

1. **Se o autômato tem múltiplos estados iniciais:**
   - Execute a **Opção 0**: *Multiestado → AFN-λ*
   
2. **Se o autômato é um AFN-λ (tem transições `&`):**
   - Execute a **Opção 1**: *AFN-λ → AFN*

3. **Se o autômato é um AFN (não determinístico, sem `&`):**
   - Execute a **Opção 2**: *AFN → AFD*
   - *Nota:* Esta etapa gera um **AFD Completo** (com estado de erro explícito se necessário) e **renomeia** os estados para um formato amigável (ex: `S0`, `S1`, `q_erro`).

4. **Se o autômato é um AFD:**
   - Execute a **Opção 3**: *Minimizar AFD*

> **Importante:** Se você carregar um arquivo que já é um **AFN**, não selecione a opção 0. Vá direto para a opção 2. O sistema assume que o usuário sabe em qual etapa do processo o arquivo de entrada se encaixa.

## ▶️ Execução

Com o Python instalado, execute:

```bash
# Linux / macOS
python3 main.py

# Windows
python main.py
# ou
py main.py
```

---

## 🐘 Implementação em Pascal (FPC)

Além da versão em Python, o repositório traz uma porta completa em Pascal (Free Pascal 3.2.2) no diretório `pascal/`, reproduzindo as mesmas funcionalidades de conversão e teste de palavras.

### Estrutura Pascal
- `pascal/main.pas`: programa CLI com o mesmo menu do Python; lê/gera JSON em `../exemplos` e `../resultados`.
- `pascal/af_utils.pas`: tipos do autômato, fecho-λ, mover, checagens de determinismo e finais.
- `pascal/io_utils.pas`: leitura/escrita de autômatos em JSON e impressão formatada no terminal.
- `pascal/conversoes/`:
   - `multi_ini_para_afn_lambda.pas`: cria estado inicial único com transições λ.
   - `afn_lambda_para_afn.pas`: remove λ via fecho-λ.
   - `afn_para_afd.pas`: construção de subconjuntos, estado de erro e renomeação (`S0`, `S1`, `q_erro`).
   - `minimizacao_afd.pas`: minimização via refinamento de partições.
- `pascal/testes/`:
   - `testar_terminal.pas`: aceita/rejeita palavras digitadas.
   - `testar_arquivo.pas`: testa palavras de um `.txt` linha a linha.

### Compilar
Execute os comandos dentro de `pascal/` (paths relativos já apontam para `../exemplos` e `../resultados`):

```bash
cd pascal
fpc -Fu. -Fuconversoes -Futestes main.pas
```

Gerará o binário `main` (ou `main.exe` no Windows). Se quiser limpar antes, remova `main` e os `.o`/`.ppu` gerados.

### Executar

```bash
cd pascal
./main
```

Fluxo de uso no CLI Pascal:
1. Opção 5: escolha um JSON em `../exemplos/` (ex.: `af.json`, `afn_lambda.json`).
2. Opções 0–3: conversões; saída em `../resultados/` (`resultado_multi_ini.json`, `resultado_afn.json`, `resultado_afd.json`, `resultado_minimo.json`).
3. Opção 4: testar palavras (1=terminal, 2=arquivo `.txt` em `../exemplos/` ou caminho manual).

---

## 🧪 Testes de Palavras

O sistema permite testar palavras:

- **Via terminal**: o usuário digita palavras manualmente;
- **Via arquivo**: cada linha de um arquivo `.txt` representa uma palavra.

O resultado exibido será:
- `ACEITA`
- `REJEITA`

---

## 👨‍💻 Autor

<div align="center">

**Bruno Prado dos Santos**  
*Estudante de Engenharia de Computação @ CEFET-MG*  

[![Gmail][gmail-badge]][gmail-bruno]

</div>

<div align="center">

**Paulo Henrique de Souza Hemetrio**  
*Estudante de Engenharia de Computação @ CEFET-MG*  

[![Gmail][gmail-badge]][gmail-paulo]

</div>

<div align="center">

**João Paulo da Cunha Faria**  
*Estudante de Engenharia de Computação @ CEFET-MG*  

[![Gmail][gmail-badge]][gmail-joao]

</div>
---

[gmail-badge]: https://img.shields.io/badge/-Gmail-D14836?style=for-the-badge&logo=Gmail&logoColor=white
[gmail-bruno]: mailto:bruno.santos@aluno.cefetmg.br
[gmail-joao]: mailto:joao@aluno.cefetmg.br
[gmail-paulo]: mailto:henriquepaulete40@gmail.com

[cefet-badge]: https://img.shields.io/badge/CEFET--MG-Campus%20V-blue?logo=academia
[cefet-url]: https://www.cefetmg.br/

[vscode-badge]: https://img.shields.io/badge/VSCode-1.86-blue?logo=visualstudiocode
[vscode-url]: https://code.visualstudio.com/

[python-badge]: https://img.shields.io/badge/Python-3.10-yellow?logo=python
[python-url]: https://www.python.org/
