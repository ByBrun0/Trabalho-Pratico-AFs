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

---

[gmail-badge]: https://img.shields.io/badge/-Gmail-D14836?style=for-the-badge&logo=Gmail&logoColor=white
[gmail-bruno]: mailto:bruno.santos@aluno.cefetmg.br

[cefet-badge]: https://img.shields.io/badge/CEFET--MG-Campus%20V-blue?logo=academia
[cefet-url]: https://www.cefetmg.br/

[vscode-badge]: https://img.shields.io/badge/VSCode-1.86-blue?logo=visualstudiocode
[vscode-url]: https://code.visualstudio.com/

[python-badge]: https://img.shields.io/badge/Python-3.10-yellow?logo=python
[python-url]: https://www.python.org/
