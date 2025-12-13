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
- Conversão de **AFN para AFD** (construção dos subconjuntos);
- **Minimização de AFD**;
- **Teste de palavras** em autômatos:
  - Via terminal;
  - Via arquivo de texto.

Todas as operações seguem rigorosamente a teoria apresentada em sala.

---

## 🧠 Visão Geral de Funcionamento

O arquivo principal do projeto é `main.py`. Ao executá-lo, o programa:

1. Solicita ao usuário um **arquivo JSON contendo a definição de um autômato**;
2. Exibe um **menu interativo** com opções de conversão e testes;
3. Permite aplicar sucessivas conversões sobre o autômato carregado;
4. Exibe o autômato resultante no terminal;
5. Salva automaticamente o autômato convertido em um novo arquivo JSON;
6. Permite testar palavras no autômato atual.

> Observação: após cada conversão, o autômato resultante passa a ser o autômato corrente para as próximas operações.

---

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

- **Python 3.10** ou superior  
- Não há dependências externas além da biblioteca padrão do Python

---

## 📂 Estrutura do Projeto

```text
trabalho_lfa/
├── main.py
├── io_utils.py
├── conversoes/
│   ├── __init__.py
│   ├── multi_ini_para_afn_lambda.py
│   ├── afn_lambda_para_afn.py
│   ├── afn_para_afd.py
│   └── minimizacao_afd.py
├── testes/
│   ├── __init__.py
│   ├── testar_terminal.py
│   └── testar_arquivo.py
├── exemplos/
│   ├── af_exemplo.json
│   └── palavras.txt
├── README.md
└── .gitignore
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
