# Guia de Instalação do Haskell

Este guia fornece instruções passo a passo para instalar o Haskell usando **Stack** e **Cabal** no **Linux**, **macOS** e **Windows**.

---
## **Instalando Haskell**

### **1. Instalando o Haskell com Stack**

O Stack é uma ferramenta que facilita o gerenciamento de projetos e dependências do Haskell.

#### **Linux & macOS**

1. Abra um terminal.
2. Execute o seguinte comando para instalar o Stack:
   ```sh
   curl -sSL https://get.haskellstack.org/ | sh
   ```
3. Verifique a instalação:
   ```sh
   stack --version
   ```
4. Instale o GHC (compilador Haskell) e outras dependências:
   ```sh
   stack setup
   ```
5. Verifique se o GHC está instalado:
   ```sh
   stack ghc -- --version
   ```

#### **Windows** (via PowerShell)

1. Abra o **PowerShell** como Administrador.
2. Execute:
   ```powershell
   iwr -useb https://get.haskellstack.org/ | iex
   ```
3. Verifique a instalação:
   ```powershell
   stack --version
   ```
4. Instale o GHC e dependências:
   ```powershell
   stack setup
   ```
5. Verifique se o GHC está instalado:
   ```powershell
   stack ghc -- --version
   ```

---

### **2. Instalando o Haskell com Cabal**

O Cabal é o gerenciador de pacotes padrão do Haskell.

#### **Linux (Debian/Ubuntu)**

1. Abra um terminal e execute:
   ```sh
   sudo apt update
   sudo apt install ghc cabal-install -y
   ```
2. Verifique a instalação:
   ```sh
   ghc --version
   cabal --version
   ```
3. Atualize o Cabal:
   ```sh
   cabal update
   ```

#### **Linux (Fedora)**

1. Execute:
   ```sh
   sudo dnf install ghc cabal-install -y
   ```
2. Verifique a instalação:
   ```sh
   ghc --version
   cabal --version
   ```
3. Atualize o Cabal:
   ```sh
   cabal update
   ```

#### **macOS (usando Homebrew)**

1. Instale o Haskell:
   ```sh
   brew install ghc cabal-install
   ```
2. Verifique a instalação:
   ```sh
   ghc --version
   cabal --version
   ```
3. Atualize o Cabal:
   ```sh
   cabal update
   ```

#### **Windows**

1. Baixe e instale o Haskell pelo [GHCup](https://www.haskell.org/ghcup/).
2. Abra o **Prompt de Comando** ou **PowerShell** e verifique as versões:
   ```powershell
   ghc --version
   cabal --version
   ```
3. Atualize o Cabal:
   ```powershell
   cabal update
   ```

---

### **Verificação**

Após a instalação, confirme que tudo está funcionando executando:

```sh
 ghc --version  # Verificar versão do GHC
 cabal --version  # Verificar versão do Cabal
 stack --version  # Verificar versão do Stack
```

Boa programação em Haskell! 🚀

---
## **Instalando Jogo**

### **1. Clonando repositório Git**

1. No diretório que deseja baixar o jogo, executar:
    ```powershell
   git clone https://github.com/ViniciusLaureano/Haskell-Project.git
   ```

>> Caso deseje acessar o respositório com os arquivos, entre em:
>> https://github.com/ViniciusLaureano/Haskell-Project

### **2. Executando o código**

1. Entre na pasta onde salvou o código baixado

2. Execute o comando:
   ```powershell
   stack build
   stack run
   ```
  
3. Sucesso! Seu jogo está pronto para você se divertir!

4. Siga o tutorial para entender melhor das funcionalidades e regras do jogo! 