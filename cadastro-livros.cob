     IDENTIFICATION DIVISION.
       PROGRAM-ID. CADASTRO-LIVROS.

      *==========================================
      *===  Autor: Waldecy Facanha
      *===  CONSULTORIA: WJiNFO
      *===  DATA: XX/XX/2025
      *===  OBJETIVO: PROJETO FINAL CADASTRO DE LIVROS
      *==========================================


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-LIVROS ASSIGN TO 'LIVROS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LIV-CODIGO
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-LIVROS.
       01 REG-LIVRO.
           05 LIV-CODIGO       PIC 9(5).
           05 LIV-TITULO       PIC X(50).
           05 LIV-AUTOR        PIC X(30).
           05 LIV-ANO          PIC 9(4).
           05 LIV-EDITORA      PIC X(30).

       WORKING-STORAGE SECTION.
       01 WS-FS                PIC XX.
       01 WS-OPCAO             PIC X.
       01 WS-CONTINUA          PIC X.
       01 WS-REGISTRO.
           05 WS-CODIGO        PIC 9(5).
           05 WS-TITULO        PIC X(50).
           05 WS-AUTOR         PIC X(30).
           05 WS-ANO           PIC 9(4).
           05 WS-EDITORA       PIC X(30).
       01 WS-EOF               PIC X.
       01 WS-ENCONTRADO        PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INICIALIZAR
           PERFORM PROCESSAR UNTIL WS-OPCAO = '5'
           PERFORM FINALIZAR
           STOP RUN.

       INICIALIZAR.
           OPEN I-O ARQ-LIVROS
           IF WS-FS NOT = '00'
               OPEN OUTPUT ARQ-LIVROS
               CLOSE ARQ-LIVROS
               OPEN I-O ARQ-LIVROS
           END-IF.

       PROCESSAR.
           DISPLAY ' '
           DISPLAY 'SISTEMA DE CADASTRO DE LIVROS'
           DISPLAY '1 - CADASTRAR'
           DISPLAY '2 - EDITAR'
           DISPLAY '3 - EXCLUIR'
           DISPLAY '4 - LISTAR'
           DISPLAY '5 - SAIR'
           DISPLAY 'ESCOLHA UMA OPCAO: '
           ACCEPT WS-OPCAO
           EVALUATE WS-OPCAO
               WHEN '1' PERFORM CADASTRAR
               WHEN '2' PERFORM EDITAR
               WHEN '3' PERFORM EXCLUIR
               WHEN '4' PERFORM LISTAR
               WHEN '5' CONTINUE
               WHEN OTHER DISPLAY 'OPCAO INVALIDA!'
           END-EVALUATE.

       CADASTRAR.
           DISPLAY ' '
           DISPLAY '*** CADASTRO DE LIVRO ***'
           DISPLAY 'CODIGO (5 DIGITOS): '
           ACCEPT WS-CODIGO
           MOVE WS-CODIGO TO LIV-CODIGO
           READ ARQ-LIVROS
               INVALID KEY
                   DISPLAY 'TITULO: '
                   ACCEPT WS-TITULO
                   DISPLAY 'AUTOR: '
                   ACCEPT WS-AUTOR
                   DISPLAY 'ANO: '
                   ACCEPT WS-ANO
                   DISPLAY 'EDITORA: '
                   ACCEPT WS-EDITORA
                   MOVE WS-TITULO TO LIV-TITULO
                   MOVE WS-AUTOR TO LIV-AUTOR
                   MOVE WS-ANO TO LIV-ANO
                   MOVE WS-EDITORA TO LIV-EDITORA
                   WRITE REG-LIVRO
                   DISPLAY 'LIVRO CADASTRADO COM SUCESSO!'
               NOT INVALID KEY
                   DISPLAY 'CODIGO JA EXISTE!'
           END-READ
           DISPLAY 'PRESSIONE ENTER PARA CONTINUAR'
           ACCEPT WS-CONTINUA.

       EDITAR.
           DISPLAY ' '
           DISPLAY '*** EDITAR LIVRO ***'
           DISPLAY 'INFORME O CODIGO DO LIVRO: '
           ACCEPT WS-CODIGO
           MOVE WS-CODIGO TO LIV-CODIGO
           READ ARQ-LIVROS
               INVALID KEY
                   DISPLAY 'LIVRO NAO ENCONTRADO!'
               NOT INVALID KEY
                   DISPLAY 'TITULO ATUAL: ' LIV-TITULO
                   DISPLAY 'NOVO TITULO: '
                   ACCEPT WS-TITULO
                   DISPLAY 'AUTOR ATUAL: ' LIV-AUTOR
                   DISPLAY 'NOVO AUTOR: '
                   ACCEPT WS-AUTOR
                   DISPLAY 'ANO ATUAL: ' LIV-ANO
                   DISPLAY 'NOVO ANO: '
                   ACCEPT WS-ANO
                   DISPLAY 'EDITORA ATUAL: ' LIV-EDITORA
                   DISPLAY 'NOVA EDITORA: '
                   ACCEPT WS-EDITORA
                   MOVE WS-TITULO TO LIV-TITULO
                   MOVE WS-AUTOR TO LIV-AUTOR
                   MOVE WS-ANO TO LIV-ANO
                   MOVE WS-EDITORA TO LIV-EDITORA
                   REWRITE REG-LIVRO
                   DISPLAY 'LIVRO ATUALIZADO COM SUCESSO!'
           END-READ
           DISPLAY 'PRESSIONE ENTER PARA CONTINUAR'
           ACCEPT WS-CONTINUA.

       EXCLUIR.
           DISPLAY ' '
           DISPLAY '*** EXCLUIR LIVRO ***'
           DISPLAY 'INFORME O CODIGO DO LIVRO: '
           ACCEPT WS-CODIGO
           MOVE WS-CODIGO TO LIV-CODIGO
           READ ARQ-LIVROS
               INVALID KEY
                   DISPLAY 'LIVRO NAO ENCONTRADO!'
               NOT INVALID KEY
                   DELETE ARQ-LIVROS
                   DISPLAY 'LIVRO EXCLUIDO COM SUCESSO!'
           END-READ
           DISPLAY 'PRESSIONE ENTER PARA CONTINUAR'
           ACCEPT WS-CONTINUA.

       LISTAR.
           DISPLAY ' '
           DISPLAY '*** LISTAGEM DE LIVROS ***'
           MOVE 'N' TO WS-EOF
           START ARQ-LIVROS KEY IS NOT LESS THAN LIV-CODIGO
           PERFORM UNTIL WS-EOF = 'Y'
               READ ARQ-LIVROS NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       DISPLAY 'CODIGO: ' LIV-CODIGO
                       DISPLAY 'TITULO: ' LIV-TITULO
                       DISPLAY 'AUTOR: ' LIV-AUTOR
                       DISPLAY 'ANO: ' LIV-ANO
                       DISPLAY 'EDITORA: ' LIV-EDITORA
                       DISPLAY '------------------------'
               END-READ
           END-PERFORM
           DISPLAY 'PRESSIONE ENTER PARA CONTINUAR'
           ACCEPT WS-CONTINUA.

       FINALIZAR.
           CLOSE ARQ-LIVROS.