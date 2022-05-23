      ******************************************************************
      * Author: HENRIQUE RODRIGUES NEVES
      * Date: 21/05/2022
      * Purpose: TAREFA 05 DESENVOLVER CRUD COMPLETO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAREFA-05.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO "c:/temp/clientes.txt"
           ORGANIZATION            IS INDEXED
           ACCESS                  IS DYNAMIC
           RECORD KEY              IS CODIGO-CLIENTE
           FILE STATUS             IS WS-FS-CLIENTES.

           SELECT PRODUTOS ASSIGN TO "c:/temp/produtos.txt"
           ORGANIZATION            IS INDEXED
           ACCESS                  IS RANDOM
           RECORD KEY              IS CODIGO-PRODUTO
           FILE STATUS             IS WS-FS-PRODUTOS.

           SELECT PEDIDO-VENDAS ASSIGN TO "c:/temp/pedido-vendas.txt"
           ORGANIZATION            IS INDEXED
           ACCESS                  IS RANDOM
           RECORD KEY              IS CODIGO-PEDIDO
           FILE STATUS             IS WS-FS-PEDIDO.
       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 REG-CLIENTES.
           03 CODIGO-CLIENTE       PIC 9(03).
           03 NOME                 PIC X(25).
           03 RG                   PIC X(15).
           03 TELEFONE             PIC X(13).

       FD PRODUTOS.
       01 REG-PRODUTOS.
           03 CODIGO-PRODUTO       PIC 9(03).
           03 PRODUTO              PIC X(25).
           03 PRECO                PIC 9(06)V99.

       FD PEDIDO-VENDAS.
       01 REG-PEDIDO-VENDAS.
           03 CODIGO-PEDIDO        PIC 9(03).
           03 PV-CODIGO-CLIENTE    PIC 9(03).
           03 PV-NOME              PIC X(25).
           03 PV-RG                PIC X(15).
           03 PV-TELEFONE          PIC X(13).
           03 PV-CODIGO-PRODUTO    PIC 9(03).
           03 PV-PRODUTO           PIC X(25).
           03 PV-PRECO             PIC 9(06)V99.
           03 PV-QTDE              PIC 9(03).
           03 PV-VALOR-TOTAL       PIC 9(09)V99.
       WORKING-STORAGE SECTION.
       77 WS-FS-CLIENTES           PIC 99.
       77 WS-FS-PRODUTOS           PIC 99.
       77 WS-FS-PEDIDO             PIC 99.
       77 WS-ESCOLHA-MENU          PIC 99.
       77 WS-ESCOLHA-CLIENTES      PIC X(01).
       77 WS-ESCOLHA-PRODUTOS      PIC X(01).
       77 WS-ESCOLHA-PEDIDOS       PIC X(01).
       77 WS-GRAVAR-CLIENTE        PIC X(03) VALUE 'SIM'.
       77 WS-EOR-CLIENTES          PIC X(01).
       77 WS-EOF-CLIENTES          PIC X(01) VALUE 'S'.
       77 WS-DEL-PERMANENTE        PIC X VALUE SPACES.
       77 WS-DEL                   PIC X VALUE SPACES.

       01 WS-REG-CLIENTES             PIC X(56).
       01  FILLER REDEFINES WS-REG-CLIENTES.
           03 WS-CODIGO-CLIENTE       PIC 9(03).
           03 WS-NOME                 PIC X(25).
           03 WS-RG                   PIC X(15).
           03 WS-TELEFONE             PIC X(13).

       01 WS-REG-PRODUTOS             PIC X(34).
       01 FILLER REDEFINES WS-REG-PRODUTOS.
           03 CODIGO-PRODUTO       PIC 9(03).
           03 PRODUTO              PIC X(25).
           03 PRECO                PIC 9(06)V99.

       01 WS-REG-PEDIDO-VENDAS     PIC X(105).
       01 FILLER REDEFINES WS-REG-PEDIDO-VENDAS.
           03 WS-CODIGO-PEDIDO     PIC 9(03).
           03 WS-PV-CODIGO-CLIENTE PIC 9(03).
           03 WS-PV-NOME           PIC X(25).
           03 WS-PV-RG             PIC X(15).
           03 WS-PV-TELEFONE       PIC X(13).
           03 WS-PV-CODIGO-PRODUTO PIC 9(03).
           03 WS-PV-PRODUTO        PIC X(25).
           03 WS-PV-PRECO          PIC 9(06)V99.
           03 WS-PV-QTDE           PIC 9(03).
           03 WS-PV-VALOR-TOTAL    PIC 9(09)V99.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            DISPLAY "---------- MENU PRINCIPAL ---------"
            DISPLAY "SELECIONE UMA OPCAO ABAIXO"
            DISPLAY "1 - CADASTRO DE CLIENTES"
            DISPLAY "2 - CADASTRO DE PRODUTOS"
            DISPLAY "3 - CADASTRO DE PEDIDO DE VENDAS"
            DISPLAY "9 - ENCERRAR PROGRAMA"
            ACCEPT WS-ESCOLHA-MENU.

            EVALUATE WS-ESCOLHA-MENU
               WHEN 1
                   PERFORM CADASTRO-CLIENTES
               WHEN 2
                   PERFORM CADASTRO-PRODUTOS
               WHEN 3
                   PERFORM CADASTRO-PEDIDOS
               WHEN 9
                   STOP RUN
               WHEN OTHER
                   DISPLAY "OPCAO INVALIDA"
                   PERFORM MAIN-PROCEDURE
            END-EVALUATE.


       CADASTRO-CLIENTES.

           DISPLAY "---------- CADASTRO DE CLIENTE ---------"
           DISPLAY "SELECIONE UMA OPCAO ABAIXO"
           DISPLAY "I - INCLUSAO DE DADOS DOS CLIENTES"
           DISPLAY "A - ALTERACAO DE DADOS DOS CLIENTES"
           DISPLAY "C - CONSULTA DE DADOS DOS CLIENTES"
           DISPLAY "E - EXCLUSAO DE DADOS DOS CLIENTES"
           DISPLAY "V - VOLTAR AO MENU PRINCIPAL"
           ACCEPT WS-ESCOLHA-CLIENTES.

           EVALUATE WS-ESCOLHA-CLIENTES
               WHEN 'I'
                   PERFORM INCLUSAO-CLIENTES
               WHEN 'A'
                   PERFORM ALTERACAO-CLIENTES
               WHEN 'C'
                   PERFORM CONSULTA-CLIENTES
               WHEN 'E'
                   PERFORM EXCLUSAO-CLIENTES
               WHEN 'V'
                   PERFORM MAIN-PROCEDURE
               WHEN OTHER
                   DISPLAY "OPCAO INVALIDA"
                   PERFORM CADASTRO-CLIENTES
            END-EVALUATE.

           INCLUSAO-CLIENTES.
           OPEN I-O CLIENTES.

            IF WS-FS-CLIENTES EQUAL 35 THEN
                OPEN OUTPUT CLIENTES
            END-IF

           PERFORM UNTIL WS-GRAVAR-CLIENTE = "NAO"
               IF WS-FS-CLIENTES EQUAL ZEROS THEN
                   DISPLAY "INFORME O CODIGO DO CLIENTE"
                   ACCEPT CODIGO-CLIENTE
                   DISPLAY "INFORME O NOME DO CLIENTE"
                   ACCEPT NOME
                   DISPLAY "INFORME O RG DO CLIENTE"
                   ACCEPT RG
                   DISPLAY "INFORME O TELEFONE DO CLIENTE"
                   ACCEPT TELEFONE

                   WRITE REG-CLIENTES

                   IF WS-FS-CLIENTES NOT EQUAL ZEROS
                    DISPLAY "ERRO! NAO FOI POSSIVEL GRAVAR O REGISTRO"
                    DISPLAY "FILE STATUS: " WS-FS-CLIENTES
                   ELSE
                    DISPLAY "REGISTRO GRAVADO COM SUCESSO!!"
                   END-IF

                   DISPLAY "DESEJA GRAVAR UM NOVO REGISTRO? SIM OU NAO?"
                   ACCEPT WS-GRAVAR-CLIENTE
               ELSE
                DISPLAY "ERRO AO CRIAR O ARQUIVO"
                DISPLAY "FILE STATUS: " WS-FS-CLIENTES
               END-IF
           END-PERFORM.

           MOVE 'SIM' TO WS-GRAVAR-CLIENTE

           CLOSE CLIENTES.
           PERFORM CADASTRO-CLIENTES.

           EXCLUSAO-CLIENTES.

           OPEN I-O CLIENTES.

            DISPLAY "----- EXCLUIR CLIENTES -----"
            DISPLAY "DIGITE O CODIGO DO CLIENTE"
            ACCEPT CODIGO-CLIENTE

            READ CLIENTES RECORD INTO WS-REG-CLIENTES
               KEY IS CODIGO-CLIENTE
                   INVALID KEY
                       DISPLAY "CODIGO DE CLIENTE INVALIDO"
                   NOT INVALID KEY
                       DISPLAY "CODIGO DO CLIENTE: " WS-CODIGO-CLIENTE
                                 " NOME DO CLIENTE: " WS-NOME
                                 " RG: " WS-RG " TELEFONE: " WS-TELEFONE
                       DISPLAY "DESEJA REALMENTE EXCLUIR O CLIENTE "
                                   NOME "?"
                       ACCEPT WS-DEL-PERMANENTE
                       IF WS-DEL-PERMANENTE EQUAL 'S'
                           MOVE 'S' TO WS-DEL
                       ELSE
                           CLOSE CLIENTES
                           STOP RUN
                       END-IF
            END-READ.

            IF WS-DEL EQUAL 'S'
                DELETE CLIENTES RECORD
                   INVALID KEY DISPLAY "CODIGO DE CLIENTE INVALIDO"
                   NOT INVALID KEY DISPLAY "CLIENTE DELETADO!!"
                END-DELETE
            END-IF.

            CLOSE CLIENTES.

           CONSULTA-CLIENTES.

           MOVE SPACES TO WS-ESCOLHA-CLIENTES

            MOVE "S" TO WS-EOR-CLIENTES

            PERFORM UNTIL WS-EOR-CLIENTES IS NOT = 'S'

            DISPLAY "DIGITE T PARA LISTAR TODOS OS CLIENTES OU "
                     "DIGITE U PARA PROCURAR UM CLIENTE ESPECIFICO "
            ACCEPT WS-ESCOLHA-CLIENTES

            OPEN INPUT CLIENTES

            IF WS-ESCOLHA-CLIENTES IS = 'U' THEN
               DISPLAY "----- CONSULTA DE CLIENTES -----"
               DISPLAY "DIGITE O CODIGO DO CLIENTE"
               ACCEPT CODIGO-CLIENTE

               READ CLIENTES RECORD INTO WS-REG-CLIENTES
                   KEY IS CODIGO-CLIENTE
                       INVALID KEY
                           DISPLAY "CODIGO DE CLIENTE INVALIDO"
                       NOT INVALID KEY
                         DISPLAY "CODIGO DO CLIENTE: " WS-CODIGO-CLIENTE
                                 " NOME DO CLIENTE: " WS-NOME
                                 " RG: " WS-RG " TELEFONE: " WS-TELEFONE
               END-READ

            ELSE IF WS-ESCOLHA-CLIENTES IS = 'T' THEN
                MOVE 'S' TO WS-EOF-CLIENTES
                PERFORM UNTIL WS-EOF-CLIENTES = 'F'
                   READ CLIENTES INTO WS-REG-CLIENTES
                       AT END MOVE 'F' TO WS-EOF-CLIENTES
                       NOT AT END
                         DISPLAY "CODIGO DO CLIENTE: " WS-CODIGO-CLIENTE
                                 " NOME DO CLIENTE: " WS-NOME
                                 " RG: " WS-RG " TELEFONE: " WS-TELEFONE
                   END-READ
                END-PERFORM
                END-IF
            END-IF

            CLOSE CLIENTES

            DISPLAY "DESEJA CONTINUAR ? (S ou N):"
            ACCEPT WS-EOR-CLIENTES

            END-PERFORM.

            PERFORM CADASTRO-CLIENTES.

           ALTERACAO-CLIENTES.



           PERFORM MAIN-PROCEDURE.

       CADASTRO-PRODUTOS.

       CADASTRO-PEDIDOS.


      *      OPEN I-O PRODUTOS.
      *      OPEN I-O PEDIDO-VENDAS.





      *      CLOSE PRODUTOS.
      *      CLOSE PEDIDO-VENDAS.
            STOP RUN.
       END PROGRAM TAREFA-05.
