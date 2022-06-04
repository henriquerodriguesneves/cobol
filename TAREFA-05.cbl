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
           ACCESS                  IS DYNAMIC
           RECORD KEY              IS CODIGO-PRODUTO
           FILE STATUS             IS WS-FS-PRODUTOS.

           SELECT PEDIDO-VENDAS ASSIGN TO "c:/temp/pedido-vendas.txt"
           ORGANIZATION            IS INDEXED
           ACCESS                  IS DYNAMIC
           RECORD KEY              IS CODIGO-PEDIDO
           FILE STATUS             IS WS-FS-PEDIDOS.
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
       77 WS-FS-PEDIDOS            PIC 99.
       77 WS-ESCOLHA-MENU          PIC 99.
       77 WS-ESCOLHA               PIC X(01).
       77 WS-GRAVAR                PIC X(03) VALUE 'SIM'.
       77 WS-EOR                   PIC X(01).
       77 WS-EOF                   PIC X(01) VALUE 'S'.
       77 WS-DEL-PERMANENTE        PIC X VALUE SPACES.
       77 WS-DEL                   PIC X VALUE SPACES.
       77 WS-UPDATE                PIC X VALUE SPACES.

       01 WS-REG-CLIENTES             PIC X(56).
       01  FILLER REDEFINES WS-REG-CLIENTES.
           03 WS-CODIGO-CLIENTE       PIC 9(03).
           03 WS-NOME                 PIC X(25).
           03 WS-RG                   PIC X(15).
           03 WS-TELEFONE             PIC X(13).

       01 WS-REG-PRODUTOS             PIC X(33).
       01 FILLER REDEFINES WS-REG-PRODUTOS.
           03 WS-CODIGO-PRODUTO       PIC 9(03).
           03 WS-PRODUTO              PIC X(25).
           03 WS-PRECO                PIC 9(06)V99.

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
       MOVE ZEROS TO WS-ESCOLHA-MENU

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

*******************************************************************************
       CADASTRO-CLIENTES.
       MOVE SPACES TO WS-ESCOLHA

           DISPLAY "---------- CADASTRO DE CLIENTE ---------"
           DISPLAY "SELECIONE UMA OPCAO ABAIXO"
           DISPLAY "I - INCLUSAO DE DADOS DOS CLIENTES"
           DISPLAY "A - ALTERACAO DE DADOS DOS CLIENTES"
           DISPLAY "C - CONSULTA DE DADOS DOS CLIENTES"
           DISPLAY "E - EXCLUSAO DE DADOS DOS CLIENTES"
           DISPLAY "V - VOLTAR AO MENU PRINCIPAL"
           ACCEPT WS-ESCOLHA.

           EVALUATE WS-ESCOLHA
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
       MOVE 'SIM' TO WS-GRAVAR

           OPEN I-O CLIENTES.

            IF WS-FS-CLIENTES EQUAL 35 THEN
                OPEN OUTPUT CLIENTES
            END-IF

           PERFORM UNTIL WS-GRAVAR = "NAO"
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
                   ACCEPT WS-GRAVAR
               ELSE
                DISPLAY "ERRO AO CRIAR O ARQUIVO"
                DISPLAY "FILE STATUS: " WS-FS-CLIENTES
               END-IF
           END-PERFORM.

           MOVE 'SIM' TO WS-GRAVAR

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

           MOVE SPACES TO WS-ESCOLHA
           MOVE 'S' TO WS-EOF

            MOVE "S" TO WS-EOR

            PERFORM UNTIL WS-EOR IS NOT = 'S'

            DISPLAY "T - LISTAR TODOS OS CLIENTES"
            DISPLAY "U - PROCURAR UM CLIENTE ESPECIFICO"
            ACCEPT WS-ESCOLHA

            OPEN INPUT CLIENTES

            IF WS-ESCOLHA IS = 'U' THEN
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

            ELSE IF WS-ESCOLHA IS = 'T' THEN
                MOVE 'S' TO WS-EOF
                PERFORM UNTIL WS-EOF = 'F'
                   READ CLIENTES INTO WS-REG-CLIENTES
                       AT END MOVE 'F' TO WS-EOF
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
            ACCEPT WS-EOR

            END-PERFORM.

            PERFORM CADASTRO-CLIENTES.

           ALTERACAO-CLIENTES.

       MOVE SPACES TO WS-UPDATE

           OPEN I-O CLIENTES.

            DISPLAY "----- ALTERACAO DE CLIENTES -----"
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
                       MOVE 'S' TO WS-UPDATE
            END-READ.

            IF WS-UPDATE EQUAL 'S'
                DISPLAY "INFORME O NOVO NOME DO CLIENTE"
                ACCEPT NOME
                DISPLAY "INFORME O NOVO RG DO CLIENTE: " NOME
                ACCEPT RG
                DISPLAY "INFORME O NOVO NUMERO DE TELEFONE DO CLIENTE "
                       NOME
                ACCEPT TELEFONE

                REWRITE REG-CLIENTES
                END-REWRITE
            END-IF.


            CLOSE CLIENTES.



           PERFORM CADASTRO-CLIENTES.

*******************************************************************************

       CADASTRO-PRODUTOS.
       MOVE SPACES TO WS-ESCOLHA

           DISPLAY "---------- CADASTRO DE PRODUTOS ---------"
           DISPLAY "SELECIONE UMA OPCAO ABAIXO"
           DISPLAY "I - INCLUSAO DE DADOS DOS PRODUTOS"
           DISPLAY "A - ALTERACAO DE DADOS DOS PRODUTOS"
           DISPLAY "C - CONSULTA DE DADOS DOS PRODUTOS"
           DISPLAY "E - EXCLUSAO DE DADOS DOS PRODUTOS"
           DISPLAY "V - VOLTAR AO MENU PRINCIPAL"
           ACCEPT WS-ESCOLHA.

           EVALUATE WS-ESCOLHA
               WHEN 'I'
                   PERFORM INCLUSAO-PRODUTOS
               WHEN 'A'
                   PERFORM ALTERACAO-PRODUTOS
               WHEN 'C'
                   PERFORM CONSULTA-PRODUTOS
               WHEN 'E'
                   PERFORM EXCLUSAO-PRODUTOS
               WHEN 'V'
                   PERFORM MAIN-PROCEDURE
               WHEN OTHER
                   DISPLAY "OPCAO INVALIDA"
                   PERFORM CADASTRO-PRODUTOS
            END-EVALUATE.

           INCLUSAO-PRODUTOS.
       MOVE 'SIM' TO WS-GRAVAR

           OPEN I-O PRODUTOS.

            IF WS-FS-PRODUTOS EQUAL 35 THEN
                OPEN OUTPUT PRODUTOS
            END-IF

           PERFORM UNTIL WS-GRAVAR = "NAO"
               IF WS-FS-PRODUTOS EQUAL ZEROS THEN
                   DISPLAY "INFORME O CODIGO DO PRODUTO"
                   ACCEPT CODIGO-PRODUTO
                   DISPLAY "INFORME O NOME DO PRODUTO"
                   ACCEPT PRODUTO
                   DISPLAY "INFORME O PRECO DO PRODUTO"
                   ACCEPT PRECO

                   WRITE REG-PRODUTOS

                   IF WS-FS-PRODUTOS NOT EQUAL ZEROS
                    DISPLAY "ERRO! NAO FOI POSSIVEL GRAVAR O REGISTRO"
                    DISPLAY "FILE STATUS: " WS-FS-PRODUTOS
                   ELSE
                    DISPLAY "REGISTRO GRAVADO COM SUCESSO!!"
                   END-IF

                   DISPLAY "DESEJA GRAVAR UM NOVO REGISTRO? SIM OU NAO?"
                   ACCEPT WS-GRAVAR
               ELSE
                DISPLAY "ERRO AO CRIAR O ARQUIVO"
                DISPLAY "FILE STATUS: " WS-FS-PRODUTOS
               END-IF
           END-PERFORM.

           MOVE 'SIM' TO WS-GRAVAR

           CLOSE PRODUTOS.
           PERFORM CADASTRO-PRODUTOS.

           EXCLUSAO-PRODUTOS.

           OPEN I-O PRODUTOS.

            DISPLAY "----- EXCLUIR PRODUTOS -----"
            DISPLAY "DIGITE O CODIGO DO PRODUTO"
            ACCEPT CODIGO-PRODUTO

            READ PRODUTOS RECORD INTO WS-REG-PRODUTOS
               KEY IS CODIGO-PRODUTO
                   INVALID KEY
                       DISPLAY "CODIGO DO PRODUTO INVALIDO"
                   NOT INVALID KEY
                       DISPLAY "CODIGO DO PRODUTO: " WS-CODIGO-PRODUTO
                                 " PRODUTO: " WS-PRODUTO " PRECO: "
                                 PRECO
                       DISPLAY "DESEJA REALMENTE EXCLUIR O PRODUTO "
                                   PRODUTO "?"
                       ACCEPT WS-DEL-PERMANENTE
                       IF WS-DEL-PERMANENTE EQUAL 'S'
                           MOVE 'S' TO WS-DEL
                       ELSE
                           CLOSE PRODUTOS
                           STOP RUN
                       END-IF
            END-READ.

            IF WS-DEL EQUAL 'S'
                DELETE PRODUTOS RECORD
                   INVALID KEY DISPLAY "CODIGO DO PRODUTO INVALIDO"
                   NOT INVALID KEY DISPLAY "PRODUTO DELETADO!!"
                END-DELETE
            END-IF.

            CLOSE PRODUTOS.

           PERFORM CADASTRO-PRODUTOS.

           CONSULTA-PRODUTOS.

           MOVE SPACES TO WS-ESCOLHA
           MOVE 'S' TO WS-EOF

            MOVE "S" TO WS-EOR

            PERFORM UNTIL WS-EOR IS NOT = 'S'

            DISPLAY "T - LISTAR TODOS OS PRODUTOS"
            DISPLAY "U - PROCURAR UM PRODUTO ESPECIFICO"
            ACCEPT WS-ESCOLHA

            OPEN INPUT PRODUTOS

            IF WS-ESCOLHA IS = 'U' THEN
               DISPLAY "----- CONSULTA DE PRODUTOS -----"
               DISPLAY "DIGITE O CODIGO DO PRODUTO"
               ACCEPT CODIGO-PRODUTO

               READ PRODUTOS RECORD INTO WS-REG-PRODUTOS
                   KEY IS CODIGO-PRODUTO
                       INVALID KEY
                           DISPLAY "CODIGO DO PRODUTO INVALIDO"
                       NOT INVALID KEY
                         DISPLAY "CODIGO DO PRODUTO: " WS-CODIGO-PRODUTO
                                 " PRODUTO: " WS-PRODUTO " PRECO: "
                                 PRECO
               END-READ

            ELSE IF WS-ESCOLHA IS = 'T' THEN
                MOVE 'S' TO WS-EOF
                PERFORM UNTIL WS-EOF = 'F'
                   READ PRODUTOS INTO WS-REG-PRODUTOS
                       AT END MOVE 'F' TO WS-EOF
                       NOT AT END
                         DISPLAY "CODIGO DO PRODUTO: " WS-CODIGO-PRODUTO
                                 " PRODUTO: " WS-PRODUTO " PRECO: "
                                 PRECO
                   END-READ
                END-PERFORM
                END-IF
            END-IF

            CLOSE PRODUTOS

            DISPLAY "DESEJA CONTINUAR ? (S ou N):"
            ACCEPT WS-EOR

            END-PERFORM.

            PERFORM CADASTRO-PRODUTOS.

           ALTERACAO-PRODUTOS.

       MOVE SPACES TO WS-UPDATE

           OPEN I-O PRODUTOS.

            DISPLAY "----- ALTERACAO DE PRODUTOS -----"
            DISPLAY "DIGITE O CODIGO DO PRODUTO"
            ACCEPT CODIGO-PRODUTO

            READ PRODUTOS RECORD INTO WS-REG-PRODUTOS
               KEY IS CODIGO-PRODUTO
                   INVALID KEY
                       DISPLAY "CODIGO DO PRODUTO INVALIDO"
                   NOT INVALID KEY
                       DISPLAY "CODIGO DO PRODUTO: " WS-CODIGO-PRODUTO
                                 " PRODUTO: " WS-PRODUTO " PRECO: "
                                 PRECO
                       MOVE 'S' TO WS-UPDATE
            END-READ.

            IF WS-UPDATE EQUAL 'S'
                DISPLAY "INFORME O NOVO NOME DO PRODUTO"
                ACCEPT PRODUTO
                DISPLAY "INFORME O NOVO PRECO DO PRODUTO: " PRODUTO
                ACCEPT PRECO

                REWRITE REG-PRODUTOS
                END-REWRITE
            END-IF.


            CLOSE PRODUTOS.

           PERFORM CADASTRO-PRODUTOS.

*******************************************************************************

       CADASTRO-PEDIDOS.

       MOVE SPACES TO WS-ESCOLHA

           DISPLAY "---------- CADASTRO DE PEDIDOS ---------"
           DISPLAY "SELECIONE UMA OPCAO ABAIXO"
           DISPLAY "I - INCLUSAO DE DADOS DOS PEDIDOS"
           DISPLAY "A - ALTERACAO DE DADOS DOS PEDIDOS"
           DISPLAY "C - CONSULTA DE DADOS DOS PEDIDOS"
           DISPLAY "E - EXCLUSAO DE DADOS DOS PEDIDOS"
           DISPLAY "V - VOLTAR AO MENU PRINCIPAL"
           ACCEPT WS-ESCOLHA.

           EVALUATE WS-ESCOLHA
               WHEN 'I'
                   PERFORM INCLUSAO-PEDIDOS
               WHEN 'A'
                   PERFORM ALTERACAO-PEDIDOS
               WHEN 'C'
                   PERFORM CONSULTA-PEDIDOS
               WHEN 'E'
                   PERFORM EXCLUSAO-PEDIDOS
               WHEN 'V'
                   PERFORM MAIN-PROCEDURE
               WHEN OTHER
                   DISPLAY "OPCAO INVALIDA"
                   PERFORM CADASTRO-PEDIDOS
            END-EVALUATE.

           INCLUSAO-PEDIDOS.
       MOVE 'SIM' TO WS-GRAVAR

           OPEN I-O PEDIDO-VENDAS.

            IF WS-FS-PEDIDOS EQUAL 35 THEN
                OPEN OUTPUT PEDIDO-VENDAS
            END-IF

           PERFORM UNTIL WS-GRAVAR = "NAO"
               IF WS-FS-PEDIDOS EQUAL ZEROS THEN
                   DISPLAY "INFORME O CODIGO DO PEDIDO"
                   ACCEPT CODIGO-PEDIDO
                   DISPLAY "INFORME O CODIGO DO CLIENTE"
                   ACCEPT PV-CODIGO-CLIENTE
                   DISPLAY "INFORME O NOME DO CLIENTE"
                   ACCEPT PV-NOME
                   DISPLAY "INFORME O RG DO CLIENTE"
                   ACCEPT PV-RG
                   DISPLAY "INFORME O TELEFONE DO CLIENTE"
                   ACCEPT PV-TELEFONE
                   DISPLAY "INFORME O CODIGO DO PRODUTO"
                   ACCEPT PV-CODIGO-PRODUTO
                   DISPLAY "INFORME O NOME DO PRODUTO"
                   ACCEPT PV-PRODUTO
                   DISPLAY "INFORME O PRECO DO PRODUTO"
                   ACCEPT PV-PRECO
                   MOVE PV-PRECO TO WS-PV-PRECO
                   DISPLAY "INFORME A QUANTIDADE"
                   ACCEPT PV-QTDE
                   MOVE PV-QTDE TO WS-PV-QTDE
                   COMPUTE WS-PV-VALOR-TOTAL =
                                              (WS-PV-QTDE * WS-PV-PRECO)

                   WRITE REG-PEDIDO-VENDAS

                   IF WS-FS-PEDIDOS NOT EQUAL ZEROS
                    DISPLAY "ERRO! NAO FOI POSSIVEL GRAVAR O REGISTRO"
                    DISPLAY "FILE STATUS: " WS-FS-PEDIDOS
                   ELSE
                    DISPLAY "REGISTRO GRAVADO COM SUCESSO!!"
                   END-IF

                   DISPLAY "DESEJA GRAVAR UM NOVO REGISTRO? SIM OU NAO?"
                   ACCEPT WS-GRAVAR
               ELSE
                DISPLAY "ERRO AO CRIAR O ARQUIVO"
                DISPLAY "FILE STATUS: " WS-FS-PEDIDOS
               END-IF
           END-PERFORM.

           MOVE 'SIM' TO WS-GRAVAR

           CLOSE PEDIDO-VENDAS.
           PERFORM CADASTRO-PEDIDOS.

           EXCLUSAO-PEDIDOS.

           OPEN I-O PEDIDO-VENDAS.

            DISPLAY "----- EXCLUIR PEDIDOS -----"
            DISPLAY "DIGITE O CODIGO DO PEDIDO"
            ACCEPT CODIGO-PEDIDO

            READ PEDIDO-VENDAS RECORD INTO WS-REG-PEDIDO-VENDAS
               KEY IS CODIGO-PEDIDO
                   INVALID KEY
                       DISPLAY "CODIGO DO PEDIDO INVALIDO"
                   NOT INVALID KEY
                       DISPLAY "CODIGO DO PEDIDO: " WS-CODIGO-PEDIDO
                                 " CLIENTE: " WS-PV-NOME " PRODUTO: "
                                 WS-PV-PRODUTO " TOTAL: "
                                 WS-PV-VALOR-TOTAL
                       DISPLAY "DESEJA REALMENTE EXCLUIR O PEDIDO "
                                   CODIGO-PEDIDO "?"
                       ACCEPT WS-DEL-PERMANENTE
                       IF WS-DEL-PERMANENTE EQUAL 'S'
                           MOVE 'S' TO WS-DEL
                       ELSE
                           CLOSE PEDIDO-VENDAS
                           STOP RUN
                       END-IF
            END-READ.

            IF WS-DEL EQUAL 'S'
                DELETE PEDIDO-VENDAS RECORD
                   INVALID KEY DISPLAY "CODIGO DO PEDIDO INVALIDO"
                   NOT INVALID KEY DISPLAY "PEDIDO DELETADO!!"
                END-DELETE
            END-IF.

            CLOSE PEDIDO-VENDAS.

           PERFORM CADASTRO-PEDIDOS.

           CONSULTA-PEDIDOS.

           MOVE SPACES TO WS-ESCOLHA
           MOVE 'S' TO WS-EOF

            MOVE "S" TO WS-EOR

            PERFORM UNTIL WS-EOR IS NOT = 'S'

            DISPLAY "T - LISTAR TODOS OS PEDIDOS"
            DISPLAY "U - PROCURAR UM PEDIDO ESPECIFICO"
            ACCEPT WS-ESCOLHA

            OPEN INPUT PEDIDO-VENDAS

            IF WS-ESCOLHA IS = 'U' THEN
               DISPLAY "----- CONSULTA DE PEDIDOS -----"
               DISPLAY "DIGITE O CODIGO DO PEDIDO"
               ACCEPT CODIGO-PEDIDO

               READ PEDIDO-VENDAS RECORD INTO WS-REG-PEDIDO-VENDAS
                   KEY IS CODIGO-PEDIDO
                       INVALID KEY
                           DISPLAY "CODIGO DO PEDIDO INVALIDO"
                       NOT INVALID KEY
                         DISPLAY "CODIGO DO PEDIDO: " WS-CODIGO-PEDIDO
                                 " CLIENTE: " WS-PV-NOME " PRODUTO: "
                                 WS-PV-PRODUTO " TOTAL: "
                                 WS-PV-VALOR-TOTAL
               END-READ

            ELSE IF WS-ESCOLHA IS = 'T' THEN
                MOVE 'S' TO WS-EOF
                PERFORM UNTIL WS-EOF = 'F'
                   READ PEDIDO-VENDAS INTO WS-REG-PEDIDO-VENDAS
                       AT END MOVE 'F' TO WS-EOF
                       NOT AT END
                         DISPLAY "CODIGO DO PEDIDO: " WS-CODIGO-PEDIDO
                                 " CLIENTE: " WS-PV-NOME " PRODUTO: "
                                 WS-PV-PRODUTO " TOTAL: "
                                 WS-PV-VALOR-TOTAL
                   END-READ
                END-PERFORM
                END-IF
            END-IF

            CLOSE PEDIDO-VENDAS

            DISPLAY "DESEJA CONTINUAR ? (S ou N):"
            ACCEPT WS-EOR

            END-PERFORM.

            PERFORM CADASTRO-PEDIDOS.

           ALTERACAO-PEDIDOS.

       MOVE SPACES TO WS-UPDATE

           OPEN I-O PEDIDO-VENDAS.

            DISPLAY "----- ALTERACAO DE PEDIDOS -----"
            DISPLAY "DIGITE O CODIGO DO PEDIDO"
            ACCEPT CODIGO-PEDIDO

            READ PEDIDO-VENDAS RECORD INTO WS-REG-PEDIDO-VENDAS
               KEY IS CODIGO-PEDIDO
                   INVALID KEY
                       DISPLAY "CODIGO DO PEDIDO INVALIDO"
                   NOT INVALID KEY
                       DISPLAY "CODIGO DO PEDIDO: " WS-CODIGO-PEDIDO
                                 " CLIENTE: " WS-PV-NOME " PRODUTO: "
                                 WS-PV-PRODUTO " TOTAL: "
                                 WS-PV-VALOR-TOTAL
                       MOVE 'S' TO WS-UPDATE
            END-READ.

            IF WS-UPDATE EQUAL 'S'
                DISPLAY "INFORME O NOVO PRECO DO PRODUTO"
                ACCEPT PV-PRECO
                DISPLAY "INFORME A NOVA QUANTIDADE DO PEDIDO: "
                                                   CODIGO-PEDIDO
                ACCEPT PV-QTDE
                COMPUTE PV-VALOR-TOTAL = PV-QTDE * PV-PRECO

                REWRITE REG-PEDIDO-VENDAS
                END-REWRITE
            END-IF.
            CLOSE PEDIDO-VENDAS.

           PERFORM CADASTRO-PEDIDOS.
            STOP RUN.
       END PROGRAM TAREFA-05.
