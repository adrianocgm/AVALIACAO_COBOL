      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GERENCIADOR.
      ******************************************************************
      *                                                                *
      * FILES......: DESCRICAO...................: TIPO.....:          *
      *                                                                *
      *                                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                                                                *
      ******************************************************************
      *
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
      *  SPECIAL-NAMES.
      *    DECIMAL-POINT IS COMMA.
      *
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      *
           SELECT ARQCLI01
           ASSIGN TO DISK WID-ARQ-CLIENTE
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           RECORD KEY   IS EXEMPLO-DADOS
                                       OF ARQCLI01
           LOCK MODE    IS MANUAL
           FILE STATUS  IS WRK-FS-ARQCLI01.
      *
           SELECT ARQVEN01
           ASSIGN TO DISK WID-ARQ-VENDEDOR
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           RECORD KEY   IS EXEMPLO-DADOS
                                       OF ARQVEN01
           LOCK MODE    IS MANUAL
           FILE STATUS  IS WRK-FS-ARQVEN01.
      *
           SELECT ARQIMP01
           ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EXEMPLO-DADOS OF ARQIMP01
           FILE STATUS IS WRK-FS-ARQIMP01.
      *
      *    RELATORIO DE CLIENTES
           SELECT RELCLI01
              ASSIGN TO DISK
              FILE STATUS  IS STATUS-WS.
      *
      *    RELATORIO DE CLIENTES
           SELECT RELVEN01
              ASSIGN TO DISK
              FILE STATUS  IS STATUS-WS.

      *
      *
       DATA DIVISION.
        FILE SECTION.
      *
         FD ARQCLI01
            COPY "COPYBOOK\CAD-SIST.CPY".
      *
         FD ARQVEN01
            COPY "COPYBOOK\CAD-SIST.CPY".
      *
         FD ARQIMP01
            VALUE OF FILE-ID IS WS-NOME-ARQ-IMPORT
            COPY "COPYBOOK\CAD-SIST.CPY".
      *
         FD RELCLI01
            LABEL RECORD IS OMITTED
            VALUE OF FILE-ID IS 'C:\GERENC\RELATORIO_CLIENTE.TXT'.
      *     VALUE OF FILE-ID IS NOME-REL-PARAM-S-WS.
            01 REG-RELCLI01                 PIC  X(132).
      *
         FD RELVEN01
            LABEL RECORD IS OMITTED
            VALUE OF FILE-ID IS 'C:\GERENC\RELATORIO_VENDEDOR.TXT'.
      *     VALUE OF FILE-ID IS NOME-REL-PARAM-S-WS.
            01 REG-RELVEN01                 PIC  X(133).
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           '* AREA PARA TESTE DE FILE-STATUS *'.
      *----------------------------------------------------------------*
      *
       01  WRK-FS-ARQCLI01             PIC X(002)      VALUE SPACES.
           88  WRK-ARQCLI01-OK                         VALUE '00' '05'.
           88  WRK-ARQCLI01-FIM                        VALUE '10'.
       01  WRK-FS-ARQVEN01             PIC X(002)      VALUE SPACES.
           88  WRK-ARQVEN01-OK                         VALUE '00' '05'.
           88  WRK-ARQVEN01-FIM                        VALUE '10'.
       01  WRK-FS-ARQIMP01             PIC X(002)      VALUE SPACES.
           88  WRK-ARQIMP01-OK                         VALUE '00' '05'.
           88  WRK-ARQIMP01-FIM                        VALUE '10'.
      *
      *---FILE STATUS DE ARQUIVOS
       01 VARIAVEIS-FS-ARQUIVOS.
          05 STATUS-WS                 PIC  X(002) VALUE ZEROS.
             88 STATUS-OK                          VALUE "00" "05".
          05 REDEFINES STATUS-WS.
             10 FILE-STATUS1           PIC  X(001).
             10 FILE-STATUS2           PIC  X(001).
             10 REDEFINES FILE-STATUS2 PIC  9(002) COMP-X.

      *
      *----------------------------------------------------------------*
       77 FILLER                       PIC  X(050)     VALUE
           '* AREA DE BOOKS FUNCIONAIS E AUXILIARES *'.
      *---------------------------------------------------------------*
      *
           COPY 'COPYBOOK\COMUNIC-FS'.

           COPY 'COPYBOOK\AREA-COMUNC'.
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
       01 WID-ARQ-CLIENTE.
           05 FILLER                   PIC  X(010)     VALUE
                                                           'C:\GERENC\'.
           05 WRK-NOME-ARQUIVO         PIC  X(012)     VALUE
                                                      'ARQCLI01.DAT'.
       01 WID-ARQ-VENDEDOR.
           05 FILLER                   PIC  X(010)     VALUE
                                                           'C:\GERENC\'. 
           05 WRK-NOME-ARQUIVO         PIC  X(012)     VALUE
                                                       'ARQVEN01.DAT'.
       01 VARIAVEIS-WS.
         05 WS-NOME-ARQ-IMPORT         PIC X(080)        VALUE SPACES.

         05 STATUS-ARQ-WS              PIC X(002) COMP-5.
      *
       01 VARIAVEIS-TELA-WS.
         05 WS-AUX-COD-CLI-TELA        PIC X(007).
         05 WS-AUX-COD-VEN-TELA        PIC X(003).
         05 WS-AUX-LATITUDE-TELA       PIC X(011).
         05 WS-AUX-LONGITUDE-TELA      PIC X(011).
      *
       01 WRK-CHV-INVALID-FLAG         PIC X(001).
          88  WRK-CHV-INVALID                        VALUE 'S'.
          88  WRK-VALID-KEY                          VALUE 'N'.
      *
       01 WS-OPCAO-TELA                PIC X(001)    VALUE SPACES.
           88 WRK-OP-INCLUIR                         VALUE "1".
           88 WRK-OP-INCLUIR-CLI                     VALUE "1".
           88 WRK-OP-INCLUIR-VEN                     VALUE "5".
      *
           88 WRK-OP-RELATORIO                       VALUE "2".
           88 WRK-OP-ALTERAR-CLI                     VALUE "2".
           88 WRK-OP-ALTERAR-VEN                     VALUE "6".
      *
           88 WRK-OP-IMPORTAR-CLI                    VALUE "3".
           88 WRK-OP-IMPORTAR-VEN                    VALUE "7".

           88 WRK-OP-EXECUTAR                        VALUE "3".
           88 WRK-OP-EXCLUIR                         VALUE "4"
                                                           "8".
           88 WRK-OP-ENCERRAR                        VALUE "x" "X".

       01 WS-AUX-REG.
           05 WS-AUX-DADOS            PIC X(021).
           05 WS-AUX-REG01-CLIENTE REDEFINES WS-AUX-DADOS.
               10 WS-AUX-CODIGO-CLI   PIC  9(007).
               10 WS-AUX-NR-CNPJ      PIC  X(014).
           05 WS-AUX-REG01-VENDEDR REDEFINES WS-AUX-DADOS.
              10 WS-AUX-CODIGO-VEND   PIC  9(003).
              10 WS-AUX-NR-CPF        PIC  X(011).
              10 FILLER               PIC  X(007).
           05 WS-AUX-RAZAO-SOCIAL     PIC  X(040).
           05 WS-AUX-LATITUDE         PIC S9(003)V9(008).
           05 WS-AUX-LONGITUDE        PIC S9(003)V9(008).

       01 WS-AUX-REG-UPT.
           05 WS-AUX-DADOS-UPT         PIC X(021).
           05 WS-AUX-REG01-CLIENTE-UPT REDEFINES WS-AUX-DADOS-UPT.
               10 WS-AUX-CODIGO-CLI-UPT
                                       PIC  9(007).
               10 WS-AUX-NR-CPF-CNPJ-UPT
                                       PIC  X(014).
           05 WS-AUX-REG01-VENDEDR-UPT REDEFINES WS-AUX-DADOS-UPT.
              10 WS-AUX-CODIGO-VEND-UPT
                                       PIC  9(003).
              10 WS-AUX-NR-CPF-CNPJ-UPT
                                       PIC  X(011).
              10 FILLER                PIC  X(007).
           05 WS-AUX-RAZAO-SOCIAL-UPT PIC  X(040).
           05 WS-AUX-LATITUDE-UPT     PIC S9(003)V9(008).

           05 WS-AUX-LONGITUDE-UPT    PIC S9(003)V9(008).
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
           'AREA PARA TELA SISTEMA'.
      *----------------------------------------------------------------*
       SCREEN SECTION.
      *----------------------------------------------------------------*

       01 CLEAR-SCREEN.
           05 BLANK SCREEN BACKGROUND-COLOR 3 FOREGROUND-COLOR 7.
      *----------------------------------------------------------------*
      *    MENU INICIAL.
      *----------------------------------------------------------------*
       01 MENU-TELA-01.
           05 BACKGROUND-COLOR 5 FOREGROUND-COLOR 7.
           05 LINE 6 COLUMN 22 HIGHLIGHT  '01.00 - CADASTRO...: 1'.
           05 LINE 7 COLUMN 22 HIGHLIGHT  '02.00 - RELATORIO..: 2'.
           05 LINE 8 COLUMN 22 HIGHLIGHT  '03.00 - EXECUTAR...: 3'.
           05 LINE 9 COLUMN 22 HIGHLIGHT  '03.00 - ENCERRAR...: X'.
           05 LINE 11 COLUMN 22 HIGHLIGHT 'OPCAO....: '.
           05 OPCAO-TELA REVERSE-VIDEO PIC X(01)
           USING WS-OPCAO-TELA.
       01 MENU-TELA-02.
           05 BACKGROUND-COLOR 5 FOREGROUND-COLOR 7.
           05 LINE 6  COLUMN 22 HIGHLIGHT 'CADASTRO CLIENTE.:'.
           05 LINE 7  COLUMN 24 HIGHLIGHT 'INCLUIR  CLIENTE.......: 1'.
           05 LINE 8  COLUMN 24 HIGHLIGHT 'ALTERAR  CLIENTE.......: 2'.
           05 LINE 9  COLUMN 24 HIGHLIGHT 'IMPORTAR ARQ. CLIENTE..: 3'.
           05 LINE 10 COLUMN 24 HIGHLIGHT 'EXCLUIR  CLIENTE.......: 4'.
           
           05 LINE 11 COLUMN 22 HIGHLIGHT 'CADASTRO VENDEDOR:'.
           05 LINE 12 COLUMN 24 HIGHLIGHT 'INCLUIR  VENDEDOR......: 5'.
           05 LINE 13 COLUMN 24 HIGHLIGHT 'ALTERAR  VENDEDOR......: 6'.
           05 LINE 14 COLUMN 24 HIGHLIGHT 'IMPORTAR ARQ. CLIENTE..: 7'.
           05 LINE 15 COLUMN 24 HIGHLIGHT 'EXCLUIR  VENDEDOR......: 8'.           
           05 LINE 17 COLUMN 22 HIGHLIGHT 'ENCERRAR...............: X'.
      *
           05 LINE 19 COLUMN 22 HIGHLIGHT 'OPCAO....: '.
           05 OPCAO-TELA REVERSE-VIDEO PIC X(01)
           USING WS-OPCAO-TELA.
       01 MENU-TELA-03.
           05 BACKGROUND-COLOR 5 FOREGROUND-COLOR 7.
           05 LINE 6 COLUMN 22 HIGHLIGHT 'RELATORIO CLIENTE.: 1'.
           05 LINE 7 COLUMN 22 HIGHLIGHT 'RELATORIO VENDEDOR: 2'.
           05 LINE 8 COLUMN 22 HIGHLIGHT 'OPCAO....: '.
           05 OPCAO-TELA REVERSE-VIDEO PIC X(01)
           USING WS-OPCAO-TELA.

       01 TELA-CLI-CAD AUTO.
           05 BACKGROUND-COLOR 5 FOREGROUND-COLOR 7.
           05 LINE 6 COLUMN 22 HIGHLIGHT '     CODIGO.: '.
           05 CODIGO-TELA REVERSE-VIDEO
                                       PIC X(07)
           USING WS-AUX-COD-CLI-TELA.
      *
           05 LINE 7 COLUMN 22 HIGHLIGHT '        CNPJ: '.
           05 CNPJ-TELA REVERSE-VIDEO  PIC X(14)
           USING WS-AUX-NR-CNPJ.
      *
           05 LINE 8 COLUMN 22 HIGHLIGHT 'RAZAO SOCIAL: '.
           05 RAZAO-TELA REVERSE-VIDEO PIC X(40)
           USING WS-AUX-RAZAO-SOCIAL.

           05 LINE 9 COLUMN 22 HIGHLIGHT '    LATITUDE: '.
           05 LATITUDE-TELA REVERSE-VIDEO
                                       PIC X(011)
           USING WS-AUX-LATITUDE-TELA.
       
           05 LINE 10 COLUMN 22 HIGHLIGHT '   LONGITUDE: '.
           05 LONGITUDE-TELA REVERSE-VIDEO
                                       PIC X(011)
           USING WS-AUX-LONGITUDE-TELA.

       01 TELA-CLI-DEL AUTO.
           05 BACKGROUND-COLOR 5 FOREGROUND-COLOR 7.
           05 LINE 6 COLUMN 22 HIGHLIGHT '     CODIGO.: '.
           05 CODIGO-TELA REVERSE-VIDEO
                                       PIC X(07)
           USING WS-AUX-COD-CLI-TELA.
      *
           05 LINE 7 COLUMN 22 HIGHLIGHT '        CNPJ: '.
           05 CNPJ-TELA REVERSE-VIDEO  PIC X(14)
           USING WS-AUX-NR-CNPJ.

       01 TELA-VENDR-CAD AUTO.
           05 BACKGROUND-COLOR 5 FOREGROUND-COLOR 7.
           05 LINE 6 COLUMN 22 HIGHLIGHT '      CODIGO.: '.
           05 CODIGO-TELA REVERSE-VIDEO
                                       PIC X(03)
           USING WS-AUX-COD-VEN-TELA.
      *
           05 LINE 7 COLUMN 22 HIGHLIGHT '         CPF.: '.
           05 CNPJ-TELA REVERSE-VIDEO  PIC X(11)
           USING WS-AUX-NR-CPF.
      *
           05 LINE 8 COLUMN 22 HIGHLIGHT 'NOME VENDEDOR: '.
           05 RAZAO-TELA REVERSE-VIDEO PIC X(40)
           USING WS-AUX-RAZAO-SOCIAL.

           05 LINE 9 COLUMN 22 HIGHLIGHT '     LATITUDE: '.
           05 LATITUDE-TELA REVERSE-VIDEO
                                       PIC X(011)
           USING WS-AUX-LATITUDE-TELA.

           05 LINE 10 COLUMN 22 HIGHLIGHT '   LONGITUDE: '.
           05 LONGITUDE-TELA REVERSE-VIDEO
                                        PIC X(011)
           USING WS-AUX-LONGITUDE-TELA.

       01 TELA-IMPORT-CAD AUTO.
           05 BACKGROUND-COLOR 5 FOREGROUND-COLOR 7.
           05 LINE 6 COLUMN 2 HIGHLIGHT 'CAMINHO.: '.
           05 CODIGO-TELA REVERSE-VIDEO
                                       PIC X(80)
           USING WS-NOME-ARQ-IMPORT.

       01 TELA-ERRO AUTO.
      *
           05 BACKGROUND-COLOR 5 FOREGROUND-COLOR 7.
           05 LINE 6 COLUMN 2 HIGHLIGHT 'SAIDA: ' .
           05 ERRO-MSG       REVERSE-VIDEO
                                       PIC X(080)
           USING MENSAGEM-FS.
      *
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       00-00-INICIALIZAR.
      *----------------------------------------------------------------*
           INITIALIZE VARIAVEIS-WS
                      WS-AUX-REG
                      WRK-FS-ARQCLI01
                      WRK-FS-ARQVEN01.
      *    MOVE 'C:\GERENC\'           TO WID-ARQ-CLIENTE
      *                                   WID-ARQ-VENDEDOR
      *
           PERFORM 00-05-TELA-INICIAL
           .
      *----------------------------------------------------------------*
       00-00-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       00-05-TELA-INICIAL              SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE WS-OPCAO-TELA
           DISPLAY CLEAR-SCREEN
           DISPLAY MENU-TELA-01
           ACCEPT MENU-TELA-01
               EVALUATE TRUE
                   WHEN WRK-OP-INCLUIR
                       PERFORM 00-10-TELA-CADASTRO THRU 00-10-SAIDA
                   WHEN WRK-OP-RELATORIO
                       PERFORM 00-20-TELA-RELATORIO THRU 00-20-SAIDA
                   WHEN WRK-OP-EXECUTAR
                       PERFORM 00-30-TELA-DISTRIBUICAO THRU 00-30-SAIDA
               END-EVALUATE
           IF WRK-OP-ENCERRAR
               PERFORM 99-99-FINALIZA
           ELSE
               PERFORM 00-05-TELA-INICIAL
           END-IF
           .
      *
      *----------------------------------------------------------------*
       00-05-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       00-10-TELA-CADASTRO.
      *----------------------------------------------------------------*
           INITIALIZE WS-OPCAO-TELA
           DISPLAY CLEAR-SCREEN.
           DISPLAY MENU-TELA-02
           ACCEPT MENU-TELA-02
               EVALUATE TRUE
                   WHEN WRK-OP-INCLUIR-CLI 
                       PERFORM 20-10-CAD-CLIENTE THRU 20-10-SAIDA
                   WHEN WRK-OP-ALTERAR-CLI
                       PERFORM 20-30-ATUALIZA-CLI THRU 20-30-SAIDA
                   WHEN WRK-OP-IMPORTAR-CLI
                       PERFORM 20-40-IMPORT-ARQUIVO-CLI
                                       THRU 20-40-SAIDA
                   WHEN WRK-OP-INCLUIR-VEN 
                       PERFORM 20-50-CAD-VENDEDOR THRU 20-50-SAIDA
                   WHEN WRK-OP-ALTERAR-VEN
                       PERFORM 20-70-ATUALIZA-VEND THRU 20-70-SAIDA
                   WHEN WRK-OP-IMPORTAR-VEN
                       PERFORM 20-40-IMPORT-ARQUIVO-CLI
                                       THRU 20-40-SAIDA
                   WHEN WRK-OP-EXCLUIR
                       PERFORM 20-20-EXCLUI-CLI THRU 20-20-SAIDA
               END-EVALUATE
           
           .
      *----------------------------------------------------------------*
       00-10-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       00-20-TELA-RELATORIO.
      *----------------------------------------------------------------*
           
           INITIALIZE WS-OPCAO-TELA
           DISPLAY CLEAR-SCREEN.
           DISPLAY MENU-TELA-03
           ACCEPT MENU-TELA-03
      *        EVALUATE TRUE
      *            WHEN WRK-OP-INCLUIR
      *                PERFORM INCLUI THRU INCLUI-FIM
      *            WHEN WRK-OP-CONSULTAR
      *                PERFORM CONSULTA THRU CONSULTA-FIM
      *            WHEN WRK-OP-ALTERAR
      *                PERFORM ALTERA THRU ALTERA-FIM
      *            WHEN WRK-OP-EXCLUIR
      *                PERFORM EXCLUI THRU EXCLUI-FIM
      *        END-EVALUATE
           .
      *----------------------------------------------------------------*
       00-20-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       00-30-TELA-DISTRIBUICAO.
      *----------------------------------------------------------------*
           
           INITIALIZE WS-OPCAO-TELA
           DISPLAY CLEAR-SCREEN.
           DISPLAY MENU-TELA-01
           ACCEPT  MENU-TELA-01
           .
      *----------------------------------------------------------------*
       00-30-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      * 
      *----------------------------------------------------------------*
       20-10-CAD-CLIENTE.
      *----------------------------------------------------------------*
      *
           DISPLAY CLEAR-SCREEN
           DISPLAY TELA-CLI-CAD
           ACCEPT  TELA-CLI-CAD
           
           INITIALIZE FILE-STATUS-AREA.

           COMPUTE WS-AUX-CODIGO-CLI = FUNCTION NUMVAL
                                       (WS-AUX-COD-CLI-TELA)
           COMPUTE WS-AUX-LATITUDE   = FUNCTION NUMVAL
                                       (WS-AUX-LATITUDE-TELA)
           COMPUTE WS-AUX-LONGITUDE  = FUNCTION NUMVAL
                                       (WS-AUX-LONGITUDE-TELA)

           PERFORM 50-20-ABRIR-ARQCLI
      *
           MOVE WS-AUX-REG             TO EXEMPLO-REG
                                       OF ARQCLI01
      *
           MOVE 'CL'                   TO COMUNIC-TPO-PESSOA
           MOVE WS-AUX-NR-CNPJ         TO COMUNIC-NR-CNPJ
           CALL 'VALID-CPF-CNPJ'       USING COMUNIC-BLOCO
           CANCEL 'VALID-CPF-CNPJ'
      *
           PERFORM 50-00-GRAVAR-ARQCLI01

           PERFORM 50-40-FECHAR-ARQCLI
           .
      *----------------------------------------------------------------*
       20-10-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       20-20-EXCLUI-CLI                SECTION.
      *----------------------------------------------------------------*
      *    
           INITIALIZE FILE-STATUS-AREA.
      *
           DISPLAY CLEAR-SCREEN
           DISPLAY TELA-CLI-DEL
           ACCEPT  TELA-CLI-DEL
      *
           COMPUTE WS-AUX-CODIGO-CLI = FUNCTION NUMVAL
                                       (WS-AUX-COD-CLI-TELA)
           
           PERFORM 50-20-ABRIR-ARQCLI
      *        
           MOVE WS-AUX-REG01-CLIENTE   TO EXEMPLO-REG01-CLIENTE  
                                       OF ARQCLI01
           MOVE 'N' TO WRK-CHV-INVALID-FLAG
           DELETE ARQCLI01
              INVALID KEY
                  MOVE 'S' TO WRK-CHV-INVALID-FLAG
           END-DELETE
           IF WRK-CHV-INVALID
             DISPLAY 'RECORD NOT FOUND'
           ELSE
              DISPLAY 'RECORD DELETED'
           END-IF
           
           PERFORM 50-40-FECHAR-ARQCLI
           .
      *
      *----------------------------------------------------------------*
       20-20-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       20-30-ATUALIZA-CLI              SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY CLEAR-SCREEN
           DISPLAY TELA-CLI-CAD
           ACCEPT  TELA-CLI-CAD
           
           INITIALIZE FILE-STATUS-AREA.

           COMPUTE WS-AUX-CODIGO-CLI = FUNCTION NUMVAL
                                       (WS-AUX-COD-CLI-TELA)
           COMPUTE WS-AUX-LATITUDE   = FUNCTION NUMVAL
                                       (WS-AUX-LATITUDE-TELA)
           COMPUTE WS-AUX-LONGITUDE  = FUNCTION NUMVAL
                                       (WS-AUX-LONGITUDE-TELA)
           PERFORM 50-20-ABRIR-ARQCLI

           MOVE 'N' TO WRK-CHV-INVALID-FLAG
           MOVE WS-AUX-REG01-VENDEDR   TO EXEMPLO-REG01-CLIENTE
                                       OF ARQCLI01
           READ ARQVEN01
              INVALID KEY
                  MOVE 'S' TO WRK-CHV-INVALID-FLAG
           END-READ
      *   
           MOVE 'N' TO WRK-CHV-INVALID-FLAG
           MOVE WS-AUX-REG-UPT         TO EXEMPLO-REG
                                       OF ARQCLI01
           REWRITE EXEMPLO-REG OF ARQCLI01
               INVALID KEY
                   MOVE 'S' TO WRK-CHV-INVALID-FLAG
           END-REWRITE
               
           PERFORM 50-40-FECHAR-ARQCLI
           .
      *
      *----------------------------------------------------------------*
       20-30-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       20-40-IMPORT-ARQUIVO-CLI        SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY CLEAR-SCREEN
           DISPLAY TELA-IMPORT-CAD
           ACCEPT  TELA-IMPORT-CAD           
       
           CALL "CBL_CHECK_FILE_EXIST" USING WS-NOME-ARQ-IMPORT
                        RETURNING STATUS-ARQ-WS      

           CALL "CBL_COPY_FILE" USING WS-NOME-ARQ-IMPORT
                                      WID-ARQ-CLIENTE                                         
                               RETURNING STATUS-ARQ-WS
           IF STATUS-ARQ-WS            NOT EQUAL ZEROS
                MOVE STATUS-ARQ-WS      TO STATUS-CODE-02
                PERFORM 50-90-DISPLAY-FS
           END-IF                    
           .
      *
      *----------------------------------------------------------------*
       20-40-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       20-50-CAD-VENDEDOR.
      *----------------------------------------------------------------*
      *
           INITIALIZE FILE-STATUS-AREA.
           DISPLAY CLEAR-SCREEN
           DISPLAY TELA-VENDR-CAD
           ACCEPT  TELA-VENDR-CAD
      *
           PERFORM 50-30-ABRIR-ARQVEN
      *
           MOVE WS-AUX-REG             TO EXEMPLO-REG
                                       OF ARQVEN01
           MOVE 'VD'                   TO COMUNIC-TPO-PESSOA
           MOVE WS-AUX-NR-CPF          TO COMUNIC-NR-CPF
           CALL   'VALID-CPF-CNPJ'     USING COMUNIC-BLOCO
           CANCEL 'VALID-CPF-CNPJ'
           IF COMUNIC-COD-RETORNO      NOT EQUAL ZEROS
               MOVE COMUNIC-COD-RETORNO
                                       TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           ELSE
               PERFORM 50-10-GRAVAR-ARQVEN01
           END-IF
           
           PERFORM 50-50-FECHAR-ARQVEN
           .
      *----------------------------------------------------------------*
       20-50-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       20-60-EXCLLUI-VEND              SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQVEN01
           IF WRK-FS-ARQVEN01          NOT EQUAL ZEROS
               MOVE WRK-FS-ARQVEN01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           END-IF

           MOVE WS-AUX-REG01-VENDEDR   TO EXEMPLO-REG01-VENDEDR
                                       OF ARQVEN01
           MOVE 'N' TO WRK-CHV-INVALID-FLAG.
           DELETE ARQVEN01
              INVALID KEY
                  MOVE 'S' TO WRK-CHV-INVALID-FLAG
           END-DELETE
      *
           IF WRK-CHV-INVALID
             DISPLAY 'RECORD NOT FOUND'
           ELSE
              DISPLAY 'RECORD DELETED'
           END-IF.
      *
           CLOSE ARQVEN01
           IF WRK-FS-ARQVEN01          NOT EQUAL ZEROS
               MOVE WRK-FS-ARQVEN01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           END-IF
           .
      *----------------------------------------------------------------*
       20-60-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       20-70-ATUALIZA-VEND             SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 'N' TO WRK-CHV-INVALID-FLAG.
           MOVE WS-AUX-REG01-VENDEDR   TO EXEMPLO-REG01-VENDEDR
                                       OF ARQVEN01
           READ ARQVEN01
              INVALID KEY
                  MOVE 'S' TO WRK-CHV-INVALID-FLAG
           END-READ
          
           MOVE 'N' TO WRK-CHV-INVALID-FLAG.
           MOVE WS-AUX-REG-UPT         TO EXEMPLO-REG
                                       OF ARQVEN01
           REWRITE EXEMPLO-REG OF ARQVEN01
               INVALID KEY
                   MOVE 'S' TO WRK-CHV-INVALID-FLAG
           END-REWRITE
           .
      *
      *----------------------------------------------------------------*
       20-70-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       20-80-IMPORT-ARQUIVO-CLI        SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY CLEAR-SCREEN
           DISPLAY TELA-IMPORT-CAD
           ACCEPT  TELA-IMPORT-CAD           
       
           CALL "CBL_CHECK_FILE_EXIST" USING WS-NOME-ARQ-IMPORT
                        RETURNING STATUS-ARQ-WS      

           CALL "CBL_COPY_FILE" USING WS-NOME-ARQ-IMPORT
                                      WID-ARQ-CLIENTE                              
                               RETURNING STATUS-ARQ-WS
           IF STATUS-ARQ-WS            NOT EQUAL ZEROS
                MOVE STATUS-ARQ-WS      TO STATUS-CODE-02
                PERFORM 50-90-DISPLAY-FS
           END-IF                    
           .
      *
      *----------------------------------------------------------------*
       20-80-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       30-10-RELAT-CLIENTE.
      *----------------------------------------------------------------*

           .
      *----------------------------------------------------------------*
       30-10-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       30-20-RELAT-VENDEDOR.
      *----------------------------------------------------------------*

           .
      *----------------------------------------------------------------*
       30-20-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       40-00-PROCS-DISTRB.
      *----------------------------------------------------------------*

           .
      *----------------------------------------------------------------*
       40-00-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
*
      *----------------------------------------------------------------*
       40-10-DISTRIBUCAO-CLI.
      *----------------------------------------------------------------*

           .
      *----------------------------------------------------------------*
       40-10-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       50-00-GRAVAR-ARQCLI01.
      *----------------------------------------------------------------*
      *
           WRITE EXEMPLO-REG           OF ARQCLI01.
           IF WRK-FS-ARQCLI01          NOT EQUAL ZEROS
               MOVE WRK-FS-ARQCLI01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           ELSE
               INITIALIZE VARIAVEIS-TELA-WS
           END-IF
           .
      *
      *----------------------------------------------------------------*
       50-00-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       50-10-GRAVAR-ARQVEN01.
      *----------------------------------------------------------------*
      *
           WRITE EXEMPLO-REG           OF ARQVEN01.
           IF WRK-FS-ARQVEN01          NOT EQUAL ZEROS
               MOVE WRK-FS-ARQVEN01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           ELSE
               INITIALIZE VARIAVEIS-TELA-WS
           END-IF
           .
      *
      *----------------------------------------------------------------*
       50-10-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       50-20-LER-ARQCLI01.
      *----------------------------------------------------------------*
      *
           READ ARQCLI01.
           IF WRK-FS-ARQCLI01          NOT EQUAL ZEROS
               MOVE WRK-FS-ARQCLI01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           END-IF
           .
      *
      *----------------------------------------------------------------*
       50-20-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       50-30-LER-ARQVEN01.
      *----------------------------------------------------------------*
      *
           READ ARQVEN01.
           IF WRK-FS-ARQVEN01          NOT EQUAL ZEROS
               MOVE WRK-FS-ARQVEN01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           END-IF
           .
      *
      *----------------------------------------------------------------*
       50-10-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       50-20-ABRIR-ARQCLI              SECTION.
      *----------------------------------------------------------------*
           OPEN I-O ARQCLI01
           IF NOT WRK-ARQCLI01-OK
               MOVE WRK-FS-ARQCLI01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           END-IF
           .
      *----------------------------------------------------------------*
       50-20-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       50-30-ABRIR-ARQVEN              SECTION.
      *----------------------------------------------------------------*
           OPEN I-O ARQVEN01
           IF NOT WRK-ARQVEN01-OK
               MOVE WRK-FS-ARQVEN01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           END-IF
           .
      *----------------------------------------------------------------*
       50-30-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *    
      *----------------------------------------------------------------*
       50-40-FECHAR-ARQCLI              SECTION.
      *----------------------------------------------------------------*
           CLOSE ARQCLI01
           IF WRK-FS-ARQCLI01          NOT EQUAL ZEROS
               MOVE WRK-FS-ARQCLI01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           END-IF
           .
      *----------------------------------------------------------------*
       50-40-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *    
      *----------------------------------------------------------------*
       50-50-FECHAR-ARQVEN              SECTION.
      *----------------------------------------------------------------*
           CLOSE ARQVEN01
           IF WRK-FS-ARQVEN01          NOT EQUAL ZEROS
               MOVE WRK-FS-ARQVEN01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           END-IF
           .
      *----------------------------------------------------------------*
       50-50-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       50-60-ABRIR-ARQIMP              SECTION.
      *----------------------------------------------------------------*
           OPEN I-O ARQIMP01
           IF NOT WRK-ARQIMP01-OK
               MOVE WRK-FS-ARQIMP01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           END-IF
           .
      *----------------------------------------------------------------*
       50-60-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *    
      *----------------------------------------------------------------*
       50-70-FECHAR-ARQIMP              SECTION.
      *----------------------------------------------------------------*
           CLOSE ARQIMP01
           IF WRK-FS-ARQIMP01          NOT EQUAL ZEROS
               MOVE WRK-FS-ARQIMP01    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           END-IF
           .
      *----------------------------------------------------------------*
       50-40-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       50-90-DISPLAY-FS                SECTION.
      *----------------------------------------------------------------*
      *
           CALL 'TRATA-FS'             USING FILE-STATUS-AREA
           DISPLAY CLEAR-SCREEN
           DISPLAY TELA-ERRO
           ACCEPT TELA-ERRO
           PERFORM 00-05-TELA-INICIAL
           .
      *
      *----------------------------------------------------------------*
       50-90-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
        99-98-COPIA-ARQUIVO-VEN        SECTION.
      *----------------------------------------------------------------*
      *
           ACCEPT WS-NOME-ARQ-IMPORT

           CALL "CBL_COPY_FILE"        USING WS-NOME-ARQ-IMPORT
                                             WID-ARQ-VENDEDOR
                                       RETURNING STATUS-ARQ-WS.
            IF STATUS-ARQ-WS         NOT EQUAL ZEROS
               MOVE STATUS-ARQ-WS    TO STATUS-CODE-02
               PERFORM 50-90-DISPLAY-FS
           END-IF
           .
      *----------------------------------------------------------------*
       99-98-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
        99-99-FINALIZA                 SECTION.
      *----------------------------------------------------------------*
      *
           GOBACK.
      *----------------------------------------------------------------*
       99-99-SAIDA.                    EXIT.
      *----------------------------------------------------------------*

      *END PROGRAM GEREC-AM.
