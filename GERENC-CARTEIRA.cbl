      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
      *
       PROGRAM-ID. GERENC-CARTEIRA.
       AUTHOR.     ADRIANO MENEZES.
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
      *    PROGRAMA....: GERENC-CARTEIRA                               *
      *    PROGRAMADOR : ADRIANO MENEZES                               *
      *----------------------------------------------------------------*
      *    OBJETIVO...: GERENC-CARTEIRA DE CLIENTES                    *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                ARQCLI01                        CAD-SIST        *
      *----------------------------------------------------------------*
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
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
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *
         FD ARQCLI01
            COPY "COPYBOOK\CAD-SIST.CPY".
      *
         FD ARQVEN01
            COPY "COPYBOOK\CAD-SIST.CPY".
      *
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
           'INICIO DA WORKING STORAGE  SECTION'.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
      *
       77  WRK-PROGRAMA                PIC  X(016)     VALUE
           'GERENC-CARTEIRA'.
       77  WRK-ARQUIVO                 PIC  X(008)     VALUE SPACES.

       77 WRK-RAIO-TERRA        COMP-2 VALUE 6378.137.
       77 WRK-PI                COMP-2 VALUE 3.14.

       77 DECIMAL2 PIC Z(9).9(9)-  USAGE DISPLAY.

       77 DECIMAL3 PIC Z(9),9(9)-  USAGE DISPLAY.

       01 WID-ARQ-CLIENTE.
           05 FILLER                   PIC  X(010)     VALUE
                                                           'C:\GERENC\'.
           05 WRK-NOME-ARQUIVO         PIC  X(008)     VALUE 'ARQCLI01'.
           05 FILLER                   PIC  X(004)     VALUE '.DAT'.
       01 WID-ARQ-VENDEDOR.
           05 FILLER                   PIC  X(010)     VALUE
                                                           'C:\GERENC\'. 
           05 WRK-NOME-ARQUIVO         PIC  X(008)     VALUE 'ARQVEN01'.
           05 FILLER                   PIC  X(004)     VALUE '.DAT'.
       01 WRK-CALC-ARTH.
          05 DEG2RADMULTIPLIER                        COMP-2.
          05 WRK-LAT-CLI                                     COMP-2.
          05 WRK-LON-CLI                                     COMP-2.
          05 WRK-LAT-VEN                                     COMP-2.
          05 WRK-LON-VEN                                     COMP-2.
          05 WRK-DISTC-LON                                     COMP-2.
                   
       01 WRK-DISTANCIAS.  
         05 WRK-DISTANCIA-CLI USAGE COMP-2.
         05 WRK-DISTANCIA-AUX  USAGE COMP-2.
         05 WRK-DISTANCIA-VEN  USAGE COMP-2.

       01  WRK-AREA-RESTART.
           05  ACU-LIDOS-ARQCLI01      PIC  9(005)  COMP-3 VALUE ZEROS.
           05  ACU-LIDOS-ARQVEN01      PIC  9(005)  COMP-3 VALUE ZEROS.

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
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           '* AREA PARA TESTE DE FILE-STATUS *'.
      *----------------------------------------------------------------*
      *
       01  WRK-FS-ARQCLI01             PIC X(002)      VALUE SPACES.
           88  WRK-ARQCLI01-OK                         VALUE ZEROS.
           88  WRK-ARQCLI01-FIM                        VALUE '10'.
       01  WRK-FS-ARQVEN01             PIC X(002)      VALUE SPACES.
           88  WRK-ARQVEN01-OK                         VALUE ZEROS.
           88  WRK-ARQVEN01-FIM                        VALUE '10'.
      *
      *----------------------------------------------------------------*
       01   FILLER                     PIC X(050)      VALUE
            '***  FINAL DA WORKING GERENC-CARTEIRA  ***'.
      *----------------------------------------------------------------*
      *
       LINKAGE SECTION.
           COPY 'COPYBOOK/AREA-COMUNC'.
      *
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL DO PROGRAMA                                   *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIALIZAR
      *
           PERFORM 2200-CAMPOS-DISTANCIA
      *
           PERFORM 2100-CALCULAR-DISTANCIA
      *
           PERFORM 2000-PROCESSAR      UNTIL WRK-ARQCLI01-FIM
      *
           PERFORM 3000-FINALIZAR
           .
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZACAO DO PROGRAMA                            *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE WRK-AREA-RESTART.
      *
           OPEN INPUT ARQCLI01
                      ARQVEN01
      *        OUTPUT ARQSCART
      *
           PERFORM 7000-TESTAR-FILE-STATUS
      *
           PERFORM 1100-VERIFICAR-VAZIO
      *
           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS.  *
      *----------------------------------------------------------------*
       1100-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 7100-LER-ARQCLI01
      *
           IF  ACU-LIDOS-ARQCLI01      NOT GREATER ZEROS
               MOVE WRK-FS-ARQCLI01    TO COMUNIC-COD-RETORNO
               PERFORM 3000-FINALIZAR
           END-IF

           PERFORM 7200-LER-ARQVEN01

           IF  ACU-LIDOS-ARQVEN01      NOT GREATER ZEROS
               MOVE WRK-FS-ARQVEN01    TO COMUNIC-COD-RETORNO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO PRINCIPAL                              *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *
           MOVE WRK-DISTANCIA-AUX      TO WRK-DISTANCIA-CLI
           IF WRK-DISTANCIA-CLI        EQUAL WRK-DISTANCIA-AUX
               PERFORM 2300-ADCIONA-VENDEDOR
               PERFORM 7200-LER-ARQVEN01
               PERFORM 2200-CAMPOS-DISTANCIA
               PERFORM 2100-CALCULAR-DISTANCIA
           ELSE
               IF WRK-DISTANCIA-CLI > WRK-DISTANCIA-AUX
      *            MOVER DADOS DO VENDEDOR SUBSTITUIR PARA VENDEDOR MAIS
      *            PROX
      *            PERFORM 2300-ADCIONA-VENDEDOR
               ELSE
                   PERFORM 7200-LER-ARQVEN01
               PERFORM 2200-CAMPOS-DISTANCIA
               PERFORM 2100-CALCULAR-DISTANCIA
               END-IF
           IF WRK-ARQVEN01-FIM
           AND NOT WRK-ARQCLI01-FIM
               CLOSE ARQVEN01
               PERFORM 7020-TESTAR-FS-ARQVEN01
               OPEN INPUT ARQVEN01
               PERFORM 7020-TESTAR-FS-ARQVEN01
               PERFORM 7100-LER-ARQCLI01
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2100-CALCULAR-DISTANCIA         SECTION.
      *----------------------------------------------------------------*
      *
           SUBTRACT WRK-LON-CLI FROM WRK-LON-VEN GIVING WRK-DISTC-LON
           DIVIDE WRK-PI BY 180 GIVING DEG2RADMULTIPLIER
           COMPUTE WRK-DISTANCIA-AUX  = FUNCTION ACOS(
                                        (FUNCTION SIN(WRK-LAT-CLI)
                                       * FUNCTION SIN(WRK-LAT-VEN))
                                       + (FUNCTION COS(WRK-LAT-CLI)
                                       *  FUNCTION COS(WRK-LAT-VEN)
                                       *  FUNCTION COS(WRK-DISTC-LON)))
                                       * WRK-RAIO-TERRA
      *    MOVE WRK-DISTANCIA-CLI
           .
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------* 
       2200-CAMPOS-DISTANCIA         SECTION.
      *----------------------------------------------------------------*
      *
           MOVE EXEMPLO-LATITUDE       OF ARQCLI01
                                       TO WRK-LAT-CLI
           MOVE EXEMPLO-LONGITUDE      OF ARQCLI01
                                       TO WRK-LON-CLI
           MOVE EXEMPLO-LATITUDE       OF ARQVEN01
                                       TO WRK-LAT-VEN
           MOVE EXEMPLO-LONGITUDE      OF ARQVEN01
                                       TO WRK-LON-VEN
           .
      *
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------* 
       2300-ADCIONA-VENDEDOR         SECTION.
      *----------------------------------------------------------------*
      *
           
           .
      *
      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE FINALIZACAO                                          *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
      *
           CLOSE ARQCLI01
                 ARQVEN01
      *
           PERFORM 7000-TESTAR-FILE-STATUS
      *
           MOVE ZEROS                  TO RETURN-CODE
      *
           PERFORM 3300-GO-BACK
           .
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZACAO.                                         *
      *----------------------------------------------------------------*
       3300-GO-BACK                   SECTION.
      *----------------------------------------------------------------*
      *
           GOBACK
           .
      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA TESTAR FILE STATUS.                                *
      *----------------------------------------------------------------*
       7000-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 7010-TESTAR-FS-ARQCLI01
      *
           PERFORM 7020-TESTAR-FS-ARQVEN01
           .
      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA TESTAR FILE STATUS DO ARQUIVO ARQCLI01.            *
      *----------------------------------------------------------------*
       7010-TESTAR-FS-ARQCLI01         SECTION.
      *----------------------------------------------------------------*
      *
           IF  NOT WRK-ARQCLI01-OK
               MOVE WRK-FS-ARQCLI01    TO COMUNIC-COD-RETORNO
               MOVE 'ARQCLI01'         TO COMUNIC-DES-PGR-ARQ
               PERFORM 3300-GO-BACK
           END-IF
           .
      *----------------------------------------------------------------*
       7010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA TESTAR FILE STATUS DO ARQUIVO ARQSRELT.            *
      *----------------------------------------------------------------*
       7020-TESTAR-FS-ARQVEN01         SECTION.
      *----------------------------------------------------------------*
      *
           IF  NOT WRK-ARQVEN01-OK
               MOVE WRK-FS-ARQVEN01    TO COMUNIC-COD-RETORNO
               MOVE 'ARQSRELT'         TO COMUNIC-DES-PGR-ARQ
               PERFORM 3300-GO-BACK
           END-IF
           .
      *----------------------------------------------------------------*
       7020-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * LEITURA DO ARQUIVO ARQCLI01.                                   *
      *----------------------------------------------------------------*
       7100-LER-ARQCLI01               SECTION.
      *----------------------------------------------------------------*
      *
           READ ARQCLI01 NEXT RECORD
      *
           IF  WRK-ARQCLI01-OK
               PERFORM 7010-TESTAR-FS-ARQCLI01
               ADD 1                   TO ACU-LIDOS-ARQCLI01
           END-IF
           .
      *
      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
      * LEITURA DO ARQUIVO ARQCLI01.                                   *
      *----------------------------------------------------------------*
       7200-LER-ARQVEN01               SECTION.
      *----------------------------------------------------------------*
      *
           READ ARQVEN01   NEXT RECORD           
      *
           IF  WRK-FS-ARQVEN01         NOT EQUAL ZEROS
               PERFORM 7020-TESTAR-FS-ARQVEN01
               ADD 1                   TO ACU-LIDOS-ARQVEN01
           END-IF
           .
      *
      *----------------------------------------------------------------*
       7200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *
