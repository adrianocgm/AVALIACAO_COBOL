      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
      *
       PROGRAM-ID. RELATORIO.
       AUTHOR.     ADRIANO MENEZES.
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
      *    PROGRAMA....: RELAT                                         *
      *    PROGRAMADOR : ADRIANO MENEZES                               *
      *    DATA........: 02/06/2019                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO...: RELATORIO DE CLIENTES/VENDEDORES               *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                ARQCLI01                        CAD-SIST        *
      *                ARQSRELT                                        *
      *----------------------------------------------------------------*
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.
      *
           SELECT ARQCLI01 ASSIGN      TO DISK
               ORGANIZATION            IS INDEXED
               ACCESS MODE             IS DYNAMIC
               RECORD KEY              IS EXEMPLO-DADOS
                                       OF ARQCLI01
               FILE STATUS             IS WRK-FS-ARQCLI01.
      *
           SELECT ARQSRELT ASSIGN      TO DISK
               FILE STATUS             IS WRK-FS-ARQSRELT.
      *
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA                                  *
      *            ORG. IINDEXADO        LRECL = 0083                  *
      *----------------------------------------------------------------*
      *
       FD  ARQCLI01
            VALUE OF FILE-ID IS NOME-ARQCLI01-WS
            COPY "COPYBOOK\CAD-SIST.CPY".
      *
      *----------------------------------------------------------------*
      *    OUTPUT: RELATORIO                                           *
      *            ORG. SEQUENCIAL     LRECL = 133                     *
      *----------------------------------------------------------------*
      *
       FD  ARQSRELT
            VALUE OF FILE-ID IS WRK-NOME-RELATORIO.
      *     VALUE OF FILE-ID IS NOME-REL-PARAM-S-WS.
      *
       01  FD-ARQSRELT                 PIC  X(133).
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
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-RESTART.
           05  ACU-LIDOS-ARQCLI01      PIC  9(005)  COMP-3 VALUE ZEROS.
      *
       77  ACU-PAGINAS                 PIC  9(004) COMP-3  VALUE ZEROS.
       77  ACU-LINHAS                  PIC  9(002) COMP-3  VALUE 99.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
      *
       77  WRK-PROGRAMA                PIC  X(008)     VALUE
           'RELAT'.
       77  WRK-ARQUIVO                 PIC  X(008)     VALUE SPACES.
      *
       01 WRK-DATA-HORA-C.
           02 WRK-DATA-C.
              03 WRK-ANO-C             PIC 9(004)      VALUE ZEROS.
              03 WRK-MES-C             PIC 9(002)      VALUE ZEROS.
              03 WRK-DIA-C             PIC 9(002)      VALUE ZEROS.
           02 WRK-HORAS-C.
              03 WRK-HORA-C            PIC 9(002)      VALUE ZEROS.
              03 WRK-MIN-C             PIC 9(002)      VALUE ZEROS.
              03 WRK-SEG-C             PIC 9(002)      VALUE ZEROS.
           02 GMT.
              03 WRK-RESTO             PIC 9(002)      VALUE ZEROS.
              03 GMTDIFR               PIC X(001).
                 88 GMTNAOSUPORTADO VALUE "0".
              03 GMTHORAS              PIC 9(002)      VALUE ZEROS.
              03 GMTMINS               PIC 9(002)      VALUE ZEROS.

       01 WRK-DATA-HORA-REL.
           02 WRK-DATA-REL.
              03 WRK-DIA-REL          PIC 9(002)       VALUE ZEROS.
              03 FILLER               PIC X(001)       VALUE '/'.
              03 WRK-MES-REL          PIC 9(002)       VALUE ZEROS.
              03 FILLER               PIC X(001)       VALUE '/'.
              03 WRK-ANO-REL          PIC 9(004)       VALUE ZEROS.
           02 WRK-HORAS-REL.                           
              03 WRK-HORA-REL         PIC 9(002)       VALUE ZEROS.
              03 FILLER               PIC X(001)       VALUE ':'.
              03 WRK-MIN-REL          PIC 9(002)       VALUE ZEROS.
              03 FILLER               PIC X(001)       VALUE ':'.
              03 WRK-SEG-REL          PIC 9(002)       VALUE ZEROS.
       01 WRK-NOME-RELATORIO.
           02 WRK-NOME-RELAT-CAMINHO  PIC X(050)       VALUE SPACES.
       01 NOME-ARQCLI01-WS            PIC X(050)       VALUE
                                      'C:\GERENC\ARQCLI01.DAT'.
      *
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           '* AREA PARA TESTE DE FILE-STATUS *'.
      *----------------------------------------------------------------*
      *
       01  WRK-FS-ARQCLI01             PIC X(002)      VALUE SPACES.
           88  WRK-ARQCLI01-OK                         VALUE ZEROS.
           88  WRK-ARQCLI01-FIM                        VALUE '10'.
       01  WRK-FS-ARQSRELT             PIC X(002)      VALUE SPACES.
           88  WRK-ARQSRELT-OK                         VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DO RELATORIO'.
      *----------------------------------------------------------------*
       01  WRK-CABEC1-REL1.
           03 FILLER                   PIC X(001)          VALUE '1'.
           03 WRK-CABEC1-NOME-RELAT    PIC X(008)          VALUE ZEROS.
           03 FILLER                   PIC X(045)          VALUE SPACES.
           03 FILLER                   PIC X(037)          VALUE
              'S I S T E M A  D E  R E L A T O R I O'.
           03 FILLER                   PIC X(032)          VALUE SPACES.
           03 FILLER                   PIC X(005)          VALUE
               'PAG: '.
           03 WRK-CABEC1-PAG           PIC Z.ZZ9           VALUE ZEROS.
       01  WRK-CABEC2-REL1.
           03 FILLER                   PIC X(001)          VALUE ' '.
           03 WRK-CABEC2-DATA-PROCM    PIC X(010)          VALUE SPACES.
           03 FILLER                   PIC X(020)          VALUE SPACES.
           03 FILLER                   PIC X(078)          VALUE SPACES.
           03 FILLER                   PIC X(015)          VALUE SPACES.
           03 WRK-CABEC2-HORA-PROCM    PIC X(008)          VALUE SPACES.

      *
       01  WRK-CABEC4-REL1.
           03 FILLER                   PIC X(133)          VALUE ALL
              '-'.
      *
       01  WRK-LINDET1-REL1.
           03 FILLER                   PIC X(001)          VALUE ' '.
           03 FILLER                   PIC X(002)          VALUE '| '.
           03 FILLER                   PIC X(007)          VALUE
              'CODIGO '.
           03 FILLER                   PIC X(003)          VALUE ' | '.
           03 FILLER                   PIC X(014)          VALUE
              '   CPF/CNPJ   '.
           03 FILLER                   PIC X(003)          VALUE ' | '.
           03 FILLER                   PIC X(010)          VALUE SPACES.
           03 FILLER                   PIC X(020)          VALUE
              'NOME / RAZAO SOCIAL '.
           03 FILLER                   PIC X(010)          VALUE SPACES.
           03 FILLER                   PIC X(003)          VALUE ' | '.
           03 FILLER                   PIC X(011)          VALUE
              'LATITUDE'.
           03 FILLER                   PIC X(003)          VALUE ' | '.
           03 FILLER                   PIC X(011)          VALUE
              'LONGITUDE'.
           03 FILLER                   PIC X(003)          VALUE ' | '.
           03 FILLER                   PIC X(32)           VALUE SPACES.
              
      *
       01  WRK-LINDET2-REL1.
           03 FILLER                   PIC X(001)          VALUE ' '.
           03 FILLER                   PIC X(002)          VALUE '| '.
           03 LD2-CODIGO               PIC 9(007)          VALUE ZEROS.
           03 FILLER                   PIC X(003)          VALUE ' | '.
           03 LD2-CPF-CNPJ             PIC X(014)          VALUE SPACES.
           03 FILLER                   PIC X(003)          VALUE ' | '.
           03 LD2-NOME-RAZAO           PIC X(040)          VALUE SPACES.
           03 FILLER                   PIC X(003)          VALUE ' | '.
           03 LD2-LATITUDE             PIC 9(003)V9(008)   VALUE ZEROS.
           03 FILLER                   PIC X(003)          VALUE ' | '.
      *    03 LD2-I-LONGITUDE          PIC 9(003)V9(008)   VALUE ZEROS.
           03 LD2-I-LONGITUDE          PIC ZZ9.99999999.
           03 FILLER                   PIC X(003)          VALUE ' | '.
           03 FILLER                   PIC X(032)          VALUE SPACES.
      *
      *
      *----------------------------------------------------------------*
       01   FILLER                     PIC X(050)      VALUE
            '***  FINAL DA WORKING RELAT  ***'.
      *----------------------------------------------------------------*
      *
       LINKAGE SECTION.
      *
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
           INITIALIZE WRK-DATA-HORA-C
                      WRK-DATA-HORA-REL
      *
           PERFORM 1210-OBTER-DATA-ATUAL

           STRING 'C:\GERENC\RELATORIO_CLIENTE_'
                  WRK-DATA-C
                  WRK-HORAS-C
                  '.TXT'
                                        INTO WRK-NOME-RELATORIO
      *
           OPEN INPUT  ARQCLI01
                OUTPUT ARQSRELT
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
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       1210-OBTER-DATA-ATUAL           SECTION.
      *----------------------------------------------------------------*
      *
           MOVE FUNCTION CURRENT-DATE  TO WRK-DATA-HORA-C
           MOVE WRK-ANO-C              TO WRK-ANO-REL
           MOVE WRK-MES-C              TO WRK-MES-REL
           MOVE WRK-DIA-C              TO WRK-DIA-REL
           MOVE WRK-HORA-C             TO WRK-HORA-REL
           MOVE WRK-MIN-C              TO WRK-MIN-REL
           MOVE WRK-SEG-C              TO WRK-SEG-REL
           .

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO PRINCIPAL                              *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *
           IF ACU-LINHAS               GREATER 60
               ADD  1                  TO ACU-PAGINAS
           END-IF
      *
           IF (ACU-LINHAS               GREATER 60)
      *
              PERFORM 2100-IMPRIMIR-CABECALHO
      *
           END-IF.

           PERFORM 2500-FORMATA-DETALHE

           PERFORM 7100-LER-ARQCLI01.
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *  FORMATA OS DADOS DE ACORDO COM O REGISTRO HEADER              *
      *----------------------------------------------------------------*
       2100-IMPRIMIR-CABECALHO         SECTION.
      *----------------------------------------------------------------*
      *
           MOVE ZEROS                  TO ACU-LINHAS
      *
           MOVE '#########'            TO WRK-CABEC1-NOME-RELAT
           MOVE ACU-PAGINAS            TO WRK-CABEC1-PAG
      *
           MOVE WRK-DATA-REL           TO WRK-CABEC2-DATA-PROCM
           MOVE WRK-HORAS-REL          TO WRK-CABEC2-HORA-PROCM
      *
           WRITE FD-ARQSRELT           FROM WRK-CABEC4-REL1
                                       AFTER ADVANCING 1 LINES
           PERFORM 7020-TESTAR-FS-ARQSRELT
           ADD 1                       TO ACU-LINHAS
      *
           WRITE FD-ARQSRELT           FROM WRK-CABEC1-REL1
                                       AFTER ADVANCING 1 LINES
           PERFORM 7020-TESTAR-FS-ARQSRELT
           ADD 1                       TO ACU-LINHAS
      *
           WRITE FD-ARQSRELT           FROM WRK-CABEC2-REL1
                                       AFTER ADVANCING 1 LINES
           PERFORM 7020-TESTAR-FS-ARQSRELT
           ADD 1                       TO ACU-LINHAS
      *
           WRITE FD-ARQSRELT           FROM WRK-LINDET1-REL1
                                       AFTER ADVANCING 1 LINES
           PERFORM 7020-TESTAR-FS-ARQSRELT
           ADD 1                       TO ACU-LINHAS
      *
           WRITE FD-ARQSRELT           FROM WRK-CABEC4-REL1
                                       AFTER ADVANCING 1 LINES
           PERFORM 7020-TESTAR-FS-ARQSRELT
           ADD 1                       TO ACU-LINHAS
           .
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * FORMATA OS DADOS DOS REGISTROS DE DETALHE                      *
      *----------------------------------------------------------------*
       2500-FORMATA-DETALHE            SECTION.
      *----------------------------------------------------------------*
      *
      *
           IF COMUNIC-TPO-PESSOA       EQUAL 'CL'
               MOVE EXEMPLO-CODIGO-CLI TO LD2-CODIGO
               MOVE EXEMPLO-NR-CNPJ    TO LD2-CPF-CNPJ
           END-IF
           MOVE EXEMPLO-RAZAO-SOCIAL     TO LD2-NOME-RAZAO
           MOVE EXEMPLO-LATITUDE
                                       TO LD2-LATITUDE
           MOVE EXEMPLO-LONGITUDE
                                       TO LD2-I-LONGITUDE
      *
           PERFORM 2700-IMPRIME-LINDET.
      *
      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA IMPRESSAO DA LINHA DO RELATORIO                    *
      *----------------------------------------------------------------*
       2700-IMPRIME-LINDET             SECTION.
      *----------------------------------------------------------------*
      *
           WRITE FD-ARQSRELT           FROM WRK-LINDET2-REL1
                                       AFTER ADVANCING 1 LINES
           PERFORM 7020-TESTAR-FS-ARQSRELT
      *
           ADD 1                       TO ACU-LINHAS
           .

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZACAO                                          *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
      *
           CLOSE ARQCLI01
                 ARQSRELT
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
           PERFORM 7020-TESTAR-FS-ARQSRELT
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
       7020-TESTAR-FS-ARQSRELT         SECTION.
      *----------------------------------------------------------------*
      *
           IF  NOT WRK-ARQSRELT-OK
               MOVE WRK-FS-ARQSRELT    TO COMUNIC-COD-RETORNO
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
           READ ARQCLI01              
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
      *
