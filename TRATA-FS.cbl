       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TRATA-FS.
      *****************************************************************
      *     
      *****************************************************************
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *****************************************************************
      *
      *****************************************************************
       01  IO-STATUS.
           05  IO-STAT1            PIC X.
           05  IO-STAT2            PIC X.
       01  WRK-02-BYTE-BINARIO        PIC 9(4)  BINARY.
       01  TWO-BYTES-ALPHA         REDEFINES WRK-02-BYTE-BINARIO.
           05  TWO-BYTES-LEFT      PIC X.
           05  TWO-BYTES-RIGHT     PIC X.
       01  IO-STATUS-04.
           05  IO-STATUS-0401      PIC 9     VALUE 0.
           05  IO-STATUS-0403      PIC 999   VALUE 0.

       01  LOOK-OUT                PIC X     VALUE 'N'.

       COPY 'COPYBOOK\TAB-MSG-FS'.

      *****************************************************************
       LINKAGE SECTION.
       COPY 'COPYBOOK\COMUNIC-FS'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING FILE-STATUS-AREA.
      *
      *----------------------------------------------------------------*
       00-00-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*
           PERFORM 00-05-CONVERSAO-FS

           PERFORM 10-00-CAPTURA-TEXTO

           PERFORM 99-99-FINALIZA
           .
      *----------------------------------------------------------------*
       00-00-SAIDA.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       00-05-CONVERSAO-FS              SECTION.
      *----------------------------------------------------------------*
           MOVE STATUS-CODE-02         TO IO-STATUS
           IF  IO-STATUS               NOT NUMERIC
           OR  IO-STAT1                EQUAL '9'
               MOVE IO-STAT1           TO IO-STATUS-04(1:1)
               SUBTRACT WRK-02-BYTE-BINARIO
                                       FROM WRK-02-BYTE-BINARIO
               MOVE IO-STAT2           TO TWO-BYTES-RIGHT
               ADD WRK-02-BYTE-BINARIO
                                       TO ZERO GIVING IO-STATUS-0403
           ELSE
               MOVE '0000'             TO IO-STATUS-04
               MOVE IO-STATUS          TO IO-STATUS-04(3:2)
           END-IF
           .
      *----------------------------------------------------------------*
       00-05-SAIDA.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10-00-CAPTURA-TEXTO.            EXIT.
      *----------------------------------------------------------------*

           MOVE IO-STATUS-04           TO STATUS-CODE-04
           ADD 1                       TO ZERO GIVING STATUS-IND
           MOVE 'N'                    TO LOOK-OUT
           MOVE SPACES                 TO MENSAGEM-FS
      *
           PERFORM UNTIL STATUS-IND    GREATER STATUS-CELLS-MAXIMUM
                      OR LOOK-OUT      EQUAL 'Y'
               IF  STATUS-CELL-04(STATUS-IND)
                                       EQUAL STATUS-CODE-04
                   MOVE STATUS-CELL(STATUS-IND)
                                       TO MENSAGEM-FS
                   MOVE 'Y'            TO LOOK-OUT
               ELSE
                   ADD 1               TO STATUS-IND
               END-IF
           END-PERFORM
           .
      *----------------------------------------------------------------*
       10-00-SAIDA.                    EXIT.
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

