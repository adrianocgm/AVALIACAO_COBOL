       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDA-CPF-CNPJ.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.

      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-RESTART.
           05 WRK-COUNTR               PIC 9(003)      VALUE ZEROS.
           05 WRK-NR-CPF               PIC X(011)      VALUE SPACES.
           05 REDEFINES                WRK-NR-CPF.
              10 WRK-NR-CPF-9          PIC 9(011).
           05 WRK-NR-CNPJ              PIC X(014)      VALUE SPACES.
           05 REDEFINES                WRK-NR-CNPJ.
              10 WRK-NR-CNPJ-9         PIC 9(014).

           05 WRK-DIG                  PIC 9(002)      VALUE ZEROS.
           05 WRK-SOMA                 PIC 9(005)      VALUE ZEROS.
           05 WRK-QUOCIENTE            PIC 9(005)      VALUE ZEROS.
           05 WRK-RESTO                PIC 9(005)      VALUE ZEROS.
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
        COPY 'COPYBOOK/AREA-COMUNC'.
       PROCEDURE DIVISION USING COMUNIC-BLOCO.

      *
      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZACAO DO PROGRAMA                            *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE WRK-AREA-RESTART.
      *
           EVALUATE TRUE
               WHEN COMUNIC-TPO-VEND
                   PERFORM 2000-VALIDAR-CPF
               WHEN COMUNIC-TPO-CLI
                   PERFORM 2100-VALIDAR-CNPJ
               WHEN OTHER
                   MOVE '98' TO COMUNIC-COD-RETORNO
                   PERFORM 3300-GO-BACK
           END-EVALUATE

           MOVE '00'                   TO COMUNIC-COD-RETORNO

           PERFORM 3300-GO-BACK
           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO PRINCIPAL                              *
      *----------------------------------------------------------------*
       2000-VALIDAR-CPF                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE COMUNIC-NR-CPF         TO WRK-NR-CPF
           INSPECT COMUNIC-NR-CPF      TALLYING WRK-COUNTR
                                       FOR ALL '0' '1' '2' '3' '4'
                                               '5' '6' '7' '8' '9'.
      *
           IF WRK-COUNTR = 11
           AND WRK-NR-CPF-9           IS NUMERIC
              COMPUTE WRK-COUNTR = 1
              EVALUATE WRK-NR-CPF
                  WHEN '00000000000'
                  WHEN '11111111111'
                  WHEN '22222222222'
                  WHEN '33333333333'
                  WHEN '44444444444'
                  WHEN '55555555555'
                  WHEN '66666666666'
                  WHEN '77777777777'
                  WHEN '88888888888'
                  WHEN '99999999999'
                      MOVE '99'        TO COMUNIC-COD-RETORNO
                      STRING 'NUMERO CPF INVALIDO: '
                              WRK-NR-CPF
                                       INTO COMUNIC-MSG
                      PERFORM 3300-GO-BACK
                 WHEN OTHER
                     COMPUTE WRK-SOMA  = 0
                     PERFORM UNTIL WRK-COUNTR GREATER 9
                         COMPUTE WRK-SOMA
                                       = WRK-SOMA + ((11 - WRK-COUNTR)
                                       * FUNCTION NUMVAL
                                          (WRK-NR-CPF(WRK-COUNTR:1)))
                         COMPUTE WRK-COUNTR
                                       = WRK-COUNTR + 1
                END-PERFORM
      *
                DIVIDE WRK-SOMA BY 11  GIVING WRK-QUOCIENTE
                                       REMAINDER WRK-RESTO
                IF WRK-RESTO           LESS 2
                   COMPUTE WRK-DIG     = 0
                ELSE
                   COMPUTE WRK-DIG     = 11 - WRK-RESTO
                END-IF
                IF WRK-DIG NOT EQUAL   TO
                                       FUNCTION NUMVAL
                                       (WRK-NR-CPF(10:1))
                   MOVE '99'           TO COMUNIC-COD-RETORNO
                   PERFORM 3300-GO-BACK
                ELSE
                   COMPUTE WRK-COUNTR   = 1
                   COMPUTE WRK-SOMA     = 0
      *
                   PERFORM UNTIL WRK-COUNTR GREATER 10
                       COMPUTE WRK-SOMA
                                       = WRK-SOMA + ((12 - WRK-COUNTR)
                                       * FUNCTION NUMVAL
                                          (WRK-NR-CPF(WRK-COUNTR:1)))
                    COMPUTE WRK-COUNTR = WRK-COUNTR + 1
                   END-PERFORM
      *
                   DIVIDE WRK-SOMA     BY 11 GIVING WRK-QUOCIENTE
                                       REMAINDER WRK-RESTO
                   IF WRK-RESTO LESS 2
                      COMPUTE WRK-DIG = 0
                   ELSE
                      COMPUTE WRK-DIG = 11 - WRK-RESTO
                   END-IF
      *
                   IF WRK-DIG NOT EQUAL
                                       TO FUNCTION NUMVAL
                                                   (WRK-NR-CPF(11:1))
                       MOVE '99'       TO COMUNIC-COD-RETORNO
                       STRING 'NUMERO CPF INVALIDO: '
                               WRK-NR-CPF
                                       INTO COMUNIC-MSG
                       PERFORM 3300-GO-BACK
                   END-IF
                END-IF
              END-EVALUATE
           ELSE
              MOVE '99'                TO COMUNIC-COD-RETORNO
              STRING 'NUMERO CPF INVALIDO: '
                      WRK-NR-CPF       INTO COMUNIC-MSG
              PERFORM 3300-GO-BACK
           END-IF.
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA VALIDAR CNPJ                                            *
      *----------------------------------------------------------------*
       2100-VALIDAR-CNPJ               SECTION.
      *----------------------------------------------------------------*
      *
           MOVE COMUNIC-NR-CNPJ        TO WRK-NR-CNPJ
           INSPECT COMUNIC-NR-CNPJ     TALLYING WRK-COUNTR
                                       FOR ALL '0' '1' '2' '3' '4' 
                                                '5' '6' '7' '8' '9'.
      *
           IF WRK-COUNTR              EQUAL 14
           AND WRK-NR-CNPJ-9          IS NUMERIC
              COMPUTE WRK-COUNTR = 1
              EVALUATE WRK-NR-CNPJ
                  WHEN '00000000000000'
                  WHEN '11111111111111'
                  WHEN '22222222222222'
                  WHEN '33333333333333'
                  WHEN '44444444444444'
                  WHEN '55555555555555'
                  WHEN '66666666666666'
                  WHEN '77777777777777'
                  WHEN '88888888888888'
                  WHEN '99999999999999'
                      MOVE '99'        TO COMUNIC-COD-RETORNO
                      STRING 'NUMERO CNPJ INVALIDO: '
                              WRK-NR-CNPJ
                                       INTO COMUNIC-MSG
                      PERFORM 3300-GO-BACK
                 WHEN OTHER
                     COMPUTE WRK-SOMA  = 0
                     PERFORM UNTIL WRK-COUNTR GREATER 9
                         COMPUTE WRK-SOMA
                                       = WRK-SOMA + ((11 - WRK-COUNTR)
                                       * FUNCTION NUMVAL
                                          (WRK-NR-CPF(WRK-COUNTR:1)))
                         COMPUTE WRK-COUNTR
                                       = WRK-COUNTR + 1



                END-PERFORM
           ELSE
              MOVE '99'                TO COMUNIC-COD-RETORNO
              STRING 'NUMERO CNPJ INVALIDO: '
                      WRK-NR-CPF       INTO COMUNIC-MSG
              PERFORM 3300-GO-BACK
           END-IF.
           .
      *
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
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