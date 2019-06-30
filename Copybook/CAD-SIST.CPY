      ******************************************************************
      * BOOK LAYOUT PARA CADASTRO DE CLIENTES/VENDEDORES.              *
      *                                                                *
      *                                                                *
      ******************************************************************
      *
           LABEL RECORD IS STANDARD                                     
           RECORD CONTAINS 083 CHARACTERS
           DATA RECORD IS EXEMPLO-REG.      
      *    
       01 EXEMPLO-REG.
           05 EXEMPLO-DADOS            PIC X(021).
           05 EXEMPLO-REG01-CLIENTE REDEFINES EXEMPLO-DADOS.
               10 EXEMPLO-CODIGO-CLI   PIC  9(007).
               10 EXEMPLO-NR-CNPJ      PIC  X(014).
           05 EXEMPLO-REG01-VENDEDR REDEFINES EXEMPLO-DADOS.
              10 EXEMPLO-CODIGO-VEND   PIC  9(003).
              10 EXEMPLO-NR-CPF        PIC  X(011).
              10 FILLER                PIC  X(007).
           05 EXEMPLO-RAZAO-SOCIAL     PIC  X(040).
           05 EXEMPLO-LATITUDE         PIC S9(003)V9(008).
           05 EXEMPLO-LONGITUDE        PIC S9(003)V9(008).
           