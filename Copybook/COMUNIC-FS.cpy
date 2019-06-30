      * The following is a summary of the fields used as linkage items.
      *
      * FSPA-STATUS-CODE-02      Two byte file status code.
      * FSPA-STATUS-CODE-04      Four Byte file status code.
      * FSPA-TEXT-MESSAGE        Short description of status.
      *
      *****************************************************************
       01  FILE-STATUS-AREA.
           05  STATUS-CODE-02     PIC X(2).
           05  STATUS-CODE-04     PIC X(4).
           05  MENSAGEM-FS        PIC X(80).
      ***  PASSSTAT - End-of-Copy File - - - - - - - - - - - PASSSTAT *
      *****************************************************************