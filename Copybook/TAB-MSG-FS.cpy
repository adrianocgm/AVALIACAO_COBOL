      *****************************************************************
      *
       01  STATUS-TABLE-OF-DATA.
           05  FILLER PIC X(56) VALUE
           '0000, SUCCESSFUL COMPLETION                             '.
           05  FILLER PIC X(56) VALUE
           '0002, INDEX FILE, IDENTICAL OR DUPLICATE KEY VALUE      '.
           05  FILLER PIC X(56) VALUE
           '0004, CONFLICT, RECORD LENGTH AND FIXED FILE ATTRIBUTES '.
           05  FILLER PIC X(56) VALUE
           '0005, OPEN FAILURE, REFERENCED FILE IS NOT FOUND        '.
           05  FILLER PIC X(56) VALUE
           '0006, WRITE FAILURE, FILE THAT HAS BEEN OPENED FOR INPUT'.
           05  FILLER PIC X(56) VALUE
           '0007, OPEN OR CLOSE FAILURE, REEL/UNIT FOR NON-REEL/UNIT'.
           05  FILLER PIC X(56) VALUE
           '0008, READ FAILURE, FILE OPENED FOR OUTPUT              '.
           05  FILLER PIC X(56) VALUE
           '0009, NO ROOM IN DIRECTORY OR DIRECTORY DOES NOT EXIST  '.
           05  FILLER PIC X(56) VALUE
           '0010, NO NEXT LOGICAL RECORD EXISTS, END OF THE FILE    '.
           05  FILLER PIC X(56) VALUE
           '0012, OPEN FAILURE, FILE IS ALREADY OPEN                '.
           05  FILLER PIC X(56) VALUE
           '0013, FILE NOT FOUND                                    '.
           05  FILLER PIC X(56) VALUE
           '0014, REQUESTED RELATIVE RECORD NUMBER > RELATIVE KEY   '.
           05  FILLER PIC X(56) VALUE
           '0015, TOO MANY INDEXED FILES OPEN (MF)                  '.
           05  FILLER PIC X(56) VALUE
           '0016, TOO MANY DEVICE FILES OPEN (MF)                   '.
           05  FILLER PIC X(56) VALUE
           '0017, RECORD ERROR: PROBABLY ZERO LENGTH (MF)           '.
           05  FILLER PIC X(56) VALUE
           '0018, EOF BEFORE EOR OR FILE OPEN IN WRONG MODE (MF)    '.
           05  FILLER PIC X(56) VALUE
           '0019, REWRITE ERROR: OPEN MODE OR ACCESS MODE WRONG (MF)'.
           05  FILLER PIC X(56) VALUE
           '0020, DEVICE OR RESOURCE BUSY (MF)                      '.
           05  FILLER PIC X(56) VALUE
           '0021, ASCENDING KEY SEQUENCE HAS BEEN VIOLATED          '.
           05  FILLER PIC X(56) VALUE
           '0022, INDICATES A DUPLICATE KEY CONDITION               '.
           05  FILLER PIC X(56) VALUE
           '0023, INDICATES NO RECORD FOUND                         '.
           05  FILLER PIC X(56) VALUE
           '0024, RELATIVE OR INDEXED FILES, A BOUNDARY VIOLATION   '.
           05  FILLER PIC X(56) VALUE
           '0030, FAILURE, BOUNDARY, PARITY CHECK, TRANSMISSION     '.
           05  FILLER PIC X(56) VALUE
           '0034, WRITE BEYOND DEFINED BOUNDARIES OF SEQUENTIAL FILE'.
           05  FILLER PIC X(56) VALUE
           '0035, OPEN FAILURE, SPECIFIED FILE NOT FOUND            '.
           05  FILLER PIC X(56) VALUE
           '0037, OPEN FAILURE, OPEN MODE SPECIFIED IS NOT SUPPORTED'.
           05  FILLER PIC X(56) VALUE
           '0038, OPEN FAILURE, FILE PREVIOUSLY CLOSED WITH A LOCK  '.
           05  FILLER PIC X(56) VALUE
           '0039, FILE ATTRIBUTES, CONFLICT OF ACTUAL VS. SPECIFIED '.
           05  FILLER PIC X(56) VALUE
           '0041, OPEN FAILURE, FILE IS ALREADY OPENED              '.
           05  FILLER PIC X(56) VALUE
           '0042, CLOSE FAILURE, FILE ALREADY CLOSED                '.
           05  FILLER PIC X(56) VALUE
           '0043, DELETE OR REWRITE ATTEMPT WITHOUT A PREVIOUS READ '.
           05  FILLER PIC X(56) VALUE
           '0044, A BOUNDARY VIOLATION EXISTS                       '.
           05  FILLER PIC X(56) VALUE
           '0046, READ FAILURE, NO VALID NEXT RECORD ESTABLISHED    '.
           05  FILLER PIC X(56) VALUE
           '0047, READ OR START FAILURE, NOT OPENED FOR INPUT OR I-O'.
           05  FILLER PIC X(56) VALUE
           '0048, A WRITE FAILURE, NOT OPENED OUTPUT, EXTEND OR I-O '.
           05  FILLER PIC X(56) VALUE
           '0049, DELETE OR REWRITE FAILURE, FILE IS NOT OPENED I-O '.
           05  FILLER PIC X(56) VALUE
           '9000/00, NO FURTHER INFORMATION                         '.
           05  FILLER PIC X(56) VALUE
           '9001/01, INSUFFICIENT BUFFER SPACE OR OUT OF MEMORY     '.
           05  FILLER PIC X(56) VALUE
           '9002/02, FILE NOT OPEN WHEN ACCESS TRIED                '.
           05  FILLER PIC X(56) VALUE
           '9003/03, SERIAL MODE ERROR                              '.
           05  FILLER PIC X(56) VALUE
           '9004/04, ILLEGAL FILE NAME                              '.
           05  FILLER PIC X(56) VALUE
           '9005/05, ILLEGAL DEVICE SPECIFICATION                   '.
           05  FILLER PIC X(56) VALUE
           '9006/06, ATTEMPT TO WRITE TO A FILE OPENED FOR INPUT    '.
           05  FILLER PIC X(56) VALUE
           '9007/07, DISK SPACE EXHAUSTED                           '.
           05  FILLER PIC X(56) VALUE
           '9008/08, ATTEMPT TO INPUT FROM A FILE OPENED FOR OUTPUT '.
           05  FILLER PIC X(56) VALUE
           '9009/09, NO ROOM IN DIRECTORY,  DIRECTORY DOES NOT EXIST'.
           05  FILLER PIC X(56) VALUE
           '9010/0A, FILE NAME NOT SUPPLIED                         '.
           05  FILLER PIC X(56) VALUE
           '9012/0C, ATTEMPT TO OPEN A FILE THAT IS ALREADY OPEN    '.
           05  FILLER PIC X(56) VALUE
           '9013/0D, FILE NOT FOUND                                 '.
           05  FILLER PIC X(56) VALUE
           '9014/0E, TOO MANY FILES OPEN SIMULTANEOUSLY             '.
           05  FILLER PIC X(56) VALUE
           '9015/0F, TOO MANY INDEXED FILES OPEN                    '.
           05  FILLER PIC X(56) VALUE
           '9016/10, TOO MANY DEVICE FILES OPEN                     '.
           05  FILLER PIC X(56) VALUE
           '9017/11, RECORD ERROR, PROBABLE ZERO RECORD LENGTH      '.
           05  FILLER PIC X(56) VALUE
           '9018/12, EOF BEFORE EOR OR FILE OPEN IN WRONG MODE      '.
           05  FILLER PIC X(56) VALUE
           '9019/13, REWRITE ERROR: OPEN MODE OR ACCESS MODE WRONG  '.
           05  FILLER PIC X(56) VALUE
           '9020/14, DEVICE OR RESOURCE BUSY                        '.
           05  FILLER PIC X(56) VALUE
           '9021/15, FILE IS A DIRECTORY                            '.
           05  FILLER PIC X(56) VALUE
           '9022/16, ILLEGAL OR IMPOSSIBLE ACCESS MODE FOR OPEN     '.
           05  FILLER PIC X(56) VALUE
           '9023/17, ILLEGAL OR IMPOSSIBLE ACCESS MODE FOR CLOSE    '.
           05  FILLER PIC X(56) VALUE
           '9024/18, DISK I/O ERROR                                 '.
           05  FILLER PIC X(56) VALUE
           '9025/19, OPERATING SYSTEM DATA ERROR                    '.
           05  FILLER PIC X(56) VALUE
           '9026/1A, BLOCK I/O ERROR                                '.
           05  FILLER PIC X(56) VALUE
           '9027/1B, DEVICE NOT AVAILABLE                           '.
           05  FILLER PIC X(56) VALUE
           '9028/1C, NO SPACE ON DEVICE                             '.
           05  FILLER PIC X(56) VALUE
           '9029/1D, ATTEMPT TO DELETE OPEN FILE                    '.
           05  FILLER PIC X(56) VALUE
           '9030/1E, FILE SYSTEM IS READ ONLY                       '.
           05  FILLER PIC X(56) VALUE
           '9031/1F, NOT OWNER OF FILE                              '.
           05  FILLER PIC X(56) VALUE
           '9032/20, TOO MANY INDEXED FILES, OR NO SUCH PROCESS     '.
           05  FILLER PIC X(56) VALUE
           '9033/21, PHYSICAL I/O ERROR                             '.
           05  FILLER PIC X(56) VALUE
           '9034/22, INCORRECT MODE OR FILE DESCRIPTOR              '.
           05  FILLER PIC X(56) VALUE
           '9035/23, FILE ACCESS ATTEMPT WITH INCORRECT PERMISSION  '.
           05  FILLER PIC X(56) VALUE
           '9036/24, FILE ALREADY EXISTS                            '.
           05  FILLER PIC X(56) VALUE
           '9037/25, FILE ACCESS DENIED                             '.
           05  FILLER PIC X(56) VALUE
           '9038/26, DISK NOT COMPATIBLE                            '.
           05  FILLER PIC X(56) VALUE
           '9039/27, FILE NOT COMPATIBLE                            '.
           05  FILLER PIC X(56) VALUE
           '9040/28, LANGUAGE INITIALIZATION NOT SET UP CORRECTLY   '.
           05  FILLER PIC X(56) VALUE
           '9041/29, CORRUPT INDEX FILE                             '.
           05  FILLER PIC X(56) VALUE
           '9042/2A, ATTEMPT TO WRITE ON BROKEN PIPE                '.
           05  FILLER PIC X(56) VALUE
           '9043/2B, FILE INFORMATION MISSING FOR INDEXED FILE      '.
           05  FILLER PIC X(56) VALUE
           '9044/2C, ATTEMPT OPEN, NLS FILE AND INCOMPATIBLE PROGRAM'.
           05  FILLER PIC X(56) VALUE
           '9045/2D, INDEX STRUCTURE OVERFLOW MAXIMUM DUPLICATE KEYS'.
           05  FILLER PIC X(56) VALUE
           '9065/41, FILE LOCKED                                    '.
           05  FILLER PIC X(56) VALUE
           '9066/42, ATTEMPT TO ADD DUPLICATE KEY TO INDEXED FILE   '.
           05  FILLER PIC X(56) VALUE
           '9067/43, INDEXED FILE NOT OPEN                          '.
           05  FILLER PIC X(56) VALUE
           '9068/44, RECORD LOCKED                                  '.
           05  FILLER PIC X(56) VALUE
           '9069/45, ILLEGAL ARGUMENT TO ISAM MODULE                '.
           05  FILLER PIC X(56) VALUE
           '9070/46, TOO MANY INDEXED FILES OPEN                    '.
           05  FILLER PIC X(56) VALUE
           '9071/47, BAD INDEXED FILE FORMAT                        '.
           05  FILLER PIC X(56) VALUE
           '9072/48, END OF INDEXED FILE                            '.
           05  FILLER PIC X(56) VALUE
           '9073/49, NO RECORD FOUND IN INDEXED FILE                '.
           05  FILLER PIC X(56) VALUE
           '9074/4A, NO CURRENT RECORD IN INDEXED FILE              '.
           05  FILLER PIC X(56) VALUE
           '9075/4B, INDEXED DATA FILE NAME TOO LONG                '.
           05  FILLER PIC X(56) VALUE
           '9076/4C, INTERNAL ISAM MODULE FAILURE                   '.
           05  FILLER PIC X(56) VALUE
           '9077/4D, ILLEGAL KEY DESCRIPTION IN INDEXED FILE        '.
           05  FILLER PIC X(56) VALUE
           '9081/51, KEY ALREADY EXISTS IN INDEXED FILE             '.
           05  FILLER PIC X(56) VALUE
           '9092/5C, PUT/UPDATE OR ERASE WITHOUT A PREVIOUS GET    '.
           05  FILLER PIC X(56) VALUE
           '9100/64, INVALID FILE OPERATION                         '.
           05  FILLER PIC X(56) VALUE
           '9101/65, ILLEGAL OPERATION ON AN INDEXED FILE           '.
           05  FILLER PIC X(56) VALUE
           '9102/66, SEQUENTIAL FILE, NON-INTEGRAL NUMBER OF RECORDS'.
           05  FILLER PIC X(56) VALUE
           '9104/68, NULL FILE NAME USED IN A FILE OPERATION        '.
           05  FILLER PIC X(56) VALUE
           '9105/69, MEMORY ALLOCATION ERROR                        '.
           05  FILLER PIC X(56) VALUE
           '9124/7C, CONNECTION FAILURE TO REMOTE SYSTEM (MF)       '.
           05  FILLER PIC X(56) VALUE
           '9125/7D, CONNECTION TO REMOTE FILE SERVER FAILED (MF)   '.
           05  FILLER PIC X(56) VALUE
           '9129/81, ATTEMPT TO ACCESS RECORD ZERO OF RELATIVE FILE '.
           05  FILLER PIC X(56) VALUE
           '9135/87, FILE MUST NOT EXIST                            '.
           05  FILLER PIC X(56) VALUE
           '9138/8A, FILE CLOSED WITH LOCK - CANNOT BE OPENED       '.
           05  FILLER PIC X(56) VALUE
           '9139/8B, RECORD LENGTH OR KEY DATA INCONSISTENCY        '.
           05  FILLER PIC X(56) VALUE
           '9141/8D, FILE ALREADY OPEN - CANNOT BE OPENED           '.
           05  FILLER PIC X(56) VALUE
           '9142/8E, FILE NOT OPEN - CANNOT BE CLOSED               '.
           05  FILLER PIC X(56) VALUE
           '9143/8F, REWRITE/DELETE NOT PRECEDED BY SUCCESSFUL READ '.
           05  FILLER PIC X(56) VALUE
           '9146/92, NO CURRENT RECORD DEFINED FOR SEQUENTIAL READ  '.
           05  FILLER PIC X(56) VALUE
           '9147/93, WRONG OPEN MODE OR ACCESS MODE FOR READ/START  '.
           05  FILLER PIC X(56) VALUE
           '9148/94, WRONG OPEN MODE OR ACCESS MODE FOR WRITE       '.
           05  FILLER PIC X(56) VALUE
           '9149/95, WRONG OPEN OR ACCESS MODE FOR REWRITE/ DELETE  '.
           05  FILLER PIC X(56) VALUE
           '9150/96, PROGRAM ABANDONED AT USER REQUEST              '.
           05  FILLER PIC X(56) VALUE
           '9151/97, RANDOM READ ON SEQUENTIAL FILE                 '.
           05  FILLER PIC X(56) VALUE
           '9152/98, REWRITE ON FILE NOT OPENED I-O                 '.
           05  FILLER PIC X(56) VALUE
           '9158/9E, ATTEMPT TO REWRITE TO A LINE-SEQUENTIAL FILE.  '.
           05  FILLER PIC X(56) VALUE
           '9159/9F, MALFORMED LINE SEQUENTIAL-FILE                 '.
           05  FILLER PIC X(56) VALUE
           '9161/A1, FILE HEADER NOT FOUND                          '.
           05  FILLER PIC X(56) VALUE
           '9173/AD, CALLED PROGRAM NOT FOUND                       '.
           05  FILLER PIC X(56) VALUE
           '9180/B4, END-OF-FILE MARKER ERROR                       '.
           05  FILLER PIC X(56) VALUE
           '9182/B6, CONSOLE INPUT OR OUTPUT OPEN IN WRONG DIRECTION'.
           05  FILLER PIC X(56) VALUE
           '9183/B7, ATTEMPT TO OPEN LINE SEQUENTIAL FILE FOR I-O   '.
           05  FILLER PIC X(56) VALUE
           '9188/BC, FILE NAME TOO LARGE                            '.
           05  FILLER PIC X(56) VALUE
           '9193/C1, ERROR IN VARIABLE LENGTH COUNT                 '.
           05  FILLER PIC X(56) VALUE
           '9194/C2, FILE SIZE TOO LARGE                            '.
           05  FILLER PIC X(56) VALUE
           '9195/C3, DELETE/REWRITE NOT PRECEDED BY A READ          '.
           05  FILLER PIC X(56) VALUE
           '9196/C4, RECORD NUMBER TOO LARGE, RELATIVE/INDEXED FILE '.
           05  FILLER PIC X(56) VALUE
           '9210/D2, FILE IS CLOSED WITH LOCK                       '.
           05  FILLER PIC X(56) VALUE
           '9213/D5, TOO MANY LOCKS                                 '.
           05  FILLER PIC X(56) VALUE
           '9218/DA, MALFORMED MULTIPLE REEL/UNIT FILE              '.
           05  FILLER PIC X(56) VALUE
           '9219/DB, OPERATING SYSTEM SHARED FILE LIMIT EXCEEDED    '.
           05  FILLER PIC X(56) VALUE
           '0099, INVALID NUMBER TO CPF OR CNPJ '.

      *****************************************************************
       01  STATUS-TABLE-01  REDEFINES STATUS-TABLE-OF-DATA.
           05  STATUS-CELL  OCCURS 134 TIMES.
               10  STATUS-CELL-04  PIC X(4).
               10  STATUS-CELL-52  PIC X(52).

       01  STATUS-CELLS-MAXIMUM    PIC 9(5)    VALUE 134.
       01  STATUS-IND              PIC 9(5)    VALUE 0.
      ***  TAB4STAT - END-OF-COPY FILE - - - - - - - - - - - TAB4STAT *
      ***************************************************************** 
