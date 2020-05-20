      $SET KEYCOMPRESS "7"
           SELECT CHEQUE    ASSIGN DISK
000270                      LOCK MANUAL
000280                      WITH LOCK ON MULTIPLE RECORDS
000290                      STATUS WS-STATUS
000300                      ACCESS DYNAMIC
000310                      ORGANIZATION INDEXED
                            RECORD CHQ-TR-NUMBER
000420                      ALTERNATE RECORD CHQ-DKEY =
                              CHQ-YMD CHQ-TR-NUMBER.
      $SET NOKEYCOMPRESS
