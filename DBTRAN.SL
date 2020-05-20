      $SET KEYCOMPRESS "7"
000010     SELECT DBTRAN  ASSIGN DISK
000020                    LOCK AUTOMATIC
000030                    STATUS WS-STATUS
000040                    ACCESS DYNAMIC
000050                    ORGANIZATION INDEXED
000060                    RECORD TRA-KEY
                          ALTERNATE RECORD TRA-TRSEQ =
                            TRA-CODE TRA-DATE DUPLICATES.
      $SET NOKEYCOMPRESS
