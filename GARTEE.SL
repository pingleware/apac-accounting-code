      $SET KEYCOMPRESS "7"
000110     SELECT GARTEE  ASSIGN DISK
                          LOCK MANUAL
                          WITH LOCK ON MULTIPLE RECORDS
000120                    STATUS WS-STATUS
000130                    ACCESS DYNAMIC
000140                    ORGANIZATION INDEXED
                          RECORD GUA-KEY
000150			  ALTERNATE RECORD GUA-EXT-CODE
                                           DUPLICATES
000150                    ALTERNATE RECORD GUA-NKEY
                                           DUPLICATES
000400                    ALTERNATE RECORD GUA-SERIAL 
000440                                     DUPLICATES.
      $SET NOKEYCOMPRESS
