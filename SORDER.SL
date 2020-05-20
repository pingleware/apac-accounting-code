      $SET KEYCOMPRESS "7"
000230     SELECT SORDER  ASSIGN DISK
                          LOCK MANUAL
                          WITH LOCK ON MULTIPLE RECORDS
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270                    RECORD SOR-KEY
                          ALTERNATE RECORD SOR-AKEY = 
                            SOR-AC SOR-KEY DUPLICATES.
      $SET NOKEYCOMPRESS
