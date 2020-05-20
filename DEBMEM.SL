      $SET KEYCOMPRESS "7"
000230     SELECT DEBMEM  ASSIGN DISK
                          LOCK MANUAL
                          WITH LOCK ON MULTIPLE RECORDS
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270                    RECORD DME-KEY.
      $SET NOKEYCOMPRESS
