      $SET KEYCOMPRESS "7"
000170	   SELECT DEBDEP  ASSIGN DISK
                          LOCK MANUAL
                          WITH LOCK ON MULTIPLE RECORDS
000180                    STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD DBD-ACNO.
      $SET NOKEYCOMPRESS
