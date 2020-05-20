      $SET KEYCOMPRESS "7"
000160     SELECT GACCNT ASSIGN DISK
                         LOCK MANUAL
                         WITH LOCK ON MULTIPLE RECORDS
000170                   STATUS WS-STATUS
000190                   ACCESS DYNAMIC
000200                   ORGANIZATION INDEXED
000200                   RECORD G-AC.
      $SET NOKEYCOMPRESS
