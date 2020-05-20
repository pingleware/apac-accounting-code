      $SET KEYCOMPRESS "7"
000230     SELECT NOT OPTIONAL
                  CREDIT ASSIGN DISK
                         LOCK MANUAL
                         WITH LOCK ON MULTIPLE RECORDS
000240                   STATUS WS-STATUS
000250                   ACCESS DYNAMIC
000260                   ORGANIZATION INDEXED
000270                   RECORD P-NUMBER
                         ALTERNATE RECORD P-NKEY DUPLICATES.
      $SET NOKEYCOMPRESS
