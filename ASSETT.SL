      $SET KEYCOMPRESS "7"
000170	   SELECT ASSETT  ASSIGN DISK
                          LOCK MANUAL
                          WITH LOCK ON MULTIPLE RECORDS
000180                    STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD ASS-CODE
			  ALTERNATE RECORD ASS-LKEY =
				    ASS-LOCATION ASS-DKEY DUPLICATES
			  ALTERNATE RECORD ASS-GKEY =
				    ASS-GROUP ASS-DKEY DUPLICATES
			  ALTERNATE RECORD ASS-DKEY DUPLICATES.
      $SET NOKEYCOMPRESS
