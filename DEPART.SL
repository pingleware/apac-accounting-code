      $SET KEYCOMPRESS "7"
000010	   SELECT DEPART  ASSIGN DISK
000020                    LOCK MANUAL
000030                    WITH LOCK ON MULTIPLE RECORDS
000040                    STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD DPT-CODE
			  ALTERNATE RECORD DPT-NKEY DUPLICATES.
      $SET NOKEYCOMPRESS
