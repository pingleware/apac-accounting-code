      $SET KEYCOMPRESS "7"
000170	   SELECT SYSUSER  ASSIGN DISK
			   LOCK MANUAL
			   WITH LOCK ON MULTIPLE RECORDS
000180			   STATUS WS-STATUS
000190			   ACCESS DYNAMIC
000200			   ORGANIZATION INDEXED
000210			   RECORD USE-NO
			   ALTERNATE RECORD USE-NKEY DUPLICATES.
      $SET NOKEYCOMPRESS
