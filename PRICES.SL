      $SET KEYCOMPRESS "7"
000200	   SELECT PRICES  ASSIGN DISK
000020                    LOCK MANUAL
000030                    WITH LOCK ON MULTIPLE RECORDS
000040                    STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
000070			  RECORD PRC-CODE
000130			  ALTERNATE RECORD PRC-DKEY DUPLICATES
000130			  ALTERNATE RECORD PRC-DKEY2 DUPLICATES
000140			  ALTERNATE RECORD PRC-XREF DUPLICATES
000140			  ALTERNATE RECORD PRC-DEPART DUPLICATES.
      $SET NOKEYCOMPRESS
