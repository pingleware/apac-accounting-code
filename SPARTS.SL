      $SET KEYCOMPRESS "7"
           SELECT SPARTS  ASSIGN DISK
                          LOCK MANUAL
                          WITH LOCK ON MULTIPLE RECORDS
                          STATUS WS-STATUS
                          ACCESS DYNAMIC
                          ORGANIZATION INDEXED
			  RECORD PRT-EXT-ITEM
                          ALTERNATE RECORD PRT-TYPE DUPLICATES.
      $SET NOKEYCOMPRESS
