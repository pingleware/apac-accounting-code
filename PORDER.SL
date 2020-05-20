      $SET KEYCOMPRESS "7"
000330     SELECT PORDER  ASSIGN DISK
                          LOCK MANUAL
                          WITH LOCK ON MULTIPLE RECORDS
000340                    STATUS WS-STATUS
000350                    ACCESS DYNAMIC
000360                    ORGANIZATION INDEXED
000390                    RECORD ORD-KEY
000400			  ALTERNATE RECORD ORD-EXT-ITEM
                                           DUPLICATES
			  ALTERNATE RECORD ORD-CKEY =
					   ORD-CRED ORD-EXT-ITEM
000440					   DUPLICATES
			  ALTERNATE RECORD ORD-DKEY =
					   ORD-CRED ORD-DATE
					   ORD-EXT-ITEM
					   DUPLICATES.
      $SET NOKEYCOMPRESS
