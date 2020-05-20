      $SET KEYCOMPRESS "7"
000010     SELECT CARDEX  ASSIGN DISK
000020                    LOCK MANUAL
000030                    WITH LOCK ON MULTIPLE RECORDS
000040                    STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
      *
      * 	Account Number / Stock Code / Date Sequence
      *
000070			  RECORD CRD-KEY
      *
      * 	Stock Code / Account Number Sequence
      *
			  ALTERNATE RECORD CRD-SKEY =
				    CRD-EXT-CODE CRD-AC DUPLICATES
      *
      * 	Stock Code / Reference (Invoice/Credit Note) Sequence
      *
			  ALTERNATE RECORD CRD-MKEY =
				    CRD-EXT-CODE CRD-REF DUPLICATES
      *
      * 	Account Number / Date Sequence
      *
			  ALTERNATE RECORD CRD-AKEY =
				    CRD-AC CRD-DTE DUPLICATES
      *
      * 	Date / Account Number Sequence
      *
			  ALTERNATE RECORD CRD-DKEY =
				    CRD-DTE CRD-AC DUPLICATES
      *
      * 	Date / Stock Code Sequence
      *
			  ALTERNATE RECORD CRD-DSKEY =
				    CRD-DTE CRD-EXT-CODE DUPLICATES
      *
      * 	Stock Code / Date Sequence
      *
			  ALTERNATE RECORD CRD-SDKEY =
				    CRD-EXT-CODE CRD-DTE DUPLICATES
      *
      * 	Sales Person / Date / Stock Code Sequence
      *
			  ALTERNATE RECORD CRD-SMKEY =
				    CRD-SMAN CRD-DTE CRD-EXT-CODE
				    DUPLICATES.
      $SET NOKEYCOMPRESS
