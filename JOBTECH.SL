      $SET NOKEYCOMPRESS
000230	   SELECT JOBTECH ASSIGN DISK
                          LOCK MANUAL
                          WITH LOCK ON MULTIPLE RECORDS
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270			  RECORD TIM-KEY
			  ALTERNATE RECORD TIM-JOB-KEY =
			    TIM-JOB TIM-TECH
			    DUPLICATES.
      $SET NOKEYCOMPRESS
