      *
      *    ****  *****	***  ***** *** ****  *****  ***
      *    *   * *     *   * *	    *  *   *   *   *   *
      *    *   * *     *     *	    *  *   *   *   *
      *    ****  ****  *     ****   *  ****    *    ***
      *    *   * *     *     *	    *  *       *       *
      *    *   * *     *   * *	    *  *       *   *   *
      *    *   * *****	***  ***** *** *       *    ***
      *
      $SET KEYCOMPRESS "7"
000010	   SELECT RECEIPTS ASSIGN DISK
000020			   LOCK MANUAL
000030			   WITH LOCK ON MULTIPLE RECORDS
000040			   STATUS WS-STATUS
000050			   ACCESS DYNAMIC
000060			   ORGANIZATION INDEXED
000070			   RECORD RCT-REF-KEY
			   ALTERNATE RECORD RCT-AKEY =
				     RCT-AC RCT-REF-KEY
			   ALTERNATE RECORD RCT-TKEY =
				     RCT-TYPE RCT-REF-KEY.
      $SET NOKEYCOMPRESS
