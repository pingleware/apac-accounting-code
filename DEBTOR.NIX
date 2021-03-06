      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    � This routine is  used to  create  an index of the various �
      *    � words that are present in the Debtor account name.        �
      *    � The first record will be used for the first 12 characters �
      *    � of the Debtor name  as it is for  the alpha index for the �
      *    � accounts. Individuals will  only have  this one  entry in �
      *    � this index (account type 1). All other account types will �
      *    � have an entry for each word that appears in the name. The �
      *    � following procedure will be applied;		       �
      *    �	Find the next word and move the next 12 characters to  �
      *    �	index key. Repeat until the end of the name has been   �
      *    �	reached. This procedure allows for words such as; the  �
      *    �	and, etc to have the word following as part of the key �
      *    �	which reduces the duplicate keys and gives a better    �
      *    �	index spread.					       �
      *    � This routine can be altered as required.		       �
      *    � See also DEBTOR.NME (Does not log to recovery file).      �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      * 							       *
      *  Jan 2008	- New file (DEBALT) - Debtor alternate index   *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Debtor name.		       *
      * 							       *
      ******************************************************************
       DEBTOR-NAME-INDEX   SECTION 60.
       DEBTOR-INDEX.
      *
      *    ****    Initialize the extended alpha index record.
      *
	     MOVE DEB-ACNO	 TO DAL-ACNO.
	     MOVE ZERO		 TO DAL-NO.
	     MOVE DEB-NAME	 TO W50-DATA
				    DAL-NAME
				    DAL-NKEY.
      *
      *    ****    Check if the account is for an Individual.
      * 	   If Individual, then remove the '*'s and '!'.
      *
	   IF DEB-TYPE = 1
000870	       INSPECT DAL-NKEY REPLACING ALL "*" BY " "
000880	       INSPECT DAL-NKEY REPLACING FIRST "!" BY " "
000870	       INSPECT DAL-NAME REPLACING ALL "*" BY " "
000880	       INSPECT DAL-NAME REPLACING FIRST "!" BY " ".
      *
      *    ****    Write the first entry of the Alternate Index.
      * 	   This is the same as the Alpha Index entry for the
      * 	   Debtor Account.
      *
	     PERFORM WRITE-DEBALT THRU WRITE-DEBALT-EXIT.
      *
      *    ****   Log to recovery file.
      *
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY47 THRU AY59.
      *
      *    ****    Check if the account is for an Individual.
      * 	   Individuals only have one entry in the alternate
      * 	   name	index.
      *
	   IF DEB-TYPE = 1
	       GO TO DEBTOR-NAME-INDEX-EXIT.
	     MOVE 1		 TO WS-S1.
	     MOVE 40		 TO WS-S3.
      *
      *    ****    Find the length of the Name (characters).
      * 	   Look for first non space character - starting with 40
      * 	   and working back. (WS-S3)
      *
       DEBTOR-FIND-LENGTH.
	   IF W50-CCHAR(WS-S3) = SPACE
	       SUBTRACT 1	 FROM WS-S3
	       GO TO DEBTOR-FIND-LENGTH.
	     MOVE ZERO		 TO WS-S4.
      *
      *    ****    Find the next word in the name.
      *
	DEBTOR-NEXT-WORD.
      *
      *    ****    Check if end of word reached and add 1 to the number
      * 	   of words found (WS-S4). This is used, together with
      * 	   the account number, to provide a unique key for the
      * 	   DEBALT records.
      *
	   IF W50-CCHAR(1) = SPACE
      *
      *    ****    Move data, one character to the left and reduce the
      * 	   number of characters left in the name.
      *
	       MOVE W50-D39	 TO W50-DATA
	       SUBTRACT 1	 FROM WS-S3
	       ADD 1		 TO WS-S4
      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
      *    � If it is necessary to exclude words from the alternate    �
      *    � index, such as '(PTY)', 'LTD' etc, then this would be the �
      *    � the point to perform the routine to exclude the word.     �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
	       GO TO DEBTOR-STORE-NEXT.
      *
      *    ****    Move data, one character to the left and reduce the
      * 	   number of characters left in the name.
      *
	     MOVE W50-D39	 TO W50-DATA.
	     SUBTRACT 1 	 FROM WS-S3.
	   IF WS-S3 > 0
	       GO TO DEBTOR-NEXT-WORD.
      *
      *    ****  No more words left in the name.
      *
	     GO TO DEBTOR-NAME-INDEX-EXIT.

       DEBTOR-STORE-NEXT.
	     MOVE WS-S4		 TO DAL-NO.
	     MOVE W50-DATA	 TO DAL-NKEY.
      *
      *    ****    Write an entry in the Alternate Index.
      * 	   This is an additional Alpha Index entry for the
      * 	   Debtor Account.
      *
	     PERFORM WRITE-DEBALT THRU WRITE-DEBALT-EXIT.
      *
      *    ****   Log to recovery file.
      *
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY47 THRU AY59.
	   IF WS-S3 > 0
	       GO TO DEBTOR-NEXT-WORD.

       DEBTOR-NAME-INDEX-EXIT.
	     EXIT.
