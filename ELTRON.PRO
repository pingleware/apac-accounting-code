      *
      *	  E L T R O N	T L P  &  L P +   B A R   C O D E
      *   P R I N T I N G
      *
      *	  X
      *	  À Command (see below)
      *
      *	  COMMANDS: A	  Print ASCII text
      *		    B	  Print Bar code
      *		    P	  Select number of labels to print.
      *
      *   B999,99,9,XXX,9,9,999,X,"XXXXXXXXXXXXXX"
      *   ³ ³  ³  ³  ³	³ ³  ³	³  À Print characters (bar code)
      *   ³ ³  ³  ³  ³	³ ³  ³	ÀÄÄÄ Print readable code
      *   ³ ³  ³  ³  ³	³ ³  ÀÄÄÄÄÄÄ Bar code height
      *   ³ ³  ³  ³  ³	³ ÀÄÄÄÄÄÄÄÄÄ Wide bar width
      *   ³ ³  ³  ³  ³	ÀÄÄÄÄÄÄÄÄÄÄÄ Narrow bar width
      *   ³ ³  ³  ³  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Bar code selection (see below)
      *   ³ ³  ³  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Printing angle (0=0,1=90,2=180,3=270)
      *   ³ ³  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Row address (distance down)
      *   ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Column address (distance right)
      *   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Select bar code print
      *
      *   Bar codes: 3	 - 3 OF 9	    9	- CODE 93
      * 	     3C  - 3 OF 9 with CD   2U	- UPC Inter 2 OF 5
      * 	     UA0 - UPC-A	    UE0 - UPC-E
      * 	     UA2 - UPC-A 2 DIG ADD  UE2 - UPC-E 2 DIG ADD
      * 	     UA5 - UPC-A 5 DIG ADD  UE5 - UPC-E 5 DIG ADD
      * 	     0	 - CODE 128 UCC     1E	- UCC/EAN 128
      * 	     1	 - CODE 128 A,B,C   P	- Postnet 5,6,8 & 11
      * 	     E30 - EAN-13	    E80 - EAN-8
      * 	     E32 - EAN-13 2 DIG ADD E82 - EAN-8 2 DIG ADD
      * 	     E35 - EAN-13 5 DIG ADD E85 - EAN-8 5 DIG ADD
      * 	     K	 - CODEBAR	    2D	- Inter 2 OF 5 CD read
      * 	     2	 - Inter 2 OF 5     2G	- German Postcode
      * 	     2C  - Inter 2 OF 5 CD
      *
       BA300.
	     MOVE W25-NAR	 TO W32-N-BAR.
	     MOVE W25-WIDE	 TO W32-W-BAR.
	     MOVE W25-COLUMN	 TO W32-COL2.
	     MOVE W25-ROW	 TO W32-ROW2.
	     MOVE W25-BC-HGHT	 TO W32-HEIGHT.
	     MOVE W25-FONT	 TO W32-FONT.
	     MOVE W25-F-COLUMN	 TO W32-COL.
	     MOVE W25-F-ROW	 TO W32-ROW.
	     MOVE SPACES	 TO REP-L1.
	     MOVE W15-CODE	 TO W32-BAR-CDE.
      *     MOVE W15-PRICE	 TO W32-VALUE.
	   IF W25-SPLIT = "Y"
	       MOVE SPACES	 TO W32-DESC
	       MOVE W15-DSC	 TO W32-SD
	       MOVE W15-DSC2	 TO W32-DESC2
	   ELSE
	       MOVE W15-DESC	 TO W32-DESC.
	     MOVE W32-SETUP	 TO REP-CIT-CONT.
	     WRITE REP-L1 BEFORE 1 LINES.
	     MOVE W32-DESCRIPTION
				 TO REP-L4.
	     WRITE REP-L1 BEFORE 1 LINES.
	   IF W25-SPLIT = "Y"
	       MOVE W32-DESCRIPTION2
				 TO REP-L4
	       WRITE REP-L1 BEFORE 1 LINES.
	   IF WS-BC-STYLE = "1"
	       MOVE "UA0"	 TO W32-BC-TYPE
	   ELSE
	   IF WS-BC-STYLE = "2"
	       MOVE "UE0"	 TO W32-BC-TYPE
	   ELSE
	   IF WS-BC-STYLE = "3"
	       MOVE "E30"	 TO W32-BC-TYPE
	   ELSE
	   IF WS-BC-STYLE = "4"
	       MOVE "E80"	 TO W32-BC-TYPE
	   ELSE
	   IF WS-BC-STYLE = "5"
	       MOVE "3" 	 TO W32-BC-TYPE
	   ELSE
	   IF WS-BC-STYLE = "6"
	       MOVE "2" 	 TO W32-BC-TYPE
	   ELSE
	       MOVE "K" 	 TO W32-BC-TYPE.
	     MOVE W32-BAR-CODE	 TO REP-CIT-BCDE.
	     WRITE REP-L1 BEFORE 1 LINES.
      *	   IF W15-PRICE > 99.99
      *	       MOVE W32-PRICE	 TO REP-L4
      *	   ELSE
      *	       MOVE W32-PRICE2	 TO REP-L4.
      *	     WRITE REP-L1 BEFORE 1 LINES.
	     MOVE "X"		 TO REP-L4.
	     WRITE REP-L1 BEFORE 1 LINES.
	     MOVE W15-QNT	 TO W32-NUMBER.
	     MOVE W32-PRINT	 TO REP-L4.
	     WRITE REP-L1 BEFORE 1 LINES.
