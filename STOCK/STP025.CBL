      $set LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	   ******   ********   *
      *   **	**     **     **    **	 **  **   **	**  **	       *
      *   **	       **     **    **	**    **       **   *******    *
      *    ******      **     *******	**    **     ** 	  **   *
      * 	**     **     **	**    **   **		  **   *
      *   **	**     **     **	 **  **   **	    **	  **   *
      *    ******      **     **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     A D J U S T   P R O D U C T I O N	P R I C E S	       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP025.
000030 AUTHOR.	       J.W.LEMMON (APAC).

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2008
				     by James William Lemmon.
				       (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@gmail.com.

000050 SECURITY.
		   This program is free software; you can redistribute
		   it and/or modify it under the terms of the GNU General
		   Public License as published by the Free Software
		   Foundation; either version 2 of the License, or (at
		   your option) any later version.

		   This program is distributed in the hope that it will
		   be useful, but WITHOUT ANY WARRANTY; without even the
		   implied warranty of MERCHANTABILITY or FITNESS FOR A
		   PARTICULAR PURPOSE.	See the GNU General Public License
		   for more details.

		   You should have received a copy of the GNU General
		   Public License along with this program; if not, write
		   to the Free Software Foundation, Inc., 59 Temple Place
		   - Suite 330, Boston, MA 02111-1307, USA.

000040 DATE-WRITTEN.   JANUARY 1983.
000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY CONTROL.SL.

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

000200 COPY SPARTS.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000380 COPY RECOVER.GFD.

000280 COPY PARAM.FDE.

       COPY SHARED.FDE.

000360 COPY SPARTS.FDE.

000300 COPY STOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	   PIC	999V99	 COMP-3.
001710 77  WS-SUB	   PIC	9(04)	 COMP-5.
001710 77  WS-S1	   PIC	9(04)	 COMP-5.
001720 77  WS-S2	   PIC	9(04)	 COMP-5.
001730 77  WS-S3	   PIC	9(04)	 COMP-5.
001740 77  WS-S4	   PIC	9(04)	 COMP-5.
001750 77  WS-S5	   PIC	9(04)	 COMP-5.
001760 77  WS-S6	   PIC	9(04)	 COMP-5.
001770 77  WS-S7	   PIC	9(04)	 COMP-5.
001780 77  WS-S8	   PIC	9(04)	 COMP-5.
001790 77  WS-IXS	   PIC	9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC	9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC	9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC	9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC	9(04)	 COMP-5.
001860 77  WS-PARKEY	   PIC	9(06)	 COMP-5.
001870 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001890 77  WS-RECKEY	   PIC	9(06)	 COMP-5.
001900 77  WS-RECOVER	   PIC	9(06)	 COMP-5.
001900 77  WS-RECORDS	   PIC	9(06)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
001980 77  WS-ITM	   PIC	X(18).
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-CARDEX	   PIC	X(01).
       77  WS-ETYPE	   PIC	X(01).
002020 77  WS-OPTION	   PIC	X(01).
002020 77  WS-SKIP	   PIC	X(01).
002040 77  WS-ADJ	   PIC	X(01).
002070 77  WS-ERROR	   PIC	9(01).
002120 77  WS-AMEND	   PIC	9(01)	 VALUE 0.
002140 77  WS-CONREC	   PIC	9(01) VALUE ZERO.
002200 77  WS-COMP	   PIC	9(01).
002210 77  WS-WEEK	   PIC	9(01).
002220 77  WS-TYPE	   PIC	X(01).
002270 77  WS-PASSWORD	   PIC	X(08).
       77  WS-PRINT	   PIC	9(01).
002280 77  WS-PASS	   PIC	X(08).
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
       77  WS-NODEP	   PIC	X(30) VALUE
			   "NO SUCH   D E P A R T M E N T".
       77  TODAY-DDMMYY    PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430	   03  WS-TOP-LNE.
002440	       05  WS-TCHR PIC X(01) OCCURS 80.
	   03  WS-T-LINE REDEFINES WS-TOP-LNE.
	       05  FILLER  PIC X(01).
	       05  WS-H-LINE
			   PIC	X(78).
	       05  FILLER  PIC X(01).
002430	   03  WS-TOP-LNE2.
002440	       05  WS-TCH  PIC	X(80) VALUE "旼컴컴컴컴컴컴컴컴컴컴컴컴�
      -        "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�".
	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "�".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "�".
	       05  FILLER      PIC  X(01) VALUE "�".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002330 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000380 COPY RECOVER.ID.

       COPY SHARED.ID.

000360 COPY SPARTS.ID.

000300 COPY STOCK.ID.

       COPY W05.VAT.

       01  W05-EXTRA.
	   03  W05-ROUND       PIC  X(01).
           03  W05-ROUND-VAL   PIC  9V9(02).
           03  W05-RVAL REDEFINES W05-ROUND-VAL.
               05  W05-RAND    PIC  9(01).
               05  W05-CENTS   PIC  9(02).
           03  W05-RVAL2 REDEFINES W05-ROUND-VAL.
               05  FILLER      PIC  9(01).
               05  W05-RVC1    PIC  9(01).
               05  W05-RVC2    PIC  9(01).
           03  W05-HLF-VAL     PIC  9V99.
           03  W05-TQTR-VAL    PIC  9V99.
           03  W05-CALC-VAL    PIC  9(02)V99.
           03  W05-CALC-RRCC REDEFINES W05-CALC-VAL.
               05  W05-RVR1    PIC  9(01).
               05  W05-RVRCC   PIC  9(01)V99.
       01  W09-STOCK.
002780	   03  W09-MARKUP     PIC S9(03)V9999  COMP-3.
002790	   03  W09-CMARKUP    PIC S9(03)V9999  COMP-3.
002790	   03  W09-WMARKUP    PIC S9(03)V9999  COMP-3.

       COPY W10.STK.

       COPY W11.STK.

       COPY W12.WS.

       COPY W15.STK.

       COPY W20.WS.

003700 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003750     03  W25-SCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCST       PIC S9(09)V99 COMP-3.
003760     03  W25-PTOT       PIC S9(09)V99 COMP-3.
003770     03  W25-TSELL      PIC S9(09)V99 COMP-3.
003770     03  W25-TOTAL      PIC S9(09)V99 COMP-3.
003780     03  W25-VALUE      PIC S9(09)V99 COMP-3.
003780     03  W25-ADJUST     PIC S9(09)V99 COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

003930 01  W80-EDIT.
003940     03  W80-CNO        PIC 9(04).
003950     03  W80-V1.
003960         05  W80-S7V2   PIC Z(06)9.99-.
003970     03  W80-V2.
003980         05  W80-S5V2   PIC Z(04)9.99-.
003990     03  W80-DTE.
004000	       05  W80-DATE   PIC Z9/99/9999.
004010     03  W80-QNT.
004020         05  W80-5      PIC Z(04)9.99.
004030     03  W80-QNT1 REDEFINES W80-QNT.
004040         05  W80-3V2    PIC Z(02)9.9999.
004050     03  W80-NO.
004060         05  W80-5N     PIC 9(05).
004070         05  W80-REC REDEFINES W80-5N
004080                        PIC ZZZZ9.
004090     03  W80-DEL.
004100         05  W80-2N      PIC 9(02).
006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.
004110 01  W90-BAL             PIC S9(07)V99 COMP-3.
004120 01  W95-COMP            PIC X(40).
005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

004440 01  S03.
004450	   03  LINE 12 COLUMN 27 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(14) FROM STK-ITEM.

004440 01  S03-ITM.
004450	   03  LINE 12 COLUMN 27 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(14) FROM STK-ITEM.
	   03	       COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 5 VALUE "/".
	   03	       COLUMN 42 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(03) FROM STK-ITM.

004440 01  S03-EXT.
004450	   03  LINE 12 COLUMN 27 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460				 PIC X(18) FROM STK-CODE.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN		SECTION.
007750 AA000-INIT.
003240	   IF LS0-STLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
007760	     PERFORM ZA000-INIT.
007780	     PERFORM BR000.
	     CLOSE RECOVER.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

       COPY STOCK.RD.

       COPY SPARTS.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY PARAM.WR.

       COPY SHARED.WR.

       COPY STOCK.WR.

       COPY SPARTS.WR.

       COPY STOCK.1ST.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
011430 AY000           SECTION.
012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

011120 AY49.
011700       MOVE 99             TO REC-FILE.
011710       MOVE ZERO           TO REC-KEY.
011720       MOVE SPACES         TO REC-DETAIL.
011730       PERFORM AY50 THRU AY59.
011740       ADD 1               TO WS-TRANS.
011750     IF WS-RECOVER > 95
011760         CLOSE RECOVER
011770         OPEN OUTPUT RECOVER
011780         CLOSE RECOVER
011790         OPEN I-O RECOVER
011800         MOVE ZERO         TO WS-RECOVER.
011810       GO TO AY59.

011820 AY50.
011830	     ADD 1		 TO WS-RECOVER.
011840       MOVE WS-RECOVER     TO WS-RECKEY.
011850       MOVE WS-TRANS       TO REC-TRAN.
011860       WRITE REC-RECORD.
011870     IF WS-STATUS NOT = "00"
011310         DISPLAY "Write error Recovery file - Status " AT 2212
011320                  WITH FOREGROUND-COLOR 14 WS-STATUS 
011330                  WITH FOREGROUND-COLOR 15
011900         STOP RUN.
011930 AY59.
011940       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
	     MOVE SHR-STOCK	 TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "ST"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the STOCK control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
	   IF WS-STOCK = "Y"
012710	       MOVE 3		 TO WS-SHARED
012720	       PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
	       PERFORM AY37 THRU AY59
	       MOVE SHR-RECORD	 TO NET-RECORD
	   ELSE
012710	       MOVE 3		 TO WS-NETKEY
012720	       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
012730	       PERFORM AY39 THRU AY59.
012740	     GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
	       IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
      *
		   IF WS-WAIT = ZERO
		       GO TO AY63
		   ELSE
		       GO TO AY62-WAIT
		   END-IF
	       ELSE
		   GO TO AY62-WAIT
	       END-IF
	   END-IF.
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 3.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       IF WS-USUB < 24
		   ADD 1	 TO WS-USUB
		   GO TO AY72
	       ELSE
		   GO TO AY85.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES)
	       MOVE PAR-PROG(WS-SUB)
				 TO PAR-PROG(WS-USUB)
	       MOVE PAR-USR(WS-SUB)
				 TO PAR-USR(WS-USUB)
	       ADD 1		 TO WS-USUB
	       GO TO AY75.

       AY80.
013070	     SUBTRACT 1 FROM WS-USUB
				 GIVING PAR-USERS.
013080	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       AY85.
	   IF LS0-POS NOT = 2
012430         CLOSE RECOVER
012440         OPEN I-O RECOVER.
013090       COMMIT.

012330 AY999.
012340       EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

       COPY ROUND.PRC.

       CB300	     SECTION.
       CB301.
	   IF W10-TAX NOT = ZERO
	       COMPUTE W10-SL(WS-S1) ROUNDED = (W10-VSL(WS-S1) /
				(100.00000 + W05-RATE)) * 100.00
	       COMPUTE W10-WSL(WS-S1) ROUNDED = (W10-VWSL(WS-S1) /
				 (100.00000 + W05-RATE)) * 100.00
	       COMPUTE W10-CSH(WS-S1) ROUNDED = (W10-VCSH(WS-S1) /
				(100.00000 + W05-RATE)) * 100.00
           ELSE
	       MOVE W10-VWSL(WS-S1)
				 TO W10-WSALE
	       MOVE W10-VCSH(WS-S1)
				 TO W10-CASH
	       MOVE W10-VSL(WS-S1)
				 TO W10-SELL.
	   IF W10-TAX NOT = ZERO
	       COMPUTE W10-CHECK ROUNDED
		   = W10-SL(WS-S1) + (W10-SL(WS-S1) * W05-RTE)
	       IF W10-CHECK > W10-VSL(WS-S1)
		   SUBTRACT 0.01 FROM W10-SL(WS-S1)
	       ELSE
	       IF W10-CHECK < W10-VSL(WS-S1)
		   ADD 0.01	 TO W10-SL(WS-S1)
	       END-IF
	       COMPUTE W10-CHECK ROUNDED
		   = W10-CSH(WS-S1) + (W10-CSH(WS-S1) * W05-RTE)
	       IF W10-CHECK > W10-VCSH(WS-S1)
		   SUBTRACT 0.01 FROM W10-CSH(WS-S1)
	       ELSE
	       IF W10-CHECK < W10-VCSH(WS-S1)
		   ADD 0.01	 TO W10-CSH(WS-S1)
	       END-IF
	       COMPUTE W10-CHECK ROUNDED
		   = W10-WSL(WS-S1) + (W10-WSL(WS-S1) * W05-RTE)
	       IF W10-CHECK > W10-VWSL(WS-S1)
		   SUBTRACT 0.01 FROM W10-WSL(WS-S1)
	       ELSE
	       IF W10-CHECK < W10-VWSL(WS-S1)
		   ADD 0.01	 TO W10-WSL(WS-S1).

       CB399.
             EXIT.
      /
      *    ****    ADJUST PRODUCTION PRICES
      *
039210 BR000		       SECTION 6.
039220 BR00.
039230       DISPLAY CLR-SCREEN.
             DISPLAY "ADJUST PRODUCTION PRICES" AT 0229
                      WITH FOREGROUND-COLOR 15.
039240       DISPLAY "Adjust prices on produced items" AT 0412.
039250	     DISPLAY "Are you sure (" AT 0612
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".
039260	     ACCEPT WS-OPTION AT 0631
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
039320	   IF WS-OPTION = "N"
039330         GO TO BR999.
039340       DISPLAY "Use " AT 0812 
		     "A" WITH FOREGROUND-COLOR 6 HIGHLIGHT "verage / "
		     "C" WITH FOREGROUND-COLOR 6 HIGHLIGHT "ost price".

039350 BR02.
039360	     ACCEPT WS-OPTION AT 0841
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
039420	   IF NOT (WS-OPTION = "C" OR "A")
039430         GO TO BR02.
039340	     DISPLAY "Adjust Selling Price " AT 1012
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE SPACE 	 TO WS-ADJ.

       BR03.
039360	     ACCEPT WS-ADJ AT 1037
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-ADJ
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
039420	   IF NOT (WS-ADJ = "Y" OR "N")
039430	       GO TO BR03.
039470	     DISPLAY "Stock Record :" AT 1212.
             MOVE "!"            TO STK-CODE.
             PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO BR999.
             PERFORM AY60 THRU AY999.

039480 BR05.
039490       PERFORM READ-STOCK-NEXT-LOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
	       GO TO BR900.
	   IF WS-USE-ITM = "Y"
	       DISPLAY S03-ITM
	   ELSE
	   IF WS-EXT-STK = "Y"
	       DISPLAY S03-EXT
	   ELSE
009490	       DISPLAY S03.
	   IF STK-IND = 4
	       GO TO BR25.
             MOVE 1101           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
	     MOVE STK-CODE	 TO PRT-EXT-ITEM.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
039550     IF WS-F-ERROR = 21
039560         GO TO BR25.
039570       DISPLAY "Busy updating record : " AT 1212 STK-CODE
                      WITH FOREGROUND-COLOR 11.
039580       MOVE STK-RECORD1    TO W15-RECORD1.
039620       MOVE 1              TO WS-S6.
039630       MOVE ZERO           TO W10-MCOST.

039640 BR10.
039650	   IF PRT-EXT-CODE (WS-S6) = SPACES
039660         GO TO BR15.
039670	     MOVE PRT-EXT-CODE (WS-S6)
                                 TO STK-CODE.
039680       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF STK-AVGE < 0.01
	       MOVE STK-COST	 TO STK-AVGE.
039690     IF WS-OPTION = "A"
039700         COMPUTE W10-COST = (PRT-QUANT (WS-S6) * STK-AVGE)
039710     ELSE
039720         COMPUTE W10-COST = (PRT-QUANT (WS-S6) * STK-COST).
039730       COMPUTE W10-MCOST = W10-MCOST + W10-COST.
039740     IF WS-S6 < 30
039750         ADD 1             TO WS-S6
039760         GO TO BR10.

039770 BR15.
039780       MOVE W15-RECORD1    TO STK-RECORD1.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     COMPUTE W10-COST ROUNDED = W10-MCOST * 1.

039810 BR20.
           IF STK-MARKUP = ZERO
               MOVE 25           TO STK-MARKUP.
           IF STK-CMARKUP = ZERO
               MOVE STK-MARKUP   TO STK-CMARKUP.
           IF STK-WMARKUP = ZERO
               MOVE STK-CMARKUP  TO STK-WMARKUP.
039830       MOVE W10-COST       TO STK-COST.  
039840	     COMPUTE W10-SELL ROUNDED = (W10-COST +
039850		     ((W10-COST * STK-MARKUP) / 100)).
039880	     COMPUTE W10-CSELL ROUNDED = (W10-COST +
039890		     ((W10-COST * STK-CMARKUP) / 100)).
039910       MOVE W10-CSELL      TO W10-CASH.
039880	     COMPUTE W10-CSELL ROUNDED = (W10-COST +
039890		     ((W10-COST * STK-WMARKUP) / 100)).
039910       MOVE W10-CSELL      TO W10-WSALE.
           IF W05-ROUND = "E"
               MOVE W10-SELL     TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-SELL
               MOVE W10-WSALE    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-WSALE
               MOVE W10-CASH     TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-CASH.
           IF STK-TAX NOT = ZERO
	       MOVE W05-VAT (STK-TAX)
				 TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W10-SELL +
                                           (W10-SELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE +
                                            (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W10-CASH +
                                           (W10-CASH * W05-RTE)
           ELSE
               MOVE W10-WSALE    TO W10-VWSALE
               MOVE W10-CASH     TO W10-VCASH
               MOVE W10-SELL     TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VSELL
               MOVE W10-VWSALE   TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VWSALE
               MOVE W10-VCASH    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VCASH
               PERFORM CB200.
	   IF WS-ADJ = "Y"
039870	       MOVE W10-SELL	 TO STK-SELL
039910	       MOVE W10-CASH	 TO STK-CASH
039910	       MOVE W10-WSALE	 TO STK-WSALE
	       IF (WS-USE-PACKS = "Y") AND
		  (STK-USE-PACKS = "Y")
		   PERFORM BR90 THRU BR105.
039920       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.

039930 BR25.
             UNLOCK STOCK.
039960       GO TO BR05.

       BR90.
	     MOVE 2		 TO WS-S1.

       BR95.
	   IF STK-UNT(WS-S1) = ZERO
	       GO TO BR100.
016780	     MULTIPLY STK-UNT(WS-S1) BY STK-COST
				 GIVING W10-CCOST.
016790	     MOVE STK-SL(WS-S1)	 TO W10-CSELL.
016840	   IF STK-SL(WS-S1) < W10-CCOST
016850	       MOVE ZERO	 TO STK-MKUP(WS-S1)
	   ELSE
016860	       COMPUTE STK-MKUP(WS-S1) ROUNDED =
		     ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
016790	     MOVE STK-CSH(WS-S1) TO W10-CSELL.
016840	   IF STK-CSH(WS-S1) < W10-CCOST
016850	       MOVE ZERO	 TO STK-CMKUP(WS-S1)
	   ELSE
016860	       COMPUTE STK-CMKUP(WS-S1) ROUNDED =
		     ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
016790	     MOVE STK-WSL(WS-S1) TO W10-CSELL.
016840	   IF STK-WSL(WS-S1) < W10-CCOST
	       MOVE ZERO	 TO STK-WMKUP(WS-S1)
	   ELSE
016860	       COMPUTE STK-WMKUP(WS-S1) ROUNDED =
		     ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
      *
      *    ****    C H E C K   I F   E X C L U S I V E	 R O U N D I N G
      * 	   R E Q U I R E D
      *
           IF W05-ROUND = "E"
	       MOVE STK-SL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-SL(WS-S1)
	       MOVE STK-WSL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-WSL(WS-S1)
	       MOVE STK-CSH(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-CSH(WS-S1).
	   IF STK-TAX NOT = ZERO
	       MOVE STK-TAX	 TO W10-TAX
	       MOVE W05-VAT (W10-TAX)
				 TO W05-RATE
	       COMPUTE W10-VSL(WS-S1) ROUNDED = W10-SL(WS-S1) +
				 (W10-SL(WS-S1) * W05-RTE)
	       COMPUTE W10-VWSL(WS-S1) ROUNDED = W10-WSL(WS-S1) +
				 (W10-WSL(WS-S1) * W05-RTE)
	       COMPUTE W10-VCSH(WS-S1) ROUNDED = W10-CSH(WS-S1) +
				 (W10-CSH(WS-S1) * W05-RTE)
           ELSE
	       MOVE W10-WSL(WS-S1)
				 TO W10-VWSL(WS-S1)
	       MOVE W10-CSH(WS-S1)
				 TO W10-VCSH(WS-S1)
	       MOVE W10-SL(WS-S1)
				 TO W10-VSL(WS-S1).
           IF W05-ROUND = "I"
	       MOVE W10-VSL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VSL(WS-S1)
	       MOVE W10-VWSL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VWSL(WS-S1)
	       MOVE W10-VCSH(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VCSH(WS-S1)
	       PERFORM CB300.
	     MOVE W10-SL(WS-S1)  TO STK-SL(WS-S1).
	     MOVE W10-CSH(WS-S1) TO STK-CSH(WS-S1).
	     MOVE W10-WSL(WS-S1) TO STK-WSL(WS-S1).

       BR100.
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BR95.

       BR105.
	     EXIT.

       BR900.
             PERFORM AY70 THRU AY999.

039970 BR999.
039980       EXIT.

      /
044130 ZA000-INIT	       SECTION 8.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
044150	     DISPLAY CLR-SCREEN.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   ELSE
	       GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
	     EXIT.

       ZA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
044230	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
	     MOVE PAR-CRDX	 TO WS-CARDEX.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.

       ZA01B.
044540	     OPEN OUTPUT RECOVER.
044550       CLOSE RECOVER.
044560       OPEN I-O RECOVER.
044580       MOVE 1              TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY        TO W12-TODAY.
044610       MOVE PAR-YMD        TO W12-T-YMD.
044620       MOVE PAR-COMPANY    TO W95-COMP.
044650       MOVE 3              TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
044750       MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-RECORDS    TO WS-RECORDS.
044760       MOVE STK-PASSWORD   TO WS-PASSWORD.
042520       MOVE 5              TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-ROUND	 TO W05-ROUND.
             MOVE PAR-ROUND-VAL  TO W05-ROUND-VAL.
           IF W05-ROUND-VAL > 0.01
               COMPUTE W05-HLF-VAL = W05-ROUND-VAL / 2
           ELSE
               MOVE ZERO         TO W05-HLF-VAL.
           IF W05-HLF-VAL > 0.01
               COMPUTE W05-TQTR-VAL = W05-HLF-VAL * 1.5
           ELSE
               MOVE ZERO         TO W05-TQTR-VAL.
	     MOVE 1		 TO W05-V-RATE.

       ZA05-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA05-VAT.
	     GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
	     ACCEPT WS-OPTION AT 1238
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
             GO TO ZA205.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(18) WS-TCHR(49)
021940				    WS-TCHR(60) WS-TCHR(70).
021950	     MOVE WS-G3 	 TO WS-MCHR(18) WS-MCHR(49)
021960				    WS-MCHR(60) WS-MCHR(70)
				    WS-MCHR(1)	WS-MCHR(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(18) WS-BCHR(49)
021980				    WS-BCHR(60) WS-BCHR(70).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisor�"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
044790 ZA200.
044800       DISPLAY "Files locked - Try later" AT 2312
044810                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
044820       DISPLAY "Press any key" AT 2512 
044830		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
044840	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION 8.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041800	   ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER  TO WS-FILE
041870         MOVE WS-RECKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.
041880	   IF WS-STATUS = "10"
041890         MOVE "End of FILE" TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980         MOVE "DISK full" TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040         MOVE "FILE locked" TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
045390       DISPLAY "Status " AT 1212 
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170	     STOP RUN.
