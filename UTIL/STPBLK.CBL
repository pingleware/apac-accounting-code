      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	*******   **	    **	  **   *
      *   **	**     **     **    **	**    **  **	    **	 **    *
      *   **	       **     **    **	**    **  **	    **	**     *
      *    ******      **     *******	*******   **	    *****      *
      * 	**     **     **	**    **  **	    **	**     *
      *   **	**     **     **	**    **  **	    **	 **    *
      *    ******      **     **	*******   ********  **	  **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *        U P D A T E   B U L K   S T O C K		       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPBLK.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   SEPTEMBER 1993.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1993 - 2008
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

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

      $SET KEYCOMPRESS "7"
000010	   SELECT BULK	  ASSIGN DISK
000020			  LOCK EXCLUSIVE
000030                    STATUS WS-STATUS
000190			  ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD BLK-KEY.
      $SET NOKEYCOMPRESS

       COPY APACFIDS.SL.

000120 COPY CONTROL.SL.

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

      $SET KEYCOMPRESS "7"
000010	   SELECT SLIP	  ASSIGN DISK
000020			  LOCK EXCLUSIVE
000030                    STATUS WS-STATUS
000190			  ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD SLP-KEY
			  ALTERNATE RECORD SLP-TKEY =
			     SLP-TYPE SLP-KEY
			  ALTERNATE RECORD SLP-DKEY =
			     SLP-DRAW SLP-KEY.
      $SET NOKEYCOMPRESS

000200 COPY SPARTS.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

      /
000010 FD  BULK      LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID W02-BULK.
000030 01  BLK-RECORD1.
	   03  BLK-KEY.
000050	       05  BLK-ITEM    PIC  X(14).
	       05  BLK-ITM     PIC  X(03).
	       05  FILLER      PIC  X(01).
	   03  BLK-EXT-ITEM REDEFINES BLK-KEY
			       PIC  X(18).
000100	   03  BLK-QNT	       PIC S9(09)V9999 COMP-3.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000280 COPY PARAM.FDE.

       COPY SLIP.FDE.

000360 COPY SPARTS.FDE.

000300 COPY STOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	    PIC 999V99	  COMP-3.
001710 77  WS-SUB           PIC 9(04)     COMP-5.
001710 77  WS-S1            PIC 9(04)     COMP-5.
001720 77  WS-S2            PIC 9(04)     COMP-5.
001730 77  WS-S3            PIC 9(04)     COMP-5.
001740 77  WS-S4            PIC 9(04)     COMP-5.
001750 77  WS-S5            PIC 9(04)     COMP-5.
001760 77  WS-S6            PIC 9(04)     COMP-5.
001770 77  WS-S7            PIC 9(04)     COMP-5.
001780 77  WS-S8            PIC 9(04)     COMP-5.
001790 77  WS-IXS           PIC 9(04)     COMP-5.
001800 77  WS-IXS1          PIC 9(04)     COMP-5.
001810 77  WS-IXS2          PIC 9(04)     COMP-5.
001820 77  WS-IXS3          PIC 9(04)     COMP-5.
001830 77  WS-IXS4          PIC 9(04)     COMP-5.
001860 77  WS-PARKEY        PIC 9(06)     COMP-5.
001870 77  WS-NETKEY        PIC 9(06)     COMP-5.
001890 77  WS-RECKEY        PIC 9(06)     COMP-5.
001900 77  WS-RECOVER       PIC 9(06)     COMP-5.
001900 77  WS-RECORDS       PIC 9(06)     COMP-5.
001910 77  WS-TRANS         PIC 9(04)     COMP-5 VALUE 1.
001920 77  WS-KEYSTR        PIC 9(04)     COMP-5.
001930 77  WS-GST-RATE      PIC S9(03)V99 COMP-3.
001940 77  WS-MONTHS        PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1          PIC 9(04)     COMP-5.
001980 77  WS-ITM           PIC X(14).
       77  WS-ETYPE         PIC X(01).
002020 77  WS-OPTION	    PIC X(01).
002020 77  WS-SKIP          PIC X(01).
002040 77  WS-ADJ           PIC X(01).
002070 77  WS-ERROR         PIC 9(01).
002080 77  WS-IND           PIC 9(01)     VALUE 0.
002090 77  WS-INDB          PIC 9(01)     VALUE 0.
002100 77  WS-INDP          PIC 9(01)     VALUE 0.
002110 77  WS-INDS          PIC 9(01)     VALUE 0.
002120 77  WS-AMEND         PIC 9(01)     VALUE 0.
002140 77  WS-CONREC        PIC 9(01) VALUE ZERO.
002150 77  WS-LEDG          PIC 9(03).
002160 77  WS-LEDG1         PIC 9(03).
002180 77  WS-CHAR1	    PIC X(01).
002190 77  WS-CHAR2         PIC X(01).
002200 77  WS-COMP          PIC 9(01).
002210 77  WS-WEEK          PIC 9(01).
002220 77  WS-TYPE          PIC X(01).
002260 77  WS-BATCH         PIC 9(05).
002270 77  WS-PASSWORD      PIC X(08).
       77  WS-PRINT         PIC 9(01).
002280 77  WS-LABCDE        PIC X(08).
002280 77  WS-PASS          PIC X(08).
       77  WS-WS-HD	    PIC	 X(10) VALUE "-Wholesale".
       77  WS-CS-HD	    PIC	 X(10) VALUE "-Cash sale".
       77  WS-NODEP	    PIC  X(30) VALUE
                            "NO SUCH   D E P A R T M E N T".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
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
	       05  FILLER      PIC  X(01) VALUE "│".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "░".
	       05  FILLER      PIC  X(01) VALUE "│".
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

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "BLK".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

000010	   03  W02-BULK.
000030	       05  FILLER      PIC  X(05) VALUE "BULK_".
000040	       05  W02-PTH59   PIC  X(03).
000050	       05  FILLER      PIC  X(04) VALUE ".DAT".

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000140 COPY SLIP.ID.

000360 COPY SPARTS.ID.

000300 COPY STOCK.ID.

       COPY W10.STK.
004210	   03  W10-PQUANT      PIC S9(06)V999 COMP-3.

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

       COPY W75.STK.

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

004110 01  W90-BAL	       PIC S9(07)V99 COMP-3.

004120 01  W95-COMP	       PIC X(40).

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

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004790 AA000		  SECTION.
004800 AA000-INIT.
003240	   IF LS0-STLEV < 3 AND LS0-SALEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
004810	     PERFORM ZA000-INIT.
	     PERFORM BA000.
	     CLOSE BULK
		   SLIP.
	     OPEN OUTPUT SLIP.
	     CLOSE SLIP.
	   IF WS-INDP = 0
	       CLOSE SPARTS.
	   IF WS-INDS = 0
	       CLOSE STOCK.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.RC2.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

000020 READ-BULK.
000030	     READ BULK
	       KEY IS BLK-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
           IF WS-STATUS = "23"
	       MOVE 59		 TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 59		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-BULK	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-BULK.
000050	     GO TO READ-BULK-EXIT.

000020 READ-BULK-NEXT.
000030	     READ BULK NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 59		 TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 59		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-BULK	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-BULK-NEXT.
000050	     GO TO READ-BULK-EXIT.

000020 START-AT-BULK-KEY.
000030	     START BULK
	       KEY >= BLK-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
           IF WS-STATUS = "23"
	       MOVE 59		 TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 59		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-BULK-EXIT.

000150 READ-BULK-EXIT.
000160       EXIT.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

011310 COPY SLIP.RD.

       COPY STOCK.RD.

       COPY SPARTS.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

010000 REWRITE-BULK.
010010	     REWRITE BLK-RECORD1.
000030     IF WS-STAT1 NOT = "0"
010030	       MOVE 59		 TO WS-F-ERROR
010040         PERFORM WRITE-ERROR.
010050	     GO TO WRITE-BULK-EXIT.

010060 WRITE-BULK.
010070	     WRITE BLK-RECORD1.
000030     IF WS-STAT1 NOT = "0"
010120	       MOVE 59		 TO WS-F-ERROR
010130	       PERFORM WRITE-ERROR.
	     CLOSE BULK.
	     OPEN I-O BULK.
010050	     GO TO WRITE-BULK-EXIT.

000160 DELETE-BULK-REC.
000170	     DELETE BULK.
000180     IF WS-STAT1 NOT = "0"
000190	       MOVE 59		 TO WS-F-ERROR
000200         PERFORM WRITE-ERROR.
000230
010140 WRITE-BULK-EXIT.
010150       EXIT.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY SLIP.WR.

       COPY STOCK.WR.

       COPY APAC.HLP.

       BA000	       SECTION 50.
       BA00.
009640	     DISPLAY CLR-SCREEN.
	     DISPLAY "UPDATE BULK STOCK" AT 0232
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Accumulating Sales Quantities" AT 0812
		      WITH HIGHLIGHT.
	     DISPLAY "Reading:     " AT 1012.
	     DISPLAY "Accumulating:" AT 1212.

       BA02.
	     INITIALIZE SLP-RECORD1.
	     MOVE ZERO		 TO SLP-SEQ
013330				    SLP-REF.
	     MOVE "I"		 TO SLP-TYPE.
	     PERFORM START-AT-SLIP-TYPE THRU READ-SLIP-EXIT.
	   IF WS-F-ERROR = 38
	       GO TO BA10.

       BA05.
	     PERFORM READ-SLIP-NEXT THRU READ-SLIP-EXIT.
	   IF WS-F-ERROR = 38
	       GO TO BA10.
	   IF SLP-TYPE > "I"
	       GO TO BA10.
	     DISPLAY SLP-KEY AT 1026
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE SLP-EXT-ITEM	 TO BLK-EXT-ITEM.
	     MOVE ZERO		 TO BLK-QNT.
	     PERFORM READ-BULK THRU READ-BULK-EXIT.
	   IF WS-F-ERROR = 59
	       PERFORM WRITE-BULK THRU WRITE-BULK-EXIT.
	     DISPLAY BLK-KEY AT 1226
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	   IF SLP-3DEC = "Y" OR "3"
	       ADD SLP-WEIGHT	 TO BLK-QNT
	   ELSE
	       ADD SLP-QNT	 TO BLK-QNT.
	     PERFORM REWRITE-BULK THRU WRITE-BULK-EXIT.
      *
      *   ****	  F L A G   S L I P   R E C O R D
      * 	  Flag the record to avoid reading it again, if a
      * 	  restart is required.
      *
	     MOVE "J"		 TO SLP-TYPE.
	     PERFORM REWRITE-SLIP THRU WRITE-SLIP-EXIT.
	     GO TO BA02.

       BA10.
009640	     DISPLAY CLR-SCREEN.
	     DISPLAY "UPDATE BULK STOCK" AT 0232
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Updating Stock Records" AT 0812
		      WITH HIGHLIGHT.
	     MOVE SPACES	 TO BLK-EXT-ITEM.
	     PERFORM START-AT-BULK-KEY THRU READ-BULK-EXIT.
	   IF WS-F-ERROR = 59
	       GO TO BA999.

       BA15.
	     PERFORM READ-BULK-NEXT THRU READ-BULK-EXIT.
	   IF WS-F-ERROR = 59
	       GO TO BA999.
	     MOVE BLK-EXT-ITEM	 TO STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF STK-IND = ZERO OR
	      STK-IND > 1
	       PERFORM DELETE-BULK-REC THRU WRITE-BULK-EXIT
013770	       GO TO BA15.
	     MOVE STK-CODE	 TO PRT-EXT-ITEM.
020530	     PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510	   IF WS-F-ERROR = 21
	       GO TO BA30.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
021370       MOVE 1              TO WS-S6.

021420 BA20.
021430	   IF PRT-EXT-CODE(WS-S6) = SPACES
021450	       GO TO BA25.
021470	     MOVE PRT-EXT-CODE(WS-S6)
				 TO STK-CODE.
021490	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
021500	   IF WS-F-ERROR = 22
021520	       GO TO BA25.
	     MULTIPLY BLK-QNT BY PRT-QUANT (WS-S6)
				 GIVING W10-PQUANT.
013780	     SUBTRACT W10-PQUANT FROM STK-QUANT.
013870	     ADD W10-PQUANT	 TO STK-MTD STK-YTD.
013950	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.

021660 BA25.
	     ADD 1		 TO WS-S6.
021670     IF WS-S6 < 31
021680	       GO TO BA20.

       BA30.
	     PERFORM DELETE-BULK-REC THRU WRITE-BULK-EXIT.
	     GO TO BA15.

       BA999.
	     EXIT.

      /
044130 ZA000-INIT	       SECTION 80.
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
	     MOVE "BULKST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-BULK.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "PSLIPS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SLIP.
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
       ZA01B.
	     OPEN I-O BULK.
	   IF WS-STATUS = "05"
	       CLOSE BULK
	       OPEN OUTPUT BULK
	       CLOSE BULK
	       GO TO ZA01B.
054310	   IF RUNTIME-ERROR
054320         IF FLE-LOCKED
054330             GO TO ZA200
054340         ELSE
054350         IF FLE-LIMIT
054360             GO TO ZA49.
021300     IF WS-STATUS NOT = "00"
021310	       MOVE 59		 TO WS-F-ERROR
021320         PERFORM OPEN-ERROR.
       ZA01C.
	     OPEN I-O SLIP.
	   IF WS-STATUS = "05"
	       CLOSE SLIP
	       OPEN OUTPUT SLIP
	       CLOSE SLIP
	       GO TO ZA01C.
054310	   IF RUNTIME-ERROR
054320         IF FLE-LOCKED
054330             GO TO ZA200
054340         ELSE
054350         IF FLE-LIMIT
054360             GO TO ZA49.
021300     IF WS-STATUS NOT = "00"
021310	       MOVE 38		 TO WS-F-ERROR
021320         PERFORM OPEN-ERROR.
044480	     OPEN I-O SPARTS.
044490	   IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT OR
		 (WS-STATUS = "39")
                   MOVE 21       TO WS-F-ERROR
                   GO TO ZA50.
	   IF WS-STATUS = "41"
	       MOVE 1		 TO WS-INDP
	   ELSE
044510	   IF WS-STATUS NOT = "00"
044520         MOVE 21           TO WS-F-ERROR
050210         PERFORM OPEN-ERROR.
029240	     OPEN I-O STOCK.
029250     IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 22       TO WS-F-ERROR
                   GO TO ZA50.
	   IF WS-STATUS = "41"
	       MOVE 1		 TO WS-INDS
	   ELSE
029330	   IF WS-STATUS NOT = "00"
029340         MOVE 22 TO WS-F-ERROR
029350         PERFORM OPEN-ERROR.
044580	     MOVE 1		 TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY        TO W12-TODAY.
044610       MOVE PAR-YMD        TO W12-T-YMD.
044620       MOVE PAR-COMPANY    TO W95-COMP.
044650       MOVE 3              TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
044750       MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-RECORDS    TO WS-RECORDS.
044760       MOVE STK-PASSWORD   TO WS-PASSWORD.
042520	     MOVE 6		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

039850 ZA50.
041850	   IF WS-F-ERROR = 21
041860         MOVE "PRODUCTION" TO WS-FILE
           ELSE
           IF WS-F-ERROR = 22
	       MOVE "STOCK file" TO WS-FILE.
039980	     DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70).
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisor─"  TO WS-WRKHD
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
044890 ZB000-ERROR	       SECTION 81.
      
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
041630	       MOVE WS-NETKEY	 TO WS-KEY
041800     ELSE
041810     IF WS-F-ERROR = 7
041820         MOVE W02-DEPART   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE PRT-EXT-ITEM TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 38
041860	       MOVE W02-SLIP	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SLP-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 59
041860	       MOVE W02-BULK	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE BLK-KEY	 TO WS-KEYX.
041880     IF WS-STATUS = "10"
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
