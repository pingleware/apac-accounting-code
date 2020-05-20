      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   ******    ******   **    **  *******	   ****    **	       *
      *  **    **  **	 **  **    **  **    **	  **  **   **	       *
      *  **	   **	     **    **  **    **	 **    **  **	       *
      *  **	    ******   ********  *******	 ********  **	       *
      *  **		 **  **    **  **    **	 **    **  **	       *
      *  **    **  **	 **  **    **  **    **	 **    **  **	       *
      *   ******    ******   **    **  *******	 **    **  ********    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C A S H   D R A W E R   B A L A N C I N G	R E P O R T    *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CSHBAL.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JUNE 1990.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2010
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
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000270 COPY PARAM.SL.

	   SELECT CSHDRW  ASSIGN W02-CSHDRW
                          ORGANIZATION SEQUENTIAL
                          ACCESS SEQUENTIAL.

000370	   SELECT PRNREP  ASSIGN W02-PRINTER
000380                    ORGANIZATION LINE SEQUENTIAL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000560 COPY PARAM.FDE.

      *
      *    O L D   S T Y L E   C A S H   D R A W E R
      *
       FD  CSHDRW    LABEL RECORD OMITTED.
       01  CSH-REC.
           03  CSH-OPEN     PIC  X(06).

000660 FD  PRNREP    LABEL RECORD OMITTED
000670               LINAGE WS-PGE-LENGTH.
000680 01  REP-LINE1.
000690	   03  REP-DETAIL1     PIC  X(80).
000700 01  REP-L1.
000720	   03  BAL-REP	       PIC  X(60).
000730	   03  FILLER	       PIC  X(10).
000740 01  REP-LINE2.
000750	   03  REP-DETAIL2     PIC  X(96).
000760 01  REP-LINE3.
000790	   03  REP-COMPANY     PIC  X(31).
	   03  REP-SLP	       PIC  X(05).
	   03  REP-SLP-NO      PIC  ZZ9.
	   03  FILLER	       PIC  X(01).
000800	   03  REP-DATE        PIC  X(06).
000810	   03  REP-DTE	       PIC  Z9/99/9999.
000820	   03  FILLER	       PIC  X(16).
000830 01  REP-LINE4.
000870	   03  REP-REMK        PIC  X(20).
000880	   03  FILLER	       PIC  X(01).
	   03  REP-VAL-NO.
000890	       05  REP-VAL     PIC  Z(06)9.99-.
	   03  REP-VAL-NO-R    REDEFINES REP-VAL-NO.
	       05  REP-NO      PIC  Z(06)9.
	       05  FILLER      PIC  X(04).
000900	   03  FILLER	       PIC  X(24).

      /
002130 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002180 77  WS-S1	   PIC	9(04)	 COMP-5.
002190 77  WS-S2           PIC  9(04)    COMP-5.
002200 77  WS-S3           PIC  9(04)    COMP-5.
002210 77  WS-S4           PIC  9(04)    COMP-5.
002220 77  WS-S5           PIC  9(04)    COMP-5.
002230 77  WS-S6           PIC  9(04)    COMP-5.
002240 77  WS-S7           PIC  9(04)    COMP-5.
002250 77  WS-S8           PIC  9(04)    COMP-5.
002260 77  WS-S9           PIC  9(04)    COMP-5.
002320 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
002520 77  WS-PORT         PIC  9(05) VALUE 1004.
002530 77  WS-OPEN         PIC  X(01) VALUE X"01".
002540 77  WS-CLOSE        PIC  X(01) VALUE X"00".
002750 77  WS-OPTION       PIC  X(01).
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
002640 77  WS-NORM	   PIC	X(06).
002650 77  WS-COND	   PIC	X(06).
002660 77  WS-EXPP	   PIC	X(06).
002670 77  WS-ECAN	   PIC	X(06).
002680 77  WS-8LPI	   PIC	X(06).
002690 77  WS-6LPI	   PIC	X(06).
002700 77  WS-10CPI	   PIC	X(06).
002710 77  WS-12CPI	   PIC	X(06).
002720 77  WS-17CPI	   PIC	X(06).
002510 77  WS-TME2         PIC  9(08).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
000070 77  WS-DRAW	   PIC	9(02).

003050 01  WS-DB-LINE.
003060     03  WS-TOP-LINE.
003070         05  WS-TCHAR PIC X(01) OCCURS 80.
003080     03  WS-TOP-LNE2.
003090         05  WS-TCR  PIC X(01) OCCURS 80.
003100     03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003140	   03  WS-MID-LINE.
003150         05  WS-MCHAR PIC X(01) OCCURS 80.
	   03  WS-MINV-LINE REDEFINES WS-MID-LINE.
	       05  WS-MIV1     PIC  X(01).
	       05  WS-MIV78    PIC  X(78).
	       05  WS-MIV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003180	   03  WS-BOT-LINE.
003190         05  WS-BCHAR PIC X(01) OCCURS 80.
003200     03  WS-BOT-LNE2.
003210         05  WS-BCR  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
003220     03  WS-TOP-LNE.
003230         05  WS-TCHR PIC X(01) OCCURS 80.
003240     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003250         05  FILLER  PIC X(01).
003260         05  WS-H-LINE
003270                     PIC X(78).
003280         05  FILLER  PIC X(01).
003290     03  WS-MID-LNE.
003300         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003310	   03  WS-BOT-LNE.
003320         05  WS-BCHR PIC X(01) OCCURS 80.
003330     03  WS-TP-LNE.
003340         05  WS-TCH  PIC X(01) OCCURS 80.
003350     03  WS-MD-LNE.
003360         05  WS-MCH  PIC X(01) OCCURS 80.
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
003370	   03  WS-BT-LNE.
003380	       05  WS-BCH      PIC X(01) OCCURS 80.
           03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE X"B3".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".
003390 01  WS-ENTRY.
003400     03  WS-ENT0     PIC  X(08).
003410     03  WS-TYPE0    PIC  X(01).
003420     03  WS-KEY0     PIC  9(06)    COMP-5.
003430 01  WS-ENTRY1.
003440     03  WS-ENT1     PIC  X(08).
003450     03  WS-TYPE1    PIC  X(01).
003460     03  WS-KEY1     PIC  9(06)    COMP-5.

003480 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003660 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

003910	   03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "CSHBAL.".
               05  W02-USER   PIC X(05) VALUE SPACES.
	   03  W02-CSHDRW     PIC X(04) VALUE "COM1".

       COPY W12.WS.

004720 01  W25-CALCS.
004730     03  W25-RESULT      PIC 9(05)V99.
004740     03  W25-RESULT1 REDEFINES W25-RESULT.
004750         05  W25-WHOLE   PIC 9(03).
004760         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
004770     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
004780     03  W25-VALUE       PIC S9(07)V99 COMP-3.
004790     03  W25-CREDIT      PIC S9(07)V99 COMP-3.

       01  W30-HEADING.
	   03  FILLER	       PIC  X(12) VALUE "CASH DRAWER".
	   03  W30-DRAWER      PIC  Z9.
	   03  FILLER	       PIC  X(03) VALUE " /".
	   03  W30-SLIP        PIC  ZZ9.
	   03  FILLER	       PIC  X(01) VALUE SPACES.
	   03  W30-DATE	       PIC  Z9/99/9999.

       COPY W40.WS.

       COPY FUNCTION.WS.

005610 COPY W50.WS.

006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.

       COPY W85.WS.

007010 01  W100-EDIT.
007020     03  W100-LBAL       PIC Z(06)9.99-.
007030	   03  W100-LARR       PIC Z(06)9.99.
007040     03  W100-BAL        PIC Z(08)9.99-.
007050     03  W100-CUR        PIC Z(08)9.99-.
007060     03  W100-30         PIC Z(08)9.99-.
007070     03  W100-60         PIC Z(08)9.99-.
007080     03  W100-90         PIC Z(08)9.99-.
007090     03  W100-120        PIC Z(08)9.99-.
007100     03  W100-INT        PIC Z(08)9.99-.
007110     03  W100-CRL        PIC Z(06)9.
           03  W100-SER        PIC ZZ9.
007020	   03  W100-PRC        PIC Z(06)9.99-.
007100	   03  W100-VAL        PIC Z(08)9.99-.
007120     03  W100-V2.
007130         05  W100-S6V2   PIC Z(05)9.99-.
007140	   03  W100-V3 REDEFINES W100-V2.
007150         05  W100-PRICE  PIC X(09).
007160         05  FILLER      PIC X(01).
007170	   03  W100-V4 REDEFINES W100-V2.
007180         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
007190 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010230 AA000-MAIN	       SECTION.
010240 AA000-INIT.
003240	   IF LS0-SALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AZ100.
010250	     PERFORM ZA000-INIT.
042770	     PERFORM EA000.
	     CLOSE PRNREP.
013280 AZ100.
013290       EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

010930 COPY LOCKED.RC2.

010950 COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
011110 AC000              SECTION.

011270 COPY PARAM.RD.

      /
011350 AD000-WRITE             SECTION.

011510 COPY PARAM.WR.

      /
      *    ****    BALANCING REPORT (CASH DRAW)
      *
050100 EA000		       SECTION 5.
050110 EA00.
050120       DISPLAY CLR-SCREEN.
050130       DISPLAY "CASH DRAW - BALANCING REPORT" AT 0227
050140                WITH FOREGROUND-COLOR 15.

050190 EA05.
050150	     MOVE "'P'rinter or 'S'creen"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
050260	   IF NOT (WS-OPTION = "P" OR "S")
050270         GO TO EA05.
050280     IF WS-OPTION NOT = "P"
050290	       DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3
050300	       MOVE 5		 TO LIN.
050310       MOVE SPACES         TO REP-DETAIL2.
050320       MOVE "Cash Draw - Balancing Report" 
050330                           TO REP-COMPANY.
050340       MOVE "Date:"        TO REP-DATE.
050350	     MOVE W12-TODAY	 TO REP-DTE W30-DATE.
	     MOVE WS-DRAW	 TO W30-DRAWER.
050440       ADD 150 WS-DRAW     GIVING WS-PARKEY.
050450	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE "Slip"	 TO REP-SLP.
	     MOVE PAR-SLP	 TO REP-SLP-NO W30-SLIP.
050370	   IF WS-OPTION = "P"
	       MOVE W30-HEADING  TO REP-LINE1.

050360 EA10.
050370     IF WS-OPTION = "P"
050380	       WRITE REP-LINE1 BEFORE 2 LINES
050390     ELSE
050400	       DISPLAY (LIN, 12) BAL-REP
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 5
050410	       ADD 1		 TO LIN.
050420	     MOVE SPACES	 TO REP-LINE1.

050430 EA15.
050460       MOVE "Cash Float"   TO REP-REMK.
050470       MOVE PAR-FLOAT      TO REP-VAL.
050480       PERFORM EA10.
050490       MOVE "Cash Takings" TO REP-REMK.
050500       MOVE PAR-CASH       TO REP-VAL.
050510       PERFORM EA10.
	     MOVE "Petty Cash Vouchers"
				 TO REP-REMK
050500	     MOVE PAR-PETTY	 TO REP-VAL.
050510       PERFORM EA10.
050520	     MOVE "Cash in Drawer"
				 TO REP-REMK.
050530	     ADD PAR-FLOAT PAR-CASH PAR-PETTY
050540                           GIVING REP-VAL.
050550       PERFORM EA10.
050560       MOVE "Cheques"      TO REP-REMK.
050570       MOVE PAR-CHEQUES    TO REP-VAL.
050580       PERFORM EA10.
050590	     MOVE "Credit Cards" TO REP-REMK.
050600       MOVE PAR-CARDS      TO REP-VAL.
050610	     PERFORM EA10.
050590	     MOVE "Speed Point"  TO REP-REMK.
050600	     MOVE PAR-SPEED	 TO REP-VAL.
050610       PERFORM EA10.
050590	     MOVE "Vouch/CNotes" TO REP-REMK.
050600	     MOVE PAR-VOUCHERS	 TO REP-VAL.
050610	     PERFORM EA10.
	     MOVE "EF Transfers" TO REP-REMK.
      *      MOVE "Credit Notes" TO REP-REMK.
050600	     MOVE PAR-BANKTRF	 TO REP-VAL.
050610	     PERFORM EA10.
050620     IF PAR-RECEIPTS NOT = ZERO
050630         MOVE "Receipts"   TO REP-REMK
050640         MOVE PAR-RECEIPTS TO REP-VAL
050650         PERFORM EA10.
050660     IF PAR-VATVAL NOT = ZERO
050670         MOVE "Tax"        TO REP-REMK
050680	       MOVE PAR-VATVAL	 TO REP-VAL
050690         PERFORM EA10.
050700       MOVE "Sales - Today"
050710                           TO REP-REMK.
050720       MOVE PAR-CSH        TO REP-VAL.
050730       PERFORM EA10.
050740       MOVE "      - Month"
050750                           TO REP-REMK.
050760       MOVE PAR-CSHMTD     TO REP-VAL.
050770       PERFORM EA10 2 TIMES.
050780       MOVE "TO BE BANKED" TO REP-REMK.
050790	     COMPUTE REP-VAL = PAR-CASH + PAR-CHEQUES
			     + PAR-CARDS + PAR-PETTY.
050800	     PERFORM EA10 2 TIMES.
	     MOVE "Sales Today"  TO REP-REMK.
	     MOVE PAR-CUST	 TO REP-NO.
	     PERFORM EA10.
050810     IF WS-OPTION = "P"
050820         WRITE REP-LINE1 BEFORE PAGE.
050830     IF WS-OPTION = "S"
050840	       MOVE "Any key to continue"
				 TO WS-ERR-MES
	       MOVE "1964"	 TO SCREEN-POS
	       PERFORM OPT-SETUP THRU OPT-EXIT.

050870 EA999.
050880       EXIT.

      /
052700 ZA000-INIT	       SECTION 9.
052710 ZA000-OPEN.
052720       PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
	     MOVE LS-DRAW	 TO WS-DRAW.
052730	     DISPLAY CLR-SCREEN.
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
052980	     MOVE 3		 TO WS-PARKEY.
052990       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.
       ZA01A.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
	   IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
	   IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
	   IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
	   IF TRM-STDP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH.
       ZA01B.
052760       DISPLAY "Waiting for printer response" AT 1432
052770                WITH FOREGROUND-COLOR 14 BLINK.
053560       OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
054760       MOVE 1              TO WS-PARKEY.
054770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
054780       MOVE PAR-DMY        TO W12-TODAY.
054790       MOVE PAR-YMD        TO W12-T-YMD.
055090     IF WS-DRAW = ZERO
055100         MOVE 40           TO WS-DRAW.
055110       ADD 150 WS-DRAW     GIVING WS-PARKEY.
055120       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
055130       MOVE PAR-RECORD151  TO W85-RECORD151.
      *
      *     O L D   S T Y L E   C A S H   D R A W E R
      *
           IF W85-DRAWER  = "F"
               IF W85-PORT = 2
                   MOVE "COM2"   TO W02-CSHDRW
               ELSE
               IF W85-PORT = 3
                   MOVE "COM3"   TO W02-CSHDRW
               ELSE
               IF W85-PORT = 4
                   MOVE "COM4"   TO W02-CSHDRW.
           IF W85-DRAWER = "F"
               OPEN OUTPUT CSHDRW
               MOVE ALL "A"      TO CSH-OPEN.
055260	     GO TO ZA999.
055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
055500 ZA55.
055510       MOVE 1 TO WS-S1.
055520	     MOVE SPACES TO WS-MID-LINE WS-MID-LNE.
055530 ZA60.
055540       MOVE WS-G1          TO WS-TCHAR (WS-S1) WS-BCHAR (WS-S1).
055550       MOVE WS-G1          TO WS-TCHR (WS-S1)  WS-BCHR (WS-S1).
055560       MOVE WS-G1          TO WS-TCH (WS-S1)   WS-BCH (WS-S1).
055570       MOVE WS-G8          TO WS-TCR (WS-S1)   WS-BCR (WS-S1).
055580     IF WS-S1 < 80
055590         ADD 1 TO WS-S1
055600         GO TO ZA60.
055610       MOVE WS-G9          TO WS-TCR   (1).
055620       MOVE WS-G10         TO WS-TCR   (80).
055630       MOVE WS-G11         TO WS-BCR   (1).
055640       MOVE WS-G12         TO WS-BCR   (80).
055650       MOVE WS-G14         TO WS-TCHAR (1)  WS-BCHAR (1)
055660                              WS-TCHR  (1)  WS-BCHR  (1)
055670                              WS-TCH   (1)  WS-BCH   (1).
055680       MOVE WS-G13         TO WS-TCHAR (80) WS-BCHAR (80)
055690                              WS-TCHR  (80) WS-BCHR  (80)
055700                              WS-TCH   (80) WS-BCH   (80).
055710       MOVE WS-G2          TO WS-TCHAR (4)  WS-TCHAR (19)
055720                              WS-TCHAR (50) WS-TCHAR (59) 
055730                              WS-TCHAR (69)
055740                              WS-TCHR  (18) WS-TCHR  (49)
055750                              WS-TCHR  (60) WS-TCHR  (70)
055760                              WS-TCH   (8)  WS-TCH   (49)
055770                              WS-TCH   (61) WS-TCH   (72).
055780       MOVE WS-G3          TO WS-MCHAR (4)  WS-MCHAR (19) 
055790                              WS-MCHAR (50) WS-MCHAR (59) 
055800                              WS-MCHAR (69) WS-MCHAR (80)
055810                              WS-MCHAR (1)
055820                              WS-MCH   (8)  WS-MCH   (49)
055830				    WS-MCH   (61) WS-MCH   (72)
055850                              WS-MCHR  (18) WS-MCHR  (49)
055860                              WS-MCHR  (60) WS-MCHR  (70)
055870				    WS-MCHR  (1)  WS-MCHR  (80).
055890       MOVE WS-G4          TO WS-BCHAR (4)  WS-BCHAR (19) 
055900                              WS-BCHAR (50) WS-BCHAR (59) 
055910                              WS-BCHAR (69)
055920                              WS-BCHR  (18) WS-BCHR  (49)
055930                              WS-BCHR  (60) WS-BCHR  (70)
055940                              WS-BCH   (8)  WS-BCH   (49)
055950                              WS-BCH   (61) WS-BCH   (72).
055960	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
055970 ZA200.
055980       DISPLAY "Files locked - Try later" AT 2312
055990                WITH FOREGROUND-COLOR 14
056000               " " WS-STATUS WITH FOREGROUND-COLOR 15.
056010       DISPLAY "Press any key" AT 2512 
056020		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
056030	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
056040 ZA205.
056050       EXIT PROGRAM.
056060 ZA999.
056070       EXIT.

056090 I-O-ERRORS      SECTION	9.
056100 OPEN-ERROR.
056110       DISPLAY CLR-SCREEN.
056120       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
056130       PERFORM DISPLAY-FILE-NAME.
056140
056150 READ-ERROR.
056160       DISPLAY CLR-SCREEN.
056170       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
056180       PERFORM DISPLAY-FILE-NAME.
056190
056200 WRITE-ERROR.
056210       DISPLAY CLR-SCREEN.
056220       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
056230       PERFORM DISPLAY-FILE-NAME.
056240

056260 DISPLAY-FILE-NAME.
056510	   IF WS-F-ERROR = 15
056520         MOVE WS-PARID     TO WS-FILE
056530	       MOVE WS-PARKEY	 TO WS-KEY.
056740     IF WS-STATUS = "10"
056750         MOVE "End of FILE" TO WS-STAT-MESSAGE
056760     ELSE
056770     IF WS-STATUS = "22"
056780         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
056790     ELSE
056800     IF WS-STATUS = "23"
056810         MOVE "Invalid record number" TO WS-STAT-MESSAGE
056820     ELSE
056830     IF WS-STATUS = "24"
056840         MOVE "DISK full" TO WS-STAT-MESSAGE
056850     ELSE
056860     IF WS-STATUS = "30"
056870         MOVE "DISK error" TO WS-STAT-MESSAGE
056880     ELSE
056890     IF WS-STATUS = "94"
056900         MOVE "FILE locked" TO WS-STAT-MESSAGE.
056910       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
056920       DISPLAY "Status " AT 1212 
056930                WS-STATUS WITH FOREGROUND-COLOR 11
056940               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
056950     IF WS-STATUS NOT = "94" 
056960         DISPLAY "Key    " AT 1412 
056970                  WS-KEY WITH FOREGROUND-COLOR 11
056980         DISPLAY "Reverse backup or contact program Support"
056990                  AT 1612.
057000         DISPLAY "Please make a note of these details" AT 1812.
057010       STOP RUN.
