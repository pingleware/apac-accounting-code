      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   ******    ******   ********	 ****	 ********   ******     *
      *  **    **  **	 **	**	**  **	    **	   **	 **    *
      *  **	   **		**     **    **	    **	   **	       *
      *  **	    ******	**     ********	    **	    ******     *
      *  **		 **	**     **    **	    **		 **    *
      *  **    **  **	 **	**     **    **	    **	   **	 **    *
      *   ******    ******	**     **    **	    **	    ******     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C A S H   D R A W E R   S T A T I S T I C	R E P O R T    *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CSTATS.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1997.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1997 - 2010
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

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000560 COPY PARAM.FDE.

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
002750 77  WS-OPTION       PIC  X(01).
002570 77  WS-PGE-LENGTH   PIC	9(02) VALUE 66.
002640 77  WS-NORM	   PIC	X(04).
002650 77  WS-COND         PIC  X(04).
002660 77  WS-EXPP         PIC  X(04).
002670 77  WS-ECAN         PIC  X(04).
002680 77  WS-8LPI         PIC  X(04).
002690 77  WS-6LPI         PIC  X(04).
002700 77  WS-10CPI        PIC  X(04).
002710 77  WS-12CPI        PIC  X(04).
002720 77  WS-17CPI        PIC  X(04).
002730 77  WS-DBLP         PIC  X(04).
002740 77  WS-DCAN         PIC  X(04).
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
	       05  FILLER      PIC  X(01) VALUE "�".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "�".
	       05  FILLER      PIC  X(01) VALUE "�".
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
	   03  WS-STATS-LINE   PIC  X(78) VALUE
	       "같개같같같같같같같낡같같같같같같개같같같같같같같낡같같같
      -	       "같같같개같같같같같같같".

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

002040 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

       COPY W12.WS.

      *
      *    ****    S T A T I S T I C S	 F O R	 A L L	 D R A W E R S
      *
       01  W15-CASH-DRAWERS.
      *
      *    ****    O C C U R A N C E   4 1   =	 T O T A L S
      *
	   03  W35-CASH-DRAWER OCCURS 41.
	       05  W15-CD	PIC  9(02).
001760	       05  W15-FLOAT	PIC S9(09)V99 COMP-3.
001770	       05  W15-CASH	PIC S9(09)V99 COMP-3.
002140	       05  W15-PETTY	PIC S9(09)V99 COMP-3.
      *
      *    ****    FLOAT + CASH + PETTY
      *
	       05  W15-CID	PIC S9(09)V99 COMP-3.
001780	       05  W15-CHEQUES	PIC S9(09)V99 COMP-3.
001790	       05  W15-CARDS	PIC S9(09)V99 COMP-3.
      *
      *    ****    CASH + PETTY + CHEQUES + CARDS
      *
	       05  W15-BANK	PIC S9(09)V99 COMP-3.
	       05  W15-SPEED	PIC S9(09)V99 COMP-3.
001810	       05  W15-VOUCHERS PIC S9(09)V99 COMP-3.
001820	       05  W15-RECEIPTS PIC S9(09)V99 COMP-3.
001800	       05  W15-VATVAL	PIC S9(09)V99 COMP-3.
001830	       05  W15-CSH	PIC S9(09)V99 COMP-3.

       01  W15-DISPLAY-VALUES REDEFINES W15-CASH-DRAWERS.
	   03  W15-DRAWER OCCURS 41.
	       05  W15-DRW	PIC  9(02).
	       05  W15-VALUE	PIC S9(09)V99 COMP-3 OCCURS 12.

       01  W20-HEADINGS.
	   03  W20-CD-HEADINGS.
	       05  FILLER	PIC  X(15) VALUE "      Float".
	       05  FILLER	PIC  X(15) VALUE "  Cash Takings".
	       05  FILLER	PIC  X(15) VALUE "   Petty Cash".
	       05  FILLER	PIC  X(15) VALUE " Cash in Drawer".
	       05  FILLER	PIC  X(15) VALUE "    Cheques".
	       05  FILLER	PIC  X(15) VALUE "  Credit Cards".
	       05  FILLER	PIC  X(15) VALUE "  To be banked".
	       05  FILLER	PIC  X(15) VALUE "   Speed Point".
	       05  FILLER	PIC  X(15) VALUE "    Vouchers".
	       05  FILLER	PIC  X(15) VALUE "    Receipts".
	       05  FILLER	PIC  X(15) VALUE "     V.A.T.".
	       05  FILLER	PIC  X(15) VALUE "  Sales today".

	   03  W20-CD1 REDEFINES W20-CD-HEADINGS.
	       05  W20-HDNG1	PIC  X(75).
	       05  FILLER	PIC  X(105).
	   03  W20-CD2 REDEFINES W20-CD-HEADINGS.
	       05  FILLER	PIC  X(15).
	       05  W20-HDNG2	PIC  X(75).
	       05  FILLER	PIC  X(90).
	   03  W20-CD3 REDEFINES W20-CD-HEADINGS.
	       05  FILLER	PIC  X(30).
	       05  W20-HDNG3	PIC  X(75).
	       05  FILLER	PIC  X(75).
	   03  W20-CD4 REDEFINES W20-CD-HEADINGS.
	       05  FILLER	PIC  X(45).
	       05  W20-HDNG4	PIC  X(75).
	       05  FILLER	PIC  X(60).
	   03  W20-CD5 REDEFINES W20-CD-HEADINGS.
	       05  FILLER	PIC  X(60).
	       05  W20-HDNG5	PIC  X(75).
	       05  FILLER	PIC  X(45).
	   03  W20-CD6 REDEFINES W20-CD-HEADINGS.
	       05  FILLER	PIC  X(75).
	       05  W20-HDNG6	PIC  X(75).
	       05  FILLER	PIC  X(30).
	   03  W20-CD7 REDEFINES W20-CD-HEADINGS.
	       05  FILLER	PIC  X(90).
	       05  W20-HDNG7	PIC  X(75).
	       05  FILLER	PIC  X(15).
	   03  W20-CD8 REDEFINES W20-CD-HEADINGS.
	       05  FILLER	PIC  X(105).
	       05  W20-HDNG8	PIC  X(75).

	   03  W20-HEADING	PIC  X(75).

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

       COPY W40.WS.

       01  W42-ATTRIB5.
           03  FILLER          PIC X(2000).

       01  W42-ATTRIB6.
           03  FILLER          PIC X(2000).

       01  W43-SCREEN5.
           03  FILLER          PIC X(2000).

       01  W43-SCREEN6.
           03  FILLER          PIC X(2000).

       COPY FUNCTION.WS.

005610 COPY W50.WS.

006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.

       COPY W85.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
007190 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S01.
	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "CASH DRAWER STATISTICS".
	   03  LINE  3 COLUMN  2 REVERSE-VIDEO VALUE "No.".
	   03	       COLUMN  5 REVERSE-VIDEO
				 PIC  X(75) FROM W20-HEADING.
	   03  LINE  4 COLUMN  1 BACKGROUND-COLOR 0 VALUE "�".
	   03	       COLUMN  2 VALUE "컴컫컴컴컴컴컴컴컴쩡컴컴컴컴컴컴
      -        "컫컴컴컴컴컴컴컴쩡컴컴컴컴컴컴컫컴컴컴컴컴컴컴".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "�".
	   03  LINE 24 COLUMN  1 BACKGROUND-COLOR 0 VALUE
	       "읕컴좔컴컴컴컴컴컴컨컴컴컴컴컴컴컴좔컴컴컴컴컴컴컨컴컴컴
      -        "컴컴컴컴좔컴컴컴컴컴컴켸".
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Use ".
	   03	       COLUMN  6 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 VALUE "  <- -> PgUp".
	   03	       COLUMN 20 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " or ".
	   03	       COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3 VALUE "PgUp".
	   03	       COLUMN 28 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to move data or ".
	   03	       COLUMN 45 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3 VALUE "Esc".
	   03	       COLUMN 48 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "ape to Exit".

       01  S02.
	   03  COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 5
			 PIC ZZ9 FROM W15-CD (WS-S9).
	   03  COLUMN  6 FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 5
			 PIC  ZZZ,ZZZ,ZZ9.99
			 FROM W15-VALUE (WS-S9, WS-S1).
	   03  COLUMN 21 FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 5
			 PIC  ZZZ,ZZZ,ZZ9.99
			 FROM W15-VALUE (WS-S9, WS-S2).
	   03  COLUMN 36 FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 5
			 PIC  ZZZ,ZZZ,ZZ9.99
			 FROM W15-VALUE (WS-S9, WS-S3).
	   03  COLUMN 51 FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 5
			 PIC  ZZZ,ZZZ,ZZ9.99
			 FROM W15-VALUE (WS-S9, WS-S4).
	   03  COLUMN 66 FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 5
			 PIC  ZZZ,ZZZ,ZZ9.99
			 FROM W15-VALUE (WS-S9, WS-S5).

007800 01  S05.
007810	   03  LINE 12 COLUMN 12 VALUE "Enter Password".

007820 01  S06.
007830	   03  LINE 12 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC	X(06)
007840                           USING W40-CSHPASS AUTO SECURE.

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
      *    ****    CASH DRAWER STATISTICS
      *
050100 EA000		       SECTION 5.
050110 EA00.
050120       DISPLAY CLR-SCREEN.
	     MOVE W20-HDNG1	 TO W20-HEADING.
	     DISPLAY S01.
	     MOVE 5		 TO LIN.

050190 EA05.
023890	     DISPLAY (LIN, 2) WS-STATS-LINE WITH FOREGROUND-COLOR 3.
	   IF LIN < 23
023900	       ADD 1		 TO LIN
	       GO TO EA05.

       EA06.
	     MOVE 1		 TO WS-S1.
      *
      *    ****    I N I T I A L I Z E	 T A B L E
      *
       EA10.
	     MOVE ZERO		 TO W15-CD (WS-S1)
				    W15-FLOAT (WS-S1)
				    W15-CASH (WS-S1)
				    W15-PETTY (WS-S1)
				    W15-CID (WS-S1)
				    W15-CHEQUES (WS-S1)
				    W15-CARDS (WS-S1)
				    W15-BANK (WS-S1)
				    W15-SPEED (WS-S1)
				    W15-VOUCHERS (WS-S1)
				    W15-RECEIPTS (WS-S1)
				    W15-VATVAL (WS-S1)
				    W15-CSH (WS-S1).
	   IF WS-S1 < 41
	       ADD 1		 TO WS-S1
	       GO TO EA10.
	     MOVE WS-S1 	 TO W15-CD (WS-S1).

050360 EA15.
050420	     MOVE 1		 TO WS-S1 WS-S9.
	     MOVE 5		 TO LIN.

050430 EA20.
050440	     ADD 150 WS-S1	 GIVING WS-PARKEY.
050450       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *
      *    ****    I S	 C A S H   D R A W E R	 B E I N G   U S E D ?
      *
	   IF PAR-CASH = ZERO AND
	      PAR-CHEQUES = ZERO AND
	      PAR-CARDS = ZERO AND
	      PAR-VATVAL = ZERO AND
	      PAR-VOUCHERS = ZERO AND
	      PAR-RECEIPTS = ZERO AND
	      PAR-CSH = ZERO AND
	      PAR-CSHMTD = ZERO AND
	      PAR-CSHYTD = ZERO
	       GO TO EA25.
      *
      *    ****    S T O R E   C A S H	 D R A W E R   S T A T S
      *
	     MOVE WS-S1 	 TO W15-CD (WS-S9).
	     MOVE PAR-FLOAT	 TO W15-FLOAT (WS-S9).
	     MOVE PAR-CASH	 TO W15-CASH (WS-S9).
	     MOVE PAR-PETTY	 TO W15-PETTY (WS-S9).
	     MOVE PAR-CHEQUES	 TO W15-CHEQUES (WS-S9).
	     MOVE PAR-CARDS	 TO W15-CARDS (WS-S9).
	     MOVE PAR-SPEED	 TO W15-SPEED (WS-S9).
	     MOVE PAR-VOUCHERS	 TO W15-VOUCHERS (WS-S9).
	     MOVE PAR-RECEIPTS	 TO W15-RECEIPTS (WS-S9).
	     MOVE PAR-VATVAL	 TO W15-VATVAL (WS-S9).
	     MOVE PAR-CSH	 TO W15-CSH (WS-S9).
050530	     ADD PAR-FLOAT PAR-CASH PAR-PETTY
050540				 GIVING W15-CID (WS-S9).
	     ADD PAR-CASH PAR-PETTY PAR-CHEQUES PAR-CARDS
				 GIVING W15-BANK (WS-S9).
      *
      *    ****    A C C U M U L A T E	 T O T A L S   F O R   A L L
      * 	   C A S H   D R A W E R S
      *
	     ADD PAR-FLOAT	 TO W15-FLOAT (41).
	     ADD PAR-CASH	 TO W15-CASH (41).
	     ADD PAR-PETTY	 TO W15-PETTY (41).
	     ADD PAR-CHEQUES	 TO W15-CHEQUES (41).
	     ADD PAR-CARDS	 TO W15-CARDS (41).
	     ADD PAR-SPEED	 TO W15-SPEED (41).
	     ADD PAR-VOUCHERS	 TO W15-VOUCHERS (41).
	     ADD PAR-RECEIPTS	 TO W15-RECEIPTS (41).
	     ADD PAR-VATVAL	 TO W15-VATVAL (41).
	     ADD PAR-CSH	 TO W15-CSH (41).
	     ADD W15-CID (WS-S9) TO W15-CID (41).
	     ADD W15-BANK (WS-S9)
				 TO W15-BANK (41).
	     ADD 1		 TO WS-S9.

       EA25.
      *
      *    ****    A L L   C A S H   D R A W E R S   H A N D L E D   ?
      *
	   IF WS-S1 < 40
	       ADD 1		 TO WS-S1
	       GO TO EA20.
      *
      *    ****    I F	 L E S S   T H A N   4 O   D R A W E R S   I N
      * 	   U S E   -   M O V E	 T O T A L S   U P   I N   T H E
      * 	   T A B L E
      *
	   IF WS-S9 < 41
	       IF W15-CD (WS-S9) = ZERO
		   MOVE W35-CASH-DRAWER (41)
				 TO W35-CASH-DRAWER (WS-S9)
		   MOVE ZERO	 TO W15-CD (41).
      *
      *    ****    S T A R T   A T   B E G I N N I N G	 O F   T A B L E
      *
	     MOVE 1		 TO WS-S9 WS-S1.

       EA30.
      *
      *    ****    S E T - U P	 D I S P L A Y	 H E A D I N G S   A N D
      * 	   A N D   C O R R E C T   V A L U E   P O I N T E R S
      *
	     ADD 1 WS-S1	 GIVING WS-S2.
	     ADD 2 WS-S1	 GIVING WS-S3.
	     ADD 3 WS-S1	 GIVING WS-S4.
	     ADD 4 WS-S1	 GIVING WS-S5.
	   IF WS-S1 = 1
	       MOVE W20-HDNG1	 TO W20-HEADING
	   ELSE
	   IF WS-S1 = 2
	       MOVE W20-HDNG2	 TO W20-HEADING
	   ELSE
	   IF WS-S1 = 3
	       MOVE W20-HDNG3	 TO W20-HEADING
	   ELSE
	   IF WS-S1 = 4
	       MOVE W20-HDNG4	 TO W20-HEADING
	   ELSE
	   IF WS-S1 = 5
	       MOVE W20-HDNG5	 TO W20-HEADING
	   ELSE
	   IF WS-S1 = 6
	       MOVE W20-HDNG6	 TO W20-HEADING
	   ELSE
	   IF WS-S1 = 7
	       MOVE W20-HDNG7	 TO W20-HEADING
	   ELSE
	       MOVE W20-HDNG8	 TO W20-HEADING.
	     DISPLAY S01.
	     MOVE 5		 TO LIN.
	     PERFORM EA05.
	     MOVE 5		 TO LIN.
      *
      *    ****    S A V E   S T A R T I N G   P O S S I T I O N
      *
	     MOVE WS-S9 	 TO WS-S8.

       EA35.
	   IF W15-CD (WS-S9) = ZERO
	       GO TO EA45.
	     DISPLAY S02 AT LINE LIN.
	   IF W15-CD (WS-S9) = 41
	       DISPLAY "TOT" AT LINE LIN COLUMN 02
			WITH FOREGROUND-COLOR 6 HIGHLIGHT.

       EA40.
	   IF WS-S9 = 41
	       GO TO EA45.
	   IF LIN < 23
	       ADD 1		 TO WS-S9 LIN
	       GO TO EA35.

       EA45.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF WS-S8 > 1
			 SUBTRACT 1  FROM WS-S8
			 MOVE WS-S8  TO WS-S9
			 GO TO EA30
		 WHEN DOWN-KEY
		     IF (LIN > 22) AND (W15-CD (WS-S9) < 41)
			 ADD 1	     TO WS-S8
			 MOVE WS-S8  TO WS-S9
			 GO TO EA30
		 WHEN LEFT-KEY
		     IF WS-S1 > 1
			 SUBTRACT 1  FROM WS-S1
			 MOVE WS-S8  TO WS-S9
			 GO TO EA30
		 WHEN RIGHT-KEY
		     IF WS-S1 < 8
			 ADD 1	     TO WS-S1
			 MOVE WS-S8  TO WS-S9
			 GO TO EA30
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO EA45
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO EA999
		 WHEN PAGE-DOWN
		     IF (LIN > 22) AND (W15-CD (WS-S9) < 41)
			 GO TO EA30
		 WHEN PAGE-UP
		     IF WS-S8 > 19
			 SUBTRACT 19 FROM WS-S8
		     ELSE
			 MOVE 1      TO WS-S8
		     END-IF
		     MOVE WS-S8      TO WS-S9
001340		     GO TO EA30
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE.
	     GO TO EA45.

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
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
      *	     MOVE 44		 TO USER-NUMBER.
      *	     MOVE 1		 TO USER-SETTING.
      *	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *    PRE-DISPLAY ACCEPT FIELDS BEFORE DATA-ENTRY
      *
      *      MOVE 76		 TO USER-NUMBER.
      *      MOVE 3		 TO USER-SETTING.
      *      MOVE 1		 TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
054760	     MOVE 1		 TO WS-PARKEY.
054770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
054780       MOVE PAR-DMY        TO W12-TODAY.
054790       MOVE PAR-YMD        TO W12-T-YMD.
055260	     GO TO ZA999.
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
	       MOVE "Supervisor�"  TO WS-WRKHD
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
