      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *  ******	 *******  *******  *******    ****    ******  **   **  *
      * **    ** **    ** **	** **	 **  **  **  **	   ** **  **   *
      * **	 **   **  **	** **	 ** **	  ** **	      ** **    *
      * **	 ******   *******  *******  ******** **	      ***      *
      * **	 **   **  **	   **	    **	  ** **	      ** **    *
      * **    ** **    ** **	   **	    **	  ** **	   ** **  **   *
      *  ******  **    ** **	   **	    **	  **  ******  **   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * 		   S T O C K   P A C K S		       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CRPPACKS.
000030 AUTHOR.	       J.W.LEMMON.
000040 DATE-WRITTEN.   JANUARY 1999.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1999 - 2011
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

       COPY APACFIDS.SL.

       COPY DEPART.SL.

000140 COPY PARAM.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY DEPART.FDE.

000290 COPY PARAM.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
       77  WS-P1	    PIC	 9(02)	   COMP-5.
       77  WS-S1	    PIC  9(04)	   COMP-5.
       77  WS-S2	    PIC  9(04)	   COMP-5.
       77  WS-S3	    PIC  9(04)	   COMP-5.
       77  WS-S4	    PIC  9(04)	   COMP-5.
001710 77  WS-SUB	    PIC  9(04)	   COMP-5.
001860 77  WS-PARKEY	    PIC  9(06)	   COMP-5.
002180 77  WS-USE-CASES     PIC  X(01).
002190 77  WS-USE-PACKS     PIC  X(01).
       77  WS-PACKS	    PIC  X(01) VALUE "N".
       77  WS-OPTION	    PIC  X(01).
       77  WS-RT-HD	    PIC	 X(10) VALUE "-Retail   ".
       77  WS-WS-HD	    PIC	 X(10) VALUE "-Wholesale".
       77  WS-CS-HD	    PIC	 X(10) VALUE "-Cash sale".
       77  TODAY-DDMMYY	    PIC	 9(08) COMP-5.

002420 01  WS-DB-LINE.
002430	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).

002330 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

       COPY W05.VAT.

       COPY W05.RND.

       01  W20-PRICES.
	   03  W20-EXCLUSIVE OCCURS 4.
000140	       05  W20-SL    PIC S9(08)V999   COMP-3.
000150	       05  W20-CSH   PIC S9(08)V999   COMP-3.
000160	       05  W20-WSL   PIC S9(08)V999   COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

       LINKAGE SECTION.

000290 01  LS-PARID.
000320	   03  LS-SYS-ID       PIC  X(03).

000010 01  LS-USER-ID.
000020	   03  LS-USER.
	       05  LS-USE      PIC  9(03).
000040	   03  LS-COMPANY      PIC  X(40).
	   03  LS-INSTR        PIC  X(01).
	   03  LS-STOCK-CODE.
	       05  LS-NEXT-PRG PIC  X(12).
	       05  LS-ACNO     PIC  X(06).
	   03  LS-DAYS	       PIC  9(02) COMP-5.
	   03  LS-DEM	       PIC  9(02) COMP-5.
000060	   03  LS-USUB	       PIC  9(04) COMP-5.
000070	   03  LS-DRAW	       PIC  9(02).
000080	   03  LS-CSS	       PIC  X(01).
000090	   03  LS-OK	       PIC  X(01).
000100	   03  LS-RET	       PIC  X(01).
	   03  LS-TODAY-DDMMYY PIC  9(08) COMP-5.
	   03  LS-CUR-YR       PIC  X(01) COMP-X.
	   03  LS-CUR-CEN      PIC  X(01) COMP-X.
	   03  LS-PRV-CEN      PIC  X(01) COMP-X.
	   03  LS-NXT-CEN      PIC  X(01) COMP-X.
	   03  LS-SLS-ORDNO.
	     05  LS-SLS-JOBNO  PIC  X(08) VALUE SPACES.
	   03  LS-CARDEX       PIC  X(01).
	   03  LS-CRDMTH       PIC  X(01).
	   03  APAC-SYSTEM     PIC  X(12).
	   03  APAC-VERSION    PIC  X(04).
	   03  LS-L-OR-N       PIC  X(08) VALUE "SYSTEMS\".

       COPY W10.CRP.

       01  W10-SLS-ETC.
002870	   03  W10-CCOST	 PIC S9(07)V9(6)  COMP-3.
002880	   03  W10-CSELL	 PIC S9(07)V9(6)  COMP-3.
	   03  W10-RSELL	 PIC S9(09)V99.
           03  W10-RSR1 REDEFINES W10-RSELL.
	       05  W10-RSR	 PIC S9(09).
               05  W10-RSRANDS REDEFINES W10-RSR.
		   07  W10-RSR8  PIC S9(08).
		   07  W10-RSR9  PIC  9(01).
	       05  W10-RSC	 PIC  9(02).
           03  W10-RSRC2 REDEFINES W10-RSELL.
	       05  FILLER	 PIC S9(08).
	       05  W10-RSRCC	 PIC  9V99.

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S05.
	   03  LINE  7 COLUMN 10 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
		       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -		       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "¿".
	   03  LINE  8 COLUMN 10 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 11 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
		       "           P A C K S :   S A L E S   D E T A I L
      -		       " S            ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  9 COLUMN 10 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "Ã".
	   03	       COLUMN 11 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´Pack 1ÃÄÄÄÄÄ´Pack
      -			     "2ÃÄÄÄÄÄ´Pack 3ÃÄÄÄ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "´".
	   03  LINE 10 COLUMN 10 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 11 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE " Priced per        ".
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 11 COLUMN 10 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 11 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE " Units per pack    ".
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 12 COLUMN 10 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 11 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE " Exclusive ".
	   03	       COLUMN 22 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-RT-HD.
	   03	       COLUMN 33 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 72 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 13 COLUMN 10 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 11 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "           ".
	   03	       COLUMN 22 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-CS-HD.
	   03	       COLUMN 33 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 72 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 14 COLUMN 10 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 11 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "           ".
	   03	       COLUMN 22 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-WS-HD.
	   03	       COLUMN 33 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 72 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 15 COLUMN 10 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 11 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE " Inclusive ".
	   03	       COLUMN 22 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-RT-HD.
	   03	       COLUMN 33 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 72 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 16 COLUMN 10 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 11 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "           ".
	   03	       COLUMN 22 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-CS-HD.
	   03	       COLUMN 33 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 72 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 17 COLUMN 10 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 11 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "           ".
	   03	       COLUMN 22 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-WS-HD.
	   03	       COLUMN 33 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 72 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 18 COLUMN 10 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "À".
	   03	       COLUMN 11 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
		       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -		       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S06.
	   02  S06-P1.
	   03  LINE 10 COLUMN 33 FOREGROUND-COLOR 0
				 PIC X(08) FROM W10-PER-2 AUTO.
	   03  LINE 11 COLUMN 33 FOREGROUND-COLOR 0
				 PIC ZZZ9.999 FROM W10-UNIT-2 AUTO.
	   03  LINE 10 COLUMN 46 FOREGROUND-COLOR 0
				 PIC X(08) FROM W10-PER-3 AUTO.
	   03  LINE 11 COLUMN 46 FOREGROUND-COLOR 0
				 PIC ZZZ9.999 FROM W10-UNIT-3 AUTO.
	   03  LINE 10 COLUMN 57 FOREGROUND-COLOR 0
				 PIC X(08) FROM W10-PER-4 AUTO.
	   03  LINE 11 COLUMN 57 FOREGROUND-COLOR 0
				 PIC ZZZ9.999 FROM W10-UNIT-4 AUTO.
	   02  S06-P2.
	   03  LINE 12 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-SELL-2 AUTO.
	   03  LINE 13 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-CASH-2 AUTO.
	   03  LINE 14 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-WSALE-2
				 AUTO.
	   03  LINE 12 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-SELL-3 AUTO.
	   03  LINE 13 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-CASH-3 AUTO.
	   03  LINE 14 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-WSALE-3
				 AUTO.
	   03  LINE 12 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-SELL-4 AUTO.
	   03  LINE 13 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-CASH-4 AUTO.
	   03  LINE 14 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-WSALE-4
				 AUTO.
	   02  S06-P3.
	   03  LINE 15 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99 USING W10-VSELL-2
				 AUTO.
	   03  LINE 16 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99 USING W10-VCASH-2
				 AUTO.
	   03  LINE 17 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99 USING W10-VWSALE-2
				 AUTO.
	   03  LINE 15 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99 USING W10-VSELL-3
				 AUTO.
	   03  LINE 16 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99 USING W10-VCASH-3
				 AUTO.
	   03  LINE 17 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99 USING W10-VWSALE-3
				 AUTO.
	   03  LINE 15 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99 USING W10-VSELL-4
				 AUTO.
	   03  LINE 16 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99 USING W10-VCASH-4
				 AUTO.
	   03  LINE 17 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99 USING W10-VWSALE-4
				 AUTO.

004870 COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

001230 PROCEDURE DIVISION USING LS-PARID
				LS-USER-ID
				W10-STOCK.
007740 AA000-MAIN	       SECTION.
007750 AA000-INIT.
007760	     PERFORM ZA000-INIT.
	     PERFORM SAVE-SCREEN-3.
	     PERFORM BB000.

       AA05.
	     EXIT PROGRAM.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY LOCKED.RC2.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

      /
      *    R O U N D I N G   O F   S E L L I N G   P R I C E S
      *
      *    This routine will round the selling price as follows:
      *
      *    If the rounding factor is less than 50 cents or greater
      *    than R1.00 but less than five rand, then the selling price
      *    (.CC or R.CC portion) will be rounded to a multiple of the
      *    rounding factor.
      *
      *    If the rounding factor is R1.00, then if the selling price
      *    is greater than 79 cents it will be rounded to the next rand.
      *
      *    If the rounding factor is greater than 49 cents and less than
      *    R1.00 or greater than R4.99 then the selling price (.CC or
      *    R.CC portion) will be changed to the rounding value. If the 
      *    rounding portion of the selling price is greater than the
      *    factor then the system will amend these figures and change
      *    the adjoining value.
      *
       CB100         SECTION.
       CB101.
      *
      *    ****    I F	 T H E	 V A L U E   I S   Z E R O  -  E X I T
      *
	   IF W10-RSELL = ZERO
	       GO TO CB199.
      *
      *    ROUND TO THE NEXT RAND
      *
           IF W05-ROUND-VAL = 1.00
               IF W10-RSC = ZERO
                   GO TO CB199
               ELSE
      *
      *    SELLING PRICE LESS THAN ONE RAND
      *
               IF W10-RSELL < 1.00
      *
      *    SELLING PRICE IS MORE THAN 79 CENTS - CHANGE TO R1.00
      *
                   IF W10-RSC > 79
                       MOVE 1.00 TO W10-RSELL
                       GO TO CB199
                   ELSE
      *
      *    IF THE SELLING PRICE IS LESS THAN 80 CENTS DO NOT ROUND UP
      *
                       GO TO CB199
      *
      *    SELLING PRICE MORE THAN ONE RAND
      *
               ELSE
      *
      *    CENTS PORTION OF PRICE IS LESS THAN TEN CENTS - DROP CENTS
      *
                   IF W10-RSC < 10
                       MOVE ZERO TO W10-RSC
                       GO TO CB199
                   ELSE
      *
      *    ROUND UP TO NEXT R1.00
      *
                       MOVE ZERO TO W10-RSC
                       ADD 1     TO W10-RSR
                       GO TO CB199.
      *
      *    ROUNDING CENTS ONLY
      *
           IF W05-ROUND-VAL < 1.00
      *
      *    ROUNDING VALUE AND CENTS PORTION ARE EQUAL - EXIT
      *
               IF W10-RSC = W05-CENTS
                   GO TO CB199
               ELSE
      *
      *    ROUNDING VALUE IS 50 CENTS OR MORE
      *
               IF W05-ROUND-VAL > .50
      *
      *    IF CENTS PORTION TO BE ROUNDED IS LESS THAN ROUNDING VALUE
      *
                   IF W10-RSC < W05-CENTS
                       MOVE W05-CENTS
                                 TO W10-RSC
                       GO TO CB199
      *
      *    IF CENTS PORTION TO BE ROUNDED IS MORE THAN ROUNDING VALUE
      *
                   ELSE
                       MOVE W05-CENTS
                                 TO W10-RSC
                       ADD 1     TO W10-RSR
                       GO TO CB199
      *
      *    ROUNDING VALUE IS LESS THAN 50 CENTS
      *
               ELSE       
                   GO TO CB120.
      *
      *    IF ROUNDING VALUE EQUALS THE R.CC OF VALUE TO BE ROUNDED - EXIT
      *
           IF W10-RSRCC = W05-ROUND-VAL
               GO TO CB199.
      *
      *    SELLING PRICE IS NOT MORE THAN HALF THE ROUNDING FACTOR - EXIT
      *
           IF W10-RSELL NOT > W05-HLF-VAL
               GO TO CB199.
           IF W05-ROUND-VAL > 5.00
               GO TO CB110.
      *
      *    ROUNDING FACTOR IS LESS THAN FIVE RAND 
      *
      *    R.CC OF VALUE TO BE ROUNDED IS LESS THAN ROUNDING FACTOR
      *
           IF W10-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL
                                 TO W10-RSRCC
               GO TO CB199.
             MOVE W05-ROUND-VAL  TO W05-CALC-VAL.
       CB105.
           IF W10-RSRCC = W05-CALC-VAL
               GO TO CB199.
           IF W10-RSRCC < W05-CALC-VAL
               MOVE W05-CALC-VAL TO W10-RSRCC
               GO TO CB199.
             ADD W05-ROUND-VAL   TO W05-CALC-VAL.
           IF W05-CALC-VAL < 10.00
               GO TO CB105.
             ADD 1               TO W10-RSR8.
             MOVE W05-RVRCC      TO W10-RSRCC.
             GO TO CB199.
      *
      *    ROUNDING FACTOR GREATER THAN R5.00
      *
      *    SELLING PRICE IS NOT MORE THAN 75% OF ROUNDING FACTOR - EXIT
      *
       CB110.
           IF W10-RSELL NOT > W05-TQTR-VAL
               GO TO CB199.
       CB115.
           IF W10-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL 
                                 TO W10-RSRCC
               GO TO CB199.
             ADD 1               TO W10-RSR8.
             MOVE W05-ROUND-VAL  TO W10-RSRCC.
             GO TO CB199.
      *
      *    ROUND UP VALUES WHERE ROUNDING FACTOR IS LESS THAN 50 CENTS
      *
       CB120.
	   IF W05-CENTS = 2 OR 4 OR 5 OR 10 OR 20 OR 25
	       MOVE ZERO	 TO WS-SUB
	   ELSE
	       MOVE W05-CENTS	 TO WS-SUB.
       CB125.
           IF W10-RSC = WS-SUB
               GO TO CB199.
           IF W10-RSC < WS-SUB
               MOVE WS-SUB       TO W10-RSC
               GO TO CB199.
             ADD W05-CENTS       TO WS-SUB.
           IF WS-SUB < 100
               GO TO CB125.
             ADD 1               TO W10-RSR.
             SUBTRACT 100 FROM WS-SUB
                                 GIVING W10-RSC.
       CB199.
             EXIT.
      *
      *    ****    C A L C U L A T E   E X C L U S I V E   P R I C E
      *              F R O M   T H E   I N C L U S I V E   P R I C E
      *
       CB200         SECTION.
       CB201.
	   IF W10-TAX NOT = ZERO
	       COMPUTE W10-SL(WS-P1) ROUNDED = (W10-VSL(WS-P1) /
				(100.00000 + W05-RATE)) * 100.00
	       COMPUTE W10-WSL(WS-P1) ROUNDED = (W10-VWSL(WS-P1) /
				 (100.00000 + W05-RATE)) * 100.00
	       COMPUTE W10-CSH(WS-P1) ROUNDED = (W10-VCSH(WS-P1) /
				(100.00000 + W05-RATE)) * 100.00
           ELSE
	       MOVE W10-VWSL(WS-P1)
				 TO W10-WSALE
	       MOVE W10-VCSH(WS-P1)
				 TO W10-CASH
	       MOVE W10-VSL(WS-P1)
				 TO W10-SELL.
	   IF W10-TAX NOT = ZERO
	       COMPUTE W10-CHECK ROUNDED
		   = W10-SL(WS-P1) + (W10-SL(WS-P1) * W05-RTE)
	       IF W10-CHECK > W10-VSL(WS-P1)
		   SUBTRACT 0.01 FROM W10-SL(WS-P1)
	       ELSE
	       IF W10-CHECK < W10-VSL(WS-P1)
		   ADD 0.01	 TO W10-SL(WS-P1)
	       END-IF
	       COMPUTE W10-CHECK ROUNDED
		   = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE)
	       IF W10-CHECK > W10-VCSH(WS-P1)
		   SUBTRACT 0.01 FROM W10-CSH(WS-P1)
	       ELSE
	       IF W10-CHECK < W10-VCSH(WS-P1)
		   ADD 0.01	 TO W10-CSH(WS-P1)
	       END-IF
	       COMPUTE W10-CHECK ROUNDED
		   = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE)
	       IF W10-CHECK > W10-VWSL(WS-P1)
		   SUBTRACT 0.01 FROM W10-WSL(WS-P1)
	       ELSE
	       IF W10-CHECK < W10-VWSL(WS-P1)
		   ADD 0.01	 TO W10-WSL(WS-P1).

       CB299.
             EXIT.
      *
      *    ****    P A C K S
      *
       BB000	       SECTION 51.
       BB00.
      *
      *    ***** SKIP SINGLE PRICES *****
      *
	     MOVE 2		 TO WS-P1.
      *
      *    ***** STORE PACK PRICES *****
      *
       BB02.
	     MOVE W10-SL(WS-P1)  TO W20-SL(WS-P1).
	     MOVE W10-CSH(WS-P1) TO W20-CSH(WS-P1).
	     MOVE W10-WSL(WS-P1) TO W20-WSL(WS-P1).
	   IF WS-P1 < 4
	       ADD 1		 TO WS-P1
	       GO TO BB02.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 12		 TO SHADE-COL.
	     MOVE 61		 TO SHADE-WIDTH.
	     MOVE 11		 TO SHADE-LINES.
	     DISPLAY S05.
	     PERFORM SCREEN-SHADOW.

       BB05.
      *
      *    ***** SKIP SINGLE PRICES *****
      *
	     MOVE 2		 TO WS-P1.
      *
      *      ***** CHECK FOR END OF PACKS *****
      *
       BB10.
	   IF W10-UNT(WS-P1) = ZERO
	       GO TO BB15.
016780	     MULTIPLY W10-UNT(WS-P1) BY W10-COST
				 GIVING W10-CCOST.
016720	   IF W10-MKUP(WS-P1) NOT > ZERO
016730	       PERFORM BB55.
      *
      *      ***** CALCULATE NEW SELLING PRICES *****
      *
016860	     COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
016870		   ((W10-CCOST * W10-MKUP(WS-P1)) / 100)).
016890	     MOVE W10-CSELL	 TO W10-SL(WS-P1).
016720	   IF W10-CMKUP(WS-P1) NOT > ZERO
016730	       PERFORM BB60.
016790	     MOVE W10-CSH(WS-P1) TO W10-CSELL.
016860	     COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
016870		   ((W10-CCOST * W10-CMKUP(WS-P1)) / 100)).
016890	     MOVE W10-CSELL	 TO W10-CSH(WS-P1).
016720	   IF W10-WMKUP(WS-P1) NOT > ZERO
016730	       PERFORM BB65.
016790	     MOVE W10-WSL(WS-P1) TO W10-CSELL.
016860	     COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
016870		   ((W10-CCOST * W10-WMKUP(WS-P1)) / 100)).
016890	     MOVE W10-CSELL	 TO W10-WSL(WS-P1).
      *
      *    ****    C H E C K   I F   E X C L U S I V E	 R O U N D I N G
      * 	   R E Q U I R E D
      *
           IF W05-ROUND = "E"
	       MOVE W10-SL(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-SL(WS-P1)
	       MOVE W10-WSL(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-WSL(WS-P1)
	       MOVE W10-CSH(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-CSH(WS-P1).
	   IF W10-TAX NOT = ZERO
      *
      *    ***** CALCULATE INCLUSIVE PRICES *****
      *
	       MOVE W05-VAT(W10-TAX)
				 TO W05-RATE
	       COMPUTE W10-VSL(WS-P1) ROUNDED = W10-SL(WS-P1) +
				 (W10-SL(WS-P1) * W05-RTE)
	       COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) +
				 (W10-WSL(WS-P1) * W05-RTE)
	       COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) +
				 (W10-CSH(WS-P1) * W05-RTE)
           ELSE
      *
      *    ***** NON TAXABLE ITEMS *****
      *
	       MOVE W10-WSL(WS-P1)
				 TO W10-VWSL(WS-P1)
	       MOVE W10-CSH(WS-P1)
				 TO W10-VCSH(WS-P1)
	       MOVE W10-SL(WS-P1)
				 TO W10-VSL(WS-P1).
      *
      *    ****    C H E C K   I F   I N C L U S I V E	 R O U N D I N G
      * 	   R E Q U I R E D
      *
	   IF W05-ROUND = "I"
	       MOVE W10-VSL(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VSL(WS-P1)
	       MOVE W10-VWSL(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VWSL(WS-P1)
	       MOVE W10-VCSH(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VCSH(WS-P1)
               PERFORM CB200.
	     MOVE ZERO		 TO W05-RATE.
           IF W10-TAX > 0
	       MOVE W05-VAT(W10-TAX)
				 TO W05-RATE.
	     COMPUTE W10-VCSH(WS-P1) ROUNDED
		   = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE).
	     COMPUTE W10-VWSL(WS-P1) ROUNDED
		   = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE).
	     COMPUTE W10-VSL(WS-P1) ROUNDED
		   = W10-SL(WS-P1) + (W10-SL(WS-P1) * W05-RTE).
      *
      *      ***** CHECK FOR END OF PACKS *****
      *
       BB15.
	   IF WS-P1 < 4
	       ADD 1		 TO WS-P1
	       GO TO BB10.
      *
      *    ***** DISPLAY PACK DETAILS *****
      *
	     DISPLAY S06.
      *
      *    ***** CHECK IF SELLING PRICES (PACK 2) THE SAME *****
      *
	   IF (W10-SL(2) = W20-SL(2)) AND
	      (W10-CSH(2) = W20-CSH(2)) AND
	      (W10-WSL(2) = W20-WSL(2))
	       GO TO BB25.
	     MOVE SPACE		 TO WS-OPTION.
      *
      *    *** ALLOW USER TO DECIDE IF NEW PRICES ARE TO BE APPLIED ***
      *
       BB20.
	     MOVE "Use new selling prices Y/N  [ ]"
				 TO WS-ERR-MES.
	     MOVE 19		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO BB20.
014730     IF WS-OPTION = "Y"
      *
      *    ***** USE NEW PRICES *****
      *
014740	       GO TO BB25.
      *
      *    ***** USE OLD PRICES *****
      *
	     PERFORM BB75 THRU BB85.
      *
      *    ***** SKIP SINGLE PRICES *****
      *
	     MOVE 2		 TO WS-P1.

       BB22.
	   IF W10-TAX NOT = ZERO
      *
      *    ***** CALCULATE THE INCLUSIVE PRICES *****
      *
	       MOVE W05-VAT(W10-TAX)
				 TO W05-RATE
      *
      *    ***** RETAIL PRICE (price 1) *****
      *
	       COMPUTE W10-VSL(WS-P1) ROUNDED = W10-SL(WS-P1) +
				 (W10-SL(WS-P1) * W05-RTE)
      *
      *    ***** WHOLESALE PRICE (price 3) *****
      *
	       COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) +
				 (W10-WSL(WS-P1) * W05-RTE)
      *
      *    ***** CASH PRICE (price 2) *****
      *
	       COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) +
				 (W10-CSH(WS-P1) * W05-RTE)
           ELSE
      *
      *	   ***** NON-TAXABLE *****
      *
	       MOVE W10-WSL(WS-P1)
				 TO W10-VWSL(WS-P1)
	       MOVE W10-CSH(WS-P1)
				 TO W10-VCSH(WS-P1)
	       MOVE W10-SL(WS-P1)
				 TO W10-VSL(WS-P1).
	   IF WS-P1 < 4
	       ADD 1		 TO WS-P1
	       GO TO BB22.
	     DISPLAY S06.
      *
      *    ***** IS ITEM SUBJECT TO VAT *****
      *
       BB25.
	   IF NOT (W10-TAX = ZERO)
	       GO TO BB35.
      *
      *    ***** ASK USER IF PRICES ARE TO BE ADJUSTED *****
      *
       BB30.
	     MOVE "Adjust selling prices Y/N  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     MOVE 19		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO BB30.
	   IF WS-OPTION = "N"
	       GO TO BB990.
      *
      *    ***** ASK THE USER WHICH PRICES ARE TO BE ADJUSTED *****
      *
       BB35.
	     MOVE "Adjust 'E'x/'I'nclusive price or 'N'one  [E]"
				 TO WS-ERR-MES.
	     MOVE "E"		 TO WS-OPTION.
	     MOVE 16		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "E" OR "I" OR "N")
	       GO TO BB35.

	   IF WS-OPTION = "N"
      *
      *    ***** DO NOT ADJUST THE PRICES *****
      *
	       GO TO BB990.
	   IF WS-OPTION = "E"
	       GO TO BB45.
      *
      *    ***** ALLOW USER TO ADJUST INCLUSIVE PRICES *****
      *
       BB40.
	     ACCEPT S06-P3.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     GO TO BB35
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BB40
	       END-EVALUATE.
      *
      *    ***** CALCULATE EXCLUSIVE PRICES *****
      *
	     MOVE 2		 TO WS-S1.
	     MOVE W05-VAT(W10-TAX)
				 TO W05-RATE.

       BB42.
      *
      *    ***** RETAIL PRICE (price 1) *****
      *
	   IF NOT (W10-VSL(WS-S1) = ZERO)
	       COMPUTE W10-SL(WS-S1) ROUNDED = (W10-VSL(WS-S1) /
		      (100.000 + W05-RATE)) * 100.000.
      *
      *    ***** CASH PRICE (price 2) *****
      *
	   IF NOT (W10-VCSH(WS-S1) = ZERO)
	       COMPUTE W10-CSH(WS-S1) ROUNDED = (W10-VCSH(WS-S1) /
		      (100.000 + W05-RATE)) * 100.000.
      *
      *    ***** WHOLESALE PRICE (price 3) *****
      *
	   IF NOT (W10-VWSL(WS-S1) = ZERO)
	       COMPUTE W10-WSL(WS-S1) ROUNDED = (W10-VWSL(WS-S1) /
		      (100.000 + W05-RATE)) * 100.000.
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BB42.
	     MOVE 20		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
	   IF WS-OPTION = "N"
	       GO TO BB40.
	     GO TO BB990.
      *
      *    ***** ALLOW USER TO ADJUST EXCLUSIVE PRICES *****
      *
       BB45.
	     ACCEPT S06-P2.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     GO TO BB35
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BB45
	       END-EVALUATE.
      *
      *    ***** CALCULATE INCLUSIVE PRICES *****
      *
	     MOVE 2		 TO WS-S1.
	     MOVE W05-VAT(W10-TAX)
				 TO W05-RATE.

       BB50.
      *
      *    ***** RETAIL PRICE (price 1) *****
      *
	   IF NOT (W10-SL(WS-S1) = ZERO)
	       COMPUTE W10-VSL(WS-P1) ROUNDED = W10-SL(WS-P1) +
				 (W10-SL(WS-P1) * W05-RTE).
      *
      *    ***** WHOLESALE PRICE (price 3) *****
      *
	   IF NOT (W10-WSL(WS-S1) = ZERO)
	       COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) +
				 (W10-WSL(WS-P1) * W05-RTE).
      *
      *    ***** CASH PRICE (price 2) *****
      *
	   IF NOT (W10-CSH(WS-S1) = ZERO)
	       COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) +
				 (W10-CSH(WS-P1) * W05-RTE).
	     MOVE 20		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
	   IF WS-OPTION = "N"
	       GO TO BB45.
	     GO TO BB990.
      *
      *    ****    D E F A U L T   M A R K U P
      *
       BB55.
	   IF (W10-SL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
	       MOVE W10-SL(WS-P1)
				 TO W10-CSELL
	       COMPUTE W10-MKUP(WS-P1) ROUNDED =
		    (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100).

       BB56.
	     EXIT.
      *
      *    ****    D E F A U L T   C A S H   M A R K U P
      *
       BB60.
	   IF (W10-CSH(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
	       MOVE W10-CSH(WS-P1)
				 TO W10-CSELL
	       COMPUTE W10-CMKUP(WS-P1) ROUNDED =
		    (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100).

       BB61.
	     EXIT.
      *
      *    ****    D E F A U L T   W H O L E S A L E   M A R K U P
      *
       BB65.
	   IF (W10-WSL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
	       MOVE W10-WSL(WS-P1)
				 TO W10-CSELL
	       COMPUTE W10-WMKUP(WS-P1) ROUNDED =
		    (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100).

       BB66.
	     EXIT.
      *
      *    ****    R E S T O R E   O L D   P R I C E S
      *
       BB75.
      *
      *    ***** SKIP SINGLE PRICES *****
      *
	     MOVE 2		 TO WS-P1.

       BB80.
	     MOVE W20-SL(WS-P1)  TO W10-SL(WS-P1).
	     MOVE W20-CSH(WS-P1) TO W10-CSH(WS-P1).
	     MOVE W20-WSL(WS-P1) TO W10-WSL(WS-P1).
	   IF WS-P1 < 4
	       ADD 1		 TO WS-P1
	       GO TO BB80.

       BB85.
	     EXIT.

       BB990.
	     PERFORM RESTORE-SCREEN-3.

       BB999.
	     EXIT.

      /
044130 ZA000-INIT	       SECTION 80.
044140 ZA000-OPEN.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
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
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
044230	     MOVE 1		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-DMY	 TO TODAY-DDMMYY.
044230       MOVE 3              TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
042520	     MOVE 5		 TO WS-PARKEY.
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
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT (PAR-RT-HD = X"00000000000000000000")
	       MOVE PAR-RT-HD	 TO WS-RT-HD.

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
041810	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
	   ELSE
041850	   IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.
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
