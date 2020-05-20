      $set LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *  ******	 ******** *******  *******    ****    ******  **   **  *
      * **    **    **	  **	** **	 **  **  **  **	   ** **  **   *
      * **	    **	  **	** **	 ** **	  ** **	      ** **    *
      *  ******     **	  *******  *******  ******** **	      ***      *
      *       **    **	  **	   **	    **	  ** **	      ** **    *
      * **    **    **	  **	   **	    **	  ** **	   ** **  **   *
      *  ******     **	  **	   **	    **	  **  ******  **   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * 	   S T O C K   P A C K S   A N D   C A S E S	       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPPACKS.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   JULY 1998.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1998 - 2008
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

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY DEPART.FDE.

000280 COPY PARAM.FDE.

000300 COPY STOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
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

000300 COPY STOCK.ID.

       COPY W05.VAT.

       COPY W05.RND.

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
	   03  LS-NEXT-PRG     PIC  X(12).
	   03  LS-DAYS	       PIC  9(02) COMP-5.
	   03  LS-DEM	       PIC  9(02) COMP-5.
000060	   03  LS-USUB	       PIC  9(04) COMP-5.
000070	   03  LS-DRAW	       PIC  9(02).
000080	   03  LS-CSS	       PIC  X(01).
000090	   03  LS-OK	       PIC  X(01).
000100	   03  LS-RET	       PIC  X(01).
	   03  LS-TODAY-DDMMYY PIC  9(08) COMP-5.
	   03  LS-SLS-ORDNO.
	     05  LS-SLS-JOBNO  PIC  X(08) VALUE SPACES.
	   03  LS-CARDEX       PIC  X(01).
	   03  LS-CRDMTH       PIC  X(01).
	   03  APAC-SYSTEM     PIC  X(12).
	   03  APAC-VERSION    PIC  X(04).
	   03  LS-L-OR-N       PIC  X(08) VALUE "SYSTEMS\".

       COPY W10.STK.

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S01.
	   03  LINE 10 COLUMN 30 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 50 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "¿".
	   03  LINE 11 COLUMN 30 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
				 VALUE " Use cases (".
	   03	       COLUMN 43 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Y".
	   03	       COLUMN 44 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
				 VALUE "/".
	   03	       COLUMN 45 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "N".
	   03	       COLUMN 46 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
				 VALUE ")   ".
	   03	       COLUMN 50 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 12 COLUMN 30 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "À".
	   03	       COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S02.
	   03  LINE 10 COLUMN 26 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 54 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "¿".
	   03  LINE 11 COLUMN 26 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 27 FOREGROUND-COLOR 4
				 BACKGROUND-COLOR 7
				 VALUE " Number of Cases           ".
	   03	       COLUMN 54 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 12 COLUMN 26 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "À".
	   03	       COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S03.
	   03  LINE 11 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9 USING W10-CASES AUTO.

       01  S04.
	   03  LINE 10 COLUMN 30 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 50 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "¿".
	   03  LINE 11 COLUMN 30 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
				 VALUE " Use packs (".
	   03	       COLUMN 43 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Y".
	   03	       COLUMN 44 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
				 VALUE "/".
	   03	       COLUMN 45 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "N".
	   03	       COLUMN 46 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
				 VALUE ")   ".
	   03	       COLUMN 50 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 12 COLUMN 30 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "À".
	   03	       COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05.
	   03  LINE  7 COLUMN 11 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
		       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -		       "ÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "¿".
	   03  LINE  8 COLUMN 11 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
		       "          P A C K S :   S A L E S   D E T A I L
      -		       "S          ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  9 COLUMN 11 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "Ã".
	   03	       COLUMN 12 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´Pack 1ÃÄÄÄÄÄ´Pack 2Ã
      -			     "ÄÄÄÄÄ´Pack 3ÃÄÄÄ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "´".
	   03  LINE 10 COLUMN 11 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 12 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE " Priced per        ".
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 11 COLUMN 11 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 12 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE " Units per pack    ".
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 12 COLUMN 11 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 12 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE " Markup ".
	   03	       COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-RT-HD.
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 13 COLUMN 11 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 12 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "        ".
	   03	       COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-CS-HD.
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 14 COLUMN 11 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 12 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "        ".
	   03	       COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-WS-HD.
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 15 COLUMN 11 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 12 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE " Price  ".
	   03	       COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-RT-HD.
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 16 COLUMN 11 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 12 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "        ".
	   03	       COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-CS-HD.
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 17 COLUMN 11 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03	       COLUMN 12 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "        ".
	   03	       COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-WS-HD.
	   03	       COLUMN 31 FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		       VALUE "                                       ".
	   03	       COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 18 COLUMN 11 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "À".
	   03	       COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
		       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -		       "ÄÄÄÄÄÄÄÄÄÄÙ".

       01  S06.
	   02  S06-P1.
	   03  LINE 10 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(08) USING W10-PER-2 AUTO.
	   03  LINE 11 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZ9.999 USING W10-UNIT-2 AUTO.
	   03  LINE 12 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.9999 USING W10-MARKUP-2 AUTO.
	   03  LINE 13 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.9999 USING W10-CMARKUP-2 AUTO.
	   03  LINE 14 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.9999 USING W10-WMARKUP-2 AUTO.
	   03  LINE 15 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-SELL-2 AUTO.
	   03  LINE 16 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-CASH-2 AUTO.
	   03  LINE 17 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-WSALE-2
				 AUTO.
	   02  S06-P2.
	   03  LINE 10 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(08) USING W10-PER-3 AUTO.
	   03  LINE 11 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZ9.999 USING W10-UNIT-3 AUTO.
	   03  LINE 12 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.9999 USING W10-MARKUP-3 AUTO.
	   03  LINE 13 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.9999 USING W10-CMARKUP-3 AUTO.
	   03  LINE 14 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.9999 USING W10-WMARKUP-3 AUTO.
	   03  LINE 15 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-SELL-3 AUTO.
	   03  LINE 16 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-CASH-3 AUTO.
	   03  LINE 17 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-WSALE-3
				 AUTO.
	   02  S06-P3.
	   03  LINE 10 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(08) USING W10-PER-4 AUTO.
	   03  LINE 11 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZ9.999 USING W10-UNIT-4 AUTO.
	   03  LINE 12 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.9999 USING W10-MARKUP-4 AUTO.
	   03  LINE 13 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.9999 USING W10-CMARKUP-4 AUTO.
	   03  LINE 14 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.9999 USING W10-WMARKUP-4 AUTO.
	   03  LINE 15 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-SELL-4 AUTO.
	   03  LINE 16 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-CASH-4 AUTO.
	   03  LINE 17 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.999 USING W10-WSALE-4
				 AUTO.

004870 COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

001230 PROCEDURE DIVISION USING LS-PARID
				LS-USER-ID
				W10-STOCK
				W10-SALES-ETC.
007740 AA000-MAIN	       SECTION.
007750 AA000-INIT.
007760	     PERFORM ZA000-INIT.
	     PERFORM SAVE-SCREEN-3.
	   IF WS-USE-CASES = "Y"
	       PERFORM BA000.
	   IF WS-USE-PACKS = "Y"
	       PERFORM BB000.

       AA05.
	     EXIT PROGRAM.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY STOCK.RD.

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
      *    ****    C A S E S
      *
       BA000	       SECTION 50.
       BA00.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 32		 TO SHADE-COL.
	     MOVE 19		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY S01.
	     PERFORM SCREEN-SHADOW.
	     MOVE SPACE 	 TO W10-USE-CASES.

       BA05.
	     ACCEPT W10-USE-CASES AT 1148
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING W10-USE-CASES
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W10-USE-CASES = "Y" OR "N")
	       CALL X"E5"
	       GO TO BA05.
	     PERFORM RESTORE-SCREEN-3.
	   IF W10-USE-CASES = "N"
	       MOVE ZERO	 TO W10-CASES
	       GO TO BA999.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 27		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY S02.
	     PERFORM SCREEN-SHADOW.

       BA10.
	     ACCEPT S03.
	     MOVE 15		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
	     PERFORM RESTORE-SCREEN-3.
	   IF WS-OPTION = "N"
	       GO TO BA00.

       BA999.
	     EXIT.
      *
      *    ****    P A C K S
      *
       BB000	       SECTION 51.
       BB00.
	   IF W10-USE-PACKS = "Y"
	       MOVE "Y" 	 TO WS-PACKS
	       PERFORM BB08
	       DISPLAY S06
	       GO TO BB90
	   ELSE
	       MOVE "N" 	 TO WS-PACKS.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 32		 TO SHADE-COL.
	     MOVE 19		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY S04.
	     PERFORM SCREEN-SHADOW.
	     MOVE SPACE 	 TO W10-USE-PACKS.

       BB05.
	     ACCEPT W10-USE-PACKS AT 1148
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING W10-USE-PACKS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W10-USE-PACKS = "Y" OR "N")
	       CALL X"E5"
	       GO TO BB05.
	     PERFORM RESTORE-SCREEN-3.
	   IF W10-USE-PACKS = "N"
	       PERFORM BB75 THRU BB85
	       GO TO BB999.

       BB08.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 13		 TO SHADE-COL.
	     MOVE 58		 TO SHADE-WIDTH.
	     MOVE 11		 TO SHADE-LINES.
	     DISPLAY S05.
	     PERFORM SCREEN-SHADOW.

       BB09.
	     DISPLAY "Esc" AT 2512
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "ape to cancel"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BB10.
	     ACCEPT S06.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     PERFORM BB75 THRU BB85
		     GO TO BB990
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BB10
	       END-EVALUATE.

       BB15.
	     DISPLAY CLEAR-L25.
	     MOVE 20		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
	   IF WS-OPTION = "N"
	       GO TO BB25.
	     MOVE W10-LEDG	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF (WS-F-ERROR = 7) OR
	      (DPT-DESC = SPACES)
	       PERFORM BB60 THRU BB70.
	     MOVE 2		 TO WS-P1.

       BB20.
	   IF W10-UNT(WS-P1) = ZERO
	       GO TO BB22.
016780	     MULTIPLY W10-UNT(WS-P1) BY W10-COST
				 GIVING W10-CCOST.
016720	   IF W10-MKUP(WS-P1) NOT > ZERO
016730	       PERFORM BB50.
016790	     MOVE W10-SL(WS-P1)	 TO W10-CSELL.
016840	   IF W10-SL(WS-P1) = ZERO
016850	       IF (W10-CCOST NOT = ZERO) AND
		  (W10-MKUP(WS-P1) NOT = ZERO)
016860		   COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
016870			   ((W10-CCOST * W10-MKUP(WS-P1)) / 100)).
	   IF W10-CSELL = ZERO
	       MOVE W10-CCOST	 TO W10-CSELL.
016890	     MOVE W10-CSELL	 TO W10-SL(WS-P1).
016720	   IF W10-CMKUP(WS-P1) NOT > ZERO
016730	       PERFORM BB52.
016790	     MOVE W10-CSH(WS-P1) TO W10-CSELL.
016840	   IF W10-CSH(WS-P1) = ZERO
016850	       IF (W10-CCOST NOT = ZERO) AND
		  (W10-CMKUP(WS-P1) NOT = ZERO)
016860		   COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
016870			   ((W10-CCOST * W10-CMKUP(WS-P1)) / 100)).
	   IF W10-CSELL = ZERO
	       MOVE W10-CCOST	 TO W10-CSELL.
016890	     MOVE W10-CSELL	 TO W10-CSH(WS-P1).
016720	   IF W10-WMKUP(WS-P1) NOT > ZERO
016730	       PERFORM BB54.
016790	     MOVE W10-WSL(WS-P1) TO W10-CSELL.
016840	   IF W10-WSL(WS-P1) = ZERO
016850	       IF (W10-CCOST NOT = ZERO) AND
		  (W10-WMKUP(WS-P1) NOT = ZERO)
016860		   COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
016870			   ((W10-CCOST * W10-WMKUP(WS-P1)) / 100)).
	   IF W10-CSELL = ZERO
	       MOVE W10-CCOST	 TO W10-CSELL.
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
	       MOVE W05-VAT (W10-TAX)
				 TO W05-RATE
	       COMPUTE W10-VSL(WS-P1) ROUNDED = W10-SL(WS-P1) +
				 (W10-SL(WS-P1) * W05-RTE)
	       COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) +
				 (W10-WSL(WS-P1) * W05-RTE)
	       COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) +
				 (W10-CSH(WS-P1) * W05-RTE)
           ELSE
	       MOVE W10-WSL(WS-P1)
				 TO W10-VWSL(WS-P1)
	       MOVE W10-CSH(WS-P1)
				 TO W10-VCSH(WS-P1)
	       MOVE W10-SL(WS-P1)
				 TO W10-VSL(WS-P1).
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
	       MOVE W05-VAT (W10-TAX)
				 TO W05-RATE.
	     COMPUTE W10-VCSH(WS-P1) ROUNDED
		   = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE).
	     COMPUTE W10-VWSL(WS-P1) ROUNDED
		   = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE).
	     COMPUTE W10-VSL(WS-P1) ROUNDED
		   = W10-SL(WS-P1) + (W10-SL(WS-P1) * W05-RTE).

       BB22.
	   IF WS-P1 < 4
	       ADD 1		 TO WS-P1
	       GO TO BB20.
	     DISPLAY S06.
	     MOVE 20		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
	   IF NOT (WS-OPTION = "N")
	       GO TO BB990.

       BB25.
	     MOVE "Re-key: 'A'll,pack '1','2','3' [A]"
				 TO WS-ERR-MES.
	     MOVE "A"		 TO WS-OPTION.
	     MOVE 20		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
020250	   IF NOT (WS-OPTION = "A" OR "1" OR "2" OR "3")
	       GO TO BB25.
	   IF WS-OPTION = "A"
	       PERFORM RESTORE-SCREEN-3
	       GO TO BB00.
	     DISPLAY "Esc" AT 2512
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "ape to cancel"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	   IF WS-OPTION = "2"
	       GO TO BB35
	   ELSE
	   IF WS-OPTION = "3"
	       GO TO BB40.
      *
      *    ****    P A C K   1
      *
       BB30.
	     ACCEPT S06-P1.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     PERFORM BB75 THRU BB85
		     GO TO BB990
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BB30
	       END-EVALUATE.
	     GO TO BB15.
      *
      *    ****    P A C K   2
      *
       BB35.
	     ACCEPT S06-P2.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     PERFORM BB75 THRU BB85
		     GO TO BB990
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BB35
	       END-EVALUATE.
	     GO TO BB15.
      *
      *    ****    P A C K   3
      *
       BB40.
	     ACCEPT S06-P3.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     PERFORM BB75 THRU BB85
		     GO TO BB990
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BB40
	       END-EVALUATE.
	     GO TO BB15.
      *
      *    ****  D E F A U L T   M A R K U P
      *  
017540 BB50.
017600	   IF (W10-SL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
017610	       MOVE W10-SL(WS-P1)
				 TO W10-CSELL
017630	       COMPUTE W10-MKUP(WS-P1) ROUNDED =
		    (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
017590     ELSE
017580	       MOVE DPT-MKUP(WS-P1)
				  TO W10-MKUP(WS-P1).

       BB51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *  
       BB52.
017600	   IF (W10-CSH(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
017610	       MOVE W10-CSH(WS-P1)
				 TO W10-CSELL
017630	       COMPUTE W10-CMKUP(WS-P1) ROUNDED =
		    (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
017590     ELSE
017580	       MOVE DPT-CMKUP(WS-P1)
				 TO W10-CMKUP(WS-P1).

       BB53.
017660       EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *  
       BB54.
017600	   IF (W10-WSL(WS-P1) > ZERO) AND (W10-CCOST > ZERO)
017610	       MOVE W10-WSL(WS-P1)
				 TO W10-CSELL
017630	       COMPUTE W10-WMKUP(WS-P1) ROUNDED =
		    (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100)
017590     ELSE
017580	       MOVE DPT-WMKUP(WS-P1)
				 TO W10-WMKUP(WS-P1).

017650 BB55.
017660       EXIT.
      *
      *    ****    N O	 D E P A R T M E N T:  Z E R O	 M A R K - U P S
      *
       BB60.
	     MOVE 1		 TO WS-S1.

       BB65.
	     MOVE ZERO		 TO DPT-MKUP(WS-S1)
				    DPT-CMKUP(WS-S1)
				    DPT-WMKUP(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BB65.

       BB70.
	     EXIT.
      *
      *    ****    C A N C E L	 P A C K S   I N P U T
      *
       BB75.
	     MOVE 2		 TO WS-P1.
	   IF WS-PACKS = "Y"
	       MOVE W10-EXT-ITEM TO STK-CODE
	       PERFORM READ-STOCK THRU READ-STOCK-EXIT
	       GO TO BB82.
	     MOVE "N"		 TO W10-USE-PACKS.

       BB80.
	     MOVE ZERO		 TO W10-UNT(WS-P1)
				    W10-MKUP(WS-P1)
				    W10-CMKUP(WS-P1)
				    W10-WMKUP(WS-P1)
				    W10-SL(WS-P1)
				    W10-CSH(WS-P1)
				    W10-WSL(WS-P1).
	   IF WS-P1 < 4
	       ADD 1		 TO WS-P1
	       GO TO BB80.
	     GO TO BB85.

       BB82.
	     MOVE STK-UNT(WS-P1) TO W10-UNT(WS-P1).
	     MOVE STK-PR(WS-P1)  TO W10-PR(WS-P1).
	     MOVE STK-MKUP(WS-P1)
				 TO W10-MKUP(WS-P1).
	     MOVE STK-CMKUP(WS-P1)
				 TO W10-CMKUP(WS-P1).
	     MOVE STK-WMKUP(WS-P1)
				 TO W10-WMKUP(WS-P1).
	     MOVE STK-SL(WS-P1)  TO W10-SL(WS-P1).
	     MOVE STK-CSH(WS-P1) TO W10-CSH(WS-P1).
	     MOVE STK-WSL(WS-P1) TO W10-WSL(WS-P1).
	   IF WS-P1 < 4
	       ADD 1		 TO WS-P1
	       GO TO BB82.

       BB85.
	     EXIT.

       BB90.
	     MOVE "'A'mend,'D'elete or 'E'xit  [ ]"
				 TO WS-ERR-MES.
	     MOVE "A"		 TO WS-OPTION.
	     MOVE 20		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
020250	   IF NOT (WS-OPTION = "A" OR "D" OR "E")
	       GO TO BB90.
	   IF WS-OPTION = "E"
	       GO TO BB990.
	   IF WS-OPTION = "A"
	       GO TO BB09.
	     MOVE "N"		 TO WS-PACKS.
	     PERFORM BB75 THRU BB85.

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
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
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
041820         MOVE W02-DEPART   TO WS-FILE
041830	       MOVE DPT-CODE	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX.
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
