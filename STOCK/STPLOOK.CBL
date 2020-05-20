      $set LINKCOUNT"640"
      ******************************************************************
      * 							       *
      *  ******  ******** *******  **	     ******   ******  **   **  *
      * **    **    **	  **	** **	    **	  ** **    ** **  **   *
      * **	    **	  **	** **	    **	  ** **    ** ** **    *
      *  ******     **	  *******  **	    **	  ** **    ** ****     *
      *       **    **	  **	   **	    **	  ** **    ** ** **    *
      * **    **    **	  **	   **	    **	  ** **    ** **  **   *
      *  ******     **	  **	   ********  ******   ******  **   **  *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      * 	   S T O C K   L O O K - U P S			       *
      *                                                                *
      *     VERSION 8.14.02 - November 2009			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Aug 2008	- New file (STKALT) - Stock alternate index    *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Stock description.	       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPLOOK.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   JULY 1998.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1998 - 2010
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

       COPY CREDIT.SL.

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

000200 COPY SPARTS.SL.

000140 COPY STKALT.SL.

000140 COPY STKMEM.SL.

000140 COPY STOCK.SL.

       COPY WSTOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CREDIT.FDE.

       COPY DEPART.FDE.

000280 COPY PARAM.FDE.

000360 COPY SPARTS.FDE.

000140 COPY STKALT.FDE.

000140 COPY STKMEM.FDE.

000300 COPY STOCK.FDE.

000280 COPY WSTOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
       77  WS-P1	   PIC	9(02)	 COMP-5.
       77  WS-S1	   PIC	9(02)	 COMP-5.
002190 77  WS-S2	   PIC	9(04)	 COMP-5.
002200 77  WS-S3           PIC  9(04)    COMP-5.
002210 77  WS-S4           PIC  9(04)    COMP-5.
002220 77  WS-S5           PIC  9(04)    COMP-5.
002230 77  WS-S6           PIC  9(04)    COMP-5.
002240 77  WS-S7           PIC  9(04)    COMP-5.
002250 77  WS-S8           PIC  9(04)    COMP-5.
002260 77  WS-S9           PIC  9(04)    COMP-5.
002270 77  WS-IXS          PIC  9(04)    COMP-5.
002280 77  WS-IXS1         PIC  9(04)    COMP-5.
002290 77  WS-IXS2         PIC  9(04)    COMP-5.
002300 77  WS-IXS3         PIC  9(04)    COMP-5.
002310 77  WS-IXS4         PIC  9(04)    COMP-5.
001860 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001860 77  WS-PARKEY	   PIC	9(06)	 COMP-5.
       77  WS-USE-CASES	   PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-PRICE	   PIC	X(01).
       77  WS-OPTION	   PIC	X(01).
       77  WS-WSTOCK	   PIC	X(01).
       77  WS-WH-QUANT	   PIC S9(08)V999 COMP-3.
       77  WS-NODEP	   PIC	X(30) VALUE
			   "NO SUCH   D E P A R T M E N T".
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
       77  TODAY-DDMMYY	   PIC	9(08)	 COMP-5.
       77  WS-USUB	   PIC	9(04)	 COMP-5.

003050 01  WS-DB-LINE.
003080	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003160	   03  WS-MID-LNE2.
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
	   03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

003480 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CREDIT.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000360 COPY SPARTS.ID.

000140 COPY STKALT.ID.

000140 COPY STKMEM.ID.

000300 COPY STOCK.ID.

000360 COPY WSTOCK.ID.

       COPY W05.VAT.

       01  W09-STOCK.
	   03  W09-S-MARKUPS.
002780	     05  W09-MARKUP	PIC S9(03)V9999  COMP-3.
002790	     05  W09-CMARKUP	PIC S9(03)V9999  COMP-3.
002790	     05  W09-WMARKUP	PIC S9(03)V9999  COMP-3.
002780	     05  W09-MARKUP-2	PIC S9(03)V9999  COMP-3.
002790	     05  W09-CMARKUP-2	PIC S9(03)V9999  COMP-3.
002790	     05  W09-WMARKUP-2	PIC S9(03)V9999  COMP-3.
002780	     05  W09-MARKUP-3	PIC S9(03)V9999  COMP-3.
002790	     05  W09-CMARKUP-3	PIC S9(03)V9999  COMP-3.
002790	     05  W09-WMARKUP-3	PIC S9(03)V9999  COMP-3.
002780	     05  W09-MARKUP-4	PIC S9(03)V9999  COMP-3.
002790	     05  W09-CMARKUP-4	PIC S9(03)V9999  COMP-3.
002790	     05  W09-WMARKUP-4	PIC S9(03)V9999  COMP-3.
	   03  W09-ST-MARKUPS REDEFINES W09-S-MARKUPS.
	       05  W09-STK-MARK OCCURS 4.
000300		   07  W09-MKUP  PIC S9(03)V9999  COMP-3.
000300		   07  W09-CMKUP PIC S9(03)V9999  COMP-3.
000300		   07  W09-WMKUP PIC S9(03)V9999  COMP-3.

       01  W10.
	   03  W10-ALT-DISC	 PIC  X(01).

       COPY W20.WS.

       01  W30.
	   03  W30-ITEM.
	       05  W30-CODE	 PIC  X(14).
	       05  FILLER	 PIC  X(04).
	   03  W30-ITM-CODE REDEFINES W30-ITEM
				 PIC  X(14)/X(03).
	   03  W30-EXT-CODE REDEFINES W30-ITEM
				 PIC  X(18).

       COPY W40.WS.
      *
      *    ****    U S E D   F O R   W O R D   L O O K U P
      *
       01  W40-DESCRIPTION.
003040	   03  W40-ADESC.
	       05  W40-ALT-LUP.
		 07  W40-ALT-CHAR PIC X(01) OCCURS 12.
	       05  FILLER      PIC  X(18).
	   03  W40-ALT-KEY.
	       05  W40-DESC-CH PIC X(01) OCCURS 12.

       COPY FUNCTION.WS.

       COPY W50.WS.

       01  W75-KEYS.
	   03  W75-P		 PIC  9(02) COMP-5.
	   03  W75-S		 PIC  9(02) COMP-5.
	   03  W75-S1		 PIC  9(02) COMP-5.
	   03  W75-S2		 PIC  9(02) COMP-5.
	   03  W75-PACK 	 PIC  9(02) COMP-5.
           03  W75-ITEMS OCCURS 18.
	     05  W75-EXT-CODE.
	       07  W75-CODE	 PIC  X(14).
	       07  W75-ITM	 PIC  X(03).
	       07  FILLER	 PIC  X(01).
	     05  W75-PRICES.
	       07  W75-PER	 PIC  X(08).
	       07  W75-SELL	 PIC  9(08)V999 COMP-3.
	       07  W75-VSELL	 PIC  9(09)V99	COMP-3.
	       07  W75-CASH	 PIC  9(08)V999 COMP-3.
	       07  W75-VCASH	 PIC  9(09)V99	COMP-3.
	       07  W75-WSALE	 PIC  9(08)V999 COMP-3.
	       07  W75-VWSALE	 PIC  9(09)V99	COMP-3.
	       07  W75-PER-2	 PIC  X(08).
	       07  W75-SELL-2	 PIC  9(08)V999 COMP-3.
	       07  W75-VSELL-2	 PIC  9(09)V99	COMP-3.
	       07  W75-CASH-2	 PIC  9(08)V999 COMP-3.
	       07  W75-VCASH-2	 PIC  9(09)V99	COMP-3.
	       07  W75-WSALE-2	 PIC  9(08)V999 COMP-3.
	       07  W75-VWSALE-2  PIC  9(09)V99	COMP-3.
	       07  W75-PER-3	 PIC  X(08).
	       07  W75-SELL-3	 PIC  9(08)V999 COMP-3.
	       07  W75-VSELL-3	 PIC  9(09)V99	COMP-3.
	       07  W75-CASH-3	 PIC  9(08)V999 COMP-3.
	       07  W75-VCASH-3	 PIC  9(09)V99	COMP-3.
	       07  W75-WSALE-3	 PIC  9(08)V999 COMP-3.
	       07  W75-VWSALE-3  PIC  9(09)V99	COMP-3.
	       07  W75-PER-4	 PIC  X(08).
	       07  W75-SELL-4	 PIC  9(08)V999 COMP-3.
	       07  W75-VSELL-4	 PIC  9(09)V99	COMP-3.
	       07  W75-CASH-4	 PIC  9(08)V999 COMP-3.
	       07  W75-VCASH-4	 PIC  9(09)V99	COMP-3.
	       07  W75-WSALE-4	 PIC  9(08)V999 COMP-3.
	       07  W75-VWSALE-4  PIC  9(09)V99	COMP-3.
	     05  W75-DSP-PRICES REDEFINES W75-PRICES.
	       07  W75-DSP-PRC OCCURS 4.
		 09  W75-PR	 PIC  X(08).
		 09  W75-SL	 PIC  9(08)V999 COMP-3.
		 09  W75-VSL	 PIC  9(09)V99	COMP-3.
		 09  W75-CSH	 PIC  9(08)V999 COMP-3.
		 09  W75-VCSH	 PIC  9(09)V99	COMP-3.
		 09  W75-WSL	 PIC  9(08)V999 COMP-3.
		 09  W75-VWSL	 PIC  9(09)V99	COMP-3.
	   03  W75-SLEDG	 PIC  X(04) OCCURS 10.

005120 01  W100-EDIT.
003970     03  W100-V2.
003980	       05  W100-S6V2   PIC  Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
	       05  W100-PRICE  PIC  X(09).
	       05  FILLER      PIC  X(01).
           03  W100-V5 REDEFINES W100-V2.
	       05  W100-QNT    PIC  Z(05)9.99.
	       05  FILLER      PIC  X(01).
	   03  W100-V6 REDEFINES W100-V2.
	       05  W100-NQNT   PIC  -----9.99.
	       05  FILLER      PIC  X(01).
	   03  W100-V7 REDEFINES W100-V2.
	       05  W100-LQNT   PIC  Z(06)9.9.
	       05  FILLER      PIC  X(01).
	   03  W100-V8 REDEFINES W100-V2.
	       05  W100-QUANT  PIC  X(09).
	       05  FILLER      PIC  X(01).
	   03  W100-Q1.
	       05  W100-S7V3   PIC Z(06)9.999-.

       01  W200-MEMO.
           03  W200-MEM-FLDS. 
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY CHAIN.LS.

       COPY W10.LNK.

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY STOCK.CRT.

004850 01  S04C.
004860	   03  LINE  2 COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
004870                           VALUE "STOCK ENQUIRY".

       COPY STKLUP.CRT.

       01  S15.
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

       01  S16.
	   02  S16-P1.
	   03  LINE 10 COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(08) USING W10-PER-2 AUTO.
	   03  LINE 11 COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZ9.999 USING W10-UNIT-2 AUTO.
	   03  LINE 12 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.99 USING W09-MARKUP-2 AUTO.
	   03  LINE 13 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.99 USING W09-CMARKUP-2 AUTO.
	   03  LINE 14 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.99 USING W09-WMARKUP-2 AUTO.
	   03  LINE 15 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.99 USING W10-SELL-2 AUTO.
	   03  LINE 16 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.99 USING W10-CASH-2 AUTO.
	   03  LINE 17 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.99 USING W10-WSALE-2 AUTO.
	   02  S16-P2.
	   03  LINE 10 COLUMN 47 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(08) USING W10-PER-3 AUTO.
	   03  LINE 11 COLUMN 47 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZ9.999 USING W10-UNIT-3 AUTO.
	   03  LINE 12 COLUMN 49 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.99 USING W09-MARKUP-3 AUTO.
	   03  LINE 13 COLUMN 49 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.99 USING W09-CMARKUP-3 AUTO.
	   03  LINE 14 COLUMN 49 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.99 USING W09-WMARKUP-3 AUTO.
	   03  LINE 15 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.99 USING W10-SELL-3 AUTO.
	   03  LINE 16 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.99 USING W10-CASH-3 AUTO.
	   03  LINE 17 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.99 USING W10-WSALE-3 AUTO.
	   02  S16-P3.
	   03  LINE 10 COLUMN 60 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(08) USING W10-PER-4 AUTO.
	   03  LINE 11 COLUMN 60 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZ9.999 USING W10-UNIT-4 AUTO.
	   03  LINE 12 COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.99 USING W09-MARKUP-4 AUTO.
	   03  LINE 13 COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.99 USING W09-CMARKUP-4 AUTO.
	   03  LINE 14 COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.99 USING W09-WMARKUP-4 AUTO.
	   03  LINE 15 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.99 USING W10-SELL-4 AUTO.
	   03  LINE 16 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.99 USING W10-CASH-4 AUTO.
	   03  LINE 17 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZ9.99 USING W10-WSALE-4 AUTO.

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / S P E C I F I C A T I O N S    
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION USING LS-PARID
				LS-USER-ID
				LS0-PROGRAMS
				LS0-SECURITY
				W10-STOCK.
010230 AA000-MAIN	       SECTION.
010240 AA000-INIT.
	     PERFORM ZA000-INIT.
	   IF W10-ETYPE = "W"
	       PERFORM STOCK-A-LOOKUP
	   ELSE
	       PERFORM STOCK-LOOKUP.

       AA49.
	   IF WS-WSTOCK = "Y"
	       CLOSE WSTOCK
	       MOVE "N" 	 TO WS-WSTOCK.
	     EXIT PROGRAM.
      *
      *    ****    D I S P L A Y   D E P A R T M E N T	 N A M E
      *
       AA400              SECTION.
       AA401.
	     MOVE W10-LEDG	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF (WS-F-ERROR = 7) OR
	      (DPT-DESC = SPACES)
	       MOVE WS-NODEP	 TO W10-DEPT
	   ELSE
	       MOVE DPT-DESC	 TO W10-DEPT.
	     DISPLAY W10-DEPT AT 0623
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5.
       AA405.
	     EXIT.

       COPY ERROR.SCR.

       COPY FUNCTION.SCR.

       COPY LOCKED.RC2.

       COPY DATE.CNV.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
008180 AC000-READ	       SECTION.

       COPY CREDIT.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

000140 COPY STKMEM.RD.

000140 COPY STKALT.RD.

       COPY STOCK.RD.

       COPY SPARTS.RD.

       COPY WSTOCK.RD.

       COPY CA000X.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

      /
      *    ****    S T O C K   L O O K U P   R O U T I N E
      *
      *            PERFORMED FROM AA50 
      *            (P R O C E S S   F U N C T I O N   K E Y S)
      *
000010 STOCK-LOOKUP    SECTION.
000030 STOCK-LUP-SCREEN.
000040       DISPLAY CLR-SCREEN. 
000050       DISPLAY S-STK.
	     DISPLAY WS-RT-HD AT 0354
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     DISPLAY "Single" AT 0368
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     MOVE 1		 TO WS-PRICE.
000060	   IF W10-ETYPE = "A"
000070         DISPLAY S-STKA
000080     ELSE
000090	   IF W10-ETYPE = "X"
000100         DISPLAY S-STKC
000110     ELSE
000120	   IF W10-ETYPE = "D"
000130	       DISPLAY S-STKD
	   ELSE

	   IF W10-ETYPE = "2"
	       DISPLAY S-STKD2.
             MOVE SPACE          TO WS-OPTION.

       STOCK-LUP-CLR-SCREEN.
000140       MOVE 0601           TO CPOS.
000150       PERFORM STOCK-CRT-EDGE UNTIL CLIN = 22.
000160	     MOVE SPACES	 TO W10-ACODE W10-DESC W10-DESC2
000170				    W10-EXT-CODE2 W10-EXT-CODE1.

000200 STOCK-LUP-DATA.
	   IF W10-ETYPE = "Z"
               NEXT SENTENCE
           ELSE
000210	   IF W10-ETYPE = "I"
000220         ACCEPT SA-STK
000230     ELSE
000240	   IF W10-ETYPE = "A"
000250         ACCEPT SA-STKA
000260     ELSE
000270	   IF W10-ETYPE = "X"
000280         ACCEPT SA-STKC
000290	   ELSE
	   IF W10-ETYPE = "D"
000300	       ACCEPT SA-STKD
	   ELSE
	       ACCEPT SA-STKD2.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
000140		     MOVE SPACES TO W10-EXT-ITEM
000150		     GO TO STOCK-LOOKUP-EXIT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO STOCK-LUP-DATA.

       STOCK-START-LOOKUP.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
000340	     DISPLAY CLEAR-L25.
000350	     MOVE 6		 TO CLIN.
000360	     MOVE 1		 TO W75-S W75-S1 WS-P1 W75-P.

000380 STOCK-LUP-TAB-LOOP.
000390	     MOVE SPACES	 TO W75-EXT-CODE (W75-S1).
000400     IF W75-S1 < 18
000410	       ADD 1		 TO W75-S1
000420         GO TO STOCK-LUP-TAB-LOOP.
000430	     MOVE ZERO		 TO W75-S1.

000450 STOCK-LUP-ENTRY-TYPE.
000460	   IF W10-ETYPE = "I" OR "Z"
000470	       IF W10-EXT-ITEM = SPACES
000480		   MOVE X"1F"	 TO W10-EXT-ITEM.
000490	   IF W10-ETYPE = "A"
000500         IF W10-ACODE = SPACES
000510		   MOVE X"1F"	 TO W10-ACODE.
000520	   IF W10-ETYPE = "X"
000530	       IF W10-EXT-CODE2 = SPACES
000540		   MOVE X"1F"	 TO W10-EXT-CODE2.
000550	   IF W10-ETYPE = "D"
000560         IF W10-DESC = SPACES
000570		   MOVE X"1F"	 TO W10-DESC.
000550	   IF W10-ETYPE = "2"
000560	       IF W10-DESC2 = SPACES
000570		   MOVE X"1F"	 TO W10-DESC2.
000580	   IF W10-ETYPE = "I" OR "Z"
000590         PERFORM CA00 THRU CA06.
000600	   IF W10-ETYPE = "A"
000610         PERFORM CA101 THRU CA105.
000620	   IF W10-ETYPE = "X"
000630         PERFORM CA201 THRU CA205.
000640	   IF W10-ETYPE = "D"
000650	       MOVE W10-DESC	 TO W50-DATA
000660	       MOVE 30		 TO W50-LENGTH
	       CALL "CBL_TOUPPER" USING W50-30
				  BY VALUE W50-LENGTH
				  RETURNING WS-STATUS
000720	       MOVE W50-30	 TO W10-DESC.
000640	   IF W10-ETYPE = "2"
000650	       MOVE W10-DESC2	 TO W50-DATA
000660	       MOVE 30		 TO W50-LENGTH
	       CALL "CBL_TOUPPER" USING W50-30
				  BY VALUE W50-LENGTH
				  RETURNING WS-STATUS
000720	       MOVE W50-30	 TO W10-DESC2.
000730	   IF W10-ETYPE = "I" OR "Z"
000740	       MOVE W10-EXT-ITEM TO STK-CODE
000750         PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT
000760     ELSE
000770	   IF W10-ETYPE = "A"
000780	       MOVE W10-ACODE	 TO STK-ACODE
	       MOVE SPACES	 TO STK-CODE
000790         PERFORM START-AT-ALT-CODE THRU READ-STOCK-EXIT
000800     ELSE
000810	   IF W10-ETYPE = "X"
000820	       MOVE W10-EXT-CODE2
				 TO STK-XREF
000830         PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT
000840     ELSE
000850	   IF W10-ETYPE = "D"
000860	       MOVE W10-DESC	 TO STK-DESC
000870	       PERFORM START-AT-STOCK-DESC THRU READ-STOCK-EXIT
000840     ELSE
000850	   IF W10-ETYPE = "2"
000860	       MOVE W10-DESC2	 TO STK-DESC2
000870	       PERFORM START-AT-STOCK-DESC2 THRU READ-STOCK-EXIT.
000880     IF WS-F-ERROR = 22
000890         GO TO STOCK-NO-RECORD.

000910 STOCK-GET-STOCK-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
               GO TO STOCK-SKIP-STOCK-READ.
000920       PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
000930     IF WS-F-ERROR = 22
000940	       SUBTRACT 1	 FROM W75-S
      *
      *    ****   U S E R   O P T I O N S
      *
000950	       PERFORM STOCK-FULL-SCREEN
	       GO TO STOCK-POSITION-CURSOR.
      *
      *    ****   S T O R E   S T O C K   R E C O R D	P R I C E S
      *
       STOCK-SKIP-STOCK-READ.
000960	     MOVE STK-CODE	 TO W75-EXT-CODE (W75-S).
	     MOVE 1		 TO W75-PACK.

       STOCK-PRICES-LOOP.
	     MOVE STK-PR(W75-PACK)
				 TO W75-PR(W75-S, W75-PACK).
	     MOVE STK-SL(W75-PACK)
				 TO W75-SL(W75-S, W75-PACK).
	     MOVE STK-CSH(W75-PACK)
				 TO W75-CSH(W75-S, W75-PACK).
	     MOVE STK-WSL(W75-PACK)
				 TO W75-WSL(W75-S, W75-PACK).
	   IF STK-TAX NOT = ZERO
	       MOVE W05-VAT (STK-TAX)
			     TO W05-RATE
	       COMPUTE W75-VSL(W75-S, W75-PACK)	ROUNDED
		     = W75-SL(W75-S, W75-PACK) +
		      (W75-SL(W75-S, W75-PACK) * W05-RTE)
	       COMPUTE W75-VCSH(W75-S, W75-PACK) ROUNDED
		     = W75-CSH(W75-S, W75-PACK)	+
		      (W75-CSH(W75-S, W75-PACK) * W05-RTE)
	       COMPUTE W75-VWSL(W75-S, W75-PACK) ROUNDED
		     = W75-WSL(W75-S, W75-PACK)	+
		      (W75-WSL(W75-S, W75-PACK) * W05-RTE)
	   ELSE
	       MOVE W75-SL(W75-S, W75-PACK)
				 TO W75-VSL(W75-S, W75-PACK)
	       MOVE W75-CSH(W75-S, W75-PACK)
				 TO W75-VCSH(W75-S, W75-PACK)
	       MOVE W75-WSL(W75-S, W75-PACK)
				 TO W75-VWSL(W75-S, W75-PACK).
      *
      *    ****    H A V E   A L L   P R I C E S   B E E N
      * 	   S T O R E D	 ?
      *
	   IF W75-PACK < 4
	       ADD 1		 TO W75-PACK
	       GO TO STOCK-PRICES-LOOP.
      *
      *    ****    U S E   D I F F E R E N T   C O L O U R S   F O R
      * 	   E A C H   S T O C K	 T Y P E
      *
       STOCK-FB-COL.
      * 					C O N T R O L E D
	   IF STK-IND = 0
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  3"
				 TO  WS-CONT
	   ELSE
      * 					P R I C E D   I T E M
	   IF STK-IND = 1
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  6"
				 TO  WS-CONT
	   ELSE
      * 					B U Y - O U T
	   IF STK-IND = 2
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  7"
				 TO  WS-CONT
	   ELSE
      * 					H E A D I N G
	   IF STK-IND = 4
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  2"
				 TO  WS-CONT
	   ELSE
      * 					L A B O U R
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  5"
				 TO  WS-CONT.

       STOCK-DISP-DET.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-CODE	 TO W30-ITM-CODE
	   ELSE
	   IF WS-EXT-STK = "Y"
	       MOVE STK-CODE	 TO W30-EXT-CODE
	   ELSE
	       MOVE STK-ITEM	 TO W30-ITEM.
000970	     DISPLAY (CLIN, 2) STK-CODE WITH CONTROL WS-CONT.
             MOVE STK-DESC       TO W10-DESC.
000980	     DISPLAY ( , 21) W10-SDSC WITH CONTROL WS-CONT.
	   IF STK-QUANT < ZERO
000990	       MOVE STK-QUANT  TO W100-NQNT
	   ELSE
	   IF STK-QUANT > 999999.99
	       MOVE STK-QUANT  TO W100-LQNT
	   ELSE
	       MOVE STK-QUANT  TO W100-QNT.
	   IF NOT (STK-IND = 4)
001000	       DISPLAY ( , 51) W100-QUANT WITH CONTROL WS-CONT
001010	       MOVE W75-SL(W75-S, W75-P)
				 TO W100-S6V2
001020	       DISPLAY ( , 61) W100-PRICE WITH CONTROL WS-CONT
001030	       MOVE W75-VSL(W75-S, W75-P)
				 TO W100-S6V2
001040	       DISPLAY ( , 71) W100-PRICE WITH CONTROL WS-CONT.
001050     IF CLIN < 21
001060	       ADD 1		 TO CLIN W75-S
001070         GO TO STOCK-GET-STOCK-REC.
      *
      *    ****   U S E R   O P T I O N S
      *
001090 STOCK-FULL-SCREEN.
001020	     DISPLAY "" AT 2302 WITH FOREGROUND-COLOR 14
		     X"1B1A" WITH FOREGROUND-COLOR 14
001030		     "," WITH FOREGROUND-COLOR 7
		     "PgUp" WITH FOREGROUND-COLOR 14
		     "," WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 14
		     "," WITH FOREGROUND-COLOR 7
001040		     "ENTER" WITH FOREGROUND-COLOR 14
001050		     "," WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 14
		     "," WITH FOREGROUND-COLOR 7
		     "F2" WITH FOREGROUND-COLOR 14
		     "=View," WITH FOREGROUND-COLOR 7
		     "F5" WITH FOREGROUND-COLOR 14
		     "=Packs," WITH FOREGROUND-COLOR 7
		     "F6" WITH FOREGROUND-COLOR 14
		     "=Dsc2," WITH FOREGROUND-COLOR 7
		     "F7" WITH FOREGROUND-COLOR 14
		     "=Bin," WITH FOREGROUND-COLOR 7
		     "F8" WITH FOREGROUND-COLOR 14
		     "=Xref/alt,"
		      WITH FOREGROUND-COLOR 7
		     "F9" WITH FOREGROUND-COLOR 14
		     "=remks"
		      WITH FOREGROUND-COLOR 7.
001170	     DISPLAY "1" AT 2502
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     WS-RT-HD WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "," WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     WS-CS-HD WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "," WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     WS-WS-HD WITH BACKGROUND-COLOR 3
				   FOREGROUND-COLOR 1.

       STOCK-POSITION-CURSOR.
	     PERFORM SAVE-SCREEN-2.
001220	     MOVE 0		 TO W75-S1.
001230       MOVE 0601           TO CPOS.

001250 STOCK-CRT-ATTRIB.
	     MOVE 1920		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
001290	     MOVE 2000		 TO W41-LENGTH.

001310 STOCK-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY SPACE AT 2540
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
001320		     GO TO STOCK-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO STOCK-CHK-NO-KEYS
		 WHEN LEFT-KEY
		     IF W75-P > 1
			 SUBTRACT 1  FROM W75-P
			 GO TO STOCK-CHK-PACKS
		 WHEN RIGHT-KEY
		     IF W75-P < 4
			 ADD 1	     TO W75-P
			 GO TO STOCK-CHK-PACKS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO STOCK-CHK-SELECT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO STOCK-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO STOCK-LOOKUP-EXIT
		 WHEN PAGE-DOWN
001340		     GO TO STOCK-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO STOCK-GET-PREV-PAGE
		 WHEN F2-KEY
		     PERFORM BA000
001510		     GO TO STOCK-GET-USER-OPT
		 WHEN F5-KEY
		     PERFORM BB000
001510		     GO TO STOCK-GET-USER-OPT
		 WHEN F6-KEY
		     MOVE WS1-CURS
				 TO CRT-LINE
		     PERFORM STOCK-DESC2 THRU STOCK-DESC2-EXIT
		     MOVE CRT-LINE
				 TO WS1-CURS
001510		     GO TO STOCK-GET-USER-OPT
		 WHEN F7-KEY
		     MOVE WS1-CURS
				 TO CRT-LINE
		     PERFORM STOCK-BIN THRU STOCK-BIN-EXIT
		     MOVE CRT-LINE
				 TO WS1-CURS
001510		     GO TO STOCK-GET-USER-OPT
		 WHEN F8-KEY
		     MOVE WS1-CURS
				 TO CRT-LINE
		     PERFORM STOCK-CODES THRU STOCK-CODES-EXIT
		     MOVE CRT-LINE
				 TO WS1-CURS
001510		     GO TO STOCK-GET-USER-OPT
		 WHEN F9-KEY
		     PERFORM STOCK-ADD-SPECS THRU STOCK-ADD-EXIT
      *
      *    ****   U S E R   O P T I O N S
      *
		     PERFORM STOCK-FULL-SCREEN
001510		     GO TO STOCK-GET-USER-OPT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO STOCK-GET-USER-OPT
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       GO TO STOCK-CHK-PRICES.
	     CALL X"E5".
001280	     GO TO STOCK-GET-USER-OPT.

       STOCK-CHK-PACKS.
	   IF W75-P = 1
	       DISPLAY "Single" AT 0368
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
	   ELSE
	   IF W75-P = 2
	       DISPLAY "Pack 1" AT 0368
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
	   ELSE
	   IF W75-P = 3
	       DISPLAY "Pack 2" AT 0368
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
	   ELSE
	   IF W75-P = 4
	       DISPLAY "Pack 3" AT 0368
			WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     GO TO STOCK-DISP-PRICES.

       STOCK-CHK-PRICES.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
001500	   IF NOT (WS-OPTION = "1" OR "2" OR "3")
001510         GO TO STOCK-GET-USER-OPT.
	     MOVE WS-OPTION	 TO WS-PRICE.

       STOCK-DISP-PRICES.
	   IF WS-PRICE = "2"
               GO TO STOCK-CASH-PRICES.
	   IF WS-PRICE = "1"
               GO TO STOCK-RETAIL-PRICES.
	   IF WS-PRICE = "3"
               GO TO STOCK-WSALE-PRICES.
	     CALL X"E5".
001280	     GO TO STOCK-GET-USER-OPT.

       STOCK-GET-NEXT-PAGE.
001550     IF W75-S < 16
001560         GO TO STOCK-GET-USER-OPT.

       STOCK-NEW-PAGE.
001570       MOVE 0601           TO CPOS.
001580       PERFORM STOCK-CRT-EDGE UNTIL CLIN = 22.
001590       MOVE 6              TO CLIN.
001600       MOVE LOW-VALUES TO W75-KEYS.
001610	     MOVE 1		 TO W75-S W75-S1 W75-P.
001620       PERFORM STOCK-LUP-TAB-LOOP.
	     DISPLAY WS-RT-HD AT 0354 WITH FOREGROUND-COLOR 11.
001630       GO TO STOCK-GET-STOCK-REC.

       STOCK-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       STOCK-GET-PREV-LOOP.
             PERFORM READ-STOCK-PREV THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
	       MOVE SPACE	 TO WS-OPTION
	       PERFORM STOCK-LUP-SCREEN THRU STOCK-LUP-CLR-SCREEN
	       GO TO STOCK-START-LOOKUP.
	   IF WS-IXS1 < 31
               ADD 1             TO WS-IXS1
               GO TO STOCK-GET-PREV-LOOP.
             GO TO STOCK-NEW-PAGE.

       STOCK-CASH-PRICES.
	     DISPLAY WS-CS-HD AT 0354 WITH FOREGROUND-COLOR 11.
             MOVE 1              TO W75-S2.
             MOVE 06             TO LIN.

       STOCK-CASH-LOOP.
	   IF NOT (W75-CSH(W75-S2, W75-P) = ZERO)
001010	       MOVE W75-CSH(W75-S2, W75-P)
				 TO W100-S6V2
001020	       DISPLAY (LIN, 61) W100-PRICE WITH FOREGROUND-COLOR 11
001010	       MOVE W75-VCSH(W75-S2, W75-P)
				 TO W100-S6V2
001040	       DISPLAY ( , 71) W100-PRICE WITH FOREGROUND-COLOR 11
	   ELSE
	   IF W75-P > 1
	       DISPLAY (LIN, 61) "   N/A   " WITH FOREGROUND-COLOR 11
	       DISPLAY ( , 71) "         " WITH FOREGROUND-COLOR 11
	   ELSE
	       DISPLAY (LIN, 61) "         " WITH FOREGROUND-COLOR 11.
           IF W75-S2 < W75-S
               ADD 1             TO W75-S2 LIN
               GO TO STOCK-CASH-LOOP.
             GO TO STOCK-SAVE-FULL-SCREEN.

       STOCK-RETAIL-PRICES.
	     DISPLAY WS-RT-HD AT 0354 WITH FOREGROUND-COLOR 11.
             MOVE 1              TO W75-S2.
             MOVE 06             TO LIN.

       STOCK-RETAIL-LOOP.
	   IF NOT (W75-SL(W75-S2, W75-P) = ZERO)
001010	       MOVE W75-SL(W75-S2, W75-P)
				 TO W100-S6V2
001020	       DISPLAY (LIN, 61) W100-PRICE WITH FOREGROUND-COLOR 11
001010	       MOVE W75-VSL(W75-S2, W75-P)
				 TO W100-S6V2
001040	       DISPLAY ( , 71) W100-PRICE WITH FOREGROUND-COLOR 11
	   ELSE
	   IF W75-P > 1
	       DISPLAY (LIN, 61) "   N/A   " WITH FOREGROUND-COLOR 11
	       DISPLAY ( , 71) "         " WITH FOREGROUND-COLOR 11
	   ELSE
	       DISPLAY (LIN, 61) "         " WITH FOREGROUND-COLOR 11.
           IF W75-S2 < W75-S
               ADD 1             TO W75-S2 LIN
               GO TO STOCK-RETAIL-LOOP.
             GO TO STOCK-SAVE-FULL-SCREEN.

       STOCK-WSALE-PRICES.
	     DISPLAY WS-WS-HD AT 0354 WITH FOREGROUND-COLOR 11.
             MOVE 1              TO W75-S2.
             MOVE 06             TO LIN.

       STOCK-WSALE-LOOP.
	   IF NOT (W75-WSL(W75-S2, W75-P) = ZERO)
001010	       MOVE W75-WSL(W75-S2, W75-P)
				 TO W100-S6V2
001020	       DISPLAY (LIN, 61) W100-PRICE WITH FOREGROUND-COLOR 11
001010	       MOVE W75-VWSL(W75-S2, W75-P)
				 TO W100-S6V2
001040	       DISPLAY ( , 71) W100-PRICE WITH FOREGROUND-COLOR 11
	   ELSE
	   IF W75-P > 1
	       DISPLAY (LIN, 61) "   N/A   " WITH FOREGROUND-COLOR 11
	       DISPLAY ( , 71) "         " WITH FOREGROUND-COLOR 11
	   ELSE
	       DISPLAY (LIN, 61) "         " WITH FOREGROUND-COLOR 11.
           IF W75-S2 < W75-S
               ADD 1             TO W75-S2 LIN
               GO TO STOCK-WSALE-LOOP.
             GO TO STOCK-SAVE-FULL-SCREEN.

       STOCK-SAVE-FULL-SCREEN.
	     DISPLAY " " AT 2572 WITH BACKGROUND-COLOR 3
				      FOREGROUND-COLOR 1.
001990       PERFORM STOCK-CRT-ATTRIB.
001180       MOVE 0 TO W44-FUNCTION.
001190	     CALL X"B7" USING W44-FUNCTION W41-SAVE W43-SCREEN2.
           IF W75-S1 > 0
               GO TO STOCK-HIGHLIGHT.
             GO TO STOCK-GET-USER-OPT.

001650 STOCK-NO-RECORD.
001660	     DISPLAY "No record - " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001670		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001680		     "ext or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001690		     "Q" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001700		     "uit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

001720 STOCK-GET-USER-OPT-2.
001730	     ACCEPT WS-OPTION AT 2538
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
001790	   IF NOT (WS-OPTION = "N" OR "Q")
001800         GO TO STOCK-GET-USER-OPT-2.
001810     IF WS-OPTION = "N"
001820         GO TO STOCK-LUP-SCREEN.
001830	     MOVE SPACES	 TO W10-EXT-ITEM.
001840	     GO TO STOCK-LOOKUP-EXIT.

001860 STOCK-ADJ-NO-KEYS.
001870     IF W75-S1 > 1
001880         SUBTRACT 1        FROM W75-S1 CLIN.
001890       GO TO STOCK-HIGHLIGHT.

001910 STOCK-CHK-NO-KEYS.
001920     IF W75-S1 = ZERO
001930         MOVE 1            TO W75-S1
001940         GO TO STOCK-HIGHLIGHT.
001950     IF W75-S1 < W75-S
001960         ADD 1             TO W75-S1 CLIN.

001980 STOCK-HIGHLIGHT.
001990       PERFORM STOCK-CRT-ATTRIB.
002000       DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
002010       GO TO STOCK-GET-USER-OPT.

002030 STOCK-CHK-SELECT.
002040     IF W75-S1 = ZERO
002050         GO TO STOCK-GET-USER-OPT.
002060	     MOVE CSTART	 TO WS1-CURSOR.
002070	     MOVE W75-EXT-CODE(W75-S1)
				 TO W10-EXT-ITEM STK-CODE.
002080       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
      *
      *    ****   To avoid non stock items labour and free format items
      * 	  having the last item on screens description when the
      * 	  lookup program returns control to the calling program
      * 	  the stock description is moved to the W10- Link area.
      *
	     MOVE STK-DESC	 TO W10-DESC.
002090	     MOVE ZERO		 TO COL-AND-LINE.
002100	     GO TO STOCK-LOOKUP-EXIT.

       STOCK-ADD-SPECS.
002040     IF W75-S1 = ZERO
002050         GO TO STOCK-ADD-EXIT.
	     INITIALIZE SME-RECORD1.
002070	     MOVE W75-EXT-CODE(W75-S1)
				 TO SME-CODE W10-EXT-ITEM.
             MOVE 1              TO SME-SEQ.
             PERFORM READ-STKMEM THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
	       DISPLAY "No add. specifications" AT 2545
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
               GO TO STOCK-ADD-EXIT.
             INITIALIZE W200-MEM-FLDS.

       STOCK-ADD-LOOP.
             MOVE SME-SEQ        TO WS-S1.
             MOVE SME-DATA       TO W200-MEM (WS-S1).
             PERFORM READ-STKMEM-NEXT THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR = ZERO
	       IF SME-CODE = W10-EXT-ITEM
                   GO TO STOCK-ADD-LOOP.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY SA-MEM.
021280	     DISPLAY "ENTER to continue" AT 2555
                      WITH FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN-3.

       STOCK-ADD-EXIT.
             EXIT.

       STOCK-CODES.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 1		 TO W75-S2.
	     MOVE 06		 TO CLIN LIN.
	     MOVE 51		 TO CCOL
	     DISPLAY S-CODES.
000150	     PERFORM STOCK-CDE-EDGE UNTIL CLIN = 22.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
000340	     DISPLAY CLEAR-L25.
      *
      *    ****    D I S P L A Y   X - R E F   A N D   A L T   C O D E S
      *
       STOCK-CDE-DISP.
002070	     MOVE W75-EXT-CODE(W75-S2)
				 TO STK-CODE.
002080       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     PERFORM STOCK-FB-COL.
	   IF NOT STK-IND = 4
	       DISPLAY (LIN, 51) STK-XREF WITH CONTROL WS-CONT
	       DISPLAY ( , 70) STK-ACODE WITH CONTROL WS-CONT.
	   IF W75-S2 < W75-S
               ADD 1             TO W75-S2 LIN
	       GO TO STOCK-CDE-DISP.
021280       DISPLAY "ENTER to continue" AT 2355
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN-3.

       STOCK-CODES-EXIT.
             EXIT.
      *
      *    ****    D I S P L A Y   D E S C R I P T I O N   2
      *
       STOCK-DESC2.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 1		 TO W75-S2.
	     MOVE 06		 TO LIN.
	     DISPLAY "Description-2" AT 0421 WITH REVERSE-VIDEO.
000340	     DISPLAY CLEAR-L25.

       STOCK-DSC-DISP.
002070	     MOVE W75-EXT-CODE(W75-S2)
				 TO STK-CODE.
002080       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     PERFORM STOCK-FB-COL.
	   IF STK-DESC2 = SPACES
	       MOVE STK-DESC	 TO W10-DESC
	   ELSE
	       MOVE STK-DESC2	 TO W10-DESC.
	   IF NOT STK-IND = 4
	       DISPLAY (LIN, 21) W10-SDSC WITH CONTROL WS-CONT.
	   IF W75-S2 < W75-S
               ADD 1             TO W75-S2 LIN
	       GO TO STOCK-DSC-DISP.
021280	     DISPLAY "ENTER to continue" AT 2555
                      WITH FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN-3.

       STOCK-DESC2-EXIT.
             EXIT.

002120 STOCK-CDE-EDGE.
002130	     DISPLAY "                  ³          " AT CPOS
		      WITH FOREGROUND-COLOR 3.
002140	     ADD 1		 TO CLIN.

       STOCK-BIN.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 1		 TO W75-S2.
	     MOVE 06		 TO CLIN LIN.
	     MOVE 51		 TO CCOL
	     DISPLAY S-BIN.
000150	     PERFORM STOCK-BIN-EDGE UNTIL CLIN = 22.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
000340	     DISPLAY CLEAR-L25.

       STOCK-BIN-DISP.
002070	     MOVE W75-EXT-CODE(W75-S2)
				 TO STK-CODE.
002080       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     PERFORM STOCK-FB-COL.
	   IF NOT STK-IND = 4
	       DISPLAY (LIN, 53) STK-BIN WITH CONTROL WS-CONT.
	   IF W75-S2 < W75-S
               ADD 1             TO W75-S2 LIN
	       GO TO STOCK-BIN-DISP.
021280       DISPLAY "ENTER to continue" AT 2355
                      WITH FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN-3.

       STOCK-BIN-EXIT.
             EXIT.

002120 STOCK-BIN-EDGE.
002130	     DISPLAY "         " AT CPOS WITH FOREGROUND-COLOR 3.
002140	     ADD 1		 TO CLIN.

002120 STOCK-CRT-EDGE.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     "                  ³                             ³
      -		     "        ³         ³         "
		      WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
002140	     ADD 1		 TO CLIN.

002160 STOCK-LOOKUP-EXIT.
002170       EXIT.
      /    *************************************************************
      *    ****    S T O C K   L O O K U P   -	 S T K A L T	    ****
      *    *************************************************************
      * 							       *
      * 	   PERFORMED FROM AA50				       *
      * 	   (P R O C E S S   F U N C T I O N   K E Y S)	       *
      * 							       *
      *  Aug 2008	- New file (STKALT) - Stock alternate index    *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Stock description.	       *
      * 							       *
      * Nov 2009	- Include words from Description 2 and from    *
      * 		  the Item code (Some item codes are comprised *
      * 		  of individual words and these will be        *
      * 		  included in the alternate Index)	       *
      * 							       *
      ******************************************************************
000010 STOCK-A-LOOKUP		SECTION.

       STOCK-A-SAVE-LIN.
	     MOVE LIN		 TO SLIN.

000030 STCKA-LUP-SCREEN.
000040       DISPLAY CLR-SCREEN. 
000050	     DISPLAY S-STK.
	     DISPLAY "STOCK LOOKUP" AT 0230
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     " (ANY WORD)" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     DISPLAY WS-RT-HD AT 0354
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     DISPLAY "Single" AT 0368
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
008890	     DISPLAY "Find Word or Part :" AT 0302.
	     MOVE 1		 TO WS-PRICE.
	     MOVE SPACE 	 TO WS-OPTION.

       STCKA-LUP-CLR-SCREEN.
000060       MOVE 0601           TO CPOS.
000080	     PERFORM STOCK-CRT-EDGE UNTIL CLIN = 22.
000160	     MOVE SPACES	 TO W10-ACODE W10-DESC W10-DESC2
000170				    W10-EXT-CODE2 W10-EXT-CODE1.

000110 STCKA-LUP-DATA.
000120	     ACCEPT SA-STKD.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
000140		     MOVE SPACES TO W10-EXT-ITEM
000150		     GO TO STOCK-A-LOOKUP-EXIT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO STCKA-LUP-DATA.

       STCKA-START-LOOKUP.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
000170	     DISPLAY CLEAR-L25.
000180       MOVE 6              TO CLIN.
000200	     MOVE 1		 TO W75-S W75-S1 WS-P1 W75-P.

       STCKA-LUP-TAB-LOOP.
	     MOVE SPACES	 TO W75-EXT-CODE(W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
	       GO TO STCKA-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       STCKA-LUP-ENTRY-TYPE.
      *
      *    ****    D E S C R I P T I O N   S E Q U E N C E
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If no description has been entered, inform the User that  ³
      *    ³ to view the stock in description sequence, starting from  ³
      *    ³ the first description in the file, the F2 key lookup must ³
      *    ³ be used.						       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF W10-DESC = SPACES
	       DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
		       AT 1217 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				    BACKGROUND-COLOR 4
		       "¿" WITH FOREGROUND-COLOR 0 HIGHLIGHT
				BACKGROUND-COLOR 4
		       "³" AT 1317 WITH FOREGROUND-COLOR 4 HIGHLIGHT
					BACKGROUND-COLOR 4
		       "Use " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 4
		       "F2" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 4
		       " for all descriptions  -  "
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 4
		       "Press ANY key"
		       WITH FOREGROUND-COLOR 6 HIGHLIGHT
			    BACKGROUND-COLOR 4
		       "³" WITH FOREGROUND-COLOR 0 HIGHLIGHT
				BACKGROUND-COLOR 4
		       "À" AT 1417 WITH FOREGROUND-COLOR 4 HIGHLIGHT
					BACKGROUND-COLOR 4
		       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
			WITH FOREGROUND-COLOR 0 HIGHLIGHT
			     BACKGROUND-COLOR 4
	       MOVE 13		 TO SHADE-ROW
	       MOVE 19		 TO SHADE-COL
	       MOVE 45		 TO SHADE-WIDTH
	       MOVE 2		 TO SHADE-LINES
	       PERFORM SCREEN-SHADOW
	       PERFORM HIDE-THE-CURSOR
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
	       PERFORM DISPLAY-THE-CURSOR
	       GO TO STOCK-A-LOOKUP-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Convert the description to upper case, as all keys in the ³
      *    ³ system are stored as upper case.			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
000650	     MOVE W10-DESC	 TO W50-DATA.
000660	     MOVE 30		 TO W50-LENGTH.
	     CALL "CBL_TOUPPER" USING W50-30
				  BY VALUE W50-LENGTH
				  RETURNING WS-STATUS.
000720	     MOVE W50-30	 TO W10-DESC.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W10-DESC	 TO W40-ADESC.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ WS-SW4 - Will be equal to the number of characters that   ³
      *    ³ have been keyed in by the User.			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *	     MOVE 1		 TO WS-S4.
      *	     PERFORM UNTIL W40-ALT-CHAR(WS-S4) = SPACE OR
      *			   WS-S4 > 12
      *		 ADD 1		 TO WS-S4.
      *      SUBTRACT 1 	 FROM WS-S4.
	     MOVE 12		 TO WS-S4.
	     PERFORM UNTIL W40-ALT-CHAR(WS-S4) NOT = SPACE OR
			   WS-S4 < 2
		 SUBTRACT 1	 FROM WS-S4.
	     MOVE W10-DESC	 TO STKA-DKEY.
	     PERFORM START-AT-STKA-DESC THRU READ-STKALT-EXIT.
	   IF WS-F-ERROR = 49
	       GO TO STCKA-NO-RECORD.

000810 STCKA-GET-STOCK-REC.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  Must the READ-STKALT-NEXT statement be skipped?      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO STCKA-SKIP-STCKA-READ.
000840	     PERFORM READ-STKALT-NEXT THRU READ-STKALT-EXIT.
	   IF WS-F-ERROR = 49
               SUBTRACT 1        FROM W75-S
      *
      *    ****   U S E R   O P T I O N S
      *
	       PERFORM STOCK-FULL-SCREEN
	       GO TO STCKA-POSITION-CURSOR.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check that the details (word) of the desc that have been  ³
      *    ³ read, match the characters that have been entered by the  ³
      *    ³ User. Number of characters that were entered are stored   ³
      *    ³ in WS-S4.						       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO WS-S3.
	     MOVE SPACES	 TO W40-ALT-KEY.
	     PERFORM UNTIL WS-S3 > WS-S4
		 MOVE STKA-DCHAR(WS-S3)
				 TO W40-DESC-CH(WS-S3)
		 ADD 1		 TO WS-S3.
	   IF W40-ALT-KEY > W40-ALT-LUP
	       SUBTRACT 1	 FROM W75-S
      *
      *    ****   U S E R   O P T I O N S
      *
	       PERFORM STOCK-FULL-SCREEN
	       GO TO STCKA-POSITION-CURSOR.

       STCKA-SKIP-STCKA-READ.
	     MOVE STKA-CODE	 TO W75-EXT-CODE(W75-S) STK-CODE.
002020	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     MOVE 1		 TO W75-PACK.

       STCKA-PRICES-LOOP.
	     MOVE STK-PR(W75-PACK)
				 TO W75-PR(W75-S, W75-PACK).
	     MOVE STK-SL(W75-PACK)
				 TO W75-SL(W75-S, W75-PACK).
	     MOVE STK-CSH(W75-PACK)
				 TO W75-CSH(W75-S, W75-PACK).
	     MOVE STK-WSL(W75-PACK)
				 TO W75-WSL(W75-S, W75-PACK).
	   IF STK-TAX NOT = ZERO
	       MOVE W05-VAT (STK-TAX)
			     TO W05-RATE
	       COMPUTE W75-VSL(W75-S, W75-PACK)	ROUNDED
		     = W75-SL(W75-S, W75-PACK) +
		      (W75-SL(W75-S, W75-PACK) * W05-RTE)
	       COMPUTE W75-VCSH(W75-S, W75-PACK) ROUNDED
		     = W75-CSH(W75-S, W75-PACK)	+
		      (W75-CSH(W75-S, W75-PACK) * W05-RTE)
	       COMPUTE W75-VWSL(W75-S, W75-PACK) ROUNDED
		     = W75-WSL(W75-S, W75-PACK)	+
		      (W75-WSL(W75-S, W75-PACK) * W05-RTE)
	   ELSE
	       MOVE W75-SL(W75-S, W75-PACK)
				 TO W75-VSL(W75-S, W75-PACK)
	       MOVE W75-CSH(W75-S, W75-PACK)
				 TO W75-VCSH(W75-S, W75-PACK)
	       MOVE W75-WSL(W75-S, W75-PACK)
				 TO W75-VWSL(W75-S, W75-PACK).
      *
      *    ****    H A V E   A L L   P R I C E S   B E E N
      * 	   S T O R E D	 ?
      *
	   IF W75-PACK < 4
	       ADD 1		 TO W75-PACK
	       GO TO STCKA-PRICES-LOOP.
      *
      *    ****    U S E   D I F F E R E N T   C O L O U R S   F O R
      * 	   E A C H   S T O C K	 T Y P E
      *
       STCKA-FB-COL.
      * 					C O N T R O L E D
	   IF STK-IND = 0
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  3"
				 TO  WS-CONT
	   ELSE
      * 					P R I C E D   I T E M
	   IF STK-IND = 1
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  6"
				 TO  WS-CONT
	   ELSE
      * 					B U Y - O U T
	   IF STK-IND = 2
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  7"
				 TO  WS-CONT
	   ELSE
      * 					H E A D I N G
	   IF STK-IND = 4
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  2"
				 TO  WS-CONT
	   ELSE
      * 					L A B O U R
	       MOVE "HIGHLIGHT FOREGROUND-COLOR  5"
				 TO  WS-CONT.

       STCKA-DISP-DET.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-CODE	 TO W30-ITM-CODE
	   ELSE
	   IF WS-EXT-STK = "Y"
	       MOVE STK-CODE	 TO W30-EXT-CODE
	   ELSE
	       MOVE STK-ITEM	 TO W30-ITEM.
000970	     DISPLAY (CLIN, 2) STK-CODE WITH CONTROL WS-CONT.
             MOVE STK-DESC       TO W10-DESC.
000980	     DISPLAY ( , 21) W10-SDSC WITH CONTROL WS-CONT.
	   IF STK-QUANT < ZERO
000990	       MOVE STK-QUANT  TO W100-NQNT
	   ELSE
	   IF STK-QUANT > 999999.99
	       MOVE STK-QUANT  TO W100-LQNT
	   ELSE
	       MOVE STK-QUANT  TO W100-QNT.
	   IF NOT (STK-IND = 4)
001000	       DISPLAY ( , 51) W100-QUANT WITH CONTROL WS-CONT
001010	       MOVE W75-SL(W75-S, W75-P)
				 TO W100-S6V2
001020	       DISPLAY ( , 61) W100-PRICE WITH CONTROL WS-CONT
001030	       MOVE W75-VSL(W75-S, W75-P)
				 TO W100-S6V2
001040	       DISPLAY ( , 71) W100-PRICE WITH CONTROL WS-CONT.
001050     IF CLIN < 21
001060	       ADD 1		 TO CLIN W75-S
001070	       GO TO STCKA-GET-STOCK-REC.
      *
      *    ****   U S E R   O P T I O N S
      *
	     PERFORM STOCK-FULL-SCREEN.

       STCKA-POSITION-CURSOR.
	     PERFORM SAVE-SCREEN-2.
001220	     MOVE 0		 TO W75-S1.
001230       MOVE 0601           TO CPOS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Restore the screen attributes.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
001250 STCKA-CRT-ATTRIB.
	     MOVE 1920		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
001290	     MOVE 2000		 TO W41-LENGTH.

001310 STCKA-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY SPACE AT 2540
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
001320		     GO TO STCKA-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO STCKA-CHK-NO-KEYS
		 WHEN LEFT-KEY
		     IF W75-P > 1
			 SUBTRACT 1  FROM W75-P
			 GO TO STCKA-CHK-PACKS
		 WHEN RIGHT-KEY
		     IF W75-P < 4
			 ADD 1	     TO W75-P
			 GO TO STCKA-CHK-PACKS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO STCKA-CHK-SELECT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO STCKA-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO STOCK-A-LOOKUP-EXIT
		 WHEN PAGE-DOWN
001340		     GO TO STCKA-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO STCKA-GET-PREV-PAGE
		 WHEN F2-KEY
		     PERFORM BA000
001510		     GO TO STCKA-GET-USER-OPT
		 WHEN F5-KEY
		     PERFORM BB000
001510		     GO TO STCKA-GET-USER-OPT
		 WHEN F6-KEY
		     MOVE WS1-CURS
				 TO CRT-LINE
		     PERFORM STOCK-DESC2 THRU STOCK-DESC2-EXIT
		     MOVE CRT-LINE
				 TO WS1-CURS
001510		     GO TO STCKA-GET-USER-OPT
		 WHEN F7-KEY
		     MOVE WS1-CURS
				 TO CRT-LINE
		     PERFORM STOCK-BIN THRU STOCK-BIN-EXIT
		     MOVE CRT-LINE
				 TO WS1-CURS
001510		     GO TO STCKA-GET-USER-OPT
		 WHEN F8-KEY
		     MOVE WS1-CURS
				 TO CRT-LINE
		     PERFORM STOCK-CODES THRU STOCK-CODES-EXIT
		     MOVE CRT-LINE
				 TO WS1-CURS
001510		     GO TO STCKA-GET-USER-OPT
		 WHEN F9-KEY
		     PERFORM STOCK-ADD-SPECS THRU STOCK-ADD-EXIT
      *
      *    ****   U S E R   O P T I O N S
      *
		     PERFORM STOCK-FULL-SCREEN
001510		     GO TO STCKA-GET-USER-OPT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO STCKA-GET-USER-OPT
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       GO TO STCKA-CHK-PRICES.
	     CALL X"E5".
001280	     GO TO STCKA-GET-USER-OPT.

       STCKA-CHK-PACKS.
	   IF W75-P = 1
	       DISPLAY "Single" AT 0368
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
	   ELSE
	   IF W75-P = 2
	       DISPLAY "Pack 1" AT 0368
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
	   ELSE
	   IF W75-P = 3
	       DISPLAY "Pack 2" AT 0368
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
	   ELSE
	   IF W75-P = 4
	       DISPLAY "Pack 3" AT 0368
			WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     GO TO STCKA-DISP-PRICES.

       STCKA-CHK-PRICES.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
001500	   IF NOT (WS-OPTION = "1" OR "2" OR "3")
001510	       GO TO STCKA-GET-USER-OPT.
	     MOVE WS-OPTION	 TO WS-PRICE.

       STCKA-DISP-PRICES.
	   IF WS-PRICE = "2"
	       GO TO STCKA-CASH-PRICES.
	   IF WS-PRICE = "1"
	       GO TO STCKA-RETAIL-PRICES.
	   IF WS-PRICE = "3"
	       GO TO STCKA-WSALE-PRICES.
	     CALL X"E5".
001280	     GO TO STCKA-GET-USER-OPT.

       STCKA-GET-NEXT-PAGE.
001550     IF W75-S < 16
001560	       GO TO STCKA-GET-USER-OPT.

       STCKA-NEW-PAGE.
001570       MOVE 0601           TO CPOS.
001580	     PERFORM STOCK-CRT-EDGE UNTIL CLIN = 22.
001590       MOVE 6              TO CLIN.
001600       MOVE LOW-VALUES TO W75-KEYS.
001610	     MOVE 1		 TO W75-S W75-S1 W75-P.
001620	     PERFORM STCKA-LUP-TAB-LOOP.
	     DISPLAY WS-RT-HD AT 0354 WITH FOREGROUND-COLOR 11.
001630	     GO TO STCKA-GET-STOCK-REC.

       STCKA-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       STCKA-GET-PREV-LOOP.
	     PERFORM READ-STKALT-PREV THRU READ-STKALT-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE SPACE	 TO WS-OPTION
	       PERFORM STCKA-LUP-SCREEN THRU STCKA-LUP-CLR-SCREEN
	       MOVE W40-ADESC	 TO STKA-DKEY
	       PERFORM START-AT-STKA-DESC THRU READ-STKALT-EXIT
	       GO TO STCKA-NEW-PAGE.
	   IF WS-IXS1 < 31
               ADD 1             TO WS-IXS1
	       GO TO STCKA-GET-PREV-LOOP.
	     GO TO STCKA-NEW-PAGE.

       STCKA-CASH-PRICES.
	     DISPLAY WS-CS-HD AT 0354 WITH FOREGROUND-COLOR 11.
             MOVE 1              TO W75-S2.
             MOVE 06             TO LIN.

       STCKA-CASH-LOOP.
	   IF NOT (W75-CSH(W75-S2, W75-P) = ZERO)
001010	       MOVE W75-CSH(W75-S2, W75-P)
				 TO W100-S6V2
001020	       DISPLAY (LIN, 61) W100-PRICE WITH FOREGROUND-COLOR 11
001010	       MOVE W75-VCSH(W75-S2, W75-P)
				 TO W100-S6V2
001040	       DISPLAY ( , 71) W100-PRICE WITH FOREGROUND-COLOR 11
	   ELSE
	   IF W75-P > 1
	       DISPLAY (LIN, 61) "   N/A   " WITH FOREGROUND-COLOR 11
	       DISPLAY ( , 71) "         " WITH FOREGROUND-COLOR 11
	   ELSE
	       DISPLAY (LIN, 61) "         " WITH FOREGROUND-COLOR 11.
           IF W75-S2 < W75-S
               ADD 1             TO W75-S2 LIN
	       GO TO STCKA-CASH-LOOP.
	     GO TO STCKA-SAVE-FULL-SCREEN.

       STCKA-RETAIL-PRICES.
	     DISPLAY WS-RT-HD AT 0354 WITH FOREGROUND-COLOR 11.
             MOVE 1              TO W75-S2.
             MOVE 06             TO LIN.

       STCKA-RETAIL-LOOP.
	   IF NOT (W75-SL(W75-S2, W75-P) = ZERO)
001010	       MOVE W75-SL(W75-S2, W75-P)
				 TO W100-S6V2
001020	       DISPLAY (LIN, 61) W100-PRICE WITH FOREGROUND-COLOR 11
001010	       MOVE W75-VSL(W75-S2, W75-P)
				 TO W100-S6V2
001040	       DISPLAY ( , 71) W100-PRICE WITH FOREGROUND-COLOR 11
	   ELSE
	   IF W75-P > 1
	       DISPLAY (LIN, 61) "   N/A   " WITH FOREGROUND-COLOR 11
	       DISPLAY ( , 71) "         " WITH FOREGROUND-COLOR 11
	   ELSE
	       DISPLAY (LIN, 61) "         " WITH FOREGROUND-COLOR 11.
           IF W75-S2 < W75-S
               ADD 1             TO W75-S2 LIN
	       GO TO STCKA-RETAIL-LOOP.
	     GO TO STCKA-SAVE-FULL-SCREEN.

       STCKA-WSALE-PRICES.
	     DISPLAY WS-WS-HD AT 0354 WITH FOREGROUND-COLOR 11.
             MOVE 1              TO W75-S2.
             MOVE 06             TO LIN.

       STCKA-WSALE-LOOP.
	   IF NOT (W75-WSL(W75-S2, W75-P) = ZERO)
001010	       MOVE W75-WSL(W75-S2, W75-P)
				 TO W100-S6V2
001020	       DISPLAY (LIN, 61) W100-PRICE WITH FOREGROUND-COLOR 11
001010	       MOVE W75-VWSL(W75-S2, W75-P)
				 TO W100-S6V2
001040	       DISPLAY ( , 71) W100-PRICE WITH FOREGROUND-COLOR 11
	   ELSE
	   IF W75-P > 1
	       DISPLAY (LIN, 61) "   N/A   " WITH FOREGROUND-COLOR 11
	       DISPLAY ( , 71) "         " WITH FOREGROUND-COLOR 11
	   ELSE
	       DISPLAY (LIN, 61) "         " WITH FOREGROUND-COLOR 11.
           IF W75-S2 < W75-S
               ADD 1             TO W75-S2 LIN
	       GO TO STCKA-WSALE-LOOP.
	     GO TO STCKA-SAVE-FULL-SCREEN.

       STCKA-SAVE-FULL-SCREEN.
	     DISPLAY " " AT 2572 WITH BACKGROUND-COLOR 3
				      FOREGROUND-COLOR 1.
001990	     PERFORM STCKA-CRT-ATTRIB.
001180       MOVE 0 TO W44-FUNCTION.
001190	     CALL X"B7" USING W44-FUNCTION W41-SAVE W43-SCREEN2.
           IF W75-S1 > 0
	       GO TO STCKA-HIGHLIGHT.
	     GO TO STCKA-GET-USER-OPT.

001650 STCKA-NO-RECORD.
001660	     DISPLAY "No record - " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001670		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001680		     "ext or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001690		     "Q" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001700		     "uit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

001720 STCKA-GET-USER-OPT-2.
001730	     ACCEPT WS-OPTION AT 2538
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
001790	   IF NOT (WS-OPTION = "N" OR "Q")
001800	       GO TO STCKA-GET-USER-OPT-2.
001810     IF WS-OPTION = "N"
001820	       GO TO STCKA-LUP-SCREEN.
001830	     MOVE SPACES	 TO W10-EXT-ITEM.
001840	     GO TO STOCK-A-LOOKUP-EXIT.

001860 STCKA-ADJ-NO-KEYS.
001870     IF W75-S1 > 1
001880         SUBTRACT 1        FROM W75-S1 CLIN.
001890	     GO TO STCKA-HIGHLIGHT.

001910 STCKA-CHK-NO-KEYS.
001920     IF W75-S1 = ZERO
001930         MOVE 1            TO W75-S1
001940	       GO TO STCKA-HIGHLIGHT.
001950     IF W75-S1 < W75-S
001960         ADD 1             TO W75-S1 CLIN.

001980 STCKA-HIGHLIGHT.
001990	     PERFORM STCKA-CRT-ATTRIB.
002000       DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
002010	     GO TO STCKA-GET-USER-OPT.

002030 STCKA-CHK-SELECT.
002040     IF W75-S1 = ZERO
002050	       GO TO STCKA-GET-USER-OPT.
002060	     MOVE CSTART	 TO WS1-CURSOR.
002070	     MOVE W75-EXT-CODE(W75-S1)
				 TO W10-EXT-ITEM STK-CODE.
002080	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
      *
      *    ****   To avoid non stock items labour and free format items
      * 	  having the last item on screens description when the
      * 	  lookup program returns control to the calling program
      * 	  the stock description is moved to the W10- Link area.
      *
	     MOVE STK-DESC	 TO W10-DESC.
002090	     MOVE ZERO		 TO COL-AND-LINE.

       STOCK-A-LOOKUP-EXIT.
002170       EXIT.
      /
      *  ****  E N Q U I R Y   U S I N G   S T O C K   C O D E
      *
021000 BA000		       SECTION 50.
021010 BA000-ENQUIRY.
002040	   IF W75-S1 = ZERO
002050	       GO TO BA999.
	     MOVE W75-EXT-CODE(W75-S1)
				 TO W10-EXT-ITEM.
	     PERFORM SAVE-SCREEN-3.
021020       DISPLAY CLR-SCREEN.
021030       DISPLAY S04C.
021040       DISPLAY S04.
	   IF WS-USE-ITM = "Y"
	       DISPLAY "/ITM" AT 0312
		       "/" AT 0332.
	   IF W10-ART11 = "Y"
	       DISPLAY "VAT - Article 11/1g" AT 0849.
021100	     PERFORM CA000.
	   IF WS-EXT-STK = "Y"
	       DISPLAY S05-EXT
	   ELSE
	   IF WS-USE-ITM = "Y"
	       DISPLAY S05-ITM
	   ELSE
021070	       DISPLAY S05.
	     MOVE ZERO		 TO WS-WH-QUANT.
	   IF LS0-WHS = 2
	       PERFORM BA30 THRU BA40.
037210	     MOVE STK-XREF	 TO W10-EXT-CODE2.
037220       MOVE STK-BIN        TO W10-BIN.
037230       MOVE STK-DESC       TO W10-DESC.
037230	     MOVE STK-DESC2	 TO W10-DESC2.
037240       MOVE STK-TAX        TO W10-TAX.
037240	     MOVE STK-EXEMPT	 TO W10-EXEMPT.
037250       MOVE STK-MARKUP     TO W10-MARKUP.
037260       MOVE STK-CMARKUP    TO W10-CMARKUP.
037270       MOVE STK-WMARKUP    TO W10-WMARKUP.
037280       MOVE STK-IND        TO W10-IND.
037290       MOVE STK-QUANT      TO W10-QUANT.
	     MOVE STK-PER	 TO W10-PER.
	   IF W10-PER = SPACES OR LOW-VALUES
	       MOVE "Each"	 TO W10-PER.
	     MOVE STK-PRC	 TO W10-PRC.
037300       MOVE STK-LEVEL      TO W10-REORD.
037310       MOVE STK-LTIME      TO W10-LTIME.
037320       MOVE STK-COST       TO W10-COST.
037330       MOVE STK-SELL       TO W10-SELL.
037340       MOVE STK-CASH       TO W10-CASH.
037350       MOVE STK-WSALE      TO W10-WSALE.
037360	     MOVE STK-LDG	 TO W10-LEDG.
037370       MOVE STK-GUAR       TO W10-GUAR.
037380       MOVE STK-ORD        TO W10-ORDER.
037390	     MOVE STK-DTE	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
	     MOVE W22-DTE3	 TO W10-ODTE.
037400	     MOVE STK-PUR	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
	     MOVE W22-DTE3	 TO W10-PURCH.
	   IF STK-IND < 2
016950	       IF STK-AVGE = ZERO
016960		   MOVE STK-COST TO STK-AVGE.
037410	     MOVE STK-AVGE	 TO W10-AVRG.
037420       MOVE STK-ACODE      TO W10-ACODE.
010310	     COMPUTE W09-WMARKUP ROUNDED =
		   ((STK-WSALE - STK-COST) * 100.000) / STK-COST.
010310	     COMPUTE W09-MARKUP ROUNDED =
		   ((STK-SELL - STK-COST) * 100.000) / STK-COST.
010330	     COMPUTE W09-CMARKUP ROUNDED =
		   ((STK-CASH - STK-COST) * 100.000) / STK-COST.
	     DISPLAY S06C.
021150	   IF STK-ACT = ZERO
021160	       MOVE SPACE	 TO W10-ACTIVE
021170     ELSE
021180	       MOVE STK-ACT	 TO W22-DTE2
	       PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
	       MOVE W22-DTE3	 TO W10-ADATE.
021220	     MOVE STK-MTDC	 TO W10-MTDC.
021230	     MOVE STK-YTDC	 TO W10-YTDC.
021240	   IF LS-OK = "N"
021250         MOVE ZERO         TO W10-COST W10-MARKUP  
021260                              W10-AVRG W10-CMARKUP 
                                    W10-MTDC W10-WMARKUP 
				    W10-YTDC
	   ELSE
	       MOVE W09-MARKUP	 TO W10-MARKUP
	       MOVE W09-CMARKUP	 TO W10-CMARKUP
	       MOVE W09-WMARKUP	 TO W10-WMARKUP.
	   IF LS0-STLEV < 2
	       DISPLAY S06A
	       DISPLAY S06C
	   ELSE
021270	       DISPLAY S06.
	   IF W10-ART11 = "Y"
	       DISPLAY W10-EXEMPT AT 0869
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	     PERFORM AA400.
           IF NOT (STK-SUPP = SPACES OR LOW-VALUES)
               MOVE STK-SUPP     TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = ZERO
		   DISPLAY "Supplier      " AT 2303
			    P-NAME AT 2318
			    WITH FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5.
	   IF NOT (WS-WH-QUANT = ZERO)
	       ADD W10-QUANT WS-WH-QUANT
				 GIVING W100-S7V3
	       DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 2054
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		       "¿" WITH FOREGROUND-COLOR 4 HIGHLIGHT
				BACKGROUND-COLOR 4
	       DISPLAY "³" AT 2154
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		       "Including all warehouse"
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 4
		       "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
				BACKGROUND-COLOR 4
	       DISPLAY "³" AT 2254
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		       "quantities "
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 4
			W100-Q1 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
		       "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
				BACKGROUND-COLOR 4
	       DISPLAY "À" AT 2354
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
			WITH FOREGROUND-COLOR 4 HIGHLIGHT
			     BACKGROUND-COLOR 4.
21280	     DISPLAY "Press ANY key to continue" AT 2550
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.

       BA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN F9-KEY
		     IF STK-IND < 2
016950			IF STK-AVGE = ZERO
016960			   MOVE STK-COST
				 TO STK-AVGE
			END-IF
		     END-IF
037410		     MOVE STK-AVGE
				 TO W10-AVRG
		     MOVE STK-COST
				 TO W10-COST
		     DISPLAY S06A-C
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05.
	     PERFORM RESTORE-SCREEN-3.
	     GO TO BA999.

       BA30.
	     MOVE W10-EXT-ITEM	 TO WST-EXT-CODE.
	     MOVE SPACES	 TO WST-ITM WST-WAR.
	     PERFORM START-AT-WH-ITEM THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO BA40.

       BA35.
	     PERFORM READ-WSTOCK-NEXT THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO BA40.
	   IF NOT (W10-EXT-ITEM = WST-EXT-CODE)
	       GO TO BA40.
	     ADD WST-QUANT	 TO WS-WH-QUANT.
	     GO TO BA35.

       BA40.
	     EXIT.

021720 BA999.
021730       EXIT.
      /
      *    ****    D I S P L A Y   P A C K   D E T A I L S
      *
021000 BB000	       SECTION 51.
021010 BB00.
002040	   IF W75-S1 = ZERO
	       DISPLAY "No Item selected - Press ANY key" AT 2541
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 4
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
	       DISPLAY "                                " AT 2541
			WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
002050	       GO TO BB999.
	     MOVE W75-EXT-CODE(W75-S1)
				 TO W10-EXT-ITEM.
	     PERFORM SAVE-SCREEN-3.
021100       PERFORM CA000.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 13		 TO SHADE-COL.
	     MOVE 58		 TO SHADE-WIDTH.
	     MOVE 11		 TO SHADE-LINES.
	     DISPLAY S15.
	     PERFORM SCREEN-SHADOW.
	     MOVE 2		 TO WS-P1.

       BB05.
	     MOVE STK-UNT(WS-P1) TO W10-UNT(WS-P1).
	     MOVE STK-PR(WS-P1)  TO W10-PR(WS-P1).
	     MOVE STK-SL(WS-P1)  TO W10-SL(WS-P1).
000150	     MOVE STK-CSH(WS-P1) TO W10-CSH(WS-P1).
000160	     MOVE STK-WSL(WS-P1) TO W10-WSL(WS-P1).
	   IF WS-P1 < 4
	       ADD 1		 TO WS-P1
	       GO TO BB05.
	     PERFORM BC000.
	     DISPLAY S16.
	     DISPLAY "Press any key to continue" AT 2550
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN-3.

       BB999.
	     EXIT.
      *
      *    ****    C A L C U L A T E   M A R K U P S
      *
       BC000	       SECTION 51.
       BC00.
	     MOVE 2		 TO WS-P1.
	     MOVE STK-COST	 TO W10-ICOST.

       BC05-MKUPS.
	   IF LS-OK = "Y"
010310	       COMPUTE W09-WMKUP(WS-P1) ROUNDED =
		   ((STK-WSL(WS-P1) - W10-ICOST) * 100.000) / W10-ICOST
010310	       COMPUTE W09-MKUP(WS-P1) ROUNDED =
		   ((STK-SL(WS-P1) - W10-ICOST) * 100.000) / W10-ICOST
010330	       COMPUTE W09-CMKUP(WS-P1) ROUNDED =
		   ((STK-CSH(WS-P1) - W10-ICOST) * 100.000) / W10-ICOST
	   ELSE
	       MOVE ZERO	 TO W09-WMKUP(WS-P1)
				    W09-MKUP(WS-P1)
				    W09-CMKUP(WS-P1).
	   IF WS-P1 < 4
	       ADD 1		 TO WS-P1
	       GO TO BC05-MKUPS.

       BC999.
	     EXIT.
      /
      *    ****  I N I T I A L I Z E   P R O G R A M
      *
052700 ZA000-INIT	       SECTION 91.
052710 ZA000-OPEN.
055960	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
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
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *	   *************************************************************
      *    *** S T O C K   ( I V E N T O R Y )	 P A R T S   F I L E ***
      *    *************************************************************
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
      *    *************************************************************
      *    ****	S T O C K   A L T E R N A T E	I N D E X   F I L E ****
      *    *************************************************************
	     MOVE "STKALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKALT.
      *    *************************************************************
      *    **** 	   S T O C K   M E M O	 F I L E	    ****
      *    *************************************************************
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
      *    *************************************************************
      *    ****      S T O C K	 ( I V E N T O R Y )   F I L E	    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
      *    *************************************************************
      *    W A R E H O U S E  -  S T O C K  ( I V E N T O R Y )  F I L E
      *    *************************************************************
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
052980	     MOVE 3		 TO WS-PARKEY.
052990       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
053550	     MOVE PAR-VAT-REG	 TO W05-VATNO.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	   IF NOT (PAR-ART11 = "Y" OR "N")
	       MOVE "N" 	 TO PAR-ART11.
	     MOVE PAR-ART11	 TO W10-ART11.
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
	   IF NOT (PAR-DESC-H1 = X"0000000000000000000000000000")
	       MOVE PAR-DESC-H1  TO WS-DESC-H1.
	   IF NOT (PAR-DESC-H2 = X"0000000000000000000000000000")
	       MOVE PAR-DESC-H2  TO WS-DESC-H2.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT (PAR-RT-HD = X"00000000000000000000")
	       MOVE PAR-RT-HD	 TO WS-RT-HD.
	     MOVE "N"		 TO WS-WSTOCK.
	     MOVE SPACES	 TO WST-EXT-CODE WST-WAR.
006850	     START WSTOCK
	       KEY >= WST-IKEY.
	   IF WS-STATUS = "47"
	       OPEN I-O WSTOCK
	       MOVE "Y" 	 TO WS-WSTOCK.

056060 ZA999.
056070       EXIT.

056090 I-O-ERRORS      SECTION	92.
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
041650	   IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT   TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
               MOVE P-NUMBER     TO WS-KEYX
041800	   ELSE
056430	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
056500	   ELSE
056510     IF WS-F-ERROR = 15
056520         MOVE WS-PARID     TO WS-FILE
056530         MOVE WS-PARKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
056620	   ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 49
041860	       MOVE W02-STKALT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STKA-KEY	 TO WS-KEYX
041840	   ELSE
041770     IF WS-F-ERROR = 52
041780	       MOVE W02-WSTOCK	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.
056740	   IF WS-STATUS = "10"
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
