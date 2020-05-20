      $set LINKCOUNT"640"
      ******************************************************************
      *                                                                *
      *  ******  ******** *******  **	  **  ******  **    ** ******* *
      * **    **    **	  **	** ***	 *** **    ** **    ** **      *
      * **	    **	  **	** ** * * ** **    ** **    ** **      *
      *  ******     **	  *******  **  *  ** **    ** **    ** *****   *
      *       **    **	  **	   **	  ** **    **  **  **  **      *
      * **    **    **	  **	   **	  ** **    **	****   **      *
      *  ******     **	  **	   **	  **  ******	 **    ******* *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K	M O V E M E N T   L O O K - U P S	       *
      *                                                                *
      *     VERSION 8.06 - August 2002				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPMOVE.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   JULY 1998.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1998 - 2002
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CARDEX.SL.

       COPY CREDIT.SL.

000120 COPY PARAM.SL.

       COPY PURDEX.SL.

       COPY STKDEX.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FDE.

       COPY CREDIT.FDE.

000280 COPY PARAM.FDE.

       COPY PURDEX.FDE.

       COPY STKDEX.FDE.

000300 COPY STOCK.FDE.

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
       77  WS-WH-QUANT	   PIC S9(08)V999 COMP-3.
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

      /
001230 PROCEDURE DIVISION USING LS-PARID
				LS-USER-ID
				LS0-PROGRAMS
				LS0-SECURITY
				W10-STOCK.
010230 AA000-MAIN	       SECTION.
010240 AA000-INIT.
	     PERFORM ZA000-INIT.
	     PERFORM STOCK-LOOKUP.

       AA49.
	     EXIT PROGRAM.



	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.



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
