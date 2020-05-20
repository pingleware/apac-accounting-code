      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	    ****     ******    *
      *   **	**     **     **    **	 **  **    **  **   **	  **   *
      *   **	       **     **    **	**    **  **	**	 **    *
      *    ******      **     *******   **    **  **    **     **      *
      * 	**     **     **	**    **  **	**   **        *
      *   **	**     **     **	 **  **    **  **   **	       *
      *    ******      **     **          ****      ****    ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K	A M E N D   E X I S T I N G   R E C O R D      *
      *                                                                *
      *     VERSION 8.15.00 - April 2011			       *
      * 							       *
      ******************************************************************
      * 							       *
      * August 2008    - New file (STKALT) - Stock alternate index     *
      * 		 included for lookups, using any word con-     *
      * 		 tained in the Stock description.	       *
      * 							       *
      * November 2009	- Include words from Description 2 and from    *
      * 		  the Item code (Some item codes are comprised *
      * 		  of individual words and these will be        *
      * 		  included in the alternate Index)	       *
      * 							       *
      *  Jan 2010 - Allow for two discounts for a Debtor on the same   *
      * 	    Sales Ledger code. Included two additional Debtor  *
      * 	    discount codes (11 and 12) each with a field to    *
      * 	    define which debtor discount code uses this as an  *
      * 	    alternate discount. The relevant Stock Item will   *
      * 	    have an indicator field to instruct the system to  *
      * 	    use the alternate discount. The alternate discount *
      * 	    is only applicable if the discount code for which  *
      * 	    this alternate discount is applicable is the code  *
      * 	    on the Debtor account record.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP002.
000030 AUTHOR.	       J.W.LEMMON.
000040 DATE-WRITTEN.   JANUARY 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2011
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

000120 COPY CONTROL.SL.

000120 COPY DEPART.SL.

       COPY LEDTRF.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

000200 COPY SPARTS.SL.

000140 COPY STKALT.SL.

       COPY STKDEX.SL.

000140 COPY STKMEM.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

       COPY LEDTRF.FDE.

000380 COPY RECOVER.GFD.

000280 COPY PARAM.FDE.

       COPY SHARED.FDE.

000360 COPY SPARTS.FDE.

000140 COPY STKALT.FDE.

       COPY STKDEX.FDE.

000140 COPY STKMEM.FDE.

000300 COPY STOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	   PIC 999V99	 COMP-3.
001710 77  WS-SUB	   PIC 9(04)	 COMP-5.
001710 77  WS-S1	   PIC 9(04)	 COMP-5.
001720 77  WS-S2	   PIC 9(04)	 COMP-5.
001730 77  WS-S3	   PIC 9(04)	 COMP-5.
001740 77  WS-S4	   PIC 9(04)	 COMP-5.
001750 77  WS-S5	   PIC 9(04)	 COMP-5.
001760 77  WS-S6	   PIC 9(04)	 COMP-5.
001770 77  WS-S7	   PIC 9(04)	 COMP-5.
001780 77  WS-S8	   PIC 9(04)	 COMP-5.
001790 77  WS-IXS	   PIC 9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC 9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC 9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC 9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC 9(04)	 COMP-5.
001860 77  WS-PARKEY	   PIC 9(06)	 COMP-5.
001870 77  WS-NETKEY	   PIC 9(06)	 COMP-5.
001890 77  WS-RECKEY	   PIC 9(06)	 COMP-5.
001900 77  WS-RECOVER	   PIC 9(06)	 COMP-5.
001900 77  WS-RECORDS	   PIC 9(06)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
001920 77  WS-KEYSTR	   PIC	9(04)	 COMP-5.
001940 77  WS-MONTHS	   PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1	   PIC	9(04)	 COMP-5.
001980 77  WS-ITM	   PIC	X(18).
       77  WS-CARDEX	   PIC	X(01).
       77  WS-ETYPE	   PIC	X(01).
002020 77  WS-OPTION	   PIC	X(01).
002020 77  WS-SKIP	   PIC	X(01).
002040 77  WS-ADJ	   PIC	X(01).
002070 77  WS-ERROR	   PIC	9(01).
002080 77  WS-IND	   PIC	9(01)	 VALUE 0.
002090 77  WS-INDB	   PIC	9(01)	 VALUE 0.
002100 77  WS-INDP	   PIC	9(01)	 VALUE 0.
002110 77  WS-INDS	   PIC	9(01)	 VALUE 0.
002120 77  WS-AMEND	   PIC	9(01)	 VALUE 0.
002140 77  WS-CONREC	   PIC	9(01)	 VALUE ZERO.
002150 77  WS-LEDG	   PIC	X(04).
002160 77  WS-LEDG1	   PIC	X(04).
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-CON-FACT	   PIC	X(01).
002200 77  WS-COMP	   PIC	9(01).
002210 77  WS-WEEK	   PIC	9(01).
002220 77  WS-TYPE	   PIC	X(01).
002260 77  WS-BATCH	   PIC	9(05).
002270 77  WS-PASSWORD	   PIC	X(08) VALUE SPACES.
       77  WS-QPASSWORD    PIC	X(06) VALUE SPACES.
       77  WS-PRINT	   PIC	9(01).
002280 77  WS-LABCDE	   PIC	X(08).
002280 77  WS-PASS	   PIC	X(08) VALUE SPACES.
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
       77  WS-NODEP	   PIC	X(30) VALUE
			   "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
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
	       05  FILLER  PIC	X(01).
002430     03  WS-TOP-LNE2.
002440	       05  WS-TCH  PIC	X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
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
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
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
	   03  WS-PROG	       PIC  X(03) VALUE "002".

       COPY LEDTRF.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

000380 COPY RECOVER.ID.

       COPY SHARED.ID.

000360 COPY SPARTS.ID.

000140 COPY STKALT.ID.

       COPY STKDEX.ID.

000140 COPY STKMEM.ID.

000300 COPY STOCK.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT PIC X(07) VALUE "STP001.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY W05.VAT.

       COPY W05.RND.

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

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
	   03  W75-SLEDG      PIC  X(04) OCCURS 10.

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
004070	       05  W80-REC    PIC ZZZZ9.
004090     03  W80-DEL.
004100	       05  W80-2NM     PIC 9(02).
      *
      *    ****   T A B L E   F O R   P R I C E   C H E C K   D I G I T
      * 	  C A L C U L A T I O N S
      *
       01  W80-CHECK-DIGIT.
	   03  W80-VALUES.
	       05  FILLER	PIC  9(09) VALUE 516273849.
	       05  FILLER	PIC  9(09) VALUE 594837261.
	       05  FILLER	PIC  9(09) VALUE 246891357.
	   03  W80-TABLE-VALUES REDEFINES W80-VALUES.
	       05  W80-5P	PIC  9(01) OCCURS 9.
	       05  W80-5N	PIC  9(01) OCCURS 9.
	       05  W80-2N	PIC  9(01) OCCURS 9.
	   03  W80-SUB		PIC  9(02) COMP-5.
	   03  W80-RESULT	PIC  9(02).
	   03  W80-RES-DIG REDEFINES W80-RESULT.
	       05  W80-R-DIG	PIC  9(01) OCCURS 2.
	   03  W80-COMP REDEFINES W80-RESULT.
	       05  W80-CD	PIC  9(01).
	       05  W80-UNIT	PIC  9(01).
	   03  W80-WEIGHTS.
	       05  W80-WGHTS	PIC  9(05).
	   03  W80-WGHT REDEFINES W80-WEIGHTS.
	       05  W80-WEIGHT	PIC  9(01) OCCURS 5.
	   03  W80-BAR-CODE.
	       05  W80-XREF.
	       07  W80-GROUP	PIC  9(02).
	       07  W80-ITM-CODE PIC  9(04).
	       05  W80-PRC-CD	PIC  9(01).
	       05  W80-PRICE	PIC  9(03)V99.
	       05  W80-PRC-DIG	REDEFINES W80-PRICE
				PIC  9(01) OCCURS 5.
	       05  W80-BAR-CD	PIC  9(01).
	   03  W80-BAR-CODE-CH REDEFINES W80-BAR-CODE
				     PIC  9(01) COMP OCCURS 13.
	   03  W80-BAR-CODE-EAN13 REDEFINES W80-BAR-CODE.
	       05  W80-BAR-CODE-12   PIC  9(12) COMP.
	       05  W80-BAR-CODE-CHK-DIG
				     PIC  9(01) COMP.
	   03  W80-BAR-CODE-RESULT-1 PIC  9(04) COMP.
	   03  W80-BAR-CODE-RESULT-2 PIC  9(04) COMP.
	   03  W80-BAR-CODE-RESULT-3 PIC  9(04) COMP.
	   03  W80-BAR-CODE-CALC REDEFINES W80-BAR-CODE-RESULT-3.
	       05  W80-BAR-CODE-TEN  PIC  9(03) COMP.
	       05  W80-BAR-CODE-UNIT PIC  9(01) COMP.

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

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

004230 01  S02.
004240     03  LINE  3 COLUMN  2 FOREGROUND-COLOR 11
004250                           VALUE "Logged In" BLINK.

004820 01  S04B.
004830	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
004840                           VALUE "STOCK AMENDMENT".

       COPY STOCK.CRT.

003880 01  S20.
	   03  LINE 11 COLUMN 20 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
		       VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003890	   03  LINE 12 COLUMN 20 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
		       VALUE "³ Enter quantity Amend Password        ³".
	   03  LINE 13 COLUMN 20 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
		       VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S20A.
003910	   03  LINE 12 COLUMN 52 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC  X(06)
				 USING WS-QPASSWORD AUTO SECURE.

       COPY DEPART.CRT.

      *
      *    ****    PRODUCTION DETAILS DISPLAY SCREEN
      *
007450 01  S32.
007460     03  LINE 5 COLUMN 21 FOREGROUND-COLOR 15 VALUE 
007470                       "Details of Manufacture (Include Labour)".
007480	   03  LINE 6 COLUMN  8 VALUE "Comments".
007500	   03  LINE 7 COLUMN  4 VALUE "Stock code".
	   03	      COLUMN 19 VALUE "ITM".
007510	   03	      COLUMN 28 VALUE "Description".
007520     03         COLUMN 66 VALUE "Quantity".

       01  S32A.
	   03 COLUMN  4 FOREGROUND-COLOR 7 HIGHLIGHT
			BACKGROUND-COLOR 5
			PIC  X(14)/X(03)
			USING W10-EXT-CODE2 AUTO.

       01  S32B.
	   03 COLUMN  4 FOREGROUND-COLOR 7 HIGHLIGHT
			BACKGROUND-COLOR 5
			PIC  X(18) USING W10-EXT-CODE2 AUTO.

       01  S32C.
	   03 COLUMN  4 FOREGROUND-COLOR 7 HIGHLIGHT
			BACKGROUND-COLOR 5
			PIC  X(14) USING W10-EXT-CODE2 AUTO.

       01  S38.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC ZZZZ9.9999 USING W10-MQNT AUTO.

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

       01  SA-FACT.
	   03  LINE 11 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZ9.999 USING W10-FACT AUTO.

004870 COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN	       SECTION.
007750 AA000-INIT.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      * 	       Check the security level of the USER.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
003240	   IF LS0-STLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
007760	     PERFORM ZA000-INIT.
007780	     PERFORM BC000.
	     CLOSE RECOVER.

       AA49.
	     EXIT PROGRAM.

       COPY AA50.LUP.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  SCREEN-SHADOW 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:						       ³
      *	   ³	       SHADE-ROW   - Top line of the box + 1.	       ³
      *	   ³	       SHADE-COL   - Left line of box + 2.	       ³
      *	   ³	       SHADE-WIDTH - Width of the box - 2.	       ³
      *	   ³	       SHADE-LINES - Hight of box - 1.		       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  CHECK-CORRECT 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³	      "Correct ENTER - N if incorrect"		       ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.			       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  ERROR-MESSAGE 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display an error message with " - Press ANY key" at    ³
      *    ³ the end; use PERFORM ERROR-MESSAGE.		       ³
      *    ³							       ³
      *    ³ To display just the error message; use PERFORM	       ³
      *    ³ ERROR-LENGTH THRU ERROR-EXIT.			       ³
      *    ³							       ³
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
      *    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
      *    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
      *    ³ THRU ERROR-EXIT.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY FUNCTION.CRT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			  OPT-MESSAGE			       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'. 			       ³
      *    ³							       ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³							       ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE.				       ³
      *    ³							       ³
      *    ³ This would be displayed as:			       ³
      *    ³	ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³	³	   Print transactions (Y/N) [ ] 	 ³°°   ³
      *    ³	ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³	  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 13) move the line number to be used to SLIN and   ³
      *    ³ then PEFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	      ERASE SCREEN FROM SPECIFIED LOCATION	       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Uses CPOS (CLIN,CCOL) as starting position and increases  ³
      *    ³ CLIN by 1 until CLIN reaches the line allocated to exit.  ³
      *    ³ The screen is cleared with Column 1 and 80 containing "³" ³
      *    ³ and columns 2 to 79 containing spaces.		       ³
      *    ³  eg.						       ³
      *    ³	  MOVE 0801	 TO CPOS.			       ³
      *    ³	  PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY CLEAR.CRT.

       COPY AA300.STK.

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
      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
       AA500              SECTION.
       AA501.
	     PERFORM SAVE-SCREEN.
	     MOVE 6		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 27		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0526
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "¿"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
023190	     DISPLAY "³" AT 0626
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     " Insufficient security     "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
023200	     DISPLAY "³" AT 0726
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     " Press ENTER to continue   "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     DISPLAY "À" AT 0826
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     PERFORM SCREEN-SHADOW.
023210	     ACCEPT WS-OPTION AT 0752
		    WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7 AUTO.
	     PERFORM RESTORE-SCREEN.

       AA505.
             EXIT.

       COPY DATE.CHK.

       COPY DATE.CNV.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
008180 AC000-READ	       SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY LEDTRF.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

       COPY STKALT.RD.

000140 COPY STKMEM.RD.

       COPY STOCK.RD.

       COPY SPARTS.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
009850 AD000-WRITE	       SECTION.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY LEDTRF.WR.

       COPY PARAM.WR.

       COPY SHARED.WR.

       COPY STKALT.WR.

       COPY STKDEX.WR.

000140 COPY STKMEM.WR.

       COPY STOCK.WR.

       COPY SPARTS.WR.

       COPY STOCK.1ST.

       COPY DEPART.LUP.

       COPY APAC.HLP.

      /
      *       ****   *****   ***    ***   *   *  *****	****   *   *
      *       *   *  *	    *	*  *   *  *   *  *	*   *  *   *
      *       ****   ***    *	   *   *  *   *  ***	****	* *
      *       *   *  *	    *	*  *   *   * *	 *	*   *	 *
      *       *   *  *****   ***    ***     *	 *****	*   *	 *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      * 	 THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED	     ÄÄ     (REWRITE)	   ³
      *        ³ 1 = RECORD WAS ADDED	     ÄÄ     (DELETE)	   ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)	   ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011430 AY000	       SECTION.
011440 AY01.
011450       MOVE 01             TO REC-FILE.
011460       MOVE WS-PARKEY      TO REC-KEY.
011470       MOVE PAR-RECORD1    TO REC-PARAM.
011480       GO TO AY50.

011490 AY05.
011500       MOVE 05             TO REC-FILE.
011510       MOVE ZERO           TO REC-KEY.
           IF WS-ACTION = 0 OR 2
               PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
	   IF WS-ACTION = 9
	       MOVE 0		 TO REC-TYPE
	   ELSE
	       MOVE WS-ACTION	 TO REC-TYPE.
011520       MOVE STK-RECORD1    TO REC-STOCKF.
011530       GO TO AY50.

011590 AY20.
011600       MOVE 20             TO REC-FILE.
011610       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011620       MOVE PRT-REC1       TO REC-SPARTS.
011630       GO TO AY50.

012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.

       AY38.
             MOVE 38             TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
	     MOVE DPT-RECORD	 TO REC-DEPART.
             GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

011690 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.

011690 AY46.
011080	     MOVE 46		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE STX-REC1	 TO REC-STKDEX.
011110       GO TO AY50.

010970 AY49S.
010980	     MOVE 49		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011000	     MOVE STKA-RECORD1	 TO REC-STKALT.
011010       GO TO AY50.

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

       COPY LEDTRF.UPD.

      /
      *    ****    B A R C O D E   C A L C U L A T I O N
      *
       BA000	       SECTION 50.
       BA100.
	     MOVE STK-XREF	 TO W80-XREF.
	     MOVE STK-CASH	 TO W80-PRICE.
	     MOVE ZERO		 TO W80-WGHTS.
	   IF W80-PRC-DIG(1) > ZERO
	       MOVE W80-PRC-DIG(1)
				 TO W80-SUB
	       MOVE W80-5P(W80-SUB)
				 TO W80-WEIGHT(1).
	   IF W80-PRC-DIG(2) > ZERO
	       MOVE W80-PRC-DIG(2)
				 TO W80-SUB
	       MOVE W80-2N(W80-SUB)
				 TO W80-WEIGHT(2).
	   IF W80-PRC-DIG(3) > ZERO
	       MOVE W80-PRC-DIG(3)
				 TO W80-SUB
	       MOVE W80-5N(W80-SUB)
				 TO W80-WEIGHT(3).
	   IF W80-PRC-DIG(4) > ZERO
	       MOVE W80-PRC-DIG(4)
				 TO W80-SUB
	       MOVE W80-5P(W80-SUB)
				 TO W80-WEIGHT(4).
	   IF W80-PRC-DIG(5) > ZERO
	       MOVE W80-PRC-DIG(5)
				 TO W80-SUB
	       MOVE W80-2N(W80-SUB)
				 TO W80-WEIGHT(5).
	     ADD W80-WEIGHT(1) W80-WEIGHT(2) W80-WEIGHT(3)
		 W80-WEIGHT(4) W80-WEIGHT(5) GIVING W80-RESULT.
	   IF W80-UNIT > ZERO
	       SUBTRACT W80-UNIT FROM 10
				 GIVING W80-SUB
	   ELSE
	       MOVE ZERO	 TO W80-SUB.
	   IF W80-SUB = ZERO
	       MOVE ZERO	 TO W80-CD
	       GO TO BA110-PRC-CD-FOUND.
	     MOVE 1		 TO W80-CD.

       BA105-FIND-PRC-CD.
	   IF W80-SUB = W80-5N(W80-CD)
	       GO TO BA110-PRC-CD-FOUND.
	     ADD 1		 TO W80-CD.
	   IF W80-CD < 9
	       GO TO BA105-FIND-PRC-CD.

       BA110-PRC-CD-FOUND.
	     MOVE W80-CD	 TO W80-PRC-CD.
      *
      *    ****    C A L C U L A T E   T H E   B A R - C O D E
      * 	   C H E C K   D I G I T
      *
       BA115-CALC-CHECK-DIGIT.
	     ADD W80-BAR-CODE-CH (1),
		 W80-BAR-CODE-CH (3),
		 W80-BAR-CODE-CH (5),
		 W80-BAR-CODE-CH (7),
		 W80-BAR-CODE-CH (9),
		 W80-BAR-CODE-CH (11),
				 GIVING W80-BAR-CODE-RESULT-1.
	     ADD W80-BAR-CODE-CH (2),
		 W80-BAR-CODE-CH (4),
		 W80-BAR-CODE-CH (6),
		 W80-BAR-CODE-CH (8),
		 W80-BAR-CODE-CH (10),
		 W80-BAR-CODE-CH (12)
				 GIVING W80-BAR-CODE-RESULT-2.
	     MULTIPLY 3 	 BY W80-BAR-CODE-RESULT-2.
	     ADD W80-BAR-CODE-RESULT-1
		 W80-BAR-CODE-RESULT-2
				 GIVING W80-BAR-CODE-RESULT-3.
	   IF W80-BAR-CODE-UNIT > ZERO
	       SUBTRACT W80-BAR-CODE-UNIT FROM 10
				 GIVING W80-BAR-CODE-CHK-DIG
	   ELSE
	       MOVE ZERO	 TO W80-BAR-CODE-CHK-DIG.
	     MOVE W80-BAR-CODE	 TO STK-XREF.

       BA199.
	     EXIT.

      /
      *    ****  A M E N D   E X S I S T I N G   S T O C K
      *
019400 BC000	       SECTION 50.
019410 BC000-AMEND.
019420       DISPLAY CLR-SCREEN.
019430       DISPLAY S04B.
019440       DISPLAY S04-N.
	     MOVE ZERO		 TO WS-INDB.
	   IF WS-USE-ITM = "Y"
	       DISPLAY "/ITM" AT 0312
		       "/" AT 0332.
019460	     MOVE SPACE 	 TO W10-EXT-ITEM.
	   IF W10-STCK = "N"
	       IF W10-SLNGTH = 3
		   MOVE ZERO	 TO W10-I3
	       ELSE
	       IF W10-SLNGTH = 4
		   MOVE ZERO	 TO W10-I4
	       ELSE
	       IF W10-SLNGTH = 5
		   MOVE ZERO	 TO W10-I5
	       ELSE
	       IF W10-SLNGTH = 6
		   MOVE ZERO	 TO W10-I6
	       ELSE
	       IF W10-SLNGTH = 7
		   MOVE ZERO	 TO W10-I7
	       ELSE
	       IF W10-SLNGTH = 8
		   MOVE ZERO	 TO W10-I8
	       ELSE
	       IF W10-SLNGTH = 9
		   MOVE ZERO	 TO W10-I9
	       ELSE
	       IF W10-SLNGTH = 10
		   MOVE ZERO	 TO W10-I10
	       ELSE
	       IF W10-SLNGTH = 11
		   MOVE ZERO	 TO W10-I11
	       ELSE
	       IF W10-SLNGTH = 12
		   MOVE ZERO	 TO W10-I12
	       ELSE
	       IF W10-SLNGTH = 13
		   MOVE ZERO	 TO W10-I13
	       ELSE
		   MOVE ZERO	 TO W10-I14.
019470 BC005.
             PERFORM AA300.
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   ACCEPT S05-EXT
	       ELSE
	       IF WS-USE-ITM = "Y"
		   ACCEPT S05-ITM
	       ELSE
021070		   ACCEPT S05
	       END-IF
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S05-3
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S05-4
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S05-5
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S05-6
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S05-7
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S05-8
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S05-9
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S05-10
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S05-11
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S05-12
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S05-13
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S05-14.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
                     GO TO BC999
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BC005
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN F9-KEY
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
                     CALL X"E5"
                     GO TO BC005
               END-EVALUATE
	       PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S05-EXT
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S05-ITM
		   ELSE
		       DISPLAY S05
		   END-IF
		   END-IF
	       ELSE
		   DISPLAY S05
	       END-IF
	       IF W10-EXT-ITEM = SPACES
                   GO TO BC005.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
019490	   IF W10-EXT-ITEM = SPACE
019500         GO TO BC999.

       BC05.
019510	     PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   DISPLAY S05-EXT
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S05-ITM
	       ELSE
018630		   DISPLAY S05
	       END-IF
	       END-IF
	   ELSE
	       DISPLAY S05.
019520	   IF WS-F-ERROR = 22
	       MOVE "Z"		 TO W10-ETYPE
018620	       PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S05-EXT
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S05-ITM
		   ELSE
		       DISPLAY S05
		   END-IF
		   END-IF
	       ELSE
		   DISPLAY S05
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO BC005
               ELSE
		   GO TO BC05.
	     PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.

019560 BC012-DET.
019570	     MOVE STK-CODE	 TO W10-EXT-ITEM.
019580	     MOVE STK-XREF	 TO W10-EXT-CODE2.
019590       MOVE STK-BIN        TO W10-BIN.
019600       MOVE STK-DESC       TO W10-DESC.
019600	     MOVE STK-DESC2	 TO W10-DESC2.
019610       MOVE STK-TAX        TO W10-TAX.
019640	     MOVE STK-IND	 TO W10-IND.
019650       MOVE STK-QUANT      TO W10-QUANT.
	     MOVE STK-PRC	 TO W10-PRC.
019660	     MOVE STK-LEVEL	 TO W10-REORD.
019670       MOVE STK-LTIME      TO W10-LTIME.
019680       MOVE STK-COST       TO W10-COST.
019710	     MOVE STK-LDG	 TO W10-LEDG.
             MOVE STK-GUAR       TO W10-GUAR.
019720       MOVE STK-ORD        TO W10-ORDER.
019730	     MOVE STK-DTE	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3       TO W10-ODTE.
037400	     MOVE STK-PUR	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
	     MOVE W22-DTE3	 TO W10-PURCH.
	   IF STK-IND < 2
016950	       IF STK-AVGE = ZERO
016960		   MOVE STK-COST TO STK-AVGE.
019800	     MOVE STK-AVGE	 TO W10-AVRG.
	     MOVE STK-ALT-DISC	 TO W10-ALT-DISC.
019810	     MOVE STK-ACODE	 TO W10-ACODE.
	     MOVE STK-USE-PACKS  TO W10-USE-PACKS.
	     MOVE STK-EXEMPT	 TO W10-EXEMPT.
	     MOVE STK-FACT	 TO W10-FACT.
      *
      *    ****    U N I T S   A N D   M A R K U P S
      *
	     MOVE 1		 TO WS-S1.
	     PERFORM BC45.
      *
      *    ****    P R I C E S	 -   I N C L U S I V E	 A N D
      * 			     E X C L U S I V E
      * 			     S E E   C A 0 0 0	 &   C A 5 0 0
      *
	     DISPLAY S06C.

019820 BC015-AMEND-DET.
019830	   IF LS-OK = "N"
019840         MOVE ZERO         TO W10-MARKUP  W10-COST 
                                    W10-CMARKUP W10-AVRG
				    W10-WMARKUP.
	     PERFORM AA400.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     " Sales Ledger Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BC16-AMEND-DET.
019850       ACCEPT S06A.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     UNLOCK STOCK
		     GO TO BC999
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BC16-AMEND-DET
		 WHEN F6-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		     GO TO BC015-AMEND-DET
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BC16-AMEND-DET
               END-EVALUATE
015990	       GO TO BC16-AMEND-DET.
019880	   IF LS-OK = "N"
019890         MOVE STK-MARKUP   TO W10-MARKUP
019900         MOVE STK-CMARKUP  TO W10-CMARKUP
019900         MOVE STK-WMARKUP  TO W10-WMARKUP
               MOVE STK-GUAR     TO W10-GUAR 
019910         MOVE STK-COST     TO W10-COST
019920         MOVE STK-SELL     TO W10-SELL
019930         MOVE STK-CASH     TO W10-CASH
019930         MOVE STK-WSALE    TO W10-WSALE
019940         MOVE STK-QUANT    TO W10-QUANT
019950	       MOVE STK-AVGE	 TO W10-AVRG.
	   IF NOT (STK-QUANT = W10-QUANT)
	       PERFORM SAVE-SCREEN-2
018770	       DISPLAY S20
	       MOVE 12		 TO SHADE-ROW
	       MOVE 22		 TO SHADE-COL
	       MOVE 38		 TO SHADE-WIDTH
	       MOVE 2		 TO SHADE-LINES
	       PERFORM SCREEN-SHADOW
018790	       ACCEPT S20A
018800	       MOVE 6		 TO WS-LENGTH
	       CALL "CBL_TOUPPER" USING WS-QPASSWORD
				 BY VALUE WS-LENGTH
				 RETURNING WS-STATUS
018860	       MOVE 1		 TO WS-LENGTH
018870	       MOVE 5		 TO WS-PARKEY
018880	       PERFORM READ-PARAM THRU READ-PARAM-EXIT
018890	       MOVE PAR-PW	 TO W85-PASS
018900	       IF NOT (WS-QPASSWORD = W85-SUPER(1) OR
018940				      W85-SUPER(7) OR
018990				     "YTITNQ")
019940		  MOVE STK-QUANT TO W10-QUANT
	       END-IF
	       PERFORM RESTORE-SCREEN-2.
             PERFORM AA400.
	     MOVE ZERO		 TO W05-RATE.
           IF W10-TAX > 0
	       MOVE W05-VAT(W10-TAX)
				 TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED 
		   = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED 
		   = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED 
		   = W10-SELL + (W10-SELL * W05-RTE).
	     PERFORM BC21D.
             DISPLAY S06C.
	     MOVE 18		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
020030	   IF WS-OPTION = "N"
020040         GO TO BC015-AMEND-DET.
020060	   IF ((W10-IND < 3) OR (W10-IND = 8 OR 9)) AND
               (W10-DESC = SPACES)
	       MOVE "Invalid description"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
020080	       GO TO BC015-AMEND-DET.
	     MOVE W10-LEDG	 TO DPT-CODE.
016040	   IF (W10-LEDG = SPACES OR "0000" OR "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT") OR
	      (WS-NODEP	= W10-DEPT)
020100	       MOVE "Invalid ledger code"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
020110	       GO TO BC015-AMEND-DET.
020120     IF W10-BIN = SPACES
020130	       MOVE "Invalid bin number"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
020140	       GO TO BC015-AMEND-DET.
	   IF NOT(W10-ALT-DISC = "Y")
	       MOVE "N" 	 TO W10-ALT-DISC.
	   IF W10-TAX > 6
	       MOVE "Invalid VAT code"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO BC015-AMEND-DET.
      *
      *    ****    TEMPORARY PATCH - ONLY 1 RATE APPLICABLE
      *
	   IF W10-TAX > 1
	       MOVE 1		 TO W10-TAX.
020220	   IF W10-ODTE = ZERO
020240         GO TO BC18.
             MOVE W10-ODTE       TO W10-EDTE.
020250       PERFORM CHECK-DATE.
020260     IF WS-ERROR = 1
020270	       MOVE "Invalid date"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
020280	       GO TO BC015-AMEND-DET.
             MOVE W22-DTE1       TO W22-DTE2.

       BC18.
020220     IF W10-PURCH = ZERO
020240         GO TO BC020-AMEND.
	     MOVE W10-PURCH	 TO W10-EDTE.
020250       PERFORM CHECK-DATE.
020260     IF WS-ERROR = 1
020270	       MOVE "Invalid purchase date"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
020280	       GO TO BC015-AMEND-DET.

020290 BC020-AMEND.
020330	     MOVE W10-EXT-ITEM	 TO WS-ITM.
	   IF W10-ART11 = "N"
	       GO TO BC20-CHECK.

       BC20-ART11.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 17		 TO SHADE-COL.
	     MOVE 49		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY S-ART11.
	     PERFORM SCREEN-SHADOW.

       BC20-EXEMPT.
	     ACCEPT W10-EXEMPT AT 1263
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING W10-EXEMPT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W10-EXEMPT = "N" OR "Y")
	       GO TO BC20-EXEMPT.
	     PERFORM RESTORE-SCREEN-3.

017250 BC20-CHECK.
019880	   IF LS-OK = "N"
	       GO TO BC20A.
	   IF W10-TAX = ZERO
	       MOVE "'C'ontinue or 'R'ekey  [C]"
				 TO WS-ERR-MES
	   ELSE
015910	       MOVE "'C'ontinue, amend 'I'nclusive or 'R'ekey  [C]"
				 TO WS-ERR-MES.
             MOVE "C"            TO WS-OPTION.
	     MOVE 16		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
015980	   IF NOT (WS-OPTION = "C" OR "I" OR "R")
	       GO TO BC20-CHECK.
017890	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
           IF WS-OPTION = "R"
015990	       GO TO BC015-AMEND-DET.
	   IF NOT (W10-TAX = ZERO) AND (WS-OPTION = "I")
	       ACCEPT S06C
	       COMPUTE W10-CASH
		   = (W10-VCASH / (100 + W05-RATE)) * 100
	       COMPUTE W10-WSALE ROUNDED
		   = (W10-VWSALE / (100 + W05-RATE)) * 100
	       COMPUTE W10-SELL ROUNDED
		   = (W10-VSELL / (100 + W05-RATE)) * 100
	       DISPLAY S06E
	       GO TO BC20-CHECK.

020420 BC20A.
020430	     PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
020440       PERFORM AY05 THRU AY59.
020450	     MOVE ZERO		 TO W25-TOTAL W25-VALUE.
020460	   IF STK-IND = 0 OR 1
020470         MULTIPLY STK-QUANT BY STK-AVGE 
                                 GIVING W25-VALUE.
020480     IF W10-IND = 0 OR 8 OR 9
020490         MULTIPLY W10-QUANT BY W10-AVRG 
                                 GIVING W25-TOTAL.
             SUBTRACT W25-VALUE FROM W25-TOTAL 
                                 GIVING W25-ADJUST.
           IF W25-ADJUST NOT = ZERO
	       MOVE W10-LEDG	 TO DPT-CODE
	       PERFORM READ-DEPART THRU READ-DEPART-EXIT
               PERFORM AY38 THRU AY59
               ADD W25-ADJUST    TO DPT-ADJ-DAY DPT-ADJ-MTD
                                    DPT-ADJ-YTD STK-ADJMTD 
                                    STK-ADJYTD
               PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT
               IF WX-INT = "Y"
                   MOVE W12-T-YMD
                                 TO WX-DATE
                   MOVE 28       TO WX-TYPE
                   MOVE W10-LEDG TO WX-LEDG
                   MOVE DPT-PUR-GL
                                 TO WX-AC
                   MOVE "Stock Amended"
                                 TO WX-NAR
                   MOVE W25-ADJUST
                                 TO WX-VALUE
                   PERFORM GL-TRF-UPDATE
                   MOVE 27       TO WX-TYPE
                   MOVE WX-ADJGL TO WX-AC
                   MULTIPLY -1   BY WX-VALUE
		   PERFORM GL-TRF-UPDATE.
	   IF WS-CARDEX = "Y"
	       IF (W25-ADJUST NOT = ZERO) OR
		  (STK-QUANT NOT = W10-QUANT)
		   INITIALIZE STX-REC1
		   MOVE W10-EXT-ITEM
				 TO STX-EXT-CODE
		   MOVE W12-T-YMD
				 TO STX-DTE
		   MOVE ZERO	 TO STX-SEQ
		   MOVE LS0-NO	 TO STX-USER
		   MOVE "AMEND"  TO STX-PROG
		   MOVE LS-USER  TO STX-WS
		   MOVE "Quantity amended"
				 TO STX-REMARKS
		   SUBTRACT STK-QUANT FROM W10-QUANT
				 GIVING STX-QNT
		   MOVE W25-ADJUST
				 TO STX-COST
		   COMPUTE STX-SELL ROUNDED = (W25-ADJUST +
			   ((W25-ADJUST * W10-MARKUP) / 100))
		   PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
		   MOVE 1	 TO WS-ACTION
		   PERFORM AY46 THRU AY59.

016660 BC21.
016670	     MOVE W10-EXT-ITEM	 TO STK-CODE WS-ITM.
016680	     MOVE W10-BIN	 TO STK-BIN.
           IF W10-DESC = SPACES
	       GO TO BC21B.
             MOVE 30             TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W10-DESC
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

       BC21A.
             MOVE W10-DESC       TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-29	 TO W10-DESC
	       GO TO BC21A.

       BC21B.
	   IF NOT(W10-DESC = STK-DESC)
	       MOVE 1		 TO WS-INDB.
016690       MOVE W10-DESC       TO STK-DESC.
	   IF W10-DESC2 = SPACES
	       GO TO BC21BC.
	     MOVE 30		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W10-DESC2
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

       BC21BA.
	     MOVE W10-DESC2	 TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-29	 TO W10-DESC2
	       GO TO BC21BA.

       BC21BC.
	   IF NOT(W10-DESC2 = STK-DESC2)
	       MOVE 1		 TO WS-INDB.
019600	     MOVE W10-DESC2	 TO STK-DESC2.
             MOVE 10             TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W10-ACODE
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	     MOVE 18		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W10-EXT-CODE2
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
016700       MOVE W10-TAX        TO STK-TAX.
016710     IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
016720         IF W10-MARKUP NOT > ZERO
016730		   PERFORM BC50.
016740       MOVE W10-MARKUP     TO STK-MARKUP.
016750       MOVE W10-QUANT      TO STK-QUANT.
	     MOVE W10-PER	 TO STK-PER.
	     MOVE W10-PRC	 TO STK-PRC.
016760       MOVE W10-REORD      TO STK-LEVEL.
016770       MOVE W10-LTIME      TO STK-LTIME.
016780       MOVE W10-COST       TO W10-CCOST.
016790       MOVE W10-SELL       TO W10-CSELL.
016800     IF W10-COST = ZERO
016810	       IF (W10-SELL NOT = ZERO) AND (W10-MARKUP NOT = ZERO)
016820		   COMPUTE W10-CCOST ROUNDED = (W10-CSELL /
016830			   (100 + W10-MARKUP) * 100).
016840     IF W10-SELL = ZERO
016850         IF (W10-COST NOT = ZERO) AND (W10-MARKUP NOT = ZERO)
016860		   COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
016870			   ((W10-CCOST * W10-MARKUP) / 100)).
	   IF W10-CSELL = ZERO
	       MOVE W10-CCOST	 TO W10-CSELL.
016880	     MOVE W10-CCOST	 TO W10-COST.
016890       MOVE W10-CSELL      TO W10-SELL.
016900       MOVE W10-COST       TO STK-COST.
016920	     MOVE W10-LEDG	 TO STK-LDG.
             MOVE W10-GUAR       TO STK-GUAR.
016930       MOVE W10-ORDER      TO STK-ORD.
016940       MOVE W22-DTE2       TO STK-DTE.
	     MOVE W10-PURCH	 TO W22-DTE3.
	     PERFORM SWITCH-DATE-TO-CYMD THRU SWITCH-DATE-EXIT.
	     MOVE W22-DTE2	 TO STK-PUR.
016950     IF W10-AVRG = ZERO
016960         MOVE W10-COST     TO STK-AVGE
016970     ELSE
016980         MOVE W10-AVRG     TO STK-AVGE.
	     MOVE W10-ALT-DISC	 TO STK-ALT-DISC.
016990       MOVE W10-IND        TO STK-IND.
016710     IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
016720         IF W10-CMARKUP NOT > ZERO
016730		   PERFORM BC52.
	   IF W10-IND = 2
	       MOVE "-LAB-"	 TO STK-SUPP.
017000	   IF (W10-CMARKUP = ZERO) AND
017010        (W10-CASH = ZERO)
017020         MOVE W10-MARKUP   TO W10-CMARKUP
017030         MOVE W10-SELL     TO W10-CASH
017040	       GO TO BC21C.
017050     IF W10-CMARKUP = ZERO
017060	       MOVE W10-CASH	 TO W10-CSELL
017070	       COMPUTE W10-CMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017080					     / W10-CCOST) * 100).
017090     IF W10-CASH = ZERO
017100	       COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST *
017110					    W10-CMARKUP) / 100))
017120	       MOVE W10-CSELL	 TO W10-CASH.

017130 BC21C.
017140	     MOVE W10-CMARKUP	 TO STK-CMARKUP.
016710     IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
016720         IF W10-WMARKUP NOT > ZERO
016730		   PERFORM BC54.
017000     IF (W10-WMARKUP = ZERO) AND
017010        (W10-WSALE = ZERO)
017020	       MOVE W10-CMARKUP  TO W10-WMARKUP
017030	       MOVE W10-CASH	 TO W10-WSALE
017040	       GO TO BC21D.
017050     IF W10-WMARKUP = ZERO
017060	       MOVE W10-WSALE	 TO W10-CSELL
017070	       COMPUTE W10-WMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017080						/ W10-CCOST) * 100).
017090     IF W10-WSALE = ZERO
017100	       COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST *
017110					    W10-WMARKUP) / 100))
017120	       MOVE W10-CSELL	 TO W10-WSALE.

017130 BC21D.
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
	       MOVE W05-VAT(STK-TAX)
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
	     MOVE ZERO		 TO W05-RATE.
           IF W10-TAX > 0
	       MOVE W05-VAT(W10-TAX)
				 TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED 
		   = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED 
		   = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED 
		   = W10-SELL + (W10-SELL * W05-RTE).

       BC21E.
017140       MOVE W10-WMARKUP    TO STK-WMARKUP.
016910       MOVE W10-SELL       TO STK-SELL.
	   IF NOT (W10-CASH = STK-CASH)
017150	       MOVE W10-CASH	 TO STK-CASH
	       PERFORM BA000.
017150       MOVE W10-WSALE      TO STK-WSALE.
017160	     MOVE W10-EXT-CODE2  TO STK-XREF.
017170       MOVE W10-ACODE      TO STK-ACODE.
	     MOVE W10-EXEMPT	 TO STK-EXEMPT.
	   IF STK-IND = 2
               MOVE ZERO         TO STK-COST
                                    STK-MARKUP
                                    STK-CMARKUP
                                    STK-WMARKUP.
	   IF (LS0-LEV > 3) AND (W10-IND < 2)
	       MOVE 2		 TO WS-S1
	       PERFORM BC35.
	   IF WS-CON-FACT = "Y"
	       PERFORM BD000.
	     MOVE W10-FACT	 TO STK-FACT.
017190	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
017230       MOVE STK-RECORD1    TO W15-RECORD1.
020930	     SUBTRACT W25-VALUE  FROM STK-TOTAL.
020940       ADD W25-TOTAL       TO STK-TOTAL.
	     PERFORM AY70 THRU AY999.
	   IF LS-OK = "Y"
	       DISPLAY S06E
	       DISPLAY S06C.
             INITIALIZE W200-MEM-FLDS.
             INITIALIZE SME-RECORD1.
             MOVE STK-CODE       TO SME-CODE.
             MOVE 1              TO SME-SEQ.
	     PERFORM READ-STKMEM THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO BC22A.

       BC22.
             MOVE SME-SEQ        TO WS-S1.
	     MOVE SME-DATA	 TO W200-MEM(WS-S1).
             PERFORM DELETE-STKMEM-REC THRU WRITE-STKMEM-EXIT.
	     PERFORM READ-STKMEM-NEXT THRU READ-STKMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF SME-CODE = STK-CODE
                   GO TO BC22.
             PERFORM BC23A.
             GO TO BC23C.

       BC22A.
	   IF LS0-LEV < 3
	       GO TO BC24C.
	     MOVE "Memo/Remarks/Specifications required Y/N  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.

       BC23.
	     PERFORM OPT-MESSAGE.
017380	   IF NOT (WS-OPTION = "Y" OR "N")
017390	       GO TO BC22A.
017400     IF WS-OPTION = "N"
               GO TO BC24C.

       BC23A.
	     PERFORM SAVE-SCREEN-2.

       BC23B.
             INITIALIZE W200-MEM-FLDS.

       BC23C.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.

       BC24.
             ACCEPT SA-MEM.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO BC24. 
             MOVE 1              TO WS-S1.
             INITIALIZE SME-RECORD1.
             MOVE WS-ITM         TO SME-CODE.
             MOVE 1              TO SME-SEQ.

       BC24A.
	   IF W200-MEM(WS-S1) NOT = SPACES
	       MOVE W200-MEM(WS-S1)
                                 TO SME-DATA
               PERFORM WRITE-STKMEM THRU WRITE-STKMEM-EXIT
               ADD 1             TO SME-SEQ.
           IF WS-S1 < 12
               ADD 1             TO WS-S1
               GO TO BC24A.
	     PERFORM RESTORE-SCREEN-2.

       BC24C.
020520	     MOVE STK-CODE	 TO PRT-EXT-ITEM.
020530	     PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = ZERO
020540         MOVE 1            TO WS-S6
020550         GO TO BC25.
020970	     GO TO BC30.

020730 BC25.
	     MOVE "Must production details be updated Y/N  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.

       BC26.
	     PERFORM OPT-MESSAGE.
017380	   IF NOT (WS-OPTION = "Y" OR "N")
017390	       GO TO BC25.
017400     IF WS-OPTION = "N"
	       GO TO BC30.

       BC27.
020740	     MOVE PRT-EXT-CODE(WS-S6)
				 TO W11-EXT-CODE(WS-S6).
020750	     MOVE SPACES	 TO W11-DESC(WS-S6).
020760	     MOVE PRT-QUANT(WS-S6)
				 TO W11-QUANT(WS-S6).
020770     IF WS-S6 < 30
020780         ADD 1             TO WS-S6
020790	       GO TO BC27.
020800       MOVE PRT-COMMENT    TO W10-COMMENT.
020810       MOVE 1              TO WS-AMEND.
020820       MOVE ZERO           TO W10-MCOST.
020830	     PERFORM BC63 THRU BC99.
	   IF WS-INDB = 1
	       PERFORM BC200 THRU BC210.
020960	     PERFORM AY70 THRU AY999.
      *
      *    ****    C H E C K   F O R   C A S E S   O R	 P A C K S
      * 	   A N D   A L L O W   A M E N D M E N T S
      *
       BC30.
	   IF (LS0-LEV > 3) AND (W10-IND < 2)
	       IF (WS-USE-PACKS = "Y") OR
		  (WS-USE-CASES = "Y")
		   DISPLAY CLEAR-L25
		   PERFORM SAVE-SCREEN
		   CALL "STP\STPPACKS"
			 USING WS-PARID LS-USER-ID W10-STOCK
		       ON EXCEPTION
		       MOVE
		       "Program- STPPACKS not on disk, press ANY key"
				 TO WS-ERR-STRING
003260		       PERFORM ERROR-MESSAGE
		       GO TO BC22
		   END-CALL
		   CANCEL "STP\STPPACKS"
		   PERFORM RESTORE-SCREEN
		   GO TO BC32.
	     GO TO BC000-AMEND.

       BC32.
017230	     MOVE W15-RECORD1	 TO STK-RECORD1.
      *
      *    ****   R E - S T A R T   R E C O V E R Y   L O G I N G
      *
020430	     PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
020440       PERFORM AY05 THRU AY59.
	     MOVE W10-USE-CASES  TO STK-USE-CASES.
	     MOVE W10-CASES	 TO STK-CASES.
	     MOVE W10-USE-PACKS  TO STK-USE-PACKS.
	     MOVE 2		 TO WS-S1.
      *
      *    ****   P A C K   D E T A I L S
      *
       BC35.
	     MOVE W10-UNT(WS-S1) TO STK-UNT(WS-S1).
	     MOVE W10-PR(WS-S1)  TO STK-PR(WS-S1).
	     MOVE W10-MKUP(WS-S1)
				 TO STK-MKUP(WS-S1).
	     MOVE W10-CMKUP(WS-S1)
				 TO STK-CMKUP(WS-S1).
	     MOVE W10-WMKUP(WS-S1)
				 TO STK-WMKUP(WS-S1).
	     MOVE W10-SL(WS-S1)  TO STK-SL(WS-S1).
	     MOVE W10-CSH(WS-S1) TO STK-CSH(WS-S1).
	     MOVE W10-WSL(WS-S1) TO STK-WSL(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BC35.

       BC40.
017190	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
	     PERFORM AY70 THRU AY999.
	     GO TO BC000-AMEND.

       BC45.
	     MOVE STK-UNT(WS-S1) TO W10-UNT(WS-S1).
	     MOVE STK-PR(WS-S1)  TO W10-PR(WS-S1).
	     MOVE STK-MKUP(WS-S1)
				 TO W10-MKUP(WS-S1).
	     MOVE STK-CMKUP(WS-S1)
				 TO W10-CMKUP(WS-S1).
	     MOVE STK-WMKUP(WS-S1)
				 TO W10-WMKUP(WS-S1).
	     MOVE STK-SL(WS-S1)  TO W10-SL(WS-S1).
	     MOVE STK-CSH(WS-S1) TO W10-CSH(WS-S1).
	     MOVE STK-WSL(WS-S1) TO W10-WSL(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BC45.
      *
      *    ****  D E F A U L T   M A R K U P
      *  
017540 BC50.
017600     IF W10-SELL > ZERO AND W10-COST > ZERO
017610         MOVE W10-SELL     TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630	       COMPUTE W10-MARKUP ROUNDED =
		    (((W10-CSELL - W10-CCOST) / W10-COST) * 100)
017590     ELSE
017580         MOVE DPT-MARKUP   TO W10-MARKUP.

       BC51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *  
       BC52.
017600     IF W10-CASH > ZERO AND W10-COST > ZERO
017610         MOVE W10-CASH     TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630	       COMPUTE W10-CMARKUP =
		    (((W10-CSELL - W10-CCOST) / W10-COST) * 100)
017590     ELSE
017580         MOVE DPT-CMRKUP   TO W10-CMARKUP.

       BC53.
017660       EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *  
       BC54.
017600     IF W10-WSALE > ZERO AND W10-COST > ZERO
017610         MOVE W10-WSALE    TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630	       COMPUTE W10-WMARKUP =
		    (((W10-CSELL - W10-CCOST) / W10-COST) * 100)
017590     ELSE
017580         MOVE DPT-WMRKUP   TO W10-WMARKUP.

017650 BC55.
017660       EXIT.

017770 BC63.
             MOVE 0501           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
017790       DISPLAY S32.

017800 BC64.
017810       ACCEPT W10-COMMENT AT 0618
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     PERFORM CHECK-CORRECT.
017900	   IF WS-OPTION = "N"
017910	       GO TO BC64.

017920 BC65.
017930       MOVE 1              TO WS-S6 WS-S7.
017940       MOVE 8              TO LIN.

017950 BC70.
017990	     MOVE SPACES	 TO W10-EXT-CODE2.
018000       MOVE ZERO           TO W10-MQNT.
018010	   IF W11-EXT-CODE(WS-S6) = SPACES
018020	       GO TO BC75.
018040	     MOVE W11-EXT-CODE(WS-S6)
				 TO W10-EXT-CODE2.
018050	     MOVE W11-QUANT(WS-S6)
                                 TO W10-MQNT.

018060 BC75.
             PERFORM AA300.
	   IF WS-EXT-STK = "Y"
	       ACCEPT S32B AT LINE LIN
	   ELSE
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   ACCEPT S32A AT LINE LIN
	       ELSE
021070		   ACCEPT S32C AT LINE LIN.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-CODE2
		     GO TO BC75A
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BC75
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN F9-KEY
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BC75
               END-EVALUATE
	       PERFORM AA50-LOOKUP
	       MOVE W10-EXT-ITEM TO W10-EXT-CODE2
	       IF WS-EXT-STK = "Y"
		   DISPLAY S32B AT LINE LIN
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S32A AT LINE LIN
	       ELSE
021070		   DISPLAY S32C AT LINE LIN
	       END-IF
	       END-IF
	       IF W10-EXT-CODE2 = SPACES
		   GO TO BC75.
	     MOVE 18		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W10-EXT-CODE2
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

       BC75A.
018280	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	     DISPLAY CLEAR-L25.
018080	   IF W10-EXT-CODE2 = SPACE
018090         IF WS-AMEND = 1
018100		   GO TO BC95
018110         ELSE
018120		   GO TO BC85.
018130	     MOVE W10-EXT-CODE2	 TO W10-EXT-ITEM.

       BC76.
018140       PERFORM CA000.
018150     IF WS-F-ERROR = 22
	       MOVE "Z"		 TO W10-ETYPE
018620	       PERFORM AA50-LOOKUP
	       MOVE W10-EXT-ITEM TO W10-EXT-CODE2
	       IF WS-EXT-STK = "Y"
		   DISPLAY S32B AT LINE LIN
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S32A AT LINE LIN
	       ELSE
		   DISPLAY S32C AT LINE LIN
	       END-IF
	       END-IF
018640	       IF W10-EXT-CODE2 = SPACES
018720		   GO TO BC75
               ELSE
		   GO TO BC76.
018200	     DISPLAY (LIN, 28) STK-DESC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
018290	   IF WS-OPTION = "N"
018310	       GO TO BC75.

018330 BC80.
             MOVE 64             TO COL.
018340       ACCEPT S38 AT LINE LIN COLUMN COL.
	     PERFORM CHECK-CORRECT.
018450	   IF WS-OPTION = "N"
018460	       GO TO BC80.
018470     IF W10-MQNT = ZERO
018480	       GO TO BC85.
018490	     MOVE W10-EXT-CODE2  TO W11-EXT-CODE(WS-S6).
018500	     MOVE STK-DESC	 TO W11-DESC(WS-S6).
018510	     MOVE W10-MQNT	 TO W11-QUANT(WS-S6).
018520       COMPUTE W10-MCOST = W10-MCOST + (W10-MQNT * STK-AVGE).
           IF WS-S6 < 30
018530         ADD 1             TO WS-S6
           ELSE
	       GO TO BC85.
018580     IF LIN < 22
018590         ADD 1             TO LIN
018600	       GO TO BC70.
018550       ADD 1               TO WS-S7.
018560       MOVE 8              TO LIN.
             MOVE WS-S7          TO WS-S8.

       BC81.
	     MOVE W11-EXT-CODE(WS-S8)
				 TO W10-EXT-CODE2.
	   IF WS-EXT-STK = "Y"
	       DISPLAY S32B AT LINE LIN
	   ELSE
	   IF WS-USE-ITM = "Y"
	       DISPLAY S32A AT LINE LIN
	   ELSE
021070	       DISPLAY S32C AT LINE LIN.
	     MOVE W11-DESC(WS-S8)
                                 TO W10-DESC.
018200	     DISPLAY (LIN, 28) W10-DESC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     MOVE W11-QUANT(WS-S8)
                                 TO W10-MQNT.
018340       DISPLAY S38 AT LINE LIN COLUMN COL.
           IF LIN < 22
               ADD 1             TO LIN WS-S8
	       GO TO BC81.
019250	     DISPLAY WS-BLNK78 AT 2202 WITH FOREGROUND-COLOR 3.
	     GO TO BC70.

018610 BC85.
018620       DISPLAY "Input details complete" AT 2224
                      WITH FOREGROUND-COLOR 14.
	     PERFORM CHECK-CORRECT.
	     MOVE 2201		 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
018710     IF WS-OPTION = "N"
018720         MOVE ZERO         TO W10-MCOST
018730         MOVE 1            TO WS-S6
018740         MOVE 8            TO LIN
018750	       GO TO BC70.
             PERFORM AY60 THRU AY999.
018760     IF WS-AMEND = 1
               MOVE 0            TO WS-ACTION
               PERFORM AY20 THRU AY59.

018890 BC88.
018900       INITIALIZE PRT-REC1.
018920	     MOVE WS-ITM	 TO PRT-EXT-ITEM.
018930       MOVE W15-RECORD1    TO STK-RECORD1.
	     COMPUTE STK-COST ROUNDED = W10-MCOST * 1.
	     MOVE STK-COST	 TO W10-CCOST.
018950	     COMPUTE W10-SELL ROUNDED = (W10-CCOST +
018960             ((W10-CCOST * STK-MARKUP) / 100)).
018970       COMPUTE W10-CASH ROUNDED = (W10-CCOST + 
018980             ((W10-CCOST * STK-CMARKUP) / 100)).
018970       COMPUTE W10-WSALE ROUNDED = (W10-CCOST + 
018980             ((W10-CCOST * STK-WMARKUP) / 100)).
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
	       MOVE W05-VAT(STK-TAX)
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
	   IF W10-TAX > 0
	       MOVE W05-VAT(W10-TAX)
				 TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED 
		   = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED 
		   = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED 
		   = W10-SELL + (W10-SELL * W05-RTE).

       BC89.
	     MOVE "Change the prices Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO BC89.
	   IF LS-OK = "Y" AND WS-OPTION = "Y"
016910	       MOVE W10-SELL	 TO STK-SELL
017150	       MOVE W10-CASH	 TO STK-CASH
017150	       MOVE W10-WSALE	 TO STK-WSALE
	       PERFORM BC100 THRU BC199
	   ELSE
017230	       MOVE W15-RECORD1	 TO STK-RECORD1.
019000       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
019010       MOVE 1              TO WS-S6.
019020       MOVE W10-COMMENT    TO PRT-COMMENT.

019030 BC90.
019040	     MOVE W11-EXT-CODE(WS-S6)
				 TO PRT-EXT-CODE(WS-S6).
019060	     MOVE W11-QUANT(WS-S6)
				 TO PRT-QUANT(WS-S6).
019070     IF WS-S6 < 30
019080         ADD 1             TO WS-S6
019090	       GO TO BC90.
019100     IF WS-AMEND = 0
019110         PERFORM WRITE-SPARTS THRU WRITE-SPARTS-EXIT
019120         MOVE 1            TO WS-ACTION
               PERFORM AY20 THRU AY59
019130     ELSE
019140         PERFORM REWRITE-SPARTS THRU WRITE-SPARTS-EXIT.
019150	     GO TO BC99.

019160 BC95.
019170	   IF W11-EXT-CODE(WS-S6) = SPACES
019180	       GO TO BC85.
019190       ADD 1 WS-S6         GIVING WS-S8.
019200	     MOVE SPACES	 TO W11-EXT-CODE(WS-S6)
				    W11-DESC(WS-S6).
019220	     MOVE ZERO		 TO W11-QUANT(WS-S6).
019230     IF WS-S8 > 29
019240	       GO TO BC85.

019250 BC96.
019260	     MOVE W11-EXT-CODE(WS-S8)
				 TO W11-EXT-CODE(WS-S6).
019270	     MOVE W11-DESC(WS-S8)
				 TO W11-DESC(WS-S6).
019280	     MOVE W11-QUANT(WS-S8)
				 TO W11-QUANT(WS-S6).
019290	     MOVE SPACES	 TO W11-EXT-CODE(WS-S8)
				    W11-DESC(WS-S8).
019310	     MOVE ZERO		 TO W11-QUANT(WS-S8).
019320     IF WS-S8 < 30
019330         ADD 1             TO WS-S6 WS-S8
019340	       GO TO BC96.
019350	     GO TO BC70.

019360 BC99.
019370       EXIT.

       BC100.
             MOVE 0301           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
019430       DISPLAY S04B.
019440       DISPLAY S04-N.
	     PERFORM BC012-DET.
             MOVE STK-GUAR       TO W10-GUAR.
             PERFORM AA400.

       BC105.
	     MOVE ZERO		 TO W05-RATE.
           IF W10-TAX > 0
	       MOVE W05-VAT(W10-TAX)
				 TO W05-RATE.

       BC110.
             COMPUTE W10-VCASH ROUNDED 
		   = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED 
		   = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED 
		   = W10-SELL + (W10-SELL * W05-RTE).
             DISPLAY S06A.
             DISPLAY S06C.

       BC115.
             ACCEPT S06D.
018940       MOVE W10-COST       TO W10-MCOST W10-CCOST.
018950       COMPUTE W10-CSELL ROUNDED = (W10-CCOST + 
018960             ((W10-CCOST * STK-MARKUP) / 100)).
016890       MOVE W10-CSELL      TO W10-SELL.
018970       COMPUTE W10-CSELL ROUNDED = (W10-CCOST + 
018980             ((W10-CCOST * STK-CMARKUP) / 100)).
017120       MOVE W10-CSELL      TO W10-CASH.
018970       COMPUTE W10-CSELL ROUNDED = (W10-CCOST + 
018980             ((W10-CCOST * STK-WMARKUP) / 100)).
017120       MOVE W10-CSELL      TO W10-WSALE.
             DISPLAY S06E.
	     PERFORM BC105 THRU BC110.
             ACCEPT S06E.
	     MOVE 18		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
	   IF WS-OPTION = "N"
	       GO TO BC115.
             MOVE W10-COST       TO STK-COST.
016910       MOVE W10-SELL       TO STK-SELL.
017150       MOVE W10-CASH       TO STK-CASH.
017150       MOVE W10-WSALE      TO STK-WSALE.

       BC199.
             EXIT.
      *
      *    ****    C H E C K   T H E   A L T E R N A T E   I N D E X
      *
       BC200.
	     MOVE ZERO		 TO WS-ERROR.
	     MOVE STK-CODE	 TO STKA-CODE.
	     MOVE ZERO		 TO STKA-NO.
	     PERFORM READ-STKALT THRU READ-STKALT-EXIT.
	   IF WS-F-ERROR = 49
	       MOVE 9		 TO WS-ERROR
	       GO TO BC210.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If  either of the two descriptions or Item code has  been ³
      *    ³ amended, then  the existing Alternate Index  entries will ³
      *    ³ be deleted and new entries generated.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BC205.
	     PERFORM DELETE-STKALT-REC THRU WRITE-STKALT-EXIT.
	     PERFORM READ-STKALT-NEXT THRU READ-STKALT-EXIT.
	   IF NOT(WS-F-ERROR = 49)
	       IF (STKA-CODE = STK-CODE)
		   GO TO BC205.
021960	     PERFORM AY60 THRU AY999.
	     PERFORM STOCK-DESC-INDEX.
022520	     PERFORM AY70 THRU AY999.

       BC210.
	     EXIT.

020980 BC999.
020990       EXIT.
      *
      *    ****    C O N V E R T I O N	 F A C T O R
      *
       BD000	       SECTION 50.
       BD00.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1025
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1125
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Convertion Factor "
		      WITH FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
		     "          " WITH FOREGROUND-COLOR 3 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     " " WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.

	     DISPLAY "À" AT 1225
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
		     PERFORM SCREEN-SHADOW.

       BD05.
	     ACCEPT SA-FACT.
	     MOVE 15		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
	     PERFORM RESTORE-SCREEN-3.
	   IF WS-OPTION = "N"
	       GO TO BD00.

       BD999.
	     EXIT.
      /
      *    ****  A L T E R N A T E   S T O C K	 I N D E X
      *   

	     COPY STOCK.DIX.

      /    *************************************************************
      *    ****		I N I T I A L I S E   P R O G R A M	    ****
      *    *************************************************************
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
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
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 	  I N T E G R A T I O N	  F I L E	    ****
      *    *************************************************************
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	    R E C O V E R Y   F I L E		    ****
      *    *************************************************************
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
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
      *    ****  A D J U S T M E N T   -   H I S T O R Y   F I L E  ****
      *    *************************************************************
	     MOVE "STKDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKDEX.
      *    *************************************************************
      *    ***	S T O C K   ( I V E N T O R Y )   M E M O   F I L E  ***
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
	   IF NOT (PAR-ART11 = "Y" OR "N")
	       MOVE "N" 	 TO PAR-ART11.
	     MOVE PAR-ART11	 TO W10-ART11.

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
	     MOVE STK-CON-FACT	 TO WS-CON-FACT.
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
018890       MOVE PAR-PW         TO W85-PASS.
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
042520	     MOVE 6		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"          TO WX-INT
           ELSE
               MOVE "Y"          TO WX-INT.
             MOVE PAR-DEBGL      TO WX-DEBGL.
             MOVE PAR-CREGL      TO WX-CREGL.
             MOVE PAR-INTGL      TO WX-INTGL.
             MOVE PAR-BNKGL      TO WX-BNKGL.
             MOVE PAR-UNPROF     TO WX-UNPROF.
             MOVE PAR-REDGL      TO WX-REDGL.
             MOVE PAR-ADJGL      TO WX-ADJGL.
             MOVE PAR-RLGL       TO WX-RLGL.
             MOVE PAR-DSGL       TO WX-DSGL.
             GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Setup the Screen Graphics Characters: ³ Ä Ú ¿ À Ù Â Á etc ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250	     MOVE WS-G1 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 TO WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
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
	       MOVE "SupervisorÄ"  TO WS-WRKHD
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
	       MOVE PRT-EXT-ITEM TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 46
041860	       MOVE W02-STKDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 49
041860	       MOVE W02-STKALT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STKA-KEY	 TO WS-KEYX.
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
