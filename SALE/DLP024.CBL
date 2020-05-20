      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   **	      *******	  ****	   ******	***    *
      *   **	**  **	      **    **	 **  **   **	**     ****    *
      *   **	**  **	      **    **	**    **       **     ** **    *
      *   **	**  **	      *******	**    **     **      **  **    *
      *   **	**  **	      **	**    **   **	    ********   *
      *   **	**  **	      **	 **  **   **		 **    *
      *   *******   ********  **	  ****	  ********	****   *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E L I V E R I E S   T R F   S T O C K / V E H I C L E    *
      * 							       *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DLP024.
000030 AUTHOR.	       J.W.LEMMON (JWL Software).
000040 DATE-WRITTEN.   APRIL 1996.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1996 - 2010
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

000140 COPY STKMEM.SL.

000140 COPY STOCK.SL.

       COPY WARHSE.SL.

       COPY WSTOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000280 COPY PARAM.FDE.

000380 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000140 COPY STKMEM.FDE.

000300 COPY STOCK.FDE.

000280 COPY WARHSE.FDE.

000280 COPY WSTOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001710 77  WS-SUB	    PIC 9(04)	  COMP-5.
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
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
       77  WS-ETYPE	    PIC X(01).
002020 77  WS-OPTION	    PIC X(01).
002020 77  WS-SKIP          PIC X(01).
002040 77  WS-ADJ           PIC X(01).
002070 77  WS-ERROR         PIC 9(01).
002120 77  WS-AMEND         PIC 9(01)     VALUE 0.
002140 77  WS-CONREC        PIC 9(01) VALUE ZERO.
002150 77  WS-LEDG          PIC 9(03).
002200 77  WS-COMP          PIC 9(01).
002200 77  WS-IND	    PIC 9(01) VALUE ZERO.
002220 77  WS-TYPE          PIC X(01).
002270 77  WS-PASSWORD      PIC X(08).
       77  WS-PRINT         PIC 9(01).
002280 77  WS-PASS          PIC X(08).
       77  WS-ERR1	   PIC	X(20) VALUE "Quantity > Available".
002030 77  WS-ERR3         PIC  X(15) VALUE "Data incomplete".
002070 77  WS-ERR7	   PIC	X(23) VALUE
002080                     "Address > 67 characters".
       77  TODAY-DDMMYY	   PIC	9(06) COMP-5.
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

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "064".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

000380 COPY RECOVER.ID.

       COPY SHARED.ID.

000140 COPY STKMEM.ID.

000300 COPY STOCK.ID.

000360 COPY WARHSE.ID.

000360 COPY WSTOCK.ID.

       01  W05-TAX-RATES.
000760     03  W05-VAT-RATES.
000770         05  W05-VAT     PIC S9(03)V99 COMP-3 OCCURS 6.
000780     03  W05-TAX-RATES REDEFINES W05-VAT-RATES.
000790         05  W05-VAT-R1  PIC S9(03)V99 COMP-3.
000800         05  W05-VAT-R2  PIC S9(03)V99 COMP-3.
000810         05  W05-VAT-R3  PIC S9(03)V99 COMP-3.
000820         05  W05-VAT-R4  PIC S9(03)V99 COMP-3.
000830         05  W05-VAT-R5  PIC S9(03)V99 COMP-3.
000840         05  W05-VAT-R6  PIC S9(03)V99 COMP-3.
           03  W05-RATE        PIC S9(03)V99 COMP-3.
           03  W05-RTE REDEFINES W05-RATE
                               PIC S9V9(04)  COMP-3.

       COPY W10.STK.

       COPY W12.WS.

000030 01  W15-RECORD1.
	   03  W15-KEY.
	       05  W15-WAR     PIC  X(06).
000040	       05  W15-CODE    PIC  X(14).
000060	       05  W15-ITM     PIC  X(03).
	   03  W15-BIN	       PIC  X(04).
000110	   03  W15-QUANT       PIC S9(07)V9(04) COMP-3.
000200	   03  W15-MTD	       PIC S9(09)V99  COMP-3.
000230	   03  W15-YTD	       PIC S9(09)V99  COMP-3.
000220	   03  W15-MTDV        PIC S9(09)V99  COMP-3.
000250	   03  W15-YTDV        PIC S9(09)V99  COMP-3.
	   03  FILLER	       PIC  X(03).

000030 01  W16-RECORD1.
	   03  W16-KEY.
	       05  W16-WAR     PIC  X(06).
000040	       05  W16-CODE    PIC  X(14).
000060	       05  W16-ITM     PIC  X(03).
	   03  W16-BIN	       PIC  X(04).
000110	   03  W16-QUANT       PIC S9(07)V9(04) COMP-3.
000200	   03  W16-MTD	       PIC S9(09)V99  COMP-3.
000230	   03  W16-YTD	       PIC S9(09)V99  COMP-3.
000220	   03  W16-MTDV        PIC S9(09)V99  COMP-3.
000250	   03  W16-YTDV        PIC S9(09)V99  COMP-3.
	   03  FILLER	       PIC  X(03).

       COPY W40.WS.

002920 01  W40-WAREHOUSE.
002930     03  W40-NAME1.
002940	       05  W40-NAME    PIC  X(40).
003020	   03  W40-NAME2 REDEFINES W40-NAME1.
003030	       05  W40-NCHAR OCCURS 40 PIC X(01).
           03  W40-CONTACT     PIC  X(30).
	   03  W40-CODE	       PIC  X(06).
	   03  W40-CODE1       PIC  X(06).
	   03  W40-CODE2       PIC  X(06).
003070	   03  W40-ADD.
003080	       05  W40-ADDRESS1.
003090		   07  W40-ADD1	PIC X(30).
003100		   07  W40-ADD2	PIC X(30).
003110		   07  W40-ADD3	PIC X(30).
003120		   07  W40-ADD4	PIC X(30).
003130	       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
003140		   07  W40-ACHAR OCCURS 120 PIC X(01).
003230     03  W40-POST        PIC 9(04).
003250     03  W40-TEL         PIC X(14).
003260     03  W40-FAX         PIC X(14).

       COPY FUNCTION.WS.

003800 COPY W50.WS.

003550 01  W60-ADD.
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
	   03  W75-WHOUSE OCCURS 18.
	       05  W75-WHCDE  PIC  X(06).
	   03  W75-ITEMS OCCURS 18.
	       05  W75-CODE   PIC  X(14).
	       05  W75-ITM    PIC  X(03).
	       05  W75-SELL   PIC  9(07)V99 COMP-3.
	       05  W75-VSELL  PIC  9(07)V99 COMP-3.
	       05  W75-CASH   PIC  9(07)V99 COMP-3.
	       05  W75-VCASH  PIC  9(07)V99 COMP-3.
	       05  W75-WSALE  PIC  9(07)V99 COMP-3.
	       05  W75-VWSALE PIC  9(07)V99 COMP-3.
	   03  W75-SLEDG      PIC  9(03)    COMP-3 OCCURS 10.

003930 01  W80-EDIT.
003950	   03  W80-Q1.
003960	       05  W80-S7V4   PIC Z(06)9.9999-.

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

       01  S01.
	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "STOCK WAREHOUSE TRANSFER".
	   03  LINE  4 COLUMN  5 VALUE "Item Code   ".
006240	   03  LINE  5 COLUMN  5 VALUE "Description ".
	   03  LINE  7 COLUMN  5 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Transfer Details".
	   03  LINE  8 COLUMN  5 VALUE "Source      ".
	   03  LINE  9 COLUMN  5 VALUE "Quantity    ".
	   03  LINE 11 COLUMN  5 VALUE "Destination ".
	   03  LINE 12 COLUMN  5 VALUE "Quantity    ".
	   03  LINE 14 COLUMN  5 VALUE "Transfer Qty".
	   03  LINE 16 COLUMN  7 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	     "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
      -      "컴컴컴컴커".
	   03  LINE 17 COLUMN  7 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	     "� Source and Destination refer to LOCATION CODES and keyin
      -	     "g spaces �".
	   03  LINE 18 COLUMN  7 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	     "� into either of these fields refers to the Database Stock
      -	     " Record. �".
	   03  LINE 19 COLUMN  7 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE
	     "읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
      -      "컴컴컴컴켸".

       01  S02.
007920	   03  LINE  8 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W40-CODE1 AUTO.

       01  S03.
007920	   03  LINE 11 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W40-CODE2 AUTO.

       01  S04.
007920	   03  LINE 14 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(06)9.9999 USING W10-QNT AUTO.

005530 01  S05.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550                           PIC X(14) USING W10-ITEM AUTO.

005530 01  S05-3.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(03) USING W10-I3 AUTO.

005530 01  S05-4.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S05-5.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S05-6.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S05-7.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S05-8.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S05-9.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S05-10.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S05-11.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S05-12.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S05-13.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S05-14.
005540	   03  LINE  4 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(14) USING W10-I14 AUTO.

       COPY STKLUP.CRT.

       COPY WARLUP.CRT.

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

004870 COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008230 AA000		       SECTION.
008240 AA00.
003240	   IF LS0-STLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
008250	     PERFORM ZA000-INIT.
	     PERFORM CB000.
013040	     CLOSE RECOVER.
013140 AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
             PERFORM STOCK-LOOKUP.
	     PERFORM RESTORE-SCREEN.
	     GO TO AA99.

       AA50W.
011050       PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090       PERFORM SCREEN-CONTENTS.
	     PERFORM WARHSE-LOOKUP.
011320       PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
       AA300              SECTION.
       AA301.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     " Item code/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011040		     " Alt code/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     " Description/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011080		     " X/Ref Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA305.
             EXIT.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

000140 COPY STKMEM.RD.

       COPY STOCK.RD.

       COPY WARHSE.RD.

       COPY WSTOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY STOCK.WR.

       COPY WARHSE.WR.

       COPY WSTOCK.WR.

       COPY STOCK.LUP.

       COPY WARHSE.LUP.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
010910 AY000           SECTION.
010920 AY01.
010930       MOVE 01             TO REC-FILE.
010940       MOVE WS-PARKEY      TO REC-KEY.
010950       MOVE PAR-RECORD1    TO REC-PARAM.
010960       GO TO AY50.
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
010970 AY051.
010980	     MOVE 51		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
	       PERFORM READ-WARHSE-LOCK THRU READ-WARHSE-EXIT.
011000	     MOVE WAR-RECORD1	 TO REC-WARHSE.
011010       GO TO AY50.
010970 AY052.
010980	     MOVE 52		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
	       PERFORM READ-WSTOCK-LOCK THRU READ-WSTOCK-EXIT.
011000	     MOVE WST-RECORD1	 TO REC-WSTOCK.
011010       GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
011120 AY49.
011130       MOVE 99             TO REC-FILE.
011140       MOVE ZERO           TO REC-KEY.
011150       MOVE SPACES         TO REC-DETAIL.
011160       PERFORM AY50 THRU AY59.
011170       ADD 1               TO WS-TRANS.
011180     IF WS-RECOVER > 95
011190         CLOSE RECOVER
011200         OPEN OUTPUT RECOVER
011210         CLOSE RECOVER
011220         OPEN I-O RECOVER
011230         MOVE ZERO         TO WS-RECOVER.
011240       GO TO AY59.
011250 AY50.
           IF WS-SKIP = "Y"
               GO TO AY59.
011260       ADD 1               TO WS-RECOVER.
011270       MOVE WS-RECOVER     TO WS-RECKEY.
011280       MOVE WS-TRANS       TO REC-TRAN.
011290       WRITE REC-RECORD.
011300     IF WS-STATUS NOT = "00"
011310         DISPLAY "Write error Recovery file - Status " AT 2212
011320			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			WS-STATUS
011330			WITH FOREGROUND-COLOR 7 HIGHLIGHT
011340         STOP RUN.
011370 AY59.
011380       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
	   IF WS-SKIP = "Y" OR "A"
               GO TO AY999.
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
012690	     PERFORM AY39 THRU AY59.
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
012760		   IF PAR-PROG(WS-SUB) = "CS" OR "DS" OR "ST"
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
      * 	   NETWORK/SHARED record 3.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY40 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

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
013090	     COMMIT.

	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

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
013090	     COMMIT.

011860 AY999.
011870       EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

      *
      *    ****   G E T   W A R E H O U S E   R E C O R D
      *
015170 CA600	     SECTION.
015180 CA601.
013310	     MOVE 6		 TO W50-LENGTH.

       CA605.
013320	     MOVE W40-CODE	 TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-5	 TO W40-CODE
	       GO TO CA605.
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-DENT	 TO W40-CODE.

       CA606.
013380	     MOVE W50-DENT	 TO WAR-CODE.

       CA610.
	     PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.

       CA699.
             EXIT.
      /
      *    ****   C R E A T E	N E W	W A R E H O U S E   R E C O R D
      *   
018130 CB000		 SECTION 50.
018140 CB00.
018150	     DISPLAY CLR-SCREEN.
	     MOVE 17		 TO SHADE-ROW.
	     MOVE 9		 TO SHADE-COL.
	     MOVE 66		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY S01.
             PERFORM SCREEN-SHADOW.
019460	     MOVE SPACE 	 TO W10-ITEM W10-ITM.
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

       CB05.
	     PERFORM AA300.
	   IF W10-STCK = "A"
021070	       ACCEPT S05
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
                     MOVE SPACES TO W10-ITEM
		     GO TO CB999
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO CB05
		 WHEN F2-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN F5-KEY
		     MOVE "X"	 TO WS-ETYPE
                 WHEN OTHER
                     CALL X"E5"
		     GO TO CB05
               END-EVALUATE
	       PERFORM AA50-LOOKUP
               DISPLAY S05
               IF W10-ITEM = SPACES
		   GO TO CB05.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
019490     IF W10-ITEM = SPACE
019500	       GO TO CB999.

       CB10.
019510	     PERFORM CA000.
019520     IF WS-F-ERROR = 22
	       MOVE "Z"		 TO WS-ETYPE
018620	       PERFORM AA50-LOOKUP
018630         DISPLAY S05
018640         IF W10-ITEM = SPACES
018720		   GO TO CB05
               ELSE
		   GO TO CB10.
	     DISPLAY STK-DESC AT 0518
		     WITH HIGHLIGHT BACKGROUND-COLOR 5.
019570	     MOVE STK-CODE	 TO W10-ITEM.
      *     MOVE STK-ITM	 TO W10-ITM.
019590       MOVE STK-BIN        TO W10-BIN.
019600       MOVE STK-DESC       TO W10-DESC.
019650       MOVE STK-QUANT      TO W10-QUANT.
	     MOVE SPACES	 TO W40-CODE W40-CODE1 W40-CODE2.
	     MOVE ZERO		 TO W10-QNT.
	     DISPLAY "F6" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=View Quantities"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       CB15.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     "=Warehouse Lookup,"
		     "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=View W/H Quantities"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     ACCEPT S02.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CB05
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO CB15
		 WHEN F2-KEY
016560		     PERFORM AA50W
		     MOVE W40-CODE
				 TO W40-CODE1
		 WHEN F6-KEY
		     PERFORM CB100 THRU CB199
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CB15
	       END-EVALUATE
	       DISPLAY S02
015830	       IF W40-CODE1 = SPACE
015840		   GO TO CB15.
019250	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
019490	   IF W40-CODE1 = SPACE
	       DISPLAY "Stock data base" AT 0825
			WITH HIGHLIGHT BACKGROUND-COLOR 5
	       MOVE W10-QUANT	 TO W80-S7V4
019500	       GO TO CB25.

       CB20.
	     MOVE W40-CODE1	 TO W40-CODE.
	     PERFORM CA600.
015860	   IF WS-F-ERROR = 51
017520	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50W
	       MOVE W40-CODE	 TO W40-CODE1
018630	       DISPLAY S02
018640	       IF W40-CODE1 = SPACES
018720		   GO TO CB15
               ELSE
		   GO TO CB20.
019840	     MOVE WAR-NAME	 TO W40-NAME.
	     DISPLAY W40-NAME AT 0825
		     WITH HIGHLIGHT BACKGROUND-COLOR 5.
	     MOVE W40-CODE1	 TO WST-WAR.
	     MOVE W10-ITEM	 TO WST-CODE.
	     MOVE SPACES	 TO WST-ITM.
	     PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       MOVE "No Record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CB15.
	     MOVE WST-QUANT	 TO W80-S7V4.
	     MOVE WST-RECORD1	 TO W15-RECORD1.

       CB25.
015900	     DISPLAY W80-Q1 AT 0918
		     WITH HIGHLIGHT BACKGROUND-COLOR 5.
	     MOVE SPACES	 TO W40-CODE.

       CB30.
	     ACCEPT S03.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CB15
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO CB30
		 WHEN F2-KEY
016560		     PERFORM AA50W
		     MOVE W40-CODE
				 TO W40-CODE2
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CB30
	       END-EVALUATE
	       DISPLAY S03
015830	       IF W40-CODE2 = SPACE
015840		   GO TO CB30.
019250	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
	   IF W40-CODE1 = W40-CODE2
	       MOVE "Source = Desctination"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CB30.
019490	   IF W40-CODE2 = SPACE
	       DISPLAY "Stock data base" AT 1125
			WITH HIGHLIGHT BACKGROUND-COLOR 5
	       MOVE W10-QUANT	 TO W80-S7V4
019500	       GO TO CB40.

       CB35.
	     MOVE W40-CODE2	 TO W40-CODE.
	     PERFORM CA600.
015860	   IF WS-F-ERROR = 51
017520	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50W
	       MOVE W40-CODE	 TO W40-CODE2
018630	       DISPLAY S03
018640	       IF W40-CODE2 = SPACES
018720		   GO TO CB30
               ELSE
		   GO TO CB35.
019840	     MOVE WAR-NAME	 TO W40-NAME.
	     DISPLAY W40-NAME AT 1125
		     WITH HIGHLIGHT BACKGROUND-COLOR 5.
	     INITIALIZE WST-RECORD1.
	     MOVE W40-CODE	 TO WST-WAR.
	     MOVE W10-ITEM	 TO WST-CODE.
	     MOVE SPACES	 TO WST-ITM.
	     PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       MOVE 1		 TO WS-IND
	       MOVE ZERO	 TO W80-S7V4
	   ELSE
	       MOVE ZERO	 TO WS-IND
	       MOVE WST-QUANT	 TO W80-S7V4.
	     MOVE WST-RECORD1	 TO W16-RECORD1.

       CB40.
015900	     DISPLAY W80-Q1 AT 1218
		     WITH HIGHLIGHT BACKGROUND-COLOR 5.

       CB45.
	     ACCEPT S04.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CB30
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CB30
	       END-EVALUATE.
	   IF W10-QNT = ZERO
	       GO TO CB00.
	     MOVE 16		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
015980	   IF WS-OPTION = "N"
	       GO TO CB45.
	   IF W40-CODE1 = SPACES
	       IF W10-QNT > STK-QUANT
		   MOVE WS-ERR1  TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO CB45
	       END-IF
	   ELSE
	   IF W10-QNT > W15-QUANT
	       MOVE WS-ERR1  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CB45.
	     PERFORM AY60 THRU AY999.
	   IF WS-IND = 1
	       INITIALIZE W16-RECORD1
	       MOVE W40-CODE	 TO W16-WAR
	       MOVE W10-ITEM	 TO W16-CODE
	       MOVE SPACES	 TO W16-ITM W16-BIN
	       MOVE ZERO	 TO W16-MTD W16-MTDV W16-QUANT
				    W16-YTD W16-YTDV
	       PERFORM WRITE-WSTOCK THRU WRITE-WSTOCK-EXIT
	       MOVE 1		 TO WS-ACTION
	       PERFORM AY052 THRU AY59.
	   IF W40-CODE1 = SPACES
	       MOVE 0		 TO WS-ACTION
020440	       PERFORM AY05 THRU AY59
	       SUBTRACT W10-QNT  FROM STK-QUANT
	       PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT
	   ELSE
	       MOVE W15-RECORD1  TO WST-RECORD1
	       MOVE 0		 TO WS-ACTION
020440	       PERFORM AY052 THRU AY59
	       SUBTRACT W10-QNT  FROM WST-QUANT
	       PERFORM REWRITE-WSTOCK-UNLOCK THRU WRITE-WSTOCK-EXIT.
	   IF W40-CODE2 = SPACES
	       MOVE 0		 TO WS-ACTION
020440	       PERFORM AY05 THRU AY59
	       ADD W10-QNT	 TO STK-QUANT
	       PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT
	   ELSE
	       MOVE W16-RECORD1  TO WST-RECORD1
	       MOVE 0		 TO WS-ACTION
020440	       PERFORM AY052 THRU AY59
	       ADD W10-QNT	 TO WST-QUANT
	       PERFORM REWRITE-WSTOCK-UNLOCK THRU WRITE-WSTOCK-EXIT.
             PERFORM AY70 THRU AY999.
	     GO TO CB00.

       CB100.
	     PERFORM SAVE-SCREEN.
	     DISPLAY CLEAR-L25.
	     DISPLAY "旼컴컴컫컴컴컴컴컴컴커" AT 0554
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "� Code �  Quantity   �" AT 0654
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "쳐컴컴컵컴컴컴컴컴컴캑" AT 0754
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "읕컴컴컨컴컴컴컴컴컴켸" AT 1854
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     MOVE 0854		 TO CPOS.
	     PERFORM CB180 UNTIL CLIN = 18.
	     MOVE 6		 TO SHADE-ROW.
	     MOVE 56		 TO SHADE-COL.
	     MOVE 20		 TO SHADE-WIDTH.
	     MOVE 13		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     MOVE W10-ITEM	 TO WST-CODE.
	     MOVE SPACES	 TO WST-ITM WST-WAR.

       CB105.
	     PERFORM START-AT-WH-ITEM THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO CB190.

       CB110.
000180	     MOVE 08		 TO CLIN.

       CB115.
	     PERFORM READ-WSTOCK-NEXT THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO CB120.
	   IF NOT (W10-ITEM = WST-CODE)
	       GO TO CB120.
	     MOVE WST-QUANT	 TO W80-S7V4.
	     DISPLAY (CLIN, 55) WST-WAR
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY ( , 62) W80-Q1
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	   IF CLIN < 17
	       ADD 1		 TO CLIN
	       GO TO CB115.

       CB120.
001020	     DISPLAY "PgUp" AT 2527
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "PgDn" WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
		     " - " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001170		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape to quit" WITH BACKGROUND-COLOR 3
					FOREGROUND-COLOR 1.

001310 CB125.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO CB190
		 WHEN PAGE-DOWN
001340		     GO TO CB130
		 WHEN PAGE-UP
001340		     GO TO CB140
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CB125.
	     CALL X"E5".
001280	     GO TO CB125.

       CB130.
	   IF CLIN < 17
	       GO TO CB125.

       CB135.
	     MOVE 0854		 TO CPOS.
	     PERFORM CB180 UNTIL CLIN = 18.
001630	     GO TO CB110.

       CB140.
	     MOVE 1		 TO WS-IXS1.

       CB145.
	     PERFORM READ-WSTOCK-PREV THRU READ-WSTOCK-EXIT.
	   IF (WS-F-ERROR = 52) OR
	      (W10-ITEM NOT = WST-CODE)
	       GO TO CB110.
	   IF WS-IXS1 < 10
	       ADD 1		 TO WS-IXS1
	       GO TO CB145.
	     GO TO CB110.

       CB180.
	     DISPLAY "�      �             �" AT CPOS
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     ADD 1		 TO CLIN.

       CB190.
011320       PERFORM RESTORE-SCREEN.

       CB199.
	     EXIT.

       CB999.
	     EXIT.

      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 80.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
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

	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.


042080	     MOVE 3		 TO WS-PARKEY.
042090       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042110	     MOVE WS-PTH15	 TO W02-PTH2  W02-PTH18
				    W02-PTH22 W02-PTH25
				    W02-PTH51 W02-PTH52.
042160	     MOVE LS-USER	 TO W02-USE18.
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
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.
042720       MOVE 1              TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
042520	     MOVE 5		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050550       MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
044780	     GO TO ZA999-EXIT.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
021940                              WS-TCHR (61) WS-TCHR (72).
021950       MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
021960                              WS-MCHR (61) WS-MCHR (72)
021960				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisor�"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 81.
      
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
041510       GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER TO WS-FILE
041870         MOVE WS-RECKEY TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041770	   IF WS-F-ERROR = 51
041780	       MOVE W02-WARHSE TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WAR-CODE	 TO WS-KEYX
041840	   ELSE
041770	   IF WS-F-ERROR = 52
041780	       MOVE W02-WSTOCK TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.
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
042170       STOP RUN.
