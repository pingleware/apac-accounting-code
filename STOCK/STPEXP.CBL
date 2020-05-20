      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******   ********  **    **  *******    *
      *   **    **     **     **    **  **         **  **   **    **   *
      *   **           **     **    **  **          ****    **    **   *
      *    ******      **     *******   *****        **     *******    *
      *         **     **     **        **          ****    **         *
      *   **    **     **     **        **         **  **   **         *
      *    ******      **     **        ********  **    **  **         *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   E X P O R T   P R O G R A M                    *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPEXP.
000030 AUTHOR.         J W LEMMON (CSS).
000040 DATE-WRITTEN.   SEPTEMBER 1992.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2008
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
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY DEPART.SL.

       COPY PARAM.SL.

000140 COPY STOCK.SL.

000200     SELECT STKDET  ASSIGN W02-STKDET
000040                    STATUS WS-STATUS
                          ORGANIZATION LINE SEQUENTIAL.

           SELECT STKSEL  ASSIGN DISK
                          STATUS WS-STATUS
                          ACCESS RANDOM
                          ORGANIZATION RELATIVE
                          RELATIVE WS-SELKEY.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY DEPART.FDE.

       COPY PARAM.FDE.

000270 COPY STOCK.FDE.

       FD  STKDET    LABEL RECORD OMITTED.
       01  EXP-REC1.
	   03  EXP-DATA     PIC  X(650).

       FD  STKSEL    LABEL RECORD STANDARD
                     VALUE OF FILE-ID W02-STKSEL.
       01  SEL-REC.
	   03  SEL-CODE     PIC  X(18).

      /
000350 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
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
001840 77  WS-SELKEY	   PIC	9(06)	 COMP-5.
001850 77  WS-SPAKEY	   PIC	9(06)	 COMP-5.
001860 77  WS-PARKEY	   PIC	9(06)	 COMP-5.
001870 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001880 77  WS-SINKEY	   PIC	9(06)	 COMP-5.
001890 77  WS-RECKEY	   PIC	9(06)	 COMP-5.
001900 77  WS-RECOVER	   PIC	9(06)	 COMP-5.
001910 77  WS-TRANS	   PIC	9(04)	 COMP-5 VALUE 1.
001920 77  WS-KEYSTR	   PIC	9(04)	 COMP-5.
001940 77  WS-MONTHS	   PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1	   PIC	9(04)	 COMP-5.
001970 77  WS-KSTORE	   PIC	9(04)	 COMP-5.
001980 77  WS-ITM	   PIC	X(14).
       77  WS-ETYPE	   PIC	X(01).
002010 77  WS-PAGE	   PIC	9(04)	 COMP-5.
003050 77  WS-NORM         PIC  X(04).
003060 77  WS-COND         PIC  X(04).
003070 77  WS-EXPP         PIC  X(04).
003080 77  WS-ECAN         PIC  X(04).
003090 77  WS-8LPI         PIC  X(04).
003100 77  WS-6LPI         PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI	   PIC	X(04).
       77  WS-17CPI	   PIC	X(04).
       77  WS-DBLP	   PIC	X(04).
       77  WS-DCAN	   PIC	X(04).
002020 77  WS-OPTION	   PIC	X(01).
002020 77  WS-SKIP	   PIC	X(01).
002040 77  WS-ADJ	   PIC	X(01).
002070 77  WS-ERROR	   PIC	9(01).
002080 77  WS-IND	   PIC	9(01)	  VALUE 0.
002120 77  WS-AMEND	   PIC	9(01)	  VALUE 0.
002140 77  WS-CONREC	   PIC	9(01) VALUE ZERO.
002150 77  WS-LEDG	   PIC	X(04).
002160 77  WS-LEDG1	   PIC	X(04).
002200 77  WS-COMP	   PIC	9(01).
002210 77  WS-WEEK	   PIC	9(01).
002220 77  WS-TYPE	   PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
002260 77  WS-BATCH	   PIC	9(05).
002270 77  WS-PASSWORD	   PIC	X(08).
       77  WS-PRINT	   PIC	9(01).
002280 77  WS-PASS	   PIC	X(08).
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY    PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
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
	   03  WS-PROG	       PIC  X(03) VALUE "EXP".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000300 COPY STOCK.ID.

       COPY STKSEL.ID.

       01  W02-FILE-IDS.
           03  W02-STKDET.
               05  W02-EXPORT PIC X(64) VALUE "STOCK".

       COPY W05.VAT.

       COPY W10.STK.

	   03  W10-VTAX OCCURS 4.
002850	       05  W10-SVAT    PIC S9(07)V99  COMP-3.
002860	       05  W10-CVAT    PIC S9(07)V99  COMP-3.
002860	       05  W10-WVAT    PIC S9(07)V99  COMP-3.

       COPY W11.STK.

       COPY W12.WS.

       COPY W15.STK.

003610 01  W20-DATE.
003620     03  W20-YY.
003630         05  W20-YEAR   PIC 9(02).
003640     03  W20-MM.
003650         05  W20-MONTH  PIC 9(02).
003660     03  W20-DD.
003670         05  W20-DAY    PIC 9(02).
003680 01  W20-DTE1 REDEFINES W20-DATE.
003690     03  W20-DTE        PIC 9(06).
002730 01  W22-DATES.
002740     03  W22-DTE1        PIC 9(06).
002750     03  W22-DTE2        PIC 9(06).
002760     03  W22-DATE2 REDEFINES W22-DTE2.
002770         05  W22-YY2     PIC 9(02).
002780         05  W22-MM2     PIC 9(02).
002790         05  W22-DD2     PIC 9(02).
002800     03  W22-DTE3        PIC 9(06).
002810     03  W22-DATE3 REDEFINES W22-DTE3.
002820         05  W22-DD3     PIC 9(02).
002830         05  W22-MM3     PIC 9(02).
002840         05  W22-YY3     PIC 9(02).
004310 01  W25-CALCS.
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

       01  W55-EXPORT.
           03  W55-S1          PIC  9(04) COMP-5.
           03  W55-EXP-DATA.
	       05  W55-EXP     PIC  X(01) OCCURS 650.

003820 01  W60-FIELDS.
           03  W60-S           PIC  9(02) COMP-5.
003830	   03  W60-FNO	       PIC  9(02) COMP-5 OCCURS 90.

003850 01  W65-FIELDS.
           03  W65-S           PIC  9(02) COMP-5.
           03  W65-FIELD       PIC  X(22).
003860     03  W65-FLDS.
      *    Field 01
003870         05  FILLER      PIC X(21) VALUE "Item Code".
      *    Field 02
003870         05  FILLER      PIC X(21) VALUE "Alternate Code".
      *    Field 03
003870         05  FILLER      PIC X(21) VALUE "Bin Location".
      *    Field 04
003870         05  FILLER      PIC X(21) VALUE "Sales Ledger".
      *    Field 05
003870         05  FILLER      PIC X(21) VALUE "Description".
      *    Field 06
003870	       05  FILLER      PIC X(21) VALUE "Description 2".
      *    Field 07
003870         05  FILLER      PIC X(21) VALUE "Quantity".
      *    Field 08
003870         05  FILLER      PIC X(21) VALUE "Re-order Level".
      *    Field 09
003870         05  FILLER      PIC X(21) VALUE "Cost Price".
      *    Field 10
	       05  FILLER      PIC X(21) VALUE "Price Per (Pack 1)".
      *    Field 11
	       05  FILLER      PIC X(21) VALUE "Units 1".
      *    Field 12
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Markup 1".
      *    Field 13
003870	       05  FILLER      PIC X(21) VALUE "Cash Markup 1".
      *    Field 14
003870	       05  FILLER      PIC X(21) VALUE "Retail Markup 1".
      *    Field 15
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Exclusive 1".
      *    Field 16
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Vat 1".
      *    Field 17
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Inclusive 1".
      *    Field 18
003870	       05  FILLER      PIC X(21) VALUE "Cash Exclusive 1".
      *    Field 19
003870	       05  FILLER      PIC X(21) VALUE "Cash Vat 1".
      *    Field 20
003870	       05  FILLER      PIC X(21) VALUE "Cash Inclusive 1".
      *    Field 21
003870	       05  FILLER      PIC X(21) VALUE "Retail Exclusive 1".
      *    Field 22
003870	       05  FILLER      PIC X(21) VALUE "Retail Vat 1".
      *    Field 23
003870	       05  FILLER      PIC X(21) VALUE "Retail Inclusive 1".
      *    Field 24
	       05  FILLER      PIC X(21) VALUE "Price Per (Pack 2)".
      *    Field 25
	       05  FILLER      PIC X(21) VALUE "Units 2".
      *    Field 26
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Markup 2".
      *    Field 27
003870	       05  FILLER      PIC X(21) VALUE "Cash Markup 2".
      *    Field 28
003870	       05  FILLER      PIC X(21) VALUE "Retail Markup 2".
      *    Field 29
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Exclusive 2".
      *    Field 30
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Vat 2".
      *    Field 31
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Inclusive 2".
      *    Field 32
003870	       05  FILLER      PIC X(21) VALUE "Cash Exclusive 2".
      *    Field 33
003870	       05  FILLER      PIC X(21) VALUE "Cash Vat 2".
      *    Field 34
003870	       05  FILLER      PIC X(21) VALUE "Cash Inclusive 2".
      *    Field 35
003870	       05  FILLER      PIC X(21) VALUE "Retail Exclusive 2".
      *    Field 36
003870	       05  FILLER      PIC X(21) VALUE "Retail Vat 2".
      *    Field 37
003870	       05  FILLER      PIC X(21) VALUE "Retail Inclusive 2".
      *    Field 38
	       05  FILLER      PIC X(21) VALUE "Price Per (Pack 3)".
      *    Field 39
	       05  FILLER      PIC X(21) VALUE "Units 3".
      *    Field 40
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Markup 3".
      *    Field 41
003870	       05  FILLER      PIC X(21) VALUE "Cash Markup 3".
      *    Field 42
003870	       05  FILLER      PIC X(21) VALUE "Retail Markup 3".
      *    Field 43
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Exclusive 3".
      *    Field 44
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Vat 3".
      *    Field 45
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Inclusive 3".
      *    Field 46
003870	       05  FILLER      PIC X(21) VALUE "Cash Exclusive 3".
      *    Field 47
003870	       05  FILLER      PIC X(21) VALUE "Cash Vat 3".
      *    Field 48
003870	       05  FILLER      PIC X(21) VALUE "Cash Inclusive 3".
      *    Field 49
003870	       05  FILLER      PIC X(21) VALUE "Retail Exclusive 3".
      *    Field 50
003870	       05  FILLER      PIC X(21) VALUE "Retail Vat 3".
      *    Field 51
003870	       05  FILLER      PIC X(21) VALUE "Retail Inclusive 3".
      *    Field 52
	       05  FILLER      PIC X(21) VALUE "Price Per (Pack 4)".
      *    Field 53
	       05  FILLER      PIC X(21) VALUE "Units 4".
      *    Field 54
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Markup 4".
      *    Field 55
003870	       05  FILLER      PIC X(21) VALUE "Cash Markup 4".
      *    Field 56
003870	       05  FILLER      PIC X(21) VALUE "Retail Markup 4".
      *    Field 57
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Exclusive 4".
      *    Field 58
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Vat 4".
      *    Field 59
003870	       05  FILLER      PIC X(21) VALUE "Wholesale Inclusive 4".
      *    Field 60
003870	       05  FILLER      PIC X(21) VALUE "Cash Exclusive 4".
      *    Field 61
003870	       05  FILLER      PIC X(21) VALUE "Cash Vat 4".
      *    Field 62
003870	       05  FILLER      PIC X(21) VALUE "Cash Inclusive 4".
      *    Field 63
003870	       05  FILLER      PIC X(21) VALUE "Retail Exclusive 4".
      *    Field 64
003870	       05  FILLER      PIC X(21) VALUE "Retail Vat 4".
      *    Field 65
003870	       05  FILLER      PIC X(21) VALUE "Retail Inclusive 4".
      *    Field 66
003870	       05  FILLER      PIC X(21) VALUE "Average Cost".
      *    Field 67
003870         05  FILLER      PIC X(21) VALUE "Quantity Ordered".
      *    Field 68
003870         05  FILLER      PIC X(21) VALUE "Lead Time".
      *    Field 69
003870         05  FILLER      PIC X(21) VALUE "Quantity MTD".
      *    Field 70
003870         05  FILLER      PIC X(21) VALUE "Cost MTD".
      *    Field 71
003870         05  FILLER      PIC X(21) VALUE "Selling MTD".
      *    Field 72
003870         05  FILLER      PIC X(21) VALUE "Quantity YTD".
      *    Field 73
003870         05  FILLER      PIC X(21) VALUE "Cost YTD".
      *    Field 74
003870         05  FILLER      PIC X(21) VALUE "Selling YTD".
      *    Field 75
003870         05  FILLER      PIC X(21) VALUE "Quantity Month 1".
      *    Field 76
003870         05  FILLER      PIC X(21) VALUE "Quantity Month 2".
      *    Field 77
003870         05  FILLER      PIC X(21) VALUE "Quantity Month 3".
      *    Field 78
003870         05  FILLER      PIC X(21) VALUE "Average Monthly Quant".
      *    Field 79
003870	       05  FILLER      PIC X(21) VALUE "Date Last Active".
      *    Field 80
003870         05  FILLER      PIC X(21) VALUE "Date Last Purchased".
      *    Field 81
003870         05  FILLER      PIC X(21) VALUE "Date Ordered".
      *    Field 82
003870         05  FILLER      PIC X(21) VALUE "Low Level Indicator".
      *    Field 83
003870         05  FILLER      PIC X(21) VALUE "Type Indicator".
      *    Field 84
003870         05  FILLER      PIC X(21) VALUE "Guarantee Period".
      *    Field 85
003870         05  FILLER      PIC X(21) VALUE "Tax Rate Code".
      *    Field 86
003870         05  FILLER      PIC X(21) VALUE "XRef Code".
      *    Field 87
003870         05  FILLER      PIC X(21) VALUE "Supplier Code".
      *    Field 88
003870	       05  FILLER      PIC X(21) VALUE "Article 11 VAT".
      *    Field 89
003870	       05  FILLER      PIC X(21) VALUE "Number of Cases".
      *    Field 90
003870	       05  FILLER      PIC X(21) VALUE "Convertion Factor".
           03  W65-RFLDS REDEFINES W65-FLDS.
	       05  W65-FLD     PIC  X(21) OCCURS 90.

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
004020         05  W80-5      PIC Z(04)9.
004030     03  W80-QNT1 REDEFINES W80-QNT.
004040         05  W80-3V2    PIC Z(02)9.9999.
004050     03  W80-NO.
004060         05  W80-5N     PIC 9(05).
004070         05  W80-REC REDEFINES W80-5N
004080                        PIC ZZZZ9.
004090     03  W80-DEL.
004100         05  W80-2N      PIC 9(02).

004110 01  W90-BAL             PIC S9(07)V99 COMP-3.

004120 01  W95-COMP            PIC X(40).

005120 01  W100-EDIT.
           03  W100-1.
               05  W100-N      PIC  9(01).
           03  W100-2.
               05  W100-N2     PIC  9(02).
           03  W100-3.
               05  W100-NO1    PIC  9(03).
           03  W100-9.
               05  W100-NO2    PIC  Z(08)9.
           03  W100-S9.
	       05  W100-NO3    PIC  -(09)9.
	   03  W100-S9Q.
	       05  W100-QNT    PIC  -(09)9.9999.
           03  W100-PRICE.
               05  W100-S6V2   PIC  Z(05)9.99.
           03  W100-S9V2.
	       05  W100-V1     PIC  -(09)9.99.
           03  W100-9V2.
               05  W100-V2     PIC  Z(08)9.99.
	   03  W100-9V3.
               05  W100-V3     PIC  Z(08)9.999.
	   03  W100-9V4.
	       05  W100-V3B    PIC  Z(08)9.9999.
           03  W100-3V2.
               05  W100-V4     PIC  Z(02)9.99.
           03  W100-DATE.
	       05  W100-DTE    PIC  9(08) BLANK WHEN ZERO.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       01  MENU-INSTRUCT.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Use ".
	   03	       COLUMN  6 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN  7 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " or ".
	   03	       COLUMN 11 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to point to an item, and ".
	   03	       COLUMN 38 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Enter>".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Select ".
	   03	       COLUMN 56 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Esc>".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Exit".

000850 01  S-PROG.
000860	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "STOCK EXPORT PROGRAM".

       01  S-MENU.
	   03  LINE  6 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
		       VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  7 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³        ".
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "EXPORT OPTIONS".
	   03	       COLUMN 48 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "        ³".
	   03  LINE  8 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 27 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 28 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Code, Description & Prices ³".
	   03  LINE  9 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 27 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 28 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " All Fields 	            ³".
001470	   03  LINE 10 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 27 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 28 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Selected Fields            ³".
001470	   03  LINE 11 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 27 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 28 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Field Separator            ³".
001470	   03  LINE 12 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 27 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 28 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Maintain Selected Items    ³".
001570	   03  LINE 13 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 27 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 28 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit                       ³".
	   03  LINE 14 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
		       VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".


       01  S02.
004210     03  LINE  2 COLUMN 31 FOREGROUND-COLOR 15 
                                 VALUE "STOCK EXPORT PROGRAM".
       01  S03.
           03  LINE  4 COLUMN  2 VALUE 
                      "Export: Item code, Description and Prices".

       01  S04.
           03  LINE  4 COLUMN  2 VALUE "Export all the fields".

       01  S05.
           03  LINE  4 COLUMN  2 VALUE "Select the fields required".
           03  LINE  6 COLUMN  2 VALUE "Field :".

       01  S06.
           03  LINE  8 COLUMN  2 VALUE "Fields:".
	   03	       COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(1).
	   03	       COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(2).
	   03	       COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(3).
	   03	       COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(4).
	   03	       COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(5).
	   03	       COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(6).
	   03	       COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(7).
	   03	       COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(8).
	   03	       COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(9).
	   03	       COLUMN 37 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(10).
	   03	       COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(11).
	   03	       COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(12).
	   03	       COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(13).
	   03	       COLUMN 49 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(14).
	   03	       COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(15).
	   03	       COLUMN 55 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(16).
	   03	       COLUMN 58 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(17).
	   03	       COLUMN 61 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(18).
	   03	       COLUMN 64 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(19).
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(20).
	   03	       COLUMN 70 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(21).
	   03	       COLUMN 73 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(22).
	   03  LINE  9 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(23).
	   03	       COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(24).
	   03	       COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(25).
	   03	       COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(26).
	   03	       COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(27).
	   03	       COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(28).
	   03	       COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(29).
	   03	       COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(30).
	   03	       COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(31).
	   03	       COLUMN 37 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(32).
	   03	       COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(33).
	   03	       COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(34).
	   03	       COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(35).
	   03	       COLUMN 49 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(36).
	   03	       COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(37).
	   03	       COLUMN 55 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(38).
	   03	       COLUMN 58 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(39).
	   03	       COLUMN 61 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(40).
	   03	       COLUMN 64 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(41).
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(42).
	   03	       COLUMN 70 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(43).
	   03	       COLUMN 73 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(44).
	   03  LINE 10 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(45).
	   03	       COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(46).
	   03	       COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(47).
	   03	       COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(48).
	   03	       COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(49).
	   03	       COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(50).
	   03	       COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(51).
	   03	       COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(52).
	   03	       COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(53).
	   03	       COLUMN 37 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(54).
	   03	       COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(55).
	   03	       COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(56).
	   03	       COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(57).
	   03	       COLUMN 49 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(58).
	   03	       COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(59).
	   03	       COLUMN 55 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(60).
	   03	       COLUMN 58 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(61).
	   03	       COLUMN 61 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(62).
	   03	       COLUMN 64 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(63).
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(64).
	   03	       COLUMN 70 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(65).
	   03	       COLUMN 73 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(66).
	   03  LINE 11 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(67).
	   03	       COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(68).
	   03	       COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(69).
	   03	       COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(70).
	   03	       COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(71).
	   03	       COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(72).
	   03	       COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(73).
	   03	       COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(74).
	   03	       COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(75).
	   03	       COLUMN 37 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(76).
	   03	       COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(77).
	   03	       COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(78).
	   03	       COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(79).
	   03	       COLUMN 49 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(80).
	   03	       COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(81).
	   03	       COLUMN 55 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(82).
	   03	       COLUMN 58 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(83).
	   03	       COLUMN 61 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(84).
	   03	       COLUMN 64 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(85).
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(86).
	   03	       COLUMN 70 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(87).
	   03	       COLUMN 73 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(88).
	   03  LINE 12 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(89).
	   03	       COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ FROM W60-FNO(90).

       01  S07.
           03  LINE 16 COLUMN  2 VALUE "Exporting:".

       01  S08.
	   03  LINE 16 COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(14) FROM STK-ITEM.
	   03	       COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  X(30) FROM STK-DESC.

       01  S08-ITM.
	   03  LINE 16 COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(14) FROM STK-ITEM.
	   03	       COLUMN 27 FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
				 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(03) FROM STK-ITM.
	   03	       COLUMN 32 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  X(30) FROM STK-DESC.

       01  S08-EXT.
	   03  LINE 16 COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  X(14) FROM STK-CODE.
	   03	       COLUMN 32 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  X(30) FROM STK-DESC.

       01  S09.
	   03  LINE  6 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC 99 FROM W65-S.
	   03	       COLUMN 13 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  X(21) FROM W65-FIELD.
       01  S10.
           03  LINE  4 COLUMN  2 VALUE "Select the Items required".
           03  LINE  6 COLUMN  2 VALUE "Item :".

005530 01  S11.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(14) USING W10-EXT-ITEM AUTO.

005530 01  S11-ITM.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(14) USING W10-ITEM AUTO.
	   03	       COLUMN 23 FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
				 VALUE "/".
	   03	       COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(03) USING W10-ITM AUTO.

005530 01  S11-EXT.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(18) USING W10-EXT-ITEM AUTO.

005530 01  S11-3.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(03) USING W10-I3 AUTO.

005530 01  S11-4.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S11-5.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S11-6.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S11-7.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S11-8.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S11-9.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S11-10.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S11-11.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S11-12.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S11-13.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S11-14.
005540	   03  LINE  6 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(14) USING W10-I14 AUTO.

       01  S12.
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Change Field Separator".
	   03  LINE  6 COLUMN  2 VALUE "Character Used:".

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN	       SECTION.
007750 AA000-INIT.
003240	   IF LS0-STLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
007760	     PERFORM ZA000-INIT.
007770     IF WS-STATUS NOT = "00"
007780         PERFORM OPEN-ERROR.
	     PERFORM BA000.

       AA49.
	     EXIT PROGRAM.

       COPY AA50.LUP.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

      *
      *    ****    G E T   T H E   E X P O R T   F I L E   I D 
      *
       AA100           SECTION.
       AA101.
             DISPLAY "Keyin the <pathname>/<filename> " AT 2202.
             DISPLAY "EXPORT file :" AT 2302.
             MOVE ZERO           TO WS-ERROR.
       AA105.
             ACCEPT W02-EXPORT AT 2316
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO AA199
		 WHEN OTHER
		     CALL X"E5"
		     GO TO AA105
	       END-EVALUATE.
	     MOVE 64		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W02-EXPORT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
             DISPLAY W02-EXPORT AT 2316
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5 UPDATE AUTO.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO AA105.
	     OPEN OUTPUT STKDET.
           IF WS-STATUS = "00"
	       PERFORM AA800
               GO TO AA199.
	     MOVE "Invalid file name"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
             GO TO AA105.
       AA199.
             EXIT.
      *
      *    ****    M O V E   F I E L D   T O   E X P O R T   A R E A
      *
      *            W50-DATA   : CONTAINS DATA TO BE INSERTED
      *            W50-LENGTH : LENGTH OF DATA 
      *            W50-EXP    : FIELD TERMINATION CHARACTER
      *
       AA200           SECTION.
       AA201.
           IF W50-DATA = SPACES
               GO TO AA225.
             MOVE W50-LENGTH     TO W50-S1.
      *
      *    ****    D E T E R M I N E   E N D   O F   F I E L D
      *
      *            THIS LOOP EXCLUDES THE TRAILING SPACES
      *
       AA205.
	   IF W50-CCHAR(W50-S1) = SPACE
               SUBTRACT 1        FROM W50-S1
               GO TO AA205.
      *
      *    ****    R E M O V E   L E A D I N G   S P A C E S
      *
       AA210.
           IF W50-1 = SPACE
               MOVE W50-D39      TO W50-DATA
               SUBTRACT 1        FROM W50-S1
               GO TO AA210.
             MOVE W50-S1         TO W50-S2.     
             MOVE 1              TO W50-S1.
       AA220.
	     MOVE W50-CCHAR(W50-S1)
				 TO W55-EXP(W55-S1).
             ADD 1               TO W50-S1
                                    W55-S1.
           IF W50-S1 NOT > W50-S2
               GO TO AA220.
       AA225.
	     MOVE W50-EXP	 TO W55-EXP(W55-S1).
             ADD 1               TO W55-S1.
       AA299.
             EXIT.
      *
      *    ****    M O V E   I N D V I D U A L   F I E L D S
      *
      *            W50-BIN : FIELD NUMBER TO MOVE
      *
       AA300              SECTION.
       AA301.
	   IF W50-BIN < 01 OR > 90
               GO TO AA399.
      *
           IF W50-BIN = 01
               MOVE STK-CODE     TO W50-DATA
	       MOVE 18		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 02
               MOVE STK-ACODE    TO W50-DATA
               MOVE 10           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 03
               MOVE STK-BIN      TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 04
	       MOVE STK-LDG	 TO W50-DATA
	       MOVE 4		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 05
               MOVE STK-DESC     TO W50-DATA
               MOVE 30           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 06
	       MOVE STK-DESC2	 TO W50-DATA
               MOVE 30           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 07
	       MOVE STK-QUANT	 TO W100-QNT
	       MOVE W100-S9Q	 TO W50-DATA
	       MOVE 15		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 08
               MOVE STK-LEVEL    TO W100-NO2
               MOVE W100-9       TO W50-DATA
               MOVE 9            TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 09
               MOVE STK-COST     TO W100-V3
               MOVE W100-9V3     TO W50-DATA
               MOVE 13           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 10
	       MOVE STK-PR(1)	 TO W50-DATA
	       MOVE 8		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 11
	       MOVE STK-UNT(1)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 12
	       MOVE STK-WMKUP(1) TO W100-V4
               MOVE W100-3V2     TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 13
	       MOVE STK-CMKUP(1) TO W100-V4
	       MOVE W100-3V2	 TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 14
	       MOVE STK-MKUP(1)  TO W100-V4
	       MOVE W100-3V2	 TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
	       PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 15
	       MOVE STK-WSL(1)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 16
	       MOVE W10-WVAT(1)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
	       MOVE 12		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 17
	       MOVE W10-VWSL(1)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 18
	       MOVE STK-CSH(1)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 19
	       MOVE W10-CVAT(1)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
               MOVE 12           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 20
	       MOVE W10-VCSH(1)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 21
	       MOVE STK-SL(1)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 22
	       MOVE W10-SVAT(1)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
	       MOVE 12		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 23
	       MOVE W10-VSL(1)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 24
	       MOVE STK-PR(2)	 TO W50-DATA
	       MOVE 8		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 25
	       MOVE STK-UNT(2)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 26
	       MOVE STK-WMKUP(2) TO W100-V4
               MOVE W100-3V2     TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 27
	       MOVE STK-CMKUP(2) TO W100-V4
	       MOVE W100-3V2	 TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 28
	       MOVE STK-MKUP(2)  TO W100-V4
	       MOVE W100-3V2	 TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
	       PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 29
	       MOVE STK-WSL(2)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 30
	       MOVE W10-WVAT(2)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
	       MOVE 12		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 31
	       MOVE W10-VWSL(2)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 32
	       MOVE STK-CSH(2)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 33
	       MOVE W10-CVAT(2)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
               MOVE 12           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 34
	       MOVE W10-VCSH(2)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 35
	       MOVE STK-SL(2)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 36
	       MOVE W10-SVAT(2)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
	       MOVE 12		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 37
	       MOVE W10-VSL(2)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 38
	       MOVE STK-PR(3)	 TO W50-DATA
	       MOVE 8		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 39
	       MOVE STK-UNT(3)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 40
	       MOVE STK-WMKUP(3) TO W100-V4
               MOVE W100-3V2     TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 41
	       MOVE STK-CMKUP(3) TO W100-V4
	       MOVE W100-3V2	 TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 42
	       MOVE STK-MKUP(3)  TO W100-V4
	       MOVE W100-3V2	 TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
	       PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 43
	       MOVE STK-WSL(3)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 44
	       MOVE W10-WVAT(3)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
	       MOVE 12		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 45
	       MOVE W10-VWSL(3)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 46
	       MOVE STK-CSH(3)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 47
	       MOVE W10-CVAT(3)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
               MOVE 12           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 48
	       MOVE W10-VCSH(3)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 49
	       MOVE STK-SL(3)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 50
	       MOVE W10-SVAT(3)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
	       MOVE 12		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 51
	       MOVE W10-VSL(3)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 52
	       MOVE STK-PR(4)	 TO W50-DATA
	       MOVE 8		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 53
	       MOVE STK-UNT(4)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 54
	       MOVE STK-WMKUP(4) TO W100-V4
               MOVE W100-3V2     TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 55
	       MOVE STK-CMKUP(4) TO W100-V4
	       MOVE W100-3V2	 TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 56
	       MOVE STK-MKUP(4)  TO W100-V4
	       MOVE W100-3V2	 TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
	       PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 57
	       MOVE STK-WSL(4)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 58
	       MOVE W10-WVAT(4)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
	       MOVE 12		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 59
	       MOVE W10-VWSL(4)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 60
	       MOVE STK-CSH(4)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 61
	       MOVE W10-CVAT(4)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
               MOVE 12           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 62
	       MOVE W10-VCSH(4)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 63
	       MOVE STK-SL(4)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 64
	       MOVE W10-SVAT(4)  TO W100-V2
	       MOVE W100-9V2	 TO W50-DATA
	       MOVE 12		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 65
	       MOVE W10-VSL(4)	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 66
	       MOVE STK-AVGE	 TO W100-V3B
	       MOVE W100-9V4	 TO W50-DATA
	       MOVE 14		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 67
               MOVE STK-ORD      TO W100-NO2
               MOVE W100-9       TO W50-DATA
               MOVE 9            TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 68
               MOVE STK-LTIME    TO W100-V4
               MOVE W100-3V2     TO W50-DATA
               MOVE 6            TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 69
               MOVE STK-MTD      TO W100-NO3
               MOVE W100-S9      TO W50-DATA
               MOVE 10           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 70
               MOVE STK-MTDC     TO W100-V1
               MOVE W100-S9V2    TO W50-DATA
               MOVE 13           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 71
               MOVE STK-MTDV     TO W100-V1
               MOVE W100-S9V2    TO W50-DATA
               MOVE 13           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 72
               MOVE STK-YTD      TO W100-NO3
               MOVE W100-S9      TO W50-DATA
               MOVE 10           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 73
               MOVE STK-YTDC     TO W100-V1
               MOVE W100-S9V2    TO W50-DATA
               MOVE 13           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 74
               MOVE STK-YTDV     TO W100-V1
               MOVE W100-S9V2    TO W50-DATA
               MOVE 13           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 75
               MOVE STK-MTH1     TO W100-NO3
               MOVE W100-S9      TO W50-DATA
               MOVE 10           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 76
               MOVE STK-MTH2     TO W100-NO3
               MOVE W100-S9      TO W50-DATA
               MOVE 10           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 77
               MOVE STK-MTH3     TO W100-NO3
               MOVE W100-S9      TO W50-DATA
               MOVE 10           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 78
               MOVE STK-MAVG     TO W100-NO3
               MOVE W100-S9      TO W50-DATA
               MOVE 10           TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 79
               MOVE STK-ACT      TO W100-DTE
               MOVE W100-DATE    TO W50-DATA
	       MOVE 8		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 80
               MOVE STK-PUR      TO W100-DTE
               MOVE W100-DATE    TO W50-DATA
	       MOVE 8		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 81
               MOVE STK-DTE      TO W100-DTE
               MOVE W100-DATE    TO W50-DATA
	       MOVE 8		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 82
               MOVE STK-DISC     TO W50-DATA
               MOVE 1            TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 83
               MOVE STK-IND      TO W100-N
               MOVE W100-1       TO W50-DATA
               MOVE 1            TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 84
               MOVE STK-GUAR     TO W100-N2
               MOVE W100-2      TO W50-DATA
               MOVE 2            TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 85
               MOVE STK-TAX      TO W100-N
               MOVE W100-1       TO W50-DATA
               MOVE 1            TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 86
               MOVE STK-XREF     TO W50-DATA
	       MOVE 18		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 87
               MOVE STK-SUPP     TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
	       PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 88
	       MOVE STK-EXEMPT	 TO W50-DATA
	       MOVE 1		 TO W50-LENGTH
	       PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 89
	       MOVE STK-CASES	 TO W100-NO2
	       MOVE W100-9	 TO W50-DATA
	       MOVE 9		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 90
	       MOVE STK-FACT	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200.

       AA399.
               EXIT.
      *
      *    ****    P R O C E S S   T H E   S E L E C T E D   F I E L D S
      *
       AA400           SECTION.
       AA401.
             MOVE 1              TO W60-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
       AA405.
	   IF W60-FNO(W60-S) = ZERO
               GO TO AA499.
	     MOVE W60-FNO(W60-S) TO W50-BIN.
             PERFORM AA300.
	   IF W60-S < 90
               ADD 1             TO W60-S
               GO TO AA405.
       AA499.
             EXIT.
      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
       AA500              SECTION.
       AA501.
023190       DISPLAY "Insufficient security" AT 0412
                      WITH FOREGROUND-COLOR 14.
023200       DISPLAY "Press ENTER to continue" AT 0612
                      WITH FOREGROUND-COLOR 14.
023210	     ACCEPT WS-OPTION AT 0636
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
       AA505.
             EXIT.
      *
      *    ****    Calculate the inclusive prices
      *
       AA600         SECTION.
      *
      *    ****    U N I T S   A N D   M A R K U P S
      *
       AA601.
	     MOVE 1		 TO WS-S1.

       AA602.
	     MOVE ZERO		 TO W10-VSL(WS-S1)
				    W10-VCSH(WS-S1)
				    W10-VWSL(WS-S1)
				    W10-SVAT(WS-S1)
				    W10-CVAT(WS-S1)
				    W10-WVAT(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO AA602.
	     MOVE 1		 TO WS-S1.

       AA605.
           IF STK-WSALE = ZERO
               IF STK-CASH NOT = ZERO
                   MOVE STK-CASH TO STK-WSALE
               ELSE
                   MOVE STK-SELL TO STK-WSALE.
015630     IF STK-CASH = ZERO
015640         MOVE STK-SELL     TO STK-CASH.
	     MOVE ZERO		 TO W05-RATE.
	   IF STK-TAX > 0
	       MOVE W05-VAT(STK-TAX)
                                 TO W05-RATE.

       AA610.
	     COMPUTE W10-VCSH(WS-S1) ROUNDED
		   = STK-CSH(WS-S1) + (STK-CSH(WS-S1) * W05-RTE).
	     COMPUTE W10-VWSL(WS-S1) ROUNDED
		   = STK-WSL(WS-S1) + (STK-WSL(WS-S1) * W05-RTE).
	     COMPUTE W10-VSL(WS-S1) ROUNDED
		   = STK-SL(WS-S1) + (STK-SL(WS-S1) * W05-RTE).
	     COMPUTE W10-SVAT(WS-S1) = W10-VSL(WS-S1) - STK-SL(WS-S1).
	     COMPUTE W10-CVAT(WS-S1) = W10-VCSH(WS-S1) - STK-CSH(WS-S1).
	     COMPUTE W10-WVAT(WS-S1) = W10-VWSL(WS-S1) - STK-WSL(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO AA610.
	   IF STK-IND < 2
016950	       IF STK-AVGE = ZERO
016960		   MOVE STK-COST TO STK-AVGE.

015650 AA699.
015660       EXIT.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
       AA700              SECTION.
       AA701.
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
011060		     " Desc/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     " Desc2/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011080		     " X/Ref Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA705.
             EXIT.
      *
      *    ****   H E A D E R S
      *
       AA800		  SECTION.
       AA801.
	     MOVE 1		 TO W60-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
       AA805.
	   IF W60-FNO(W60-S) = ZERO
	       MOVE W55-EXP-DATA TO EXP-REC1
	       PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT
	       GO TO AA899.
	     MOVE W60-FNO(W60-S) TO W50-BIN.
	     MOVE 21		 TO W50-LENGTH.
	     MOVE W65-FLD(W50-BIN)
				 TO W50-DATA.
	     PERFORM AA200.
	   IF W60-S < 90
               ADD 1             TO W60-S
	       GO TO AA805.
       AA899.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY STOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       WRITE-STKDET.
000250       WRITE EXP-REC1.
000260     IF WS-STATUS NOT = "00"
000270         MOVE 99           TO WS-F-ERROR
000280         PERFORM WRITE-ERROR.
000310
000320 WRITE-STKDET-EXIT.
000330       EXIT.

       COPY APAC.HLP.

      /
      *    ****    P R O G R A M   M E N U / C O N T R O L
      *
       BA000           SECTION.
       BA00.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	   IF LS0-LEV < 3
	       DISPLAY "(N/A)" AT 1045
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
	       DISPLAY "N/A" AT 1253
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       BA02.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 12		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 27		 TO SCREEN-COL.
	     MOVE 28		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 5
		   CALL X"E5"
		   GO TO BA05
	       END-IF
	       GO TO BA16
	   ELSE
	       CALL X"E5"
	       GO TO BA05.

       BA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO BA05
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 11		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA18.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
012640	   IF WS-OPTION = "0"
012650         GO TO BA999.
012660     IF WS-OPTION = "1"
012670         PERFORM DA000 
012680	   ELSE
012690     IF WS-OPTION = "2"
012700         PERFORM EA000 
012710	   ELSE
012720	   IF WS-OPTION = "3" AND
	      LS0-LEV > 2
012730	       PERFORM FA000
012740	   ELSE
           IF WS-OPTION = "4"
	       PERFORM HA000
012740	   ELSE
	   IF WS-OPTION = "5" AND
	      LS0-LEV > 2
	       PERFORM GA000.
	     GO TO BA00.
       BA999.
             EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

      /
      *    ****    E X P O R T :    (PACK 1)
      *
      *                I T E M   C O D E
      *                D E S C R I P T I O N
      *                D I S T R I B U T O R   P R I C E 
      *                       E X C L U S I V E
      *                       V A T
      *                       I N C L U S I V E
      *                D E A L E R   P R I C E
      *                       E X C L U S I V E
      *                       V A T
      *                       I N C L U S I V E
      *                B U S I N E S S   P R I C E
      *                       E X C L U S I V E
      *                       V A T
      *                       I N C L U S I V E
      *
       DA000	       SECTION 5.
       DA00.
             DISPLAY CLR-SCREEN.
             DISPLAY S02.
             DISPLAY S03.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO DA999.
	     INITIALIZE W60-FIELDS.
	     MOVE 01		 TO W60-FNO(1).
	     MOVE 05		 TO W60-FNO(2).
	     MOVE 15		 TO W60-FNO(3).
	     MOVE 16		 TO W60-FNO(4).
	     MOVE 17		 TO W60-FNO(5).
	     MOVE 18		 TO W60-FNO(6).
	     MOVE 19		 TO W60-FNO(7).
	     MOVE 20		 TO W60-FNO(8).
	     MOVE 21		 TO W60-FNO(9).
	     MOVE 22		 TO W60-FNO(10).
	     MOVE 23		 TO W60-FNO(11).
             DISPLAY S06.
             PERFORM AA100.
       DA02.
	     MOVE "'L'edger,'I'tem sequence,'S'eleted items  [I]"
				 TO WS-ERR-MES.
	     MOVE "I"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "I" OR "L" OR "S")
               GO TO DA02.
	   IF WS-OPTION = "S"
               GO TO DA10.
             DISPLAY S07.
             MOVE "!"            TO STK-CODE.
           IF WS-OPTION = "I"
               PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT
           ELSE
	       MOVE "!"		 TO STK-LDG
009360         PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO DA900.

       DA05.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO DA900.
	   IF STK-IND = 4
	       GO TO DA05.
      *    ****    A V E R A G E   C O S T
	   IF STK-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
	   IF WS-USE-ITM = "Y"
	       DISPLAY S08-ITM
	   ELSE
	   IF WS-EXT-STK = "Y"
	       DISPLAY S08-EXT
	   ELSE
	       DISPLAY S08.
             PERFORM AA600.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
             PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT.
             GO TO DA05.
       DA10.
             OPEN I-O STKSEL.
           IF WS-STATUS NOT = "00"
               DISPLAY W02-STKSEL AT 2302 WITH FOREGROUND-COLOR 14
                       " has not been created!" with FOREGROUND-COLOR 7
               GO TO DA02.
             MOVE 1              TO WS-SELKEY.
       DA15.
             READ STKSEL.
           IF WS-STATUS NOT = "00"
               CLOSE STKSEL
               GO TO DA900.
           IF SEL-CODE = SPACES
               GO TO DA20.
             MOVE SEL-CODE       TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO DA15.
	   IF STK-IND = 4
	       GO TO DA15.
      *    ****    A V E R A G E   C O S T
	   IF STK-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
	   IF WS-USE-ITM = "Y"
	       DISPLAY S08-ITM
	   ELSE
	   IF WS-EXT-STK = "Y"
	       DISPLAY S08-EXT
	   ELSE
	       DISPLAY S08.
	     PERFORM AA600.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
             PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT.
       DA20.
             ADD 1               TO WS-SELKEY.
             GO TO DA15.
       DA900.
	     CLOSE STKDET.

       DA999.
             EXIT.
      /
      *    ****    E X P O R T :
      *
      *                A L L   T H E   F I E L D S
      *
       EA000	       SECTION 5.
       EA00.
             DISPLAY CLR-SCREEN.
             DISPLAY S02.
             DISPLAY S04.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO EA999.
	     INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.
       EA05.
	     MOVE W60-S 	 TO W60-FNO(W60-S).
	   IF W60-S < 90
               ADD 1             TO W60-S
               GO TO EA05.
             DISPLAY S06.
             PERFORM AA100.
       EA06.
	     MOVE "'S'ales ledger or 'I'tem sequence  [I]"
				 TO WS-ERR-MES.
	     MOVE "I"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT(WS-OPTION = "I" OR "S")
               GO TO EA06.
             DISPLAY S07.
             MOVE "!"            TO STK-CODE.
           IF WS-OPTION = "I"
               PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT
           ELSE
	       MOVE "!" 	 TO STK-LDG
009360         PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO EA900.
       EA10.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO EA900.
	   IF STK-IND = 4
	       GO TO EA10.
      *    ****    A V E R A G E   C O S T
	   IF STK-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
	   IF WS-USE-ITM = "Y"
	       DISPLAY S08-ITM
	   ELSE
	   IF WS-EXT-STK = "Y"
	       DISPLAY S08-EXT
	   ELSE
	       DISPLAY S08.
	     PERFORM AA600.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
             PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT.
             GO TO EA10.
       EA900.
             CLOSE STKDET.
       EA999.
             EXIT.
      /
      *    ****    E X P O R T :
      *
      *                S E L E C T E D   F I E L D S
      *
       FA000	       SECTION 5.
       FA05.
             DISPLAY CLR-SCREEN.
             DISPLAY S02.
             DISPLAY S05.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO FA999.
	     INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.
001020	     DISPLAY " " AT 1803 WITH FOREGROUND-COLOR 14
                     " to view the fields available " 
                      WITH FOREGROUND-COLOR 7
                     "S" WITH FOREGROUND-COLOR 14
                     "elect/" WITH FOREGROUND-COLOR 7
                     "A" WITH FOREGROUND-COLOR 14
		     "ccept - " WITH FOREGROUND-COLOR 7
                     "Esc" WITH FOREGROUND-COLOR 14
                     "ape" WITH FOREGROUND-COLOR 7.
             MOVE 1              TO W65-S.
       FA10.
	     MOVE W65-FLD(W65-S) TO W65-FIELD.
             DISPLAY S09.
       FA15.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY SPACE AT 2541
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF W65-S = 1
			 GO TO FA15
		     ELSE
			 SUBTRACT 1  FROM W65-S
			 GO TO FA10
		     END-IF
		 WHEN DOWN-KEY
		     IF W65-S NOT < 90
			 GO TO FA15
		     ELSE
			 ADD 1	 TO W65-S
			 GO TO FA10
		     END-IF
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO FA18
		 WHEN OTHER
		     CALL X"E5"
	       END-EVALUATE
	       GO TO FA15
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO FA05
		 WHEN OTHER
		     CALL X"E5"
	       END-EVALUATE
	       GO TO FA15
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
           IF NOT (WS-OPTION = "A" OR "S")
               GO TO FA15.
           IF WS-OPTION = "A"
               GO TO FA30.
       FA18.
             MOVE 1              TO WS-S1.
       FA20.
           IF WS-S1 = W60-S
               GO TO FA25.
	   IF W60-FNO(WS-S1) = W65-S
               GO TO FA15.
             ADD 1               TO WS-S1.
             GO TO FA20.
       FA25.
	     MOVE W65-S 	 TO W60-FNO(W60-S).
             DISPLAY S06.
             ADD 1               TO W60-S.
	   IF W65-S < 90
               ADD 1             TO W65-S.
             GO TO FA10.
       FA30.
             PERFORM AA100.
             DISPLAY S07.
       FA32.
	     MOVE "'S'ales ledger or 'I'tem sequence  [I]"
				 TO WS-ERR-MES.
	     MOVE "I"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "I" OR "S")
               GO TO FA32.
             MOVE "!"            TO STK-CODE.
           IF WS-OPTION = "I"
               PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT
           ELSE
	       MOVE "!" 	 TO STK-LDG
009360         PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO FA900.
       FA35.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO FA900.
	   IF STK-IND = 4
	       GO TO FA35.
      *    ****    A V E R A G E   C O S T
	   IF STK-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
	   IF WS-USE-ITM = "Y"
	       DISPLAY S08-ITM
	   ELSE
	   IF WS-EXT-STK = "Y"
	       DISPLAY S08-EXT
	   ELSE
	       DISPLAY S08.
	     PERFORM AA600.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
             PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT.
             GO TO FA35.
       FA900.
             CLOSE STKDET.
       FA999.
             EXIT.
      *
      *    ****    SELECT ITEMS TO EXTRACT
      *
       GA000	       SECTION 5.
       GA00.
             DISPLAY CLR-SCREEN.
             DISPLAY S02.
       GA05.
	     MOVE "'A'mend existing, 'C'reate new file  [C]"
				 TO WS-ERR-MES.
             MOVE "C"            TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "A" OR "C")
               GO TO GA05.
             MOVE WS-OPTION      TO WS-SKIP.
	     DISPLAY S10.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO GA999.
	   IF WS-SKIP = "C"
               OPEN OUTPUT STKSEL
               CLOSE STKSEL.
             OPEN I-O STKSEL.
             MOVE 1              TO WS-SELKEY.
           IF WS-SKIP = "A"
               GO TO GA25.
       GA10.
	     DISPLAY WS-BLNK78 AT 0602 WITH FOREGROUND-COLOR 14.
             DISPLAY S10.
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
       GA15.
             PERFORM AA700.
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   ACCEPT S11-EXT
	       ELSE
	       IF WS-USE-ITM = "Y"
		   ACCEPT S11-ITM
	       ELSE
021070		   ACCEPT S11
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S11-3
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S11-4
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S11-5
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S11-6
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S11-7
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S11-8
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S11-9
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S11-10
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S11-11
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S11-12
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S11-13
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S11-14.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-CODE2
		     GO TO GA25
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO GA15
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
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GA15
	       END-EVALUATE
	       PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S11-EXT
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S11-ITM
		   ELSE
		       DISPLAY S11
		   END-IF
	       ELSE
		   DISPLAY S11
	       END-IF
	       IF W10-EXT-ITEM = SPACES
                   GO TO GA15.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
019490	   IF W10-EXT-ITEM = SPACE
019500         GO TO GA99.
013310	     MOVE 18		 TO W50-LENGTH.
       GA20.
013320	     MOVE W10-EXT-ITEM	  TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-17	 TO W10-EXT-ITEM
               GO TO GA20.      
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
013380	     MOVE W50-EXT-ITEM	 TO W10-EXT-ITEM
013380                              STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
019520     IF WS-F-ERROR = 22
019530	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019540         GO TO GA15.
	     DISPLAY STK-DESC AT 0624
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GA15.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO GA15.
	     MOVE STK-CODE	 TO SEL-CODE.
             WRITE SEL-REC.
             ADD 1               TO WS-SELKEY.
             GO TO GA10.
       GA25.
             READ STKSEL.
           IF WS-STATUS NOT = "00"
               MOVE 1            TO WS-IND
               GO TO GA10.
             MOVE WS-SELKEY      TO WS-KEYSTR.
	     DISPLAY SEL-CODE AT 0609
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5.
           IF SEL-CODE = SPACES
	       DISPLAY "No details" AT 0624 WITH FOREGROUND-COLOR 14
               GO TO GA30.
             MOVE SEL-CODE       TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               DISPLAY "Invalid item code" AT 0624
                        WITH FOREGROUND-COLOR 14
               GO TO GA30.
	     DISPLAY STK-DESC AT 0624
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
       GA30.
001020       DISPLAY X"182019" AT 2303 WITH FOREGROUND-COLOR 14
                     " view selection, " WITH FOREGROUND-COLOR 7
                     "A" WITH FOREGROUND-COLOR 14
                     "mend/" WITH FOREGROUND-COLOR 7
                     "D" WITH FOREGROUND-COLOR 14
                     "elete item, " WITH FOREGROUND-COLOR 7
                     "E" WITH FOREGROUND-COLOR 14
		     "nd" WITH FOREGROUND-COLOR 7.

       GA35.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF WS-SELKEY = 1
			 GO TO GA35
		     ELSE
			 SUBTRACT 1  FROM WS-SELKEY
			 GO TO GA25
		     END-IF
		 WHEN DOWN-KEY
		     IF WS-SELKEY NOT < WS-KEYSTR
			 IF WS-IND = 1
			     GO TO GA35
			 END-IF
			 ADD 1	  TO WS-SELKEY
			 GO TO GA25
		     END-IF
		     CALL X"E5"
	       END-EVALUATE
	       GO TO GA35
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO GA99
		 WHEN OTHER
		     CALL X"E5"
	       END-EVALUATE
	       GO TO GA35
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
           IF NOT (WS-OPTION = "A" OR "D")
               GO TO GA35.
           IF WS-OPTION = "A"
               GO TO GA40.
             MOVE SPACES         TO SEL-CODE
             GO TO GA55.
       GA40.
019460	     MOVE SEL-CODE	 TO W10-EXT-ITEM.
       GA45.
             PERFORM AA700.
021070       ACCEPT S11.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-CODE2
		     GO TO GA25
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO GA45
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
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GA45
	       END-EVALUATE
	       PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S11-EXT
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S11-ITM
		   ELSE
		       DISPLAY S11
		   END-IF
	       ELSE
		   DISPLAY S11
	       END-IF
	       IF W10-EXT-ITEM = SPACES
                   GO TO GA45.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
019490	   IF W10-EXT-ITEM = SPACE
019500         GO TO GA25.
013310	     MOVE 18		 TO W50-LENGTH.
       GA50.
013320	     MOVE W10-EXT-ITEM	  TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-17	 TO W10-EXT-ITEM
               GO TO GA50.      
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
013380	     MOVE W50-EXT-ITEM	 TO W10-EXT-ITEM
013380                              STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
019520     IF WS-F-ERROR = 22
019530	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019540         GO TO GA45.
	     DISPLAY STK-DESC AT 0624
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GA45.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO GA45.
	     MOVE STK-CODE	 TO SEL-CODE.
       GA55.
             REWRITE SEL-REC.
             ADD 1               TO WS-SELKEY.
             GO TO GA25.
       GA99.
             CLOSE STKSEL.
       GA999.
             EXIT.      
      /
      *    ****    F I E L D   S E P A R A T O R
      *
       HA000	       SECTION 5.
       HA00.
             DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     DISPLAY S12.
       HA05.
	     ACCEPT W50-EXP AT 0618
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
       HA999.
	     EXIT.

      /
019370 ZA000-INIT	       SECTION 8.
019380 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
019390	     DISPLAY CLR-SCREEN.
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
	     MOVE "STKSEL"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-STKSEL.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
019480	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
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
019820	     MOVE 1		 TO WS-PARKEY.
019830       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019840       MOVE PAR-DMY        TO W12-TODAY.
019850	     MOVE PAR-COMPANY	 TO W95-COMP.
       ZA05.
             MOVE ","            TO W50-EXP.
019900       GO TO ZA999-EXIT.
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
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
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
019910 ZA200.
019920       DISPLAY "Files locked - Try later" AT 2312
019930                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
019940       DISPLAY "Press any key" AT 2512 
019950		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
019960	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
019990 ZA999-EXIT.
020000       EXIT.
      /
020010 ZB000-ERROR	       SECTION 8.
      
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
      
041560 DISPLAY-FILE-NAME.
	   IF WS-F-ERROR = 7
	       MOVE W02-DEPART	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
	       MOVE DPT-CODE	 TO WS-KEYX
	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 99
041860         MOVE W02-STKDET   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY.
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
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
