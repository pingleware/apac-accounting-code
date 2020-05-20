      $set LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	   ******     ****     *
      *   **	**  **	  **  **    **	 **  **   **	**   **  **    *
      *   **	    **	  **  **    **	**    **       **   **	  **   *
      *   **	    ******    *******	**    **     **     **	  **   *
      *   **	    **	 **   **	**    **   **	    **	  **   *
      *   **	**  **	  **  **	 **  **   **	     **  **    *
      *    ******   **	  **  **	  ****	  ********    ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *       P R I C E   L A B E L S	V I A	I N V O I C I N G      *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	     CRP020.
000030 AUTHOR.               J.W. LEMMON (APAC).
000040 DATE-WRITTEN.         AUGUST, 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2011
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

000070 ENVIRONMENT DIVISION.
000080 CONFIGURATION SECTION.
000090 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000110 INPUT-OUTPUT SECTION.
000120 FILE-CONTROL.

       COPY APACFIDS.SL.

000140 COPY CONTROL.SL.

       COPY DEPART.SL.

000140 COPY PARAM.SL.

000300 COPY PRCLAB.SL.

000220 COPY STOCK.SL.

000290 DATA DIVISION.
000300 FILE SECTION.

       COPY APACFIDS.FDE.

000140 COPY CONTROL.FDE.

       COPY DEPART.FDE.

000320 COPY PARAM.FDE.

000300 COPY PRCLAB.FD.

000400 COPY STOCK.FDE.

      /
002980 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002990 77  WS-STKKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003000 77  WS-NETKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003030 77  WS-PRCKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003030 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003380 77  WS-KEYSTR	   PIC	9(04)	 COMP-5.
003060 77  WS-ERROR	   PIC	9(01)	 VALUE 0.
003080 77  WS-S 	   PIC	9(04)	 COMP-5.
003090 77  WS-S1	   PIC	9(04)	 COMP-5.
003100 77  WS-S2	   PIC	9(04)	 COMP-5.
003110 77  WS-S3	   PIC	9(04)	 COMP-5.
003120 77  WS-S4	   PIC	9(04)	 COMP-5.
003120 77  WS-S5	   PIC	9(04)	 COMP-5.
003120 77  WS-S6	   PIC	9(04)	 COMP-5.
003120 77  WS-S7	   PIC	9(04)	 COMP-5.
003120 77  WS-S8	   PIC	9(04)	 COMP-5.
003130 77  WS-IXS1	   PIC	9(04)	 COMP-5.
003140 77  WS-IXS2	   PIC	9(04)	 COMP-5.
003150 77  WS-IXS3	   PIC	9(04)	 COMP-5.
003160 77  WS-IXS4	   PIC	9(04)	 COMP-5.
003170 77  WS-KEY1	   PIC	9(04)	 COMP-5.
003180 77  WS-CHAR1	   PIC	X(01).
003200 77  WS-ORD	   PIC	X(01).
003200 77  WS-SEQ	   PIC	X(01).
003210 77  WS-OPTION	   PIC	X(01).
003330 77  WS-LAB	   PIC	X(01).
003330 77  WS-PRICE        PIC  X(01).
       77  WS-LABCDE	   PIC	X(08).
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
003240 77  WS-LINES	   PIC	9(04)	 COMP-5 VALUE 0.
003250 77  WS-PAGE	   PIC	9(04)	 COMP-5 VALUE 0.
003260 77  WS-IND	   PIC	9(01).
003330 77  WS-H1	   PIC	X(05)	 VALUE "DATE:".
003340 77  WS-H3	   PIC	X(05)	 VALUE "PAGE:".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

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
002450     03  WS-MID-LNE2.
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
002430     03  WS-TP-LNE.
002440         05  WS-TCR  PIC X(01) OCCURS 80.
002450     03  WS-MD-LNE.
002460         05  WS-MCR  PIC X(01) OCCURS 80.
002470     03  WS-BT-LNE.
002480         05  WS-BCR  PIC X(01) OCCURS 80.
      /

003360 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003380 01  W02-FID.

       COPY APACFIDS.ID.

000340 COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000400 COPY STOCK.ID.

       COPY PRCLAB.ID.

       01  W02-PRINTER-DETAILS.
	   03  W02-PRINTER     PIC  X(12).
002570	   03  W02-PGE-LENGTH  PIC  9(02).
001870	   03  W02-PRN-LENGTH  PIC  9(02).
	   03  W02-LINAGE      PIC  9(02).
	   03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      * 	   C  =  Condensed print
      * 	   E  =  Expanded print
      * 	   H  =  Header line
      * 	   X  =  Cancel expanded print
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  W02-PRN-TYPE    PIC  X(01).
	   03  W02-PRN-LINE    PIC  X(136).

	   03  R-SL1 REDEFINES W02-PRN-LINE.
	       05  R-SLA       PIC  X(78).
	       05  FILLER      PIC  X(58).
	   03  R-SL2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(58).
	       05  R-SLB       PIC  X(78).
000390	   03  R-L1 REDEFINES W02-PRN-LINE.
000400	       05  REP-DETAIL1 PIC  X(140).
000410	   03  REP-LINE2 REDEFINES W02-PRN-LINE.
000420	       05  REP-DETAIL2 PIC  X(80).
	   03  LAB-L1 REDEFINES W02-PRN-LINE.
	       05  LAB-HEAD    PIC  X(06).
	       05  LAB-DESC    PIC  X(74).
	   03  LAB-L2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(03).
	       05  LAB-ITEM    PIC  X(14).
	       05  FILLER      PIC  X(63).
	   03  LAB-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(06).
	       05  LAB-COST    PIC  X(08).
	       05  FILLER      PIC  X(66).
	   03  LAB-L4 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(03).
	       05  LAB-SELL    PIC  Z(04)9.99 BLANK WHEN ZERO.
	       05  FILLER      PIC  X(69).
	   03  LB2-L1 REDEFINES W02-PRN-LINE.
	       05  LB2-DET OCCURS 3.
		   07  LB2-DATA.
		       09  LB2-H   PIC	X(16).
		       09  LB2-PRC PIC	ZZZZ9.99 BLANK WHEN ZERO.
		   07  FILLER	   PIC	X(04).
	   03  LB3-L1 REDEFINES W02-PRN-LINE.
	       05  LB3-DET OCCURS 4.
		   07  LB3-DATA.
		       09  LB3-H   PIC	X(10).
		       09  LB3-PRC PIC	ZZZZZZ9.99 BLANK WHEN ZERO.
		       09  FILLER  PIC	X(01).
		   07  FILLER	   PIC	X(04).
	   03  LB4-L1 REDEFINES W02-PRN-LINE.
	       05  LB4-DET OCCURS 3.
		   07  LB4-DATA.
		       09  LB4-CODE
				   PIC	X(14).
		   07  FILLER	   PIC	X(03).
	   03  LB4-L2 REDEFINES W02-PRN-LINE.
	       05  LB4-DATA2 OCCURS 3.
		   07  LB4-DESC    PIC	X(32).
		   07  FILLER	   PIC	X(02).
	   03  LB4-L3 REDEFINES W02-PRN-LINE.
	       05  LB4-DATA3 OCCURS 3.
		   07  LB4-H	   PIC	X(13).
		   07  LB4-LABCDE.
		       09  LB4-PRC PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(03).
		   07  LB4-DTE	   PIC	9(06).
		   07  FILLER	   PIC	X(04).
	   03  SAPDC-L1 REDEFINES W02-PRN-LINE.
	       05  SAPDC-DET.
		   07  SAPDC-CDE1  PIC	X(14).
		   07  FILLER	   PIC	X(04).
		   07  SAPDC-CDE2  PIC	X(14).
		   07  FILLER	   PIC	X(03).
		   07  SAPDC-CDE3  PIC	X(14).
		   07  FILLER	   PIC	X(04).
		   07  SAPDC-CDE4  PIC	X(14).
		   07  FILLER	   PIC	X(03).
	   03  SAPDC-L2 REDEFINES W02-PRN-LINE.
	       05  SAPDC-DATA2 OCCURS 4.
		   07  SAPDC-DESC  PIC	X(27).
		   07  FILLER	   PIC	X(08).
	   03  SAPDC-L3 REDEFINES W02-PRN-LINE.
	       05  SAPDC-DATA3 OCCURS 4.
		   07  SAPDC-H	   PIC	X(18).
		   07  SAPDC-PRC   PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(09).
	   03  SAPDC-L4 REDEFINES W02-PRN-LINE.
	       05  SAPDC-DET.
		   07  FILLER	   PIC	X(05).
		   07  SAPDC-PRC1  PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(10).
		   07  SAPDC-PRC2  PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(09).
		   07  SAPDC-PRC3  PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(10).
		   07  SAPDC-PRC4  PIC	ZZZZ9.99.
	       05  FILLER      PIC  X(04).

	   03  SAPDC-L5 REDEFINES W02-PRN-LINE.
	       05  SAPDC-DATE-SUPP.
		   07  SAPDC-DT1   PIC	X(06).
		   07  FILLER	   PIC	X(03).
		   07  SAPDC-SUP1  PIC	X(05).
		   07  FILLER	   PIC	X(21).
		   07  SAPDC-DT2   PIC	X(06).
		   07  FILLER	   PIC	X(03).
		   07  SAPDC-SUP2  PIC	X(05).
		   07  FILLER	   PIC	X(21).
		   07  SAPDC-DT3   PIC	X(06).
		   07  FILLER	   PIC	X(03).
		   07  SAPDC-SUP3  PIC	X(05).
		   07  FILLER	   PIC	X(21).
		   07  SAPDC-DT4   PIC	X(06).
		   07  FILLER	   PIC	X(03).
		   07  SAPDC-SUP4  PIC	X(05).
		   07  FILLER	   PIC	X(02).
	   03  LB5-L1 REDEFINES W02-PRN-LINE.
	       05  LB5-DET OCCURS 4.
		   07  LB5-CODE    PIC	X(14).
		   07  FILLER	   PIC	X(03).
	   03  LB5-L2 REDEFINES W02-PRN-LINE.
	       05  LB5-DATA2 OCCURS 4.
		   07  LB5-DESC.
		     09  FILLER	   PIC	X(28).
		     09  LB5-SPC   PIC	X(02).
		   07  FILLER	   PIC	X(03).
	   03  LB5-L3 REDEFINES W02-PRN-LINE.
	       05  LB5-DATA3 OCCURS 4.
		   07  LB5-H	   PIC	X(18).
		   07  LB5-PRC	   PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(07).
	   03  LB5-L4 REDEFINES W02-PRN-LINE.
	       05  LB5-DATA4 OCCURS 4.
		   07  LB5-COST    PIC	X(08).
		   07  FILLER	   PIC	X(06).
		   07  LB5-LAB-SUPP.
		       09  LB5-LABCDE	PIC  X(08).
		       09  LB5-DTE	PIC  X(06).
		   07  LB5-LBSP REDEFINES LB5-LAB-SUPP.
		       09  LB5-H2	PIC  X(06).
		       09  LB5-SUPP	PIC  X(06).
		       09  FILLER	PIC  X(02).
		   07  FILLER	   PIC	X(05).
	   03  LB6-L1 REDEFINES W02-PRN-LINE.
	       05  LB6-DET OCCURS 4.
		   07  LB6-CODE    PIC	X(14).
		   07  FILLER	   PIC	X(04).
	   03  LB6-L2 REDEFINES W02-PRN-LINE.
	       05  LB6-DATA2 OCCURS 4.
		   07  LB6-DESC.
		       09  FILLER  PIC	X(28).
		       09  LB6-SPC PIC	X(02).
		   07  FILLER	   PIC	X(06).
	   03  LB6-L3 REDEFINES W02-PRN-LINE.
	       05  LB6-DATA3 OCCURS 4.
		   07  LB6-H	   PIC	X(18).
		   07  LB6-PRC	   PIC	ZZZZ9.99.
		   07  FILLER	   PIC	X(10).
	   03  LB6-L4 REDEFINES W02-PRN-LINE.
	       05  LB6-DATA4 OCCURS 4.
		   07  LB6-COST    PIC	X(08).
		   07  FILLER	   PIC	X(06).
		   07  LB6-LAB-SUPP.
		       09  LB6-LABCDE	PIC  X(08).
		       09  LB6-DTE	PIC  X(06).
		   07  LB6-LBSP REDEFINES LB6-LAB-SUPP.
		       09  LB6-H2	PIC  X(06).
		       09  LB6-SUPP	PIC  X(06).
		       09  FILLER	PIC  X(02).
		   07  FILLER	   PIC	X(08).

       COPY W05.WS.

       COPY W05.VAT.

003820 01  W10-MONTHS.
003830     03  W10-MONTH-NAMES PIC X(36)     VALUE
003840                         "JanFebMarAprMayJunJulAugSepOctNovDec".
003850     03  W10-MONTH       REDEFINES W10-MONTH-NAMES
003860                         PIC X(03)     OCCURS 12.
       01  W10-STOCK.
002850	   03  W10-SELL        PIC S9(07)V999  COMP-3.
002860	   03  W10-CASH        PIC S9(07)V999  COMP-3.
002860	   03  W10-WSALE       PIC S9(07)V999  COMP-3.
002850	   03  W10-VSELL       PIC S9(09)V99   COMP-3.
002860	   03  W10-VCASH       PIC S9(09)V99   COMP-3.
002860	   03  W10-VWSALE      PIC S9(09)V99   COMP-3.
	   03  W10-CHECK       PIC S9(09)V99   COMP-3.
	   03  W10-STCK        PIC  X(01).
	   03  W10-SLNGTH      PIC  X(01)      COMP-X.
	   03  W10-ITEM.
004570         05  W15-ITEM    PIC X(14).
	   03  W10-ITEM14 REDEFINES W10-ITEM.
	       05  W10-I14    PIC  9(14).
	   03  W10-ITEM13 REDEFINES W10-ITEM.
	       05  W10-I13    PIC  9(13).
	       05  FILLER     PIC  X(01).
	   03  W10-ITEM12 REDEFINES W10-ITEM.
	       05  W10-I12    PIC  9(12).
	       05  FILLER     PIC  X(02).
	   03  W10-ITEM11 REDEFINES W10-ITEM.
	       05  W10-I11    PIC  9(11).
	       05  FILLER     PIC  X(03).
	   03  W10-ITEM10 REDEFINES W10-ITEM.
	       05  W10-I10    PIC  9(10).
	       05  FILLER     PIC  X(04).
	   03  W10-ITEM9 REDEFINES W10-ITEM.
	       05  W10-I9     PIC  9(09).
	       05  FILLER     PIC  X(05).
	   03  W10-ITEM8 REDEFINES W10-ITEM.
	       05  W10-I8     PIC  9(08).
	       05  FILLER     PIC  X(06).
	   03  W10-ITEM7 REDEFINES W10-ITEM.
	       05  W10-I7     PIC  9(07).
	       05  FILLER     PIC  X(07).
	   03  W10-ITEM6 REDEFINES W10-ITEM.
	       05  W10-I6     PIC  9(06).
	       05  FILLER     PIC  X(08).
	   03  W10-ITEM5 REDEFINES W10-ITEM.
	       05  W10-I5     PIC  9(05).
	       05  FILLER     PIC  X(09).
	   03  W10-ITEM4 REDEFINES W10-ITEM.
	       05  W10-I4     PIC  9(04).
	       05  FILLER     PIC  X(10).
	   03  W10-ITEM3 REDEFINES W10-ITEM.
	       05  W10-I3     PIC  9(03).
	       05  FILLER     PIC  X(11).
       01  W15-LABELS.
           03  W15-DEPTH          OCCURS 2.
               05  W15-WIDTH      OCCURS 3.
                   07  W15-ITM    PIC  X(14).
                   07  W15-DSC1   PIC  X(30).
                   07  W15-PRC    PIC  9(05)V99.
                   07  W15-SUPP   PIC  X(05).
       01  W15-LAB-2 REDEFINES W15-LABELS.
           03  W15-LABS OCCURS 4.
               05  W15-SCDE       PIC  X(14).
               05  W15-DSC        PIC  X(22).
               05  W15-PRICE      PIC  9(05)V99.
               05  W15-SUPP2      PIC  X(05).
       01  W15-LAB-3 REDEFINES W15-LABELS.
           03  W15-LAB4 OCCURS 3.
               05  W15-ITM4       PIC  X(14).
               05  W15-DSC4       PIC  X(30).
               05  W15-PRC4       PIC  9(05)V99.
               05  W15-SUPP4      PIC  X(05).
	       05  W15-COST4	  PIC  X(08).
	       05  W15-INV4.
		   07  W15-INVN4  PIC  Z(08).
       01  W15-SAPDC REDEFINES W15-LABELS.
           03  W15-LAB5 OCCURS 4.
               05  W15-ITM5       PIC  X(14).
	       05  W15-DSC5	  PIC  X(30).
               05  W15-PRC5       PIC  9(05)V99.
               05  W15-SUPP5      PIC  X(05).
	       05  W15-COST	  PIC  X(08).
	       05  W15-INV.
		   07  W15-INVN	  PIC  Z(08).
       01  W20-LABEL.
           03  W20-CODE           PIC  X(14).
           03  W20-DESC           PIC  X(30).
           03  W20-COST.
	       05  W20-CST	  PIC  9(06)V99.
           03  W20-SELL           PIC  9(05)V99.
           03  W20-NO             PIC  9(05)    COMP-0.
           03  W20-SUPP           PIC  X(05).
	   03  W20-INV.
	       05  W20-INVN	  PIC  Z(08).

004460 01  W20-CALCS.
004470     03  W20-RESULT      PIC 9(06)V99.
004480     03  W20-RESULT-1    REDEFINES W20-RESULT.
004490         05  W20-WHOLE   PIC 9(06).
004500         05  W20-DECIMAL PIC 9(02).
004510     03  W20-ORD         PIC 9(07)     COMP-3.
003700 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
004520 01  W25-VALUES.
004530     03  W25-CFLOW OCCURS 6 PIC S9(07)V99 COMP-3.
004540	   03  W25-V1	       PIC S9(09)V99 COMP-3.
004550	   03  W25-V2	       PIC S9(09)V99 COMP-3.
004560 01  W25-EDIT.
004570     03  W25-5.
004580         05  W25-1       PIC ZZZZ9.
004590     03  W25-3V2.
004600         05  W25-2       PIC ZZ9.99.
004610     03  W25-7V2.
004620         05  W25-3       PIC Z(06)9.99.
004630     03  W25-3N.
004640         05  W25-4       PIC ZZ9.
004650     03  W25-DTE.
004660         05  W25-DATE    PIC 99/99/99.
004670     03  W25-6.
004680         05  W25-NO      PIC Z(05)9.
004690     03  W25-5V2.
004700         05  W25-VAL     PIC Z(04)9.99.
004710     03  W25-S5V2.
004720         05  W25-SVAL    PIC Z(04)9.99DB.
004730     03  W25-S7V2.
004740         05  W25-VAL2    PIC Z(06)9.99DB.
004750 01  W30-DATE.
004760     03  W30-MM          PIC X(03).
004770     03  FILLER          PIC X(01)     VALUE ",".
004780     03  W30-YY          PIC 9(02).

003810 01  W30-COST-CODE.
003820     03  W30-CODE           PIC X(01) OCCURS 10.

003830 01  W35-COST.
003840     03  W35-CST            PIC 9(08)V99.
003850     03  W35-WHOLE  REDEFINES W35-CST
                                  PIC 9(10).
003860     03  W35-CPRINT.
003870         05  W35-CEDIT      PIC ZZZZZZ9999.
003880     03  W35-EOC REDEFINES W35-CPRINT.
003890         05  W35-CHAR       PIC X(01) OCCURS 10.
003900     03  W35-EON REDEFINES W35-CPRINT.
003910         05  W35-NUM        PIC 9(01) OCCURS 10.
	   03  W35-CPRN REDEFINES W35-CPRINT.
	       05  FILLER	  PIC  X(02).
	       05  W35-LCST	  PIC  X(08).
003920     03  W35-S1             PIC 9(02) COMP-5.
003930     03  W35-S2             PIC 9(02) COMP-5.
003930	   03  W35-S3		  PIC 9(02) COMP-5.

       COPY W40.WS.

       COPY FUNCTION.WS.

004800 COPY W50.WS.

004820 01  W50-H.
004830     03  FILLER          PIC X(01)     VALUE "(".
004840     03  W50-MONTH       PIC X(03).
004850     03  FILLER          PIC X(01)     VALUE ")".
004860 01  W55-DEL.
004870     03  W55-MONTH       PIC X(03).
004880     03  FILLER          PIC X(01)     VALUE ",".
004890     03  W55-YY          PIC 9(02).
004900 01  W60-ITMCODE.
004910     03  W60-ITM.
004920         05  W60-ICHAR OCCURS 12 PIC X(01).
004930 01  W60-H2.
004940     03  W60-COMPANY     PIC X(40).
004950     03  FILLER          PIC X(02)     VALUE "-".
004960     03  W60-DESC        PIC X(27).
004970 01  W65-CREDITOR.
004980     03  W65-CFLOW       PIC S9(07)V99 COMP-3 OCCURS 6.

005020 01  W70-VALUES.
005030     03  W70-BAL         PIC S9(07)V99 COMP-3.
005040     03  W70-AREAR       PIC S9(07)V99 COMP-3.
005050     03  W70-30D         PIC S9(07)V99 COMP-3.
005060     03  W70-60D         PIC S9(07)V99 COMP-3.
005070     03  W70-90D         PIC S9(07)V99 COMP-3.
005080     03  W70-120D        PIC S9(07)V99 COMP-3.
005090     03  W70-150D        PIC S9(07)V99 COMP-3.
005100     03  W70-180D        PIC S9(07)V99 COMP-3.

005140 COPY W70CRED.WS.

005160 01  W90-TABLE.
005170     03  W90-ENTRIES.
005180         05  W90-ENTRY    OCCURS 80.
005190             07  W90-NAME PIC X(04).
005200             07  W90-KEY  PIC 9(04)     COMP-5.
005210     03  W90-R1           REDEFINES W90-ENTRIES.
005220         05  W90-F20      PIC X(120).
005230         05  W90-L60      PIC X(360).
005240     03  W90-R2           REDEFINES W90-ENTRIES.
005250         05  W90-F60      PIC X(360).
005260         05  W90-L20      PIC X(120).
005270     03  W90-R3           REDEFINES W90-ENTRIES.
005280         05  W90-20       PIC X(120).
005290         05  W90-40       PIC X(120).
005300         05  W90-60       PIC X(120).
005310         05  W90-80       PIC X(120).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
005320 SCREEN SECTION.

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
004170	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
004180                           VALUE "STOCK LABELS CONTROL".
       01  S-MENU.
	   03  LINE  4 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "PRICE LABEL FORMATS".
	   03	       COLUMN 50 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " ³".
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Format 1          ³".
	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Format 2          ³".
001470	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Format 3          ³".
001470	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Format 4          ³".
001470	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Format 5          ³".
001470	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Format 6          ³".
001470	   03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Format 7          ³".
001470	   03  LINE 13 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Format 8 (SAPDC)  ³".
001470	   03  LINE 14 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "9".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Format 9          ³".
001470	   03  LINE 15 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit              ³".
	   03  LINE 16 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       COPY ERROR.CRT.

001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
005670 AA000-START-UP  SECTION.
       AA000.
003240	   IF LS0-CRLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA105.
008030	     PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM KA000.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA105.
             EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP8.

      /
006460 AB00-READ SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY STOCK.RD.

       COPY PARAM.RD.

       COPY PRCLAB.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010390 AC000-WRITE   SECTION.

000300 COPY PRCLAB.WR.

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
013270 CA000         SECTION.
013280 CA00.
013290       MOVE 14             TO W50-LENGTH.
013300 CA05.
013310       MOVE W10-ITEM       TO W50-DATA.
013320     IF W50-1 = SPACE
013330         MOVE W50-13       TO W10-ITEM
013340         GO TO CA05.      
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
013400	     MOVE W50-ITEM	 TO W10-ITEM.
013410 CA06.
013420       MOVE W50-ITEM       TO STK-CODE.
013430 CA10.
013440       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
013450     IF WS-F-ERROR = 0
013460         PERFORM CA500.
013470 CA99.
013480       EXIT.
      *
      *    ****    Calculate the inclusive prices
      *
       CA500         SECTION.
       CA505.
      * 			 Adjust the average cost for negative
      * 			 or zero quantity
	   IF STK-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
           IF STK-WSALE = ZERO
               IF STK-CASH NOT = ZERO
                   MOVE STK-CASH TO STK-WSALE
               ELSE
                   MOVE STK-SELL TO STK-WSALE.
015630	   IF STK-CASH = ZERO
015640	       MOVE STK-SELL	 TO STK-CASH.
	   IF STK-IND < 2
	       IF STK-CASH < STK-WSALE
		   MOVE STK-WSALE    TO STK-CASH
	       END-IF
	       IF STK-SELL < STK-CASH
		   MOVE STK-CASH     TO STK-SELL
	       END-IF
	       IF STK-CMARKUP < STK-WMARKUP
		   MOVE STK-WMARKUP  TO STK-CMARKUP
	       END-IF
	       IF STK-MARKUP < STK-CMARKUP
		   MOVE STK-CMARKUP  TO STK-MARKUP
	       END-IF.
	     MOVE STK-CASH	 TO W10-CASH
	     MOVE STK-SELL	 TO W10-SELL
	     MOVE STK-WSALE	 TO W10-WSALE.
             MOVE ZERO           TO W05-RATE.
           IF STK-TAX > 0
	       MOVE W05-VAT(1)	 TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED 
		   = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED 
		   = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED 
		   = W10-SELL + (W10-SELL * W05-RTE).
	   IF STK-PRC = ZERO
	       MOVE "EA"	 TO STK-PACK.
015650 CA599.
015660       EXIT.

      /
       KA000	     SECTION 5.
       KA00.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     DISPLAY CLR-SCREEN.
             DISPLAY "PRICE LABELS" AT 0235 WITH FOREGROUND-COLOR 15.
           IF WS-PRCKEY < 3
               DISPLAY "No labels to print - Press any key" AT 1212
                        WITH FOREGROUND-COLOR 14
               ACCEPT WS-OPTION AT 1247 WITH AUTO
               GO TO KA999.
	     DISPLAY S-PROG.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 21		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       KA01.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 14		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 19		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

008690 KA02.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 KA03.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO KA04
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO KA03
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO KA03
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO KA03
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO KA03
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO KA04
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO KA02
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO KA06
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO KA02
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < "0" OR > "9")
		   CALL X"E5"
		   GO TO KA02
	       END-IF
	       GO TO KA05
	   ELSE
	       CALL X"E5"
	       GO TO KA02.

       KA04.
	   IF SCREEN-LIN < 5
	       CALL X"E5"
	       GO TO KA02
	   ELSE
	   IF SCREEN-LIN = 5
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 6
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "8"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "9"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "0"		 TO WS-OPTION.
	   GO TO KA06.

       KA05.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 5		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 6		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "9"
	       MOVE 13		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       KA06.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
008760	   IF WS-OPTION = "0"
008770         GO TO KA999.
008780	   IF (WS-OPTION < "1" OR > "9")
	       GO TO KA02.
             MOVE WS-OPTION      TO WS-LAB.
       KA10.
018630       DISPLAY CLR-SCREEN.
           IF WS-LAB = "1"
               DISPLAY "FORMAT 1 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR 15
           ELSE
           IF WS-LAB = "2"
               DISPLAY "FORMAT 2 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR 15
           ELSE
           IF WS-LAB = "3"
               DISPLAY "FORMAT 3 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR 15
	   ELSE
           IF WS-LAB = "4"
               DISPLAY "FORMAT 4 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR 15
	   ELSE
	   IF WS-LAB = "5"
	       DISPLAY "FORMAT 5 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR 15
	   ELSE
	   IF WS-LAB = "6"
	       DISPLAY "FORMAT 6 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR 15
	   ELSE
	   IF WS-LAB = "7"
	       DISPLAY "FORMAT 7 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR 15
	   ELSE
	   IF WS-LAB = "8"
               DISPLAY "SAPDC FORMAT - PRICE LABELS" AT 0227
                        WITH FOREGROUND-COLOR 15
      *
      *    ****    8   L P I
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 8		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
      *
      *    ****    1 7	 C P I
      *
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE SPACES	 TO R-L1
	   ELSE
	   IF WS-LAB = "9"
	       DISPLAY "FORMAT 9 - PRICE LABELS" AT 0229
			WITH FOREGROUND-COLOR 15.
	   IF WS-LAB = "3" OR "5" OR "6" OR "7"
      *
      *    ****    1 2	 C P I
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE SPACES	 TO R-L1.
	     DISPLAY "Omit price from label " AT 0812
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-PRICE.
       KA15.
	     ACCEPT WS-PRICE AT 0839
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRICE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-PRICE = "N" OR "Y")
               GO TO KA15.
             DISPLAY "Item code :" AT 1012.
             MOVE WS-PRCKEY      TO WS-KEYSTR.
             MOVE 2              TO WS-PRCKEY.
       KA18.
             MOVE 1              TO W35-S1 W35-S2.
	   IF WS-LAB = 1 OR 9
               INITIALIZE W20-LABEL
           ELSE
           IF WS-LAB = 2
               INITIALIZE W15-LABELS
           ELSE
           IF WS-LAB = 3
               INITIALIZE W15-LAB-2
           ELSE
	   IF WS-LAB = 4 OR 6
               INITIALIZE W15-LAB-3
           ELSE
	   IF WS-LAB = 5 OR 7 OR 8
               INITIALIZE W15-SAPDC.
       KA20.
             PERFORM READ-PRCLAB THRU READ-PRCLAB-EXIT.
             MOVE PLB-ITEM       TO W10-ITEM.
	     DISPLAY PLB-ITEM AT 1024
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             MOVE PLB-QUANT      TO W20-NO.
	     MOVE PLB-INV	 TO W20-INVN.
             PERFORM CA000.
015860     IF WS-F-ERROR NOT = ZERO
	       GO TO KA795.
	   IF WS-LAB = "1" OR "9"
               GO TO KA25
           ELSE
           IF WS-LAB = "2"
               GO TO KA35
           ELSE
           IF WS-LAB = "3"
	       GO TO KA65
           ELSE
           IF WS-LAB = "4"
               GO TO KA90
	   ELSE
	   IF WS-LAB = "5"
	       GO TO KA160
	   ELSE
	   IF WS-LAB = "6"
	       GO TO KA200
	   ELSE
	   IF WS-LAB = "7"
	       GO TO KA260
	   ELSE
               GO TO KA125.
	     GO TO KA800.
       KA25.
             MOVE STK-CODE       TO W20-CODE.
             MOVE STK-DESC       TO W20-DESC.
             MOVE STK-COST       TO W35-CST.
             MOVE STK-SUPP       TO W20-SUPP.
009540       MOVE W35-WHOLE      TO W35-CEDIT.
009550	     MOVE 3		 TO W35-S3.
009560 KA26.
	   IF W35-S3 < 6
	       IF W35-CHAR (W35-S3) = SPACE
		   MOVE SPACE	 TO W35-CHAR (W35-S3)
		   ADD 1	 TO W35-S3
		   GO TO KA26.
009570	   IF (W35-CHAR (W35-S3) NOT NUMERIC) OR
009590	      (W35-NUM (W35-S3) = 0)
009600	       MOVE 10		 TO W35-S2
009610     ELSE
009620	       MOVE W35-NUM (W35-S3)
				 TO W35-S2.
009630	     MOVE W30-CODE (W35-S2)
				 TO W35-CHAR (W35-S3).
009650	   IF W35-S3 < 10
009660	       ADD 1		 TO W35-S3
009670         GO TO KA26.
	     MOVE W35-LCST	 TO W20-COST.
       KA28.
	     MOVE W10-VSELL	 TO W20-SELL.
       KA30.
           IF W20-NO < 1
	       GO TO KA795.
	     MOVE SPACES	 TO R-L1.
	   IF WS-LAB = "9"
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE "Item:"	 TO LAB-HEAD.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-L1.
	     MOVE W20-CODE	 TO LAB-ITEM.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-L1.
           IF WS-PRICE = "N"
	       MOVE "Prod:"	 TO LAB-HEAD
	       MOVE W20-COST	 TO LAB-COST.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-L1.
	     MOVE "Desc:"	 TO LAB-HEAD.
	     MOVE W20-DESC	 TO LAB-DESC.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-L1.
           IF WS-PRICE = "N"
	       MOVE "Price"	 TO LAB-HEAD
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE SPACES	 TO R-L1
	       MOVE W20-SELL	 TO LAB-SELL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF WS-PRICE = "N"
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "X"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-L1.
           IF NOT (W20-SUPP = SPACES OR LOW-VALUES)
	       MOVE "Supp:"	 TO LAB-HEAD
               MOVE W20-SUPP     TO LAB-DESC.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-L1.
	   IF WS-LAB = "9"
	       PERFORM CALL-PRINTUTL.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO KA800.
	     SUBTRACT 1 	 FROM W20-NO.
             GO TO KA30.
       KA35.
           IF W20-NO < 1
	       GO TO KA795.
             MOVE STK-CODE       TO W15-ITM (W35-S1, W35-S2).
             MOVE STK-DESC       TO W15-DSC1 (W35-S1, W35-S2).
             MOVE STK-SUPP       TO W15-SUPP (W35-S1, W35-S2).
           IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC (W35-S1, W35-S2)
           ELSE
               MOVE ZERO         TO W15-PRC (W35-S1, W35-S2).
             SUBTRACT 1          FROM W20-NO.
           IF W35-S1 < 2
               ADD 1             TO W35-S1
               GO TO KA35.
           IF W35-S2 < 3
               MOVE 1            TO W35-S1
               ADD 1             TO W35-S2
               GO TO KA35.
             PERFORM KA40 THRU KA60.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
014990	       GO TO KA800.
             PERFORM KA18.
             GO TO KA35.
       KA40.
             MOVE 1              TO W35-S1.
       KA45.
             MOVE 1              TO W35-S2.
	     MOVE SPACES TO R-L1.

       KA50.
           IF W15-ITM (W35-S1, W35-S2) NOT = SPACES
               MOVE W15-ITM (W35-S1, W35-S2)
                                 TO LB2-DATA (W35-S2).
           IF W35-S2 < 3
               ADD 1             TO W35-S2
               GO TO KA50.

	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA45.

       KA55.
           IF W15-ITM (W35-S1, W35-S2) NOT = SPACES
               MOVE W15-DSC1 (W35-S1, W35-S2)
                                 TO LB2-DATA (W35-S2).
           IF W35-S2 < 3
               ADD 1             TO W35-S2
               GO TO KA55.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA45.

       KA58.
           IF W15-ITM (W35-S1, W35-S2) NOT = SPACES
               MOVE "VAT inclusive"
                                 TO LB2-H (W35-S2)
               MOVE W15-PRC (W35-S1, W35-S2)
                                 TO LB2-PRC (W35-S2).
           IF W35-S2 < 3
               ADD 1             TO W35-S2
               GO TO KA58.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA45.

       KA60.
           IF NOT (W15-SUPP (W35-S1, W35-S2) = SPACES OR LOW-VALUES)
               MOVE W15-SUPP (W35-S1, W35-S2)
                                 TO LB2-DATA (W35-S2).
           IF W35-S2 < 3
               ADD 1             TO W35-S2
               GO TO KA60.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA45.
           IF W35-S1 < 2
               ADD 1             TO W35-S1
               GO TO KA50.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES TO R-L1.

       KA65.
           IF W20-NO < 1
	       GO TO KA795.
             MOVE STK-CODE       TO W15-SCDE (W35-S1).
             MOVE STK-DESC       TO W15-DSC (W35-S1).
             MOVE STK-SUPP       TO W15-SUPP2 (W35-S1).
	   IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRICE (W35-S1)
           ELSE
               MOVE ZERO         TO W15-PRICE (W35-S1).
             SUBTRACT 1          FROM W20-NO.
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KA65.
             PERFORM KA70 THRU KA85.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO KA800.
             PERFORM KA18.
             GO TO KA65.

       KA70.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO R-L1.

       KA75.
           IF W15-SCDE (W35-S1) NOT = SPACES
               MOVE W15-SCDE (W35-S1)
                                 TO LB3-DATA (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KA75.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA70.

       KA80.
           IF W15-SCDE (W35-S1) NOT = SPACES
               MOVE W15-DSC (W35-S1)
                                 TO LB3-DATA (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KA80.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA70.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

       KA84.
           IF W15-SCDE (W35-S1) NOT = SPACES
	       IF WS-PRICE = "N"
                   MOVE "VAT incl"   
                                 TO LB3-H (W35-S1)
                   MOVE W15-PRICE (W35-S1)
                                 TO LB3-PRC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KA84.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA70.

       KA85.
           IF NOT (W15-SUPP2 (W35-S1) = SPACES OR LOW-VALUES)
               MOVE W15-SUPP2 (W35-S1)
                                 TO LB3-DATA (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KA85.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA70.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

       KA90.
           IF W20-NO < 1
	       GO TO KA795.
             MOVE STK-CODE       TO W15-ITM4 (W35-S1).
             MOVE STK-DESC       TO W15-DSC4 (W35-S1).
             MOVE STK-SUPP       TO W15-SUPP4 (W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC4 (W35-S1)
           ELSE
               MOVE ZERO         TO W15-PRC4 (W35-S1).
             SUBTRACT 1          FROM W20-NO.
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO KA90.
             PERFORM KA95 THRU KA120.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO KA800.
             PERFORM KA18.
             GO TO KA90.

       KA95.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       KA100.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO R-L1.

       KA105.
           IF W15-ITM4 (W35-S1) NOT = SPACES
               MOVE W15-ITM4 (W35-S1)
                                 TO LB4-CODE (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO KA105.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM KA100.

       KA110.
           IF W15-ITM4 (W35-S1) NOT = SPACES
               MOVE W15-DSC4 (W35-S1)
                                 TO LB4-DESC (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO KA110.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA100.

       KA112.
           IF W15-ITM4 (W35-S1) NOT = SPACES
           IF NOT (W15-SUPP4 (W35-S1) = SPACES OR LOW-VALUES)
               MOVE "Supplier:"  TO LB4-H (W35-S1)
               MOVE W15-SUPP4 (W35-S1)
                                 TO LB4-LABCDE (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO KA112.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA100.

       KA115.
           IF W15-ITM4 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"
                                 TO LB4-H (W35-S1)
                   MOVE W15-PRC4 (W35-S1)
                                 TO LB4-PRC (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO KA115.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA100.

       KA120.
           IF W15-ITM4 (W35-S1) NOT = SPACES
               MOVE WS-LABCDE    TO LB4-LABCDE (W35-S1)
               MOVE W05-MMYY     TO LB4-DTE (W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO KA120.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA100.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

       KA125.
           IF W20-NO < 1
	       GO TO KA795.
             MOVE STK-CODE       TO W15-ITM5 (W35-S1).
             MOVE STK-DESC       TO W15-DSC5 (W35-S1).
	     MOVE STK-SUPP	 TO W15-SUPP5 (W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC5 (W35-S1)
           ELSE
               MOVE ZERO         TO W15-PRC5 (W35-S1).
             SUBTRACT 1          FROM W20-NO.
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KA125.
	     PERFORM KA130 THRU KA155B.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO KA800.
             PERFORM KA18.
             GO TO KA125.

       KA130.
	     MOVE SPACES	 TO R-L1.

       KA135.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       KA140.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO R-L1.

       KA145.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               IF W35-S1 = 1
                   MOVE W15-ITM5 (1)
                                 TO SAPDC-CDE1
               ELSE
               IF W35-S1 = 2
                   MOVE W15-ITM5 (2)
                                 TO SAPDC-CDE2
               ELSE
               IF W35-S1 = 3
                   MOVE W15-ITM5 (3)
                                 TO SAPDC-CDE3
               ELSE
                   MOVE W15-ITM5 (4)
                                 TO SAPDC-CDE4.
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KA145.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-L1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM KA140.

       KA150.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               MOVE W15-DSC5 (W35-S1)
                                 TO SAPDC-DESC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KA150.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA140.
	     PERFORM CALL-PRINTUTL.

       KA155.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"
				 TO SAPDC-H (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
               GO TO KA155.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-L1.
	     PERFORM KA140.

       KA155A.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
	       IF W35-S1 = 1
		   MOVE W15-PRC5 (1)
				 TO SAPDC-PRC1
               ELSE
               IF W35-S1 = 2
		   MOVE W15-PRC5 (2)
				 TO SAPDC-PRC2
               ELSE
               IF W35-S1 = 3
		   MOVE W15-PRC5 (3)
				 TO SAPDC-PRC3
               ELSE
		   MOVE W15-PRC5 (4)
				 TO SAPDC-PRC4.
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA155A.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM KA140.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

       KA155B.
	   IF W15-ITM5 (W35-S1) NOT = SPACES
               IF W35-S1 = 1
		   MOVE W15-SUPP5 (1)
				 TO SAPDC-SUP1
		   MOVE W05-MMYY TO SAPDC-DT1
	       ELSE
               IF W35-S1 = 2
		   MOVE W15-SUPP5 (2)
				 TO SAPDC-SUP2
		   MOVE W05-MMYY TO SAPDC-DT2
	       ELSE
               IF W35-S1 = 3
		   MOVE W15-SUPP5 (3)
				 TO SAPDC-SUP3
		   MOVE W05-MMYY TO SAPDC-DT3
	       ELSE
		   MOVE W15-SUPP5 (4)
				 TO SAPDC-SUP4
		   MOVE W05-MMYY TO SAPDC-DT4.
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA155B.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA140.
	     PERFORM CALL-PRINTUTL.
	     PERFORM CALL-PRINTUTL.

       KA160.
           IF W20-NO < 1
	       GO TO KA795.
             MOVE STK-CODE       TO W15-ITM5 (W35-S1).
             MOVE STK-DESC       TO W15-DSC5 (W35-S1).
	   IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC5 (W35-S1)
           ELSE
               MOVE ZERO         TO W15-PRC5 (W35-S1).
             MOVE STK-COST       TO W35-CST.
009540	     MOVE W35-WHOLE	 TO W35-CEDIT.
009550	     MOVE 1		 TO W35-S3.
	     PERFORM KA26.
	     MOVE W20-COST	 TO W15-COST (W35-S1).
	     MOVE W20-INV	 TO W15-INV (W35-S1).
	     SUBTRACT 1 	 FROM W20-NO.
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA160.
	     PERFORM KA165 THRU KA192.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO KA800.
             PERFORM KA18.
	     GO TO KA160.

       KA165.
	     MOVE SPACES TO R-L1.

       KA170.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       KA175.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO R-L1.

       KA180.
           IF W15-ITM5 (W35-S1) NOT = SPACES
	       MOVE W15-ITM5 (W35-S1)
				 TO LB5-CODE (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA180.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-L1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM KA175.

       KA185.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               MOVE W15-DSC5 (W35-S1)
				 TO LB5-DESC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA185.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA175.

       KA186.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"
				 TO LB5-H (W35-S1)
                   MOVE W15-PRC5 (W35-S1)
				 TO LB5-PRC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA186.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA175.

       KA190.
	   IF W15-ITM5 (W35-S1) NOT = SPACES
	       MOVE W15-COST (W35-S1)
				 TO LB5-COST (W35-S1)
	       MOVE WS-LABCDE	 TO LB5-LABCDE (W35-S1)
	       MOVE W05-MMYY	 TO LB5-DTE (W35-S1).
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA190.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA175.

       KA192.
	   IF W15-ITM5 (W35-S1) NOT = SPACES
	       MOVE W15-INV (W35-S1)
				 TO LB5-COST (W35-S1).
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA192.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA175.
	     PERFORM CALL-PRINTUTL.

       KA200.
           IF W20-NO < 1
	       GO TO KA795.
	     MOVE STK-CODE	 TO W15-ITM4 (W35-S1).
	     MOVE STK-DESC	 TO W15-DSC4 (W35-S1).
	   IF WS-PRICE = "N"
	       MOVE W10-VSELL	 TO W15-PRC4 (W35-S1)
           ELSE
	       MOVE ZERO	 TO W15-PRC4 (W35-S1).
             MOVE STK-COST       TO W35-CST.
009540	     MOVE W35-WHOLE	 TO W35-CEDIT.
009550	     MOVE 1		 TO W35-S3.
	     PERFORM KA26.
	     MOVE W20-COST	 TO W15-COST4 (W35-S1).
	     MOVE W20-INV	 TO W15-INV4 (W35-S1).
	     MOVE STK-SUPP	 TO W15-SUPP4 (W35-S1).
	     SUBTRACT 1 	 FROM W20-NO.
	   IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO KA200.
	     PERFORM KA205 THRU KA240.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO KA800.
             PERFORM KA18.
	     GO TO KA200.

       KA205.
	     MOVE SPACES	 TO R-L1.

       KA210.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       KA215.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO R-L1.

       KA220.
	   IF W15-ITM4 (W35-S1) NOT = SPACES
	       MOVE W15-ITM4 (W35-S1)
				 TO LB5-CODE (W35-S1).
	   IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO KA220.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-L1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM KA215.

       KA225.
	   IF W15-ITM4 (W35-S1) NOT = SPACES
	       MOVE W15-DSC4 (W35-S1)
				 TO LB5-DESC (W35-S1).
	   IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO KA225.
	     MOVE SPACES	 TO LB5-SPC (3).
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA215.

       KA230.
	   IF W15-ITM4 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"
				 TO LB5-H (W35-S1)
		   MOVE W15-PRC4 (W35-S1)
				 TO LB5-PRC (W35-S1).
	   IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO KA230.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA215.

       KA235.
	   IF W15-ITM4 (W35-S1) NOT = SPACES
	       MOVE W15-COST (W35-S1)
				 TO LB5-COST (W35-S1)
	       MOVE WS-LABCDE	 TO LB5-LABCDE (W35-S1)
	       MOVE W05-MMYY	 TO LB5-DTE (W35-S1).
	   IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO KA235.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA215.

       KA240.
	   IF W15-ITM4 (W35-S1) NOT = SPACES
	       MOVE W15-INV4 (W35-S1)
				 TO LB5-COST (W35-S1).
	   IF NOT (W15-SUPP4 (W35-S1)= SPACES OR LOW-VALUES)
	       MOVE "Supp:"	 TO LB5-H2 (W35-S1)
	       MOVE W15-SUPP4 (W35-S1)
				 TO LB5-SUPP (W35-S1).
	   IF W35-S1 < 3
               ADD 1             TO W35-S1
	       GO TO KA240.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA215.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

       KA260.
           IF W20-NO < 1
	       GO TO KA795.
             MOVE STK-CODE       TO W15-ITM5 (W35-S1).
             MOVE STK-DESC       TO W15-DSC5 (W35-S1).
	   IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC5 (W35-S1)
           ELSE
               MOVE ZERO         TO W15-PRC5 (W35-S1).
             MOVE STK-COST       TO W35-CST.
009540	     MOVE W35-WHOLE	 TO W35-CEDIT.
009550	     MOVE 1		 TO W35-S3.
	     PERFORM KA26.
	     MOVE W20-COST	 TO W15-COST (W35-S1).
	     MOVE W20-INV	 TO W15-INV (W35-S1).
	     SUBTRACT 1 	 FROM W20-NO.
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA260.
	     PERFORM KA265 THRU KA292.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO KA800.
             PERFORM KA18.
	     GO TO KA260.

       KA265.
	     MOVE SPACES	 TO R-L1.

       KA270.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       KA275.
             MOVE 1              TO W35-S1.
	     MOVE SPACES	 TO R-L1.

       KA280.
           IF W15-ITM5 (W35-S1) NOT = SPACES
	       MOVE W15-ITM5 (W35-S1)
				 TO LB6-CODE (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA280.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-L1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM KA275.

       KA285.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               MOVE W15-DSC5 (W35-S1)
				 TO LB6-DESC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA285.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA275.

       KA286.
           IF W15-ITM5 (W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"
				 TO LB6-H (W35-S1)
                   MOVE W15-PRC5 (W35-S1)
				 TO LB6-PRC (W35-S1).
           IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA286.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA275.

       KA290.
	   IF W15-ITM5 (W35-S1) NOT = SPACES
	       MOVE W15-COST (W35-S1)
				 TO LB6-COST (W35-S1)
	       MOVE WS-LABCDE	 TO LB6-LABCDE (W35-S1)
	       MOVE W05-MMYY	 TO LB6-DTE (W35-S1).
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA290.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA275.

       KA292.
	   IF W15-ITM5 (W35-S1) NOT = SPACES
	       MOVE W15-INV (W35-S1)
				 TO LB6-COST (W35-S1).
	   IF W35-S1 < 4
               ADD 1             TO W35-S1
	       GO TO KA292.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM KA275.
	     PERFORM CALL-PRINTUTL.

       KA795.
             ADD 1               TO WS-PRCKEY.
           IF WS-PRCKEY < WS-KEYSTR
               GO TO KA20.
           IF WS-LAB = "2"
               IF (W35-S1 > 1) OR (W35-S2 > 1)
                   PERFORM KA40 THRU KA60
               END-IF
           ELSE
           IF WS-LAB = "3"
               IF W35-S1 > 1
                   PERFORM KA70 THRU KA85
               END-IF
	   ELSE
           IF WS-LAB = "4"
               IF W35-S1 > 1
                   PERFORM KA95 THRU KA120
               END-IF
	   ELSE
	   IF WS-LAB = "5"
               IF W35-S1 > 1
		   PERFORM KA165 THRU KA192
               END-IF
      *
      *    ****    1 0	 C P I
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	   ELSE
	   IF WS-LAB = "6"
               IF W35-S1 > 1
		   PERFORM KA205 THRU KA240
               END-IF
      *
      *    ****    1 0	 C P I
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	   ELSE
	   IF WS-LAB = "7"
               IF W35-S1 > 1
		   PERFORM KA265 THRU KA292
               END-IF
      *
      *    ****    1 0	 C P I
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	   ELSE
	   IF WS-LAB = "8"
               IF W35-S1 > 1
		   PERFORM KA130 THRU KA155B
               END-IF
      *
      *    ****    1 0	 C P I
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
      *
      *    ****    6   L P I
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 6		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     CLOSE PRCLAB.
             OPEN OUTPUT PRCLAB.
             MOVE 1              TO WS-PRCKEY.
             INITIALIZE PLB-REC1.
             MOVE 2              TO PLB-QUANT.
             PERFORM WRITE-PRCLAB THRU WRITE-PRCLAB-EXIT.
             CLOSE PRCLAB.
             GO TO KA999.

       KA800.
             MOVE WS-KEYSTR      TO WS-PRCKEY.
       KA999.
             EXIT.
      /
041830 ZA000-INIT    SECTION 8.
041840 ZA000-START.
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
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "PRCLAB"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-PRCLAB.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
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
       ZA05.
042360       MOVE 1              TO WS-PARKEY.
042370       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042380       MOVE PAR-COMPANY    TO W60-COMPANY.
042390       MOVE PAR-DMY        TO W05-DATE.
042400       MOVE PAR-YMD        TO W05-DATE-YMD.
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
	     MOVE 6		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019880	     MOVE PAR-COST-CODE  TO W30-COST-CODE.
	     OPEN I-O PRCLAB.
           IF WS-STATUS = "05"
               CLOSE PRCLAB
               OPEN OUTPUT PRCLAB
               INITIALIZE PLB-REC1
               MOVE 2            TO PLB-QUANT
               PERFORM WRITE-PRCLAB THRU WRITE-PRCLAB-EXIT
               CLOSE PRCLAB
               OPEN I-O PRCLAB.
             PERFORM READ-PRCLAB THRU READ-PRCLAB-EXIT.
             MOVE PLB-QUANT      TO WS-PRCKEY.
019620	     MOVE 3 TO WS-NETKEY.
019630       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE STK-LABCDE     TO WS-LABCDE.
042540	     DISPLAY CLR-SCREEN.
042550       GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MD-LNE WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1)
                                    WS-TCR  (WS-S1) WS-BCR  (WS-S1).
041250       MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70)
021930                              WS-TCR  (8)  WS-TCR  (50)
021940                              WS-TCR  (66).
021950       MOVE WS-G3          TO WS-MCHR (1)  WS-MCHR (80)
                                    WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
021950                              WS-MCR  (8)  WS-MCR  (50)
021960                              WS-MCR  (66) WS-MCR  (1)
				    WS-MCR  (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70)
021970                              WS-BCR  (8)  WS-BCR  (50)
021980                              WS-BCR  (66).
051190       MOVE WS-G9          TO WS-TCH  (1).
051200       MOVE WS-G10         TO WS-TCH  (80).
051210       MOVE WS-G11         TO WS-BCH  (1).
051220       MOVE WS-G12         TO WS-BCH  (80).
051230       MOVE WS-G14         TO WS-TCR  (1)  WS-BCR  (1)
                                    WS-TCHR (1)  WS-BCHR (1).
051260       MOVE WS-G13         TO WS-TCR  (80) WS-BCR  (80)
                                    WS-TCHR (80) WS-BCHR (80).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

042560 ZA200.
042570       DISPLAY "Files locked - Try later" AT 2312
042580                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
042590       DISPLAY "Press any key" AT 2512 
042600		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
042610	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.

       ZA205.
             EXIT PROGRAM.

042640 ZA999-EXIT.
042650       EXIT.
      /
042660 ZB000-I-O-ERROR SECTION 9.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
051950	   ELSE
051960     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-PRCLAB	 TO WS-FILE
041870	       MOVE WS-PRCKEY	 TO WS-KEY.
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
042170       STOP RUN.
