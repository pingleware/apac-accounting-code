      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	   ******   ********   *
      *   **	**  **	  **  **    **	 **  **   **	**  **	       *
      *   **	    **	  **  **    **	**    **       **   *******    *
      *   **	    ******    *******	**    **     ** 	  **   *
      *   **	    **	 **   **	**    **   **		  **   *
      *   **	**  **	  **  **	 **  **   **	    **	  **   *
      *    ******   **	  **  **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R ' S   O U T S T A N D I N G   O R D E R S  *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	     CRP025.
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

000160 COPY CREDIT.SL.

000140 COPY PARAM.SL.

000260 COPY PORDER.SL.

000220 COPY STOCK.SL.

000140 COPY SYSUSE.SL.

000290 DATA DIVISION.
000300 FILE SECTION.

       COPY APACFIDS.FDE.

000140 COPY CONTROL.FDE.

000340 COPY CREDIT.FDE.

000320 COPY PARAM.FDE.

000440 COPY PORDER.FDE.

000400 COPY STOCK.FDE.

000310 COPY SYSUSE.FD.

      /
002980 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002990 77  WS-STKKEY	       PIC 9(04)     COMP-5 VALUE 1.
003000 77  WS-NETKEY		PIC 9(04)     COMP-5 VALUE 1.
003010 77  WS-CTRKEY           PIC 9(04)     COMP-5 VALUE 1.
003020 77  WS-CREKEY           PIC 9(04)     COMP-5 VALUE 1.
003030 77  WS-PARKEY           PIC 9(04)     COMP-5 VALUE 1.
003040 77  WS-CINDKEY          PIC 9(04)     COMP-5 VALUE 1.
003050 77  WS-OTRKEY           PIC 9(04)     COMP-5 VALUE 1.
003060 77  WS-ERROR            PIC 9(01)     VALUE 0.
003080 77  WS-S                PIC 9(04)     COMP-5.
003090 77  WS-S1               PIC 9(04)     COMP-5.
003100 77  WS-S2               PIC 9(04)     COMP-5.
003110 77  WS-S3               PIC 9(04)     COMP-5.
003120 77  WS-S4               PIC 9(04)     COMP-5.
003120 77  WS-S6               PIC 9(04)     COMP-5.
003130 77  WS-IXS1             PIC 9(04)     COMP-5.
003140 77  WS-IXS2             PIC 9(04)     COMP-5.
003150 77  WS-IXS3             PIC 9(04)     COMP-5.
003160 77  WS-IXS4             PIC 9(04)     COMP-5.
003170 77  WS-KEY1             PIC 9(04)     COMP-5.
003180 77  WS-CHAR1            PIC X(01).
003190 77  WS-ITM1             PIC X(12).
003200 77  WS-ORD              PIC X(01).
003200 77  WS-SEQ              PIC X(01).
003210 77  WS-OPTION           PIC X(01).
003240 77  WS-LINES	       PIC 9(04)     COMP-5 VALUE 0.
003250 77  WS-PAGE             PIC 9(04)     COMP-5 VALUE 0.
003260 77  WS-IND              PIC 9(01).
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003330 77  WS-H1	       PIC X(05)     VALUE "DATE:".
003340 77  WS-H3               PIC X(05)     VALUE "PAGE:".
002300 77  WS-HDNG	   PIC	X(118) VALUE
002310	   "ORD ITEM CODE           QUANTITY  X-REF/BAR CODE     DESCRIP
      -    "TION                     UNIT COST     TOTAL COST CONTACT".
002300 77  WS-HDNG2	    PIC X(128) VALUE
002310	   " ORD NUMBER ITEM CODE          QUANTITY X-REF/BAR CODE     D
      -    "ESCRIPTION                       UNIT COST   TOTAL COST ORDE
      -    "RED BY".
003700 77  WS-ER1	   PIC X(09)	 VALUE "No record".
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
002450	   03  WS-MD-LNE.
002460	       05  WS-MCR  PIC X(80) VALUE "³      ³
      - 	 "                     ³               ³             ³".
	   03  WS-MDCR-LNE REDEFINES WS-MD-LNE.
	       05  WS-MCR1     PIC  X(01).
	       05  WS-MCR78    PIC  X(78).
	       05  WS-MCR80    PIC  X(01).
002470	   03  WS-BT-LNE.
002480         05  WS-BCR  PIC X(01) OCCURS 80.
      /

003360 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "ORD".
	   03  WS-PROG	       PIC  X(03) VALUE "PRN".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003380 01  W02-FID.

       COPY APACFIDS.ID.

000340 COPY CONTROL.ID.

000340 COPY CREDIT.ID.

000400 COPY STOCK.ID.

       COPY PARAM.ID.

000440 COPY PORDER.ID.

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

	   03  R-L1 REDEFINES W02-PRN-LINE.
	       05  R-DET	   PIC X(140).

	   03  R-L2 REDEFINES W02-PRN-LINE.
	       05  R-H1 	   PIC X(06).
	       05  R-DTE1	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(18).
	       05  R-H2 	   PIC X(63).
	       05  FILLER	   PIC X(26).
	       05  R-H3 	   PIC X(06).
	       05  R-PGE	   PIC ZZ9.

	   03  R-L3 REDEFINES W02-PRN-LINE.
	       05  R-H4 	   PIC X(06).
	       05  R-H5 	   PIC X(34).
	       05  R-H6 	   PIC X(12).
	       05  R-H7 	   PIC X(12).
	       05  R-H8 	   PIC X(12).
	       05  R-H9 	   PIC X(12).
	       05  R-H10	   PIC X(11).
	       05  R-H11	   PIC X(12).
	       05  R-H12	   PIC X(12).
	       05  R-H13	   PIC X(09).

	   03  R-L4 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(64).
	       05  R-H14	   PIC X(12).
	       05  R-H15	   PIC X(12).
	       05  R-H16	   PIC X(12).
	       05  R-H17	   PIC X(12).
	       05  R-H18	   PIC X(12).
	       05  R-H19	   PIC X(08).

	   03  R-L5 REDEFINES W02-PRN-LINE.
	       05  CF-SNO	   PIC X(06).
	       05  FILLER	   PIC X(01).
	       05  CF-SUPP	   PIC X(29).
	       05  CF-BAL	   PIC Z(07)9.99-.
	       05  CF-AREAR	   PIC Z(07)9.99-.
	       05  CF-30D	   PIC Z(07)9.99-.
	       05  CF-60D	   PIC Z(07)9.99-.
	       05  CF-90D	   PIC Z(07)9.99-.
	       05  CF-120D	   PIC Z(07)9.99-.
	       05  CF-150D	   PIC Z(07)9.99-.
	       05  CF-180D	   PIC Z(07)9.99-.

	   03  R-L6 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(40).
	       05  R-H20	   PIC X(07).
	       05  R-H21	   PIC X(20).
	       05  R-H22.
		   07  FILLER	   PIC X(04).
		   07  R-H22A	   PIC X(16).
	       05  R-H23	   PIC X(16).
	       05  R-H24	   PIC X(14).
	       05  R-H25	   PIC X(15).

	   03  R-L7 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(05).
	       05  R-H26	   PIC X(07).
	       05  PJ-BN01	   PIC 9(02).
	       05  FILLER	   PIC X(01).
	       05  PJ-BRANCH1	   PIC X(25).
	       05  R-H27	   PIC X(17).
	       05  PJ-BN02	   PIC 9(02).
	       05  FILLER	   PIC X(01).
	       05  PJ-BRANCH2	   PIC X(24).
	       05  FILLER	   PIC X(48).

	   03  R-L8 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(40).
	       05  PJ-TRN	   PIC X(03).
	       05  FILLER	   PIC X(04).
	       05  PJ-GRNN	   PIC Z(05)9.
	       05  FILLER	   PIC X(01).
	       05  PJ-GRND	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(01).
	       05  PJ-INVN	   PIC Z(07)9.
	       05  FILLER	   PIC X(01).
	       05  PJ-INVD	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(03).
	       05  PJ-COST	   PIC Z(06)9.99DB.
	       05  FILLER	   PIC X(02).
	       05  PJ-SELL	   PIC Z(06)9.99DB.
	       05  FILLER	   PIC X(02).
	       05  PJ-URP	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(02).
	       05  PJ-PERC	   PIC Z(02)9.99.

	   03  R-L9 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(02).
	       05  R-H28	   PIC X(12).
	       05  R-H29	   PIC X(06).
	       05  R-H30	   PIC X(11).
	       05  FILLER	   PIC X(08).
	       05  R-H31	   PIC X(28).
	       05  R-H32	   PIC X(28).
	       05  R-H33	   PIC X(29).
	       05  R-H34	   PIC X(08).

	   03  R-L10 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(43).
	       05  R-H35	   PIC X(10).
	       05  R-H36	   PIC X(18).
	       05  R-H37	   PIC X(10).
	       05  R-H38	   PIC X(18).
	       05  R-H39	   PIC X(10).
	       05  R-H40	   PIC X(19).

	   03  R-L11 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(02).
	       05  CF-BNO	   PIC 9(02).
	       05  FILLER	   PIC X(03).
	       05  CF-BRANCH	   PIC X(24).
	       05  FILLER	   PIC X(07).
	       05  CF-SDEB	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(01).
	       05  CF-SCRE	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(07).
	       05  CF-CDEB	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(01).
	       05  CF-CCRE	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(07).
	       05  CF-URPD	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(01).
	       05  CF-URPC	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(08).
	       05  CF-PERC2	   PIC Z(02)9.99.
	       05  FILLER	   PIC X(03).

	   03  R-L12 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(31).
	       05  ST-H1	   PIC X(70).
	       05  FILLER	   PIC X(31).

	   03  R-L13 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(10).
	       05  ST-NADD	   PIC X(40).
	       05  FILLER	   PIC X(10).
	       05  ST-H2	   PIC X(12).
	       05  ST-ACNO	   PIC X(06).
	       05  FILLER	   PIC X(08).
	       05  ST-H3	   PIC X(06).
	       05  ST-DATE	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(08).
	       05  ST-H4	   PIC X(06).
	       05  ST-PAGE	   PIC Z(03)9.
	       05  FILLER	   PIC X(12).

	   03  R-L14 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(10).
	       05  ST-POST	   PIC 9(04).
	       05  FILLER	   PIC X(75).
	       05  ST-H5	   PIC X(26).
	       05  FILLER	   PIC X(17).

	   03  R-L15 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(05).
	       05  ST-H6	   PIC X(15).
	       05  ST-H7	   PIC X(12).
	       05  ST-H8	   PIC X(20).
	       05  ST-H9	   PIC X(20).
	       05  ST-H10	   PIC X(17).
	       05  ST-H11	   PIC X(29).
	       05  ST-H12	   PIC X(14).

	   03  R-L16 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(05).
	       05  ST-CODE	   PIC 9(02).
	       05  FILLER	   PIC X(01).
	       05  ST-TRN	   PIC X(08).
	       05  FILLER	   PIC X(04).
	       05  ST-ORD	   PIC 9(05)/99   BLANK WHEN ZERO.
	       05  FILLER	   PIC X(04).
	       05  ST-GRNN	   PIC Z(05)9	  BLANK WHEN ZERO.
	       05  FILLER	   PIC X(01).
	       05  ST-GRND	   PIC Z9/99/9999 BLANK WHEN ZERO.
	       05  FILLER	   PIC X(01).
	       05  ST-INVN	   PIC Z(07)9	  BLANK WHEN ZERO.
	       05  FILLER	   PIC X(01).
	       05  ST-INVD	   PIC Z9/99/9999 BLANK WHEN ZERO.
	       05  FILLER	   PIC X(03).
	       05  ST-COST	   PIC Z(06)9.99DB.
	       05  FILLER	   PIC X(05).
	       05  ST-NAR.
	       07  ST-30	   PIC X(01).
	       07  FILLER	   PIC X(03).
	       07  ST-60	   PIC X(01).
	       07  FILLER	   PIC X(03).
	       07  ST-90	   PIC X(01).
	       07  FILLER	   PIC X(03).
	       07  ST-120	   PIC X(01).
	       07  FILLER	   PIC X(03).
	       07  ST-150	   PIC X(01).
	       07  FILLER	   PIC X(03).
	       07  ST-180	   PIC X(01).
	       07  FILLER	   PIC X(05).
	       07  ST-BAL	   PIC Z(06)9.99DB.
	       07  FILLER	   PIC X(05).

	   03  R-L17 REDEFINES W02-PRN-LINE.
	       05  ST-H13	   OCCURS 12.
		   07  FILLER	   PIC X(05).
		   07  ST-H14	   PIC X(06).

	   03  R-L18 REDEFINES W02-PRN-LINE.
	       05  ST-DUE	   OCCURS 12.
		   07  ST-VAL	   PIC Z(06)9.99-.

	   03  R-L19 REDEFINES W02-PRN-LINE.
	       05  A-H1 	   PIC X(06).
	       05  A-H2 	   PIC X(41).
	       05  A-H3 	   PIC X(61).
	       05  A-H4 	   PIC X(05).
	       05  A-H5 	   PIC X(19).

	   03  R-L20 REDEFINES W02-PRN-LINE.
	       05  A-ACNO	   PIC X(06).
	       05  FILLER	   PIC X(01).
	       05  A-NAME	   PIC X(39).
	       05  FILLER	   PIC X(01).
	       05  A-ADDRESS	   PIC X(60).
	       05  FILLER	   PIC X(01).
	       05  A-PC 	   PIC 9(04).
	       05  FILLER	   PIC X(01).
	       05  A-30 	   PIC 9(01).
	       05  FILLER	   PIC X(02).
	       05  A-60 	   PIC 9(01).
	       05  FILLER	   PIC X(02).
	       05  A-90 	   PIC 9(01).
	       05  FILLER	   PIC X(03).
	       05  A-120	   PIC 9(01).
	       05  FILLER	   PIC X(03).
	       05  A-150	   PIC 9(01).
	       05  FILLER	   PIC X(03).
	       05  A-180	   PIC 9(01).

	   03  R-L21 REDEFINES W02-PRN-LINE.
	       05  OR-H1	   PIC X(12).
	       05  OR-H2.
		   07  FILLER	   PIC X(04).
		   07  OR-H2A	   PIC X(27).
	       05  OR-H3	   PIC X(06).
	       05  OR-H4	   PIC X(12).
	       05  OR-H5	   PIC X(10).
	       05  OR-H6	   PIC X(09).
	       05  OR-H7	   PIC X(22).
	       05  OR-H8	   PIC X(13).
	       05  OR-H9	   PIC X(09).
	       05  OR-H10	   PIC X(08).

	   03  R-L22 REDEFINES W02-PRN-LINE.
	       05  OR-ORD	   PIC Z(07)9/99.
	       05  FILLER	   PIC X(01).
	       05  OR-DESC.
		   07  OR-ITEM	   PIC	X(14).
		   07  OR-SL	   PIC	X(01).
		   07  OR-ITM	   PIC	X(03).
		   07  FILLER	   PIC	X(12).
	       05  FILLER	   PIC X(01).
	       05  OR-QUANT	   PIC Z(04)9.
	       05  FILLER	   PIC X(01).
	       05  OR-CURRENCY.
		   07  FILLER	   PIC X(01).
		   07  OR-RATE	   PIC Z(02)9.99.
		   07  FILLER	   PIC X(03).
	       05  FILLER	   PIC X(01).
	       05  OR-COST	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(01).
	       05  OR-PMT.
		   07  FILLER	   PIC X(01).
		   07  OR-DISC	   PIC Z9.99 BLANK WHEN ZERO.
		   07  FILLER	   PIC X(02).
	       05  FILLER	   PIC X(01).
	       05  OR-NAME.
		   07  OR-CONTACT.
		     09  OR-ACNO   PIC X(06).
		     09  FILLER    PIC X(15).
		   07  FILLER	   PIC X(01).
		   07  OR-TEL	   PIC X(12).
	       05  FILLER	   PIC X(01).
	       05  OR-DEL	   PIC Z9/99/99.
	       05  FILLER	   PIC X(01).
	       05  OR-RMKS.
		   07  OR-PSLIP    PIC Z(08).

	   03  R-L23 REDEFINES W02-PRN-LINE.
	       05  REP-PG	   PIC X(06).
	       05  REP-P-N	   PIC Z(03)9.
	       05  FILLER	   PIC X(16).
	       05  REP-COMP	   PIC X(40).
	       05  FILLER	   PIC X(02).
	       05  REP-REPORT	   PIC X(40).
	       05  FILLER	   PIC X(08).
	       05  REP-DAT	   PIC X(06).
	       05  REP-DT	   PIC Z9/99/9999.

	   03  PD-L1 REDEFINES W02-PRN-LINE.
	       05  PD-H1	   PIC X(06).
	       05  PD-DTE1	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(04).
	       05  PD-H2	   PIC X(40).
	       05  FILLER	   PIC X(11).
	       05  PD-H3	   PIC X(06).
	       05  PD-PGE	   PIC ZZ9.

	   03  PD-L2 REDEFINES W02-PRN-LINE.
	       05  PD-H4	   PIC	X(06).
	       05  PD-H5.
		   07  FILLER	   PIC	X(06).
		   07  PD-H6.
		       09  FILLER  PIC	X(01).
		       09  PD-H7   PIC	X(06).
		   07  PD-H8	   PIC	X(41).
	       05  PD-H9	   PIC	X(15).
	       05  PD-H10	   PIC	X(05).

	   03  PD-L3 REDEFINES W02-PRN-LINE.
	       05  PD-AC	   PIC	X(06).
	       05  FILLER	   PIC	X(01).
	       05  PD-SUPPLIER	   PIC	X(40).
	       05  FILLER	   PIC	X(33).

	   03  PD-L4 REDEFINES W02-PRN-LINE.
	       05  PD-DATE1	   PIC	Z9/99/9999.
	       05  FILLER	   PIC	X(01).
	       05  PD-DTE-AC.
		   07  PD-DATE2  PIC  Z9/99/9999.
	       05  PD-AC1 REDEFINES PD-DTE-AC.
		   07  FILLER	 PIC  X(02).
		   07  PD-AC2	 PIC  X(06).
		   07  FILLER	 PIC  X(02).
	       05  FILLER	 PIC  X(01).
	       05  PD-SUPP-NAR	 PIC  X(37).
	       05  FILLER	 PIC  X(01).
	       05  PD-REF	 PIC  Z(06)9.
	       05  FILLER	 PIC  X(01).
	       05  PD-VAL	 PIC  Z(08)9.99.

000710	   03  R-L24 REDEFINES W02-PRN-LINE.
	       05  O-HDNG.
		   07  FILLER	   PIC	X(01).
000720		   07  O-LINE	   PIC	99.
		   07  FILLER	   PIC	X(01).
		   07  O-EXT-ITEM.
000740		     09	 O-ITEM	   PIC	X(14).
000750		     09	 O-SL	   PIC	X(01).
		     09	 O-ITM	   PIC	X(03).
		   07  FILLER	   PIC	X(03).
000780		   07  O-QNT	   PIC	Z(05)9.
		   07  FILLER	   PIC	X(03).
		   07  O-XREF	   PIC	X(18).
		   07  FILLER	   PIC	X(01).
000760		   07  O-DESC	   PIC	X(30).
		   07  FILLER	   PIC	X(01).
000800		   07  O-COST	   PIC	Z(07)9.999.
		   07  FILLER	   PIC	X(01).
000820		   07  O-EXT	   PIC	Z(08)9.99.
		   07  FILLER	   PIC	X(01).
000840		   07  O-CONTACT   PIC	X(22).

000590	   03  R-L25 REDEFINES W02-PRN-LINE.
	       05  REP-EXPNAME	   PIC	X(40).
	       05  FILLER	   PIC	X(92).

	   03  R-L26 REDEFINES W02-PRN-LINE.
000930	       05  REP-NADD	   PIC	X(44).
000940	       05  REP-RADD	 REDEFINES REP-NADD.
000950		   07  REP-PC	   PIC	X(04).
000960		   07  FILLER	   PIC	X(40).
	       05  FILLER	   PIC	X(40).
001120	       05  REP-H2	   PIC	X(16).
	       05  REP-SMAN.
001130		   07  REP-TEL	   PIC	X(14).
		   07  REP-GNO REDEFINES REP-TEL
				   PIC	X(14).
		   07  FILLER	   PIC	X(09).
		   07  O2-PAGE	   PIC	X(06).
		   07  O2-PGNO	   PIC	ZZ9.

000980	   03  R-L27 REDEFINES W02-PRN-LINE.
001000	       05  REP-NAME	   PIC	X(40).
001010	       05  REP-DADD	   REDEFINES REP-NAME.
001020		   07  REP-PC1	   PIC	X(04).
001030		   07  FILLER	   PIC	X(36).
001040	       05  FILLER	   PIC	X(12).
	       05  REP-H3	   PIC	X(08).
001060	       05  REP-DADD2.
001070		   07  REP-PC2	   PIC	X(30).
001090	       05  FILLER	   PIC	X(02).
	       05  REP-H4	   PIC	X(17).
	       05  REP-STEL.
001550		   07  REP-ORD.
		     09  REP-TERMS.
		       11  FILLER  PIC	X(05).
		       11  REP-AC.
			 13  FILLER  PIC  X(03).
			 13  REP-PGE PIC  ZZ9.
		     09  FILLER    PIC	X(01).
		   07  FILLER	   PIC	X(02).
	       05  FILLER	   PIC	X(01).

	   03  R-L28 REDEFINES W02-PRN-LINE.
000800	       05  REP-H1	   PIC	X(23).
000810	       05  REP-REF	   PIC	9(08).
000860	       05  FILLER	   PIC	X(49).

	   03  R-L29 REDEFINES W02-PRN-LINE.
001540	       05  FILLER	   PIC	X(17).
	       05  REP-INVOICE.
		   07  FILLER	   PIC	X(03).
		   07  REP-CRNOTE.
		       09  FILLER  PIC	X(06).
		       09  REP-COPY
				   PIC	X(12).
		       09  FILLER  PIC	X(06).
		   07  FILLER	   PIC	X(03).
	       05  FILLER	   PIC	X(01).
	       05  REP-H14	   PIC	X(08).
	       05  REP-DTE	   PIC	99/99/9999.

000710	   03  R-L30 REDEFINES W02-PRN-LINE.
	       05  O2-HDNG.
000720		   07  O2-ORD	   PIC	Z(07)9.
000720		   07  O2-SL	   PIC	X(01).
000720		   07  O2-LINE	   PIC	9(02).
000730		   07  FILLER	   PIC	X(01).
		   07  O2-EXT-ITEM.
000740		     09	 O2-ITEM   PIC	X(14).
000750		     09	 O2-SL2	   PIC	X(01).
		     09	 O2-ITM	   PIC	X(03).
		   07  FILLER	   PIC	X(02).
000780		   07  O2-QNT	   PIC	Z(05)9.
		   07  FILLER	   PIC	X(02).
		   07  O2-XREF	   PIC	X(18).
		   07  FILLER	   PIC	X(01).
000760		   07  O2-DESC	   PIC	X(30).
		   07  FILLER	   PIC	X(01).
		   07  O2-HDV.
000800		     09	 O2-COST   PIC	Z(07)9.999.
		   07  FILLER	   PIC	X(01).
000820		   07  O2-EXT	   PIC	Z(08)9.99.
		   07  FILLER	   PIC	X(01).
		   07  O2-SMAN	   PIC	X(20).

       COPY SYSUSE.ID.

       COPY W05.WS.

       COPY W05.VAT.

003820 01  W10-MONTHS.
003830     03  W10-MONTH-NAMES PIC X(36)     VALUE
003840                         "JanFebMarAprMayJunJulAugSepOctNovDec".
003850     03  W10-MONTH       REDEFINES W10-MONTH-NAMES
003860                         PIC X(03)     OCCURS 12.
       01  W10-STOCK.
002850     03  W10-VSELL      PIC S9(07)V99  COMP-3.
002860     03  W10-VCASH      PIC S9(07)V99  COMP-3.
002860     03  W10-VWSALE     PIC S9(07)V99  COMP-3.
	   03  W10-NAME.
	       05  W10-N1-2    PIC  X(02).
	       05  W10-N3-4    PIC  X(02).
	       05  W10-N5-6    PIC  X(02).
	       05  W10-N7-8    PIC  X(02).
	       05  W10-N9-10   PIC  X(02).
	       05  W10-N11-12  PIC  X(02).
	       05  W10-N13-14  PIC  X(02).
	       05  W10-N15-16  PIC  X(02).
	       05  W10-N17-18  PIC  X(02).
	       05  W10-N19-20  PIC  X(02).
	       05  W10-N21-22  PIC  X(02).
	       05  W10-N23-24  PIC  X(02).
	       05  W10-N25-26  PIC  X(02).
	       05  W10-N27-28  PIC  X(02).
	       05  W10-N29-30  PIC  X(02).

       COPY W15.CRP.
	   03  W15-EXT-ITEM.
000040	       05  W15-ITEM	 PIC  X(14).
000060	       05  W15-ITM	 PIC  X(03).
	       05  FILLER	 PIC  X(01).
	   03  W15-YMD	     PIC  9(08).
           03  W15-RYMD REDEFINES W15-YMD.
	       05  W15-CN    PIC  9(02).
	       05  W15-YR    PIC  9(02).
	       05  W15-MT    PIC  9(02).
	       05  W15-DY    PIC  9(02).

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
004540	   03  W25-V1	       PIC S9(09)V99 COMP-3.
004550	   03  W25-V2	       PIC S9(09)V99 COMP-3.
004540	   03  W25-TV1	       PIC S9(09)V99 COMP-3.
004550	   03  W25-TV2	       PIC S9(09)V99 COMP-3.

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
004660	       05  W25-DATE    PIC 99/99/9999.
004670     03  W25-6.
004680         05  W25-NO      PIC Z(05)9.
004690     03  W25-5V2.
004700         05  W25-VAL     PIC Z(04)9.99.
004710     03  W25-S5V2.
004720         05  W25-SVAL    PIC Z(04)9.99DB.
004730     03  W25-S7V2.
004740         05  W25-VAL2    PIC Z(06)9.99DB.


004930 01  W40-COMPANY.
005090	     05  W40-ADDRESS1.
005100	       07  W40-ADD1    PIC  X(30).
005110	       07  W40-ADD2    PIC  X(30).
005120	       07  W40-ADD3    PIC  X(30).
005130	       07  W40-ADD4    PIC  X(30).
005140       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
005150         07  W40-ACHAR OCCURS 120 PIC X(01).
005160	     05  W40-PC1       PIC  X(04).
	     05  W40-TEL       PIC  X(14).
	     05  W40-FAX       PIC  X(14).

       COPY W40.WS.

       COPY FUNCTION.WS.

004800 COPY W50.WS.

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

       01  W75-KEYS.
	   03  W75-S	       PIC 9(02) COMP-5.
	   03  W75-S1	       PIC 9(02) COMP-5.
	   03  W75-S2	       PIC 9(02) COMP-5.
	   03  W75-CNO	       PIC X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
	       05  W75-CODE.
		   07  W75-POR PIC  9(10).
		   07  FILLER  PIC  X(08).

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

006860 01  W95-STM.
006870     03  W95-COMP        PIC X(40).
006880     03  W95-ADD1        PIC X(30).
006890     03  W95-ADD2        PIC X(30).
006900     03  W95-ADD3        PIC X(30).
006910     03  W95-ADD4        PIC X(30).
006920     03  W95-PC.
006930         05  W95-POST    PIC 9(04).
006940     03  W95-TEL         PIC X(14).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
005320 SCREEN SECTION.

       COPY BLANK.CRT.

007490 01  S21.
007500	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC Z(07)9 USING W15-OPREF AUTO.

007510 01  S22.
007520	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC 99 USING W15-OSUB AUTO.

       COPY CRDLUP.CRT.

       01  S-POR.
	   03  LINE  6 COLUMN 9 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  7 COLUMN 9 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³                     O R D E R
      -	       "   L O O K U P                      ³".
	   03  LINE  8 COLUMN 9 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ´".
	   03  LINE 21 COLUMN 9 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ".

       01  SD-POR.
	   03  COLUMN 10 FOREGROUND-COLOR 5
			 BACKGROUND-COLOR 7 PIC Z(07)9 FROM ORD-ORD.
	   03  COLUMN 18 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
			 VALUE "/".
	   03  COLUMN 19 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
			 PIC 99 FROM ORD-LINE.
	   03  COLUMN 22 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
			 PIC X(14) FROM STK-CODE.
	   03  COLUMN 37 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
			 PIC X(30) FROM STK-DESC.
	   03  COLUMN 68 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
			 PIC Z(04)9.99 FROM ORD-QUANT.

       COPY S99.CRT.

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
	     PERFORM GA000.
005970	     GO TO AA100.

005980 AA050.
	     MOVE "H"		 TO W02-PRN-TYPE.
005990	     MOVE SPACES	 TO R-DET.
006000	     MOVE WS-H1 	 TO R-H1.
006010	     MOVE W60-H2	 TO R-H2.
006020	     MOVE WS-H3 	 TO R-H3.
006030	     ADD 1		 TO WS-PAGE.
006040	     MOVE W05-DATE	 TO R-DTE1.
006050	     MOVE WS-PAGE	 TO R-PGE.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
006080	     MOVE SPACES	 TO R-DET.

006090 AA100.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
006180	     DISPLAY CLR-SCREEN.

       AA105.
	     CLOSE SYSUSER.
             EXIT PROGRAM.

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

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP4.

      /
006460 AB00-READ SECTION.

       COPY CONTROL.RD.

       COPY STOCK.RD.

       COPY CREDIT.RD.

       COPY PARAM.RD.

       COPY PORDER.RD.

       COPY SYSUSE.RD.

       COPY CREDIT.ILU.

       COPY APAC.HLP.

       BB010	       SECTION.
       BB10.
	     MOVE W15-N29-30	 TO W10-N1-2.
	     MOVE W15-N1-2	 TO W10-N3-4.
	     MOVE W15-N27-28	 TO W10-N5-6.
	     MOVE W15-N3-4	 TO W10-N7-8.
	     MOVE W15-N25-26	 TO W10-N9-10.
	     MOVE W15-N5-6	 TO W10-N11-12.
	     MOVE W15-N23-24	 TO W10-N13-14.
	     MOVE W15-N7-8	 TO W10-N15-16.
	     MOVE W15-N21-22	 TO W10-N17-18.
	     MOVE W15-N9-10	 TO W10-N19-20.
	     MOVE W15-N19-20	 TO W10-N21-22.
	     MOVE W15-N11-12	 TO W10-N23-24.
	     MOVE W15-N17-18	 TO W10-N25-26.
	     MOVE W15-N13-14	 TO W10-N27-28.
	     MOVE W15-N15-16	 TO W10-N29-30.

       BB15.
	     EXIT.

      /
014820 GA000	     SECTION 5.
014830 GA00.
014840       DISPLAY CLR-SCREEN.
014880       DISPLAY "OUTSTANDING ORDERS" AT 0231
                      WITH FOREGROUND-COLOR 15.
	     DISPLAY "C" AT 0412 WITH FOREGROUND-COLOR 6 HIGHLIGHT
                     "reditor or " WITH FOREGROUND-COLOR 7
                     "O" WITH FOREGROUND-COLOR 14
		     "rder no. sequence or " WITH FOREGROUND-COLOR 7
		     "S" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "elective per Creditor" WITH FOREGROUND-COLOR 7.
	     MOVE "S"		 TO WS-OPTION.
	     DISPLAY "Esc" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "ape to Exit" WITH FOREGROUND-COLOR 1
					BACKGROUND-COLOR 3.
       GA00A.
	     ACCEPT WS-OPTION AT 0469
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GA00A
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "C" OR "O" OR "S")
               GO TO GA00A.
	   IF WS-OPTION = "C" OR "O"
014850	      DISPLAY "Busy printing outstanding orders" AT 0612.
           IF WS-OPTION = "O"
               DISPLAY "Order No." AT 0812
	   ELSE
	   IF WS-OPTION = "C"
	       PERFORM GC000
	       GO TO GA00
	   ELSE
	       PERFORM GB000
	       GO TO GA00.
             MOVE WS-OPTION      TO WS-ORD.
014870	     MOVE ZERO		 TO WS-PAGE.
	     MOVE ZERO		 TO W25-V1 W25-V2.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

014910 GA10.
	     MOVE "H"		 TO W02-PRN-TYPE.
008540       MOVE SPACES         TO R-DET.
008550       MOVE "Page:"        TO REP-PG.
008560       MOVE W60-COMPANY    TO REP-COMP.
008570       MOVE "Date:"        TO REP-DAT.
008580	     ADD 1		 TO WS-PAGE.
008590       MOVE W05-DATE       TO REP-DT.
008600       MOVE WS-PAGE        TO REP-P-N.
032210       MOVE "OUTSTANDING - STOCK PURCHASE ORDERS" 
                                 TO REP-REPORT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
008650	     MOVE SPACES	 TO R-DET.
032320	     MOVE "ORDER.NO"	 TO OR-H1.
032330	     MOVE "ITEM CODE"	 TO OR-H2.
032350	     MOVE "QUANT"	 TO OR-H3.
	     MOVE "CURRENCY"	 TO OR-H4.
032360	     MOVE "UNIT COST"	 TO OR-H5.
	     MOVE "PAYMENT"	 TO OR-H6.
032370	     MOVE "CREDITOR"	 TO OR-H7.
	     MOVE "FAX NO"	 TO OR-H8.
032380	     MOVE "DELIVERY"	 TO OR-H9.
	     MOVE "PCK-SLIP"	 TO OR-H10.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
032410	     MOVE SPACES	 TO R-DET.
	     MOVE "/DESCRIPTION" TO OR-H2A.
	     MOVE " /RATE"	 TO OR-H4.
	     MOVE "  /RAND"	 TO OR-H5.
	     MOVE " /DISC"	 TO OR-H6.
             MOVE "/NAME/CONTACT"
				 TO OR-H7.
	     MOVE "/TELEPHONE"	 TO OR-H8.
032390	     MOVE "/REMARKS"	 TO OR-H10.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
032410	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

015030 GA20.
	     MOVE 1		 TO ORD-ORD.
	     MOVE ZERO		 TO ORD-LINE.
	     PERFORM START-AT-ORDER-NO THRU READ-PORDER-EXIT.
015050	   IF NOT (WS-F-ERROR = ZERO)
015060         GO TO GA70.
             MOVE SPACES         TO W15-ACNO.

015070 GA30.
015080       PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
015090	   IF NOT (WS-F-ERROR = ZERO)
015100         GO TO GA70.          
           IF ORD-CRED = W15-ACNO
               GO TO GA40.
           IF W15-ACNO NOT = SPACES
015110         MOVE SPACES       TO R-DET   
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
014990         GO TO GA75.
	     MOVE ORD-CRED	 TO P-NUMBER W15-ACNO.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
           IF WS-ORD = "C"
	       DISPLAY P-NUMBER AT 0822
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5 "°"
		       P-NAME WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 5.
015170 GA40.
015190     IF ORD-QUANT = ZERO
015200         GO TO GA30.          
015290       MOVE SPACES         TO R-DET.

015310 GA45. 
             MOVE ORD-ORD        TO W15-OPREF.
             MOVE ORD-LINE       TO W15-OSUB.
015320	     MOVE ORD-EXT-ITEM	 TO STK-CODE W15-EXT-ITEM.
015330       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
034810       MOVE STK-DESC       TO W15-DESCR.
034820	     MOVE ORD-QUANT	 TO W15-UN.
034830       MOVE ORD-DELIV      TO W15-DEL.
034840       MOVE ORD-UCOST      TO W15-COST.
             MOVE ORD-SELL       TO W15-SELL.
             MOVE ORD-OCOST      TO W15-OCOST.
             MOVE ORD-COST       TO W15-TCOST.
	     ADD ORD-QUANT	 TO W25-V1.
	     COMPUTE W25-V2 = W25-V2 + (ORD-QUANT * ORD-UCOST).
	     MOVE ORD-FAX	 TO W15-FAX.
             MOVE ORD-TEL        TO W15-TEL.
             MOVE ORD-DRATE      TO W15-RATE.
             MOVE ORD-CONTACT    TO W15-CONTACT.
             MOVE ORD-PMT        TO W15-PMT.
             MOVE ORD-INST       TO W15-RMKS.
             MOVE ORD-DISC       TO W15-DISC1.
             MOVE ORD-PSLIP      TO W15-PSLIP.
             MOVE ORD-CURRENCY   TO W15-CURRENCY.
033850       MOVE ORD-TERMS      TO W15-SETT.
034260	     MOVE W15-ORD	 TO OR-ORD.
           IF WS-ORD = "O"
	       DISPLAY OR-ORD AT 0822
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
034270	     MOVE W15-EXT-ITEM	 TO OR-DESC.
034300	     MOVE W15-UN	 TO OR-QUANT.
	     MOVE W15-CURRENCY	 TO OR-CURRENCY.
034310	     MOVE W15-OCOST	 TO OR-COST.
	     MOVE W15-PMT	 TO OR-PMT.
034280	     MOVE W15-ACNO	 TO OR-ACNO.
             MOVE W15-FAX        TO OR-TEL.
034370       MOVE W15-DEL        TO OR-DEL.
             MOVE W15-PSLIP      TO OR-PSLIP.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
014990         GO TO GA75.
034450	     MOVE SPACES	 TO R-DET.
034290       MOVE W15-DESCR      TO OR-DESC.
             MOVE W15-RATE       TO OR-RATE.
034310       MOVE W15-COST       TO OR-COST.
             MOVE W15-DISC1      TO OR-DISC.
             MOVE P-NAME         TO OR-NAME.
             MOVE W15-RMKS       TO OR-RMKS.
	     PERFORM CALL-PRINTUTL.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
014990         GO TO GA75.
034450	     MOVE SPACES	 TO R-DET.
             MOVE W15-CONTACT    TO OR-CONTACT.
             MOVE W15-TEL        TO OR-TEL.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
034440	       PERFORM GA10.
015510       MOVE SPACES         TO R-DET.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
014990         GO TO GA75.

015530 GA50.
015570       GO TO GA30.

015620 GA70.   
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
034450	     MOVE SPACES	 TO R-DET.
	     MOVE "******  TOTAL ORDERS -   Items"
				 TO O2-DESC.
	     MOVE "    Value"	 TO O2-HDV.
	     MOVE W25-V1	 TO O2-QNT.
	     MOVE W25-V2	 TO O2-EXT.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
034450	     MOVE SPACES	 TO R-DET.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015640	     MOVE "******  END OF REPORT  ******"
				 TO R-DET.

       GA75.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.

015680 GA999.
015690       EXIT.

       GB000	     SECTION.
       GB00.
042000	     DISPLAY CLR-SCREEN.
	     DISPLAY "PRINT SELECTIVE ORDERS" AT 0230
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Creditor No." AT 0404.
	     MOVE SPACES	 TO W15-ACNO.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Creditor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     MOVE ZERO		 TO W25-V1 W25-V2.

       GB05.
	     ACCEPT W15-ACNO AT 0417
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
011080		     MOVE 4	 TO W44-FUNCTION
011090		     PERFORM SCREEN-CONTENTS
011180		     PERFORM CREDITOR-LOOKUP
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB05
	       END-EVALUATE
018820	       DISPLAY W15-ACNO AT 0417
		       WITH BACKGROUND-COLOR 5
			    FOREGROUND-COLOR 7 HIGHLIGHT
018830	       IF W15-ACNO = SPACES
018840		   GO TO GB05.
	     MOVE 5		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W15-ACNO = SPACES
	       GO TO GB999.
	     DISPLAY CLEAR-L25.
	     MOVE W15-ACNO	 TO P-NUMBER.
033630	     PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
	   IF WS-F-ERROR = 3
	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GB05.
	     DISPLAY P-NAME AT 0424
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
012180	     PERFORM CHECK-CORRECT.
014160	   IF WS-OPTION = "N"
014170	       GO TO GB00.
	     DISPLAY "A" AT 0604 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ll or " WITH FOREGROUND-COLOR 7
		     "S" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "elective orders" WITH FOREGROUND-COLOR 7.
	     MOVE "A"		 TO WS-OPTION.
	     DISPLAY "Esc" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "ape to Exit" WITH FOREGROUND-COLOR 1
					BACKGROUND-COLOR 3.

       GB06.
	     ACCEPT WS-OPTION AT 0628
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB06
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "A" OR "S")
	       GO TO GB06.
	     DISPLAY CLEAR-L25.
	   IF WS-OPTION = "A"
	       MOVE P-NUMBER	 TO ORD-CRED
	       MOVE SPACES	 TO ORD-EXT-ITEM
	       GO TO GB30.
	     DISPLAY "Order No.   " AT 0804.

       GB10.
	     MOVE ZERO		 TO W15-OPREF.
	     DISPLAY  "F2" AT 2502 WITH BACKGROUND-COLOR 3
					FOREGROUND-COLOR 6 HIGHLIGHT
		      "=View Orders or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
014980	     ACCEPT S21 AT 0817.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ORD
		     DISPLAY "        " AT 0817
			      WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 5
		 WHEN F2-KEY
018810		     PERFORM PORDER-LOOKUP
		     IF W15-ORD = ZERO
			 GO TO GB10
		     END-IF
014980		     DISPLAY S21 AT 0817
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB10
	       END-EVALUATE.
014990	   IF W15-OPREF = ZERO
	       GO TO GB00.
015040	     DISPLAY CLEAR-L25.
	     MOVE W15-OPREF	 TO ORD-ORD.
	     MOVE ZERO		 TO ORD-LINE.
	     PERFORM START-AT-ORDER-NO THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
015190	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015200	       GO TO GB10.
	     PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
	   IF NOT (W15-OPREF = ORD-ORD)
	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015200	       GO TO GB10.
	   IF NOT (P-NUMBER = ORD-CRED)
	       MOVE "Invalid order number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GB10.
014060	     UNSTRING P-ADDR DELIMITED BY "*"
014070				 INTO W40-ADD1
014080				      W40-ADD2
014090				      W40-ADD3
014100				      W40-ADD4
014110       OVERFLOW
014120           MOVE 1          TO WS-ERROR.
	     MOVE P-TEL 	 TO W40-TEL.
	     MOVE P-FAX 	 TO W40-FAX.

       GB15.
	     MOVE ORD-ORD	 TO W15-OPREF.
	     MOVE ZERO		 TO W15-OSUB.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
017710	     MOVE SPACES	 TO R-DET.
017750	     MOVE "PURCHASE ORDER NUMBER:"
                                 TO REP-H1.
	     MOVE W15-OPREF	 TO REP-REF.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017750	     MOVE W95-COMP	 TO REP-EXPNAME.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DET.
017770	     MOVE W95-ADD1	 TO REP-NADD.
026690	   IF W05-VATNO NOT = SPACES
026700	       MOVE "VAT No/BTW Nr :"
				 TO REP-H2
026710	       MOVE W05-VATNO	 TO REP-GNO.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017790	     MOVE SPACES	 TO R-DET.
017800	     MOVE W95-ADD2	 TO REP-NADD.
017910	   IF W95-TEL NOT = SPACES
017920	       MOVE "TELEPHONE/FOON:"
				 TO REP-H2
017930	       MOVE W95-TEL	 TO REP-TEL.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
017820	     MOVE W95-ADD3	 TO REP-NADD.
	     MOVE "ORD.BY/BEST.DR:"
				 TO REP-H2.
	     MOVE ORD-DRATE	 TO USE-NO.
	   IF NOT (USE-NO = ZERO)
	       PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT
	       MOVE USE-NAME	 TO W15-NAME
	       PERFORM BB010
	       MOVE W10-NAME	 TO REP-SMAN.
	     PERFORM CALL-PRINTUTL.
017870	     MOVE SPACES	 TO R-DET.
017840	     MOVE W95-ADD4	 TO REP-NADD.
	     PERFORM CALL-PRINTUTL.
017870	     MOVE SPACES	 TO R-DET.
017880	     MOVE W95-PC	 TO REP-PC.
	     PERFORM CALL-PRINTUTL.
017900	     MOVE SPACES	 TO R-DET.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE "DATE  :"	 TO REP-H14.
026670	     MOVE ORD-DATE	 TO W15-YMD.
	     MOVE W15-DY	 TO W15-DD.
	     MOVE W15-MT	 TO W15-MM.
	     MOVE W15-CN	 TO W15-CC.
	     MOVE W15-YR	 TO W15-YY.
	     MOVE W15-DATE	 TO REP-DTE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
026320	     MOVE SPACES	 TO R-DET.
026330	     MOVE P-NAME	 TO REP-NAME.
	     MOVE "SUPPLIER NUMBER : "
				 TO REP-H4.
026550	     MOVE ORD-CRED	 TO REP-AC.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026350	     MOVE W40-ADD1	 TO REP-NAME.
	     MOVE "SUPPLIER TEL.NO : "
				 TO REP-H4.
	     MOVE W40-TEL	 TO REP-STEL.
	     PERFORM CALL-PRINTUTL.
026380	     MOVE W40-ADD2	 TO REP-NAME.
	     MOVE "SUPPLIER FAX.NO : "
				 TO REP-H4.
	     MOVE W40-FAX	 TO REP-STEL.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026410	     MOVE W40-ADD3	 TO REP-NAME.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026440	     MOVE W40-ADD4	 TO REP-NAME.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
026510	     MOVE P-PC		 TO REP-PC1.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	     MOVE WS-HDNG	 TO O-HDNG.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	     MOVE SPACES	 TO W15-CONTACT.

       GB20.
	     MOVE ORD-LINE	 TO O-LINE.
	     MOVE ORD-EXT-ITEM	 TO O-EXT-ITEM STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     MOVE STK-DESC	 TO O-DESC.
	     MOVE STK-XREF	 TO O-XREF.
	     MOVE ORD-QUANT	 TO O-QNT.
	     MOVE ORD-UCOST	 TO O-COST.
	     COMPUTE O-EXT = ORD-QUANT * ORD-UCOST.
	     ADD ORD-QUANT	 TO W25-V1.
	     COMPUTE W25-V2 = W25-V2 + (ORD-QUANT * ORD-UCOST).
	   IF NOT (ORD-CONTACT = W15-CONTACT)
	       MOVE ORD-CONTACT  TO O-CONTACT W15-CONTACT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE ZERO	 TO WS-IND
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 1		 TO WS-IND.
	     MOVE SPACES	 TO R-DET.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DET
014990	       GO TO GB25.
	     PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
	   IF WS-F-ERROR = 16
	       GO TO GB25.
	   IF NOT (W15-OPREF = ORD-ORD)
	       GO TO GB22.
	   IF WS-IND = 1
	       PERFORM GB15.
	     GO TO GB20.

       GB22.
	     MOVE SPACES	 TO R-DET.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "**** Total for Order"
				 TO O-DESC.
	     MOVE W25-V1	 TO O-QNT.
	     MOVE W25-V2	 TO O-EXT.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE ZERO		 TO W25-V1 W25-V2.

       GB25.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO WS-COMMAND.
	     GO TO GB00.

       GB30.
	     MOVE SPACES	 TO W15-CONTACT.
	     MOVE ZERO		 TO WS-PAGE.
	     PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
	   IF WS-F-ERROR = 16
	       GO TO GB50.
014060	     UNSTRING P-ADDR DELIMITED BY "*"
014070				 INTO W40-ADD1
014080				      W40-ADD2
014090				      W40-ADD3
014100				      W40-ADD4
014110       OVERFLOW
014120           MOVE 1          TO WS-ERROR.
	     MOVE P-TEL 	 TO W40-TEL.
	     MOVE P-FAX 	 TO W40-FAX.

       GB35.
	     PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
	   IF WS-F-ERROR = 16
	       GO TO GB50.
	   IF NOT (ORD-CRED = P-NUMBER)
	       GO TO GB50.
	   IF (WS-PAGE = ZERO) OR
	      (WS-IND = 1)
	       PERFORM GB45.

       GB40.
	     MOVE ORD-ORD	 TO O2-ORD.
	     MOVE "/"		 TO O2-SL.
	     MOVE ORD-LINE	 TO O2-LINE.
	     MOVE ORD-EXT-ITEM	 TO O2-EXT-ITEM STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     MOVE STK-XREF	 TO O2-XREF.
	     MOVE STK-DESC	 TO O2-DESC.
	     MOVE ORD-QUANT	 TO O2-QNT.
	     MOVE ORD-UCOST	 TO O2-COST.
	     COMPUTE O2-EXT = ORD-QUANT * ORD-UCOST.
	     MOVE ORD-DRATE	 TO USE-NO.
	     ADD ORD-QUANT	 TO W25-V1.
	     COMPUTE W25-V2 = W25-V2 + (ORD-QUANT * ORD-UCOST).
	   IF NOT (USE-NO = ZERO)
	       PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT
	       MOVE USE-NAME	 TO W15-NAME
	       PERFORM BB010
	       IF NOT (W10-NAME = W15-CONTACT)
		   MOVE W10-NAME TO O2-SMAN W15-CONTACT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE ZERO	 TO WS-IND
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 1		 TO WS-IND.
	     MOVE SPACES	 TO R-DET.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
	       MOVE SPACES	 TO WS-COMMAND
014990	       GO TO GB55.
	     GO TO GB35.

015170 GB45.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
017710	     MOVE SPACES	 TO R-DET.
017750	     MOVE "OUTSTANDING ORDERS"
                                 TO REP-H1.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017750	     MOVE W95-COMP	 TO REP-EXPNAME.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DET.
017770	     MOVE W95-ADD1	 TO REP-NADD.
026690	   IF W05-VATNO NOT = SPACES
026700	       MOVE "VAT No/BTW Nr :"
				 TO REP-H2
026710	       MOVE W05-VATNO	 TO REP-GNO.
006020	     MOVE WS-H3 	 TO O2-PAGE.
006030	     ADD 1		 TO WS-PAGE.
006050	     MOVE WS-PAGE	 TO O2-PGNO.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017790	     MOVE SPACES	 TO R-DET.
017800	     MOVE W95-ADD2	 TO REP-NADD.
017910	   IF W95-TEL NOT = SPACES
017920	       MOVE "TELEPHONE/FOON:"
				 TO REP-H2
017930	       MOVE W95-TEL	 TO REP-TEL.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
017820	     MOVE W95-ADD3	 TO REP-NADD.
	     PERFORM CALL-PRINTUTL.
017840	     MOVE W95-ADD4	 TO REP-NADD.
	     PERFORM CALL-PRINTUTL.
017870	     MOVE SPACES	 TO R-DET.
017880	     MOVE W95-PC	 TO REP-PC.
	     PERFORM CALL-PRINTUTL.
017900	     MOVE SPACES	 TO R-DET.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE "DATE  :"	 TO REP-H14.
026670	     MOVE ORD-DATE	 TO W15-YMD.
	     MOVE W15-DY	 TO W15-DD.
	     MOVE W15-MT	 TO W15-MM.
	     MOVE W15-CN	 TO W15-CC.
	     MOVE W15-YR	 TO W15-YY.
	     MOVE W15-DATE	 TO REP-DTE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
026320	     MOVE SPACES	 TO R-DET.
026330	     MOVE P-NAME	 TO REP-NAME.
	     MOVE "SUPPLIER NUMBER : "
				 TO REP-H4.
026550	     MOVE ORD-CRED	 TO REP-AC.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026350	     MOVE W40-ADD1	 TO REP-NAME.
	     MOVE "SUPPLIER TEL.NO : "
				 TO REP-H4.
	     MOVE W40-TEL	 TO REP-STEL.
	     PERFORM CALL-PRINTUTL.
026380	     MOVE W40-ADD2	 TO REP-NAME.
	     MOVE "SUPPLIER FAX.NO : "
				 TO REP-H4.
	     MOVE W40-FAX	 TO REP-STEL.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026410	     MOVE W40-ADD3	 TO REP-NAME.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026440	     MOVE W40-ADD4	 TO REP-NAME.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
026510	     MOVE P-PC		 TO REP-PC1.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	     MOVE WS-HDNG2	 TO O2-HDNG.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.

       GB50.
	   IF WS-PAGE = ZERO
	       MOVE "No outstanding orders"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GB00.
	     MOVE SPACES	 TO R-DET.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "**** Total for Supplier"
				 TO O-DESC.
	     MOVE W25-V1	 TO O-QNT.
	     MOVE W25-V2	 TO O-EXT.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE ZERO		 TO W25-V1 W25-V2.

       GB55.
015110	     MOVE SPACES	 TO R-DET
	     MOVE 99		 TO WS-ADVANCE
	     PERFORM CALL-PRINTUTL
015100	     GO TO GB00.

032560 GB999.
032570       EXIT.

      /
       PORDER-LOOKUP		SECTION 5.
012520 PORDER-LUP-SCREEN.
011050	     PERFORM SAVE-SCREEN.
	     DISPLAY S-POR.

       PORDER-LUP-CLR-SCREEN.
	     MOVE 0909		 TO CPOS.
	     PERFORM PORDER-CRT-EDGE UNTIL CLIN = 21.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 11		 TO SHADE-COL.
	     MOVE 66		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
000340	     DISPLAY CLEAR-L25.
000350	     MOVE 9		 TO CLIN.
000360	     MOVE 1		 TO W75-S W75-S1.

       PORDER-LUP-TAB-LOOP.
	     MOVE ZEROES	 TO W75-POR (W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
	       GO TO PORDER-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.
	     MOVE P-NUMBER	 TO ORD-CRED.
	     PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
	   IF WS-F-ERROR = 16
	       GO TO PORDER-NO-RECORD.

       PORDER-GET-ORD-RECORD.
	   IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO PORDER-SKIP-ORD-READ.
027520	     PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
	   IF WS-F-ERROR = 16
               SUBTRACT 1        FROM W75-S
	       GO TO PORDER-FULL-SCREEN.
	   IF NOT (ORD-CRED = P-NUMBER)
	       SUBTRACT 1	 FROM W75-S
	       GO TO PORDER-FULL-SCREEN.

       PORDER-SKIP-ORD-READ.
	     MOVE ORD-KEY	 TO W75-CODE (W75-S).
	     MOVE ORD-EXT-ITEM	 TO STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       SUBTRACT 1	 FROM W75-S
	       GO TO PORDER-GET-ORD-RECORD.
	     DISPLAY SD-POR AT LINE CLIN.
027700	   IF CLIN < 20
027710         ADD 1 TO CLIN W75-S
027720	       GO TO PORDER-GET-ORD-RECORD.

       PORDER-FULL-SCREEN.
001020	     DISPLAY " " AT 2503 WITH FOREGROUND-COLOR 14
001030		     " highlight - " WITH FOREGROUND-COLOR 7
		     "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 14
		     " - " WITH FOREGROUND-COLOR 7
001040		     "ENTER" WITH FOREGROUND-COLOR 14
001050		     " select - " WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 14
		     "ape" WITH FOREGROUND-COLOR 7.

       PORDER-SAVE-FULL-SCREEN.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 0		 TO W75-S1.
	     MOVE 0910		 TO CPOS.

       PORDER-CRT-ATTRIB.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.

       PORDER-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO PORDER-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO PORDER-CHK-NO-KEYS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO PORDER-CHK-SELECT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO PORDER-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO PORDER-RESTORE-SCREEN
		 WHEN PAGE-DOWN
001340		     GO TO PORDER-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO PORDER-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO PORDER-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO PORDER-GET-USER-OPT.

001240 PORDER-GET-NEXT-PAGE.
	   IF W75-S < 12
	       GO TO PORDER-GET-USER-OPT.

       PORDER-NEW-PAGE.
	     MOVE 0909		 TO CPOS.
	     PERFORM PORDER-CRT-EDGE UNTIL CLIN = 21.
	     MOVE 9		 TO CLIN.
	     MOVE LOW-VALUES	 TO W75-KEYS.
	     MOVE 1		 TO W75-S W75-S1.
	     PERFORM PORDER-LUP-TAB-LOOP.
027830	     GO TO PORDER-GET-ORD-RECORD.

       PORDER-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       PORDER-GET-PREV-LOOP.
	     PERFORM READ-PORDER-PREV THRU READ-PORDER-EXIT.
	   IF WS-F-ERROR = 16
	       MOVE SPACE	 TO WS-OPTION
	       GO TO PORDER-NEW-PAGE.
	   IF WS-IXS1 < 24
               ADD 1             TO WS-IXS1
	       GO TO PORDER-GET-PREV-LOOP.
	     GO TO PORDER-NEW-PAGE.


	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     GO TO PORDER-RESTORE-SCREEN.

027970 PORDER-NO-RECORD.
027730	     DISPLAY "No outstandig orders - Press " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " key"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     GO TO PORDER-RESTORE-SCREEN.

       PORDER-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1 FROM W75-S1 CLIN.
	     GO TO PORDER-HIGHLIGHT.

       PORDER-CHK-NO-KEYS.
	   IF W75-S1 = ZERO
	       MOVE 1		 TO W75-S1
	       GO TO PORDER-HIGHLIGHT.
           IF W75-S1 < W75-S
	       ADD 1		 TO W75-S1 CLIN.

       PORDER-HIGHLIGHT.
	     PERFORM PORDER-CRT-ATTRIB.
	     DISPLAY W43-DET2 (CLIN) AT CPOS
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 7.
	     GO TO PORDER-GET-USER-OPT.

       PORDER-CHK-SELECT.
           IF W75-S1 = ZERO
	       GO TO PORDER-GET-USER-OPT.
	     MOVE CSTART	 TO WS1-CURSOR.
	     MOVE W75-CODE (W75-S1)
				 TO ORD-KEY.
	     PERFORM READ-PORDER THRU READ-PORDER-EXIT.
	     MOVE ZERO		 TO COL-AND-LINE.
	     MOVE ORD-KEY	 TO W15-ORD.
	     GO TO PORDER-RESTORE-SCREEN.

       PORDER-CRT-EDGE.
012890	     DISPLAY "³           ³              ³
      -		     "        ³        ³" AT CPOS
		      WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
019260	     ADD 1		 TO CLIN.

       PORDER-RESTORE-SCREEN.
011320	     PERFORM RESTORE-SCREEN.

       PORDER-LOOKUP-EXIT.
             EXIT.

       GC000	     SECTION 5.
       GC00.
	     DISPLAY "Creditor" AT 0812.
	     DISPLAY "A" AT 1012
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ll or " "S"
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "elective".
	     MOVE "A"		 TO WS-SEQ.

       GC02.
	     ACCEPT WS-SEQ AT 1029
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-SEQ
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-SEQ = "A" OR "S")
	       GO TO GC02.
	     DISPLAY WS-BLNK78 AT 1002 WITH FOREGROUND-COLOR 3.
	     MOVE ZERO		 TO W25-V1 W25-V2 W25-TV1 W25-TV2.
	   IF WS-SEQ = "S"
	       GO TO GC16.
	     MOVE "     !"	 TO ORD-CRED.
015040	     PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
015050	   IF WS-F-ERROR NOT = ZERO
015060	       GO TO GC20.
             MOVE SPACES         TO W15-ACNO.
	     MOVE ZERO		 TO WS-IND.

015070 GC05.
015080       PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
015090     IF WS-F-ERROR NOT = ZERO             
      *    *************************************************************
      *    **** 	 PERFORM Print the Supplier Totals.	    ****
      *    *************************************************************
	       PERFORM GC06
015110	       MOVE SPACES	 TO R-DET
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 If printing all suppliers, print the grand totals.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       IF WS-SEQ = "A"
		   PERFORM GC10
		   MOVE SPACES	 TO R-DET
		   MOVE WS-HDNG2 TO O2-HDNG
		   MOVE 0	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
		   MOVE 2	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
		   MOVE SPACES	 TO R-DET
		   MOVE "**** Total purchase orders"
				 TO O2-DESC
		   MOVE W25-TV1	 TO O2-QNT
		   MOVE W25-TV2	 TO O2-EXT
		   MOVE 0	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
	       END-IF
015100	       GO TO GC999.
           IF ORD-CRED = W15-ACNO
	       GO TO GC15.

       GC06.
	   IF NOT (W15-ACNO = SPACES)
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	If printing all suppliers, accumulate grand totals.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       IF WS-SEQ = "A"
		   ADD W25-V1	 TO W25-TV1
		   ADD W25-V2	 TO W25-TV2
	       END-IF
      *    *************************************************************
      *    **** 	     Print the Supplier Totals		    ****
      *    *************************************************************
015110	       MOVE SPACES	 TO R-DET
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "**** Supplier Total"
				 TO O2-DESC
	       MOVE W25-V1	 TO O2-QNT
	       MOVE W25-V2	 TO O2-EXT
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE ZERO	 TO W25-V1 W25-V2
	   END-IF.

       GC07.
	   IF WS-SEQ = "S"
015110	       MOVE SPACES	 TO R-DET
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO GC16.
	   IF W15-ACNO NOT = SPACES
015110	       MOVE SPACES	 TO R-DET
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.

       GC08.
	     MOVE SPACES	 TO W15-CONTACT.
	     MOVE ZERO		 TO WS-PAGE.
             MOVE ORD-CRED       TO P-NUMBER W15-ACNO.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
	     DISPLAY P-NUMBER AT 0822
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5 "°"
		     P-NAME WITH FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5.
014060	     UNSTRING P-ADDR DELIMITED BY "*"
014070				 INTO W40-ADD1
014080				      W40-ADD2
014090				      W40-ADD3
014100				      W40-ADD4
014110       OVERFLOW
014120           MOVE 1          TO WS-ERROR.

015170 GC10.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
017710	     MOVE SPACES	 TO R-DET.
017750	     MOVE "OUTSTANDING ORDERS"
                                 TO REP-H1.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017750	     MOVE W95-COMP	 TO REP-EXPNAME.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DET.
017770	     MOVE W95-ADD1	 TO REP-NADD.
026690	   IF W05-VATNO NOT = SPACES
026700	       MOVE "VAT No/BTW Nr :"
				 TO REP-H2
026710	       MOVE W05-VATNO	 TO REP-GNO.
006020	     MOVE WS-H3 	 TO O2-PAGE.
006030	     ADD 1		 TO WS-PAGE.
006050	     MOVE WS-PAGE	 TO O2-PGNO.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017790	     MOVE SPACES	 TO R-DET.
017800	     MOVE W95-ADD2	 TO REP-NADD.
017910	   IF W95-TEL NOT = SPACES
017920	       MOVE "TELEPHONE/FOON:"
				 TO REP-H2
017930	       MOVE W95-TEL	 TO REP-TEL.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
017820	     MOVE W95-ADD3	 TO REP-NADD.
	     PERFORM CALL-PRINTUTL.
017840	     MOVE W95-ADD4	 TO REP-NADD.
	     PERFORM CALL-PRINTUTL.
017870	     MOVE SPACES	 TO R-DET.
017880	     MOVE W95-PC	 TO REP-PC.
	     PERFORM CALL-PRINTUTL.
017900	     MOVE SPACES	 TO R-DET.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE "DATE  :"	 TO REP-H14.
026670	     MOVE ORD-DATE	 TO W15-YMD.
	     MOVE W15-DY	 TO W15-DD.
	     MOVE W15-MT	 TO W15-MM.
	     MOVE W15-CN	 TO W15-CC.
	     MOVE W15-YR	 TO W15-YY.
	     MOVE W15-DATE	 TO REP-DTE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
026320	     MOVE SPACES	 TO R-DET.

       GC12.
026330	     MOVE P-NAME	 TO REP-NAME.
	     MOVE "SUPPLIER NUMBER : "
				 TO REP-H4.
026550	     MOVE ORD-CRED	 TO REP-AC.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026350	     MOVE W40-ADD1	 TO REP-NAME.
	     PERFORM CALL-PRINTUTL.
026380	     MOVE W40-ADD2	 TO REP-NAME.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026410	     MOVE W40-ADD3	 TO REP-NAME.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026440	     MOVE W40-ADD4	 TO REP-NAME.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
026510	     MOVE P-PC		 TO REP-PC1.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	     MOVE WS-HDNG2	 TO O2-HDNG.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	     MOVE ZERO		 TO WS-IND.

       GC15.
	   IF WS-IND = 1
	       PERFORM GC10 THRU GC12.
	     MOVE ORD-ORD	 TO O2-ORD.
	     MOVE "/"		 TO O2-SL.
	     MOVE ORD-LINE	 TO O2-LINE.
	     MOVE ORD-ITEM	 TO O2-EXT-ITEM STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     MOVE STK-DESC	 TO O2-DESC.
	     MOVE ORD-QUANT	 TO O2-QNT.
	     MOVE ORD-UCOST	 TO O2-COST.
	     COMPUTE O2-EXT = ORD-QUANT * ORD-UCOST.
	     ADD ORD-QUANT	 TO W25-V1.
	     COMPUTE W25-V2 = W25-V2 + (ORD-QUANT * ORD-UCOST).
	     MOVE ORD-DRATE	 TO USE-NO.
	   IF NOT (USE-NO = ZERO)
	       PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT
	       MOVE USE-NAME	 TO W15-NAME
	       PERFORM BB010
	       IF NOT (W10-NAME = W15-CONTACT)
		   MOVE W10-NAME TO O2-SMAN W15-CONTACT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE ZERO	 TO WS-IND
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 1		 TO WS-IND.
	     MOVE SPACES	 TO R-DET.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
	       MOVE SPACES	 TO WS-COMMAND
014990	       GO TO GC25.
	     GO TO GC05.

       GC16.
	     MOVE SPACES	 TO W15-ACNO.
	     DISPLAY "Esc" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to EXIT".

       GC17.
	     ACCEPT W15-ACNO AT 0821
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GC999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GC17
	       END-EVALUATE.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W15-ACNO	 TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
	   IF NOT (WS-F-ERROR = ZERO)
	       MOVE "No account for code selected"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GC17.
	     MOVE W15-ACNO	 TO ORD-CRED.
015040	     PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
015050	   IF WS-F-ERROR NOT = ZERO
015060	       GO TO GC18.
015080	     PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
	   IF NOT (W15-ACNO = ORD-CRED)
	       GO TO GC18.
	     MOVE ZERO		 TO WS-IND.
	     GO TO GC08.

       GC18.
	     MOVE "No outstanding orders for account"
				 TO WS-ERR-STRING
003260	     PERFORM ERROR-MESSAGE.
	     GO TO GC17.

015620 GC20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015640	     MOVE "******  END OF REPORT  ******" TO R-DET.

       GC25.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.

032560 GC999.
032570       EXIT.

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
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "PORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PORDER.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "SYSUSE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SYSUSER.
	     OPEN I-O SYSUSER.
       ZA05.
042360       MOVE 1              TO WS-PARKEY.
042370       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042380       MOVE PAR-COMPANY    TO W60-COMPANY.
042390       MOVE PAR-DMY        TO W05-DATE.
042400       MOVE PAR-YMD        TO W05-DATE-YMD.
044620	     MOVE PAR-COMPANY	 TO W95-COMP.
054820       MOVE PAR-TELEPHONE  TO W95-TEL.
054870	     ADD 1		 TO WS-PARKEY.
054880       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
054890       MOVE PAR-ADDRESS1   TO W95-ADD1.
054900       MOVE PAR-ADDRESS2   TO W95-ADD2.
054910       MOVE PAR-ADDRESS3   TO W95-ADD3.
054920       MOVE PAR-ADDRESS4   TO W95-ADD4.
054930       MOVE PAR-POST-CDE   TO W95-POST.
054940     IF W95-ADD4 = SPACES
054950         MOVE W95-PC       TO W95-ADD4
054960         MOVE SPACES       TO W95-PC.
054970     IF W95-ADD3 = SPACES
054980         MOVE W95-ADD4     TO W95-ADD3
054990         MOVE SPACES       TO W95-ADD4.
055000     IF W95-ADD2 = SPACES
055010         MOVE W95-ADD3     TO W95-ADD2
055020         MOVE SPACES       TO W95-ADD3.
055030     IF W95-ADD1 = SPACES
055040         MOVE W95-ADD2     TO W95-ADD1
055050         MOVE SPACES       TO W95-ADD2.
042360	     MOVE 3		 TO WS-PARKEY.
042370       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
053550	     MOVE PAR-VAT-REG	 TO W05-VATNO.
	     MOVE 2		 TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
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
021870	     MOVE SPACES	 TO WS-MID-LNE.
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
021960				    WS-MCHR (60) WS-MCHR (70).
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
041640     ELSE
041650     IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT   TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 16
041860         MOVE W02-PORDER   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY.
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
