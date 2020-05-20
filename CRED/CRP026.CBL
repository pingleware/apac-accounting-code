      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	   ******    ******    *
      *   **	**  **	  **  **    **	 **  **   **	**  **	  **   *
      *   **	    **	  **  **    **	**    **       **   **	       *
      *   **	    ******    *******	**    **     **     *******    *
      *   **	    **	 **   **	**    **   **	    **	  **   *
      *   **	**  **	  **  **	 **  **   **	    **	  **   *
      *    ******   **	  **  **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R ' S   P O S T   D A T E D   C H E Q U E S  *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	     CRP026.
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

       COPY PDCHEQ.SL.

000290 DATA DIVISION.
000300 FILE SECTION.

       COPY APACFIDS.FDE.

000140 COPY CONTROL.FDE.

000340 COPY CREDIT.FDE.

000320 COPY PARAM.FDE.

       COPY PDCHEQ.FDE.

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

000340 COPY CREDIT.ID.

       COPY PARAM.ID.

       COPY PDCHEQ.ID.

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
		   07  OR-ITEM	   PIC X(14).
		   07  FILLER	   PIC X(16).
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

001030	   03  LAB-L1 REDEFINES W02-PRN-LINE.
001040	       05  LAB-DETAIL OCCURS 2.
001050		   07  FILLER	 PIC  X(02).
001060		   07  LAB-DET	 PIC  X(36).
001070		   07  FILLER	 PIC  X(02).

       COPY W05.WS.

003820 01  W10-MONTHS.
003830     03  W10-MONTH-NAMES PIC X(36)     VALUE
003840                         "JanFebMarAprMayJunJulAugSepOctNovDec".
003850     03  W10-MONTH       REDEFINES W10-MONTH-NAMES
003860                         PIC X(03)     OCCURS 12.
       01  W10-STOCK.
002850     03  W10-VSELL      PIC S9(07)V99  COMP-3.
002860     03  W10-VCASH      PIC S9(07)V99  COMP-3.
002860     03  W10-VWSALE     PIC S9(07)V99  COMP-3.

       COPY W15.CRP.

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
004660	       05  W25-DATE    PIC 99/99/9999.
004670     03  W25-6.
004680         05  W25-NO      PIC Z(05)9.
004690     03  W25-5V2.
004700         05  W25-VAL     PIC Z(04)9.99.
004710     03  W25-S5V2.
004720         05  W25-SVAL    PIC Z(04)9.99DB.
004730     03  W25-S7V2.
004740         05  W25-VAL2    PIC Z(06)9.99DB.

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
	     PERFORM EA000.
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
             EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP4.

      /
006460 AB00-READ SECTION.

       COPY CONTROL.RD.

       COPY CREDIT.RD.

       COPY PARAM.RD.

       COPY PDCHEQ.RD.

      /
007700 AE155-GET-CREDITOR      SECTION.
007710 AE155-ACCOUNT.
011060       MOVE W15-ACNO       TO P-NUMBER.
011130       PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
011140     IF WS-F-ERROR = 3 
011150         MOVE 1 TO WS-ERROR
011160     ELSE
011170     IF P-NUMBER NOT = W15-ACNO
011180         MOVE 2 TO WS-ERROR
011190     ELSE
011200         MOVE 0 TO WS-ERROR.
007870 AE160-EXIT.
007880       EXIT.
      /
012300 EA000	     SECTION 5.
012310 EA00.
012320       DISPLAY CLR-SCREEN.
	     MOVE ZERO		 TO WS-PAGE W25-V1 W25-V2.
012330	     DISPLAY "POST DATED CHEQUE REPORTS" AT 0228
012340		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "S" AT 0812 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "upplier or "
		     "D" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ate sequence > <".
	     MOVE "D"		 TO WS-SEQ.
       EA05.
	     ACCEPT WS-SEQ AT 0839
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-SEQ
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-SEQ = "D" OR "S")
	       GO TO EA05.
       EA10.
005990	     MOVE SPACES	 TO R-DET.
006000	     MOVE WS-H1 	 TO PD-H1.
006010	     MOVE W60-COMPANY	 TO PD-H2.
006020	     MOVE WS-H3 	 TO PD-H3.
006030	     ADD 1		 TO WS-PAGE.
006040	     MOVE W05-DATE	 TO PD-DTE1.
006050	     MOVE WS-PAGE	 TO PD-PGE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
006080	     MOVE SPACES	 TO R-DET.
	   IF WS-SEQ = "D"
	       MOVE "POST DATED CHEQUES - DATE SEQUENCE"
				 TO PD-H2
	   ELSE
	       MOVE "POST DATED CHEQUES - SUPPLIER SEQUENCE"
				 TO PD-H2.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	   IF WS-SEQ = "D"
	       MOVE "  DATE"	 TO PD-H4
	       MOVE "AC.NO"	 TO PD-H7
	       MOVE "SUPPLIER NAME"
				 TO PD-H8
	   ELSE
	       MOVE "AC.NO"	 TO PD-H4
	       MOVE "SUPPLIER NAME"
				 TO PD-H5
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DET
	       MOVE "DATE"	 TO PD-H6
	       MOVE "NARRATIVE"  TO PD-H8.
	     MOVE "CHEQ NO"	 TO PD-H9.
	     MOVE "VALUE"	 TO PD-H10.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.

       EA15.
	     MOVE ZERO		 TO W25-V1 W25-V2.
	     INITIALIZE PDT-REC.
	   IF WS-SEQ = "D"
	       GO TO EA45.
	     MOVE "    !"	 TO PDT-AC W15-ACNO.
	     PERFORM START-AT-PDCHEQ-AC THRU READ-PDCHEQ-EXIT.
	   IF WS-F-ERROR = 44
	       GO TO EA80.

       EA20.
	     PERFORM READ-PDCHEQ-NEXT THRU READ-PDCHEQ-EXIT.
	   IF WS-F-ERROR = 44
	       PERFORM EA26
	       GO TO EA75.
	     DISPLAY PDT-KEY AT 1028
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF PDT-AC NOT = W15-ACNO
	       PERFORM EA25 THRU EA40.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
	       GO TO EA85.
	     MOVE PDT-DATE	 TO PD-DATE2.
	     MOVE PDT-NAR	 TO PD-SUPP-NAR.
	     MOVE PDT-REFER	 TO PD-REF.
	     MOVE PDT-VAL	 TO PD-VAL.
	     ADD PDT-VAL	 TO W25-V1 W25-V2.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM EA10
	       PERFORM EA35 THRU EA40.
	     MOVE SPACES	 TO R-DET.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
	       GO TO EA85.
	     GO TO EA20.

       EA25.
	   IF W15-ACNO = "    !"
	       GO TO EA30.

       EA26.
	     MOVE "TOTAL FOR SUPPLIER"
				 TO PD-SUPP-NAR.
	     MOVE W25-V1	 TO PD-VAL.
	     MOVE ZERO		 TO W25-V1.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 4)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM EA10.
	     MOVE SPACES	 TO R-DET.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
	       GO TO EA85.

       EA30.
	     MOVE PDT-AC	 TO W15-ACNO.
	     PERFORM AE155-GET-CREDITOR.
	   IF WS-ERROR NOT = ZERO
	       MOVE "**** NOT ON FILE ****"
				 TO P-NAME.

       EA35.
	     MOVE W15-ACNO	 TO PD-AC.
	     MOVE P-NAME	 TO PD-SUPPLIER.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.

       EA40.
	     EXIT.

       EA45.
	     MOVE ZERO		 TO PDT-DATE W15-DATE.
	     PERFORM START-AT-PDCHEQ-KEY THRU READ-PDCHEQ-EXIT.
	   IF WS-F-ERROR = 44
	       GO TO EA80.

       EA50.
	     PERFORM READ-PDCHEQ-NEXT THRU READ-PDCHEQ-EXIT.
	   IF WS-F-ERROR = 44
	       PERFORM EA56
	       GO TO EA75.
	     DISPLAY PDT-KEY AT 1028
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF PDT-DATE NOT = W15-DATE
	       PERFORM EA55 THRU EA70.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
	       GO TO EA85.
	     MOVE PDT-AC	 TO PD-AC2.
	     MOVE PDT-NAR	 TO PD-SUPP-NAR.
	     MOVE PDT-REFER	 TO PD-REF.
	     MOVE PDT-VAL	 TO PD-VAL.
	     ADD PDT-VAL	 TO W25-V1 W25-V2.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM EA10
	       PERFORM EA65 THRU EA70.
	     MOVE SPACES	 TO R-DET.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
	       GO TO EA85.
	     GO TO EA50.

       EA55.
	   IF W15-DATE = ZERO
	       GO TO EA60.

       EA56.
	     MOVE "TOTAL FOR DAY"
				 TO PD-SUPP-NAR.
	     MOVE W25-V1	 TO PD-VAL.
	     MOVE ZERO		 TO W25-V1.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 4)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM EA10.
	     MOVE SPACES	 TO R-DET.

       EA60.
	     MOVE PDT-DATE	 TO W15-DATE.

       EA65.
	     MOVE W15-DATE	 TO PD-DATE1.

       EA70.
	     EXIT.
       EA75.
	     MOVE "**** TOTAL POST DATED CHEQUES ****"
				 TO PD-SUPP-NAR.
	     MOVE W25-V2	 TO PD-VAL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE SPACES	 TO R-DET.
	     GO TO EA999.

       EA80.
	     MOVE "***  N O   P O S T   D A T E D   C H E Q U E S  ***"
				 TO R-DET.

       EA85.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE SPACES	 TO R-DET.

       EA999.
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
	     MOVE "PDCHEQ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PDCHEQ.

       ZA05.
042360       MOVE 1              TO WS-PARKEY.
042370       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042380       MOVE PAR-COMPANY    TO W60-COMPANY.
042390       MOVE PAR-DMY        TO W05-DATE.
042400       MOVE PAR-YMD        TO W05-DATE-YMD.
             MOVE 2              TO WS-NETKEY.
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
041640     ELSE
041650     IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT   TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 44
041860	       MOVE W02-PDCHEQ	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE PDT-KEY	 TO WS-KEYX.
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
