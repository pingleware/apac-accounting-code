      $SET LINKCOUNT"320"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	   ******   ********   *
      *   **	**  **	  **  **    **	 **  **   **	**	 **    *
      *   **	    **	  **  **    **	**    **       **	**     *
      *   **	    ******    *******	**    **     **        **      *
      *   **	    **	 **   **	**    **   **	      **       *
      *   **	**  **	  **  **	 **  **   **	      **       *
      *    ******   **	  **  **	  ****	  ********    **       *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *  C R E D I T O R   A D D R E S S   L A B E L S	 P R O G R A M *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CRP027.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1982.

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
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000120 COPY PARAM.SL.

000140 COPY CREDIT.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000250 COPY PARAM.FDE.

000270 COPY CREDIT.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
002780 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002790 77  WS-SUB-1	   PIC 9(04)	 COMP-5.
002800 77  WS-SUB-2        PIC 9(04)     COMP-5.
002810 77  WS-SUB-3        PIC 9(04)     COMP-5.
002820 77  WS-SUB-4        PIC 9(04)     COMP-5.
002830 77  WS-SUB-5        PIC 9(04)     COMP-5.
002840 77  WS-SUB-6        PIC 9(04)     COMP-5.
002850 77  WS-IXS1         PIC 9(04)     COMP-5.
002860 77  WS-IXS2         PIC 9(04)     COMP-5.
002870 77  WS-IXS3         PIC 9(04)     COMP-5.
002880 77  WS-IXS4         PIC 9(04)     COMP-5.
002890 77  WS-MSUB         PIC 9(04)     COMP-5.
002900 77  WS-PARKEY       PIC 9(04)     COMP-5.
002910 77  WS-DEPKEY       PIC 9(04)     COMP-5.
002910 77  WS-DEBKEY       PIC 9(06)     COMP-5.
002930 77  WS-NETKEY       PIC 9(06)     COMP-5.
002940 77  WS-WRKEY        PIC 9(04)     COMP-5.
002950 77  WS-GST-RATE     PIC S9(03)V99 COMP-3.
002960 77  WS-NTP          PIC 9(01).
002970 77  WS-LINES        PIC 9(02).
002980 77  WS-PAGE         PIC 9(04)     COMP-5.
002990 77  WS-OPTION       PIC X(01).
002990 77  WS-SEQ          PIC X(01).
003000 77  WS-CREDIT       PIC X(01).
002990 77  WS-ZERO         PIC X(01).
       77  WS-NUM          PIC Z9.
003020 77  WS-ADDR         PIC X(01).
003030 77  WS-ERROR        PIC 9(01).
003040 77  WS-ERROR1       PIC 9(01).
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003180 77  WS-COMP	   PIC 9(01).
003260 77  WS-S1           PIC 9(04)     COMP-5.
003270 77  WS-S2           PIC 9(04)     COMP-5.
003280 77  WS-S3           PIC 9(04)     COMP-5.
003290 77  WS-S4           PIC 9(04)     COMP-5.
003300 77  WS-S5           PIC 9(04)     COMP-5.
003310 77  WS-S6           PIC 9(04)     COMP-5.
003320 77  WS-S7           PIC 9(04)     COMP-5.
003330 77  WS-S8           PIC 9(04)     COMP-5.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
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
	   03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
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

003440 COPY WS.WS.

003460 01  W00-TRAN-CODES.
003470     03  W00-TRAN-TYPES  OCCURS 100.
003480         05  W00-TRAN-CODE PIC 9(02)   COMP-5.
003490         05  W00-ACTION  PIC 9(02)     COMP-5.
003500         05  W00-E-DESC  PIC X(12).
003510         05  W00-A-DESC  PIC X(12).

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY CREDIT.ID.

       COPY PARAM.ID.

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
	       05  CF-SNO	   PIC X(05).
	       05  FILLER	   PIC X(01).
	       05  CF-SUPP	   PIC X(30).
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
	       05  ST-ACNO	   PIC X(05).
	       05  FILLER	   PIC X(09).
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
	       05  A-ACNO	   PIC X(05).
	       05  FILLER	   PIC X(01).
	       05  A-NAME	   PIC X(40).
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

001030	   03  LAB-L1 REDEFINES W02-PRN-LINE.
001040	       05  LAB-DETAIL OCCURS 2.
001050		   07  FILLER	 PIC  X(02).
001060		   07  LAB-DET	 PIC  X(36).
001070		   07  FILLER	 PIC  X(02).

003650 01  W05-LIST.
003660     03  W05-OPT         PIC 9(03).
003670     03  W05-TYPES REDEFINES W05-OPT.
003680         05  W05-COMP    PIC 9(01).
003690         05  W05-INDV    PIC 9(01).
003700         05  W05-INT     PIC 9(01).

003710 01  W10-SPARES.
003720	   03  W10-DATE        PIC X(10).
           03  W10-RDATE REDEFINES W10-DATE.
	       05  W10-DTE     PIC 99/99/9999.

       COPY W12.WS.

       01  W15-DATES.
004120	   03  W15-START       PIC  9(08).
004130     03  W15-SDTE-DMY REDEFINES W15-START.
004140	       05  W15-SDAY    PIC  9(02).
004150	       05  W15-SMNTH   PIC  9(02).
	       05  W15-SCENT   PIC  9(02).
004160	       05  W15-SYEAR   PIC  9(02).
004170	   03  W15-S-YMD       PIC  9(08).
004180     03  W15-STRT-YMD REDEFINES W15-S-YMD.
	       05  W15-SCC     PIC  9(02).
004190	       05  W15-SYY     PIC  9(02).
004200	       05  W15-SMM     PIC  9(02).
004210	       05  W15-SDD     PIC  9(02).
004120	   03  W15-END	       PIC  9(08).
004130     03  W15-EDTE-DMY REDEFINES W15-END.
004140	       05  W15-EDAY    PIC  9(02).
004150	       05  W15-EMNTH   PIC  9(02).
	       05  W15-ECENT   PIC  9(02).
004160	       05  W15-EYEAR   PIC  9(02).
004170	   03  W15-E-YMD       PIC  9(08).
004180     03  W15-END-YMD REDEFINES W15-E-YMD.
	       05  W15-ECC     PIC  9(02).
004190	       05  W15-EYY     PIC  9(02).
004200	       05  W15-EMM     PIC  9(02).
004210	       05  W15-EDD     PIC  9(02).

003840 01  W20-DATE.
	   03  W20-CC.
	       05  W20-CENT    PIC  9(02).
003850     03  W20-YY.
003860	       05  W20-YEAR    PIC  9(02).
003870     03  W20-MM.
003880	       05  W20-MONTH   PIC  9(02).
003890     03  W20-DD.
003900	       05  W20-DAY     PIC  9(02).
003910 01  W20-DTE1 REDEFINES W20-DATE.
003920	   03  W20-DTE	       PIC  9(08).

003930 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
003980     03  W25-TOTAL       PIC S9(07)V99 COMP-3.
003990     03  W25-VALUE       PIC S9(07)V99 COMP-3.

004760 01  W40-COMPANY.
004770     02  W40-NAME1.
004780         03  W40-NAME.
004790             05  W40-ENTRY PIC X(07).
004800             05  FILLER  PIC X(33).
004810         03  FILLER      PIC X(32).
004820     02  W40-NAME2 REDEFINES W40-NAME1.
004830         03  W40-SURNAME PIC X(24).
004840         03  W40-FNAME   PIC X(24).
004850         03  W40-SNAME   PIC X(24).
004860     02  W40-NAME3 REDEFINES W40-NAME1.
004870         03  W40-NCHAR OCCURS 72 PIC X(01).
004880     02  W40-ADDRESS1.
004890         03  W40-ADD1    PIC X(30).
004900         03  W40-ADD2    PIC X(30).
004910         03  W40-ADD3    PIC X(30).
004920         03  W40-ADD4    PIC X(30).
004930     02  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
004940         03  W40-ACHAR OCCURS 120 PIC X(01).
004950     02  W40-POST        PIC 9(04).
004960     02  W40-TEL         PIC X(14).
004970     02  W40-HTEL        PIC X(14).
004980     02  W40-TAX REDEFINES W40-HTEL.
004990         03  W40-TAXNO   PIC 9(14)     COMP-3.
005000     02  W40-TITLE       PIC 9(04)     COMP-5.
005010     02  W40-LANG        PIC X(01).

       COPY W40.WS.

      *
      *    STORAGE FOR PRINTING OF LABELS
      *
005070 01  W46-LABELS.
005080     03  W46-LABEL OCCURS 2.
005090         05  W46-NAME    PIC X(40).
005100         05  W46-ADD1    PIC X(30).
005110         05  W46-ADD2    PIC X(30).
005120         05  W46-ADD3    PIC X(30).
005130         05  W46-ADD4    PIC X(30).
005140         05  W46-POST.
005150             07  W46-PC  PIC 9(04).

       COPY FUNCTION.WS.

005170 COPY W50.WS.

005190 01  W60-NME-ADD.
005200     03  W60-NAME.
005210         05  W60-NCHAR OCCURS 40 PIC X(01).
005220     03  W60-ADDRESS.
005230         05  W60-ACHAR OCCURS 60 PIC X(01).

005490 01  W80-ACCOUNT.
005500	   03  W80-ACNO        PIC X(05).
005510     03  W80-NUMD REDEFINES W80-ACNO.
005530	       05  W80-SERIAL  PIC 9(05).

005810 01  W95-STM.
005820     03  W95-COMP        PIC X(40).
005830     03  W95-ADD1        PIC X(30).
005840     03  W95-ADD2        PIC X(30).
005850     03  W95-ADD3        PIC X(30).
005860     03  W95-ADD4        PIC X(30).
005870     03  W95-EDIT        REDEFINES W95-ADD4.
005880         05  W95-V1.
005890             07  W95-T   PIC Z(04)9.99.
005900         05  W95-V2.
005910             07  W95-B   PIC Z(06)9.99-.
005920         05  FILLER      PIC X(11).
005930     03  W95-POST        PIC 9(04).
005940     03  W95-TEL         PIC X(14).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
005990 SCREEN SECTION.

       COPY BLANK.CRT.

006120 01  S03.
006130	   03  LINE  4 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
006140				 VALUE "Printing Supplier labels".
006150	   03  LINE  6 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")".

006190 01  S05.
006200	   03  LINE 10 COLUMN 12 VALUE "Keyin Category code".

       COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
003240	   IF LS0-CRLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM CH000.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA49.
006950	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP4.

      /
      *    ****    READ FILES ROUTINES
      *
007340 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.
      
       COPY CREDIT.RD.
      
      /
007700 AE155-GET-CREDITOR      SECTION.
007710 AE155-ACCOUNT.
011060	     MOVE W80-ACNO	 TO P-NUMBER.
011130       PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
011140     IF WS-F-ERROR = 3 
011150         MOVE 1 TO WS-ERROR
011160     ELSE
011170	   IF P-NUMBER NOT = W80-ACNO
011180         MOVE 2 TO WS-ERROR
011190     ELSE
011200         MOVE 0 TO WS-ERROR.
007870 AE160-EXIT.
007880       EXIT.

009210 CA175-ADDRESS           SECTION 2.
009220 CA175-EXTRACT.
009230       MOVE 0 TO WS-ERROR.
012620	     UNSTRING P-ADDR DELIMITED BY "*"
009250	       INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
009260         OVERFLOW
009270           MOVE 1 TO WS-ERROR.
009280 CA180-EXIT.
009290       EXIT.
      /
      *    ****    S U P P L I E R   L A B E L S
      *
009650 CH000	       SECTION 5.
009660 CH00.
009670       DISPLAY CLR-SCREEN.
009680       DISPLAY S03.
009690	     ACCEPT WS-OPTION AT 0632
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
009760	   IF WS-OPTION = "N"
009770         GO TO CH999.
009780	     MOVE SPACES	 TO R-DET.
009790	     MOVE ALL "X"	 TO W46-LABELS.
009800	     MOVE "9999"	 TO W46-PC (1) W46-PC (2).
009810 CH10.
009820     IF W46-ADD2 (1) = SPACES
009830         MOVE W46-POST (1) TO W46-ADD2 (1)
009840	       MOVE SPACES	 TO W46-POST (1)
009850         GO TO CH12.
009860     IF W46-ADD3 (1) = SPACES
009870         MOVE W46-POST (1) TO W46-ADD3 (1)
009880	       MOVE SPACES	 TO W46-POST (1)
009890         GO TO CH12.
009900     IF W46-ADD4 (1) = SPACES
009910         MOVE W46-POST (1) TO W46-ADD4 (1)
009920	       MOVE SPACES	 TO W46-POST (1).
009930 CH12.
009940     IF W46-ADD2 (2) = SPACES
009950         MOVE W46-POST (2) TO W46-ADD2 (2)
009960	       MOVE SPACES	 TO W46-POST (2)
009970         GO TO CH14.
009980     IF W46-ADD3 (2) = SPACES
009990         MOVE W46-POST (2) TO W46-ADD3 (2)
010000	       MOVE SPACES	 TO W46-POST (2)
010010         GO TO CH14.
010020     IF W46-ADD4 (2) = SPACES
010030         MOVE W46-POST (2) TO W46-ADD4 (2)
010040	       MOVE SPACES	 TO W46-POST (2).
010050 CH14.
010060	     MOVE SPACES	 TO LAB-L1.
010070	     MOVE W46-NAME (1)	 TO LAB-DET (1).
010080	     MOVE W46-NAME (2)	 TO LAB-DET (2).
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
010100	     MOVE W46-ADD1 (1)	 TO LAB-DET (1).
010110	     MOVE W46-ADD1 (2)	 TO LAB-DET (2).
	     PERFORM CALL-PRINTUTL.
010130	     MOVE W46-ADD2 (1)	 TO LAB-DET (1).
010140	     MOVE W46-ADD2 (2)	 TO LAB-DET (2).
	     PERFORM CALL-PRINTUTL.
010160	     MOVE W46-ADD3 (1)	 TO LAB-DET (1).
010170	     MOVE W46-ADD3 (2)	 TO LAB-DET (2).
	     PERFORM CALL-PRINTUTL.
010190	     MOVE W46-ADD4 (1)	 TO LAB-DET (1).
010200	     MOVE W46-ADD4 (2)	 TO LAB-DET (2).
	     PERFORM CALL-PRINTUTL.
010220	     MOVE W46-POST (1)	 TO LAB-DET (1).
010230	     MOVE W46-POST (2)	 TO LAB-DET (2).
	     PERFORM CALL-PRINTUTL.
010250	     MOVE SPACES	 TO LAB-L1.
	     MOVE 3		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
010290	     MOVE 1		 TO WS-S6.
010300 CH15.
012180	     PERFORM CHECK-CORRECT.
010390	   IF WS-OPTION = "N"
010400         GO TO CH14.
       CH17.
             DISPLAY "A" AT 1212 WITH FOREGROUND-COLOR 11
                     "ccount or " WITH FOREGROUND-COLOR 3
                     "N" WITH FOREGROUND-COLOR 11
                     "ame sequence" WITH FOREGROUND-COLOR 3.
             MOVE "N"            TO WS-SEQ.
       CH17A.
	     ACCEPT WS-SEQ AT 1237
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-SEQ
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-SEQ = "A" OR "N")
               GO TO CH17A.
	     DISPLAY "Is this a restart " AT 1412
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-OPTION.
       CH18.
	     ACCEPT WS-OPTION AT 1434
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO CH18.
           IF WS-OPTION = "Y"
               GO TO CH40.
	     MOVE " !"		 TO P-NAME P-NUMBER.
       CH19.
	     DISPLAY "Creditor account:" AT 1612.
           IF WS-SEQ = "A"
	       PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT
           ELSE
	       PERFORM START-AT-CRED-NAME THRU READ-CREDIT-EXIT.
010780	   IF WS-F-ERROR = 3
010790         GO TO CH999.
010800 CH20.
010810	     PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
010780	   IF WS-F-ERROR = 3
010790         GO TO CH35.
       CH21.      
	     DISPLAY P-NUMBER AT 1630
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
010860	     MOVE P-NAME	 TO W40-NAME.
011250 CH34.
011270       PERFORM CA175-ADDRESS.
011280	     MOVE W40-NAME	 TO W46-NAME (WS-S6).
011290	     MOVE W40-ADD1	 TO W46-ADD1 (WS-S6).
011300	     MOVE W40-ADD2	 TO W46-ADD2 (WS-S6).
011310	     MOVE W40-ADD3	 TO W46-ADD3 (WS-S6).
011320	     MOVE W40-ADD4	 TO W46-ADD4 (WS-S6).
012640	     MOVE P-PC		 TO W46-PC (WS-S6).
011340	     ADD 1		 TO WS-S6.
011350     IF WS-S6 > 2
011360         PERFORM CH10 THRU CH14
011370	       MOVE SPACES	 TO W46-LABELS.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
014970         MOVE "**** PRINT ABORTED BY OPERATOR ****" 
				 TO R-DET
014990	       GO TO CH50.
             GO TO CH20.
       CH35.
011420     IF WS-S6 > 1
011430         PERFORM CH10 THRU CH14.
             GO TO CH999.
       CH40.
             MOVE SPACES         TO W80-ACNO.
	     MOVE 1801		 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
017040	     DISPLAY "Account No:" AT 1812.
017050	     ACCEPT W80-ACNO AT 1824
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF W80-ACNO = SPACES
               GO TO CH18.
	     MOVE 5		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W80-ACNO	 TO P-NUMBER.
           IF WS-SEQ = "A"
               GO TO CH19.
	     PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
	     PERFORM START-AT-CRED-NAME THRU READ-CREDIT-EXIT.
       CH45.
	     PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
	   IF W80-ACNO NOT = P-NUMBER
               GO TO CH45.               
	     DISPLAY "Creditor account:" AT 1612.
             GO TO CH21.

       CH50.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.

011470 CH999.
011480       EXIT.

      /
025020 ZA00-INIT	      SECTION 9.
025030 ZA00-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
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
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.

025620 ZA25.
025630	     MOVE 1		 TO WS-PARKEY.
025640       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025650	     MOVE PAR-COMPANY	 TO W95-COMP.
025660	     MOVE PAR-DMY	 TO W12-TODAY.
025670	     MOVE PAR-YMD	 TO W12-T-YMD.
025730	     MOVE 1		 TO WS-NETKEY.
025740       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
025830	     GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
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
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
025840 ZA200.
025850       DISPLAY "Files locked - Try later" AT 2312
025860                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
025870       DISPLAY "Press any key" AT 2512 
025880		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
025890	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
025920 ZA999-EXIT.
025930       EXIT.
      /
025940 ZB000		     SECTION 9.
      
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
041720     ELSE
041770	   IF WS-F-ERROR = 3
041780	       MOVE W02-CREDIT	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE P-NUMBER	 TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
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
042170       STOP RUN.
