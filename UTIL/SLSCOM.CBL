      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *    ******   **	       ******	 ******    ******   **	   **  *
      *   **	**  **	      **    **	**    **  **	**  ***   ***  *
      *   **	    **	      **	**	  **	**  ** * * **  *
      *    ******   **	       ******	**	  **	**  **	*  **  *
      * 	**  **		    **	**	  **	**  **	   **  *
      *   **	**  **	      **    **	**    **  **	**  **	   **  *
      *    ******   ********   ******	 ******    ******   **	   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *       S A L E S   C O M M I S S I O N	P R O G R A M	       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
      /
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     SLSCOM.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1996.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1996 - 2008
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

000060 ENVIRONMENT DIVISION.
000070 CONFIGURATION SECTION.
000080 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000110	   SELECT NETWORK ASSIGN DISK
                          LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000130                    ACCESS RANDOM
000140                    ORGANIZATION RELATIVE
000150                    RELATIVE WS-NETKEY.
000120     SELECT PARAM   ASSIGN DISK
000130                    LOCK EXCLUSIVE
000140                    STATUS WS-STATUS
000150                    ACCESS RANDOM
000160                    ORGANIZATION RELATIVE
000170                    RELATIVE WS-PARKEY.

000490 DATA DIVISION.
000500 FILE SECTION.

       COPY APACFIDS.FDE.

000520 COPY CONTROL.IFD.

000520 COPY PARAM.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
003220 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
003230 77  WS-CLEAR	   PIC 9(01)	 VALUE ZERO.
       77  WS-AUD          PIC  9(04)    COMP-5 VALUE ZERO.
003240 77  WS-S1           PIC 9(04)     COMP-5.
003250 77  WS-S2           PIC 9(04)     COMP-5.
003260 77  WS-S3           PIC 9(04)     COMP-5.
003270 77  WS-S4           PIC 9(04)     COMP-5.
003280 77  WS-S5           PIC 9(04)     COMP-5.
003290 77  WS-S6           PIC 9(04)     COMP-5.
003300 77  WS-NO           PIC 9(04)     COMP-5.
001070 77  WS-SNO	   PIC 9(02).
003320 77  WS-PARKEY       PIC 9(04)     COMP-5.
003330 77  WS-DEBKEY       PIC 9(04)     COMP-5.
003330 77  WS-DEPKEY       PIC 9(04)     COMP-5.
003330 77  WS-NETKEY       PIC 9(04)     COMP-5.
003360 77  WS-AUDKEY       PIC 9(04)     COMP-5.
003370 77  WS-INDEX        PIC 9(04)     COMP-5.
003380 77  WS-INDTYPE      PIC 9(01).
003390 77  WS-LINES        PIC 9(02) VALUE 66.
003400 77  WS-PAGE         PIC 9(04)     COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003430 77  WS-GST-RATE	   PIC	9(02)V99   COMP-3.
003440 77  WS-OPTION       PIC  X(01).
       77  WS-STATOP       PIC  X(01).
003460 77  WS-REPLY        PIC  X(01).
003470 77  WS-ERROR        PIC  9(01).
003480 77  WS-SELECT       PIC  X(13) VALUE "Select Option".
003500 77  WS-CHAR1        PIC  X(01).
003510 77  WS-CHAR2        PIC  X(01).
003520 77  WS-COMP         PIC  9(01).
003530 77  WS-TYPE         PIC  X(01).
003550 77  WS-LEDG         PIC  9(03).
003570 77  WS-NUM          PIC  Z9.
003570 77  WS-REF          PIC  X(08).
003590 77  WS-BATCH        PIC  9(05)      COMP-3.
003620 77  WS-REP1	   PIC 9(01) VALUE 0.
003630 77  WS-REP2         PIC 9(01) VALUE 0.
003640 77  WS-REP3         PIC 9(01) VALUE 0.
003650 77  WS-REP4         PIC 9(01) VALUE 0.
003660 77  WS-REP5         PIC 9(01) VALUE 0.
003670 77  WS-REP6         PIC 9(01) VALUE 0.
003680 77  WS-REP7         PIC 9(01) VALUE 0.
003690 77  WS-REP8         PIC 9(01) VALUE 0.
003700 77  WS-REP9         PIC 9(01) VALUE 0.
003710 77  WS-REP10        PIC 9(01) VALUE 0.
003710 77  WS-REP11	   PIC 9(01) VALUE 0.
003720 77  WS-H1           PIC X(18) VALUE "*** END REPORT ***".
003730 77  WS-H2           PIC X(06) VALUE "M.T.D.".
003740 77  WS-H3           PIC X(06) VALUE "Y.T.D.".
003750 77  WS-H4           PIC X(05) VALUE "TOTAL".
003760 77  WS-H5           PIC X(06) VALUE "CREDIT".
003770 77  WS-H6           PIC X(05) VALUE "DEBIT".
003780 77  WS-H7           PIC X(09) VALUE "ITEM CODE".
003790 77  WS-H8           PIC X(11) VALUE "DESCRIPTION".
003800 77  WS-H9           PIC X(05) VALUE "QUANT".
003810 77  WS-H10          PIC X(04) VALUE "CASH".
003820 77  WS-H11          PIC X(05) VALUE "VALUE".
003830 77  WS-H12          PIC X(03) VALUE "NO.".
003840 77  WS-H13          PIC X(03) VALUE "MTD".
003850 77  WS-H14          PIC X(03) VALUE "YTD".
003860 77  WS-H15          PIC X(08) VALUE "G/LEDGER".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC  X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC  X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC  X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC  X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC  X(01) OCCURS 80.

003880 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003900 01  W02-FID.

       COPY APACFIDS.ID.

       COPY AUDIT.ID.

       COPY CONTROL.ID.

000540 COPY CREDIT.ID.

       COPY DEPART.ID.

       COPY DEBTOR.ID.

       COPY PARAM.ID.

       COPY SORDER.ID.

       COPY STOCK.ID.

       COPY TXPTRN.ID.

       COPY TXTRAN.ID.

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
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  W02-PRN-TYPE    PIC  X(01).
	   03  W02-PRN-LINE    PIC  X(136).
000660	   03  R-L1 REDEFINES W02-PRN-LINE.
000670	       05  R-DETAIL	   PIC	X(136).
000680	   03  R-L2 REDEFINES W02-PRN-LINE.
000690	       05  R-PGE	   PIC	X(06).
000700	       05  R-PNO	   PIC	Z(03)9.
000710	       05  FILLER	   PIC	X(09).
000720	       05  R-CPY	   PIC	X(40).
000730	       05  FILLER	   PIC	X(05).
000740	       05  R-DATE	   PIC	X(06).
000750	       05  R-DTE	   PIC	Z9/99/9999.
000760	   03  R-L2A REDEFINES W02-PRN-LINE.
000770	       05  RC-PGE	   PIC	X(06).
000780	       05  RC-PNO	   PIC	Z(03)9.
000790	       05  FILLER	   PIC	X(33).
000800	       05  RC-CPY	   PIC	X(40).
000810	       05  FILLER	   PIC	X(33).
000820	       05  RC-DATE	   PIC	X(06).
000830	       05  RC-DTE	   PIC	Z9/99/9999.
000840	   03  R-L3 REDEFINES W02-PRN-LINE.
000850	       05  FILLER	   PIC	X(30).
000860	       05  R-HEAD	   PIC	X(20).
000870	       05  FILLER	   PIC	X(30).
000880	   03  R-L3A REDEFINES W02-PRN-LINE.
000890	       05  FILLER	   PIC	X(56).
000900	       05  RC-HEAD	   PIC	X(20).
000910	       05  FILLER	   PIC	X(56).
000920	   03  R-L4 REDEFINES W02-PRN-LINE.
000930	       05  FILLER	   PIC	X(02).
000940	       05  R-H1 	   PIC	X(17).
000950	       05  R-H2 	   PIC	X(27).
000960	       05  R-H3 	   PIC	X(09).
000970	       05  R-H4 	   PIC	X(17).
000980	       05  R-H5 	   PIC	X(08).
000990	   03  R-L5 REDEFINES W02-PRN-LINE.
001000	       05  FILLER	   PIC	X(02).
001010	       05  R-I1 	   PIC	X(15).
001020	       05  FILLER	   PIC	X(02).
001030	       05  R-DESC1	   PIC	X(24).
001040	       05  FILLER	   PIC	X(03).
001050	       05  R-Q1 	   PIC	Z(04)9.
001060	       05  FILLER	   PIC	X(05).
001070	       05  R-UP1	   PIC	Z(04)9.99.
001080	       05  FILLER	   PIC	X(04).
001090	       05  R-V1 	   PIC	Z(06)9.99.
001100	       05  FILLER	   PIC	X(02).
001110	   03  R-L6 REDEFINES W02-PRN-LINE.
001120	       05  R-H6 	   PIC	X(14).
001130	       05  R-H7 	   PIC	X(25).
001140	       05  R-H8 	   PIC	X(06).
001150	       05  R-H9 	   PIC	X(09).
001160	       05  R-H10	   PIC	X(07).
001170	       05  R-H11	   PIC	X(09).
001180	       05  R-H12	   PIC	X(06).
001190	       05  R-H13	   PIC	X(04).
001200	   03  R-L7 REDEFINES W02-PRN-LINE.
001210	       05  R-I2 	   PIC	X(13).
001220	       05  FILLER	   PIC	X(01).
001230	       05  R-DESC2	   PIC	X(25).
001240	       05  R-LEV	   PIC	Z(04)9.
001250	       05  FILLER	   PIC	X(01).
001260	       05  R-Q2 	   PIC	Z(04)9.
001270	       05  FILLER	   PIC	X(01).
001280	       05  R-COST1	   PIC	Z(04)9.99.
001290	       05  FILLER	   PIC	X(01).
001300	       05  R-AVGE	   PIC	Z(04)9.99.
001310	       05  FILLER	   PIC	X(01).
001320	       05  R-NMTD	   PIC	Z(04)9.
001330	       05  FILLER	   PIC	X(01).
001340	       05  R-NYTD	   PIC	Z(04)9.
001350	   03  R-L8 REDEFINES W02-PRN-LINE.
001360	       05  R-H14	   PIC	X(07).
001370	       05  R-H14A	   PIC	X(41).
001380	       05  R-H15	   PIC	X(09).
001390	       05  R-H16	   PIC	X(15).
001400	       05  R-H17	   PIC	X(08).
001410	   03  R-L9 REDEFINES W02-PRN-LINE.
001420	       05  R-AC 	   PIC	X(06).
001430	       05  FILLER	   PIC	X(01).
001440	       05  R-DNME	   PIC	X(39).
001450	       05  FILLER	   PIC	X(01).
001460	       05  R-REF	   PIC	X(08).
001470	       05  FILLER	   PIC	X(01).
001480	       05  R-DEBT	   PIC	Z(06)9.99-.
001490	       05  FILLER	   PIC	X(02).
001500	       05  R-CASH	   PIC	Z(06)9.99-.
001510	   03  R-L10 REDEFINES W02-PRN-LINE.
001520	       05  R-H18	   PIC	X(06).
001530	       05  R-H19	   PIC	X(32).
001540	       05  R-H20	   PIC	X(14).
001550	       05  R-H21	   PIC	X(07).
001560	       05  R-H22	   PIC	X(11).
001570	       05  R-H23	   PIC	X(10).
001580	   03  R-L10A REDEFINES W02-PRN-LINE.
001590	       05  FILLER	   PIC	X(04).
001600	       05  RC-H18	   PIC	X(09).
001610	       05  RC-H19	   PIC	X(38).
001620	       05  RC-H20	   PIC	X(14).
001630	       05  RC-H21	   PIC	X(14).
001640	       05  RC-H22	   PIC	X(14).
001650	       05  RC-H23	   PIC	X(12).
001660	       05  RC-H24	   PIC	X(13).
001670	       05  RC-H25	   PIC	X(14).
001680	   03  R-L10B REDEFINES W02-PRN-LINE.
001690	       05  RS-H18	   PIC	X(04).
001700	       05  RS-H19	   PIC	X(36).
001710	       05  RS-H20	   PIC	X(13).
001720	       05  RS-H21	   PIC	X(13).
001730	       05  RS-H22	   PIC	X(13).
001740	       05  RS-H23	   PIC	X(13).
001750	       05  RS-H23A	   PIC	X(13).
001760	       05  RS-H23B	   PIC	X(11).
001770	       05  RS-H24	   PIC	X(08).
001780	       05  RS-H25	   PIC	X(08).
001580	   03  R-L10C REDEFINES W02-PRN-LINE.
001590	       05  FILLER	   PIC	X(01).
001600	       05  RP-H18	   PIC	X(08).
001610	       05  RP-H19	   PIC	X(37).
001620	       05  RP-H20	   PIC	X(13).
001630	       05  RP-H21	   PIC	X(13).
001640	       05  RP-H22	   PIC	X(13).
001650	       05  RP-H23	   PIC	X(09).
001650	       05  RP-H23A	   PIC	X(15).
001660	       05  RP-H24	   PIC	X(13).
001670	       05  RP-H25	   PIC	X(10).
001790	   03  R-L11 REDEFINES W02-PRN-LINE.
001800	       05  FILLER	   PIC	X(46).
001810	       05  R-H24	   PIC	X(21).
001820	       05  R-H25	   PIC	X(13).
001830	   03  R-L11A REDEFINES W02-PRN-LINE.
001840	       05  FILLER	   PIC	X(47).
001850	       05  RS-H26	   PIC	X(25).
001860	       05  RS-H27	   PIC	X(26).
001870	       05  RS-H27A	   PIC	X(22).
001880	       05  RS-H28	   PIC	X(12).
001890	   03  R-L11B REDEFINES W02-PRN-LINE.
001900	       05  FILLER	   PIC	X(56).
001910	       05  RC-H26	   PIC	X(28).
001920	       05  RC-H27	   PIC	X(27).
001930	       05  RC-H28	   PIC	X(21).
001890	   03  R-L11C REDEFINES W02-PRN-LINE.
001900	       05  FILLER	   PIC	X(51).
001910	       05  RP-H26	   PIC	X(26).
001920	       05  RP-H27	   PIC	X(37).
001930	       05  RP-H28	   PIC	X(18).
001940	   03  R-L12 REDEFINES W02-PRN-LINE.
001950	       05  FILLER	   PIC	X(01).
001960	       05  R-LEDG	   PIC	9(03).
001970	       05  FILLER	   PIC	X(02).
001980	       05  R-SDESC	   PIC	X(31).
001990	       05  R-DMTD	   PIC	Z(06)9.99.
002000	       05  FILLER	   PIC	X(01).
002010	       05  R-DYTD	   PIC	Z(06)9.99.
002020	       05  FILLER	   PIC	X(01).
002030	       05  R-CMTD	   PIC	Z(06)9.99.
002040	       05  FILLER	   PIC	X(01).
002050	       05  R-CYTD	   PIC	Z(06)9.99.
002060	   03  R-L12A REDEFINES W02-PRN-LINE.
002070	       05  FILLER	   PIC	X(05).
002080	       05  RC-LEDG	   PIC	9(03).
002090	       05  FILLER	   PIC	X(05).
002100	       05  RC-SDESC	   PIC	X(34).
002110	       05  RC-DMTD	   PIC	Z(06)9.99.
002120	       05  FILLER	   PIC	X(04).
002130	       05  RC-DYTD	   PIC	Z(06)9.99.
002140	       05  FILLER	   PIC	X(04).
002150	       05  RC-CMTD	   PIC	Z(06)9.99.
002160	       05  FILLER	   PIC	X(04).
002170	       05  RC-CYTD	   PIC	Z(06)9.99.
002180	       05  FILLER	   PIC	X(07).
002190	       05  RC-GL1	   PIC	9999/99.
002200	       05  FILLER	   PIC	X(06).
002210	       05  RC-GL2	   PIC	9999/99.
002220	       05  FILLER	   PIC	X(06).
002230	   03  R-L12B REDEFINES W02-PRN-LINE.
002240	       05  RS-LEDG	   PIC	9(03).
002250	       05  FILLER	   PIC	X(02).
002260	       05  RS-SDESC	   PIC	X(33).
002270	       05  RS-DMTD	   PIC	Z(06)9.99.
002280	       05  FILLER	   PIC	X(03).
002290	       05  RS-DYTD	   PIC	Z(06)9.99.
002300	       05  FILLER	   PIC	X(03).
002310	       05  RS-CMTD	   PIC	Z(06)9.99.
002320	       05  FILLER	   PIC	X(03).
002330	       05  RS-CYTD	   PIC	Z(06)9.99.
002340	       05  FILLER	   PIC	X(03).
002350	       05  RS-DSMT	   PIC	Z(06)9.99-.
002360	       05  FILLER	   PIC	X(02).
002370	       05  RS-DSYT	   PIC	Z(06)9.99-.
002380	       05  FILLER	   PIC	X(02).
002390	       05  RS-GL1	   PIC	9999/99.
002400	       05  FILLER	   PIC	X(02).
002410	       05  RS-GL2	   PIC	9999/99.
002060	   03  R-L12C REDEFINES W02-PRN-LINE.
002070	       05  FILLER	   PIC	X(03).
002080	       05  RP-LEDG	   PIC	9(03).
002090	       05  FILLER	   PIC	X(03).
002100	       05  RP-SDESC	   PIC	X(33).
002110	       05  RP-DMTD	   PIC	Z(06)9.99.
002120	       05  FILLER	   PIC	X(03).
002130	       05  RP-DYTD	   PIC	Z(06)9.99.
002140	       05  FILLER	   PIC	X(03).
002150	       05  RP-CMTD	   PIC	Z(06)9.99.
002160	       05  FILLER	   PIC	X(03).
002170	       05  RP-CYTD	   PIC	Z(06)9.99.
002160	       05  FILLER	   PIC	X(03).
002170	       05  RP-ADJ	   PIC	Z(06)9.99-.
002180	       05  FILLER	   PIC	X(03).
002190	       05  RP-GL1	   PIC	9999/99.
002200	       05  FILLER	   PIC	X(06).
002210	       05  RP-GL2	   PIC	9999/99.
002220	       05  FILLER	   PIC	X(04).
002420	   03  R-L13 REDEFINES W02-PRN-LINE.
002430	       05  R-H26	   PIC	X(20).
002440	       05  FILLER	   PIC	X(02).
002450	       05  R-H27	   PIC	X(05).
002460	       05  R-H28	   PIC	X(06).
002470	       05  R-H29	   PIC	X(09).
002480	       05  R-H30	   PIC	X(06).
002490	       05  R-H31	   PIC	X(05).
002500	       05  R-H32	   PIC	X(10).
002510	       05  R-H33	   PIC	X(05).
002520	       05  R-H34	   PIC	X(06).
002530	       05  R-H35	   PIC	X(06).
002540	   03  R-L14 REDEFINES W02-PRN-LINE.
002550	       05  FILLER	   PIC	X(05).
002560	       05  R-H36	   PIC	X(15).
002570	       05  R-N1 	   PIC	Z(04)9.
002580	       05  FILLER	   PIC	X(03).
002590	       05  R-V5 	   PIC	Z(06)9.99-.
002600	       05  FILLER	   PIC	X(01).
002610	       05  R-N2 	   PIC	Z(04)9.
002620	       05  FILLER	   PIC	X(03).
002630	       05  R-V6 	   PIC	Z(06)9.99-.
002640	       05  FILLER	   PIC	X(02).
002650	       05  R-N3 	   PIC	Z(04)9.
002660	       05  FILLER	   PIC	X(03).
002670	       05  R-V7 	   PIC	Z(06)9.99-.
002680	   03  SAL-L1 REDEFINES W02-PRN-LINE.
002690	       05  SAL-H1	   PIC	X(10).
002700	       05  SAL-NO	   PIC	ZZ9.
002710	       05  FILLER	   PIC	X(01).
002720	       05  SAL-SMAN	   PIC	X(30).
002730	       05  FILLER	   PIC	X(36).
002740	   03  SAL-L2 REDEFINES W02-PRN-LINE.
002760	       05  SAL-H2	   PIC	X(15).
002770	       05  SAL-H3	   PIC	X(36).
002780	       05  SAL-H4	   PIC	X(09).
002790	       05  SAL-H5	   PIC	X(11).
002800	       05  SAL-H6	   PIC	X(10).
	       05  SAL-H6A	   PIC	X(11).
	       05  SAL-H6B	   PIC	X(04).
002810	   03  SAL-L3 REDEFINES W02-PRN-LINE.
002830	       05  SAL-ITEM	   PIC	X(14).
002840	       05  FILLER	   PIC	X(01).
002850	       05  SAL-DESC	   PIC	X(30).
002860	       05  FILLER	   PIC	X(01).
002870	       05  SAL-COST	   PIC	Z(06)9.99-.
002880	       05  FILLER	   PIC	X(01).
002890	       05  SAL-SELL	   PIC	Z(06)9.99-.
002900	       05  FILLER	   PIC	X(01).
002910	       05  SAL-GP	   PIC	ZZ9.99-.
002920	       05  FILLER	   PIC	X(04).
	       05  SAL-MARKUP	   PIC	ZZ9.99-.
	       05  FILLER	   PIC	X(04).
	       05  SAL-SUPER	   PIC	ZZ.
	       05  FILLER	   PIC	X(02).
002930	   03  SAL-L4 REDEFINES W02-PRN-LINE.
002940	       05  FILLER	   PIC	X(02).
002950	       05  SAL-H7	   PIC	X(14).
002960	       05  SAL-REF	   PIC	X(08).
002970	       05  FILLER	   PIC	X(02).
002980	       05  SAL-H8	   PIC	X(22).
002990	       05  FILLER	   PIC	X(30).
003000	   03  SAL-L5 REDEFINES W02-PRN-LINE.
003010	       05  FILLER	   PIC	X(02).
003020	       05  SAL-H9	   PIC	X(10).
003030	       05  SAL-H10	   PIC	X(20).
003040	       05  SAL-H11	   PIC	X(12).
003050	       05  SAL-H12	   PIC	X(16).
003060	       05  SAL-H13	   PIC	X(13).
003070	       05  SAL-H14	   PIC	X(07).
003080	   03  SAL-L6 REDEFINES W02-PRN-LINE.
003090	       05  FILLER	   PIC	X(01).
003100	       05  SAL-RF	   PIC	X(08).
003110	       05  FILLER	   PIC	X(03).
003120	       05  SAL-DSC	   PIC	X(11).
003130	       05  FILLER	   PIC	X(04).
003140	       05  SAL-CST	   PIC	Z(06)9.99-.
003150	       05  FILLER	   PIC	X(04).
003160	       05  SAL-SEL	   PIC	Z(06)9.99-.
003170	       05  FILLER	   PIC	X(04).
003180	       05  SAL-PRF	   PIC	Z(06)9.99-.
003190	       05  FILLER	   PIC	X(04).
003200	       05  SAL-GPR	   PIC	ZZ9.99.
003210	       05  FILLER	   PIC	X(02).
	   03  VAT-L1 REDEFINES W02-PRN-LINE.
	       05  VAT-DAT	   PIC	99/99/9999.
	       05  FILLER	   PIC	X(01).
	       05  VAT-REF	   PIC	X(08).
	       05  FILLER	   PIC	X(01).
	       05  VAT-TRAN	   PIC	X(11).
	       05  FILLER	   PIC	X(01).
	       05  VAT-NAME	   PIC	X(14).
	       05  FILLER	   PIC	X(01).
	       05  VAT-TRAN-AMT    PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  VAT-NO-TAX	   PIC	Z(06)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  VAT-TXBL-AMT    PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  VAT-VAT-AMT	   PIC	Z(06)9.99-.
	   03  VAT-L2 REDEFINES W02-PRN-LINE.
	       05  VAT-H1	   PIC	X(09).
	       05  VAT-H2A.
		   07  VAT-H2	   PIC	X(09).
		   07  VAT-H3	   PIC	X(13).
	       05  VAT-H3A	   PIC	X(16).
	       05  VAT-H4	   PIC	X(13).
	       05  VAT-H5	   PIC	X(16).
	       05  VAT-H6	   PIC	X(09).
	       05  VAT-H7	   PIC	X(11).
000680	   03  VAT-L3 REDEFINES W02-PRN-LINE.
000690	       05  V-PGE	   PIC	X(06).
000700	       05  V-PNO	   PIC	Z(03)9.
	       05  V-HEADING.
000710		   07  FILLER	   PIC	X(18).
000720		   07  V-CPY.
		       09  FILLER  PIC	X(08).
		       09  V-HEAD  PIC	X(32).
		   07  V-RATE	   PIC	Z9.99.
		   07  V-PERC	   PIC	X(01).
000730		   07  FILLER	   PIC	X(06).
000740	       05  V-DATE	   PIC	X(06).
000750	       05  V-DTE	   PIC	Z9/99/9999.

       01  W10-DATE.
	   03  W10-DTE	       PIC 99/99/9999.

001840 01  W11-RECORD101.
001850	   03  W11-SALESMAN    PIC  X(30).
      *
      *            s a l e s
      *
001860	   03  W11-SL-MTD      PIC S9(09)V99 COMP-3.
001870	   03  W11-SL-YTD      PIC S9(09)V99 COMP-3.
      *
      *            c o s t
      *
001880	   03  W11-CT-MTD      PIC S9(09)V99 COMP-3.
001890	   03  W11-CT-YTD      PIC S9(09)V99 COMP-3.
      *
      *            c o m m i s i o n
      *
001900	   03  W11-VC-MTD      PIC S9(09)V99 COMP-3.
001910	   03  W11-VC-YTD      PIC S9(09)V99 COMP-3.
      *
      *            c o m m i s s i o n   s c a l e
      *
001920	   03  W11-SCOM.
001930	       05  W11-COMMISION OCCURS 7.
001940		 07  W11-FROM  PIC S9(09)    COMP-3.
001960		 07  W11-CRATE PIC S9(03)V99 COMP-3.
001970	   03  W11-SCOMR REDEFINES W11-SCOM.
001980	       05  W11-FROM1   PIC S9(09)    COMP-3.
002000	       05  W11-CRATE1  PIC S9(03)V99 COMP-3.
002010	       05  W11-FROM2   PIC S9(09)    COMP-3.
002030	       05  W11-CRATE2  PIC S9(03)V99 COMP-3.
002040	       05  W11-FROM3   PIC S9(09)    COMP-3.
002060	       05  W11-CRATE3  PIC S9(03)V99 COMP-3.
002070	       05  W11-FROM4   PIC S9(09)    COMP-3.
002090	       05  W11-CRATE4  PIC S9(03)V99 COMP-3.
002070	       05  W11-FROM5   PIC S9(09)    COMP-3.
002090	       05  W11-CRATE5  PIC S9(03)V99 COMP-3.
002070	       05  W11-FROM6   PIC S9(09)    COMP-3.
002090	       05  W11-CRATE6  PIC S9(03)V99 COMP-3.
002070	       05  W11-FROM7   PIC S9(09)    COMP-3.
002090	       05  W11-CRATE7  PIC S9(03)V99 COMP-3.
      *
      * 	   c o m m i s s i o n	 t y p e
      *
      * 	   Type :   T = Turnover
      * 		    G = Gross profit
      *
	   03  W11-TCOM        PIC  X(01).
	   03  FILLER	       PIC  X(03).

       COPY W12.WS.

       COPY W15.WS.

004220 01  W20-DATE.
	   03  W20-CCYY.
	       05  W20-CC.
		   07  W20-CENT	PIC  9(02).
002650	       05  W20-YY.
002660		   07  W20-YEAR	PIC  9(02).
	   03  W20-CNYR REDEFINES W20-CCYY
			       PIC  9(04).
002670     03  W20-MM.
002680	       05  W20-MONTH   PIC  9(02).
002690     03  W20-DD.
002700	       05  W20-DAY     PIC  9(02).

       01  W22-PERIOD.
           03  FILLER          PIC X(17) VALUE "For the period : ".
	   03  W22-DTE1        PIC Z9/99/9999.
           03  FILLER          PIC X(04) VALUE " to ".
	   03  W22-DTE2        PIC Z9/99/9999.

004310 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
	   03  W25-TOTAL       PIC S9(09)V99  COMP-3.
004370	   03  W25-VALUE       OCCURS 7
			       PIC S9(09)V99  COMP-3.
004380	   03  W25-COM	       PIC S9(09)V99  COMP-3.
	   03  W25-RATE.
	       05  W25-CRATE   PIC  9(03)V99  COMP-3.
	   03  W25-CRTE REDEFINES W25-RATE.
	       05  W25-RCALC   PIC  9V9(04)   COMP-3.

004440 01  W40-INDIC.
004450     03  W40-SMAN.
004460         05  W40-SREP    PIC 9(01) COMP-5 OCCURS 50.
004470     03  W40-DRAW.
004480         05  W40-DREP    PIC 9(01) COMP-5 OCCURS 40.
004490     03  W40-EDIT.
004500         05  W40-KEY     PIC ZZZZ9.
004510         05  W40-PERC    PIC ZZ9.99-.
004520     03  W40-EDIT2 REDEFINES W40-EDIT.
004530         05  FILLER      PIC X(01).
004540         05  W40-AMOUNT  PIC Z(06)9.99-.

       COPY W40.WS.

       COPY FUNCTION.WS.

004600 01  W60-NME-ADD.
004610     03  W60-NAME.
004620         05  W60-NCHAR   PIC X(01)      OCCURS 40.
004630     03  W60-ADDR.
004640         05  W60-ACHAR   PIC X(01)      OCCURS 60.
004650 01  W65-ENTRY.
004660     03  W65-FOUR.
004670         05  W65-CHAR    PIC X(01)      OCCURS 4.
004890 01  W70-WORK.
004900     03  W70-REP         PIC 9(04)     COMP-5.
004910 01  W80-ACCOUNT.
004920     03  W80-ACNO        PIC 9(05).
004960 01  W90-TRAN.
004970	   03  W90-DATE        PIC X(10).
004980     03  W90-DTE REDEFINES W90-DATE
004990			       PIC 99/99/9999.
005000     03  W90-CODE        PIC 9(02).
005010     03  W90-REF         PIC X(06).
005020     03  W90-DB          PIC 9(07)V99  COMP-3.
005030     03  W90-CR          PIC 9(07)V99  COMP-3.
005040     03  W90-BDT         PIC S9(07)V99 COMP-3.
005050     03  W90-BCR         PIC S9(07)V99 COMP-3.
005060     03  W90-DESC        PIC X(12).
005070     03  W90-90          PIC S9(07)V99 COMP-3.
005080     03  W90-60          PIC S9(07)V99 COMP-3.
005090     03  W90-30          PIC S9(07)V99 COMP-3.
005100     03  W90-CUR         PIC S9(07)V99 COMP-3.
005110     03  W90-BAL         PIC S9(07)V99 COMP-3.
005120 01  W95-STM.
005130     03  W95-COMP        PIC X(40).
005140     03  W95-ADD1        PIC X(30).
005150     03  W95-ADD2        PIC X(30).
005160     03  W95-ADD3        PIC X(30).
005170     03  W95-ADD4        PIC X(30).
005180     03  W95-POST        PIC 9(04)     COMP-5.
005190     03  W95-TEL         PIC X(08).
005200 01  W100-DAYS.
005210     03  W100-VAL.
005220         05  FILLER      PIC 9(12) VALUE 000031059090.
005230         05  FILLER      PIC 9(12) VALUE 120151181212.
005240         05  FILLER      PIC 9(12) VALUE 243273304334.
005250     03  W100-MONTH REDEFINES W100-VAL.
005260         05  W100-DAY    PIC 9(03) OCCURS 12.
005270     03  W100-DAY1       PIC 9(05)     COMP-3.
005280     03  W100-DAY2       PIC 9(05)     COMP-3.
005290     03  W100-DATE.
005300         05  W100-DD     PIC 9(02).
005310         05  W100-MM     PIC 9(02).
005320         05  W100-YY     PIC 9(02).
005330     03  W100-DTE REDEFINES W100-DATE
005340                         PIC 9(06).
005350     03  W100-RESULT     PIC 9(05)V99.
005360     03  W100-R1 REDEFINES W100-RESULT.
005370         05  W100-WHOLE  PIC 9(05).
005380         05  W100-DEC    PIC 9(02).
005390 01  W200-TRAN.
005400     03  W200-ACCUM.
005410         05  W200-DT     PIC S9(07)V99 COMP-3 OCCURS 300.
005420         05  W200-CR     PIC S9(07)V99 COMP-3 OCCURS 300.
005430         05  W200-DTOT   PIC S9(07)V99 COMP-3.
005440         05  W200-CTOT   PIC S9(07)V99 COMP-3.
005450         05  W200-COST   PIC S9(07)V99 COMP-3.
005460         05  W200-SELL   PIC S9(07)V99 COMP-3.
005470         05  W200-NET    PIC S9(07)V99 COMP-3.
005470         05  W200-ADJ    PIC S9(07)V99 COMP-3.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
      *   ******    ******   *******   ********  ********  **     ** 
      *  **    **  **    **  **    **  **        **        ***    ** 
      *  **        **        **   **   **        **        ** *   ** 
      *   ******   **        ******    *****     *****     **  *  ** 
      *        **  **        **   **   **        **        **   * ** 
      *  **    **  **    **  **    **  **        **        **    *** 
      *   ******    ******   **    **  ********  ********  **     ** 
      *                                                              
005480 SCREEN SECTION.

       COPY BLANK.CRT.

005520 01  S10.
005530	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 15
005540				 VALUE "SALES STAFF COMMISSION".

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
      *        *******    *******     ******     ****** 
      *        **    **   **    **   **    **   **    **
      *        **    **   **   **    **    **   **
      *        *******    ******     **    **   **
      *        **         **   **    **    **   **
      *        **         **    **   **    **   **    **
      *        **         **    **    ******     ****** 
      *
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006570 AA00M		  SECTION.
006580 AA00.
003240	   IF LS0-SALEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006590	     PERFORM ZA00-INIT.
	     PERFORM BA000.
	     CLOSE NETWORK
		   PARAM.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
006630 AC000-READ              SECTION.

008650 READ-CONTROL.
008660       READ NETWORK.
008670     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008680         GO TO READ-CONTROL-EXIT.
008690     IF WS-STAT1 = "2" OR "3" OR "4"
008700         MOVE 2            TO WS-F-ERROR
008710         PERFORM READ-ERROR.
008720     IF RECORD-LOCKED
008730         PERFORM LOCKED-RECORD
008740         GO TO READ-CONTROL.
008750       MOVE 2              TO WS-F-ERROR.
008760       PERFORM READ-ERROR.

008930 READ-CONTROL-EXIT.
008940       EXIT.

008690 READ-PARAM.
008700       READ PARAM.
008710     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008720         GO TO READ-PARAM-EXIT.
008730     IF WS-STAT1 = "2" OR "3" OR "4"
008740         MOVE 15           TO WS-F-ERROR
008750         PERFORM READ-ERROR.
008760     IF RECORD-LOCKED
008770         PERFORM LOCKED-RECORD
008780         GO TO READ-PARAM.
008790       MOVE 15             TO WS-F-ERROR.
008800       PERFORM READ-ERROR.
      
008960 READ-PARAM-EXIT.
008970       EXIT.

       COPY PRINTUTL.AP3.

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
006940 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       BA000	       SECTION 50.
       BA00.
024760	     DISPLAY CLR-SCREEN.
024770       DISPLAY S10.
	     DISPLAY "Print sales staff commission report" AT 0612
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
024900	     DISPLAY "Are you sure Y/N" AT 0812.
024910	     ACCEPT WS-OPTION AT 0830
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
024970	   IF WS-OPTION = "N"
024980	       GO TO BA999.
	     DISPLAY "Processing:" AT 1012.
	     MOVE 8		 TO W02-PRN-TYPE.
	     PERFORM OPEN-PRINTER.
	     MOVE 1		 TO WS-SNO.
	     MOVE ZERO		 TO WS-PAGE.

028540 BA05.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE "H"		 TO W02-PRN-TYPE.
007080	     MOVE SPACES	 TO R-DETAIL.
007090	     MOVE "Page:"	 TO R-PGE.
007100	     MOVE "Date:"	 TO R-DATE.
007110       ADD 1               TO WS-PAGE.
007120       MOVE WS-PAGE        TO R-PNO.
007130       MOVE W12-TODAY      TO R-DTE.
007140       MOVE W95-COMP       TO R-CPY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007160       MOVE SPACES         TO R-DETAIL.
028560	     MOVE "SALES COMMISSION" TO R-HEAD.
	     PERFORM CALL-PRINTUTL.
028580	     MOVE SPACES	 TO R-DETAIL.
028620	     MOVE "No." 	 TO R-H18.
028630	     MOVE "Sales Person" TO R-H19.
028640	     MOVE "Turnover"	 TO R-H20.
028650	     MOVE "Cost"	 TO R-H21.
	     MOVE "Gross Prof"	 TO R-H22.
	     MOVE "Commission"	 TO R-H23.
	     PERFORM CALL-PRINTUTL.
028670       MOVE SPACES TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       BA08.
010850	     ADD 100 WS-SNO	 GIVING WS-PARKEY.
010860	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF (PAR-SALESMAN = SPACES) OR (PAR-FROM1 = 999999999)
	       GO TO BA30.
	     DISPLAY WS-SNO AT 1024 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " " PAR-SALESMAN WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	   IF NOT (PAR-TCOM = "G" OR "T")
	       MOVE "G"		 TO PAR-TCOM.
	     MOVE PAR-RECORD101  TO W11-RECORD101.
	     MOVE ZERO		 TO W25-COM.
	     MOVE 1		 TO WS-S1.

       BA10.
	     MOVE ZERO		 TO W25-VALUE (WS-S1).
	   IF WS-S1 < 7
	       ADD 1		 TO WS-S1
	       GO TO BA10.
	   IF W11-TCOM = "G"
	       SUBTRACT PAR-CT-MTD FROM PAR-SL-MTD
				 GIVING W25-TOTAL
	   ELSE
	       MOVE PAR-SL-MTD	 TO W25-TOTAL.
	     MOVE 7		 TO WS-S1.

       BA15.
	   IF W11-FROM (WS-S1) = 999999999
	       GO TO BA20.
	   IF NOT (W25-TOTAL > W11-FROM (WS-S1))
	       GO TO BA20.
	     SUBTRACT W11-FROM (WS-S1) FROM W25-TOTAL
				 GIVING W25-VALUE (WS-S1).
	     SUBTRACT W25-VALUE (WS-S1)
				 FROM W25-TOTAL.

       BA20.
	   IF WS-S1 > 1
	       SUBTRACT 1	 FROM WS-S1
	       GO TO BA15.

       BA25.
	     MOVE W11-CRATE (WS-S1)
				 TO W25-CRATE.
	     COMPUTE W25-COM = W25-COM +
			      (W25-VALUE (WS-S1) * W25-RCALC).
	   IF WS-S1 < 7
	       ADD 1		 TO WS-S1
	       GO TO BA25.
028750	     MOVE WS-SNO	 TO R-LEDG.
028760	     MOVE W11-SALESMAN	 TO R-SDESC.
028770	     MOVE W11-SL-MTD	 TO R-DMTD.
028780	     MOVE W11-CT-MTD	 TO R-DYTD.
028790	     COMPUTE R-CMTD = W11-SL-MTD - W11-CT-MTD.
028800	     MOVE W25-COM	 TO R-CYTD.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800         PERFORM BA05.
028860	     MOVE SPACES	 TO R-DETAIL.

       BA30.
	   IF WS-SNO < 50
	       ADD 1		 TO WS-SNO
	       GO TO BA08.
015590	     MOVE "**** END REPORT ****"
				 TO R-CPY.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       BA999.
	     EXIT.

      /
      *    ****    INITIALIZE ROUTINE    ****
      *
029020 ZA00-INIT              SECTION 89.
       ZA00.
041220       MOVE 1 TO WS-S1.
021870	     MOVE SPACES TO WS-MID-LNE.
041240 ZA02.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA02.
             MOVE WS-G9  TO WS-TCH  (1).
             MOVE WS-G10 TO WS-TCH  (80).
             MOVE WS-G11 TO WS-BCH  (1).
             MOVE WS-G12 TO WS-BCH  (80).
             MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2  TO WS-TCHR (16) WS-TCHR (41)
021940                      WS-TCHR (64) WS-TCHR (71).
021950       MOVE WS-G3  TO WS-MCHR (16) WS-MCHR (41)
021960                      WS-MCHR (64) WS-MCHR (71)
			    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4  TO WS-BCHR (16) WS-BCHR (41)
021980                      WS-BCHR (64) WS-BCHR (71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
029030 ZA00-OPEN.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
029040	     DISPLAY CLR-SCREEN.
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
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
029050	     OPEN I-O PARAM LOCK.
029060     IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
029080     IF WS-STATUS NOT = "00"
029090	       MOVE 15		 TO WS-F-ERROR
029100         PERFORM OPEN-ERROR.
	     OPEN I-O NETWORK.
029250     IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
029330     IF WS-STATUS NOT = "00"
029340	       MOVE 2		 TO WS-F-ERROR
029350         PERFORM OPEN-ERROR.
	     MOVE 1		 TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
029620 ZA10.
029890	     MOVE 1		 TO WS-PARKEY.
029900       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
029910	     MOVE PAR-DMY	 TO W12-TODAY.
029920	     MOVE PAR-YMD	 TO W12-T-YMD.
029930	     MOVE PAR-COMPANY	 TO W95-COMP.
030190       GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
	     ACCEPT WS-OPTION AT 1238
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5.
             GO TO ZA205.
030200 ZA200.
             DISPLAY "This program can only be run when no other worksta
      -      "tions are logged into the" AT 0803
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
             DISPLAY "accounting system. All other workstations should r
      -      "eturn to the program con-" AT 0903
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
             DISPLAY "trol menu or exit from the accounting system befor
      -      "e you may continue       " AT 1003
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
030210       DISPLAY "Files locked - Try later" AT 2312
030220                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
030230       DISPLAY "Press any key" AT 2512 
030240		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
030250	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
030280 ZA999.
030290       EXIT.
      /
030740 ZB00-ERROR	      SECTION 90.
      
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
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041850	   IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
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
	   IF WS-STAT1 = 9
045390	       DISPLAY "Status " AT 1212
			WS-STAT1 WITH FOREGROUND-COLOR 11
			WS-STAT2 WITH FOREGROUND-COLOR 11
	   ELSE
045390	       DISPLAY "Status " AT 1212
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
