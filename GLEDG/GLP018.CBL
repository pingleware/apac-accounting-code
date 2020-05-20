      $set LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *	   ******   **	      *******	  ****	     **      ******    *
      *   **	**  **	      **    **	 **  **     ***     **	  **   *
      *   **	    **	      **    **	**    **     **     **	  **   *
      *   **	    **	      *******	**    **     **      ******    *
      *   **   ***  **	      **	**    **     **     **	  **   *
      *   **	**  **	      **	 **  **      **     **	  **   *
      *    ******   ********  **	  ****	   ******    ******    *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *     P O S T   D A T E D   C H E Q U E S 		       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLP018.
000030 AUTHOR.                 J.W. LEMMON (APAC).
000040 DATE-WRITTEN.           MARCH 1984.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1984 - 2008
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

000200 COPY GACCNT.SL.

000220 COPY GLTRAN.SL.

000260 COPY GLJRN.SL.

       COPY GPDCHQ.SL.

000140 COPY PARAM.SL.

000280 COPY RECOVER.SL.

       COPY SHARED.SL.

000310 DATA DIVISION.
000320 FILE SECTION.

       COPY APACFIDS.FDE.

000140 COPY CONTROL.FDE.

000400 COPY GACCNT.FDE.

000420 COPY GLTRAN.FDE.

000460 COPY GLJRN.FD.

       COPY GPDCHQ.FDE.

000340 COPY PARAM.FDE.

000480 COPY RECOVER.GFD.

       COPY SHARED.FDE.

      /
002060 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001790 77  WS-SUB	    PIC  9(04)	  COMP-5.
001790 77  WS-IXS           PIC  9(04)    COMP-5.
001800 77  WS-IXS1          PIC  9(04)    COMP-5.
001810 77  WS-IXS2          PIC  9(04)    COMP-5.
001820 77  WS-IXS3          PIC  9(04)    COMP-5.
001830 77  WS-IXS4          PIC  9(04)    COMP-5.
002070 77  WS-PARKEY       PIC 9(04)     COMP-5 VALUE 1.
002070 77  WS-DEPKEY       PIC 9(04)     COMP-5 VALUE 1.
002070 77  WS-NETKEY       PIC 9(04)     COMP-5 VALUE 1.
002080 77  WS-GLKEY        PIC 9(04)     COMP-5 VALUE 1.
002090 77  WS-GTRKEY       PIC 9(04)     COMP-5 VALUE 1.
002100 77  WS-GINKEY       PIC 9(04)     COMP-5 VALUE 1.
002110 77  WS-CTRKEY       PIC 9(04)     COMP-5 VALUE 1.
002120 77  WS-CREKEY       PIC 9(04)     COMP-5 VALUE 1.
003330 77  WS-CINDKEY      PIC 9(04)     COMP-5 VALUE 1.
002130 77  WS-KSTORE       PIC 9(04)     COMP-5.
002140 77  WS-KEYSTR       PIC  9(04)    COMP-5.
002150 77  WS-GLJSTR       PIC  9(04)    COMP-5.
002160 77  WS-GLJKEY       PIC  9(04)    COMP-5.
002170 77  WS-RECKEY       PIC 9(04)     COMP-5.
002180 77  WS-RECOVER      PIC 9(04)     COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002190 77  WS-TRANS        PIC 9(04)     COMP-5 VALUE 1.
002200 77  WS-ISUB         PIC 9(04)     COMP-5.
002210 77  WS-CHEQUE       PIC 9(04)     COMP-5.
002220 77  WS-PAGE         PIC 9(04)     COMP-5.
002230 77  WS-LINES        PIC 9(04)     COMP-5 VALUE 0.
002240 77  WS-S            PIC 9(04)     COMP-5.
002250 77  WS-S1           PIC 9(04)     COMP-5.
002260 77  WS-S2           PIC 9(04)     COMP-5.
002270 77  WS-S3           PIC 9(04)     COMP-5.
002280 77  WS-S4	   PIC 9(04)	 COMP-5.
002280 77  WS-S5	   PIC 9(04)	 COMP-5.
002280 77  WS-S6           PIC 9(04)     COMP-5.
002280 77  WS-S7	   PIC 9(04)	 COMP-5.
002280 77  WS-S8	   PIC 9(04)	 COMP-5.
001900 77  WS-Y-END        PIC  9(02) VALUE ZERO.
001900 77  WS-L-END	   PIC	9(04) VALUE ZERO.
001900 77  WS-OPEN         PIC  9(02) VALUE ZERO.
001910 77  WS-MS1          PIC  9(04)    COMP-5.
001920 77  WS-MS2          PIC  9(04)    COMP-5.
001930 77  WS-VALUE        PIC S9(09)V99 COMP-3.
002080 77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
002290 77  WS-DEL	   PIC	X(01).
002290 77  WS-XFRC         PIC  X(01) VALUE "N".
002300 77  WS-NUM          PIC Z9.
002310 77  WS-UPDATE       PIC 9(01).
002320 77  WS-CONREC       PIC  9(01) VALUE ZERO.
002330 77  WS-GLCON        PIC  9(01) VALUE ZERO.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003650 77  WS-COND	   PIC	X(06).
003660 77  WS-NORM	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
002370 77  WS-HDTE         PIC X(05) VALUE "DATE".
002380 77  WS-HACNO        PIC X(06) VALUE "A/C NO".
002390 77  WS-HQNT         PIC X(05) VALUE "QUANT".
002400 77  WS-HDESC        PIC X(11) VALUE "DESCRIPTION".
002410 77  WS-HCOST        PIC X(07) VALUE "C/PRICE".
002430 77  WS-HNAME        PIC X(04) VALUE "NAME".
002450 77  WS-HCHQ         PIC X(06) VALUE "CHEQUE".
002530 77  WS-OPTION       PIC X(01).
001970 77  WS-SKIP         PIC  X(01) VALUE "N".
002780 77  WS-LOOK	   PIC	X(01) VALUE "C".
002540 77  WS-FIRST	   PIC	9(02) VALUE 0.
002550 77  WS-LAST         PIC  9(02) VALUE 0.
003560 77  WS-CHAR1        PIC X(01).
002570 77  WS-ERROR        PIC 9(01)     VALUE ZERO.
003620 77  WS-KEY1         PIC 9(04)     COMP-5.
000630 77  WS-ENT          PIC X(08).
000640 77  WS-ENT1         PIC X(08).
002600 77  WS-BATCH        PIC 9(05)     COMP-3.
002610 77  WS-TNEXT        PIC 9(04)     COMP-5.
002620 77  WS-SELECT       PIC X(13)     VALUE
002630                     "Select Option".
002640 77  WS-H1           PIC X(06)     VALUE "DATE:".
002650 77  WS-H3           PIC X(07)     VALUE "BATCH:".
002660 77  WS-H4           PIC X(07)     VALUE "PAGE:".
002690 77  WS-ER1	   PIC X(08)	 VALUE "Account".
002700 77  WS-ER3          PIC X(05)     VALUE "Date".
002710 77  WS-ER4          PIC X(06)     VALUE "Value".
002720 77  WS-ER5          PIC X(07)     VALUE "Cheq no".
002730 77  WS-ER6          PIC X(11)     VALUE "No record".
002740 77  WS-ER7          PIC X(13)     VALUE "Group heading".
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
	   03  WS-MDGL-LNE REDEFINES WS-MID-LNE.
	       05  WS-MGL1     PIC X(01).
	       05  WS-MGL78    PIC X(78).
	       05  WS-MGL80    PIC X(01).
002450	   03  WS-MID-LNE2.
002460         05  WS-MCH  PIC X(01) OCCURS 80.
	   03  WS-MID78 REDEFINES WS-MID-LNE2.
	       05  FILLER      PIC  X(01).
	       05  WS-BLNK78   PIC  X(78).
	       05  FILLER      PIC  X(01).
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
	   03  WS-MDCR-LNE REDEFINES WS-MD-LNE.
	       05  WS-MCR1     PIC  X(01).
	       05  WS-MCR78    PIC  X(78).
	       05  WS-MCR80    PIC  X(01).
002470	   03  WS-BT-LNE.
002480         05  WS-BCR  PIC X(01) OCCURS 80.

002760 COPY WS.WS.

       COPY LEDTRF.WS.

002250 01  W00-TRAN-CODES.
           03  W00-C-ABR   PIC  X(03).
001410     03  W00-C-DESC  PIC  X(15).
001430     03  W00-C-ACT   PIC  9(01).
001440     03  W00-C-CODE  PIC  9(02)    COMP-5.
001450     03  W00-C-GL    PIC  9(06)    COMP-3.
001460     03  W00-C-DAY   PIC  9(06)    COMP-5.
001470     03  W00-C-VAL   PIC S9(09)V99 COMP-3.
001480     03  W00-C-MTD   PIC  9(06)    COMP-5.
001490     03  W00-C-MTDV  PIC S9(09)V99 COMP-3.
001500     03  W00-C-YTD   PIC  9(06)    COMP-5.
001510     03  W00-C-YTDV  PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002870 01  W02-FID.

       COPY APACFIDS.ID.

000140 COPY CONTROL.ID.

000400 COPY GACCNT.ID.

000420 COPY GLTRAN.ID.

000460 COPY GLJRN.ID.

       COPY GPDCHQ.ID.

       COPY PARAM.ID.

000480 COPY RECOVER.ID.

       COPY SHARED.ID.

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

000540	   03  R-L2 REDEFINES W02-PRN-LINE.
000550	       05  R-H1        PIC X(07).
000560	       05  R-DTE1      PIC Z9/99/99.
000570	       05  FILLER      PIC X(33).
000580	       05  R-H2        PIC X(40).
000590	       05  FILLER      PIC X(33).
000600	       05  R-H3        PIC X(08).
000610	       05  R-PGE       PIC ZZ9.

	   03  RG-L2 REDEFINES W02-PRN-LINE.
	       05  RG-H1       PIC  X(06).
	       05  RG-DTE1     PIC  Z9/99/99.
	       05  FILLER      PIC  X(25).
	       05  RG-H2       PIC  X(40).
	       05  FILLER      PIC  X(15).
	       05  RG-H3       PIC  X(07).
	       05  RG-BTCH     PIC  ZZZZ9.
	       05  FILLER      PIC  X(17).
	       05  RG-H4       PIC  X(06).
	       05  RG-PGE      PIC  ZZ9.

000620	   03  R-L2A REDEFINES W02-PRN-LINE.
000630	       05  FILLER      PIC X(46).
000640	       05  R-HEAD      PIC X(40).
000650	       05  FILLER      PIC X(46).

000660	   03  R-L3A REDEFINES W02-PRN-LINE.
000670	       05  FILLER      PIC X(05).
000680	       05  I-H1        PIC X(11).
000690	       05  I-H2        PIC X(34).
000700	       05  I-H3        PIC X(44).
000710	       05  I-H4        PIC X(11).
000720	       05  I-H5        PIC X(27).

000730	   03  R-L4 REDEFINES W02-PRN-LINE.
000740	       05  G-H1        PIC X(10).
000750	       05  G-SUPP      PIC X(05).
000760	       05  FILLER      PIC X(02).
000770	       05  G-NAMER     PIC X(40).
000780	       05  FILLER      PIC X(23).

000790	   03  R-L4A REDEFINES W02-PRN-LINE.
000800	       05  G-H3        PIC X(05).
000810	       05  G-INVN      PIC Z(07)9.
000820	       05  FILLER      PIC X(01).
000830	       05  G-H4        PIC X(08).
000850	       05  FILLER      PIC X(01).
000840	       05  G-INVD      PIC Z9/99/99.
000860	       05  G-H5        PIC X(06).
000870	       05  G-RAIL      PIC Z(04)9.99-.
000880	       05  FILLER      PIC X(01).
000890	       05  G-H6        PIC X(06).
000900	       05  G-DISC      PIC Z(04)9.99-.
000910	       05  FILLER      PIC X(01).
000920	       05  G-H7        PIC X(06).
000930	       05  G-TOTAL     PIC Z(06)9.99-.

	   03  RG-L5 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(65).
	       05  RG-H16      PIC  X(10).
	       05  FILLER      PIC  X(01).
	       05  RG-H17      PIC  X(15).
	       05  RG-H18      PIC  X(10).
	       05  FILLER      PIC  X(31).

000940	   03  R-L5A REDEFINES W02-PRN-LINE.
000950	       05  FILLER      PIC X(05).
000960	       05  I-ACNO      PIC 9(04)/99.
000970	       05  FILLER      PIC X(04).
000980	       05  I-NAME      PIC X(30).
000990	       05  FILLER      PIC X(04).
001000	       05  I-NAR       PIC X(36).
001010	       05  FILLER      PIC X(02).
001020	       05  I-VALUE     PIC Z(08)9.99-.
001030	       05  FILLER      PIC X(04).
001040	       05  I-30        PIC X(04).
001050	       05  I-60        PIC X(04).
001060	       05  I-90        PIC X(04).
001070	       05  I-120       PIC X(04).
001080	       05  I-150       PIC X(04).
001090	       05  I-180       PIC X(07).

001100	   03  R-L6 REDEFINES W02-PRN-LINE.
001110	       05  D-H1        PIC X(06).
001120	       05  D-H2        PIC X(10).
001130	       05  D-H3        PIC X(08).
001140	       05  D-H4        PIC X(10).
001150	       05  D-H5        PIC X(08).
001160	       05  D-H6        PIC X(13).
001170	       05  D-H7        PIC X(13).
001180	       05  D-H8        PIC X(12).

001190	   03  R-L6A REDEFINES W02-PRN-LINE.
001200	       05  D-ACNO      PIC X(05).
001210	       05  FILLER      PIC X(02).
001220	       05  D-NAME      PIC X(73).

001230	   03  R-L6B REDEFINES W02-PRN-LINE.
001240	       05  FILLER      PIC X(06).
001250	       05  D-DNO       PIC Z(05)9.
001260	       05  FILLER      PIC X(02).
001270	       05  D-DNDTE     PIC Z9/99/99.
001280	       05  FILLER      PIC X(01).
001290	       05  D-INV       PIC Z(07)9.
001300	       05  FILLER      PIC X(01).
001310	       05  D-IDTE      PIC Z9/99/99.
001320	       05  FILLER      PIC X(01).
001330	       05  D-COST      PIC Z(07)9.99-.
001340	       05  FILLER      PIC X(01).
001350	       05  D-SELL      PIC Z(08)9.99-.
001360	       05  FILLER      PIC X(01).
001370	       05  D-30        PIC X(02).
001380	       05  D-60        PIC X(02).
001390	       05  D-90        PIC X(02).
001400	       05  D-120       PIC X(02).
001410	       05  D-150       PIC X(02).
001420	       05  D-180       PIC X(02).

001430	   03  R-L7 REDEFINES W02-PRN-LINE.
001440	       05  CH-H1       PIC X(07).
001450	       05  CH-H2       PIC X(32).
001460	       05  CH-H3       PIC X(08).
001470	       05  CH-H4       PIC X(13).
001480	       05  CH-H5       PIC X(08).
001490	       05  CH-H6       PIC X(12).

001500	   03  R-L7A REDEFINES W02-PRN-LINE.
001510	       05  CH-ACNO     PIC X(05).
001520	       05  FILLER      PIC X(01).
001530	       05  CH-NAME     PIC X(29).
001540	       05  FILLER      PIC X(02).
001550	       05  CH-DATE     PIC Z9/99/99.
001560	       05  FILLER      PIC X(02).
001570	       05  CH-CHQ      PIC Z(05)9.
001580	       05  FILLER      PIC X(01).
001590	       05  CH-VAL      PIC Z(08)9.99-.
001600	       05  FILLER      PIC X(01).
001610	       05  CH-30       PIC X(02).
001620	       05  CH-60       PIC X(02).
001630	       05  CH-90       PIC X(02).
001640	       05  CH-120      PIC X(02).
001650	       05  CH-150      PIC X(02).
001660	       05  CH-180      PIC X(02).

001670	   03  R-L8 REDEFINES W02-PRN-LINE.
001680	       05  FILLER      PIC  X(03).
001690	       05  R-H5        PIC  X(11).
001700	       05  R-H6        PIC  X(05).
001710	       05  FILLER      PIC  X(01).
001720	       05  R-H7        PIC  X(14).
001730	       05  R-H8        PIC  X(05).
001740	       05  FILLER      PIC  X(05).
001750	       05  R-H9        PIC  X(09).
001760	       05  R-H10       PIC  X(12).
001770	       05  R-H11       PIC  X(13).
001780	       05  R-H12       PIC  X(10).
001790	       05  R-H13       PIC  X(13).
001800	       05  FILLER      PIC  X(08).
001810	       05  R-H14       PIC  X(14).
001820	       05  R-H15       PIC  X(09).

	   03  RG-L8 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(03).
	       05  RG-H5       PIC  X(11).
	       05  RG-H6       PIC  X(05).
	       05  FILLER      PIC  X(01).
	       05  RG-H7       PIC  X(13).
	       05  RG-H8       PIC  X(05).
	       05  FILLER      PIC  X(06).
	       05  RG-H9       PIC  X(09).
	       05  RG-H10      PIC  X(12).
	       05  RG-H11      PIC  X(13).
	       05  FILLER      PIC  X(01).
	       05  RG-H12      PIC  X(09).
	       05  RG-H13      PIC  X(13).
	       05  FILLER      PIC  X(08).
	       05  RG-H14      PIC  X(14).
	       05  RG-H15      PIC  X(09).

001830	   03  R-L9 REDEFINES W02-PRN-LINE.
001840	       05  FILLER      PIC  X(03).
001850	       05  R-ACC       PIC  9(04)/99.
001860	       05  FILLER      PIC  X(04).
001870	       05  R-ANME      PIC  X(24).
001880	       05  FILLER      PIC  X(04).
001890	       05  R-DTE2      PIC  Z9/99/99.
001900	       05  FILLER      PIC  X(04).
001910	       05  R-REFX.
		   07  R-REF   PIC  Z(07).
001920	       05  FILLER      PIC  X(04).
001930	       05  R-NAR       PIC  X(36).
001940	       05  FILLER      PIC  X(03).
001950	       05  R-DBT       PIC  Z(08)9.99.
001960	       05  FILLER      PIC  X(02).
001970	       05  R-CRD       PIC  Z(08)9.99-.
001980	       05  FILLER      PIC  X(01).

001990	   03  R-L10 REDEFINES W02-PRN-LINE.
002000	       05  FILLER      PIC X(65).
002010	       05  R-H16       PIC X(06).
002020	       05  FILLER      PIC X(01).
002030	       05  R-H17       PIC X(23).
002040	       05  R-H18       PIC X(06).
002050	       05  FILLER      PIC X(31).

000770	   03  R2-L2 REDEFINES W02-PRN-LINE.
000780	       05  R2-H1       PIC X(06).
000790	       05  R2-DTE1     PIC Z9/99/99.
000800	       05  FILLER      PIC X(10).
000810	       05  R2-H2       PIC X(40).
000820	       05  R2-H2A      PIC X(27).
000830	       05  FILLER      PIC X(10).
000840	       05  R2-H3       PIC X(07).
000850	       05  R2-BTCH     PIC Z(04)9.
000860	       05  FILLER      PIC X(10).
000870	       05  R2-H4       PIC X(06).
000880	       05  R2-PGE      PIC ZZ9.

000570	   03  R2-L3 REDEFINES W02-PRN-LINE.
000580	       05  FILLER      PIC  X(03).
000590	       05  R2-H5       PIC  X(11).
000600	       05  R2-H6       PIC  X(05).
000610	       05  FILLER      PIC  X(01).
000620	       05  R2-H7       PIC  X(13).
000630	       05  R2-H8       PIC  X(05).
000640	       05  FILLER      PIC  X(06).
000650	       05  R2-H9       PIC  X(09).
000660	       05  R2-H10      PIC  X(12).
000670	       05  R2-H11      PIC  X(13).
000680	       05  FILLER      PIC  X(01).
000690	       05  R2-H12      PIC  X(09).
000700	       05  R2-H13      PIC  X(13).
000710	       05  FILLER      PIC  X(08).
000720	       05  R2-H14      PIC  X(14).
000730	       05  R2-H15      PIC  X(09).

000740	   03  R2-L4 REDEFINES W02-PRN-LINE.
000750	       05  FILLER      PIC  X(03).
000760	       05  R2-ACC      PIC  9(04)/99.
000770	       05  FILLER      PIC  X(04).
000780	       05  R2-ANME     PIC  X(24).
000790	       05  FILLER      PIC  X(04).
000800	       05  R2-DTE2     PIC  Z9/99/99.
000810	       05  FILLER      PIC  X(04).
000820	       05  R2-REF      PIC  Z(07).
000830	       05  FILLER      PIC  X(04).
000840	       05  R2-NAR      PIC  X(36).
001170	       05  FILLER      PIC X(01).
	       05  R2-DBRKO    PIC X(01).
001180	       05  R2-DBT      PIC Z(08)9.99.
	       05  R2-DBRKC    PIC X(01).
	       05  R2-CBRKO    PIC X(01).
001200	       05  R2-CRD      PIC Z(08)9.99-.
	       05  R2-CBRKC    PIC X(01).
001210	       05  FILLER      PIC X(01).

	   03  PD-L1 REDEFINES W02-PRN-LINE.
	       05  PD-H1.
		   07  PD-H2   PIC  X(06).
		   07  PD-H3   PIC  X(41).
	       05  PD-H4.
		   07  PD-H5   PIC  X(07).
		   07  PD-H6   PIC  X(07).
	       05  PD-H7.
		   07  PD-H8   PIC  X(43).
		   07  PD-H9   PIC  X(07).
		   07  PD-H10  PIC  X(15).
		   07  PD-H11  PIC  X(06).

	   03  PD-L2 REDEFINES W02-PRN-LINE.
	       05  PD-ACNO     PIC  X(05).
	       05  FILLER      PIC  X(01).
	       05  PD-NAME     PIC  X(40).
	       05  FILLER      PIC  X(01).
	       05  PD-BANK     PIC  9(06).
	       05  FILLER      PIC  X(01).
	       05  PD-CRED     PIC  9(06).
	       05  FILLER      PIC  X(01).
	       05  PD-NAR      PIC  X(40).
	       05  FILLER      PIC  X(01).
	       05  PD-DATE     PIC  Z9/99/99.
	       05  FILLER      PIC  X(01).
	       05  PD-REF      PIC  Z(06)9.
	       05  FILLER      PIC  X(01).
	       05  PD-VALUE    PIC  Z(08)9.99.
	       05  FILLER      PIC  X(01).

	   03  PD-L3 REDEFINES W02-PRN-LINE.
	       05  PG-DATE     PIC  Z9/99/9999.
	       05  FILLER      PIC  X(01).
	       05  PG-REF      PIC  Z(06)9.
	       05  FILLER      PIC  X(01).
	       05  PG-D-VALUE  PIC  Z(10)9.99.
	       05  FILLER      PIC  X(01).
	       05  PG-C-VALUE  PIC  Z(10)9.99-.
	       05  FILLER      PIC  X(02).
	       05  PG-GL-AC    PIC  9(04)/99.
	       05  FILLER      PIC  X(02).
	       05  PG-AC-NAME  PIC  X(30).
	       05  FILLER      PIC  X(02).
	       05  PG-PAYEE    PIC  X(40).

	   03  PD-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(03).
	       05  PG-H1       PIC  X(08).
	       05  PG-H2       PIC  X(17).
	       05  PG-H3       PIC  X(14).
	       05  PG-H4       PIC  X(09).
	       05  PG-H5       PIC  X(09).
	       05  PG-H6       PIC  X(32).
	       05  PG-H7       PIC  X(40).

       COPY W05.GL.

       COPY W05.VAT.

003250 01  W10-H2.
003260     03  FILLER          PIC X(27).
003270     03  W10-HEAD        PIC X(13).

       COPY W15.GL.

003550	   03  W15-COST        PIC S9(09)V99.
003560     03  W15-TCOST       PIC S9(09)V99 COMP-3.
003570     03  W15-SETT        PIC 9(06).
003580     03  W15-SET         REDEFINES W15-SETT.
003590         05  W15-30      PIC 9(01).
003600         05  W15-60      PIC 9(01).
003610         05  W15-90      PIC 9(01).
003620         05  W15-120     PIC 9(01).
003630         05  W15-150     PIC 9(01).
003640         05  W15-180     PIC 9(01).
003650     03 W15-PAY          REDEFINES W15-SETT.
003660        05  W15-MONTH    PIC 9(01) OCCURS 6.
003670     03  W15-INVN        PIC 9(08)    COMP-3.
003680     03  W15-INVD        PIC 9(06)    COMP-3.
003690     03  W15-GRNN        PIC 9(06)    COMP-3.
003700     03  W15-GRND        PIC 9(06)    COMP-3.
003710     03  W15-CHQVAL      PIC S9(09)V99 COMP-3.
003720     03  W15-DISCOUNT    PIC S9(09)V99 COMP-3.
003730     03  W15-RAIL        PIC S9(09)V99 COMP-3.
           03  W15-INCL        PIC S9(09)V99 COMP-3.
           03  W15-EXCL        PIC S9(09)V99 COMP-3.
           03  W15-TXBL        PIC S9(09)V99 COMP-3.
           03  W15-NTXBL       PIC S9(09)V99 COMP-3.
           03  W15-VAT         PIC S9(09)V99 COMP-3.
           03  W15-LVAT        PIC S9(09)V99 COMP-3.
           03  W15-TRNTOT      PIC S9(09)V99 COMP-3.

003750 01  W20-TOTALS.
003760     03  W20-DEBIT       PIC S9(09)V99 COMP-3.
003770     03  W20-CREDIT      PIC S9(09)V99 COMP-3.
003770     03  W20-VAT         PIC S9(09)V99 COMP-3.
003780     03  W20-RESULT      PIC S9(09)V99.
003790     03  W20-RES1        REDEFINES W20-RESULT.
003800         05  W20-WHOLE   PIC 9(09).
003810         05  W20-DEC     PIC S9(02).
003820     03  W20-TOTAL       PIC S9(09)V99 COMP-3.
003830     03  W20-VALUE       PIC S9(09)V99 COMP-3.

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

003840 01  W25-EDIT.
003850     03  W25-3V2.
003860         05  W25-2       PIC ZZ9.99.
003870     03  W25-7V2.
003880         05  W25-3       PIC Z(06)9.99.
003890     03  W25-3N.
003900         05  W25-4       PIC ZZ9.
003910     03  W25-DATE.
003920	       05  W25-DTE     PIC 99/99/9999.
003930     03  W25-6.
003940         05  W25-NO      PIC Z(07)9.
003950     03  W25-5V2.
003960         05  W25-VAL     PIC Z(04)9.99.
003970     03  W25-S5V2.
003980         05  W25-SVAL    PIC Z(04)9.99DB.
003990     03  W25-S7V2.
004000         05  W25-VAL2    PIC Z(06)9.99DB.
003990	   03  W25-S8V2.
004000	       05  W25-VG1     PIC Z(07)9.99CR.
003990	   03  W25-72.
004000         05  W25-VAL3    PIC Z(06)9.99-.
004010     03  W25-REF.
004020         05  W25-REFER   PIC Z(07).

004030 01  W25-VALUES.
004040     03  W25-CFLOW  OCCURS 6 PIC S9(07)V99  COMP-3.
004050     03  W25-V1          PIC S9(07)V99  COMP-3. 
004060     03  W25-V2          PIC S9(07)V99  COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

005450 COPY W50.WS.

004070 01  W50-H4.
004080	   03  W50-HEAD        PIC X(40).
004090 01  W55-DEL.
004100     03  W55-MNTH        PIC X(03).
004110     03  FILLER          PIC X(01)   VALUE ",".
004120     03  W55-YY          PIC 9(02).

004130 01  W60-H2.
004140	   03  W60-DESC        PIC X(20)   VALUE
004150				   "G/LEDGER AUDIT TRIAL".
004180 01  W70-ACTS.
004190     03  W70-BRANCH      PIC 9(04)     COMP-5.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
	   03  W75-GNO	      PIC 9(06) OCCURS 18.

004410 01  W90-COMP.
004420     03  W90-CNAME       PIC X(40).
003980 01  W90-TRAN.
004020     03  W90-CODE        PIC  9(02).
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004430 SCREEN SECTION.

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
000860	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "G/LEDGER POST DATED CHEQUES".

       01  S-MENU.
	   03  LINE  4 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³  ".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "PD CHEQUE OPTIONS".
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "  ³".
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Cheques Issued    ³".
	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Amend Cheque      ³".
001470	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Cancel Cheque     ³".
001470	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Post Cheques Due  ³".
001570	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit              ³".
	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".


       01  S-CHEQUE.
      *	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
      *				 VALUE "ISSUE PD CHEQUE".
	   03  LINE  4 COLUMN 14 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
	   VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 65 FOREGROUND-COLOR 5 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "¿".
	   03  LINE  5 COLUMN 14 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 16 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "Date    ".
	   03	       COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "        ".
	   03	       COLUMN 32 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 VALUE "               ".
	   03	       COLUMN 47 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "Chq.No.".
	   03	       COLUMN 54 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 VALUE "           ".
	   03	       COLUMN 65 FOREGROUND-COLOR 5 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "³".
	   03  LINE  6 COLUMN 14 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 16 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "Payee   ".
	   03	       COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE
			    "                                        ".
	   03	       COLUMN 64 FOREGROUND-COLOR 5 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE " ³".
	   03  LINE  7 COLUMN 14 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 16 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "Bank Ac ".
	   03	       COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "      ".
	   03	       COLUMN 30 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE
				  ":				  ".
	   03	       COLUMN 61 FOREGROUND-COLOR 5 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "    ³".
	   03  LINE  8 COLUMN 14 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 16 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "Narrat. ".
	   03	       COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE
			    "    				     ".
	   03	       COLUMN 64 FOREGROUND-COLOR 5 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE " ³".
	   03  LINE  9 COLUMN 14 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 16 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "G/L. Ac ".
	   03	       COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "      ".
	   03	       COLUMN 30 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE
				  ":				  ".
	   03	       COLUMN 61 FOREGROUND-COLOR 5 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "    ³".
	   03  LINE 10 COLUMN 14 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 16 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "Narrat. ".
	   03	       COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE
			    "    				     ".
	   03	       COLUMN 64 FOREGROUND-COLOR 5 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE " ³".
	   03  LINE 11 COLUMN 14 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 16 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE "Amount  ".
	   03	       COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 VALUE "              ".
	   03	       COLUMN 38 FOREGROUND-COLOR 5 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE
				 "     			     ³".
	   03  LINE 12 COLUMN 14 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "À".
	   03	       COLUMN 15 FOREGROUND-COLOR 5 HIGHLIGHT
				 BACKGROUND-COLOR 5
	   VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  SA-CHEQUE.
	   02  SA-CHQ-DTE.
	   03  LINE  5 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC 99/99/99
				 USING W15-DATE AUTO.
	   02  SA-CHQ-NO.
	   03  LINE  5 COLUMN 55 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC ZZZZZZZZ
				 USING W15-CHQ-NO AUTO.

	   02  SA-CHQ-PAY.
	   03  LINE  6 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC X(40)
				 USING W15-PAYEE AUTO.

	   02  SA-CHQ-BANK.
	   03  LINE  7 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC 9(06)
				 USING W15-BANK AUTO.

	   02  SD-CHQ-BNAME.
	   03  LINE  7 COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC X(30)
				 FROM W15-BANK-NAME.

	   02  SA-CHQ-NAR-1.
	   03  LINE  8 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC X(40)
				 USING W15-NARRATIVE-1 AUTO.

	   02  SA-CHQ-GLAC.
	   03  LINE  9 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC 9(06)
				 USING W15-GL-AC AUTO.

	   02  SD-CHQ-GNAME.
	   03  LINE  9 COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC X(30)
				 FROM W15-GLAC-NAME.

	   02  SA-CHQ-NAR-2.
	   03  LINE 10 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC X(40)
				 USING W15-NARRATIVE-2 AUTO.

	   02  SA-CHQ-VAL.
	   03  LINE 11 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC ZZZ,ZZZ,ZZ9.99
				 USING W15-VALUE AUTO.

004870 COPY S99.CRT.

       COPY LEDLUP.CRT.

       01  S-LEDG.
           03  LINE 23 COLUMN  2 VALUE "Balance :".
	   03	       COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z(08)9.99CR USING G-BAL.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
005310 AA000	       SECTION.
005320 AA00.
003240	   IF LS0-GLLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA11E.
005330	     PERFORM ZA000.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM HA000.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA11E.
             EXIT PROGRAM.

       AA20-LOOKUP     SECTION.
       AA20.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM GACCNT-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.
005910 AA50.
      *
      *    ****    Print the PD Cheques posted audit trail HEADING
      *
	     MOVE "H"		 TO W02-PRN-TYPE.
007350	     MOVE SPACES	 TO R-DET.
007360	     MOVE WS-H1 	 TO RG-H1.
007370	     MOVE W90-CNAME	 TO RG-H2.
007380	     MOVE WS-H3 	 TO RG-H3.
007390	     MOVE WS-H4 	 TO RG-H4.
007400	     ADD 1		 TO WS-PAGE.
007410	     MOVE WS-PAGE	 TO RG-PGE.
007420	     MOVE WS-BATCH	 TO RG-BTCH.
007430	     MOVE W05-TODAY	 TO RG-DTE1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007450	     MOVE SPACES	 TO R-DET.
007460	     MOVE W50-H4	 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007480	     MOVE SPACES	 TO R-DET.

006080 AA65.
006090       EXIT.
      *
      *    ****    Print the G/Ledger PD Cheques HEADING
      *
       AA50GP.
005920	     MOVE SPACES	 TO R-DET.
005930	     MOVE WS-H1 	 TO R-H1.
005940	     MOVE W90-CNAME	 TO R-H2.
005950	     MOVE WS-H4 	 TO R-H3.
005960	     ADD 1		 TO WS-PAGE.
005970	     MOVE WS-PAGE	 TO R-PGE.
005980	     MOVE W05-TODAY	 TO R-DTE1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
006000	     MOVE SPACES	 TO R-DET.
007460	     MOVE W50-H4	 TO R-HEAD.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007480	     MOVE SPACES	 TO R-DET.
	     MOVE "DATE"	 TO PG-H1.
	     MOVE "CHQ.NO."	 TO PG-H2.
	     MOVE "DEBIT"	 TO PG-H3.
	     MOVE "CREDIT"	 TO PG-H4.
	     MOVE "G/L ACT"	 TO PG-H5.
	     MOVE "ACCOUNT NAME" TO PG-H6.
	     MOVE "PAYEE/NARRATIVE"
				 TO PG-H7.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

       AA55GP.
	     EXIT.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
006190 AB000           SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD2.

       COPY GLTRAN.RD.

       COPY GLJRN.RD.

       COPY GPDCHQ.RD.

       COPY SHARED.RD.

007740 AC000	       SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

       COPY GLJRN.WR.

       COPY GPDCHQ.WR.

      /
      *    THIS ROUTINE UPDATES THE VARIOUS MONTHS FROM TRANSACTION
      *    DATE UP TO THE CURRENT MONTH
      *
       AH000           SECTION.
       AH00.
           IF WS-PERIOD = W05-MM
               GO TO AH999.
           IF WS-PERIOD > 12
               SUBTRACT 13 FROM WS-PERIOD GIVING WS-S3
               PERFORM AH10
               ADD 1 WS-OPEN GIVING WS-MS1
           ELSE
               MOVE WS-PERIOD TO WS-MS1.
       AH05.
             ADD WS-VALUE        TO G-VAL (WS-MS1).
             ADD 1               TO WS-MS1.
           IF WS-MS1 > 12
               MOVE 1            TO WS-MS1.
           IF WS-MS1 = W05-MM
               GO TO AH999.
             GO TO AH05.
       AH10.
             ADD 1               TO WS-S3.
           IF WS-S3 > 12
               MOVE 1            TO WS-S3.
             ADD WS-VALUE        TO G-LVAL (WS-S3).
           IF WS-S3 NOT = WS-OPEN
               GO TO AH10.
	     ADD WS-VALUE	 TO G-OBAL.
       AH15.
             EXIT.
       AH999.
             EXIT.

       COPY GACCNT.LUP.

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
009530 AY000	       SECTION.

009540 AY01.
009550       MOVE 01             TO REC-FILE.
009560       MOVE WS-PARKEY      TO REC-KEY.
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
009570       MOVE PAR-RECORD1    TO REC-PARAM.
009580       GO TO AY50.

009690 AY12.
009700       MOVE 12             TO REC-FILE.
009710       MOVE WS-GLKEY       TO REC-KEY.
             PERFORM READ-GACCNT-LOCK THRU READ-GACCNT-EXIT.
009720       MOVE G-REC          TO REC-GACCNT.
009730       GO TO AY50.

009740 AY13.
009750       MOVE 13             TO REC-FILE.
009760       MOVE WS-GTRKEY      TO REC-KEY.
009770       MOVE T-REC          TO REC-GLTRAN.
009780       GO TO AY50.

012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.

009790 AY45.
011080	     MOVE 45		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE PDT-REC	 TO REC-GPDCHQ.
011110       GO TO AY50.

012040 AY49.
009800       MOVE 99 TO REC-FILE.
009810       MOVE ZERO TO REC-KEY.
009820       MOVE SPACES TO REC-DETAIL.
009830       PERFORM AY50.
009840       ADD 1 TO WS-TRANS.
009850     IF WS-RECOVER > 95
009860         CLOSE RECOVER
009870         OPEN OUTPUT RECOVER
009880         CLOSE RECOVER
009890         OPEN I-O RECOVER
009900         MOVE ZERO TO WS-RECOVER.
009910       GO TO AY59.

009920 AY50.
009930       ADD 1 TO WS-RECOVER.
009940       MOVE WS-RECOVER TO WS-RECKEY.
009950       MOVE WS-TRANS TO REC-TRAN.
009960       WRITE REC-RECORD.
009970     IF WS-STATUS NOT = "00"
009980         DISPLAY (22, 12) "Write error Recovery file - Status "
009990                  WITH FOREGROUND-COLOR 14 
010000                  WS-STATUS WITH FOREGROUND-COLOR 15
010010         STOP RUN.

010040 AY59.
010050       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *	     MOVE SHR-STOCK	 TO WS-STOCK.
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
012620	     MOVE "GL"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the LEDGER control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010       MOVE 4              TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
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
012760		   IF PAR-PROG(WS-SUB) = "CG" OR "GL"
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

011260 AY65.
011270       MOVE 4              TO WS-NETKEY.
011300       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
011310       MOVE G-BATCH        TO WS-BATCH.
011320     IF G-BATCH = 99999
011330         MOVE 1            TO G-BATCH
011340     ELSE
011350         ADD 1             TO G-BATCH.
011360       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
011380       GO TO AY999.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      * 	   NETWORK record 2.
      *
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY49 THRU AY59.
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

011860 AY999.
011870       EXIT.
      /  
010710 BA000         SECTION.
010720 BA00.
010730       MOVE ZERO TO WS-ERROR.
010740     IF W15-DD < 01 OR > 31
010750	       GO TO BA10.
010760     IF W15-MM < 01 OR > 12
010770	       GO TO BA10.
010780     IF W15-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
010790	       GO TO BA15.
010800     IF W15-MM = 02 AND W15-DD > 28
010810         GO TO BA05.
010820     IF W15-DD > 30
010830	       GO TO BA10.
010840	     GO TO BA15.
010850 BA05.
010860     IF W15-DD > 29
010870	       GO TO BA10.
010880       DIVIDE W15-YY BY 4 GIVING W20-RESULT.
010890     IF W20-DEC > ZERO
010900	       GO TO BA10.
010910	     GO TO BA15.
010920 BA10.
010930       MOVE 1 TO WS-ERROR.
	     GO TO BA999.
      *
      *   ****    CALCULATE THE PERIOD INTO WHICH THIS DATE FALLS
      * 
       BA15.
             MOVE W15-MM TO WS-PERIOD.
	   IF W15-YY > W05-YY
	       GO TO BA10.
	   IF W15-YY < W05-YY
	       GO TO BA20.
	   IF W05-MM = W15-MM
	      GO TO BA999.
	   IF W15-MM > W05-MM
	       GO TO BA10.
	   IF W15-MM > WS-Y-END
	       GO TO BA999.
           IF W15-YY > WS-L-END
	       GO TO BA999.
             ADD 12 TO WS-PERIOD
	     GO TO BA999.
       BA20.
           IF W15-YY > WS-L-END
	       GO TO BA999.
             ADD 12 TO WS-PERIOD.
010940 BA999.
010950       EXIT.
      /
      *    ****    P O S T   D A T E D	 C H E Q U E S
      *
       HA000	       SECTION 5.
       HA00.
005370       DISPLAY CLR-SCREEN.
006620 HA01.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
006630       DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 21		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       HA02.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 10		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 19		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       HA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 HA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO HA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO HA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO HA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO HA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO HA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO HA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO HA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO HA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO HA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 4
		   CALL X"E5"
		   GO TO HA05
	       END-IF
	       GO TO HA16
	   ELSE
	       CALL X"E5"
	       GO TO HA05.

       HA15.
	   IF SCREEN-LIN < 5
	       CALL X"E5"
	       GO TO HA05
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO HA18.

       HA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 9		 TO SCREEN-LIN
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
	       MOVE 8		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       HA18.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
006730	   IF WS-OPTION = "0"
006740	       GO TO HA99.
006750     IF WS-OPTION = "1"
006760	       PERFORM HB000
006770     ELSE
006780     IF WS-OPTION = "2"
006790	       PERFORM HC000
006800     ELSE
006810     IF WS-OPTION = "3"
006820	       PERFORM HD000
006830     ELSE
006840     IF WS-OPTION = "4"
006850	       PERFORM HE000.
007060	     GO TO HA01.

       HA100	       SECTION 5.
       HA102.
010730	     MOVE ZERO TO WS-ERROR.
010740     IF W15-DD < 01 OR > 31
010750	       GO TO HA110.
010760     IF W15-MM < 01 OR > 12
010770	       GO TO HA110.
010780     IF W15-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
010790	       GO TO HA999.
010800     IF W15-MM = 02 AND W15-DD > 28
010810	       GO TO HA105.
010820     IF W15-DD > 30
010830	       GO TO HA110.
010840	     GO TO HA999.

010850 HA105.
010860     IF W15-DD > 29
010870	       GO TO HA110.
010880       DIVIDE W15-YY BY 4 GIVING W20-RESULT.
010890     IF W20-DEC > ZERO
010900	       GO TO HA110.
010910	     GO TO HA999.

010920 HA110.
010930       MOVE 1 TO WS-ERROR.

010940 HA999.
010950       EXIT.
      /
      *    ****    C A P T U R E   P O S T   D A T E D	 C H E Q U E S
      *
       HB000	       SECTION 50.
       HB00.
019570	     MOVE "PD. CHEQUES ISSUED (GENERAL LEDGER)"
                                 TO W50-HEAD.
019580       MOVE WX-BNKGL       TO W15-BANK.
019620	     MOVE ZERO		 TO W20-TOTAL W20-DEBIT
				    WS-PAGE.

       HB05.
026330       DISPLAY CLR-SCREEN.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 16		 TO SHADE-COL.
	     MOVE 50		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S-CHEQUE.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY "POST DATED CHEQUES (G/L)" AT 0229
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE SPACES	 TO W15-NARRATIVE-1 W15-PAYEE
				    W15-NARRATIVE-2 W15-CRED
				    W15-BANK-NAME   W15-GLAC-NAME
				    W15-CNAME.
	     MOVE ZERO		 TO W15-VALUE W15-CHQ-NO
				    W15-GL-AC.
	     MOVE SPACE		 TO WS-CRD-CHQ.
013720       MOVE W70-BNKGL      TO W15-BANK.
      *
      *      ****    ***   *****  *****
      *      *	 *  *	*    *	  *
      *      *	 *  *****    *	  ***
      *      *	 *  *	*    *	  *
      *      ****   *	*    *	  *****
      *
       HB10.
	     ACCEPT SA-CHQ-DTE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      GO TO HB990
		 WHEN OTHER	 CALL X"E5"
		      GO TO HB10
	       END-EVALUATE.
027380	     MOVE W15-DATE	 TO W10-EDTE.
027390       PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Date"	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO HB10.
      *
      *    ****    I N C L U D E   C E N T U R Y   I N	 D A T E
      *
	     MOVE W22-DTE1	 TO W22-YMD
				    W22-DTE2.
	   IF W22-YY2 > 55
      *
      *    ****   ASSUME 1900
      *
	       MOVE 19		 TO W22-C
	   ELSE
      *
      *    ****   ASSUME 2000
      *
	       MOVE 20		 TO W22-C.
      *
      *    ****    C H E C K   I F   T H E   C H E Q U E   I S
      * 	   P O S T   D A T E D
      *
	   IF NOT (W22-CYMD > W12-T-CYMD)
	       MOVE "Cheque is not Post-Dated"
				 TO WS-ERR-STRING
	       PERFORM ERROR-SETUP THRU ERROR-POS
	       MOVE 15		 TO SLIN
003260	       PERFORM ERROR-DISPLAY THRU ERROR-EXIT
	       GO TO HB10.
      *
      *     ***   *   *  *****	 ***   *   *  *****    *   *   ***
      *    *   *  *   *  *	*   *  *   *  *        **  *  *   *
      *    *	  *****  ***	*   *  *   *  ***      * * *  *   *
      *    *   *  *   *  *	*  **  *   *  *        *  **  *   *
      *     ***   *   *  *****	 ****	***   *****    *   *   ***
      *
       HB15.
	     ACCEPT SA-CHQ-NO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      GO TO HB10
		 WHEN OTHER	 CALL X"E5"
		      GO TO HB15
	       END-EVALUATE.
	   IF W15-CHQ-NO = ZERO
	       MOVE "Cheque is not Post-Dated"
				 TO WS-ERR-STRING
	       PERFORM ERROR-SETUP THRU ERROR-POS
	       MOVE 15		 TO SLIN
003260	       PERFORM ERROR-DISPLAY THRU ERROR-EXIT
	       GO TO HB15.
      *
      *    ****    ***	 *   *	*****  *****
      *    *   *  *   *  *   *	*      *
      *    ****   *****   * *	***    ***
      *    *	  *   *    *	*      *
      *    *	  *   *    *	*****  *****
      *
       HB20.
	     ACCEPT SA-CHQ-PAY.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      GO TO HB15
		 WHEN OTHER	 CALL X"E5"
		      GO TO HB20
	       END-EVALUATE.
	   IF W15-PAYEE = SPACES
	       MOVE "Payee may not be left BLANK"
				 TO WS-ERR-STRING
	       PERFORM ERROR-SETUP THRU ERROR-POS
	       MOVE 15		 TO SLIN
003260	       PERFORM ERROR-DISPLAY THRU ERROR-EXIT
	       GO TO HB20.
	   IF NOT (WS-BATCH = "Y")
	       DISPLAY WS-NOT-APP AT 0724
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    HBCKGROUND-COLOR 5
	       DISPLAY WS-NOT-APP AT 0824
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    HBCKGROUND-COLOR 5
	       DISPLAY WS-NOT-APP AT 0924
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    HBCKGROUND-COLOR 5
	       DISPLAY WS-NOT-APP AT 1024
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    HBCKGROUND-COLOR 5
	       GO TO HB55.
	   IF NOT (W15-BANK = ZERO)
	       MOVE W15-BANK	 TO G-AC
008980	       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
	       IF NOT (WS-F-ERROR = 8)
		   MOVE G-NAME	 TO W15-BANK-NAME
		   DISPLAY SD-CHQ-BNAME.
      *
      *    *	   ***	  ***	*   *  *   *  ****
      *    *	  *   *  *   *	*  *   *   *  *   *
      *    *	  *   *  *   *	***    *   *  ****
      *    *	  *   *  *   *	*  *   *   *  *
      *    *****   ***	  ***	*   *	***   *
      *
       HB25.
	     DISPLAY "F1" AT 2502
		      WITH HBCKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH HBCKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH HBCKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup - "
		      WITH HBCKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH HBCKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to exit"
		      WITH HBCKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      *
      *    ****    ***	 *   *	*   *
      *    *   *  *   *  **  *	*  *
      *    ****   *****  * * *	***
      *    *   *  *   *  *  **	*  *
      *    ****   *   *  *   *	*   *
      *
       HB30.
	     ACCEPT SA-CHQ-BANK.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      DISPLAY CLEAR-L25
		      GO TO HB20
		 WHEN F1-KEY
		      PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		      MOVE ZERO	 TO W15-ACCOUNT
		      PERFORM AA350
		 WHEN OTHER
		      CALL X"E5"
		      GO TO HB30
	       END-EVALUATE
               IF W15-ACCOUNT = ZERO
015990		   GO TO HB30
	       END-IF
	       MOVE W15-ACCOUNT  TO W15-BANK
	       DISPLAY SA-CHQ-BANK.
	     DISPLAY CLEAR-L25.
013810	   IF W15-BANK = ZERO
013820	       GO TO HB20.
	     MOVE W15-BANK	 TO W15-ACCOUNT
013830     IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013850	       GO TO HB30.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
013900	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013910	       GO TO HB25.
	     MOVE G-NAME	 TO W15-BANK-NAME.
	     DISPLAY SD-CHQ-BNAME.
      *
      *    *   *   ***	 ****	****	***   *****  ***  *   *  *****
      *    **  *  *   *  *   *	*   *  *   *	*     *   *   *  *
      *    * * *  *****  ****	****   *****	*     *   *   *  ***
      *    *  **  *   *  *  *	*  *   *   *	*     *    * *	 *
      *    *   *  *   *  *   *	*   *  *   *	*    ***    *	 *****
      *
       HB35.
	     ACCEPT SA-CHQ-NAR-1.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      GO TO HB25
		 WHEN OTHER	 CALL X"E5"
		      GO TO HB35
	       END-EVALUATE.
	   IF W15-NARRATIVE-1 = SPACES
	       MOVE "Narrative may not be left BLANK"
				 TO WS-ERR-STRING
	       PERFORM ERROR-SETUP THRU ERROR-POS
	       MOVE 15		 TO SLIN
003260	       PERFORM ERROR-DISPLAY THRU ERROR-EXIT
	       GO TO HB35.
	   IF NOT (W15-GL-AC = ZERO)
	       MOVE W15-GL-AC	 TO G-AC
008980	       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
	       IF NOT (WS-F-ERROR = 8)
		   MOVE G-NAME	 TO W15-GLAC-NAME
		   DISPLAY SD-CHQ-GNAME.

       HB40.
	     PERFORM HB25.
      *
      *     ***       *  *	      ***    ***    ***   *****
      *    *   *     *	 *	     *	 *  *	*  *   *    *
      *    *	    *	 *	***  *****  *	   *	    *
      *    *  **   *	 *	     *	 *  *	*  *   *    *
      *     ***   *	 *****	     *	 *   ***    ***     *
      *
       HB45.
	     ACCEPT SA-CHQ-GLAC.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      DISPLAY CLEAR-L25
		      GO TO HB35
		 WHEN F1-KEY
		      PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		      MOVE ZERO	 TO W15-ACCOUNT
		      PERFORM AA350
		 WHEN OTHER
		      CALL X"E5"
		      GO TO HB45
	       END-EVALUATE
               IF W15-ACCOUNT = ZERO
015990		   GO TO HB45
	       END-IF
	       MOVE W15-ACCOUNT  TO W15-GL-AC
	       DISPLAY SA-CHQ-GLAC.
	     DISPLAY CLEAR-L25.
013810	   IF W15-GL-AC = ZERO
013820	       GO TO HB35.
	      MOVE W15-GL-AC	 TO W15-ACCOUNT
013830     IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013850	       GO TO HB40.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
013900	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013910	       GO TO HB45.
	     MOVE G-NAME	 TO W15-GLAC-NAME.
	     DISPLAY SD-CHQ-GNAME.
      *
      *    *   *   ***	 ****	****	***   *****  ***  *   *  *****
      *    **  *  *   *  *   *	*   *  *   *	*     *   *   *  *
      *    * * *  *****  ****	****   *****	*     *   *   *  ***
      *    *  **  *   *  *  *	*  *   *   *	*     *    * *	 *
      *    *   *  *   *  *   *	*   *  *   *	*    ***    *	 *****
      *
       HB50.
	   IF W15-NARRATIVE-2 = SPACES
	       MOVE W15-NARRATIVE-1
				 TO W15-NARRATIVE-2.
	     ACCEPT SA-CHQ-NAR-2.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      GO TO HB40
		 WHEN OTHER	 CALL X"E5"
		      GO TO HB50
	       END-EVALUATE.
	   IF W15-NARRATIVE-2 = SPACES
	       MOVE "Narrative may not be left BLANK"
				 TO WS-ERR-STRING
	       PERFORM ERROR-SETUP THRU ERROR-POS
	       MOVE 15		 TO SLIN
003260	       PERFORM ERROR-DISPLAY THRU ERROR-EXIT
	       GO TO HB50.
      *
      *    *   *   ***	 *	*   *  *****
      *    *   *  *   *  *	*   *  *
      *    *   *  *****  *	*   *  ***
      *     * *   *   *  *	*   *  *
      *      *	  *   *  *****	 ***   *****
      *
       HB55.
	     ACCEPT SA-CHQ-VAL.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      IF NOT (WS-BATCH = "Y")
			  GO TO HB20
		      END-IF
		      GO TO HB50
		 WHEN OTHER	 CALL X"E5"
		      GO TO HB55
	       END-EVALUATE.
	   IF W15-VALUE < 2.00
	       MOVE "Cheque amount LESS than R2.00"
				 TO WS-ERR-STRING
	       PERFORM ERROR-SETUP THRU ERROR-POS
	       MOVE 15		 TO SLIN
003260	       PERFORM ERROR-DISPLAY THRU ERROR-EXIT
	       GO TO HB55.
	     MOVE 14		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
020250	   IF WS-OPTION = "N"
	       GO TO HB10.
	   IF WS-PAGE = ZERO
	       PERFORM AA50GP.
      *
      *    ****    I N C L U D E   C E N T U R Y   I N	 D A T E
      *
	     MOVE W22-DTE1	 TO W22-YMD
				    W22-DTE2.
	   IF W22-YY2 > 55
      *
      *    ****   ASSUME 1900
      *
	       MOVE 19		 TO W22-C
	   ELSE
      *
      *    ****   ASSUME 2000
      *
	       MOVE 20		 TO W22-C.
      *
      *    ****    S W I T C H	 D A T E   T O	 DDMMCCYY
      *
	     MOVE W22-CY	 TO W22-CCYY
	     MOVE W22-M 	 TO W22-MM
	     MOVE W22-D 	 TO W22-DD
	     MOVE W22-DMCY	 TO W02-PRN-DATE.
	     MOVE W15-PAYEE	 TO W02-PRN-PAY.
	     MOVE W15-VALUE	 TO W02-PRN-VAL.
	   IF WS-PAGE = ZERO
	       PERFORM AA50GP.
      *
      *    ****    A D D   C H E Q U E	 T O   P . D .	 C H E Q U E
      * 	   F I L E
      *
	     INITIALIZE GPD-REC.
	     MOVE W22-YMD	 TO GPD-DATE.
	     MOVE W22-DMCY	 TO PG-DATE.
	     MOVE W15-REFNO	 TO PG-REF GPD-REFER.
	     MOVE W15-BANK	 TO PG-GL-AC GPD-BANK.
	     MOVE W15-BNKNME	 TO PG-AC-NAME.
	     MOVE W15-CRED	 TO GPD-GL.
	     MOVE W15-NAR	 TO GPD-NAR1.
	     MOVE W15-NAR2	 TO GPD-NAR2.
	     MOVE W15-TCOST	 TO PG-D-VALUE GPD-VAL.
019960	     MOVE W15-PAYEE	 TO PG-PAYEE GPD-PAYEE.
	     ADD W15-TCOST	 TO W20-TOTAL.
	     PERFORM WRITE-GPDCHQ THRU WRITE-GPDCHQ-EXIT.
	   IF WS-F-ERROR = 45
	       MOVE SPACES	 TO WS-ERR-STRING
	       STRING "Cheque duplicated: "
			    DELIMITED SIZE
		       W15-REFNO DELIMITED SIZE
				 INTO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA35.
	     PERFORM AY60 THRU AY999.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY45 THRU AY50.
	     PERFORM AY70 THRU AY999.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	   IF W15-NAR  = W15-NAR2
	       GO TO HB60.
	     MOVE W15-NAR	 TO PG-PAYEE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.

       HB60.
	     MOVE W15-CRED	 TO PG-GL-AC.
	     MOVE W15-BNKNME	 TO PG-AC-NAME.
	     MOVE W15-NAR2	 TO PG-PAYEE.
	     COMPUTE PG-C-VALUE = W15-TCOST * -1.
	     PERFORM DELETE-CHQBATCH-REC THRU WRITE-CHQBATCH-EXIT.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 5)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM HA50.
	     MOVE SPACES	 TO R-DET.

       HB65.
	     MOVE "'C'ontinue or 'E'xit  [ ]"
				 TO WS-ERR-MES.
	     MOVE 15		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "C" OR "E")
	       GO TO HB65.
	   IF WS-OPTION = "E"
	       GO TO HB990.
	     GO TO HB05.

       HB990.
	   IF WS-PAGE = ZERO
	      GO TO HB999.
	     MOVE SPACES	 TO R-DET.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
023620	     MOVE "****  E N D   O F   R E P O R T  ****"
				 TO PD-NAR.
016780	     MOVE W20-TOTAL	 TO PG-D-VALUE.
016790	     MULTIPLY -1 BY W20-TOTAL
				 GIVING PG-C-VALUE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

       HB999.
	     EXIT.

      /
       HC000	       SECTION 51.
       HC00.
019570	     MOVE "PD. CHEQUES AMENDED (GENERAL LEDGER)"
                                 TO W50-HEAD.
019620	     MOVE ZERO		 TO W20-TOTAL W20-DEBIT
				    WS-PAGE.

       HC05.
026330	     DISPLAY CLR-SCREEN.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 16		 TO SHADE-COL.
	     MOVE 50		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S-CHEQUE.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY "AMEND POST DATED CHEQUES (G/L)" AT 0226
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE SPACES	 TO W15-NARRATIVE-1 W15-PAYEE
				    W15-NARRATIVE-2 W15-CRED
				    W15-BANK-NAME   W15-GLAC-NAME
				    W15-CNAME.
	     MOVE ZERO		 TO W15-VALUE W15-CHQ-NO
				    W15-GL-AC.
	     MOVE SPACE		 TO WS-CRD-CHQ.

       HC10.
	     DISPLAY "Key in the cheque number" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
020210	     ACCEPT S14 AT 0619.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO HC70
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HC10
	       END-EVALUATE.
019910	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	     DISPLAY CLEAR-L25.
020220	     MOVE W15-GRNN	 TO W15-REFNO PDT-REFER.
020230     IF W15-GRNN = ZERO
020240	       GO TO HC70.
	     PERFORM START-AT-GPDCHQ-REF THRU READ-GPDCHQ-EXIT.
	   IF WS-F-ERROR = ZERO
	       GO TO HC20.
       HC15.
	     DISPLAY "Not on file" AT 2302
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     GO TO HC10.
       HC20.
	     PERFORM READ-GPDCHQ-NEXT THRU READ-GPDCHQ-EXIT.
	   IF (WS-F-ERROR = 44) OR
	      (W15-GRNN < PDT-REFER)
	       GO TO HC15.
	     MOVE PDT-DATE	 TO W21-DTE.
	     MOVE W21-YY	 TO W15-YY.
	     MOVE W21-MM	 TO W15-MM.
	     MOVE W21-DD	 TO W15-DD.
	     MOVE W15-DATE	 TO W15-GRND.
	     MOVE PDT-AC	 TO W15-ACNO.
	     MOVE PDT-REFER	 TO W15-GRNN.
	     MOVE PDT-BANK	 TO W15-BANK.
	     MOVE PDT-NAR	 TO W15-NAR.
	     MOVE PDT-CRED	 TO W15-CRED.
	     MOVE PDT-VAL	 TO W15-TCOST.

019920	     PERFORM AE155-GET-CREDITOR.

022060	     MOVE W15-CRED	 TO W15-ACCOUNT.
022120       PERFORM AD000.
	     MOVE G-NAME	 TO W15-ACNME.
022060	     MOVE W15-BANK	 TO W15-ACCOUNT.
022120       PERFORM AD000.
	     MOVE G-NAME	 TO W15-BNKNME.
	     DISPLAY S21.
	     PERFORM CHECK-CORRECT.
020110	   IF WS-OPTION = "N"
	       GO TO HC05.
020140 HC22.
020150	     ACCEPT S15 AT 0519.
019910	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO HC05
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HC22
	       END-EVALUATE.
020160       MOVE W15-GRND TO W15-DATE.
020170	     PERFORM HA100.
020180     IF WS-ERROR = 1
	       DISPLAY WS-ER3 AT 2302
		       WITH BACKGROUND-COLOR 3
			    FOREGROUND-COLOR 6 HIGHLIGHT
020190	       GO TO HC22.
             MOVE W15-DD         TO W20-DD.
             MOVE W15-MM         TO W20-MM.
             MOVE W15-YY         TO W20-YY.
	   IF W20-DTE NOT > W05-TYMD
	       DISPLAY "Not a post dated cheque" AT 2302
		       WITH BACKGROUND-COLOR 3
			    FOREGROUND-COLOR 6 HIGHLIGHT
		       " Correct date or press "
		       WITH BACKGROUND-COLOR 3
		       "Esc" WITH BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 6 HIGHLIGHT
		       "ape" WITH BACKGROUND-COLOR 3
020190	       GO TO HC22.
020200 HC25.
020210	     ACCEPT S14 AT 0619.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO HC22
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HC25
	       END-EVALUATE.
020220	     MOVE W15-GRNN	 TO W15-REFNO.
020230     IF W15-GRNN = ZERO
020240	       GO TO HC25.
020250 HC30.
020260	     ACCEPT S16 AT 0719.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO HC25
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HC30
	       END-EVALUATE.
	   IF W15-COST NOT < ZERO
020270	       MULTIPLY -1 BY W15-TCOST
                                 GIVING W15-CHQVAL.
020280	     DISPLAY S17 AT 0719.
020290     IF W15-TCOST = ZERO
020300	       GO TO HC30.
021740 HC35.
021750       MOVE W15-BANK       TO W15-ACCOUNT.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
021760	     ACCEPT S09 AT 0919.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO HC30
		 WHEN F1-KEY
		     PERFORM AA20G THRU AA20
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HC35
	       END-EVALUATE
	       DISPLAY S09 AT 0919
               IF W15-ACCOUNT = ZERO
015990		   GO TO HC35.
	     DISPLAY CLEAR-L25.
021770	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
021780     IF W15-ASER = ZERO
021790	       DISPLAY WS-ER7 AT 2324
		       WITH FOREGROUND-COLOR 6 HIGHLIGHT
021800	       GO TO HC35.
021810       MOVE W15-ACCOUNT    TO W15-BANK.
021820 HC40.
021830       PERFORM AD000.
021840     IF WS-ERROR = 1
021850	       DISPLAY WS-ER6 AT 2324
		       WITH FOREGROUND-COLOR 6 HIGHLIGHT
021860	       GO TO HC35.
021870	     DISPLAY G-NAME AT 0926
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
021880 HC45.
	     PERFORM CHECK-CORRECT.
021990	   IF WS-OPTION = "N"
022000	       GO TO HC35.
022010 HC55.
022020	     ACCEPT W15-NAR AT 1019
022030		    WITH FOREGROUND-COLOR 3 HIGHLIGHT
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO HC35
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HC55
	       END-EVALUATE.
022040	   IF W15-NAR = SPACES
022050	       GO TO HC55.
022060	     MOVE W15-CRED	 TO W15-ACCOUNT.
022070 HC60.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
022080	     ACCEPT S09 AT 1119.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO HC55
		 WHEN F1-KEY
		     PERFORM AA20G THRU AA20
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HC60
	       END-EVALUATE
	       DISPLAY S09 AT 1119
               IF W15-ACCOUNT = ZERO
015990		   GO TO HC60.
	     DISPLAY CLEAR-L25.
022090	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
022100     IF W15-ASER = ZERO
022110	       GO TO HC60.
021810	     MOVE W15-ACCOUNT	 TO W15-CRED.
022120       PERFORM AD000.
022130     IF WS-ERROR = 1
022140	       DISPLAY WS-ER6 AT 2324 WITH FOREGROUND-COLOR 6 HIGHLIGHT
022150	       GO TO HC60.
022160	     DISPLAY G-NAME AT 1126 WITH FOREGROUND-COLOR 6 HIGHLIGHT.
022170 HC65.
	     PERFORM CHECK-CORRECT.
022260	   IF WS-OPTION = "N"
022270	       GO TO HC60.
	     PERFORM AY60 THRU AY999.
	   IF (W20-DTE NOT = PDT-DATE) OR
	      (W15-ACNO NOT = PDT-AC) OR
	      (W15-REFNO NOT = PDT-REFER)
	       PERFORM DELETE-GPDCHQ-REC THRU WRITE-GPDCHQ-EXIT
	       INITIALIZE PDT-REC
	       MOVE "Y"		 TO WS-DEL
	    ELSE
	       MOVE "N" 	 TO WS-DEL.
	     MOVE W15-ACNO	 TO PD-ACNO PDT-AC.
	     MOVE P-NAME	 TO PD-NAME.
	     MOVE W20-DTE	 TO PDT-DATE.
	     MOVE W15-GRND	 TO PD-DATE.
	     MOVE W15-REFNO	 TO PD-REF PDT-REFER.
	     MOVE W15-BANK	 TO PD-BANK PDT-BANK.
	     MOVE W15-CRED	 TO PD-CRED PDT-CRED.
	     MOVE W15-NAR	 TO PD-NAR PDT-NAR.
	     MOVE W15-CHQVAL	 TO PD-VALUE PDT-VAL.
	     ADD W15-CHQVAL	 TO W20-TOTAL.
	   IF LINAGE-COUNTER < WS-PRN-LENGTH
	       WRITE R-L1 BEFORE 1 LINES
	   ELSE
	       WRITE R-L1 BEFORE PAGE
	       PERFORM HA50.
	     MOVE SPACES	 TO R-DET.
	   IF WS-DEL = "Y"
	       PERFORM WRITE-GPDCHQ THRU WRITE-GPDCHQ-EXIT
	       MOVE 1		 TO WS-ACTION
	   ELSE
	       PERFORM REWRITE-GPDCHQ THRU WRITE-GPDCHQ-EXIT
	       MOVE 0		 TO WS-ACTION.
	     PERFORM AY45 THRU AY50.
	     PERFORM AY70 THRU AY999.
       HC70.
023500	     DISPLAY CLEAR-L25.
023510       DISPLAY "ENTER to continue - E to end" AT 2512
023520		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
023530	     ACCEPT WS-OPTION AT 2541
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
023590	     DISPLAY CLEAR-L25.
023600     IF NOT (WS-OPTION = "E")
023610	       GO TO HC05.
	     MOVE SPACES	 TO R-DET.
023650       WRITE R-L1 BEFORE 1 LINES.
023620	     MOVE "****  TOTAL ****"
				 TO PD-NAR.
023630	     MOVE W20-TOTAL	 TO PD-VALUE.
023650       WRITE R-L1 BEFORE 1 LINES.
023660	     MOVE WS-NORM	 TO R-DET.
023670       WRITE R-L1 BEFORE PAGE.
023680       MOVE SPACES         TO R-DET.
       HC999.
	     EXIT.
      /
       HD000	       SECTION 5.
       HD00.
019570	     MOVE "PD. CHEQUES DELETED"
                                 TO W50-HEAD.
019600	     MOVE WS-COND	 TO R-DET.
019610       WRITE R-L1 BEFORE 0 LINES.
019620       MOVE ZERO           TO W20-TOTAL W20-DEBIT.
       HD02.
	     PERFORM HA50.
       HD05.
019840	     DISPLAY CLR-SCREEN.
019850	     DISPLAY S20.
	     DISPLAY "DELETE PD CHEQUE" AT 0233
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
019860	     MOVE ZERO		 TO W15-GRND
				    W15-GRNN.
	     MOVE SPACES	 TO W15-ACNO.
       HD10.
	     DISPLAY "Key in the cheque number" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
020210	     ACCEPT S14 AT 0619.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO HD70
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HD10
	       END-EVALUATE.
019910	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	     DISPLAY CLEAR-L25.
020220	     MOVE W15-GRNN	 TO W15-REFNO PDT-REFER.
020230     IF W15-GRNN = ZERO
020240	       GO TO HD70.
	     PERFORM START-AT-GPDCHQ-REF THRU READ-GPDCHQ-EXIT.
	   IF WS-F-ERROR = ZERO
	       GO TO HD20.
       HD15.
	     DISPLAY "Not on file" AT 2302
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     GO TO HD10.
       HD20.
	     PERFORM READ-GPDCHQ-NEXT THRU READ-GPDCHQ-EXIT.
	   IF (WS-F-ERROR = 44) OR
	      (W15-GRNN < PDT-REFER)
	       GO TO HD15.
	     MOVE PDT-DATE	 TO W21-DTE.
	     MOVE W21-YY	 TO W15-YY.
	     MOVE W21-MM	 TO W15-MM.
	     MOVE W21-DD	 TO W15-DD.
	     MOVE W15-DATE	 TO W15-GRND.
	     MOVE PDT-AC	 TO W15-ACNO.
	     MOVE PDT-REFER	 TO W15-GRNN.
	     MOVE PDT-BANK	 TO W15-BANK.
	     MOVE PDT-NAR	 TO W15-NAR.
	     MOVE PDT-CRED	 TO W15-CRED.
	     MOVE PDT-VAL	 TO W15-TCOST.

019920	     PERFORM AE155-GET-CREDITOR.

022060	     MOVE W15-CRED	 TO W15-ACCOUNT.
022120       PERFORM AD000.
	     MOVE G-NAME	 TO W15-ACNME.
022060	     MOVE W15-BANK	 TO W15-ACCOUNT.
022120       PERFORM AD000.
	     MOVE G-NAME	 TO W15-BNKNME.
	     DISPLAY S21.
	     PERFORM CHECK-CORRECT.
020110	   IF WS-OPTION = "N"
	       GO TO HD05.
	     MOVE W15-ACNO	 TO PD-ACNO.
	     MOVE P-NAME	 TO PD-NAME.
	     MOVE W15-GRND	 TO PD-DATE.
	     MOVE W15-REFNO	 TO PD-REF.
	     MOVE W15-BANK	 TO PD-BANK.
	     MOVE W15-CRED	 TO PD-CRED.
	     MOVE W15-NAR	 TO PD-NAR.
	     MOVE W15-CHQVAL	 TO PD-VALUE.
	     ADD W15-CHQVAL	 TO W20-TOTAL.
	   IF LINAGE-COUNTER < WS-PRN-LENGTH
	       WRITE R-L1 BEFORE 1 LINES
	   ELSE
	       WRITE R-L1 BEFORE PAGE
	       PERFORM HA50.
	     MOVE SPACES	 TO R-DET.
	     PERFORM DELETE-GPDCHQ-REC THRU WRITE-GPDCHQ-EXIT.
       HD70.
023500	     DISPLAY CLEAR-L25.
023510       DISPLAY "ENTER to continue - E to end" AT 2512
023520		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
023530	     ACCEPT WS-OPTION AT 2541
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
023590	     DISPLAY CLEAR-L25.
023600     IF NOT (WS-OPTION = "E")
023610	       GO TO HD05.
	     MOVE SPACES	 TO R-DET.
023650       WRITE R-L1 BEFORE 1 LINES.
023620	     MOVE "****  TOTAL ****"
				 TO PD-NAR.
023630	     MOVE W20-TOTAL	 TO PD-VALUE.
023650       WRITE R-L1 BEFORE 1 LINES.
023660	     MOVE WS-NORM	 TO R-DET.
023670       WRITE R-L1 BEFORE PAGE.
023680       MOVE SPACES         TO R-DET.
       HD999.
	     EXIT.
      /
       HE000	      SECTION 5.
       HE0.
019570	     MOVE "PD. CHEQUES PROCESSED"
                                 TO W50-HEAD.
019600	     MOVE WS-COND	 TO R-DET.
019610       WRITE R-L1 BEFORE 0 LINES.
019620       MOVE ZERO           TO W20-TOTAL W20-DEBIT.
019630 HE00B.
019640       PERFORM AA50 THRU AA60.
019650       MOVE WS-HACNO       TO CH-H1.
019660       MOVE WS-HNAME       TO CH-H2.
019670       MOVE WS-HDTE        TO CH-H3.
019680       MOVE WS-HCHQ        TO CH-H4.
019690       MOVE "VALUE"        TO CH-H5.
019700       MOVE WS-HAGE        TO CH-H6.
019710       WRITE R-L1 BEFORE 2 LINES.
019720       MOVE SPACES         TO R-DET.
019730       MOVE WS-HACNO       TO R-H5.
019740       MOVE ALL "_"        TO R-H6 R-H8 R-H16 R-H18.
019750       MOVE "ACCOUNT NAME" TO R-H7.
019760       MOVE WS-HDTE        TO R-H9.
019770       MOVE "CHEQ NO"      TO R-H10.
019780       MOVE "PAYEE/NARRATIVE" 
                                 TO R-H17.
019790       MOVE "DEBIT"        TO R-H14.
019800       MOVE "CREDIT"       TO R-H15.
019810       WRITE R-L1 BEFORE 2 LINES.
019820       MOVE SPACES         TO R-DET.
019830 HE00.
019840       DISPLAY CLR-SCREEN.
019850	     DISPLAY S20.
	     DISPLAY "POST PD CHEQUES DUE" AT 0231
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY S22.
       HE02.
019620	     MOVE ZERO		 TO W20-TOTAL W20-DEBIT.
	     INITIALIZE PDT-REC.
	     PERFORM START-AT-GPDCHQ-KEY THRU READ-GPDCHQ-EXIT.
	   IF WS-F-ERROR = 44
	       GO TO HE145.
019870 HE05.
	     PERFORM READ-GPDCHQ-NEXT THRU READ-GPDCHQ-EXIT.
	   IF (WS-F-ERROR = 44) OR
	      (PDT-DATE > W05-TYMD)
	       GO TO HE135.
	     MOVE PDT-DATE	 TO W21-DTE.
	     MOVE W21-YY	 TO W15-YY W20-YY.
	     MOVE W21-MM	 TO W15-MM W20-MM.
	     MOVE W21-DD	 TO W15-DD W20-DD.
	     MOVE W15-DATE	 TO W15-GRND.
	     MOVE PDT-AC	 TO W15-ACNO.
	     MOVE PDT-REFER	 TO W15-GRNN W15-REFNO.
	     MOVE PDT-BANK	 TO W15-BANK.
	     MOVE PDT-NAR	 TO W15-NAR.
	     MOVE PDT-CRED	 TO W15-CRED.
	     MOVE PDT-VAL	 TO W15-CHQVAL.
020270	     MULTIPLY -1 BY PDT-VAL
				 GIVING W15-TCOST.

019920	     PERFORM AE155-GET-CREDITOR.

019960	     MOVE P-NAME	 TO W15-NAME W15-PAYEE.
022060	     MOVE W15-BANK	 TO W15-ACCOUNT.
022120       PERFORM AD000.
021890       MOVE WS-GLKEY       TO WS-KSTORE.
	     MOVE G-NAME	 TO W15-BNKNME.
022060	     MOVE W15-CRED	 TO W15-ACCOUNT.
022120       PERFORM AD000.
	     MOVE G-NAME	 TO W15-ACNME.
	     DISPLAY S21.
	     MOVE ZERO		 TO W15-SETT.
	   IF P-OUT NOT > ZERO
	       IF P-30 > ZERO
		   MOVE 1	 TO W15-30
	       ELSE
	       IF P-60 > ZERO
		   MOVE 1	 TO W15-60
	       ELSE
	       IF P-90 > ZERO
		   MOVE 1	 TO W15-90
	       ELSE
	       IF P-120 > ZERO
		   MOVE 1	 TO W15-120
	       ELSE
	       IF P-150 > ZERO
		   MOVE 1	 TO W15-150
	       ELSE
	       IF P-180 > ZERO
		   MOVE 1	 TO W15-180.
020320	     DISPLAY W15-30 AT 0819 WITH FOREGROUND-COLOR 3 HIGHLIGHT.
020330	     DISPLAY W15-60 AT 0826 WITH FOREGROUND-COLOR 3 HIGHLIGHT.
020340	     DISPLAY W15-90 AT 0833 WITH FOREGROUND-COLOR 3 HIGHLIGHT.
020350	     DISPLAY W15-120 AT 0841 WITH FOREGROUND-COLOR 3 HIGHLIGHT.
020370	     DISPLAY W15-150 AT 0849 WITH FOREGROUND-COLOR 3 HIGHLIGHT.
020390	     DISPLAY W15-180 AT 0856 WITH FOREGROUND-COLOR 3 HIGHLIGHT.
020500	     DISPLAY CLEAR-L25.
020510       MOVE 1              TO WS-S2.
020520       MOVE ZERO           TO WS-S.
020530 HE30.
020540       MOVE ZERO           TO W25-CFLOW (WS-S2).
020550     IF W15-MONTH (WS-S2) > ZERO
020560         ADD 1             TO WS-S.
020570     IF WS-S2 < 6
020580         ADD 1             TO WS-S2
020590	       GO TO HE30.
020600       MOVE ZERO           TO W25-V1 W25-V2.
020610       MOVE 1 TO WS-S3.
020620     IF WS-S < 2
020630         MOVE W15-CHQVAL   TO W25-V2
020640	       GO TO HE35.
020650       MOVE W15-TCOST      TO W20-RESULT.
020660       DIVIDE W20-RESULT BY WS-S 
                                 GIVING W25-V1 ROUNDED.
020670       SUBTRACT 1          FROM WS-S.
020680       COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
020690       ADD 1               TO WS-S.
020700       MULTIPLY -1         BY W25-V1.
020710       MULTIPLY -1         BY W25-V2.
020720 HE35.
012750	     DISPLAY "Processing payment" AT 2520 WITH
012760		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
020730     IF WS-S = ZERO
020740	       GO TO HE45.
020750 HE40.
020760     IF W15-MONTH (WS-S3) = ZERO
020770         ADD 1             TO WS-S3
020780	       GO TO HE40.
020790     IF WS-S > 1
020800         MOVE W25-V1       TO W25-CFLOW (WS-S3)
020810         SUBTRACT 1        FROM WS-S
020820         ADD 1             TO WS-S3
020830	       GO TO HE40.
020840       MOVE W25-V2         TO W25-CFLOW (WS-S3).
020850       MOVE ZERO           TO W25-V1 W25-V2.
	     DISPLAY "Creditor" AT 2540
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15.
020860 HE45.
020870       PERFORM AY60 THRU AY999.
020980 HE50.
021440	     MOVE W15-ACNO	 TO CH-ACNO.
021450       MOVE W15-NAME       TO CH-NAME.
021460       MOVE W15-GRNN       TO CH-CHQ.
021470       MOVE W15-GRND       TO CH-DATE.
021480       MOVE W15-CHQVAL     TO CH-VAL.
021610	   IF LINAGE-COUNTER < (WS-PRN-LENGTH - 1)
021620         WRITE R-L1 BEFORE 2 LINES
021630     ELSE
021640         WRITE R-L1 BEFORE PAGE
021650	       PERFORM HE00B.
021660       MOVE SPACES         TO R-DET.
021670       MOVE W15-TCOST      TO W15-VAL.
021680       ADD W15-TCOST       TO W20-TOTAL.
021690       ADD W15-TCOST       TO W20-DEBIT.
021740 HE75.
022250	     DISPLAY CLEAR-L25.
012750	     DISPLAY "Updating Creditor G/L" AT 2520 WITH
012760		      BACKGROUND-COLOR 3
		      FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
022290       PERFORM AY12 THRU AY59.
022310       INITIALIZE T-REC.
022460 HE105.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
012680       MOVE W15-YMD        TO T-DATE.
022490       MOVE W15-NAR        TO T-NARATIVE.
022500       MOVE W15-REFER      TO T-REFER.
022520       MOVE G-AC           TO T-AC.
022530       MOVE 03             TO T-CODE.
022540       ADD W15-VAL         TO G-DT.
022550       MOVE W15-VAL        TO T-VAL.
             MOVE W15-VAL        TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
022560       ADD W15-VAL         TO G-BAL.
022610       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015270       PERFORM AY13 THRU AY59.
022630       MOVE W15-PAYEE      TO T-NARATIVE.
015510       MOVE 5              TO T-SUB.
             ADD 1               TO T-SEQ.
015530       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015270       PERFORM AY13 THRU AY59.
022690       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
022700 HE110.
022710       MOVE W15-ACCOUNT    TO R-ACC.
022720       MOVE G-NAME         TO R-ANME.
022730       MOVE W15-DATE       TO R-DTE2.
022740       MOVE W15-REFER      TO R-REF.
022750       MOVE W15-NAR        TO R-NAR.
022760       MOVE W15-VAL        TO R-DBT.
022770	   IF LINAGE-COUNTER < WS-PRN-LENGTH
022780         WRITE R-L1 BEFORE 1 LINES
022790     ELSE
022800         WRITE R-L1 BEFORE PAGE
022810	       PERFORM HE00B.
022820       MOVE SPACES         TO R-DET.
022830 HE115.
012750	     DISPLAY "Updating Bank account" AT 2520 WITH
012760		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
022840       MOVE WS-KSTORE      TO WS-GLKEY.
022860       PERFORM AY12 THRU AY59.
022870       MOVE G-GLTRAN       TO WS-GTRKEY.
022880       INITIALIZE T-REC.
023030 HE120.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
012680       MOVE W15-YMD        TO T-DATE.
023060       MOVE W15-NAR        TO T-NARATIVE.
023070       MOVE W15-REFER      TO T-REFER.
023080       ADD 1               TO G-GLTRAN.
023100       MOVE G-AC           TO T-AC.
023110       MOVE 04             TO T-CODE.
023120       MULTIPLY -1 BY W15-VAL 
                                 GIVING W20-CREDIT.
023130       ADD W20-CREDIT      TO G-CR.
023150       MOVE W20-CREDIT     TO T-VAL.
             MOVE W20-CREDIT     TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
023140       ADD W20-CREDIT      TO G-BAL.
023170       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015920       PERFORM AY13 THRU AY59.
023190       MOVE W15-PAYEE      TO T-NARATIVE.
016230       MOVE 5              TO T-SUB.
             ADD 1               TO T-SEQ.
023220       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015920       PERFORM AY13 THRU AY59.
023250       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
023260 HE125.
023270	   IF LINAGE-COUNTER < WS-PRN-LENGTH
023280         WRITE R-L1 BEFORE 1 LINES
023290     ELSE
023300         WRITE R-L1 BEFORE PAGE
023310	       PERFORM HE00B.
023320       MOVE G-AC           TO R-ACC.
023330       MOVE G-NAME         TO R-ANME.
023340       MOVE W15-DATE       TO R-DTE2.
023350       MOVE W15-REFER      TO R-REF.
023360       MOVE W15-PAYEE      TO R-NAR.
023370       MOVE W20-CREDIT     TO R-CRD.
023400       PERFORM AY70 THRU AY999.
	     PERFORM DELETE-GPDCHQ-REC THRU WRITE-GPDCHQ-EXIT.
023420	   IF LINAGE-COUNTER < WS-PRN-LENGTH
023430         WRITE R-L1 BEFORE 2 LINES
023440     ELSE
023450         WRITE R-L1 BEFORE PAGE
023460	       PERFORM HE00B.
023470       MOVE SPACES TO R-DET.
023490 HE130.
023500	     DISPLAY CLEAR-L25.
	     PERFORM HE00.
023610	     GO TO HE05.
       HE135.
023620       MOVE "BATCH TOTAL"  TO R-NAR.
023630       MOVE W20-TOTAL      TO R-DBT.
023640       MULTIPLY -1 BY W20-TOTAL 
                                 GIVING R-CRD.
       HE140.
023650       WRITE R-L1 BEFORE 1 LINES.
023660	     MOVE WS-NORM	 TO R-DET.
023670       WRITE R-L1 BEFORE PAGE.
023680       MOVE SPACES         TO R-DET.
	     GO TO HE999.
       HE145.
	     MOVE "****    N O   P D   C H E Q U E S   D U E   ****"
				 TO R-DET.
	     GO TO HE140.
023690 HE999.
023700       EXIT.

      /
023710 ZA000	     SECTION 9.
023720 ZA00.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
      *	     MOVE 44		 TO USER-NUMBER.
      *	     MOVE 1		 TO USER-SETTING.
      *	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *    PRE-DISPLAY ACCEPT FIELDS BEFORE DATA-ENTRY
      *
      *      MOVE 76		 TO USER-NUMBER.
      *      MOVE 3		 TO USER-SETTING.
      *      MOVE 1		 TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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

041920	     MOVE WS-PTH15	 TO W02-PTH2 W02-PTH18
				    W02-PTH3 W02-PTH26
				    W02-PTH4 W02-PTH28
				    W02-PTH7 W02-PTH32
				    W02-PTH8 W02-PTH11
				    W02-PTH35 W02-PTH40
				    W02-PTH45.
042030	     MOVE LS-USER	 TO W02-USE26 W02-USE18.


	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.


	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.
       ZA01A.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
	   ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
				 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
				 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH.
       ZA01B.
023900       OPEN I-O GACCNT.
023910     IF RUNTIME-ERROR
               IF FLE-LOCKED
041880             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
035500     IF WS-STATUS NOT = "00"
035510         MOVE 8 TO WS-F-ERROR
050480         PERFORM OPEN-ERROR.
023930       OPEN I-O GLTRAN.
023940     IF RUNTIME-ERROR
               IF FLE-LOCKED
041880             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
035560     IF WS-STATUS NOT = "00"
035570         MOVE 11 TO WS-F-ERROR
050480         PERFORM OPEN-ERROR.
044300	     OPEN I-O LEDTRF.
           IF WS-STATUS = "05"
               CLOSE LEDTRF
               OPEN OUTPUT LEDTRF
               CLOSE LEDTRF
               OPEN I-O LEDTRF.
044310     IF RUNTIME-ERROR
               IF FLE-LIMIT
		   GO TO ZA49.
050190     IF WS-STATUS NOT = "00"
050200         MOVE 40           TO WS-F-ERROR
050210         PERFORM OPEN-ERROR.
024050	     OPEN OUTPUT RECOVER GLJRN.
024060       CLOSE RECOVER GLJRN.
024070       OPEN I-O RECOVER GLJRN.
042360       MOVE 1              TO WS-PARKEY.
042370       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042380       MOVE PAR-COMPANY    TO W90-COMP.
042390       MOVE PAR-DMY        TO W05-TODAY.
042400       MOVE PAR-YMD        TO W05-TYMD.
050520       MOVE 5              TO WS-PARKEY.
050530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050550       MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
	     MOVE PAR-RATE-DATE	 TO WS-VAT-DATE.
             MOVE W05-VAT-R1     TO W05-RATE.
050560       MOVE PAR-DISCOUNT   TO W15-DISCOUNT.
             MOVE 6              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
036040     IF WX-BNKGL = ZERO
036050         DISPLAY (20, 20) "Parameters not set up - ENTER"
036060                           WITH FOREGROUND-COLOR 14
036070         ACCEPT (20, 50) WS-OPTION WITH FOREGROUND-COLOR 15 AUTO
036080         MOVE 1 TO WS-ERROR.
             MOVE 196            TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-C-ABR (1) = SPACES
               MOVE "Invoice (G/L)"
                                 TO PAR-C-DESC (1)
               MOVE "Inv"        TO PAR-C-ABR (1)
               MOVE 0            TO PAR-C-ACT (1)
               MOVE 11           TO PAR-C-CODE (1)
               MOVE "Debit Note (G/L)" 
                                 TO PAR-C-DESC (2)
               MOVE "D/N"        TO PAR-C-ABR (2)
               MOVE 1            TO PAR-C-ACT (2)
               MOVE 12           TO PAR-C-CODE (2)
               PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT
               ADD 1             TO WS-PARKEY
               PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT
               MOVE "Payment (G/L)"
                                 TO PAR-C-DESC (2)
               MOVE "Pmt"        TO PAR-C-ABR (2)
               MOVE 0            TO PAR-C-ACT (2)
               MOVE 14           TO PAR-C-CODE (2)
               PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
	     MOVE 4		 TO WS-NETKEY.
035770       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
035780       MOVE G-BATCH TO WS-BATCH.
035900     IF G-YEAR-END = ZERO
035910         MOVE 2 TO G-YEAR-END
035920         PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *
035930	     MOVE 1		 TO WS-S1.
035940	     MOVE G-YEAR-END	 TO WS-S2 WS-Y-END.
035160	     MOVE G-LAST-YE	 TO WS-L-END.
	   IF G-OPEN = ZERO
	       MOVE WS-Y-END	 TO G-OPEN.
	     MOVE G-OPEN	 TO WS-OPEN.
024330	     PERFORM AB70 THRU AB75.
       ZA48.
	   IF WS-PRN-NO < 3
	       PERFORM ZA210 THRU ZA215
	       IF WS-ERROR = 1
		   GO TO ZA205
	   ELSE
	       DISPLAY "Waiting for printer response" AT 1412
			WITH FOREGROUND-COLOR 14 BLINK.
024370	     OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
024380	     GO TO ZA999.
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
021870       MOVE SPACES         TO WS-MID-LNE2 WS-MD-LNE WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCR  (WS-S1) WS-BCR  (WS-S1).
             MOVE WS-G8          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1)
                                    WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
021930       MOVE WS-G2          TO WS-TCR  (8)  WS-TCR (50)
021940                              WS-TCR  (66).
021950	     MOVE WS-G3 	 TO WS-MCHR (11) WS-MCHR (45)
021960				    WS-MCHR (63)
				    WS-MCHR (1)	 WS-MCHR (80)
				    WS-MCR  (8)	 WS-MCR (50)
021960                              WS-MCR  (66) WS-MCR (1)
                                    WS-MCR  (80) WS-MCH (1)
                                    WS-MCH  (80).
021970       MOVE WS-G4          TO WS-BCR  (8)  WS-BCR (50)
021980                              WS-BCR  (66).
051190       MOVE WS-G9          TO WS-TCH  (1).
051200       MOVE WS-G10         TO WS-TCH  (80).
051210       MOVE WS-G11         TO WS-BCH  (1).
051220       MOVE WS-G12         TO WS-BCH  (80).
051230       MOVE WS-G14         TO WS-TCR  (1)  WS-BCR (1).
051260       MOVE WS-G13         TO WS-TCR (80)  WS-BCR (80).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
024390 ZA200.
024400	     DISPLAY "Files locked - Try later" AT 2312
024410		      WITH FOREGROUND-COLOR 14.
024420       DISPLAY "Press any key" AT 2512 
024430		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
024440	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA210.
	     MOVE ZERO		 TO WS-ERROR
				    WS-S1 WS-S2 WS-S3 WS-S4
				    WS-S5 WS-S6 WS-S7 WS-S8.
	     CALL "PC_TEST_PRINTER" USING WS-PRN-NO
					  WS-PRN-STAT
				    RETURNING WS-STATUS.
	   IF WS-PRN-STAT > 127
	       MOVE 1		 TO WS-S1
	       SUBTRACT 128	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 63
	       MOVE 1		 TO WS-S2
	       SUBTRACT 64	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 31
	       MOVE 1		 TO WS-S3
	       SUBTRACT 32	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 15
	       MOVE 1		 TO WS-S4
	       SUBTRACT 16	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 7
	       MOVE 1		 TO WS-S5
	       SUBTRACT 8	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 3
	       MOVE 1		 TO WS-S6
	       SUBTRACT 4	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 1
	       MOVE 1		 TO WS-S7
	       SUBTRACT 2	 FROM WS-PRN-STAT.
	     MOVE WS-PRN-STAT	 TO WS-S8.
	   IF WS-S3 = 0 AND
	      WS-S5 = 0 AND
	      WS-S8 = 0
	       GO TO ZA215.
	     DISPLAY "Check printer - " AT 2304
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "R" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "etry - "
		     "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "bort".
       ZA212.
	     ACCEPT WS-OPTION AT 2334
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     IF NOT (WS-OPTION = "A" OR "R")
		 GO TO ZA212.
	     IF WS-OPTION = "A"
		 MOVE 1 	 TO WS-ERROR
		 GO TO ZA215.
	       GO TO ZA210.
       ZA215.
	     EXIT.
024470 ZA999.
024480       EXIT.
      /
024490 ZB000	     SECTION 9.
      
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
041840	   ELSE
041810     IF WS-F-ERROR = 8
041820         MOVE W02-GACCNT   TO WS-FILE
041830         MOVE WS-GLKEY     TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 11
041860         MOVE W02-GLTRAN   TO WS-FILE
041870         MOVE WS-GTRKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER  TO WS-FILE
041870         MOVE WS-RECKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 26
041860         MOVE W02-GLJRN    TO WS-FILE
041870         MOVE WS-GLJKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 35
041860	       MOVE W02-TXPTRN	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TXP-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 45
041860	       MOVE W02-GPDCHQ	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
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
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
