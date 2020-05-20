      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	   ******     ******   *
      *   **	**  **	  **  **    **	 **  **   **	**   **    **  *
      *   **	    **	  **  **    **	**    **	**	   **  *
      *   **	    ******    *******	**    **     ****	****   *
      *   **	    **	 **   **	**    **	**	   **  *
      *   **	**  **	  **  **	 **  **   **	**   **    **  *
      *    ******   **	  **  **	  ****	   ******     ******   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *        C R E D I T O R / G L   C H E Q U E S   I S S U E D     *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       CRP033.
000030 AUTHOR.		       James William Lemmon.
000040 DATE-WRITTEN.           MARCH 1984.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1984 - 2011
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

000180 COPY CRTRAN.SL.

000140 COPY CRDMEM.SL.

000200 COPY GACCNT.SL.

000220 COPY GLTRAN.SL.

       COPY LEDTRF.SL.

000140 COPY PARAM.SL.

000280 COPY RECOVER.SL.

       COPY SHARED.SL.

000200 COPY TXTRAN.SL.

000310 DATA DIVISION.
000320 FILE SECTION.

       COPY APACFIDS.FDE.

000140 COPY CONTROL.FDE.

000360 COPY CREDIT.FDE.

000380 COPY CRTRAN.FDE.

000140 COPY CRDMEM.FDE.

000400 COPY GACCNT.FD.

000420 COPY GLTRAN.FD.

       COPY LEDTRF.FD.

000340 COPY PARAM.FDE.

000480 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000200 COPY TXTRAN.FDE.

      /
002060 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001790 77  WS-SUB	   PIC	9(04)	 COMP-5.
001790 77  WS-IXS	   PIC	9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC	9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC	9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC	9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC	9(04)	 COMP-5.
002070 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
002070 77  WS-NETKEY	   PIC	9(04)	 COMP-5 VALUE 1.
002130 77  WS-KSTORE	   PIC	9(06)	 COMP-5.
001760 77  WS-SKSTORE	   PIC	9(06)	 COMP-5.
002140 77  WS-KEYSTR       PIC  9(04)    COMP-5.
002150 77  WS-GLJSTR       PIC  9(04)    COMP-5.
002160 77  WS-GLJKEY       PIC  9(04)    COMP-5.
002170 77  WS-RECKEY	   PIC	9(04)	 COMP-5.
002180 77  WS-RECOVER	   PIC	9(04)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002190 77  WS-TRANS	   PIC	9(04)	 COMP-5 VALUE 1.
002200 77  WS-ISUB	   PIC	9(04)	 COMP-5.
002210 77  WS-CHEQUE	   PIC	9(04)	 COMP-5.
002220 77  WS-PAGE	   PIC	9(04)	 COMP-5.
002230 77  WS-LINES	   PIC	9(04)	 COMP-5 VALUE 0.
002240 77  WS-S 	   PIC	9(04)	 COMP-5.
002250 77  WS-S1	   PIC	9(04)	 COMP-5.
002260 77  WS-S2	   PIC	9(04)	 COMP-5.
002270 77  WS-S3	   PIC	9(04)	 COMP-5.
002280 77  WS-S4	   PIC	9(04)	 COMP-5.
002280 77  WS-S5	   PIC	9(04)	 COMP-5.
002280 77  WS-S6	   PIC	9(04)	 COMP-5.
002280 77  WS-S7	   PIC	9(04)	 COMP-5.
002280 77  WS-S8	   PIC	9(04)	 COMP-5.
001900 77  WS-Y-END        PIC  9(02) VALUE ZERO.
001900 77  WS-L-END	   PIC	9(04) VALUE ZERO.
001900 77  WS-OPEN         PIC  9(02) VALUE ZERO.
001910 77  WS-MS1          PIC  9(04)    COMP-5.
001920 77  WS-MS2          PIC  9(04)    COMP-5.
001930 77  WS-VALUE        PIC S9(09)V99 COMP-3.
002080 77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
002290 77  WS-DEL	   PIC	X(01).
002290 77  WS-XFRC         PIC  X(01) VALUE "N".
002300 77  WS-NUM	   PIC	Z9.
002310 77  WS-UPDATE	   PIC	9(01).
002320 77  WS-CONREC       PIC  9(01) VALUE ZERO.
002330 77  WS-GLCON        PIC  9(01) VALUE ZERO.
002360 77  WS-HCRED	   PIC	X(09) VALUE "CREDITOR".
002370 77  WS-HDTE	   PIC	X(05) VALUE "DATE".
002380 77  WS-HACNO	   PIC	X(06) VALUE "A/C NO".
002430 77  WS-HNAME	   PIC	X(04) VALUE "NAME".
002440 77  WS-HAGE	   PIC	X(12) VALUE "AGE 30 - 180".
002450 77  WS-HCHQ	   PIC	X(06) VALUE "CHEQUE".
002530 77  WS-OPTION	   PIC	X(01).
001970 77  WS-SKIP         PIC  X(01) VALUE "N".
002780 77  WS-LOOK	   PIC	X(01) VALUE "C".
       77  WS-CHQ-PRN	   PIC	X(01) VALUE "N".
002540 77  WS-FIRST	   PIC	9(02) VALUE 0.
002550 77  WS-LAST         PIC  9(02) VALUE 0.
003560 77  WS-CHAR1	   PIC	X(01).
002570 77  WS-ERROR	   PIC	9(01) VALUE ZERO.
003620 77  WS-KEY1	   PIC	9(04)	  COMP-5.
000630 77  WS-ENT	   PIC	X(08).
000640 77  WS-ENT1	   PIC	X(08).
002600 77  WS-BATCH	   PIC	9(05)	  COMP-3.
002610 77  WS-TNEXT	   PIC	9(04)	  COMP-5.
002640 77  WS-H1	   PIC	X(06) VALUE "DATE:".
002650 77  WS-H3	   PIC	X(07) VALUE "BATCH:".
002660 77  WS-H4	   PIC	X(07) VALUE "PAGE:".
002690 77  WS-ER1	   PIC	X(08) VALUE "Account".
002700 77  WS-ER3	   PIC	X(05) VALUE "Date".
002710 77  WS-ER4	   PIC	X(06) VALUE "Value".
002720 77  WS-ER5	   PIC	X(07) VALUE "Cheq no".
002730 77  WS-ER6	   PIC	X(11) VALUE "No record".
002740 77  WS-ER7	   PIC	X(13) VALUE "Group heading".
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
       77  TODAY-DDMMYY	   PIC	9(08)	  COMP-5.
       77  WS-USUB	   PIC	9(04)	  COMP-5.
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
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
002430	   03  WS-TP-LNE.
002440         05  WS-TCR  PIC X(01) OCCURS 80.
002450     03  WS-MD-LNE.
002460         05  WS-MCR  PIC X(01) OCCURS 80.
	   03  WS-MDCR-LNE REDEFINES WS-MD-LNE.
	       05  WS-MCR1     PIC  X(01).
	       05  WS-MCR78    PIC  X(78).
	       05  WS-MCR80    PIC  X(01).
002470	   03  WS-BT-LNE.
002480         05  WS-BCR  PIC X(01) OCCURS 80.
	   03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002760 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "CRP".
	   03  WS-PROG	       PIC  X(03) VALUE "033".

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

000360 COPY CREDIT.ID.

000380 COPY CRTRAN.ID.

000140 COPY CRDMEM.ID.

000400 COPY GACCNT.ID.

000420 COPY GLTRAN.ID.

       COPY LEDTRF.IDG.

       COPY PARAM.ID.

000480 COPY RECOVER.ID.

       COPY SHARED.ID.

000200 COPY TXTRAN.ID.

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
	       05  R-DET       PIC  X(140).

000540	   03  R-L2 REDEFINES W02-PRN-LINE.
000550	       05  R-H1        PIC  X(07).
000560	       05  R-DTE1      PIC  Z9/99/9999.
000570	       05  FILLER      PIC  X(31).
000580	       05  R-H2        PIC  X(40).
000590	       05  FILLER      PIC  X(33).
000600	       05  R-H3        PIC  X(08).
000610	       05  R-PGE       PIC  ZZ9.

000620	   03  R-L2A REDEFINES W02-PRN-LINE.
000630	       05  FILLER      PIC  X(56).
000640	       05  R-HEAD      PIC  X(20).
000650	       05  FILLER      PIC  X(56).

000660	   03  R-L3A REDEFINES W02-PRN-LINE.
000670	       05  FILLER      PIC  X(05).
000680	       05  I-H1        PIC  X(11).
000690	       05  I-H2        PIC  X(34).
000700	       05  I-H3        PIC  X(44).
000710	       05  I-H4        PIC  X(11).
000720	       05  I-H5        PIC  X(27).

000730	   03  R-L4 REDEFINES W02-PRN-LINE.
000740	       05  G-H1        PIC  X(10).
000750	       05  G-SUPP      PIC  X(05).
000760	       05  FILLER      PIC  X(02).
000770	       05  G-NAMER     PIC  X(40).
000780	       05  FILLER      PIC  X(23).

000790	   03  R-L4A REDEFINES W02-PRN-LINE.
	       05  G-H3 	   PIC X(04).
	       05  FILLER	   PIC X(01).
	       05  G-INVN	   PIC X(08).
000820	       05  FILLER	   PIC X(01).
	       05  G-H4 	   PIC X(04).
	       05  FILLER	   PIC X(01).
	       05  G-INVD	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(01).
	       05  G-H5 	   PIC X(05).
	       05  FILLER	   PIC X(01).
000870	       05  G-RAIL	   PIC Z(04)9.99-.
000880	       05  FILLER	   PIC X(01).
	       05  G-H6 	   PIC X(04).
	       05  FILLER	   PIC X(01).
	       05  G-DISC	   PIC Z(04)9.99-.
	       05  FILLER	   PIC X(01).
	       05  G-H6A	   PIC X(04).
	       05  FILLER	   PIC X(01).
	       05  G-VAT	   PIC Z(05)9.99-.
	       05  FILLER	   PIC X(01).
	       05  G-H7 	   PIC X(06).
	       05  G-TOTAL	   PIC Z(07)9.99-.

000940	   03  R-L5A REDEFINES W02-PRN-LINE.
000950	       05  FILLER      PIC  X(05).
000960	       05  I-ACNO      PIC  9(04)/99.
000970	       05  FILLER      PIC  X(04).
000980	       05  I-NAME      PIC  X(30).
000990	       05  FILLER      PIC  X(04).
001000	       05  I-NAR       PIC  X(36).
001010	       05  FILLER      PIC  X(02).
001020	       05  I-VALUE     PIC  Z(08)9.99-.
001030	       05  FILLER      PIC  X(04).
001040	       05  I-30        PIC  X(04).
001050	       05  I-60        PIC  X(04).
001060	       05  I-90        PIC  X(04).
001070	       05  I-120       PIC  X(04).
001080	       05  I-150       PIC  X(04).
001090	       05  I-180       PIC  X(07).

001100	   03  R-L6 REDEFINES W02-PRN-LINE.
001110	       05  D-H1        PIC  X(06).
001120	       05  D-H2        PIC  X(10).
001130	       05  D-H3        PIC  X(08).
001140	       05  D-H4        PIC  X(10).
001150	       05  D-H5        PIC  X(08).
001160	       05  D-H6        PIC  X(13).
001170	       05  D-H7        PIC  X(13).
001180	       05  D-H8        PIC  X(12).

001190	   03  R-L6A REDEFINES W02-PRN-LINE.
001200	       05  D-ACNO      PIC  X(05).
001210	       05  FILLER      PIC  X(02).
001220	       05  D-NAME      PIC  X(73).

001230	   03  R-L6B REDEFINES W02-PRN-LINE.
001240	       05  FILLER      PIC X(05).
001250	       05  D-DNO       PIC Z(05)9.
001260	       05  FILLER      PIC X(01).
001270	       05  D-DNDTE     PIC Z9/99/9999.
001280	       05  FILLER      PIC X(01).
001290	       05  D-INV       PIC Z(07)9.
001300	       05  FILLER      PIC X(01).
001310	       05  D-IDTE      PIC Z9/99/9999.
001320	       05  FILLER      PIC X(01).
001330	       05  D-COST      PIC Z(06)9.99-.
001340	       05  FILLER      PIC X(01).
001350	       05  D-SELL      PIC Z(07)9.99-.
001360	       05  FILLER      PIC X(01).
001370	       05  D-30        PIC X(02).
001380	       05  D-60        PIC X(02).
001390	       05  D-90        PIC X(02).
001400	       05  D-120       PIC X(02).
001410	       05  D-150       PIC X(02).
001420	       05  D-180       PIC X(02).

001430	   03  R-L7 REDEFINES W02-PRN-LINE.
001440	       05  CH-H1       PIC  X(07).
001450	       05  CH-H2       PIC  X(32).
001460	       05  CH-H3       PIC  X(08).
001470	       05  CH-H4       PIC  X(13).
001480	       05  CH-H5       PIC  X(08).
001490	       05  CH-H6       PIC  X(12).

001500	   03  R-L7A REDEFINES W02-PRN-LINE.
001510	       05  CH-ACNO     PIC  X(06).
001520	       05  FILLER      PIC  X(01).
001530	       05  CH-NAME     PIC  X(28).
001540	       05  FILLER      PIC  X(01).
001550	       05  CH-DATE     PIC  Z9/99/9999.
001560	       05  FILLER      PIC  X(01).
001570	       05  CH-CHQ      PIC  Z(05)9.
001580	       05  FILLER      PIC  X(01).
001590	       05  CH-VAL      PIC  Z(08)9.99-.
001600	       05  FILLER      PIC  X(01).
001610	       05  CH-30       PIC  X(02).
001620	       05  CH-60       PIC  X(02).
001630	       05  CH-90       PIC  X(02).
001640	       05  CH-120      PIC  X(02).
001650	       05  CH-150      PIC  X(02).
001660	       05  CH-180      PIC  X(02).

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

001830	   03  R-L9 REDEFINES W02-PRN-LINE.
001840	       05  FILLER      PIC  X(03).
001850	       05  R-ACC       PIC  9(04)/99.
001860	       05  FILLER      PIC  X(04).
001870	       05  R-ANME      PIC  X(24).
001880	       05  FILLER      PIC  X(02).
001890	       05  R-DTE2      PIC  Z9/99/9999.
001900	       05  FILLER      PIC  X(03).
001910	       05  R-REF       PIC  Z(08).
001920	       05  FILLER      PIC  X(04).
001930	       05  R-NAR       PIC  X(36).
001940	       05  FILLER      PIC  X(03).
001950	       05  R-DBT       PIC  Z(08)9.99.
001960	       05  FILLER      PIC  X(02).
001970	       05  R-CRD       PIC  Z(08)9.99-.
001980	       05  FILLER      PIC  X(01).

001990	   03  R-L10 REDEFINES W02-PRN-LINE.
002000	       05  FILLER      PIC  X(65).
002010	       05  R-H16       PIC  X(06).
002020	       05  FILLER      PIC  X(01).
002030	       05  R-H17       PIC  X(23).
002040	       05  R-H18       PIC  X(06).
002050	       05  FILLER      PIC  X(31).

000770	   03  R2-L2 REDEFINES W02-PRN-LINE.
000780	       05  R2-H1       PIC  X(06).
000790	       05  R2-DTE1     PIC  Z9/99/9999.
000800	       05  FILLER      PIC  X(08).
000810	       05  R2-H2       PIC  X(40).
000820	       05  R2-H2A      PIC  X(27).
000830	       05  FILLER      PIC  X(10).
000840	       05  R2-H3       PIC  X(07).
000850	       05  R2-BTCH     PIC  Z(04)9.
000860	       05  FILLER      PIC  X(10).
000870	       05  R2-H4       PIC  X(06).
000880	       05  R2-PGE      PIC  ZZ9.

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
000790	       05  FILLER      PIC  X(03).
000800	       05  R2-DTE2     PIC  Z9/99/9999.
000810	       05  FILLER      PIC  X(03).
000820	       05  R2-REF      PIC  Z(07).
000830	       05  FILLER      PIC  X(04).
000840	       05  R2-NAR      PIC  X(36).
001170	       05  FILLER      PIC  X(01).
	       05  R2-DBRKO    PIC  X(01).
001180	       05  R2-DBT      PIC  Z(08)9.99.
	       05  R2-DBRKC    PIC  X(01).
	       05  R2-CBRKO    PIC  X(01).
001200	       05  R2-CRD      PIC  Z(08)9.99-.
	       05  R2-CBRKC    PIC  X(01).
001210	       05  FILLER      PIC  X(01).

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
	       05  PD-NAR      PIC  X(38).
	       05  FILLER      PIC  X(01).
	       05  PD-DATE     PIC  Z9/99/9999.
	       05  FILLER      PIC  X(01).
	       05  PD-REF      PIC  Z(06)9.
	       05  FILLER      PIC  X(01).
	       05  PD-VALUE    PIC  Z(08)9.99.
	       05  FILLER      PIC  X(01).

       COPY W05.WS.

003250 01  W10-H2.
003260	   03  FILLER	       PIC  X(27).
003270	   03  W10-HEAD        PIC  X(13).

       COPY W15.CGL.

003750 01  W20-TOTALS.
003760     03  W20-DEBIT       PIC S9(09)V99 COMP-3.
003770     03  W20-CREDIT      PIC S9(09)V99 COMP-3.
003770     03  W20-VAT         PIC S9(09)V99 COMP-3.
003780     03  W20-RESULT      PIC S9(09)V99.
003790     03  W20-RES1        REDEFINES W20-RESULT.
003800	       05  W20-WHOLE   PIC  9(09).
003810         05  W20-DEC     PIC S9(02).
003820     03  W20-TOTAL       PIC S9(09)V99 COMP-3.
003830     03  W20-VALUE       PIC S9(09)V99 COMP-3.
	   03  W20-DATE.
	       05  W20-CC      PIC 99.
               05  W20-YY      PIC 99.
               05  W20-MM      PIC 99.
               05  W20-DD      PIC 99.
           03  W20-DTE REDEFINES W20-DATE
			       PIC 9(08).
	   03  W21-DATE.
	       05  W21-CC      PIC 99.
	       05  W21-YY      PIC 99.
	       05  W21-MM      PIC 99.
	       05  W21-DD      PIC 99.
	   03  W21-DTE REDEFINES W21-DATE
			       PIC 9(08).

003700 01  W25-CALCS.
005040	   03  W25-RESULT      PIC  9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340	       05  W25-WHOLE   PIC  9(05).
004350	       05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).

003840 01  W25-EDIT.
003850     03  W25-3V2.
003860	       05  W25-2       PIC  ZZ9.99.
003870     03  W25-7V2.
003880	       05  W25-3       PIC  Z(06)9.99.
003890     03  W25-3N.
003900	       05  W25-4       PIC  ZZ9.
003910     03  W25-DATE.
003920	       05  W25-DTE     PIC  99/99/9999.
003930     03  W25-6.
003940	       05  W25-NO      PIC  Z(07)9.
003950     03  W25-5V2.
003960	       05  W25-VAL     PIC  Z(04)9.99.
003970     03  W25-S5V2.
003980	       05  W25-SVAL    PIC  Z(04)9.99DB.
003990     03  W25-S7V2.
004000	       05  W25-VAL2    PIC  Z(06)9.99DB.
003990	   03  W25-S8V2.
004000	       05  W25-VG1     PIC  Z(07)9.99CR.
003990	   03  W25-72.
004000	       05  W25-VAL3    PIC  Z(06)9.99-.
004010     03  W25-REF.
004020	       05  W25-REFER   PIC  Z(07).

004030 01  W25-VALUES.
004040     03  W25-CFLOW  OCCURS 6 PIC S9(07)V99  COMP-3.
004050     03  W25-V1          PIC S9(07)V99  COMP-3. 
004060     03  W25-V2          PIC S9(07)V99  COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

005450 COPY W50.WS.

004070 01  W50-H4.
004080	   03  W50-HEAD        PIC  X(20).

004090 01  W55-DEL.
004100	   03  W55-MNTH        PIC  X(03).
004110	   03  FILLER	       PIC  X(01)   VALUE ",".
004120	   03  W55-YY	       PIC  9(02).

004130 01  W60-H2.
004140	   03  W60-DESC        PIC  X(21)   VALUE
004150                             "CREDITORS AUDIT TRIAL".

004160 01  W65-CREDITOR.
004170     03  W65-CFLOW OCCURS 6 PIC 9(07)V99 COMP-3.

004180 01  W70-ACTS.
004190	   03  W70-BRANCH      PIC  9(04)     COMP-5.

004370 COPY W70CRED.WS.

       01  W75-KEYS.
	   03  W75-S	       PIC  9(02) COMP-5.
	   03  W75-S1	       PIC  9(02) COMP-5.
	   03  W75-CNO	       PIC  X(06) OCCURS 18.
	   03  W75-GNO	       PIC  9(06) COMP-5 OCCURS 18.

004410 01  W90-COMP.
004420	   03  W90-CNAME       PIC  X(40).

003980 01  W90-TRAN.
004020     03  W90-CODE        PIC  9(02).
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.

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

       01  W300-CHEQUE.
	   03  W300-CHQDTE     PIC  9(06)    COMP.
	   03  W300-CHQNO      PIC  9(08)    COMP.
	   03  W300-PAYEE      PIC  X(36).
	   03  W300-NAR        PIC  X(36).
	   03  W300-CHQVAL     PIC S9(09)V99 COMP.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004430 SCREEN SECTION.

       COPY BLANK.CRT.

004950 01  S05.
004960	   03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "CREDITORS - CHEQUES ISSUED".
004980	   03  LINE  4 COLUMN  4 VALUE "Creditor Ac.no.:".
004990	   03  LINE  6 COLUMN 14 VALUE "Date".
005000     03          COLUMN 23 VALUE "Chq no".
005010     03          COLUMN 34 VALUE "Cheq value".
005020	   03	       COLUMN 49 VALUE "30".
	   03	       COLUMN 53 VALUE "60".
	   03	       COLUMN 57 VALUE "90".
	   03	       COLUMN 60 VALUE "120".
	   03	       COLUMN 64 VALUE "150".
	   03	       COLUMN 68 VALUE "180".

005030 01  S06.
005040	   03  FOREGROUND-COLOR 7 HIGHLIGHT
				  BACKGROUND-COLOR 5
				  PIC X(06) USING W15-ACNO AUTO.

005090 01  S09.
005100	   03  FOREGROUND-COLOR 7 HIGHLIGHT
				  BACKGROUND-COLOR 5
				  PIC 9(06) USING W15-ACCOUNT AUTO.

005200 01  S14.
005210	   03  FOREGROUND-COLOR 7 HIGHLIGHT
				  BACKGROUND-COLOR 5
				  PIC Z(06)9 USING W15-GRNNO AUTO.

005220 01  S15.
005230	   03  FOREGROUND-COLOR 7 HIGHLIGHT
				  BACKGROUND-COLOR 5
				  PIC Z9/99/9999 USING W15-GRND AUTO.

005240 01  S16.
005250	   03  FOREGROUND-COLOR 7 HIGHLIGHT
				  BACKGROUND-COLOR 5
				  PIC Z(07)9.99DB
005260                                 USING W15-TCOST AUTO.

005270 01  S17.
005280	   03  FOREGROUND-COLOR 7 HIGHLIGHT
				  BACKGROUND-COLOR 5
				  PIC Z(07)9.99DB
005290                                 USING W15-CHQVAL AUTO.

       COPY S99.CRT.

       COPY CRDLUP.CRT.

       COPY LEDLUP.CRT.

       01  S-CRED.
	   03  LINE 23 COLUMN  4 VALUE "Balance :".
	   03	       COLUMN 14 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-BAL.
	   03	       COLUMN 30 VALUE "Due :".
	   03	       COLUMN 36 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-OUT.

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
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
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

       COPY ERROR.CRT.

       COPY OPT.CRT.

       COPY UPDATE.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
005310 AA000	       SECTION.
005320 AA00.
003240	   IF LS0-CRLEV < 3
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
	     PERFORM FA000.

005640 AA11.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
005650	     CLOSE GACCNT
005670		   GLTRAN
		   LEDTRF.

       AA11E.
             EXIT PROGRAM.

       AA20-LOOKUP     SECTION.
       AA20G.
	     MOVE "G"		 TO WS-LOOK.

       AA20.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	   IF WS-LOOK = "C"
	       PERFORM CREDITOR-LOOKUP
	   ELSE
	       PERFORM GACCNT-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
	     MOVE "C"		 TO WS-LOOK.
       AA99.
             EXIT.

005910 AA50.
      *
      *    ****    Print the audit trail HEADING
      *
	     MOVE "H"		 TO W02-PRN-TYPE.
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
006010	     MOVE W60-DESC	 TO R-H2.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
006030	     MOVE SPACE 	 TO R-DET.

006040 AA60.
006050	     MOVE W50-H4	 TO R-HEAD.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
006070	     MOVE SPACES	 TO R-DET.

006080 AA65.
006090       EXIT.

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

       COPY LOCKED.REC.

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

       COPY CREDITOR.TRN.

       COPY PRINTUTL.AS4.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
006190 AB000	       SECTION.

       COPY CONTROL.RD.

       COPY CRTRAN.RD.

       COPY CREDIT.RD.

000140 COPY CRDMEM.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD2.

       COPY GLTRAN.RD.

       COPY LEDTRF.RD1.

       COPY SHARED.RD.

000200 COPY TXTRAN.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
007740 AC000	       SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

       COPY CRTRAN.WR.

       COPY CREDIT.WR.

       COPY LEDTRF.WR.

000200 COPY TXTRAN.WR.

      /
009010 AE155-GET-CREDITOR    SECTION.
009020 AE155-ACCOUNT.
011060       MOVE W15-ACNO       TO P-NUMBER.
011130       PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
011140     IF WS-F-ERROR = 3 
011150         MOVE 1 TO WS-ERROR
011160     ELSE
011170     IF P-NUMBER NOT = W15-ACNO
011180         MOVE 2 TO WS-ERROR
011190     ELSE
011200         MOVE 0 TO WS-ERROR.
009180 AE160-EXIT.
009190       EXIT.

       COPY CREDIT.LUP.

       COPY APAC.HLP.

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
	     MOVE G-AC		 TO WS-SKSTORE.
	   IF G-AC < 400000 AND WX-UNPROF > 400000
	       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT
	       MOVE WX-UNPROF	 TO W15-ACCOUNT
008970	       MOVE W15-ACCOUNT	 TO G-AC
008980	       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
	       IF WS-F-ERROR = 0
		   MOVE G-AC	 TO T-AC
015130		   PERFORM AY12 THRU AY59
	       ELSE
		   MOVE WS-SKSTORE
				 TO W15-ACCOUNT G-AC
		   PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
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
011740 AY01.
011690       MOVE 01             TO REC-FILE.
011700       MOVE WS-PARKEY      TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
011720       MOVE PAR-RECORD1    TO REC-PARAM.
011780       GO TO AY50.

009590 AY07.
009600	     MOVE 07		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
009620       MOVE P-REC          TO REC-CREDIT.
009630       GO TO AY50.
009640 AY08.
011900       MOVE 08             TO REC-FILE.
011910	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011920	     MOVE CT-REC	 TO REC-CRTRAN.
009680       GO TO AY50.
009690 AY12.
009700       MOVE 12             TO REC-FILE.
009710	     MOVE ZERO		 TO REC-KEY REC-TYPE.
	     PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
009720       MOVE G-REC          TO REC-GACCNT.
009730       GO TO AY50.

009740 AY13.
009750       MOVE 13             TO REC-FILE.
010660	     MOVE ZERO		 TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
009770	     MOVE T-REC 	 TO REC-GLTRAN.
009780       GO TO AY50.

011970 AY19.
011980       MOVE 19             TO REC-FILE.
011990       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
012010       MOVE TAX-RECORD1    TO REC-TXTRAN.
012020       GO TO AY50.

012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.

009790 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  End transaction in recovery log	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012040 AY49.
009800	     MOVE 99		 TO REC-FILE.
009810	     MOVE ZERO		 TO REC-KEY.
009820	     MOVE SPACES	 TO REC-DETAIL.
009830       PERFORM AY50.
009840	     ADD 1		 TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 195 - Clear (Close and Open    ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
009850	   IF WS-RECOVER > 195
009860         CLOSE RECOVER
009870         OPEN OUTPUT RECOVER
009880         CLOSE RECOVER
009890         OPEN I-O RECOVER
009900	       MOVE ZERO	 TO WS-RECOVER.
009910       GO TO AY59.
009920 AY50.
009930	     ADD 1		 TO WS-RECOVER.
009940	     MOVE WS-RECOVER	 TO WS-RECKEY.
009950	     MOVE WS-TRANS	 TO REC-TRAN.
009960       WRITE REC-RECORD.
009970     IF WS-STATUS NOT = "00"
009980	       DISPLAY "Write error Recovery file - Status " AT 2212
009990                  WITH FOREGROUND-COLOR 14 
010000                  WS-STATUS WITH FOREGROUND-COLOR 15
010010         STOP RUN.
010040 AY59.
010050       EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Start processing transaction 	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *    *************************************************************
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *    *************************************************************
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *    *************************************************************
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *    *************************************************************
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
012620	     MOVE "CG"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *    *************************************************************
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *    *************************************************************
012010	     MOVE 2		 TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
	     MOVE NET-CREDITOR	 TO W70-CREDCON.
012010       MOVE 4              TO WS-NETKEY.
012700       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012710       PERFORM AY39 THRU AY59.
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
012760		   IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG" OR "GL"
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
      * 	   NETWORK record 2.
      *
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012970	     MOVE 2		 TO WS-NETKEY.
012980       MOVE W70-CREDCON    TO NET-CREDITOR.
012990	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
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
012890	     DIVIDE W15-CCYY BY 4
				 GIVING W20-RESULT.
010890	   IF W20-DEC > ZERO
010900	       GO TO BA10.
010910	     GO TO BA15.
010920 BA10.
010930       MOVE 1 TO WS-ERROR.
	     GO TO BA999.
      *
      *   ****    CALCULATE THE PERIOD INTO WHICH THIS DATE FALLS
      * 
       BA15.
	     MOVE W15-MM	 TO WS-PERIOD.
	   IF W15-CCYY > W05-CY
	       GO TO BA10.
	   IF W15-CCYY < W05-CY
	       GO TO BA20.
	   IF W05-MM = W15-MM
	      GO TO BA999.
	   IF W15-MM > W05-MM
	       GO TO BA10.
	   IF W15-MM > WS-Y-END
	       GO TO BA999.
	   IF W15-CCYY > WS-L-END
	       GO TO BA999.
	     ADD 12		 TO WS-PERIOD
	     GO TO BA999.

       BA20.
	   IF W15-CCYY > WS-L-END
	       GO TO BA999.
	   IF (W15-CCYY = WS-L-END) AND
	      (W15-MM > WS-Y-END)
	       GO TO BA999.
	     ADD 12		 TO WS-PERIOD.

010940 BA999.
010950       EXIT.
       CA600         SECTION.
       CA601.
             MOVE CT-CODE        TO W90-CODE.
             PERFORM GET-CREDITOR-TRAN-RECORD.
             SUBTRACT CT-COST    FROM W00-C-VAL
                                      W00-C-MTDV
                                      W00-C-YTDV.
             ADD 1               TO W00-C-DAY
                                    W00-C-MTD
                                    W00-C-YTD.
             PERFORM UPDATE-TRAN-RECORD.
       CA699.
             EXIT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			 GL-TRF-UPDATE			       ³
      *	   ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ GENERAL LEDGER:					       ³
      *    ³ This routine is used to UPDATE the INTEGRATION RECORDS    ³
      *    ³ for posting to the G/L ACCOUNTS.			       ³
      *    ³							       ³
      *    ³ THE KEY FOR EACH OF THE RECORDS IS:		       ³
      *    ³							       ³
      *    ³	RUN DATE (CCYYMMDD)  -	WX-YMD			       ³
      *    ³							       ³
      *    ³	TYPE -	WX-TYPE 				       ³
      *    ³		01 : DEBTORS				       ³
      *    ³		02 : CREDITORS				       ³
      *    ³		03 : BAD DEBTS				       ³
      *    ³		04 : SALES				       ³
      *    ³		05 : SALES (RETURNS)			       ³
      *    ³		06 : PURCHASES				       ³
      *    ³		07 : PURCHASES (RETURNS)		       ³
      *    ³		08 : COST-OF-SALES			       ³
      *    ³		09 : COST-OF-SALES (RETURNS)		       ³
      *    ³		10 : OUTPUT VAT (DEBTORS)		       ³
      *    ³		11 : INPUT VAT (DEBTORS)		       ³
      *    ³		12 : OUTPUT VAT (CREDITORS)		       ³
      *    ³		13 : INPUT VAT (CREDITORS)		       ³
      *    ³		14 : DEBTOR TRANSACTIONS		       ³
      *    ³							       ³
      *    ³	TRANSACTION CODE / SALES LEDGER NO - WX-LEDG	       ³
      *    ³							       ³
      *    ³ OTHER DETAILS ARE: 				       ³
      *    ³							       ³
      *    ³	ACCOUNT NUMBER -  WX-AC				       ³
      *    ³							       ³
      *    ³	NARRATIVE - WX-NAR				       ³
      *    ³							       ³
      *    ³	TRANSACTION VALUE - WX-VAL			       ³
      *    ³							       ³
      *    ³ The routine will first read the file using the DETAILS    ³
      *    ³ ABOVE as the KEY. If the record exists the routine WILL   ³
      *    ³ UPDATE the record. If no record exists a NEW record WILL  ³
      *    ³ be CREATED.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY LEDTRF.UPD.

       EA000	     SECTION 50.
       EA00.
	     MOVE "Date in a prior Fin Year -Continue Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
033910	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO EA00.
	   IF WS-OPTION = "Y"
	       MOVE ZERO	 TO WS-ERROR
	   ELSE
	       MOVE 1		 TO WS-ERROR.

       EA99.
	     EXIT.

      /
019550 FA000	     SECTION 50.
019560 FA00A.
019570       MOVE "CHEQUES ISSUED" 
                                 TO W50-HEAD.
019580       MOVE WX-BNKGL       TO W15-BANK.
019590       MOVE WX-CREGL       TO W15-CRED.
019600*      MOVE WS-CONP	 TO R-DET.
019610*      WRITE R-L1 BEFORE 0 LINES.
      *    *************************************************************
      *    ****   C L E A R   T O T A L S   T O	  Z E R O
      *    *************************************************************
019620	     MOVE ZERO		 TO W20-TOTAL W20-DEBIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Audit trail headings.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
019630 FA00B.
019640       PERFORM AA50 THRU AA60.
019650       MOVE WS-HACNO       TO CH-H1.
019660       MOVE WS-HNAME       TO CH-H2.
019670       MOVE WS-HDTE        TO CH-H3.
019680       MOVE WS-HCHQ        TO CH-H4.
019690       MOVE "VALUE"        TO CH-H5.
019700       MOVE WS-HAGE        TO CH-H6.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019720	     MOVE SPACES	 TO R-DET.
019730       MOVE WS-HACNO       TO R-H5.
019740       MOVE ALL "_"        TO R-H6 R-H8 R-H16 R-H18.
019750       MOVE "ACCOUNT NAME" TO R-H7.
019760       MOVE WS-HDTE        TO R-H9.
019770       MOVE "CHEQ NO"      TO R-H10.
019780       MOVE "PAYEE/NARRATIVE" 
                                 TO R-H17.
019790       MOVE "DEBIT"        TO R-H14.
019800       MOVE "CREDIT"       TO R-H15.
	     PERFORM CALL-PRINTUTL.
019820	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

019830 FA00.
019840       DISPLAY CLR-SCREEN.
019850       DISPLAY S05.
      *    *************************************************************
      *    ****   I N I T I A L I Z E	W O R K   V A R I A B L E S
      *    *************************************************************
019860	     MOVE ZERO		 TO W15-GRND  W15-TCOST
				    W15-GRNNO W15-SETT.
             MOVE SPACES         TO W15-ACNO.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Allow user to key in the Creditor account number.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
019870 FA05.
	     DISPLAY "F1" AT 2501
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Creditor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
019880	     ACCEPT S06 AT 0421.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACE	 TO W15-ACNO
		     GO TO FA130
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA20
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA05
	       END-EVALUATE
	       DISPLAY S06 AT 0421
               IF W15-ACNO = SPACES
015990		   GO TO FA05.
	     DISPLAY CLEAR-L25.
019890     IF W15-ACNO = SPACES
019900         GO TO FA130.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
019920	     PERFORM AE155-GET-CREDITOR.
019930     IF WS-ERROR NOT = ZERO
019940	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019950         GO TO FA05.
019960	     MOVE P-NAME	 TO W15-NAME W15-PAYEE.
019970	     DISPLAY P-NAME AT 0428
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             DISPLAY S-CRED.
012180	     PERFORM CHECK-CORRECT.
020110	   IF WS-OPTION = "N"
020120         GO TO FA05.

020140 FA10.
020150	     ACCEPT S15 AT 0811.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO FA05
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA10
	       END-EVALUATE.
020160	     MOVE W15-GRND	 TO W15-DATE.
020170       PERFORM BA000.
020180     IF WS-ERROR = 1
020190         GO TO FA10.
	   IF WS-PERIOD > 12
	       PERFORM EA000
	       IF WS-ERROR = 1
		   GO TO FA10.
      *    *************************************************************
      *    ****    Switch date to CCYYMMDD format
      *    *************************************************************
	     MOVE W15-DD	 TO W20-DD.
             MOVE W15-MM         TO W20-MM.
	     MOVE W15-CC	 TO W20-CC.
             MOVE W15-YY         TO W20-YY.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		     Reference code / number		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
020200 FA15.
020210       ACCEPT S14 AT 0823.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO FA10
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA10
	       END-EVALUATE.
020220	     MOVE W15-GRNNO	 TO W15-REFER.
020230	   IF W15-GRNNO = ZERO
020240         GO TO FA15.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			Transaction value		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
020250 FA20.
020260	     ACCEPT S16 AT 0833.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO FA15
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA10
	       END-EVALUATE.
020270       MULTIPLY -1 BY W15-TCOST 
                                 GIVING W15-CHQVAL.
020280	     DISPLAY S17 AT 0833.
020290     IF W15-TCOST = ZERO
020300         GO TO FA20.
020310 FA25.
020320	     ACCEPT W15-30 AT 0849
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
020330	     ACCEPT W15-60 AT 0853
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
020340	     ACCEPT W15-90 AT 0857
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
020350	     ACCEPT W15-120 AT 0861
020360		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
020370	     ACCEPT W15-150 AT 0865
020380		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
020390	     ACCEPT W15-180 AT 0869
020400		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
012180	     PERFORM CHECK-CORRECT.
020480	   IF WS-OPTION = "N"
020490         GO TO FA10.
020510	     MOVE 1		 TO WS-S2.
020520       MOVE ZERO           TO WS-S.

020530 FA30.
020540       MOVE ZERO           TO W25-CFLOW (WS-S2).
020550     IF W15-MONTH (WS-S2) > ZERO
020560         ADD 1             TO WS-S.
020570     IF WS-S2 < 6
020580         ADD 1             TO WS-S2
020590         GO TO FA30.
020600       MOVE ZERO           TO W25-V1 W25-V2.
020610       MOVE 1 TO WS-S3.
020620     IF WS-S < 2
020630         MOVE W15-CHQVAL   TO W25-V2
020640         GO TO FA35.
020650       MOVE W15-TCOST      TO W20-RESULT.
020660       DIVIDE W20-RESULT BY WS-S 
                                 GIVING W25-V1 ROUNDED.
020670       SUBTRACT 1          FROM WS-S.
020680       COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
020690       ADD 1               TO WS-S.
020700       MULTIPLY -1         BY W25-V1.
020710       MULTIPLY -1         BY W25-V2.

020720 FA35.
012750	     DISPLAY "Processing payment" AT 2520 WITH
012760		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
020730     IF WS-S = ZERO
020740         GO TO FA45.

020750 FA40.
020760     IF W15-MONTH (WS-S3) = ZERO
020770         ADD 1             TO WS-S3
020780         GO TO FA40.
020790     IF WS-S > 1
020800         MOVE W25-V1       TO W25-CFLOW (WS-S3)
020810         SUBTRACT 1        FROM WS-S
020820         ADD 1             TO WS-S3
020830         GO TO FA40.
020840       MOVE W25-V2         TO W25-CFLOW (WS-S3).
020850       MOVE ZERO           TO W25-V1 W25-V2.
	     DISPLAY "Creditor" AT 2540
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Start processing transaction - Lock the control records. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
020860 FA45.
020870       PERFORM AY60 THRU AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Read the Creditor account record, write it to the recovery³
      *    ³ file and lock the record.				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
020880	     MOVE 0		 TO WS-ACTION.
020890       PERFORM AY07 THRU AY59.

020980 FA50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Generate the G/L transaction record.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     INITIALIZE CT-REC.
      *    *************************************************************
      *    ****    Switch date to CCYYMMDD format
      *    *************************************************************
021030	     MOVE W15-GRND	 TO W15-DEL.
	     MOVE W15-DEL-DD	 TO W21-DD.
	     MOVE W15-DEL-MM	 TO W21-MM.
	     MOVE W15-DEL-CC	 TO W21-CC.
	     MOVE W15-DEL-YY	 TO W21-YY.
040840	     MOVE W21-DTE	 TO CT-DTE CT-SDTE.
021040	     MOVE W15-GRNNO	 TO CT-REF CT-SINV.
021050       MOVE P-NUMBER       TO CT-CRED.
021090       MOVE W15-CHQVAL     TO CT-COST.
021100       MOVE 03             TO CT-CODE.
	     MOVE "U"		 TO CT-TYPE.
	     MOVE "P"		 TO CT-PJ.
029080	     MOVE ZERO		 TO CT-PAID
				    CT-SEQ
				    CT-SELL
				    CT-TAXFREE
				    CT-TAX.
021110	     MOVE 1		 TO WS-S WS-S2.
021120     IF W25-V2 NOT = ZERO
021130         GO TO FA65.

021140 FA60.
021150     IF W15-MONTH (WS-S) NOT = ZERO
021160         ADD WS-S2         TO CT-AGE.
021170     IF WS-S < 6
021180         ADD 1             TO WS-S
021190         MULTIPLY 2        BY WS-S2
021200         GO TO FA60.

021210 FA65.
021220       ADD W15-CHQVAL      TO P-BAL.
021230       ADD W15-CHQVAL      TO P-DT.
021240       ADD W25-CFLOW (1)   TO P-30D.
021250       ADD W25-CFLOW (2)   TO P-60D.
021260       ADD W25-CFLOW (3)   TO P-90D.
021270       ADD W25-CFLOW (4)   TO P-120D.
021280       ADD W25-CFLOW (5)   TO P-150D.
021290       ADD W25-CFLOW (6)   TO P-180D.
021300       ADD W25-V2          TO P-OUT.
021310       PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY08 THRU AY59.
	     PERFORM REWRITE-CREDIT THRU WRITE-CREDIT-EXIT.
021360       ADD W15-CHQVAL      TO W70-CLOSE.
021370       ADD W15-CHQVAL      TO W70-DEBT.
019120       ADD W25-CFLOW (6)   TO W70-DUE-180.
019130       ADD W25-CFLOW (5)   TO W70-DUE-150.
019140       ADD W25-CFLOW (4)   TO W70-DUE-120.
019150       ADD W25-CFLOW (3)   TO W70-DUE-90.
019160       ADD W25-CFLOW (2)   TO W70-DUE-60.
019170       ADD W25-CFLOW (1)   TO W70-DUE-30.
      *
      *    ****   TRANSACTION STATISTICS
      *
             PERFORM CA600.
021440	     MOVE W15-ACNO	 TO CH-ACNO.
021450       MOVE W15-NAME       TO CH-NAME.
021460	     MOVE W15-GRNNO	 TO CH-CHQ.
021470       MOVE W15-GRND       TO CH-DATE.
021480       MOVE W15-CHQVAL     TO CH-VAL.
021490     IF W15-MONTH (1) > ZERO
021500         MOVE "Y"          TO CH-30.
021510     IF W15-MONTH (2) > ZERO
021520         MOVE "Y"          TO CH-60.
021530     IF W15-MONTH (3) > ZERO
021540         MOVE "Y"          TO CH-90.
021550     IF W15-MONTH (4) > ZERO
021560         MOVE "Y"          TO CH-120.
021570     IF W15-MONTH (5) > ZERO
021580         MOVE "Y"          TO CH-150.
021590     IF W15-MONTH (6) > ZERO
021600         MOVE "Y"          TO CH-180.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
021650	       PERFORM FA00B.
021660       MOVE SPACES         TO R-DET.
021670       MOVE W15-TCOST      TO W15-VAL.
021680       ADD W15-TCOST       TO W20-TOTAL.
021690       ADD W15-TCOST       TO W20-DEBIT.
021700	     DISPLAY "G/L Bank no  :" AT 1004.
	     DISPLAY "Payee        :" AT 1204.
021710	     DISPLAY "Narrative    :" AT 1404.
021720	     DISPLAY "G/L Creditors:" AT 1604.
021730       MOVE SPACES         TO W15-NAR.

021740 FA75.
021750       MOVE W15-BANK       TO W15-ACCOUNT.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
021760	     ACCEPT S09 AT 1019.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA20G THRU AA20
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA75
	       END-EVALUATE
016570	       DISPLAY S09 AT 1019
016580	       IF W15-ACCOUNT = ZERO
016590		   GO TO FA75.
	     DISPLAY CLEAR-L25.
021780	   IF W15-ASER = ZERO
021790	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
021800         GO TO FA75.
021810       MOVE W15-ACCOUNT    TO W15-BANK.

021820 FA80.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
021850	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
021860         GO TO FA75.
021870	     DISPLAY G-NAME AT 1026
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

021880 FA85.
021890	     MOVE G-AC		 TO WS-KSTORE.
012180	     PERFORM CHECK-CORRECT.

       FA90.
021990	   IF WS-OPTION = "N"
022000         GO TO FA75.

       FA92.
022020	     ACCEPT W15-PAYEE AT 1219
022030		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
022040	   IF W15-PAYEE = SPACES
022050	       GO TO FA92.

022010 FA95.
022020	     ACCEPT W15-NAR AT 1419
022030		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
022040     IF W15-NAR = SPACES
022050         GO TO FA95.
022060	     MOVE W15-CRED	 TO W15-ACCOUNT.

022070 FA96.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
022080	     ACCEPT S09 AT 1619.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO FA95
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA20G THRU AA20
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA96
	       END-EVALUATE
016570	       DISPLAY S09 AT 1619
016580	       IF W15-ACCOUNT = ZERO
015990		   GO TO FA96.
	     DISPLAY CLEAR-L25.
022100	   IF W15-ASER = ZERO
022110	       GO TO FA96.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
022130	   IF WS-F-ERROR = 8
022140	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
022150         GO TO FA96.
022160	     DISPLAY G-NAME AT 1626
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

022170 FA100.
012180	     PERFORM CHECK-CORRECT.
022260	   IF WS-OPTION = "N"
022270         GO TO FA96.
012750	     DISPLAY "Updating Creditor G/L" AT 2520 WITH
012760		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
022290       PERFORM AY12 THRU AY59.
022310       INITIALIZE T-REC.

022460 FA105.
      *
      *    ****    Switch date to CCYYMMDD format
      *
	     MOVE W15-CC	 TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
012680       MOVE W15-YMD        TO T-DATE.
022490       MOVE W15-NAR        TO T-NARATIVE.
022500	     MOVE W15-REFER	 TO T-REF.
022520       MOVE G-AC           TO T-AC.
022530       MOVE 03             TO T-CODE.
015380	   IF W15-VAL > ZERO
015390         ADD W15-VAL       TO G-DT
015400     ELSE
015410         ADD W15-VAL       TO G-CR.
015420       MOVE W15-VAL        TO T-VAL
                                    WS-VALUE.
	     MOVE WS-BATCH	 TO T-BATCH.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
022560       ADD W15-VAL         TO G-BAL.
022610       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015270       PERFORM AY13 THRU AY59.
022630       MOVE W15-PAYEE      TO T-NARATIVE.
015510       MOVE 5              TO T-SUB.
015530       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015270       PERFORM AY13 THRU AY59.
022690       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.

022700 FA110.
022710       MOVE W15-ACCOUNT    TO R-ACC.
022720       MOVE G-NAME         TO R-ANME.
022730       MOVE W15-DATE       TO R-DTE2.
022740       MOVE W15-REFER      TO R-REF.
022750       MOVE W15-NAR        TO R-NAR.
015710	   IF W15-VAL > ZERO
015720         MOVE W15-VAL TO R-DBT
015730     ELSE
015740         MOVE W15-VAL TO R-CRD.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
022810	       PERFORM FA00B.
022820       MOVE SPACES         TO R-DET.

022830 FA115.
012750	     DISPLAY "Updating Bank account" AT 2520 WITH
012760		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
022840	     MOVE WS-KSTORE	 TO G-AC.
022860       PERFORM AY12 THRU AY59.
022880	     INITIALIZE T-REC.

023030 FA120.
      *
      *    ****    Switch date to CCYYMMDD format
      *
	     MOVE W15-CC	 TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
012680       MOVE W15-YMD        TO T-DATE.
023060       MOVE W15-NAR        TO T-NARATIVE.
023070	     MOVE W15-REFER	 TO T-REF.
023080       ADD 1               TO G-GLTRAN.
023100       MOVE G-AC           TO T-AC.
023110       MOVE 04             TO T-CODE.
023120       MULTIPLY -1 BY W15-VAL 
                                 GIVING W20-CREDIT.
015380	   IF W20-CREDIT > ZERO
015390	       ADD W20-CREDIT	 TO G-DT
015400     ELSE
015410	       ADD W20-CREDIT	 TO G-CR.
023150	     MOVE W20-CREDIT	 TO T-VAL.
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
023220       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015920       PERFORM AY13 THRU AY59.
023250       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.

023260 FA125.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
023310	       PERFORM FA00B.
023320       MOVE G-AC           TO R-ACC.
023330       MOVE G-NAME         TO R-ANME.
023340       MOVE W15-DATE       TO R-DTE2.
023350       MOVE W15-REFER      TO R-REF.
023360       MOVE W15-PAYEE      TO R-NAR.
015710	   IF W20-CREDIT > ZERO
015720	       MOVE W20-CREDIT	 TO R-DBT
015730     ELSE
015740	       MOVE W20-CREDIT	 TO R-CRD.
023400	     PERFORM AY70 THRU AY999.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
023460	       PERFORM FA00B.
023470	     MOVE SPACES	 TO R-DET.
	   IF WS-CHQ-PRN = "N"
	       GO TO FA130.

       FA126.
	     MOVE "Print the cheque (Y/N)?  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
023600	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO FA126.
	   IF WS-OPTION = "N"
	       GO TO FA130.
	     MOVE W15-PAYEE	 TO W300-PAYEE.
	     MOVE W15-GRND	 TO W300-CHQDTE.
	     MOVE W15-NAR	 TO W300-NAR.
	     MOVE W15-CHQVAL	 TO W300-CHQVAL.
	     PERFORM SAVE-SCREEN-3.
	     CALL "UTP\CHQPRN"
		  USING LS-PARID LS-USER-ID W300-CHEQUE
		  ON EXCEPTION
		      GO TO FA127
	     END-CALL.
	     CANCEL "UTP\CHQPRN".
	     GO TO FA128.

       FA127.
	     MOVE SPACE 	 TO WS-ERR-MES.
	     STRING "Program- " DELIMITED SIZE
		     "UTP\CHQPRN" DELIMITED SIZE
		     " not on disk, press ANY key"
		     DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.

       FA128.
	     PERFORM RESTORE-SCREEN-3.

023490 FA130.
029700	     MOVE "ENTER to continue - E to exit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
023600	   IF NOT (WS-OPTION = "E")
023610         GO TO FA00.
023620       MOVE "BATCH TOTAL"  TO R-NAR.
023630       MOVE W20-TOTAL      TO R-DBT.
023640       MULTIPLY -1 BY W20-TOTAL 
                                 GIVING R-CRD.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
016830	     MOVE SPACES	 TO R-DET.

023690 FA999.
023700       EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
023710 ZA000	     SECTION 90.
023720 ZA00.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
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
      *    ****   ( A / P )   C R E D I T O R	M E M O	  F I L E   ****
      *    *************************************************************
	     MOVE "CRDMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRDMEM.
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
      *    *************************************************************
      *    ( A / P )   C R E D I T O R	 T R A N S A C T I O N	 F I L E
      *    *************************************************************
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
      *    *************************************************************
      *    *** G E N E R A L   L E D G E R   A C C O U N T   F I L E ***
      *    *************************************************************
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
      *    *************************************************************
      *    G E N E R A L   L E D G E R	 T R A N S A C T I O N	 F I L E
      *    *************************************************************
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
      *    *************************************************************
      *    I N T E G R A T I O N   F I L E   (C R E D I T O R S   G / L)
      *    *************************************************************
	     MOVE "LEDTRG"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
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
      *    *************************************************************
      *    ****       V A L U E	  A D D E D   T A X   F I L E	    ****
      *    *************************************************************
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
023770	     MOVE 3		 TO WS-PARKEY.
023780       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *
      *    ****    C H E Q U E	 P R I N T I N G   ?
      *
	   IF PAR-CHEQUE-PRN = "Y"
	       MOVE PAR-CHEQUE-PRN TO WS-CHQ-PRN.

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
024050	     OPEN OUTPUT RECOVER.
024060	     CLOSE RECOVER.
024070	     OPEN I-O RECOVER.
042360       MOVE 1              TO WS-PARKEY.
042370       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042380       MOVE PAR-COMPANY    TO W90-COMP.
042390       MOVE PAR-DMY        TO W05-TODAY.
042400       MOVE PAR-YMD        TO W05-TYMD.
	     MOVE 6		 TO WS-PARKEY.
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
036050	       DISPLAY "Parameters not set up - ENTER" AT 2020
036060			WITH FOREGROUND-COLOR 14
036070	       ACCEPT WS-OPTION AT 2050 WITH FOREGROUND-COLOR 15 AUTO
036080	       MOVE 1		 TO WS-ERROR.
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
	   IF WS-L-END = ZERO
	       SUBTRACT 1 FROM W05-CY
				 GIVING WS-L-END.
	   IF G-OPEN = ZERO
	       MOVE WS-Y-END	 TO G-OPEN.
	     MOVE G-OPEN	 TO WS-OPEN.
024380	     GO TO ZA999.

       COPY ZA49.PRO.

      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MD-LNE WS-MID-LNE.
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
				    WS-MCR  (80).
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

       COPY LOCKED.PRO.

       ZA205.
             EXIT PROGRAM.

024470 ZA999.
024480       EXIT.
      /
024490 ZB000	     SECTION 91.

       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT   TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
               MOVE P-NUMBER     TO WS-KEYX
041680     ELSE
041690     IF WS-F-ERROR = 4
041700         MOVE W02-CRTRAN   TO WS-FILE
041710	       MOVE ZERO	 TO WS-KEY
	       MOVE CT-KEY	 TO WS-KEYX
041840	   ELSE
041810     IF WS-F-ERROR = 8
041820         MOVE W02-GACCNT   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE G-AC	TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 11
041860         MOVE W02-GLTRAN   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER  TO WS-FILE
041870         MOVE WS-RECKEY    TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 24
041860	       MOVE W02-CRDMEM	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CME-KEY	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX.

       COPY DISPERR.PRO.
