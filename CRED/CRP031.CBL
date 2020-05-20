      $set LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	   ******      **      *
      *   **	**  **	  **  **    **	 **  **   **	**    ***      *
      *   **	    **	  **  **    **	**    **	**     **      *
      *   **	    ******    *******	**    **     ****      **      *
      *   **	    **	 **   **	**    **	**     **      *
      *   **	**  **	  **  **	 **  **   **	**     **      *
      *    ******   **	  **  **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *        C R E D I T O R / G L   T R A N S A C T I O N S         *
      *                                                                *
      * 		       I N V O I C E			       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       CRP031.
000030 AUTHOR.		       James William Lemmon.
000040 DATE-WRITTEN.	       MARCH 1983.

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
		   PARTICULAR PURPOSE. See the GNU General Public License
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

000160 COPY DEPART.SL.

000200 COPY GACCNT.SL.

000220 COPY GLTRAN.SL.

000260 COPY GLJRN.SL.

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

000160 COPY DEPART.FDE.

000400 COPY GACCNT.FD.

000420 COPY GLTRAN.FD.

000460 COPY GLJRN.FD.

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
002130 77  WS-KSTORE	   PIC	9(04)	 COMP-5.
001760 77  WS-SKSTORE	   PIC	9(06)	 COMP-5.
002140 77  WS-KEYSTR	   PIC	9(04)	 COMP-5.
002150 77  WS-GLJSTR       PIC  9(04)    COMP-5.
002160 77  WS-GLJKEY       PIC  9(04)    COMP-5.
002170 77  WS-RECKEY	   PIC	9(04)	 COMP-5.
002180 77  WS-RECOVER	   PIC	9(04)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002190 77  WS-TRANS	   PIC	9(04)	 COMP-5 VALUE 1.
002200 77  WS-ISUB	   PIC	9(04)	 COMP-5.
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
002280 77  WS-S9	   PIC	9(04)	 COMP-5.
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
002480 77  WS-SCRED	   PIC	X(09) VALUE "Creditor".
002530 77  WS-OPTION	   PIC	X(01).
001970 77  WS-SKIP         PIC  X(01) VALUE "N".
002780 77  WS-LOOK	   PIC	X(01) VALUE "C".
002540 77  WS-FIRST	   PIC	9(02) VALUE 0.
002550 77  WS-LAST         PIC  9(02) VALUE 0.
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
	   03  WS-PROG	       PIC  X(03) VALUE "031".

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

000160 COPY DEPART.ID.

000400 COPY GACCNT.ID.

000420 COPY GLTRAN.ID.

000460 COPY GLJRN.ID.

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
	       05  R-DET	   PIC X(140).

000540	   03  R-L2 REDEFINES W02-PRN-LINE.
000550	       05  R-H1        PIC X(07).
000560	       05  R-DTE1      PIC Z9/99/9999.
000570	       05  FILLER      PIC X(31).
000580	       05  R-H2        PIC X(40).
000590	       05  FILLER      PIC X(33).
000600	       05  R-H3        PIC X(08).
000610	       05  R-PGE       PIC ZZ9.

000620	   03  R-L2A REDEFINES W02-PRN-LINE.
000630	       05  FILLER      PIC X(56).
000640	       05  R-HEAD      PIC X(20).
000650	       05  FILLER      PIC X(56).

000660	   03  R-L3A REDEFINES W02-PRN-LINE.
000670	       05  FILLER      PIC X(05).
000680	       05  I-H1        PIC X(11).
000690	       05  I-H2        PIC X(34).
000700	       05  I-H3        PIC X(44).
000710	       05  I-H4        PIC X(11).
000720	       05  I-H5        PIC X(27).

000730	   03  R-L4 REDEFINES W02-PRN-LINE.
000740	       05  G-H1        PIC X(10).
000750	       05  G-SUPP      PIC X(06).
000760	       05  FILLER      PIC X(01).
000770	       05  G-NAMER     PIC X(40).
000780	       05  FILLER      PIC X(23).

000790	   03  R-L4A REDEFINES W02-PRN-LINE.
000800	       05  G-H3        PIC X(05).
000810	       05  G-INVN      PIC X(09).
000820	       05  FILLER      PIC X(01).
000830	       05  G-H4        PIC X(06).
000850	       05  FILLER      PIC X(01).
000840	       05  G-INVD      PIC Z9/99/9999.
000860	       05  G-H5        PIC X(06).
000870	       05  G-RAIL      PIC Z(04)9.99-.
000880	       05  FILLER      PIC X(01).
000890	       05  G-H6        PIC X(06).
000900	       05  G-DISC      PIC Z(04)9.99-.
000910	       05  FILLER      PIC X(01).
000920	       05  G-H7        PIC X(06).
000930	       05  G-TOTAL     PIC Z(06)9.99-.

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

001430	   03  R-L7 REDEFINES W02-PRN-LINE.
001440	       05  CH-H1       PIC X(07).
001450	       05  CH-H2       PIC X(32).
001460	       05  CH-H3       PIC X(08).
001470	       05  CH-H4       PIC X(13).
001480	       05  CH-H5       PIC X(08).
001490	       05  CH-H6       PIC X(12).

001500	   03  R-L7A REDEFINES W02-PRN-LINE.
001510	       05  CH-ACNO     PIC X(06).
001520	       05  FILLER      PIC X(01).
001530	       05  CH-NAME     PIC X(28).
001540	       05  FILLER      PIC X(01).
001550	       05  CH-DATE     PIC Z9/99/9999.
001560	       05  FILLER      PIC X(01).
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
001680	       05  FILLER      PIC X(03).
001690	       05  R-H5        PIC X(11).
001700	       05  R-H6        PIC X(05).
001710	       05  FILLER      PIC X(01).
001720	       05  R-H7        PIC X(14).
001730	       05  R-H8        PIC X(05).
001740	       05  FILLER      PIC X(05).
001750	       05  R-H9        PIC X(09).
001760	       05  R-H10       PIC X(12).
001770	       05  R-H11       PIC X(13).
001780	       05  R-H12       PIC X(10).
001790	       05  R-H13       PIC X(13).
001800	       05  FILLER      PIC X(08).
001810	       05  R-H14       PIC X(14).
001820	       05  R-H15       PIC X(09).

001830	   03  R-L9 REDEFINES W02-PRN-LINE.
001840	       05  FILLER      PIC X(03).
001850	       05  R-ACC       PIC 9(04)/99.
001860	       05  FILLER      PIC X(04).
001870	       05  R-ANME      PIC X(24).
001880	       05  FILLER      PIC X(03).
001890	       05  R-DTE2      PIC Z9/99/9999.
001900	       05  FILLER      PIC X(03).
001910	       05  R-REF       PIC X(07).
001920	       05  FILLER      PIC X(04).
001930	       05  R-NAR       PIC X(36).
001940	       05  FILLER      PIC X(03).
001950	       05  R-DBT       PIC Z(08)9.99.
001960	       05  FILLER      PIC X(02).
001970	       05  R-CRD       PIC Z(08)9.99-.
001980	       05  FILLER      PIC X(01).

001990	   03  R-L10 REDEFINES W02-PRN-LINE.
002000	       05  FILLER      PIC X(65).
002010	       05  R-H16       PIC X(06).
002020	       05  FILLER      PIC X(01).
002030	       05  R-H17       PIC X(23).
002040	       05  R-H18       PIC X(06).
002050	       05  FILLER      PIC X(31).

000770	   03  R2-L2 REDEFINES W02-PRN-LINE.
000780	       05  R2-H1       PIC X(06).
000790	       05  R2-DTE1     PIC Z9/99/9999.
000800	       05  FILLER      PIC X(08).
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
000790	       05  FILLER      PIC  X(03).
000800	       05  R2-DTE2     PIC  Z9/99/9999.
000810	       05  FILLER      PIC  X(03).
000820	       05  R2-REF      PIC  X(07).
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

       COPY W05.WS.

       COPY W05.VAT.

003250 01  W10-H2.
003260     03  FILLER          PIC X(27).
003270     03  W10-HEAD        PIC X(13).

       COPY W15.CGL.

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
004080     03  W50-HEAD        PIC X(20).

004090 01  W55-DEL.
004100     03  W55-MNTH        PIC X(03).
004110     03  FILLER          PIC X(01)   VALUE ",".
004120     03  W55-YY          PIC 9(02).

004130 01  W60-H2.
004140     03  W60-DESC        PIC X(21)   VALUE
004150                             "CREDITORS AUDIT TRIAL".

004160 01  W65-CREDITOR.
004170     03  W65-CFLOW OCCURS 6 PIC 9(07)V99 COMP-3.

004180 01  W70-ACTS.
004190     03  W70-BRANCH      PIC 9(04)     COMP-5.

004370 COPY W70CRED.WS.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
	   03  W75-CNO	      PIC X(06) OCCURS 18.
	   03  W75-GNO	      PIC 9(06) COMP-5 OCCURS 18.

004410 01  W90-COMP.
004420     03  W90-CNAME       PIC X(40).

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

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004430 SCREEN SECTION.

       COPY BLANK.CRT.

004710 01  S03.
004720	   03  LINE  2 COLUMN 26 FOREGROUND-COLOR 7 HIGHLIGHT
004730                           VALUE "CREDITORS INVOICE".
004740	   03  LINE  4 COLUMN  4 PIC  X(09) USING WS-SCRED.
004750	   03  LINE  5 COLUMN  4 VALUE "Invoice no:".
004760	   03	       COLUMN 56 VALUE "Invoice date:".
006650	   03  LINE  6 COLUMN  4 VALUE "Allocation - 30:".
	   03	       COLUMN 24 VALUE "60:".
	   03	       COLUMN 31 VALUE "90:".
	   03	       COLUMN 38 VALUE "120:".
	   03	       COLUMN 46 VALUE "150:".
	   03	       COLUMN 54 VALUE "180:".
004770	   03  LINE  7 COLUMN  4 VALUE "G/L Account".
004780     03          COLUMN 18 VALUE "Narrative".
004790     03          COLUMN 69 VALUE "Value".
           03  LINE 20 COLUMN 54 VALUE "Sub total :".
           03  LINE 21 COLUMN 54 VALUE "VAT       :".
006670     03  LINE 22 COLUMN 54 VALUE "Totals    :".

005030 01  S06.
005040	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC X(06) USING W15-ACNO AUTO.

005050 01  S07.
005060	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC X(08) USING W15-INVN AUTO.

005070 01  S08.
005080	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC Z9/99/9999 USING W15-INVD AUTO.

005090 01  S09.
005100	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC 9(06) USING W15-ACCOUNT AUTO.

005110 01  S10.
005120	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC Z(07)9.99- USING W15-COST AUTO.

       01  S18.
	   03  LINE  6 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC 9(01) USING W15-30 AUTO.
	   03	       COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC 9(01) USING W15-60 AUTO.
	   03	       COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC 9(01) USING W15-90 AUTO.
	   03	       COLUMN 43 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC 9(01) USING W15-120 AUTO.
	   03	       COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC 9(01) USING W15-150 AUTO.
	   03	       COLUMN 59 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC 9(01) USING W15-180 AUTO.

       01  S43.
007540	   03  FOREGROUND-COLOR 3 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC Z(07)9.99DB USING W15-VAT AUTO.

       01  S44.
007540	   03  FOREGROUND-COLOR 3 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC Z(07)9.99DB USING W15-TCOST AUTO.

       01  S45.
007540	   03  FOREGROUND-COLOR 3 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC Z(07)9.99DB
                                       USING W15-TRNTOT AUTO.
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
	     PERFORM DA000.
	   IF WS-XFRC = "Y"
	       PERFORM GA000.

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
      *    *************************************************************
      *    ****    Print the audit trail HEADING
      *    *************************************************************
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

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD2.

       COPY GLTRAN.RD.

       COPY GLJRN.RD.

       COPY LEDTRF.RD1.

       COPY SHARED.RD.

000200 COPY TXTRAN.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
007740 AC000	       SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY DEPART.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

       COPY CRTRAN.WR.

       COPY GLJRN.WR.

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

      /    *************************************************************
      *      THIS ROUTINE UPDATES THE VARIOUS MONTHS FROM TRANSACTION
      *      DATE UP TO THE CURRENT MONTH
      *    *************************************************************
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

012090 AY38.
012100       MOVE 38             TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
012120	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
012130       MOVE DPT-RECORD     TO REC-DEPART.
012140       GO TO AY50.

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
012010       MOVE 2              TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
	     MOVE NET-CREDITOR	 TO W70-CREDCON.
012010       MOVE 4              TO WS-NETKEY.
012700       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012710       PERFORM AY39 THRU AY59.
012740	     GO TO AY999.
      *    *************************************************************
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *    *************************************************************
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *    *************************************************************
      *    ****   A R E   C R E D I T O R   O R	  G / L   F I L E S
      * 	  B E I N G   U P D A T E D
      *    *************************************************************
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG" OR "GL"
      *    *************************************************************
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *    *************************************************************
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *    *************************************************************
      *    ****   I S	T H I S   P R O G R A M   I N	T H E	Q
      *    *************************************************************
	       IF PAR-USR(WS-SUB) = LS-USER
      *    *************************************************************
      *    ** I S   I T   N E X T   I N   L I N E   T O   P R O C E S S
      *    *************************************************************
		   IF WS-WAIT = ZERO
		       GO TO AY63
		   ELSE
		       GO TO AY62-WAIT
		   END-IF
	       ELSE
		   GO TO AY62-WAIT
	       END-IF
	   END-IF.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 P U T	 T H I S   P R O G R A M   I N	 T H E	 Q     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *    *************************************************************
      *    ****   S E T   W A I T   P E R I O D
      *    *************************************************************
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
011300       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
011310       MOVE G-BATCH        TO WS-BATCH.
011320     IF G-BATCH = 99999
011330         MOVE 1            TO G-BATCH
011340     ELSE
011350         ADD 1             TO G-BATCH.
011360       PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
011380       GO TO AY999.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *    *************************************************************
      *    ****    Write links back and unlock PARAM record 4 and
      * 	   NETWORK record 2.
      *    *************************************************************
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
010930	     MOVE 1		 TO WS-ERROR.
	     GO TO BA999.
      *    *************************************************************
      *    ****    CALCULATE THE PERIOD INTO WHICH THIS DATE FALLS
      *    *************************************************************
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
       CA700         SECTION.
       CA701.
             MOVE 3              TO TAX-ACTYPE.
             MOVE CT-CODE        TO TAX-CODE.
             MOVE W20-DTE        TO TAX-DATE.
             MOVE CT-REF         TO TAX-REF.
             MULTIPLY -1 BY CT-COST
                                 GIVING TAX-VALUE.
             MULTIPLY -1 BY CT-TAXFREE
                                 GIVING TAX-TAXFREE.
	     MOVE CT-CRED	 TO TAX-AC.
	   IF W00-C-ACT = 1
               IF CT-CODE = 03
                   MOVE "3"      TO TAX-TYPE
               ELSE
                   MOVE "2"      TO TAX-TYPE
           ELSE
               MOVE "1"          TO TAX-TYPE.
	     MOVE ZERO		 TO TAX-SEQ.
	     MOVE 1		 TO WS-S9.

       CA705.
	     MULTIPLY -1 BY W15-TAXVAL(WS-S9)
				 GIVING TAX-VAL(WS-S9).
	     MULTIPLY -1 BY W15-VATVAL(WS-S9)
				 GIVING TAX-VAT(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO CA705.
000890	     PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
000900	     MOVE 1		 TO WS-ACTION.
000910	     PERFORM AY19 THRU AY59.

       CA799.
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

      /
010960 DA000	     SECTION 50.
010970 DA00A.
010980	     MOVE "GOODS/SERV RECEIVED"
                                 TO W50-HEAD.
      *    *************************************************************
      *    ****   C L E A R   T O T A L S   T O	  Z E R O
      *    *************************************************************
011010	     MOVE ZERO		 TO W20-DEBIT
				    W20-VAT.
011750	     MOVE SPACES	 TO W15-NAR2.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Audit trail headings.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011020 DA00B.
011030       PERFORM AA50 THRU AA60.
011040       MOVE "ACC NO"       TO I-H1.
011050       MOVE "G/LEDGER ACCOUNT" 
                                 TO I-H2.
011060       MOVE "NARRATIVE"    TO I-H3.
011070       MOVE "VALUE"        TO I-H4.
011080       MOVE "30  60  90 120 150 180" 
                                 TO I-H5.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011100	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

011110 DA00.
011120       DISPLAY CLR-SCREEN.
011130       DISPLAY S03.
011140       MOVE SPACES         TO W15-ACNO.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Allow user to key in the Creditor account number.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011150 DA05.
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
011160	     ACCEPT S06 AT 0416.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACE	 TO W15-ACNO
		     GO TO DA115
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA20
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA05
	       END-EVALUATE
	       DISPLAY S06 AT 0416
               IF W15-ACNO = SPACES
015990             GO TO DA05.
011170	   IF W15-ACNO = SPACES
011180         GO TO DA115.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
011200	     PERFORM AE155-GET-CREDITOR.
011210     IF WS-ERROR NOT = ZERO
011220	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011230         GO TO DA05.
011240	     MOVE P-NAME	 TO W15-NAME.
011250	     DISPLAY P-NAME AT 0423
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             DISPLAY S-CRED.
012180	     PERFORM CHECK-CORRECT.
011330	   IF WS-OPTION = "N"
011340         GO TO DA05.
      *    *************************************************************
      *    ****   I N I T I A L I Z E	W O R K   V A R I A B L E S
      *    *************************************************************
	     MOVE SPACES	 TO W15-GRNN W15-INVN.
038410	     MOVE ZERO		 TO W15-INVD W15-COST.
038420       MOVE ZERO           TO W65-CFLOW (1) W65-CFLOW (2)
                                    W65-CFLOW (3) W65-CFLOW (4) 
				    W65-CFLOW (5) W65-CFLOW (6).
	     MOVE 1		 TO WS-S9.

       DA06.
018660	     MOVE ZERO		 TO W15-VATVAL(WS-S9) W15-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO DA06.
011390	     MOVE P-NUMBER	 TO W15-REFER.

011400 DA10.
011410	     ACCEPT S07 AT 0516.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA05
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA10
	       END-EVALUATE.
011420	   IF W15-INVN = SPACES
011430         GO TO DA10.

011440 DA13.
011450	     ACCEPT S08 AT 0570.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA10
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA13
	       END-EVALUATE.
011460	     MOVE W15-INVD	 TO W15-DATE.
011470       PERFORM BA000.
011480	   IF WS-ERROR = 1
011490         GO TO DA13.
	   IF WS-PERIOD > 12
	       PERFORM EA000
	       IF WS-ERROR = 1
		   GO TO DA13.
012180	     PERFORM CHECK-CORRECT.
011570	   IF WS-OPTION = "N"
011580         GO TO DA05.
      *    *************************************************************
      *    ****    Switch date to CCYYMMDD format
      *    *************************************************************
	     MOVE W15-DD	 TO W20-DD.
             MOVE W15-MM         TO W20-MM.
	     MOVE W15-CC	 TO W20-CC.
             MOVE W15-YY         TO W20-YY.
      *    *************************************************************
      *    ****  NB.  AMEND TO ALLOW FOR MULTIPLE VAT RATES!!!
      *    *************************************************************
	   IF W20-DTE < WS-VAT-DATE(1)
	       MOVE 7		 TO WS-VAT-SUB
	   ELSE
	       MOVE 1		 TO WS-VAT-SUB.
011590	     MOVE P-TERMS	 TO W15-SETT.
011600       MOVE W05-TODAY      TO W15-GRND.
011620       MOVE ZERO           TO W15-TCOST W15-RAIL W15-DISCOUNT.
             MOVE ZERO           TO W15-VAL W15-TXBL W15-TRNTOT
				    W15-VAT W15-EXCL W15-NTXBL
				    W15-LVAT.
             MOVE 7              TO LIN.
      *    *************************************************************
      *    ****    A G E   A N A L Y S I S
      *    *************************************************************
             ACCEPT S18.
011720	     MOVE 0		 TO WS-LAST.

011730 DA14.
011740       MOVE ZERO           TO W15-ACCOUNT W15-COST.
	     MOVE W15-NAR2	 TO W15-NAR.
011760	     ADD 1		 TO LIN WS-LAST.

011770 DA15.
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
011780	     ACCEPT S09 AT LINE LIN COLUMN 6.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ACCOUNT
		     GO TO DA16
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA20G THRU AA20
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA15
	       END-EVALUATE
016570	       DISPLAY S09 AT LINE LIN COLUMN 6
016580	       IF W15-ACCOUNT = ZERO
016590		   GO TO DA15.

       DA16.
011790     IF W15-ACCOUNT = ZERO
011800	       SUBTRACT 1	 FROM WS-LAST LIN
011810         GO TO DA37.
011820     IF W15-ASER = ZERO
011830	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011840         GO TO DA15.

011850 DA20.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
011870	   IF WS-F-ERROR = 8
011880	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011890         GO TO DA15.

011900 DA30.
	     DISPLAY WS-BLNK78 AT 2302
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
011920	     DISPLAY G-NAME AT 2306
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

011930 DA32.
012180	     PERFORM CHECK-CORRECT.
012020	   IF WS-OPTION = "N"
012030         GO TO DA15.

012040 DA35.
012050	     ACCEPT W15-NAR AT LINE LIN COLUMN 18
012060		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     MOVE W15-NAR	 TO W15-NAR2.
012070       ACCEPT S10 AT LINE LIN COLUMN 62.
012080       MOVE W15-COST       TO W15-VAL.
012090	   IF W15-VAL = ZERO
012100	       MOVE WS-ER4	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
012110         GO TO DA35.
012180	     PERFORM CHECK-CORRECT.
012200	   IF WS-OPTION = "N"
012210         GO TO DA15.

       DA36.
	     MOVE "Taxable Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
033910	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO DA36.
	     MOVE ZERO		 TO W15-VAT.
           IF WS-OPTION = "N"
               ADD W15-VAL       TO W15-NTXBL
               MOVE W15-VAL      TO GLJ-VAL
               GO TO DA36B.
       DA36A.
	     MOVE "'E'xclusive or 'I'nclusive  [E]"
				 TO WS-ERR-MES.
             MOVE "E"            TO WS-OPTION.
033910	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "E" OR "I")
               GO TO DA36A.
	     MOVE W05-VAT(WS-VAT-SUB)
				 TO W05-RATE.
	   IF WS-OPTION = "E"
	       ADD W15-VAL	 TO W15-TXBL
	       ADD W15-COST	 TO W15-TAXVAL(WS-VAT-SUB)
               MOVE W15-VAL      TO GLJ-VAL
	       COMPUTE W15-VAT ROUNDED = (W15-VAL * W05-RTE)
	   ELSE
	       COMPUTE W15-VAT ROUNDED =
                   ((W15-VAL / (100.00000 + W05-RATE)) * W05-RATE)
	       COMPUTE GLJ-VAL = W15-VAL - W15-VAT
	       COMPUTE W15-TXBL = W15-TXBL + (W15-VAL - W15-VAT)
	       ADD GLJ-VAL	 TO W15-TAXVAL(WS-VAT-SUB).
	     ADD W15-VAT	 TO W15-VATVAL(WS-VAT-SUB)
				    W15-LVAT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Write details to the G/L transaction work file.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       DA36B.
012220       MOVE WS-LAST        TO WS-GLJKEY.
012230       MOVE W15-ACCOUNT    TO GLJ-AC.       
012250       MOVE W15-NAR        TO GLJ-NAR.
	     MOVE W15-LVAT	 TO W15-VAT.
012270       PERFORM WRITE-GLJRN THRU WRITE-GLJRN-EXIT.
	     ADD W15-TXBL W15-NTXBL
                                 GIVING W15-TCOST.
             ADD W15-TCOST W15-VAT
                                 GIVING W15-TRNTOT.             
      *
      *      ****    C O S T
      *
012300       DISPLAY S44 AT 2066.
      *
      *      ****    V A T
      *
012300       DISPLAY S43 AT 2166.
      *
      *      ****    T O T A L
      *
012300       DISPLAY S45 AT 2266.
012310     IF LIN < 19
012320         GO TO DA14.
012330       MOVE 0801           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.
012390	     MOVE 7		 TO LIN.
012400       GO TO DA14.    

012410 DA37.
012420     IF WS-LAST = ZERO
012430         GO TO DA115.
012600	     DISPLAY CLEAR-L25.

012440 DA37X.
012180	     PERFORM CHECK-CORRECT.
012560	   IF WS-OPTION = "N"
012570         GO TO DA14.

012630 DA37A.
012610	     MOVE "'P'rocess Invoice or 'C'ancel  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
033910	     PERFORM OPT-MESSAGE.
012700	   IF NOT (WS-OPTION = "P" OR "C")
012710         GO TO DA37A.
012730	   IF WS-OPTION = "C"
012740         GO TO DA115.
	   IF W15-LVAT NOT = ZERO
	       ADD 1		 TO LIN
               PERFORM DA50 THRU DA53.
012750	     DISPLAY "Processing Invoice" AT 2520 WITH
012760		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
012770       MOVE 1              TO WS-GLJKEY.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Start processing transaction - Lock the control records. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012780	     PERFORM AY60 THRU AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Read the Creditor account record, write it to the recovery³
      *    ³ file and lock the record.				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 0		 TO WS-ACTION.
012800       PERFORM AY07 THRU AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Loop through the G/L transaction work file and update.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012810 DA38.
012820     IF WS-GLJKEY > WS-LAST
012830         GO TO DA55.
012840       PERFORM READ-GLJRN THRU READ-GLJRN-EXIT.
012850	     MOVE GLJ-AC	 TO W15-ACCOUNT G-AC.
012870       MOVE GLJ-NAR        TO W15-NAR.
012880       MOVE GLJ-VAL        TO W15-VAL.
012900       PERFORM AY12 THRU AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Generate the G/L transaction record.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012920	     INITIALIZE T-REC.
      *    *************************************************************
      *    ****    Switch date to CCYYMMDD format
      *    *************************************************************
013040 DA40.
	     MOVE W15-CC	 TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
012680       MOVE W15-YMD        TO T-DATE.
013070       MOVE W15-NAR        TO T-NARATIVE.
013080	     MOVE W15-INVN	 TO T-REFER.
013090       MOVE W15-VAL        TO T-VAL.
013120       MOVE G-AC           TO T-AC.
013130       ADD W15-VAL         TO W20-DEBIT.
013140       ADD W15-VAL         TO G-DT.
013150       MOVE 01             TO T-CODE.
             MOVE W15-VAL        TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
013160       ADD W15-VAL         TO G-BAL.
013180       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
012550       PERFORM AY13 THRU AY59.
013190       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
013210 DA45.
013220	     MOVE W15-ACCOUNT	 TO I-ACNO.
013230	     MOVE G-NAME	 TO I-NAME.
013240	     MOVE W15-NAR	 TO I-NAR.
013250	     MOVE W15-VAL	 TO I-VALUE.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
013300	       PERFORM DA00B.
013310	     MOVE SPACES	 TO R-DET.
013320	     ADD 1		 TO WS-GLJKEY.
013330       GO TO DA38.
      *    *************************************************************
      *    ****    VAT Control
      * 			   MULTIPLE RATE TO BE INCLUDED
      *    *************************************************************
       DA50.
	     MOVE "VAT1"	 TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-I-VAT-GL	 TO W15-ACCOUNT.
	     SUBTRACT W15-LVAT	 FROM W15-TRNTOT.

       DA52.
	     DISPLAY "Key in the VAT G/L account number" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 " "
		     "F1" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011780	     ACCEPT S09 AT LINE LIN COLUMN 6.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA20G THRU AA20
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA52
	       END-EVALUATE
016570	       DISPLAY S09 AT LINE LIN COLUMN 6
016580	       IF W15-ACCOUNT = ZERO
016590		   GO TO DA52.
	     DISPLAY CLEAR-L25.
011790	   IF W15-ACCOUNT = ZERO
011810         GO TO DA52.
011820     IF W15-ASER = ZERO
011830	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011840         GO TO DA52.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
011870	   IF WS-F-ERROR = 8
011880	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
011890         GO TO DA52.
011920	     DISPLAY G-NAME AT 2306
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
012180	     PERFORM CHECK-CORRECT.
012020	   IF WS-OPTION = "N"
012030         GO TO DA52.
             MOVE P-NAME         TO W15-NAR.
012050	     ACCEPT W15-NAR AT LINE LIN COLUMN 18
012060		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
012300       ACCEPT S43 AT 2166.
012180	     PERFORM CHECK-CORRECT.
012020	   IF WS-OPTION = "N"
012030         GO TO DA52.
012280       ADD 1               TO WS-LAST.
012220       MOVE WS-LAST        TO WS-GLJKEY.
012230       MOVE W15-ACCOUNT    TO GLJ-AC.       
012250       MOVE W15-NAR        TO GLJ-NAR.
012260	     MOVE W15-LVAT	 TO GLJ-VAL.
012270       PERFORM WRITE-GLJRN THRU WRITE-GLJRN-EXIT.
	     ADD W15-LVAT	 TO W15-TRNTOT.
       DA53.
             EXIT.

013340 DA55.
013350       MOVE W15-GRND       TO W15-DATE.
013360       MOVE W15-TRNTOT     TO W20-RESULT.
013370     IF W20-RESULT = ZERO
013380         GO TO DA80.
013390       MOVE ZERO           TO WS-S.
013400       MOVE 1              TO WS-S2.

013410 DA60.
013420       MOVE ZERO           TO W25-CFLOW (WS-S2).
013430     IF W15-MONTH (WS-S2) > 0
013440         ADD 1             TO WS-S.
013450     IF WS-S2 < 6
013460         ADD 1             TO WS-S2
013470         GO TO DA60.
013480       MOVE ZERO           TO W25-V1 W25-V2.
013490       MOVE 1              TO WS-S3.
013500	   IF WS-S = ZERO
	       MOVE 1		 TO WS-S
	       GO TO DA68.
      *				    W15-30.
013520     IF WS-S = 1
013530         MOVE W20-RESULT   TO W25-V2
013540         GO TO DA65.
013550       DIVIDE W20-RESULT BY WS-S 
                                 GIVING W25-V1 ROUNDED.
013560       SUBTRACT 1          FROM WS-S.
013570       COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
013580       ADD 1               TO WS-S.

013590 DA65.
013600     IF W15-MONTH (WS-S3) = ZERO
013610         ADD 1             TO WS-S3
013620         GO TO DA65.
013630     IF WS-S > 1
013640         MOVE W25-V1       TO W25-CFLOW (WS-S3)
013650         SUBTRACT 1        FROM WS-S
013660         ADD 1             TO WS-S3
013670         GO TO DA65.
013680       MOVE W25-V2         TO W25-CFLOW (WS-S3).

       DA68.
013690       MOVE 1              TO WS-S2.

013700 DA70.
013710     IF W25-CFLOW (WS-S2) = ZERO
013720         GO TO DA75.
013730       ADD W05-MM WS-S2    GIVING WS-S3.
013740     IF WS-S3 > 12
013750         SUBTRACT 12       FROM WS-S3.
013760 DA75.
013770     IF WS-S2 < 6
013780         ADD 1             TO WS-S2
013790         GO TO DA70.

013800 DA80.
013810       MOVE SPACES         TO R-DET.

013820 DA95.
013830	     DISPLAY CLEAR-L25.
      *
      *    ****   C R E D I T O R   T R A N S A C T I O N
      *
013950 DA100.
	     INITIALIZE CT-REC.
014090	     MOVE W15-GRND	 TO W15-DEL.
	     MOVE W15-DEL-DD	 TO W21-DD.
	     MOVE W15-DEL-MM	 TO W21-MM.
	     MOVE W15-DEL-CC	 TO W21-CC.
	     MOVE W15-DEL-YY	 TO W21-YY.
040840	     MOVE W21-DTE	 TO CT-DTE.
	   IF W15-GRNN NOT = SPACES
014100         MOVE W15-GRNN     TO CT-REF
           ELSE
014100         MOVE W15-INVN     TO CT-REF.
014110       MOVE W15-INVN       TO CT-SINV.
014120	     MOVE W15-INVD	 TO W15-DEL.
	     MOVE W15-DEL-DD	 TO W21-DD.
	     MOVE W15-DEL-MM	 TO W21-MM.
	     MOVE W15-DEL-CC	 TO W21-CC.
	     MOVE W15-DEL-YY	 TO W21-YY.
	     MOVE W21-DTE	 TO CT-SDTE.
014130       MOVE W15-TRNTOT     TO CT-COST.
	     MOVE W15-LVAT	 TO CT-TAX.
             MOVE W15-NTXBL      TO CT-TAXFREE.
014140       MOVE P-NUMBER       TO CT-CRED.
014150       MOVE 11             TO CT-CODE.
014170       MOVE W15-TRNTOT     TO CT-SELL
014180                              W20-RESULT.
	     MOVE "U"		 TO CT-TYPE.
	     MOVE "I"		 TO CT-PJ.
029080	     MOVE ZERO		 TO CT-PAID
				    CT-SEQ.
	     ADD W15-LVAT	 TO W20-VAT.
      *    *************************************************************
      *    ****   TRANSACTION STATISTICS
      *    *************************************************************
             PERFORM CA600.
      *    *************************************************************
      *    ****   VAT REPORT
      *    *************************************************************
             PERFORM CA700.
           IF WX-INT = "Y"
               MOVE "Y"          TO WS-XFRC
               MOVE W20-DTE      TO WX-DATE
	       MOVE "  CG"	 TO WX-LEDG
	       MOVE W90-CODE	 TO WX-TRN
               MOVE W00-C-DESC   TO WX-DESC
               MOVE "- CREDITORS"
                                 TO WX-BOOK
               COMPUTE WX-VALUE = CT-COST * -1
               MOVE WX-CREGL     TO WX-AC
               MOVE 02           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
014190       MOVE ZERO           TO WS-S.
014200       MOVE 1              TO WS-S2.
014210	     PERFORM DA60 THRU DA68.
014220       MOVE 1              TO WS-S2.

014230 DA105.
014240       MOVE W25-CFLOW (WS-S2) 
                                 TO W65-CFLOW (WS-S2).
014250     IF WS-S2 < 6
014260         ADD 1             TO WS-S2
014270	       GO TO DA105.
014280       MOVE 1              TO WS-S WS-S2.
014290 DA110.
014300     IF W25-CFLOW (WS-S) NOT = ZERO
014310         ADD WS-S2         TO CT-AGE.
014320     IF WS-S < 6
014330         ADD 1             TO WS-S
014340         MULTIPLY 2        BY WS-S2
014350         GO TO DA110.
014360       ADD W15-TRNTOT      TO P-BAL.
014370       ADD W15-TRNTOT      TO P-CR.
014380       ADD W25-CFLOW (1)   TO P-30D.
014390       ADD W25-CFLOW (2)   TO P-60D.
014400       ADD W25-CFLOW (3)   TO P-90D.
014410       ADD W25-CFLOW (4)   TO P-120D.
014420       ADD W25-CFLOW (5)   TO P-150D.
014430       ADD W25-CFLOW (6)   TO P-180D.
	  IF CT-AGE = ZERO
	      ADD W15-TRNTOT	 TO P-OUT W70-DUE-NOW.
014440	     PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY08 THRU AY59.
014460	     PERFORM REWRITE-CREDIT THRU WRITE-CREDIT-EXIT.
      *
      *    ****   C O N T R O L   T O T A L S
      *
014540	     ADD W15-TRNTOT	 TO W70-CLOSE.
014550       ADD W15-TRNTOT      TO W70-CRED.
019120       ADD W25-CFLOW (6)   TO W70-DUE-180.
019130       ADD W25-CFLOW (5)   TO W70-DUE-150.
019140       ADD W25-CFLOW (4)   TO W70-DUE-120.
019150       ADD W25-CFLOW (3)   TO W70-DUE-90.
019160       ADD W25-CFLOW (2)   TO W70-DUE-60.
019170       ADD W25-CFLOW (1)   TO W70-DUE-30.
      *    *************************************************************
      *    ****    VAT Control 
      * 			   MULTIPLE RATE TO BE INCLUDED
      *    *************************************************************
	   IF W15-LVAT = ZERO
		GO TO DA112.
	     MOVE "VAT1"	 TO DPT-CODE.
             PERFORM AY38 THRU AY59.
	     ADD W15-VATVAL(1)	 TO DPT-I-VAT-DAY
				    DPT-I-VAT-MTD
				    DPT-I-VAT-YTD.
	     ADD W15-VATVAL(7)	 TO DPT-I-VAT-DAY
				    DPT-I-VAT-MTD
				    DPT-I-VAT-YTD.
	     ADD W15-TAXVAL(1)	 TO DPT-V-PUR-DAY
				    DPT-V-PUR-MTD
				    DPT-V-PUR-YTD.
	     ADD W15-TAXVAL(7)	 TO DPT-V-PUR-DAY
				    DPT-V-PUR-MTD
				    DPT-V-PUR-YTD.
      *	   IF WX-INT = "Y"
      *	       MOVE W15-LVAT	 TO WX-VALUE
      *	       MOVE DPT-I-VAT-GL TO WX-AC
      *	       MOVE 13		 TO WX-TYPE
      *	       PERFORM GL-TRF-UPDATE.
027880	     PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
      *    *************************************************************
      *    ****    End Recovery Logging.
      *    *************************************************************
       DA112.
014640       PERFORM AY70 THRU AY999.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014700         PERFORM DA00B.
014710       MOVE SPACES         TO R-DET.
      *    *************************************************************
      *    *****	   CREDITOR INVOICE TOTAL		   *****
      *    *************************************************************
014720       MOVE WS-HCRED       TO G-H1.
014730       MOVE W15-REFER      TO G-SUPP.
014740       MOVE W15-NAME       TO G-NAMER.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
014760	     MOVE SPACES	 TO R-DET.
014770       MOVE "INV:"         TO G-H3.
014780	     MOVE "DATE:"	 TO G-H4.
             MOVE "VAT:"         TO G-H6.
014810       MOVE "TOTAL:"       TO G-H7.
014820       MOVE W15-INVN       TO G-INVN.
014830       MOVE W15-INVD       TO G-INVD.
             MOVE W15-VAT        TO G-DISC.
014860       MOVE W15-TRNTOT     TO G-TOTAL.
014870     IF W15-MONTH (1) = 1
014880         MOVE "Y"          TO I-30
014890     ELSE
014900         MOVE "N"          TO I-30.
014910     IF W15-MONTH (2) = 1
014920         MOVE "Y"          TO I-60
014930     ELSE
014940         MOVE "N"          TO I-60.
014950     IF W15-MONTH (3) = 1
014960         MOVE "Y"          TO I-90
014970     ELSE
014980         MOVE "N"          TO I-90.
014990     IF W15-MONTH (4) = 1
015000         MOVE "Y"          TO I-120
015010     ELSE
015020         MOVE "N"          TO I-120.
015030     IF W15-MONTH (5) = 1
015040         MOVE "Y"          TO I-150
015050     ELSE
015060         MOVE "N"          TO I-150.
015070     IF W15-MONTH (6) = 1
015080         MOVE "Y"          TO I-180
015090     ELSE
015100         MOVE "N"          TO I-180.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
015150	       PERFORM DA00B.
015160       MOVE SPACES         TO R-DET.
015180 DA115.
029700	     MOVE "ENTER to continue - E to exit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
015280	   IF NOT (WS-OPTION = "E")
015290         GO TO DA00.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "Total excluding VAT"
                                 TO I-NAR.
             SUBTRACT W20-VAT FROM W20-DEBIT
                                 GIVING I-VALUE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "VAT" 	 TO I-NAR.
             MOVE W20-VAT        TO I-VALUE.
	     PERFORM CALL-PRINTUTL.
015310	     MOVE "GENERAL LEDGER TOTAL"
                                 TO I-NAR.
015320       MOVE W20-DEBIT      TO I-VALUE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *    *************************************************************
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
016830	     MOVE SPACES	 TO R-DET.

015360 DA999.
015370       EXIT.

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

      /    *************************************************************
      *    ****    P R O C E S S   C R E D I T O R   C O N T R O L
      *    *************************************************************
011620 GA000	       SECTION 50.
011670 GA00.
011680       DISPLAY CLR-SCREEN.
012910       DISPLAY "PROCESSING CREDITOR CONTROL" AT 0227
012920                         WITH FOREGROUND-COLOR 15.
             INITIALIZE XFR-REC.
             PERFORM START-AT-LEDTRF-KEY THRU READ-LEDTRF-EXIT.
	   IF WS-F-ERROR = 40
	       MOVE "No entries to be posted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO GA999.
013370 GA05.
013380       PERFORM AY60 THRU AY999.
010900       PERFORM AY65 THRU AY999.
013390       MOVE ZERO           TO WS-PAGE W20-DEBIT W20-CREDIT.
013400       MOVE "G/L JOURNALS" 
                                 TO W10-HEAD.
013410       MOVE W05-TODAY      TO W15-DATE.
013420       DISPLAY "Date     :" AT 1210.
013430       DISPLAY "Account  :" AT 1410.
013440       DISPLAY "Narrative:" AT 1610.
013440       DISPLAY "Value    :" AT 1810.
      *    *************************************************************
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *    *************************************************************
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.

013480 GA10.
013490       PERFORM AA50.
013500       MOVE "ACCNT NO"     TO R2-H5.
013510       MOVE ALL "_"        TO R2-H6.
013520       MOVE "ACCOUNT NAME" TO R2-H7.
013530       MOVE ALL "_"        TO R2-H8.
013540       MOVE "DATE"         TO R2-H9.
013550       MOVE "REFERENCE"    TO R2-H10.
013560       MOVE ALL "_"        TO R2-H11.
013570       MOVE "NARRATION"    TO R2-H12.
013580       MOVE ALL "_"        TO R2-H13.
013590       MOVE "DEBIT"        TO R2-H14.
013600       MOVE "CREDIT"       TO R2-H15.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013700	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

013630 GA15.
             PERFORM READ-LEDTRF-NEXT THRU READ-LEDTRF-EXIT.
	   IF WS-F-ERROR = 40
               GO TO GA20.  
      *    *************************************************************
      *    ****    C H E C K   G / L   A C C O U N T   N O .
      *    *************************************************************
             MOVE XFR-AC         TO W15-ACCOUNT.
013940       MOVE ZERO           TO WS-ERROR.
013950       DISPLAY W15-ACCOUNT AT 1422 WITH FOREGROUND-COLOR 11.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
014040	   IF WS-F-ERROR = 8
014060         PERFORM GA30 THRU GA40
014070         GO TO GA15.
      *    *************************************************************
      *    ****    G E N E R A T E   R E F E R E N C E   N O .
      *    *************************************************************
	   IF XFR-MM > 9
	       IF XFR-MM = 10
		   MOVE "O"	 TO W15-IMM
	       ELSE
	       IF XFR-MM = 11
		   MOVE "N"	 TO W15-IMM
	       ELSE
		   MOVE "D"	 TO W15-IMM
	       END-IF
	   ELSE
	       MOVE XFR-MM	 TO W15-IMN.
	     MOVE XFR-DD	 TO W15-IDD.
013700       MOVE XFR-LEDG       TO W15-ILEDG.
      *    *************************************************************
      *    ****    Switch date to DDMMCCYY format
      *    *************************************************************
	     MOVE XFR-DATE	 TO W15-YMD.
	     MOVE W15-CN	 TO W15-CC.
	     MOVE W15-YR	 TO W15-YY.
             MOVE W15-MT         TO W15-MM.
             MOVE W15-DY         TO W15-DD.
             MOVE XFR-NAR        TO W15-NAR.
             MOVE XFR-VALUE      TO W15-VAL.
             MOVE W15-DATE       TO W25-DTE.
013450	     DISPLAY W25-DATE AT 1221
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
014000	     DISPLAY G-NAME AT 1430
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W15-NAR AT 1622
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
014110       MOVE W15-VAL        TO W25-VAL2.
014120	     DISPLAY W25-S7V2 AT 1822
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *    *************************************************************
      *    ****    PROCESS AND PRINT TRANSACTION
      *    *************************************************************
014130       MOVE G-AC           TO R2-ACC.
014140       MOVE G-NAME         TO R2-ANME.
014150       MOVE W15-NAR        TO R2-NAR.
014160       MOVE W15-REFER      TO R2-REF.
014170       MOVE W15-DATE       TO R2-DTE2.
014180     IF W15-VAL < ZERO
014190         MOVE W15-VAL      TO R2-CRD
014200     ELSE
014210         MOVE W15-VAL      TO R2-DBT.
012900       PERFORM AY12 THRU AY59.
             INITIALIZE T-REC.
012680       MOVE W15-YMD        TO T-DATE.
014370       MOVE W15-NAR        TO T-NARATIVE.
014380	     MOVE W15-REF	 TO T-REFER.
014390       MOVE W15-VAL        TO T-VAL.
014420       MOVE G-AC           TO T-AC.
014430     IF W15-VAL < ZERO
014440         ADD W15-VAL       TO W20-CREDIT
014450         ADD W15-VAL       TO G-CR
014460         MOVE 02           TO T-CODE 
014470     ELSE
014480         ADD W15-VAL       TO W20-DEBIT
014490         ADD W15-VAL       TO G-DT
014500         MOVE 01           TO T-CODE.
             MOVE W15-VAL        TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
014510       ADD W15-VAL         TO G-BAL.
014240       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
012550       PERFORM AY13 THRU AY59.
014540       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
015750	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014610	       PERFORM GA10.
014620	     MOVE SPACES	 TO R-DET.
             GO TO GA15.

016960 GA20.
016970       PERFORM AY70 THRU AY999.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017060	       PERFORM GA10.
017070       MOVE "*****   T O T A L S   *****" 
                                 TO R2-NAR.
017080       MOVE W20-DEBIT      TO R2-DBT.
017090       MOVE W20-CREDIT     TO R2-CRD.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *    *************************************************************
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
016830	     MOVE SPACES	 TO R-DET.
	     CLOSE LEDTRF.
             OPEN OUTPUT LEDTRF.
             CLOSE LEDTRF.
             OPEN I-O LEDTRF.
017210       GO TO GA999.

017220 GA30.
017300       MOVE "** INVALID G/L ACCOUNT NUMBER" TO R-DET.

017620 GA35.
014180     IF XFR-VALUE < ZERO
014190         MOVE XFR-VALUE    TO R2-CRD
               MOVE "["          TO R2-CBRKO
               MOVE "]"          TO R2-CBRKC
014200     ELSE
014210         MOVE XFR-VALUE    TO R2-DBT
               MOVE "["          TO R2-DBRKO
               MOVE "]"          TO R2-DBRKC.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017670	       PERFORM GA10.
017680	     MOVE SPACES	 TO R-DET.
017690	     MOVE ZERO		 TO WS-ERROR.

017700 GA40.
017710       EXIT.

       GA999.
             EXIT.
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
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    *** G E N E R A L   L E D G E R   A C C O U N T   F I L E ***
      *    *************************************************************
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
      *    *************************************************************
      *    **	 G / L	 T R A N S A C T I O N	 W O R K   F I L E    **
      *    *************************************************************
	     MOVE "GLJRN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-GLJRN.
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
      *    ****      V A T   T R A N S A C T I O N S   F I L E	    ****
      *    *************************************************************
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
023900	     OPEN I-O GACCNT.
023910     IF RUNTIME-ERROR
               IF FLE-LOCKED
041880             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
035500     IF WS-STATUS NOT = "00"
035510	       MOVE 8		 TO WS-F-ERROR
050480         PERFORM OPEN-ERROR.
023930       OPEN I-O GLTRAN.
023940     IF RUNTIME-ERROR
               IF FLE-LOCKED
041880             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
035560     IF WS-STATUS NOT = "00"
035570	       MOVE 11		 TO WS-F-ERROR
050480         PERFORM OPEN-ERROR.

       ZA01C.
044300	     OPEN I-O LEDTRF.
           IF WS-STATUS = "05"
               CLOSE LEDTRF
               OPEN OUTPUT LEDTRF
               CLOSE LEDTRF
               OPEN I-O LEDTRF.
044310     IF RUNTIME-ERROR
               IF FLE-LIMIT
		   GO TO ZA49.
042180	   IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
	       IF IDX-CORRUPT OR
		 (WS-STATUS = "39")
		   CLOSE PARAM
			 NETWORK
		   CALL "UTP\LTFREB"
			 USING LS-PARID LS-USER-ID LS0-PROGRAMS
			       LS0-SECURITY
		   CANCEL "UTP\LTFREB"
		   OPEN I-O PARAM NETWORK
		   GO TO ZA01C.
050190	   IF WS-STATUS NOT = "00"
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
035910	       MOVE 2		 TO G-YEAR-END
035920         PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *    *************************************************************
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *    *************************************************************
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

      *    *************************************************************
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *    *************************************************************
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
041800	   ELSE
041810     IF WS-F-ERROR = 7
041820         MOVE W02-DEPART   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
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
041850     IF WS-F-ERROR = 26
041860         MOVE W02-GLJRN    TO WS-FILE
041870         MOVE WS-GLJKEY    TO WS-KEY
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
