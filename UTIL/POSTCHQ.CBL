      $set LINKCOUNT"448"
      ******************************************************************
      *                                                                *
      * *******   ******   ******  ********  ******  **    **  ******  *
      * **    ** **    ** **	**    **    **	  ** **    ** **    ** *
      * **    ** **    ** **	      **    **	     **    ** **    ** *
      * *******  **    **  ******     **    **	     ******** **    ** *
      * **	 **    **	**    **    **	     **    ** **    ** *
      * **	 **    ** **	**    **    **	  ** **    ** **  ***  *
      * **	  ******   ******     **     ******  **    **  **** ** *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  P O S T   C H E Q U E S   F R O M   B A T C H	 /   P R I N T *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       POSTCHQ.
000030 AUTHOR.                 J.W. LEMMON (APAC).
000040 DATE-WRITTEN.	       January 1998.

	   This program is used to post cheques located in the cheque
	   batch file that have already been printed and that have had
	   cheque numbers allocated.

	   Cheques that have been post dated (determined by comparing
	   the date on the cheque with the current run date) will be
	   added to the PDCHEQ file.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1998 - 2008
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

       COPY CHQBATCH.SL.

000140 COPY CONTROL.SL.

000160 COPY CREDIT.SL.

000180 COPY CRTRAN.SL.

000200 COPY GACCNT.SL.

000220 COPY GLTRAN.SL.

       COPY GPDCHQ.SL.

000140 COPY PARAM.SL.

       COPY PDCHEQ.SL.

000280 COPY RECOVER.SL.

       COPY SHARED.SL.

000310 DATA DIVISION.
000320 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CHQBATCH.FDE.

000140 COPY CONTROL.FDE.

000360 COPY CREDIT.FDE.

000380 COPY CRTRAN.FDE.

000400 COPY GACCNT.FD.

000420 COPY GLTRAN.FD.

       COPY GPDCHQ.FDE.

000340 COPY PARAM.FDE.

       COPY PDCHEQ.FDE.

000480 COPY RECOVER.GFD.

       COPY SHARED.FDE.

      /
002060 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001790 77  WS-SUB	   PIC	9(04)	 COMP-5.
001790 77  WS-IXS	   PIC	9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC	9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC	9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC	9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC	9(04)	 COMP-5.
002070 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
002070 77  WS-NETKEY	   PIC	9(04)	 COMP-5 VALUE 1.
002110 77  WS-CTRKEY	   PIC	9(04)	 COMP-5 VALUE 1.
002130 77  WS-KSTORE	   PIC	9(04)	 COMP-5.
001760 77  WS-SKSTORE	   PIC	9(06)	 COMP-5.
002140 77  WS-KEYSTR	   PIC	9(04)	 COMP-5.
002170 77  WS-RECKEY	   PIC	9(04)	 COMP-5.
002180 77  WS-RECOVER	   PIC	9(04)	 COMP-5.
       77  WS-SHARED	   PIC	9(04)	 COMP-5.
002190 77  WS-TRANS	   PIC	9(04)	 COMP-5 VALUE 1.
002200 77  WS-ISUB	   PIC	9(04)	 COMP-5.
002220 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
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
001900 77  WS-L-END        PIC  9(02) VALUE ZERO.
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
       77  WS-H-DISP	   PIC	X(27) VALUE SPACES.
002530 77  WS-OPTION	   PIC	X(01).
001970 77  WS-SKIP         PIC  X(01) VALUE "N".
002780 77  WS-LOOK	   PIC	X(01) VALUE "C".
002540 77  WS-FIRST	   PIC	9(02) VALUE 0.
002550 77  WS-LAST         PIC  9(02) VALUE 0.
002570 77  WS-ERROR	   PIC	9(01) VALUE ZERO.
003620 77  WS-CR	   PIC	X(01) VALUE SPACE.
000630 77  WS-GL	   PIC	X(01) VALUE SPACE.
000630 77  WS-CPD	   PIC	X(01) VALUE SPACE.
000630 77  WS-GLD	   PIC	X(01) VALUE SPACE.
       77  WS-POST	   PIC	X(01).
	   88  BEGIN-POST  VALUE "N".
	   88  CR-CHEQ	   VALUE "C".
	   88  GEN-LEDG    VALUE "G".
	   88  CR-PDCHQ    VALUE "P".
	   88  GL-PDCHQ    VALUE "D".
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

002780 01  W00-TREC.
002790     03  W00-DATE        PIC 9(06).
002800     03  W00-NAR         PIC X(40).
002810     03  W00-CODE        PIC 9(02).
002820     03  W00-VAL         PIC S9(09)V99 COMP-3.
002830     03  W00-PREV        PIC 9(06)     COMP-5.
002840     03  W00-NXT         PIC 9(06)     COMP-5.
002850     03  W00-AC          PIC 9(06)     COMP-3.
002860     03  FILLER          PIC X(01).

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

       COPY CHQBATCH.ID.

000140 COPY CONTROL.ID.

000360 COPY CREDIT.ID.

000380 COPY CRTRAN.ID.

000400 COPY GACCNT.ID.

000420 COPY GLTRAN.ID.

       COPY GPDCHQ.ID.

       COPY PARAM.ID.

       COPY PDCHEQ.ID.

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
000560	       05  R-DTE1      PIC Z9/99/9999.
000570	       05  FILLER      PIC X(32).
000580	       05  R-H2        PIC X(40).
000590	       05  FILLER      PIC X(32).
000600	       05  R-H3        PIC X(08).
000610	       05  R-PGE       PIC ZZ9.

	   03  RG-L2 REDEFINES W02-PRN-LINE.
	       05  RG-H1       PIC  X(06).
	       05  RG-DTE1     PIC  Z9/99/9999.
	       05  FILLER      PIC  X(23).
	       05  RG-H2       PIC  X(40).
	       05  FILLER      PIC  X(15).
	       05  RG-H3       PIC  X(07).
	       05  RG-BTCH     PIC  ZZZZ9.
	       05  FILLER      PIC  X(17).
	       05  RG-H4       PIC  X(06).
	       05  RG-PGE      PIC  ZZ9.

000620	   03  R-L2A REDEFINES W02-PRN-LINE.
000630	       05  FILLER      PIC X(56).
000640	       05  R-HEAD      PIC X(20).
000650	       05  FILLER      PIC X(56).

	   03  RG-L5 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(65).
	       05  RG-H16      PIC  X(10).
	       05  FILLER      PIC  X(01).
	       05  RG-H17      PIC  X(15).
	       05  RG-H18      PIC  X(10).
	       05  FILLER      PIC  X(31).

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
001880	       05  FILLER      PIC  X(03).
001890	       05  R-DTE2      PIC  Z9/99/9999.
001900	       05  FILLER      PIC  X(03).
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
000790	       05  R2-DTE1     PIC Z9/99/9999.
000800	       05  FILLER      PIC X(09).
000810	       05  R2-H2       PIC X(40).
000820	       05  R2-H2A      PIC X(27).
000830	       05  FILLER      PIC X(09).
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
	       05  PD-ACNO     PIC  X(06).
	       05  FILLER      PIC  X(01).
	       05  PD-NAME     PIC  X(39).
	       05  FILLER      PIC  X(01).
	       05  PD-BANK     PIC  9(06).
	       05  FILLER      PIC  X(01).
	       05  PD-CRED     PIC  9(06).
	       05  FILLER      PIC  X(01).
	       05  PD-NAR      PIC  X(40).
	       05  FILLER      PIC  X(01).
	       05  PD-DATE     PIC  Z9/99/9999.
	       05  FILLER      PIC  X(01).
	       05  PD-REF      PIC  Z(06)9.
	       05  FILLER      PIC  X(01).
	       05  PD-VALUE    PIC  Z(07)9.99.

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

002520 01  W05-TDATE.
002530	   03  W05-TODAY   PIC	9(08).
002540     03  W05-DTE     REDEFINES W05-TODAY.
002550         05  W05-DD  PIC  9(02).
002560         05  W05-MM  PIC  9(02).
002570	       05  W05-CC  PIC	9(02).
002570         05  W05-YY  PIC  9(02).
003240	   03  W05-DATE-CYMD REDEFINES W05-TODAY.
	       05  W05-MD  PIC	9(04).
	       05  W05-CY  PIC	9(04).
002580	   03  W05-TYMD    PIC	9(08).
002590     03  W05-TDTE    REDEFINES W05-TYMD.
002600	       05  W05-C   PIC	9(02).
002600         05  W05-Y   PIC  9(02).
002610         05  W05-M   PIC  9(02).
002620         05  W05-D   PIC  9(02).
	   03  W05-T-CYMD      PIC  9(08).
	   03  W05-CHK-DATE REDEFINES W05-T-CYMD.
	       05  W05-CC      PIC  9(02).
	       05  W05-YYMMDD  PIC  9(06).
	   03  W05-CHK-DTE REDEFINES W05-T-CYMD.
	       05  W05-TCY     PIC  9(04).
	       05  W05-TMM     PIC  9(02).
	       05  W05-TDD     PIC  9(02).

       COPY W05.VAT.

003250 01  W10-H2.
003260     03  FILLER          PIC X(27).
003270     03  W10-HEAD        PIC X(13).

003280 01  W15-INPUT.
003290     03  W15-ACCOUNT     PIC 9(06).
003300     03  W15-ACNO1       REDEFINES W15-ACCOUNT.
003310         05  FILLER      PIC 9(04).
003320         05  W15-SER     PIC 9(02).
003330     03  W15-ACNO2       REDEFINES W15-ACCOUNT.
003340         05  W15-GR      PIC 9(02).
003350         05  W15-SGRP    PIC 9(02).
003360         05  W15-ASER    PIC 9(02).
003370     03  W15-ACNO3       REDEFINES W15-ACCOUNT.
003380         05  W15-APREF   PIC 9(01).
003390         05  W15-SERIAL  PIC 9(03).
003400         05  FILLER      PIC 9(02).
003310	   03  W15-ACNO        PIC X(06).
003410	   03  W15-DATE        PIC 9(08).
003420     03  W15-RDATE       REDEFINES W15-DATE.
003430         05  W15-DD      PIC 9(02).
003440         05  W15-MM      PIC 9(02).
003450	       05  W15-CC      PIC 9(02).
003450         05  W15-YY      PIC 9(02).
003420	   03  W15-RDTE        REDEFINES W15-DATE.
	       05  FILLER      PIC 9(04).
	       05  W15-CY      PIC 9(04).
	   03  W15-YMD	   PIC	9(08).
           03  W15-RYMD REDEFINES W15-YMD.
	       05  W15-CN  PIC	9(02).
               05  W15-YR  PIC  9(02).
               05  W15-MT  PIC  9(02).
               05  W15-DY  PIC  9(02).
003460     03  W15-REFER.
               05  W15-REFNO   PIC 9(07).
           03  W15-IREF REDEFINES W15-REFER.
               05  W15-IDD     PIC  9(02).
               05  W15-IMM     PIC  9(02).
               05  W15-ILEDG   PIC  9(03).
003470	   03  W15-NAR	       PIC  X(40).
003470	   03  W15-NAR2        PIC  X(40).
003480     03  W15-VAL         PIC S9(09)V99.
003490	   03  W15-BANK        PIC  9(06).
003500	   03  W15-PAYEE       PIC  X(36).
003510	   03  W15-CRED        PIC  9(06).
003520	   03  W15-PER	       PIC  9(03)V99.
003530	   03  W15-IND	       PIC  9(01).
003540	   03  W15-BNKNME      PIC  X(30).
003540	   03  W15-ACNME       PIC  X(30).
003550     03  W15-COST        PIC S9(09)V99.
003560     03  W15-TCOST       PIC S9(09)V99 COMP-3.
003570	   03  W15-SETT        PIC  9(06).
003580     03  W15-SET         REDEFINES W15-SETT.
003590	       05  W15-30      PIC  9(01).
003600	       05  W15-60      PIC  9(01).
003610	       05  W15-90      PIC  9(01).
003620	       05  W15-120     PIC  9(01).
003630	       05  W15-150     PIC  9(01).
003640	       05  W15-180     PIC  9(01).
003650     03 W15-PAY          REDEFINES W15-SETT.
003660	      05  W15-MONTH    PIC  9(01) OCCURS 6.
003670	   03  W15-INVN        PIC  X(08).
003680	   03  W15-INVD        PIC  9(08)    COMP-3.
003690	   03  W15-GRNN.
	       05  W15-GRNNO   PIC  9(08).
003700	   03  W15-GRND        PIC  9(08)    COMP-3.
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
003740     03  W15-NAME.
001490	       05  W15-NCHAR   PIC  X(01) OCCURS 40.
001500	   03  W15-N	       REDEFINES W15-NAME.
001510         05  W15-NIX.
001520		   07  W15-ENTRY
			       PIC  X(08).
001530		   07  FILLER  PIC  X(16).
001540	       05  FILLER      PIC  X(16).
	   03  W15-ALLOCATIONS OCCURS 10.
	     05  W15-GL-AC     PIC  9(06).
	     05  W15-GLAC-NAME PIC  X(30).
	     05  W15-NARRATIVE PIC  X(40).
	     05  W15-AMOUNT    PIC S9(09)V99.

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

002730 01  W22-DATES.
002740	   03  W22-DTE1        PIC  9(08).
002750	   03  W22-DTE2        PIC  9(08).
002760     03  W22-DATE2 REDEFINES W22-DTE2.
	       05  W22-CC2     PIC  9(02).
002770	       05  W22-YY2     PIC  9(02).
002780	       05  W22-MM2     PIC  9(02).
002790	       05  W22-DD2     PIC  9(02).
002760	   03  W22-DATE3 REDEFINES W22-DTE2.
	       05  W22-CY2     PIC  9(04).
	       05  FILLER      PIC  9(04).
002800	   03  W22-DTE3        PIC  9(08).
002810     03  W22-DATE3 REDEFINES W22-DTE3.
002820	       05  W22-DD3     PIC  9(02).
002830	       05  W22-MM3     PIC  9(02).
	       05  W22-CC3     PIC  9(02).
002840	       05  W22-YY3     PIC  9(02).
	   03  W22-CCYMD.
	       05  W22-C       PIC  9(02).
	       05  W22-YMD     PIC  9(06).
	   03  W22-CCYYMMDD REDEFINES W22-CCYMD.
	       05  W22-CY      PIC  9(04).
	       05  W22-M       PIC  9(02).
	       05  W22-D       PIC  9(02).
	   03  W22-CYMD REDEFINES W22-CCYMD
			       PIC  9(08).
	   03  W22-DDMMCCYY.
	       05  W22-DD      PIC  9(02).
	       05  W22-MM      PIC  9(02).
	       05  W22-CCYY    PIC  9(04).
	   03  W22-DMCY REDEFINES W22-DDMMCCYY
			       PIC  9(08).

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

       01  W30-COUNTER.
	   03  W30-CHARS       PIC  X(04) VALUE "|/-\".
	   03  W30-CHAR REDEFINES W30-CHARS
			       PIC  X(01) OCCURS 4.
	   03  W30-COUNT       PIC  9(02) COMP-5.

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

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004430 SCREEN SECTION.

       COPY BLANK.CRT.

005030 01  S06.
005040	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC X(06) USING W15-ACNO AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

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
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM DA000.

005640 AA11.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
016830	     MOVE SPACES	 TO R-DET.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
005650	     CLOSE CHQBATCH
005670		   CREDIT
		   CTRANS
		   GACCNT
		   GLTRAN
		   GPDCHQ
		   PDCHEQ
		   RECOVER.

       AA11E.
             EXIT PROGRAM.
      *
      *    ****    Print the Creditors audit trail HEADING
      *
005910 AA50.
	     MOVE "H"		 TO W02-PRN-TYPE.
005920	     MOVE SPACES	 TO R-DET.
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
006000	     MOVE SPACES	 TO R-DET.
006010	     MOVE W60-DESC	 TO R-H2.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
006030	     MOVE SPACE 	 TO R-DET.
006050	     MOVE W50-H4	 TO R-HEAD.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
006070	     MOVE SPACES	 TO R-DET.

006080 AA65.
006090       EXIT.
      *
      *    ****    Print the Creditor PD Cheque audit trail HEADING
      *
       AA50CP.
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
	     STRING W60-DESC DELIMITED SIZE
		    " - " DELIMITED SIZE
		    W50-HEAD DELIMITED SIZE
	       INTO PD-H4.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
006000	     MOVE SPACES	 TO R-DET.
019650	     MOVE "CREDITOR ACCOUNT"
				 TO PD-H1.
	     MOVE "G/L ACCOUNTS" TO PD-H4.
	     MOVE "CHEQUE DETAILS"
				 TO PD-H7.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019720	     MOVE SPACES	 TO R-DET.
019660	     MOVE "NUMB."	 TO PD-H2.
019670	     MOVE "NAME"	 TO PD-H3.
019680	     MOVE "BANK"	 TO PD-H5.
019690	     MOVE "CREDIT"	 TO PD-H6.
019780	     MOVE "NARRATIVE"	 TO PD-H8.
	     MOVE "DATE"	 TO PD-H9.
019770	     MOVE "CHEQ NO"	 TO PD-H10.
019790	     MOVE "VALUE"	 TO PD-H11.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

       AA65CP.
	     EXIT.

007340 AA50G.
      *
      *    ****    Print the audit trail HEADING
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

007490 AA55G.
007500	     EXIT.
      *
      *    ****    Print the G/Ledger PD Cheque audit trail HEADING
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

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY CREDITOR.TRN.

       COPY PRINTUTL.AS3.

      /
      *    ****    READ FILES ROUTINES
      *
006190 AB000	       SECTION.

       COPY CHQBATCH.RD.

       COPY CONTROL.RD.

       COPY CRTRAN.RD.

       COPY CREDIT.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD2.

       COPY GLTRAN.RD.

       COPY GPDCHQ.RD.

       COPY PDCHEQ.RD.

       COPY SHARED.RD.

007740 AC000	       SECTION.

       COPY CHQBATCH.WR.

       COPY CONTROL.WR.

       COPY CREDIT.WR.

       COPY CRTRAN.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

       COPY GPDCHQ.WR.

       COPY PDCHEQ.WR.

      /
009010 AE155-GET-CREDITOR    SECTION.
009020 AE155-ACCOUNT.
011060       MOVE W15-ACNO       TO P-NUMBER.
011130       PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
011140     IF WS-F-ERROR = 3 
011150	       MOVE 1		 TO WS-ERROR
011160     ELSE
011170     IF P-NUMBER NOT = W15-ACNO
011180	       MOVE 2		 TO WS-ERROR
011190     ELSE
011200	       MOVE 0		 TO WS-ERROR.

009180 AE160-EXIT.
009190       EXIT.

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
	       SUBTRACT 13 FROM WS-PERIOD
				 GIVING WS-S3
               PERFORM AH10
	       ADD 1 WS-OPEN	 GIVING WS-MS1
           ELSE
	       MOVE WS-PERIOD	 TO WS-MS1.
       AH05.
	     ADD WS-VALUE	 TO G-VAL(WS-MS1).
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
	     ADD WS-VALUE	 TO G-LVAL(WS-S3).
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
      /
      *    ****   *****   ***	 ***   *   *  *****  ****   *	*
      *    *   *  *	 *   *	*   *  *   *  *      *	 *  *	*
      *    ****   ***	 *	*   *  *   *  ***    ****    * *
      *    *   *  *	 *   *	*   *	* *   *      *	 *    *
      *    *   *  *****   ***	 ***	 *    *****  *	 *    *
      *
      *    THESE ROUTINES GENERATE THE RECOVERY FILE RECORDS
      *
009530 AY000           SECTION.
009540 AY01.
009550       MOVE 01             TO REC-FILE.
009560       MOVE WS-PARKEY      TO REC-KEY.
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
009570       MOVE PAR-RECORD1    TO REC-PARAM.
009580       GO TO AY50.

009590 AY07.
009600	     MOVE 07		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
009620       MOVE P-REC          TO REC-CREDIT.
009630       GO TO AY50.

011890 AY08.
011900       MOVE 08             TO REC-FILE.
011910	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011920       MOVE CT-REC         TO REC-CRTRAN.
011930       GO TO AY50.

009690 AY12.
009700       MOVE 12             TO REC-FILE.
009710	     MOVE ZERO		 TO REC-KEY REC-TYPE.
             PERFORM READ-GACCNT-LOCK THRU READ-GACCNT-EXIT.
009720       MOVE G-REC          TO REC-GACCNT.
009730       GO TO AY50.

009740 AY13.
010650	     MOVE 13		 TO REC-FILE.
010660       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
010670       MOVE T-REC          TO REC-GLTRAN.
010680       GO TO AY50.

012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.

009790 AY44.
011080	     MOVE 44		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE PDT-REC	 TO REC-PDCHEQ.
011110       GO TO AY50.

009790 AY45.
011080	     MOVE 45		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE GPD-REC	 TO REC-GPDCHQ.
011110       GO TO AY50.

012040 AY49.
009800	     MOVE 99		 TO REC-FILE.
009810	     MOVE ZERO		 TO REC-KEY.
009820	     MOVE SPACES	 TO REC-DETAIL.
009830       PERFORM AY50.
009840	     ADD 1		 TO WS-TRANS.
009850     IF WS-RECOVER > 95
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
009980         DISPLAY (22, 12) "Write error Recovery file - Status "
009990                  WITH FOREGROUND-COLOR 14 
010000                  WS-STATUS WITH FOREGROUND-COLOR 15
010010         STOP RUN.

010040 AY59.
010050       EXIT.
      *
      *    ****    Start processing transaction
      *
011390 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
011960       MOVE 4              TO WS-PARKEY.
011480       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
               GO TO AY62.
004870	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
             UNLOCK PARAM.
             PERFORM LOCK-REC-LOOP.
	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
             GO TO AY60.

       AY61.
011990       MOVE "CG"           TO PAR-PROG (WS-USUB).
	     MOVE LS-USER	 TO PAR-USR (WS-USUB).
011460       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010       MOVE 2              TO WS-NETKEY.
012020       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012130       PERFORM AY39 THRU AY59.
             MOVE NET-CREDITOR   TO W70-CREDCON.
012010       MOVE 4              TO WS-NETKEY.
012700       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012710       PERFORM AY39 THRU AY59.
012170       GO TO AY999.

       AY62.
           IF PAR-PROG (WS-SUB) = "CR" OR "CS" OR "CG" OR "GL"
      *
      *    ****    Test if this is a program error first
      *
	       IF LS-USER = PAR-USR (WS-USUB)
                   SUBTRACT 1    FROM PAR-USERS
                   MOVE SPACES   TO PAR-PROG (WS-USUB)
                                    PAR-USR (WS-USUB)
               ELSE
                   UNLOCK PARAM
		   DISPLAY "Waiting" AT 2572 WITH
			    BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK
                   PERFORM LOCK-REC-LOOP
		   DISPLAY "Waiting" AT 2572 WITH
			    BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK
                   GO TO AY60.
           IF WS-SUB < 24
               ADD 1             TO WS-SUB
               GO TO AY62.
	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             MOVE 1              TO WS-SUB.

       AY63.
           IF PAR-USR (WS-SUB) = SPACES
               MOVE WS-SUB       TO WS-USUB
               ADD 1             TO PAR-USERS
               GO TO AY61.
             ADD 1               TO WS-SUB. 
             GO TO AY63.

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
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 2.
      *
012180 AY70.
012290	     MOVE 4		 TO WS-PARKEY.
012300       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
012270	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012970       MOVE 2              TO WS-NETKEY.
012980       MOVE W70-CREDCON    TO NET-CREDITOR.
012990	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012280       PERFORM AY49 THRU AY59.
012310	     MOVE SPACES	 TO PAR-PROG (WS-USUB)
                                    PAR-USR (WS-USUB).
             SUBTRACT 1          FROM PAR-USERS.
012320	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
011850       COMMIT.
011860 AY999.
011870       EXIT.
      /  
010710 BA000         SECTION.
010720 BA00.
010730	     MOVE ZERO		 TO WS-ERROR.
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
010880	     DIVIDE W15-CY BY 4 GIVING W20-RESULT.
010890     IF W20-DEC > ZERO
010900	       GO TO BA10.
010910	     GO TO BA15.
010920 BA10.
010930	     MOVE 1		 TO WS-ERROR.
	     GO TO BA999.
      *
      *   ****    CALCULATE THE PERIOD INTO WHICH THIS DATE FALLS
      * 
       BA15.
	     MOVE W15-MM	 TO WS-PERIOD.
	   IF W15-CY > W05-CY
	       GO TO BA10.
	   IF W15-CY < W05-CY
	       IF W15-CY < WS-L-END
		   GO TO BA20
	       ELSE
	       IF NOT (W15-MM > WS-Y-END)
		   GO TO BA20
	       ELSE
		   GO TO BA999.
	   IF W05-MM = W15-MM
	      GO TO BA999.
	   IF W15-MM > W05-MM
	       GO TO BA10.
	   IF W15-MM > WS-Y-END
	       GO TO BA999.
	   IF W05-MM = WS-Y-END
	       GO TO BA999.

       BA20.
	   IF W15-CY > WS-L-END
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

      /
       DA000	      SECTION 5.
      *
      *    ****    D E T E R M I N E   W H I C H   R O U T I N E S
      * 	   A R E   R E Q U I R E D
      *
       DA00.
	     PERFORM SAVE-SCREEN-3.
	     DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴" AT 1024
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "�" WITH FOREGROUND-COLOR 2 HIGHLIGHT
			      BACKGROUND-COLOR 2.
	     DISPLAY "�" AT 1124
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     " Examining Cheque batch file   "
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 2
		     "�" WITH FOREGROUND-COLOR 2 HIGHLIGHT
			      BACKGROUND-COLOR 2
		     "같" WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 0.
	     DISPLAY "�" AT 1224
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸"
		      WITH FOREGROUND-COLOR 2 HIGHLIGHT
			   BACKGROUND-COLOR 2
		     "같" WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 0.
	     DISPLAY "같같같같같같같같같같같같같같같같�" AT 1326
		      WITH FOREGROUND-COLOR 0 HIGHLIGHT
			   BACKGROUND-COLOR 0.
	     MOVE 1		 TO W30-COUNT.
      *
      *    ****    R E A D   T H E   F I R S T	 R E C O R D   I N   T H E
      * 	   B A T C H   F I L E
      *
	     MOVE "N"		 TO WS-CR WS-CPD
				    WS-GL WS-GLD
				    WS-POST.
	     MOVE 1		 TO BCH-KEY.
	     PERFORM START-AT-CHQBATCH-KEY THRU READ-CHQBATCH-EXIT.
	   IF WS-F-ERROR = 58
	       GO TO DA990.

       DA05.
	     PERFORM READ-CHQBATCH-NEXT THRU READ-CHQBATCH-EXIT.
	     DISPLAY W30-CHAR (W30-COUNT) AT 1154
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF W30-COUNT < 4
	       ADD 1		 TO W30-COUNT
	   ELSE
	       MOVE 1		 TO W30-COUNT.
	   IF WS-F-ERROR = 58
	       GO TO DA10.
      *
      *    ****    C H E Q U E	 N U M B E R   B E E N	 A L L O C A T E D ?
      *
	   IF NOT (BCH-STATUS =	"A")
	       GO TO DA05.
      *
      *    ****    S W I T C H	 D A T E
      *
	     MOVE BCH-DATE	 TO W22-DMCY.
	     MOVE W22-CCYY	 TO W22-CY.
	     MOVE W22-MM	 TO W22-M.
	     MOVE W22-DD	 TO W22-D.
      *
      *    ****    I S	 T H E	 C H E Q U E   P O S T	 D A T E D
      *
	   IF W22-CYMD > W05-T-CYMD
	       IF BCH-TYPE = "C"
		   MOVE "Y"	 TO WS-CPD
	       ELSE
		   MOVE "Y"	 TO WS-GLD
	   ELSE
	   IF BCH-TYPE = "C"
	       MOVE "Y" 	 TO WS-CR
	   ELSE
	       MOVE "Y" 	 TO WS-GL.
	     GO TO DA05.
      *
      *    ****    A N Y   E N T R I E S   T O	 P O S T   O R	 L O G
      * 	   T O	 T H E	 P D - C H E Q U E S   F I L E S ?
      *
       DA10.
	     PERFORM RESTORE-SCREEN-3.
	   IF WS-CR = "N" AND WS-CPD = "N" AND
	      WS-GL = "N" AND WS-GLD = "N"
	       GO TO DA990.
	   IF WS-CR = "Y"
	       MOVE "C"		 TO WS-POST
	       GO TO DA30.

       DA15.
	   IF WS-CPD = "Y"
	       MOVE "P" 	 TO WS-POST
	       GO TO DA30.

       DA20.
	   IF WS-GL = "Y"
	       MOVE "G" 	 TO WS-POST
	       GO TO DA30.

       DA25.
	   IF WS-GLD = "Y"
	       MOVE "D"		 TO WS-POST
	       GO TO DA30.
	     GO TO DA980.
      *
      *    ****    S T A R T   A T   F I R S T	 R E C O R D  I N
      * 	   B A T C H   F I L E
      *
      * 	   There is no check made for no records as the file
      * 	   has been opened exclusive and the above routines
      * 	   found data to be posted or added to the PD cheques
      * 	   file/s
      *
       DA30.
	     MOVE 1		 TO BCH-KEY.
	     PERFORM START-AT-CHQBATCH-KEY THRU READ-CHQBATCH-EXIT.
019620	     MOVE ZERO		 TO W20-TOTAL W20-DEBIT.
	   IF CR-CHEQ
	       MOVE "CHEQUES ISSUED"
				 TO W50-HEAD
	       MOVE "Processing Creditor Cheques"
				 TO WS-H-DISP
	   ELSE
	   IF GEN-LEDG
	       MOVE "G/L CHEQUES"
				 TO W50-HEAD
	       MOVE "Processing G/Ledger Cheques"
				 TO WS-H-DISP
	   ELSE
	   IF CR-PDCHQ
	       MOVE "PD. CHEQUES (CREDIT)"
				 TO W50-HEAD
	       MOVE "Processing Cred.P-D Cheques"
				 TO WS-H-DISP
	   ELSE
	       MOVE "PD. CHEQUES (G/LEDG)"
				 TO W50-HEAD
	       MOVE "Processing G/L. P-D Cheques"
				 TO WS-H-DISP.
	     PERFORM SAVE-SCREEN-3.
	     DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴" AT 1024
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "�" WITH FOREGROUND-COLOR 2 HIGHLIGHT
			      BACKGROUND-COLOR 2.
	     DISPLAY "�" AT 1124
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     " " WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 2
		     WS-H-DISP WITH FOREGROUND-COLOR 6 HIGHLIGHT
				    BACKGROUND-COLOR 2
		     "   " WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 2
		     "�" WITH FOREGROUND-COLOR 2 HIGHLIGHT
			      BACKGROUND-COLOR 2
		     "같" WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 0.
	     DISPLAY "�" AT 1224
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸"
		      WITH FOREGROUND-COLOR 2 HIGHLIGHT
			   BACKGROUND-COLOR 2
		     "같" WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 0.
	     DISPLAY "같같같같같같같같같같같같같같같같�" AT 1326
		      WITH FOREGROUND-COLOR 0 HIGHLIGHT
			   BACKGROUND-COLOR 0.
	     MOVE 1		 TO W30-COUNT.
      *
      *    ****    R E S E T   P A G E	 N U M B E R
      *
	     MOVE ZERO		 TO WS-PAGE.

       DA35.
	     PERFORM READ-CHQBATCH-NEXT THRU READ-CHQBATCH-EXIT.
	     DISPLAY W30-CHAR (W30-COUNT) AT 1154
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF W30-COUNT < 4
	       ADD 1		 TO W30-COUNT
	   ELSE
	       MOVE 1		 TO W30-COUNT.
	   IF WS-F-ERROR = 58
	       GO TO DA80.
      *
      *    ****    C H E Q U E	 N U M B E R   B E E N	 A L L O C A T E D ?
      *
	   IF NOT (BCH-STATUS =	"A")
	       GO TO DA35.
      *
      *    ****    S W I T C H	 D A T E
      *
	     MOVE BCH-DATE	 TO W22-DMCY.
	     MOVE W22-CCYY	 TO W22-CY.
	     MOVE W22-MM	 TO W22-M.
	     MOVE W22-DD	 TO W22-D.
      *
      *    ****    S K I P   R E C O R D S   T H A T   A R E   N O T
      * 	   R E L E V A N T
	   IF ((CR-CHEQ OR CR-PDCHQ) AND BCH-TYPE = "G") OR
	      ((GEN-LEDG OR GL-PDCHQ) AND BCH-TYPE = "C")
	       GO TO DA35.
	   IF (CR-CHEQ OR GEN-LEDG) AND
	      (W22-CYMD > W05-T-CYMD)
	       GO TO DA35.
	   IF (CR-PDCHQ OR GL-PDCHQ) AND
	      (W22-CYMD NOT > W05-T-CYMD)
	       GO TO DA35.
	   IF WS-PAGE > ZERO
	       GO TO DA45.
	   IF CR-CHEQ OR GEN-LEDG
013490	       PERFORM AY65 THRU AY999.

       DA40.
	   IF CR-CHEQ
	       PERFORM AA50
019650	       MOVE WS-HACNO	 TO CH-H1
019660	       MOVE WS-HNAME	 TO CH-H2
019670	       MOVE WS-HDTE	 TO CH-H3
019680	       MOVE WS-HCHQ	 TO CH-H4
019690	       MOVE "VALUE"	 TO CH-H5
019700	       MOVE WS-HAGE	 TO CH-H6
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
019720	       MOVE SPACES	 TO R-DET
019730	       MOVE WS-HACNO	 TO R-H5
019740	       MOVE ALL "_"	 TO R-H6 R-H8 R-H16 R-H18
019750	       MOVE "ACCOUNT NAME"
				 TO R-H7
019760	       MOVE WS-HDTE	 TO R-H9
019770	       MOVE "CHEQ NO"	 TO R-H10
019780	       MOVE "PAYEE/NARRATIVE"
				 TO R-H17
019790	       MOVE "DEBIT"	 TO R-H14
019800	       MOVE "CREDIT"	 TO R-H15
	       PERFORM CALL-PRINTUTL
	   ELSE
	   IF CR-PDCHQ
	       PERFORM AA50CP
	   ELSE
	   IF GEN-LEDG
	       PERFORM AA50G
013580	       MOVE "ACCNT NO "	 TO RG-H5
013590	       MOVE ALL "_"	 TO RG-H6
013600	       MOVE "ACCOUNT NAME"
				 TO RG-H7
013610	       MOVE ALL "_"	 TO RG-H8
013620	       MOVE "DATE"	 TO RG-H9
013630	       MOVE "CHEQUE NO"  TO RG-H10
013640	       MOVE ALL "_"	 TO RG-H16
013650	       MOVE "PAYEE/NARRATIVE"
				 TO RG-H17
013660	       MOVE ALL "_"	 TO RG-H18
013670	       MOVE "DEBIT"	 TO RG-H14
013680	       MOVE "CREDIT"	 TO RG-H15
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	   ELSE
	   IF GL-PDCHQ
	       PERFORM AA50GP.
019820	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

       DA45.
      *
      * 	   Switch the date into DDMMCCYY format
      * 	   (W22-DTE3 and W15-DATE)
      *
	     MOVE W22-CCYMD	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO W15-DATE.
	     MOVE W15-DATE	 TO W15-GRND.
	     MOVE BCH-CRED	 TO W15-ACNO.
	     MOVE BCH-CHQNO	 TO W15-GRNNO W15-REFNO.
019960	     MOVE BCH-PAYEE	 TO W15-PAYEE.
	     MOVE BCH-BANK	 TO W15-BANK.
	     MOVE BCH-BANK-NAME	 TO W15-BNKNME.
	     MOVE BCH-NARRATIVE-1
				 TO W15-NAR.
	     MOVE BCH-VALUE	 TO W15-TCOST.
020270	     MULTIPLY -1 BY BCH-VALUE
				 GIVING W15-CHQVAL.
	     MOVE BCH-CNAME	 TO W15-NAME.
	     MOVE 1		 TO WS-S8.

       DA46.
	     MOVE BCH-GL-AC(WS-S8)
				 TO W15-GL-AC(WS-S8).
	     MOVE BCH-GLAC-NAME(WS-S8)
				 TO W15-GLAC-NAME(WS-S8).
	     MOVE BCH-NARRATIVE(WS-S8)
				 TO W15-NARRATIVE(WS-S8).
	     MOVE BCH-GL-VALUE(WS-S8)
				 TO W15-AMOUNT(WS-S8).
	   IF WS-S8 < 10
	       ADD 1		 TO WS-S8
	       GO TO DA46.
	     MOVE 1		 TO WS-S8.
      *
      *    ****    I S	 T H I S   A   G E N E R A L   L E D G E R
      * 	   C H E Q U E	 ?
      *
	   IF GEN-LEDG OR GL-PDCHQ
	       GO TO DA200.
      *
      *    ****    I S	 T H E	 C H E Q U E   P O S T - D A T E D   ?
      *
	   IF W22-CYMD > W05-T-CYMD
	       GO TO DA100.
      *
      *    ****    P R O C E S S   C R E D I T O R   C H E Q U E
      *
	     PERFORM AE155-GET-CREDITOR.
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
020510	     MOVE 1		 TO WS-S2.
020520       MOVE ZERO           TO WS-S.

020530 DA48.
020540       MOVE ZERO           TO W25-CFLOW (WS-S2).
020550     IF W15-MONTH (WS-S2) > ZERO
020560         ADD 1             TO WS-S.
020570     IF WS-S2 < 6
020580         ADD 1             TO WS-S2
020590	       GO TO DA48.
020600       MOVE ZERO           TO W25-V1 W25-V2.
020610	     MOVE 1		 TO WS-S3.
020620     IF WS-S < 2
020630         MOVE W15-CHQVAL   TO W25-V2
020640	       GO TO DA50.
020650       MOVE W15-TCOST      TO W20-RESULT.
020660       DIVIDE W20-RESULT BY WS-S 
                                 GIVING W25-V1 ROUNDED.
020670       SUBTRACT 1          FROM WS-S.
020680       COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
020690       ADD 1               TO WS-S.
020700       MULTIPLY -1         BY W25-V1.
020710       MULTIPLY -1         BY W25-V2.

020720 DA50.
012750	     DISPLAY "Processing payment" AT 2520 WITH
012760		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
020730     IF WS-S = ZERO
020740	       GO TO DA60.

020750 DA55.
020760     IF W15-MONTH (WS-S3) = ZERO
020770         ADD 1             TO WS-S3
020780	       GO TO DA55.
020790     IF WS-S > 1
020800         MOVE W25-V1       TO W25-CFLOW (WS-S3)
020810         SUBTRACT 1        FROM WS-S
020820         ADD 1             TO WS-S3
020830	       GO TO DA55.
020840       MOVE W25-V2         TO W25-CFLOW (WS-S3).
020850       MOVE ZERO           TO W25-V1 W25-V2.
	     DISPLAY "Creditor" AT 2540
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15.
      *
      *    ****    S T A R T   R E C O V E R Y	 L O G
      *
020860 DA60.
020870       PERFORM AY60 THRU AY999.
020880       MOVE 0              TO WS-ACTION.
020890       PERFORM AY07 THRU AY59.

020980 DA65.
      *
      *    ****    G E N E R A T E   C R E D I T O R
      * 	   T R A N S A C T I O N
      *
	     INITIALIZE CT-REC.
021020       ADD 1               TO W70-CRTRAN.
021030	     MOVE W22-CYMD	 TO CT-DTE.
021040	     MOVE W15-GRNN	 TO CT-REF CT-SINV.
021050       MOVE P-NUMBER       TO CT-CRED.
021090       MOVE W15-CHQVAL     TO CT-COST.
021100       MOVE 03             TO CT-CODE.
021110	     MOVE 1		 TO WS-S WS-S2.
021120     IF W25-V2 NOT = ZERO
021130	       GO TO DA75.
      *
      *    ****    A G E I N G	 I N D I C A T O R   (B I N A R Y)
      *
021140 DA70.
021150     IF W15-MONTH (WS-S) NOT = ZERO
021160         ADD WS-S2         TO CT-AGE.
021170     IF WS-S < 6
021180         ADD 1             TO WS-S
021190         MULTIPLY 2        BY WS-S2
021200	       GO TO DA70.
      *
      *    ****    U P D A T E	 T H E	 C R E D I T O R   A C C O U N T
      *
021210 DA75.
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
      *
      *    ****    C R E D I T O R   S T A T I S T I C S
      *
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
      *
      *    ****    P R I N T   C R E D I T O R	 A U D I T   T R A I L
      *
021440	     MOVE W15-ACNO	 TO CH-ACNO.
021450       MOVE W15-NAME       TO CH-NAME.
021460       MOVE W15-GRNN       TO CH-CHQ.
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
021650	       PERFORM DA40.
021660       MOVE SPACES         TO R-DET.
	     MOVE W15-GL-AC(1)	 TO W15-CRED.
	     MOVE W15-NARRATIVE(1)
				 TO W15-NAR2.
	     MOVE W15-GLAC-NAME(1)
				 TO W15-ACNME.
	     MOVE W15-AMOUNT(1)  TO W15-VAL.
      *
      *    ****    A U D I T   T R A I L   T O T A L S
      *
021680       ADD W15-TCOST       TO W20-TOTAL.
021690       ADD W15-TCOST       TO W20-DEBIT.
022250	     DISPLAY CLEAR-L25.
012750	     DISPLAY "Updating Creditor G/L" AT 2520 WITH
012760		      BACKGROUND-COLOR 3
		      FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
022840	     MOVE W15-CRED	 TO G-AC.
022290       PERFORM AY12 THRU AY59.
      *
      *    ****    G E N E R A T E   C R E D I T O R   C O N T R O L
      * 	   T R A N S A C T I O N
      *
022310	     INITIALIZE T-REC.
012680	     MOVE W22-CYMD	 TO T-DATE.
022490	     MOVE W15-NAR2	 TO T-NARATIVE.
022500       MOVE W15-REFER      TO T-REFER.
022520       MOVE G-AC           TO T-AC.
022530       MOVE 03             TO T-CODE.
022540       ADD W15-VAL         TO G-DT.
022550       MOVE W15-VAL        TO T-VAL.
             MOVE W15-VAL        TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
	     MOVE "N"		 TO T-FLAG.
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
022690	     PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
      *
      *    ****    P R I N T   G / L E D G E R	 A U D I T   T R A I L
      *
022710	     MOVE G-AC		 TO R-ACC.
022720       MOVE G-NAME         TO R-ANME.
022730       MOVE W15-DATE       TO R-DTE2.
022740       MOVE W15-REFER      TO R-REF.
022750	     MOVE W15-NAR2	 TO R-NAR.
022760       MOVE W15-VAL        TO R-DBT.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
022810	       PERFORM DA40.
022820       MOVE SPACES         TO R-DET.
      *
      *    ****    U P D A T E	 B A N K   A C C O U N T
      *
012750	     DISPLAY "Updating Bank account" AT 2520 WITH
012760		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
022840	     MOVE W15-BANK	 TO G-AC.
022860       PERFORM AY12 THRU AY59.
022870*      MOVE G-GLTRAN	 TO WS-GTRKEY.
      *
      *    ****    G E N E R A T E   B A N K   T R A N S A C T I O N
      *
022880       INITIALIZE T-REC.
012680	     MOVE W22-CYMD	 TO T-DATE.
023060       MOVE W15-NAR        TO T-NARATIVE.
023070       MOVE W15-REFER      TO T-REFER.
023080*      ADD 1		 TO G-GLTRAN.
023100       MOVE G-AC           TO T-AC.
023110       MOVE 04             TO T-CODE.
023120       MULTIPLY -1 BY W15-VAL 
                                 GIVING W20-CREDIT.
023130       ADD W20-CREDIT      TO G-CR.
023150       MOVE W20-CREDIT     TO T-VAL.
             MOVE W20-CREDIT     TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
	     MOVE "N"		 TO T-FLAG.
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
023250	     PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
023310	       PERFORM DA40.
      *
      *    ****    P R I N T   G / L E D G E R	 A U D I T   T R A I L
      *
023320	     MOVE G-AC		 TO R-ACC.
023330       MOVE G-NAME         TO R-ANME.
023340       MOVE W15-DATE       TO R-DTE2.
023350       MOVE W15-REFER      TO R-REF.
023360       MOVE W15-PAYEE      TO R-NAR.
023370       MOVE W20-CREDIT     TO R-CRD.
023400       PERFORM AY70 THRU AY999.
	     PERFORM DELETE-CHQBATCH-REC THRU WRITE-CHQBATCH-EXIT.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
023460	       PERFORM DA40.
023470	     MOVE SPACES	 TO R-DET.
023500	     DISPLAY CLEAR-L25.
023610	     GO TO DA35.
      *
      *    ****    P R I N T   A U D I T   T R A I L   T O T A L S
      *
       DA80.
	   IF CR-PDCHQ
	       GO TO DA105
	   ELSE
	   IF GEN-LEDG
	       GO TO DA225
	   ELSE
	   IF GL-PDCHQ
	       GO TO DA310.
023620       MOVE "BATCH TOTAL"  TO R-NAR.
023630       MOVE W20-TOTAL      TO R-DBT.
023640       MULTIPLY -1 BY W20-TOTAL 
                                 GIVING R-CRD.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
016830	     MOVE SPACES	 TO R-DET.
	     PERFORM RESTORE-SCREEN-3.
	     GO TO DA15.
      *
      *    ****    C R E D I T O R   P D   C H E Q U E
      *
       DA100.
	     MOVE W15-GL-AC(1)	 TO W15-CRED.
	     MOVE W15-NARRATIVE(1)
				 TO W15-NAR2.
	     MOVE W15-GLAC-NAME(1)
				 TO W15-ACNME.
	     MOVE W15-AMOUNT(1)  TO W15-VAL.
	     INITIALIZE PDT-REC.
	     MOVE W15-ACNO	 TO PD-ACNO PDT-AC.
	     MOVE P-NAME	 TO PD-NAME.
	     MOVE W22-CYMD	 TO PDT-DATE.
	     MOVE W15-GRND	 TO PD-DATE.
	     MOVE W15-REFNO	 TO PD-REF PDT-REFER.
	     MOVE W15-BANK	 TO PD-BANK PDT-BANK.
	     MOVE W15-CRED	 TO PD-CRED PDT-CRED.
	     MOVE W15-NAR2	 TO PD-NAR PDT-NAR.
	     MOVE W15-TCOST	 TO PD-VALUE PDT-VAL.
	     ADD W15-TCOST	 TO W20-TOTAL.
	     PERFORM WRITE-PDCHEQ THRU WRITE-PDCHEQ-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE SPACES	 TO WS-ERR-STRING
	       STRING "Cheque duplicated: "
			    DELIMITED SIZE
		       W15-REFNO DELIMITED SIZE
				 INTO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA35.
	     PERFORM AY60 THRU AY999.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY44 THRU AY50.
	     PERFORM AY70 THRU AY999.
	     PERFORM DELETE-CHQBATCH-REC THRU WRITE-CHQBATCH-EXIT.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM AA50CP.
	     MOVE SPACES	 TO R-DET.
	     GO TO DA35.

       DA105.
	     MOVE SPACES	 TO R-DET.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
023620	     MOVE "****  TOTAL ****"
				 TO PD-NAR.
023630	     MOVE W20-TOTAL	 TO PD-VALUE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     GO TO DA20.
      *
      *    ****    G / L E D G E R   C H E Q U E S
      *
       DA200.
      *
      *    ****    I S	 T H E	 C H E Q U E   P O S T - D A T E D   ?
      *
	   IF W22-CYMD > W05-T-CYMD
	       GO TO DA300.
022250	     DISPLAY CLEAR-L25.
012750	     DISPLAY "Updating G/Ledger A/C" AT 2520 WITH
012760		      BACKGROUND-COLOR 3
		      FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
      *
      *    ****    M O R E   A L L O C A T I O N S   ?
      *
       DA205.
	   IF W15-GL-AC (WS-S8) = ZERO
	       GO TO DA210.
	     MOVE W15-GL-AC(WS-S8)
				 TO W15-CRED.
	     MOVE W15-NARRATIVE(WS-S8)
				 TO W15-NAR2.
	     MOVE W15-GLAC-NAME(WS-S8)
				 TO W15-ACNME.
	     MOVE W15-AMOUNT(WS-S8)
				 TO W15-VAL.
	     PERFORM AY60 THRU AY999.
022840	     MOVE W15-CRED	 TO G-AC.
022290       PERFORM AY12 THRU AY59.
      *
      *    ****    G E N E R A T E   G / L E D G E R   A C C O U N T
      * 	   T R A N S A C T I O N
      *
022310	     INITIALIZE T-REC.
012680	     MOVE W22-CYMD	 TO T-DATE.
022490	     MOVE W15-NAR2	 TO T-NARATIVE.
022500       MOVE W15-REFER      TO T-REFER.
022520       MOVE G-AC           TO T-AC.
022530       MOVE 03             TO T-CODE.
015380	   IF W15-VAL > ZERO
015390	       ADD W15-VAL	 TO G-DT W20-DEBIT
015400     ELSE
015410	       ADD W15-VAL	 TO G-CR W20-TOTAL.
022550	     MOVE W15-VAL	 TO T-VAL.
             MOVE W15-VAL        TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
	     MOVE "N"		 TO T-FLAG.
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
022690	     PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
      *
      *    ****    G / L E D G E R   A U D I T	 T R A I L
      *
015660	     MOVE G-AC		 TO R-ACC.
015670	     MOVE G-NAME	 TO R-ANME.
015680	     MOVE W15-DATE	 TO R-DTE2.
015690	     MOVE W15-REFER	 TO R-REF.
015700	     MOVE W15-NAR2	 TO R-NAR.
015710     IF W15-VAL > ZERO
015720	       MOVE W15-VAL	 TO R-DBT
015730     ELSE
015740	       MOVE W15-VAL	 TO R-CRD.
015750	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
015790	       PERFORM DA40.
015800	     MOVE SPACES	 TO R-DET.
	   IF WS-S8 < 10
	       ADD 1		 TO WS-S8
	       GO TO DA205.
      *
      *    ****    U P D A T E	 B A N K   A C C O U N T
      *
       DA210.
012750	     DISPLAY "Updating Bank account" AT 2520 WITH
012760		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
022840	     MOVE W15-BANK	 TO G-AC.
022860       PERFORM AY12 THRU AY59.
022870*      MOVE G-GLTRAN	 TO WS-GTRKEY.
      *
      *    ****    G E N E R A T E   B A N K   T R A N S A C T I O N
      *
022880       INITIALIZE T-REC.
012680	     MOVE W22-CYMD	 TO T-DATE.
023060       MOVE W15-NAR        TO T-NARATIVE.
023070       MOVE W15-REFER      TO T-REFER.
023080*      ADD 1		 TO G-GLTRAN.
023100       MOVE G-AC           TO T-AC.
023110       MOVE 04             TO T-CODE.
023120	     MULTIPLY -1 BY W15-TCOST
                                 GIVING W20-CREDIT.
023130	     ADD W20-CREDIT	 TO G-CR W20-TOTAL.
023150       MOVE W20-CREDIT     TO T-VAL.
             MOVE W20-CREDIT     TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
	     MOVE "N"		 TO T-FLAG.
	     PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
016170       ADD W20-CREDIT      TO G-BAL.
016180       MOVE W20-CREDIT     TO T-VAL.
015970       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015920       PERFORM AY13 THRU AY59.
016220       MOVE W15-PAYEE      TO T-NARATIVE.
016230       MOVE 5              TO T-SUB.
016250       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
015920       PERFORM AY13 THRU AY59.
016360	     PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
      *
      *    ****    G / L E D G E R   A U D I T	 T R A I L
      *
016430	     MOVE G-AC		 TO R-ACC.
016440	     MOVE G-NAME	 TO R-ANME.
016450	     MOVE W15-DATE	 TO R-DTE2.
016460	     MOVE W15-REFER	 TO R-REF.
016470	     MOVE W15-PAYEE	 TO R-NAR.
016480	     MOVE W20-CREDIT	 TO R-CRD.
015710	   IF W20-CREDIT > ZERO
015720	       MOVE W20-CREDIT	 TO R-DBT
015730     ELSE
015740	       MOVE W20-CREDIT	 TO R-CRD.
016560	     PERFORM AY70 THRU AY999.
	     PERFORM DELETE-CHQBATCH-REC THRU WRITE-CHQBATCH-EXIT.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
016530	       PERFORM DA40.
016540	     MOVE SPACES	 TO R-DET.
	     GO TO DA35.

       DA225.
016770	     MOVE "BATCH TOTAL"  TO R-NAR.
016780	     MOVE W20-DEBIT	 TO R-DBT.
016790	     MOVE W20-TOTAL	 TO R-CRD.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     GO TO DA25.
      *
      *    ****    G / L E D G E R   P D   C H E Q U E S
      *
       DA300.
	     INITIALIZE GPD-REC.
	     MOVE W22-CYMD	 TO GPD-DATE.
	     MOVE W22-DMCY	 TO PG-DATE.
	     MOVE W15-REFNO	 TO PG-REF GPD-REFER.
	     MOVE W15-BANK	 TO PG-GL-AC GPD-BANK.
	     MOVE W15-BNKNME	 TO PG-AC-NAME.
	     MOVE W15-NAR	 TO GPD-NAR1.
	     MOVE W15-TCOST	 TO PG-D-VALUE GPD-VAL.
019960	     MOVE W15-PAYEE	 TO PG-PAYEE GPD-PAYEE.
	     MOVE 1		 TO WS-S8.

       DA302.
	     MOVE W15-GL-AC(WS-S8)
				 TO GPD-GL-AC(WS-S8).
	     MOVE W15-NARRATIVE(WS-S8)
				 TO GPD-NARRATIVE(WS-S8).
	     MOVE W15-AMOUNT(WS-S8)
				 TO GPD-GL-VALUE(WS-S8).
	   IF WS-S8 < 10
	       ADD 1		 TO WS-S8
	       GO TO DA302.
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
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	     PERFORM AY70 THRU AY999.
	   IF W15-NAR  = W15-NAR2
	       GO TO DA305.
	     MOVE W15-NAR	 TO PG-PAYEE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.

       DA305.
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
	       PERFORM AA50CP.
	     MOVE SPACES	 TO R-DET.
	     GO TO DA35.

       DA310.
016770	     MOVE "TOTAL"	 TO PG-H1.
016780	     MOVE W20-TOTAL	 TO PG-D-VALUE.
016790	     MULTIPLY -1 BY W20-TOTAL
				 GIVING PG-C-VALUE.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

       DA980.
	     MOVE "Processing complete - Press ANY key"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	     GO TO DA999.

       DA990.
	     MOVE "No entries found to be posted"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.

023690 DA999.
023700       EXIT.

      /
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
	     MOVE "CHQBATCH"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CHQBATCH.
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
	     MOVE "GPDCHQ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GPDCHQ.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "PDCHEQ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PDCHEQ.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.

       ZA00B.
	     OPEN I-O CHQBATCH.
	   IF WS-STATUS = "05"
	       CLOSE CHQBATCH
	       OPEN OUTPUT CHQBATCH
	       CLOSE CHQBATCH
	       OPEN I-O CHQBATCH
	       INITIALIZE BCH-REC1
	       MOVE ZERO	 TO BCH-KEY
	       MOVE 1		 TO BCH-DATE
	       PERFORM WRITE-CHQBATCH THRU WRITE-CHQBATCH-EXIT
	       CLOSE CHQBATCH
	       GO TO ZA00B.
042250	   IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 58	 TO WS-F-ERROR
                   GO TO ZA50.
050190	   IF WS-STATUS NOT = "00"
050200	       MOVE 58		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
042240	     OPEN I-O CREDIT.
042250	   IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 3        TO WS-F-ERROR
                   GO TO ZA50.
050190     IF WS-STATUS NOT = "00"
050200         MOVE 3            TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
042210	     OPEN I-O CTRANS.
042220	   IF RUNTIME-ERROR
	       IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
050190     IF WS-STATUS NOT = "00"
050200         MOVE 4            TO WS-F-ERROR
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
023900	     OPEN I-O GACCNT.
023910	   IF RUNTIME-ERROR
	       IF FLE-LOCKED
041880		   GO TO ZA200
               ELSE
               IF FLE-LIMIT
		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 8	 TO WS-F-ERROR
		   GO TO ZA50.
035500     IF WS-STATUS NOT = "00"
035510	       MOVE 8		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
023930	     OPEN I-O GLTRAN.
023940	   IF RUNTIME-ERROR
	       IF FLE-LOCKED
041880		   GO TO ZA200
	       ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
035560     IF WS-STATUS NOT = "00"
035570	       MOVE 11		 TO WS-F-ERROR
050480         PERFORM OPEN-ERROR.
019900	     OPEN I-O GPDCHQ.
	   IF WS-STATUS = "05"
	       CLOSE GPDCHQ
	       OPEN OUTPUT GPDCHQ
	       CLOSE GPDCHQ
	       OPEN I-O GPDCHQ.
019910	   IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 45	 TO WS-F-ERROR
                   GO TO ZA50.
050190	   IF WS-STATUS NOT = "00"
050200	       MOVE 45		  TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
019900	     OPEN I-O PDCHEQ.
	   IF WS-STATUS = "05"
	       CLOSE PDCHEQ
	       OPEN OUTPUT PDCHEQ
	       CLOSE PDCHEQ
	       OPEN I-O PDCHEQ.
019910	   IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 44	 TO WS-F-ERROR
                   GO TO ZA50.
050190	   IF WS-STATUS NOT = "00"
050200	       MOVE 44		  TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.

       ZA01C.
024050	     OPEN OUTPUT RECOVER.
024060	     CLOSE RECOVER.
024070	     OPEN I-O RECOVER.
042360       MOVE 1              TO WS-PARKEY.
042370       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042380       MOVE PAR-COMPANY    TO W90-COMP.
042390       MOVE PAR-DMY        TO W05-TODAY.
042400	     MOVE PAR-YMD	 TO W05-TYMD
				    W05-T-CYMD.
      *	   IF W05-YY > 55
      *
      *    ****   ASSUME 1900
      *
      *	       MOVE 19		 TO W05-CC
      *	   ELSE
      *
      *    ****   ASSUME 2000
      *
      *	       MOVE 20		 TO W05-CC.
050520       MOVE 5              TO WS-PARKEY.
050530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050550*       MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
      *	     MOVE PAR-RATE-DATE	 TO WS-VAT-DATE.
      *       MOVE W05-VAT-R1	  TO W05-RATE.
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
035780	     MOVE G-BATCH	 TO WS-BATCH.
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
	       SUBTRACT 1 FROM W05-YY
			       GIVING WS-L-END.
	   IF G-OPEN = ZERO
	       MOVE WS-Y-END	 TO G-OPEN.
	     MOVE G-OPEN	 TO WS-OPEN.
024380	     GO TO ZA999.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

039850 ZA50.
041650	   IF WS-F-ERROR = 3
041660	       MOVE "CREDITOR"	 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 8
041860	       MOVE "G/L ACCOUNT file"
				 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 44
041860	       MOVE "Cr PD CHEQUE file"
				 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 45
041860	       MOVE "G/L PD CHEQUE file"
				 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 58
041860	       MOVE "CHEQUE BATCH file"
				 TO WS-FILE.
039980	     DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

040020 ZA51.
040030	     EXIT PROGRAM.

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
	       MOVE "Supervisor�"  TO WS-WRKHD
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

024470 ZA999.
024480       EXIT.
      /
024490 ZB000	     SECTION 91.
      
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
               MOVE P-NUMBER     TO WS-KEYX
041680     ELSE
041690     IF WS-F-ERROR = 4
041700	       MOVE W02-CRTRAN	 TO WS-FILE
041710	       MOVE ZERO	 TO WS-KEY
	       MOVE CT-KEY	 TO WS-KEYX
041840	   ELSE
041810     IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE G-AC	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 11
041860	       MOVE W02-GLTRAN	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 44
041860	       MOVE W02-PDCHEQ	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE PDT-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 45
041860	       MOVE W02-GPDCHQ	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE GPD-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 58
041860	       MOVE W02-CHQBATCH TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE BCH-KEY	 TO WS-KEYX.
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
