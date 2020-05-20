      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******     ****     *
      *   **	**     **     **    **	 **  **   **	**   **  **    *
      *   **	**     **     **    **	**    **	**  **	  **   *
      *   **	**     **     *******	**    **     ****   **	  **   *
      *   **	**     **     **	**    **	**  **	  **   *
      *   **	**     **     **	 **  **   **	**   **  **    *
      *   *******      **     **	  ****	   ******     ****     *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R	  D E T A I L E D   A C C O U N T S   L I S T  *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      *  October 2008	- Amended Accounts receivable ageing to create *
      * 		  duplicate files of the accounts and trans-   *
      * 		  actions for printing of copy satements when  *
      * 		  and if required. The file names are AR_Ayymm *
      * 		  and AR_Tyymm, where yymm is replaced with    *
      * 		  the year and month.			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP030.
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

       COPY DBHIST.SL.

000140 COPY DEBTOR.ISL.

000160 COPY DBTRAN.SL.

000140 COPY DEBMEM.SL.

       COPY DTHIST.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000250 COPY PARAM.FDE.

       COPY DBHIST.FDE.

000270 COPY DEBTOR.FDE.

000290 COPY DBTRAN.FDE.

000140 COPY DEBMEM.FDE.

       COPY DTHIST.FDE.

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
002790 77  WS-SUB-1	   PIC	9(04)	  COMP-5.
002800 77  WS-SUB-2	   PIC	9(04)	  COMP-5.
002810 77  WS-SUB-3	   PIC	9(04)	  COMP-5.
002820 77  WS-SUB-4	   PIC	9(04)	  COMP-5.
002830 77  WS-SUB-5	   PIC	9(04)	  COMP-5.
002840 77  WS-SUB-6	   PIC	9(04)	  COMP-5.
002850 77  WS-IXS1	   PIC	9(04)	  COMP-5.
002860 77  WS-IXS2	   PIC	9(04)	  COMP-5.
002870 77  WS-IXS3	   PIC	9(04)	  COMP-5.
002880 77  WS-IXS4	   PIC	9(04)	  COMP-5.
002890 77  WS-MSUB	   PIC	9(04)	  COMP-5.
002900 77  WS-PARKEY	   PIC	9(04)	  COMP-5.
002910 77  WS-DEPKEY	   PIC	9(04)	  COMP-5.
002930 77  WS-NETKEY	   PIC	9(06)	  COMP-5.
002940 77  WS-WRKEY	   PIC	9(04)	  COMP-5.
002960 77  WS-NTP	   PIC	9(01).
002970 77  WS-LINES	   PIC	9(02).
002980 77  WS-PAGE	   PIC	9(04)	  COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002990 77  WS-OPTION	   PIC	X(01).
002990 77  WS-AGE	   PIC	X(01).
002990 77  WS-SEQ	   PIC	X(01).
003000 77  WS-CREDIT	   PIC	X(01).
002990 77  WS-ZERO	   PIC	X(01).
       77  WS-NUM	   PIC	Z9.
003020 77  WS-ADDR	   PIC	X(01).
       77  WS-REF	   PIC	X(01).
003030 77  WS-ERROR	   PIC	9(01).
003040 77  WS-ERROR1	   PIC	9(01).
003110 77  WS-EXPAND	   PIC	X(01)	 VALUE " ".
003180 77  WS-COMP	   PIC	9(01).
003180 77  WS-BDT	   PIC	9(01).
003190 77  WS-BAD	   PIC	X(01).
003180 77  WS-STM	   PIC	9(01).
003190 77  WS-TYPE	   PIC	X(01).
       77  WS-PERIOD	   PIC	X(01).
       88  PREVIOUS-PERIOD		 VALUE "P".
003240 77  WS-MESS	   PIC	X(02).
003250 77  WS-ACNO	   PIC	X(06).
003260 77  WS-S1	   PIC	9(04)	  COMP-5.
003270 77  WS-S2	   PIC	9(04)	  COMP-5.
003280 77  WS-S3	   PIC	9(04)	  COMP-5.
003290 77  WS-S4	   PIC	9(04)	  COMP-5.
003300 77  WS-S5	   PIC	9(04)	  COMP-5.
003310 77  WS-S6	   PIC	9(04)	  COMP-5.
003320 77  WS-S7	   PIC	9(04)	  COMP-5.
003330 77  WS-S8	   PIC	9(04)	  COMP-5.
003420 77  WS-AC	   PIC	X(07) VALUE "Account".
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

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STM".
	   03  WS-PROG	       PIC  X(03) VALUE "F06".

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

       COPY DBHIST.ID.

       COPY DEBTOR.ID.

       COPY DBTRAN.ID.

000140 COPY DEBMEM.ID.

       COPY DTHIST.ID.

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
000350	   03  REP-LINE1 REDEFINES W02-PRN-LINE.
000360	       05  REP-DETAIL1 PIC  X(136).
000370	   03  REP-LINE2 REDEFINES W02-PRN-LINE.
000380	       05  REP-DETAIL2 PIC  X(80).
000390	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
000400	       05  REP-PAGE    PIC  X(06).
000410	       05  REP-P-NO    PIC  Z(03)9.
000420	       05  FILLER      PIC  X(10).
000430	       05  REP-COMPANY PIC  X(40).
000440	       05  FILLER      PIC  X(04).
000450	       05  REP-DATE    PIC  X(06).
000460	       05  REP-DTE     PIC  Z9/99/9999.
	   03  REP-LINE3A REDEFINES W02-PRN-LINE.
	       05  REP-CO      PIC  X(40).
000470	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
000480	       05  FILLER      PIC  X(11).
000490	       05  REP-HEAD1   PIC  X(04).
000500	       05  FILLER      PIC  X(06).
000510	       05  REP-HEAD2   PIC  X(15).
000520	       05  FILLER      PIC  X(06).
000530	       05  REP-HEAD3   PIC  X(15).
000540	       05  FILLER      PIC  X(06).
000550	       05  REP-HEAD4   PIC  X(06).
000560	       05  FILLER      PIC  X(11).
000570	   03  REP-LINE5 REDEFINES W02-PRN-LINE.
000580	       05  FILLER      PIC  X(12).
000590	       05  REP-TRAN    PIC  9(02).
000600	       05  FILLER      PIC  X(09).
000610	       05  REP-ENG     PIC  X(12).
000620	       05  FILLER      PIC  X(09).
000630	       05  REP-AFR     PIC  X(12).
000640	       05  FILLER      PIC  X(10).
000650	       05  REP-ACT     PIC  X(01).
000660	       05  FILLER      PIC  X(13).
000670	   03  REP-LINE6 REDEFINES W02-PRN-LINE.
000680	       05  FILLER      PIC  X(06).
000690	       05  REP-HEAD5   PIC  X(07).
000700	       05  REP-BATCH   PIC  Z(04)9.
000710	       05  FILLER      PIC  X(04).
000720	       05  REP-HEAD6   PIC  X(20).
000730	       05  FILLER      PIC  X(38).
000740	   03  REP-LINE7 REDEFINES W02-PRN-LINE.
000750	       05  REP-HEAD7   PIC  X(07).
000760	       05  REP-HEAD8   PIC  X(44).
000770	       05  REP-HEAD9   PIC  X(09).
000780	       05  REP-HEAD10  PIC  X(14).
000790	       05  REP-HEAD11  PIC  X(06).
000800	   03  REP-LINE8 REDEFINES W02-PRN-LINE.
000810	       05  REP-TAC     PIC  X(06).
000820	       05  FILLER      PIC  X(01).
000830	       05  REP-TNAME   PIC  X(40).
000840	       05  FILLER      PIC  X(01).
000850	       05  REP-TDTE    PIC  Z9/99/9999.
000860	       05  FILLER      PIC  X(01).
000870	       05  REP-TREF    PIC  X(08).
000880	       05  FILLER      PIC  X(02).
000890	       05  REP-TVAL    PIC  Z(06)9.99-.
000900	   03  REP-LINE9 REDEFINES W02-PRN-LINE.
000910	       05  FILLER      PIC  X(54).
000920	       05  REP-BTOT    PIC  Z(06)9.99-.
000930	       05  FILLER      PIC  X(15).
001080	   03  TRL-LINE1 REDEFINES W02-PRN-LINE.
001090	       05  TRL-DET     PIC  X(136).
001100	   03  TRL-LINE2 REDEFINES W02-PRN-LINE.
001110	       05  TRL-HEAD1   PIC  X(07).
001120	       05  TRL-HEAD2   PIC  X(34).
001130	       05  TRL-HEAD3   PIC  X(08).
001140	       05  TRL-HEAD4   PIC  X(07).
001150	       05  TRL-HEAD5   PIC  X(14).
001160	       05  TRL-HEAD6   PIC  X(10).
001170	       05  TRL-HEAD7   PIC  X(14).
001180	       05  TRL-HEAD8   PIC  X(14).
001190	       05  TRL-HEAD9   PIC  X(11).
001200	       05  TRL-HEAD10  PIC  X(09).
001210	       05  TRL-HEAD11  PIC  X(08).
001230	   03  TRL-LINE3 REDEFINES W02-PRN-LINE.
001240	       05  TRL-AC      PIC  X(06).
001250	       05  FILLER      PIC  X(01).
001260	       05  TRL-NME     PIC  X(31).
001270	       05  FILLER      PIC  X(01).
001280	       05  TRL-ACT     PIC  Z9/99/9999.
001290	       05  FILLER      PIC  X(01).
001300	       05  TRL-MTH     PIC  9(01).
001310	       05  FILLER      PIC  X(02).
001420	       05  TRL-PD      PIC  Z(07)9.99-.
001330	       05  FILLER      PIC  X(01).
001460	       05  TRL-BAL     PIC  Z(07)9.99-.
001330	       05  FILLER      PIC  X(02).
001460	       05  TRL-STM     PIC  Z(07)9.99-.
001330	       05  FILLER      PIC  X(02).
	       05  TRL-LST-PAY PIC  Z(07)9.99-.
001330	       05  FILLER      PIC  X(01).
001280	       05  TRL-PAY     PIC  Z9/99/9999.
001330	       05  FILLER      PIC  X(02).
001400	       05  TRL-INT     PIC  Z(04)9.99.
001410	       05  FILLER      PIC  X(01).
001440	       05  TRL-LIMIT   PIC  Z(06)9.
001450	       05  TRL-EXL     PIC  X(01).
001470	   03  TRL-LINE4 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(10).
001220	       05  TRL-HEAD12  PIC  X(07).
001320	       05  TRL-MTD     PIC  Z(07)9.99-.
001330	       05  FILLER      PIC  X(01).
001220	       05  TRL-HEAD13  PIC  X(09).
001320	       05  TRL-CUR     PIC  Z(07)9.99-.
001330	       05  FILLER      PIC  X(01).
001220	       05  TRL-HEAD14  PIC  X(09).
001340	       05  TRL-30      PIC  Z(07)9.99.
001350	       05  FILLER      PIC  X(01).
001220	       05  TRL-HEAD15  PIC  X(09).
001360	       05  TRL-60      PIC  Z(07)9.99.
001370	       05  FILLER      PIC  X(01).
001220	       05  TRL-HEAD16  PIC  X(09).
001380	       05  TRL-90      PIC  Z(07)9.99.
001390	       05  FILLER      PIC  X(01).
001220	       05  TRL-HEAD17  PIC  X(10).
001380	       05  TRL-120     PIC  Z(07)9.99.
001470	   03  TRL-LINE5 REDEFINES W02-PRN-LINE.
001480	       05  FILLER      PIC  X(28).
001490	       05  TRL-HEADER  PIC  X(24).
001500	       05  FILLER      PIC  X(28).
001510	   03  TRL-LINE6 REDEFINES W02-PRN-LINE.
001520	       05  FILLER      PIC  X(10).
001530	       05  TRL-SUM     PIC  X(20).
001540	       05  TRL-VAL     PIC  Z(06)9.99-.
001550	       05  FILLER      PIC  X(39).
001560	   03  TRL-LINE7 REDEFINES W02-PRN-LINE.
001570	       05  TRL-PAGE    PIC  X(06).
001580	       05  TRL-P-NO    PIC  Z(03)9.
001590	       05  FILLER      PIC  X(24).
001600	       05  TRL-RESTART.
001610		   07  TRL-COMPANY  PIC X(40).
001620		   07  TRL-HEAD     PIC X(24).
001630	       05  FILLER      PIC  X(17).
001640	       05  TRL-DATE    PIC  X(06).
001650	       05  TRL-DTE     PIC  Z9/99/9999.
	   03  TRL-LINE7A REDEFINES W02-PRN-LINE.
	       05  TRL-EXPP    PIC  X(06).
	       05  FILLER      PIC  X(06).
	       05  TRL-CO      PIC  X(30).
	       05  TRL-EXHEAD  PIC  X(24).
	       05  TRL-EXPC    PIC  X(06).
001660	   03  TRL-LINE8 REDEFINES W02-PRN-LINE.
001670	       05  FILLER      PIC  X(07).
001680	       05  TRL-PCDE    PIC  9(04).
001690	       05  FILLER      PIC  X(02).
001700	       05  TRL-HEAD18  PIC  X(10).
001710	       05  TRL-HTEL    PIC  X(14).
001720	       05  FILLER      PIC  X(02).
001730	       05  TRL-BTEL    PIC  X(14).
001740	       05  FILLER      PIC  X(79).
001750	   03  CAT-L1 REDEFINES W02-PRN-LINE.
001760	       05  CAT-H1      PIC  X(10).
001770	       05  CAT-CODE    PIC  Z9.
001780	       05  FILLER      PIC  X(02).
001790	       05  CAT-DESC    PIC  X(30).
001800	       05  FILLER      PIC  X(88).
001810	   03  LST-L1 REDEFINES W02-PRN-LINE.
001820	       05  LST-H1      PIC  X(07).
001830	       05  LST-H2      PIC  X(09).
001840	       05  FILLER      PIC  X(06).
001850	       05  LST-H3      PIC  X(37).
001860	       05  FILLER      PIC  X(21).
001870	   03  LST-L2 REDEFINES W02-PRN-LINE.
001880	       05  LST-AC      PIC  X(06).
001890	       05  FILLER      PIC  X(10).
001900	       05  LST-NME     PIC  X(42).
001910	       05  LST-H4      PIC  X(06).
001920	       05  LST-LANG    PIC  X(02).
001930	       05  LST-H5      PIC  X(07).
001940	       05  LST-TIT     PIC  X(07).
001950	   03  LST-L3 REDEFINES W02-PRN-LINE.
001960	       05  FILLER      PIC  X(16).
001970	       05  LST-ADD     PIC  X(60).
001980	       05  FILLER      PIC  X(04).
001990	   03  LST-L4 REDEFINES W02-PRN-LINE.
002000	       05  FILLER      PIC  X(16).
002010	       05  LST-PC      PIC  9(04).
002020	       05  FILLER      PIC  X(01).
002030	       05  LST-H6      PIC  X(07).
002040	       05  LST-TEL     PIC  X(13).
002050	       05  LST-H7      PIC  X(07).
002060	       05  LST-HTEL    PIC  X(13).
002070	       05  LST-H8      PIC  X(06).
002080	       05  LST-ID      PIC  X(13).
002090	   03  LST-L5 REDEFINES W02-PRN-LINE.
002100	       05  FILLER      PIC  X(41).
002110	       05  LST-H9      PIC  X(05).
002120	       05  LST-TAX     PIC  X(14).
002130	       05  FILLER      PIC  X(20).
001100	   03  AGE-LINE2 REDEFINES W02-PRN-LINE.
001110	       05  AGE-HEAD1   PIC  X(06).
001120	       05  AGE-HEAD2   PIC  X(44).
001130	       05  AGE-HEAD3   PIC  X(13).
001140	       05  AGE-HEAD4   PIC  X(11).
001150	       05  AGE-HEAD5   PIC  X(12).
001160	       05  AGE-HEAD6   PIC  X(12).
001170	       05  AGE-HEAD7   PIC  X(11).
001180	       05  AGE-HEAD8   PIC  X(12).
001190	       05  AGE-HEAD9   PIC  X(11).
001230	   03  AGE-LINE3 REDEFINES W02-PRN-LINE.
001240	       05  AGE-AC      PIC  X(06).
001250	       05  FILLER      PIC  X(01).
001260	       05  AGE-NME     PIC  X(39).
001270	       05  FILLER      PIC  X(01).
001460	       05  AGE-BAL     PIC  Z(06)9.99-.
001290	       05  FILLER      PIC  X(01).
001300	       05  AGE-MTD     PIC  Z(06)9.99-.
001310	       05  FILLER      PIC  X(01).
001320	       05  AGE-CUR     PIC  Z(06)9.99-.
001330	       05  FILLER      PIC  X(01).
001340	       05  AGE-30      PIC  Z(06)9.99-.
001350	       05  FILLER      PIC  X(01).
001360	       05  AGE-60      PIC  Z(06)9.99-.
001370	       05  FILLER      PIC  X(01).
001380	       05  AGE-90      PIC  Z(06)9.99-.
001390	       05  FILLER      PIC  X(01).
001400	       05  AGE-INT     PIC  Z(06)9.99-.
001450	       05  FILLER      PIC  X(02).
001100	   03  ALF-LINE1 REDEFINES W02-PRN-LINE.
001110	       05  ALF-HEAD1   PIC  X(07).
001120	       05  ALF-HEAD2   PIC  X(42).
001130	       05  ALF-HEAD3   PIC  X(62).
001140	       05  ALF-HEAD4   PIC  X(06).
001150	       05  ALF-HEAD5   PIC  X(15).
001100	   03  ALF-LINE2 REDEFINES W02-PRN-LINE.
001110	       05  ALF-AC      PIC  X(06).
001450	       05  FILLER      PIC  X(01).
001120	       05  ALF-NME     PIC  X(40).
001450	       05  FILLER      PIC  X(02).
001130	       05  ALF-ADDR    PIC  X(60).
001450	       05  FILLER      PIC  X(02).
001140	       05  ALF-PCDE    PIC  9(04).
001450	       05  FILLER      PIC  X(02).
001150	       05  ALF-ID      PIC  X(14).
001450	       05  FILLER      PIC  X(01).
	   03  PUR-L1 REDEFINES W02-PRN-LINE.
	       05  PUR-H1      PIC  X(07).
	       05  PUR-H2      PIC  X(43).
	       05  PUR-H3      PIC  X(12).
	       05  PUR-H4      PIC  X(12).
	       05  PUR-H5      PIC  X(12).
	       05  PUR-H6      PIC  X(12).
	       05  PUR-H7      PIC  X(12).
	       05  PUR-H8      PIC  X(14).
	       05  PUR-H9      PIC  X(08).
	   03  PUR-L2 REDEFINES W02-PRN-LINE.
	       05  PUR-AC      PIC  X(06).
	       05  FILLER      PIC  X(01).
	       05  PUR-NME     PIC  X(40).
	       05  FILLER      PIC  X(01).
	       05  PUR-VAL1    PIC  Z(07)9.99-.
	       05  PUR-VAL2    PIC  Z(07)9.99-.
	       05  PUR-VAL3    PIC  Z(07)9.99-.
	       05  PUR-VAL4    PIC  Z(07)9.99-.
	       05  PUR-VAL5    PIC  Z(07)9.99-.
	       05  PUR-VAL6    PIC  Z(07)9.99-.
	       05  PUR-TOTAL   PIC  Z(07)9.99-.
002140	   03  STM-LINE1 REDEFINES W02-PRN-LINE.
002150	       05  STM-DET	PIC X(96).
002140	   03  ST7-LINE1 REDEFINES W02-PRN-LINE.
002150	       05  ST7-DET	PIC X(96).
002160	   03  ST7-LINE2 REDEFINES W02-PRN-LINE.
002170	       05  ST7-COMP.
002180		   07  ST7-EXCOMP  PIC X(36).
002190		   07  FILLER	   PIC X(36).
002200	       05  FILLER	   PIC X(24).
002160	   03  ST7-LINE2A REDEFINES W02-PRN-LINE.
002170	       05  FILLER	   PIC	X(15).
002180	       05  ST7-HEADER	   PIC	X(17).
002200	       05  FILLER	   PIC	X(64).
002230	   03  ST7-LINE3 REDEFINES W02-PRN-LINE.
002240	       05  ST7-ADD	   PIC	X(30).
002250	       05  FILLER	   PIC	X(12).
002750	       05  ST7-TEL	   PIC	X(19).
002760	       05  ST7-TNO	   PIC	X(14).
002220	       05  FILLER	   PIC	X(21).
002290	   03  ST7-LINE4 REDEFINES W02-PRN-LINE.
002350	       05  ST7-PC1	   PIC	9(04) BLANK WHEN ZERO.
002350	       05  ST7-PC2	   PIC	9(04) BLANK WHEN ZERO.
	       05  ST7-AH1	   PIC	X(12).
	       05  ST7-AC1	   PIC	X(06).
	       05  FILLER	       PIC  X(17).
	       05  ST7-AH2	       PIC  X(06).
	       05  ST7-DATE	   PIC	Z9/99/9999.
	       05  FILLER	       PIC  X(16).
	       05  ST7-AH3	       PIC  X(09).
	       05  ST7-PAGE	       PIC  ZZZ9.
	       05  FILLER	       PIC  X(08).
002290	   03  ST7-LINE5 REDEFINES W02-PRN-LINE.
	       05  FILLER	       PIC  X(04).
002300	       05  ST7-NAME	   PIC	X(40).
002310	       05  FILLER	       PIC  X(08).
002320	       05  ST7-MESS	   PIC	X(42).
	       05  FILLER	   PIC	X(02).
002340	   03  ST7-LINE6 REDEFINES W02-PRN-LINE.
	       04  FILLER	   PIC	X(03).
	       04  ST7-HD1.
	       05  FILLER	   PIC	X(01).
002420	       05  ST7-TDTE	   PIC	Z9/99/9999.
002430	       05  FILLER	   PIC	X(04).
	       05  ST7-BFWD.
002440		   07  ST7-TDESC   PIC	X(12).
		   07  FILLER	   PIC	X(04).
002470		   07  ST7-TREF    PIC	X(08).
002490	       05  FILLER	   PIC	X(04).
002500	       05  ST7-V1.
002510		   07  ST7-TDBT    PIC	Z(08)9.99CR.
002520	       05  ST7-PD	   PIC	X(02).
002500	       05  ST7-V2.
002510		   07  ST7-TCRD    PIC	Z(08)9.99CR.
002520	       05  FILLER	   PIC	X(02).
002530	       05  ST7-V3.
002540		   07  ST7-TBAL    PIC	Z(08)9.99CR.
002690	   03  ST7-LINE7 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC	X(03).
002700	       05  ST7-A1	   PIC	X(12).
002710	       05  ST7-A2	   PIC	X(10).
002720	       05  ST7-A3	   PIC	X(12).
002720	       05  ST7-A4	   PIC	X(12).
002720	       05  ST7-A5	   PIC	X(12).
002730	       05  FILLER	   PIC	X(35).
002600	   03  ST7-LINE8 REDEFINES W02-PRN-LINE.
002610	       05  FILLER	   PIC	X(01).
	       05  ST7-INT	   PIC	Z(06)9.99-.
002610	       05  FILLER	   PIC	X(01).
002620	       05  ST7-3MTHS	   PIC	Z(06)9.99-.
002610	       05  FILLER	   PIC	X(01).
002620	       05  ST7-2MTHS	   PIC	Z(06)9.99-.
002630	       05  FILLER	   PIC	X(01).
002640	       05  ST7-1MTH	   PIC	Z(06)9.99-.
002650	       05  FILLER	   PIC	X(01).
002660	       05  ST7-CUR	       PIC  Z(06)9.99-.
002670	       05  FILLER	   PIC	X(01).
002680	       05  ST7-DUE	       PIC  Z(06)9.99-.
002670	       05  FILLER	   PIC	X(12).
002680	       05  ST7-DUE2	   PIC	Z(06)9.99-.

       01  W05-DEB-HISTORY.
	   03  W05-DBT-HIS.
	       05  W05-DBT  PIC  X(04) VALUE "AR_T".
	       05  W05-TYY  PIC  9(02).
	       05  W05-TMM  PIC  9(02).
	       05  FILLER   PIC  X(04) VALUE ".DAT".
	   03  W05-DEB-HIS.
	       05  W05-DEB  PIC  X(04) VALUE "AR_A".
	       05  W05-DEBH-ID.
		   07  W05-DYY PIC  9(02).
		   07  W05-DMM PIC  9(02).
	       05  W05-HIST-ID REDEFINES W05-DEBH-ID PIC  9(04).
	       05  FILLER   PIC  X(04) VALUE ".DAT".

003650 01  W05-LIST.
003660     03  W05-OPT         PIC 9(03).
003670     03  W05-TYPES REDEFINES W05-OPT.
003680         05  W05-COMP    PIC 9(01).
003690         05  W05-INDV    PIC 9(01).
003700         05  W05-INT     PIC 9(01).

003710 01  W10-SPARES.
004300	   03  W10-DATE	       PIC  X(10).
	   03  W10-RDATE REDEFINES W10-DATE.
	       05  W10-DTE     PIC  99/99/9999.

       COPY W12.WS.

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

005020 01  W45-TRAN.
005030     03  W45-CODE        PIC 9(02).
005040     03  W45-ENG         PIC X(12).
005050     03  W45-AFR         PIC X(12).
005060     03  W45-ACT         PIC X(01).

       COPY FUNCTION.WS.

005170 COPY W50.WS.

       01  W55-STATS.
           03  W55-H1         PIC  X(09).
           03  W55-H2         PIC  X(09).
           03  W55-H3         PIC  X(09).
           03  W55-H4         PIC  X(09).
           03  W55-H5         PIC  X(09).
           03  W55-H6         PIC  X(09).
           03  W55-MONTHS.
               05  W55-MTH1   PIC  9(02) COMP-5.
               05  W55-MTH2   PIC  9(02) COMP-5.
               05  W55-MTH3   PIC  9(02) COMP-5.
               05  W55-MTH4   PIC  9(02) COMP-5.
               05  W55-MTH5   PIC  9(02) COMP-5.
               05  W55-MTH6   PIC  9(02) COMP-5.
           03  W55-VALUES.
               05  W55-VAL1   PIC S9(09)V99 COMP-3.
               05  W55-VAL2   PIC S9(09)V99 COMP-3.
               05  W55-VAL3   PIC S9(09)V99 COMP-3.
               05  W55-VAL4   PIC S9(09)V99 COMP-3.
               05  W55-VAL5   PIC S9(09)V99 COMP-3.
               05  W55-VAL6   PIC S9(09)V99 COMP-3.

005190 01  W60-NME-ADD.
005200     03  W60-NAME.
005210         05  W60-NCHAR OCCURS 40 PIC X(01).
005220     03  W60-ADDRESS.
005230         05  W60-ACHAR OCCURS 60 PIC X(01).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.

005330 01  W75-TITLES.
005340     03  W75-DESCRIP.
005350         05  FILLER      PIC X(28)       VALUE
005360                     "Mr     Mnr    Mrs    Mev    ".
005370         05  FILLER      PIC X(28)       VALUE
005380                     "Miss   Mej    Ms     Ms     ".
005390         05  FILLER      PIC X(28)       VALUE
005400                     "Dr     Dr     Rev    Ds     ".
005410         05  FILLER      PIC X(28)       VALUE
005420                     "Prof   Prof   The HonSy Edel".
005430         05  FILLER      PIC X(28)       VALUE
005440                     "                            ".
005450     03  W75-CODES REDEFINES W75-DESCRIP.
005460         05  W75-TITLE  OCCURS 10.
005470             07  W75-ETITLE  PIC X(07).
005480             07  W75-ATITLE  PIC X(07).

005490 01  W80-ACCOUNT.
005500     03  W80-ACNO        PIC X(06).
005510     03  W80-NUMD REDEFINES W80-ACNO.
005530         05  W80-SERIAL  PIC 9(06).
005520     03  W80-TYPE        PIC 9(01).

005540 01  W85-BATCHES.
005550         05  W85-BATCH   PIC 9(05).

       COPY W90.DBT.

004120	   03  W90-MTD	       PIC S9(09)V99 COMP-3.
	   03  W90-DEBT-TOTAL  PIC S9(11)V99 COMP-3.

       COPY W95.STM.

004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99-.
004500     03  W100-BAL        PIC Z(08)9.99-.
004560     03  W100-CRL        PIC Z(06)9.  

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
005990 SCREEN SECTION.

       COPY BLANK.CRT.

006250 01  S07.
006260	   03  LINE  7 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")".

006540 01  S14.
006550	   03  LINE  3 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
006560				 VALUE "DETAILED ACCOUNT LIST".
006570     03  LINE  5 COLUMN 12 VALUE "Printing for".

006580 01  S15.
006590	   03  LINE   9 COLUMN 12 VALUE
006600		       "Do you want the Debtor address printed (".
	   03	       COLUMN 52 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 53 VALUE "/".
	   03	       COLUMN 54 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 55 VALUE ")".

       COPY DEBLUP.CRT.

       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".
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

       01  S80-YEAR.
	   03  LINE 08 COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z9 USING W05-DYY AUTO.
       01  S80-MONTH.
	   03  LINE 09 COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z9 USING W05-DMM AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
003240	   IF LS0-DBLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	     PERFORM YA000.
	     PERFORM XA000.
	   IF PREVIOUS-PERIOD
	       CLOSE DBHIST
		     DTHIST.

       AA49.
006950	     EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM DEBTOR-LOOKUP.
	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
010990 AA300              SECTION.
011000 AA301.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Debtor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011090 AA305.
011100       EXIT.

       COPY DATE.CNV.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP5.

      /
      *    ****    READ FILES ROUTINES
      *
007340 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.
      
       COPY DBHIST.RD.

       COPY DEBTOR.RD1.
      
       COPY DBTRAN.RD.
      
       COPY DEBTOR.LUP.

000140 COPY DEBMEM.RD.

       COPY DTHIST.RD.

       COPY APAC.HLP.

      /
008930 CA155-GET-DEBTOR        SECTION 2.
008940 CA155-ACCOUNT.
	   IF PREVIOUS-PERIOD
	       MOVE W80-ACNO	 TO DBH-ACNO
	       PERFORM READ-DBHIST THRU READ-DBHIST-EXIT
	       MOVE DBH-RECORD1  TO DEB-RECORD1
	   ELSE
012010	       MOVE W80-ACNO	 TO DEB-ACNO
014330	       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
	   IF WS-F-ERROR = 6 OR 56
	       MOVE 1		 TO WS-ERROR
014380     ELSE
014390     IF DEB-ACNO NOT = W80-ACNO
014400         MOVE 2            TO WS-ERROR
014410     ELSE
014420         MOVE 0            TO WS-ERROR
               MOVE DEB-TYPE     TO W80-TYPE.
009100 CA160-EXIT.
009110       EXIT.
009120 CA165-NAME              SECTION 2.
009130 CA165-EXTRACT.
009140       MOVE ZERO TO WS-ERROR.
009150       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
009160         INTO W40-SURNAME W40-FNAME W40-SNAME
009170         OVERFLOW
009180           MOVE 1 TO WS-ERROR.
009190 CA170-EXIT.
009200       EXIT.
009210 CA175-ADDRESS           SECTION 2.
009220 CA175-EXTRACT.
009230       MOVE 0 TO WS-ERROR.
009240       UNSTRING W60-ADDRESS DELIMITED BY "*"
009250         INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
009260         OVERFLOW
009270           MOVE 1 TO WS-ERROR.
009280 CA180-EXIT.
009290       EXIT.
      /
      *    ****    D E T A I L E D   A C C O U N T   L I S T
      *
016170 XA000	       SECTION 5.

016180 XA00.
016190       DISPLAY CLR-SCREEN.
016200       DISPLAY S14.
	     MOVE ZERO		 TO WS-BDT.
	     MOVE "N"		 TO WS-BAD.

016210 XA05.
016220	     DISPLAY W95-COMP AT 0525 WITH FOREGROUND-COLOR 11.
016230	     DISPLAY S07.
016240	     ACCEPT WS-OPTION AT 0731
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016310	   IF WS-OPTION = "N"
016320         GO TO XA999-EXIT.
016330	     DISPLAY S15.
016340	     ACCEPT WS-ADDR AT 0957
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-ADDR
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016410 XA10.
016530	     MOVE SPACE 	 TO WS-CREDIT.
             MOVE "N"            TO WS-ZERO.

016540 XA15.
	     DISPLAY "Print accounts with zero balance (" AT 1112
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".
	     ACCEPT WS-ZERO AT 1151
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-ZERO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016490	   IF NOT (WS-ZERO = "N" OR "Y")
016500         GO TO XA15.
016550       MOVE 1              TO WS-SUB-2.
016570	     MOVE 0		 TO WS-PAGE W90-BAL W90-CUR
				    W90-30  W90-60  W90-90
016580				    W90-120 W90-BDT W90-BCR
				    W90-INT W90-MTD W90-DEBT-TOTAL.
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.

016610 XA20.
	     MOVE "H"		 TO W02-PRN-TYPE.
016620	     MOVE SPACES	 TO REP-DETAIL1.
016630	     MOVE "PAGE:"	 TO TRL-PAGE.
016640	     MOVE "DATE:"	 TO TRL-DATE.
016650	     ADD 1		 TO WS-PAGE.
016660	     MOVE W12-TODAY	 TO TRL-DTE.
016670	     MOVE WS-PAGE	 TO TRL-P-NO.
	     MOVE 0		 TO WS-ADVANCE.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO TRL-LINE1
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
016680	     MOVE W95-COMP	 TO TRL-CO.
016690	     MOVE " - DETAILED ACCOUNTS"
				 TO TRL-EXHEAD.
	     MOVE "H"		 TO W02-PRN-TYPE.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO TRL-LINE1.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "X"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012760	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
	   IF WS-BDT = 1
	       MOVE SPACES	 TO TRL-DET
	       MOVE 2		 TO WS-ADVANCE
	       MOVE "B A D   A N D   D O U B T F U L L   D E B T S"
				 TO TRL-RESTART
	       PERFORM CALL-PRINTUTL
	   ELSE
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
016710	     MOVE SPACES	 TO TRL-DET.
	     MOVE 2		 TO WS-ADVANCE.

016720 XA25.
016730	     MOVE "AC-NO."	 TO TRL-HEAD1.
016740	     MOVE "NAME"	 TO TRL-HEAD2.
016750	     MOVE "ACTIVE"	 TO TRL-HEAD3.
016760	     MOVE "MTH" 	 TO TRL-HEAD4.
	     MOVE "PAST DUE"	 TO TRL-HEAD5.
016840	     MOVE "BALANCE"	 TO TRL-HEAD6.
	     MOVE "STM-BALANCE"  TO TRL-HEAD7.
	     MOVE "LAST-PAYMNT"  TO TRL-HEAD8.
	     MOVE "DATE-PAID"	 TO TRL-HEAD9.
	     MOVE "INTEREST"	 TO TRL-HEAD10.
	     MOVE "C-LIMIT"	 TO TRL-HEAD11.
	     PERFORM CALL-PRINTUTL.
016860	     MOVE SPACES	 TO TRL-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

016870 XA30.
	   IF WS-BDT = 1
	       DISPLAY "Busy with Bad & Doubtfull Debts" AT 1812
	       GO TO XA45.
      *
      *    ****   T E S T   I F   O U T P U T	T O   S C R E E N
      *
	   IF WS-PRINTER = "S"
	       MOVE SPACES	 TO WS-ACNO
	       GO TO XA45.
016880	     DISPLAY "Is this a restart (" AT 1312
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".

016890 XA35.
016900	     ACCEPT WS-OPTION AT 1336
016910		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016970	   IF NOT (WS-OPTION = "N" OR "Y")
016980         GO TO XA35.
016990	     MOVE SPACES	 TO WS-ACNO.
017000     IF WS-OPTION = "N"
017010         GO TO XA45.

017020 XA40.
             MOVE 2001           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
017040	     DISPLAY "Account No:" AT 2012.
017050	     ACCEPT W80-ACNO AT 2024
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
017060     IF W80-ACNO = SPACES
017070         GO TO XA45.
017080       PERFORM CA155-GET-DEBTOR.
010410	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
017100     IF WS-ERROR NOT = ZERO
017110	       MOVE "Invalid account number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017130	       GO TO XA40.
017140	     MOVE DEB-NAME	 TO W40-NAME.
017150     IF DEB-TYPE = 1
017160         INSPECT W40-NAME REPLACING ALL "*" BY "."
017170         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
017180	     DISPLAY W40-NAME AT 2030
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
017270	   IF WS-OPTION = "N"
017280         GO TO XA40.
017290	     MOVE W80-ACNO	 TO WS-ACNO.
017300       MOVE "RESTART  RESTART  RESTART  RESTART RESTART RESTART"
017310				 TO TRL-RESTART.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017330	     MOVE SPACES	 TO TRL-DET.

017340 XA45.
             DISPLAY "Debtor account:" AT 2212.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Start reading from the first account in the file.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF PREVIOUS-PERIOD
	       MOVE " !"	 TO DBH-NAME
	       PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
	   ELSE
	       MOVE " !"	 TO DEB-NAME
	       PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
010780	   IF WS-F-ERROR = 6 OR 56
017370	       GO TO XA70.

017380 XA50.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO XA999-EXIT.
	   IF PREVIOUS-PERIOD
021090	       PERFORM READ-DBHIST-NEXT THRU READ-DBHIST-EXIT
	       MOVE DBH-RECORD1  TO  DEB-RECORD1
	   ELSE
021090	       PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
010780	   IF WS-F-ERROR = 6 OR 56
	       GO TO XA65.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY DEB-ACNO AT 2228 WITH FOREGROUND-COLOR 11.
017400     IF WS-ACNO = DEB-ACNO
017410	       MOVE SPACES	 TO WS-ACNO.
           IF WS-ZERO = "N"
               IF DEB-BAL = ZERO
                   GO TO XA50.
	   IF DEB-BDT = 1
	       IF WS-BDT = 0
		   MOVE "Y"	 TO WS-BAD
		   GO TO XA50.
	   IF NOT (DEB-BDT = 1)
	       IF WS-BDT = 1
		   GO TO XA50.
017430     IF WS-ERROR1 = 1
017440         GO TO XA50.
017450	     MOVE DEB-NAME	 TO W40-NAME.
017460     IF DEB-TYPE = 1
017470         INSPECT W40-NAME REPLACING ALL "*" BY "."
017480         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
017490	     MOVE DEB-ACNO	 TO TRL-AC.
017500	     MOVE W40-NAME	 TO TRL-NME.
017510	     MOVE DEB-ACTIVE	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY.
	     MOVE W22-DTE3	 TO TRL-ACT.
	   IF DEB-120 > 0
	       MOVE 4		 TO TRL-MTH
017690	   IF DEB-90 > 0
017700	       MOVE 3		 TO TRL-MTH
017710     ELSE
017720     IF DEB-60 > 0
017730	       MOVE 2		 TO TRL-MTH
017740     ELSE
017750     IF DEB-30 > 0
017760	       MOVE 1		 TO TRL-MTH.
017520	     ADD DEB-30 DEB-60 DEB-90 DEB-120
				 GIVING W25-VALUE.
017670	     MOVE W25-VALUE	 TO TRL-PD.
017530	     ADD DEB-BAL	 TO W90-DEBT-TOTAL.
017540	     ADD DEB-CUR	 TO W90-CUR.
017550	     ADD DEB-30 	 TO W90-30.
017560	     ADD DEB-60 	 TO W90-60.
017570	     ADD DEB-90 	 TO W90-90.
017570	     ADD DEB-120	 TO W90-120.
017580	     ADD DEB-INT	 TO W90-INT.
017590	   IF DEB-BAL > 0
017600	       ADD DEB-BAL	 TO W90-BDT
017610     ELSE
017620	       ADD DEB-BAL	 TO W90-BCR.
017640	     SUBTRACT W25-VALUE FROM DEB-BAL
				 GIVING W25-TOTAL.
017650	     SUBTRACT DEB-CUR	 FROM W25-TOTAL.
017660	     ADD W25-TOTAL	 TO W90-MTD.
017680	     MOVE DEB-BAL	 TO TRL-BAL.
	     MOVE DEB-STM	 TO TRL-STM.
	     MOVE DEB-LAST-PMT	 TO TRL-LST-PAY.
	     MOVE DEB-PMT	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY.
	     MOVE W22-DTE3	 TO TRL-PAY.
017820	     MOVE DEB-INT	 TO TRL-INT.
017830	     MOVE DEB-CRL	 TO TRL-LIMIT.
017840	   IF DEB-CRL > ZERO
017850         IF DEB-BAL > DEB-CRL
017860		   MOVE "*"	 TO TRL-EXL.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017950	       PERFORM XA20 THRU XA25.
017960	     MOVE SPACES	 TO TRL-DET.
	     MOVE "M.T.D:"	 TO TRL-HEAD12.
017770	     COMPUTE TRL-MTD = DEB-BAL - (DEB-INT + DEB-CUR + DEB-30
017780					 + DEB-60 + DEB-90 + DEB-120)
016770	     MOVE "CURRENT:"	 TO TRL-HEAD13.
	     MOVE DEB-CUR	 TO TRL-CUR.
016770	     MOVE "30 DAYS:"	 TO TRL-HEAD14.
017790	     MOVE DEB-30	 TO TRL-30.
016780	     MOVE "60 DAYS:"	 TO TRL-HEAD15.
017800	     MOVE DEB-60	 TO TRL-60.
016790	     MOVE "90 DAYS:"	 TO TRL-HEAD16.
017800	     MOVE DEB-90	 TO TRL-90.
016800	     MOVE "120 DAYS:"	 TO TRL-HEAD17.
017800	     MOVE DEB-120	 TO TRL-120.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017950	       PERFORM XA20 THRU XA25.
017960	     MOVE SPACES	 TO TRL-DET.
017890	   IF WS-ADDR = "Y"
017900         GO TO XA55.

       XA52.
	     PERFORM XF000.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO XA999-EXIT.
	     MOVE SPACES	 TO REP-DETAIL1.
	   IF NOT (W02-LINAGE < (W02-PRN-LENGTH - 4))
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
018270	       PERFORM XA20 THRU XA25.
017970	     GO TO XA50.

017980 XA55.
018000	     MOVE SPACES	 TO TRL-DET.
018010	     MOVE DEB-ADDRESS	 TO W60-ADDRESS.
018020       PERFORM CA175-ADDRESS.
018030	     MOVE W40-ADD1	 TO TRL-NME.
	     PERFORM CALL-PRINTUTL.
018050     IF W40-ADD2 = SPACES
018060         GO TO XA60.
018070	     MOVE W40-ADD2	 TO TRL-NME.
	     PERFORM CALL-PRINTUTL.
018090	   IF W40-ADD3 = SPACES
018100         GO TO XA60.
018110	     MOVE W40-ADD3	 TO TRL-NME.
	     PERFORM CALL-PRINTUTL.
018130	   IF W40-ADD4 = SPACES
018140         GO TO XA60.
018150	     MOVE W40-ADD4	 TO TRL-NME.
	     PERFORM CALL-PRINTUTL.

018170 XA60.
018180	     MOVE SPACES	 TO TRL-DET.
018190	     MOVE DEB-POST	 TO TRL-PCDE.
018200	     MOVE "TEL NO'S:"	 TO TRL-HEAD18.
018210	     MOVE DEB-H-TEL	 TO TRL-HTEL.
018220	     MOVE DEB-B-TEL	 TO TRL-BTEL.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 4)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
018250	   ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
018270	       PERFORM XA20 THRU XA25.
018280	     MOVE SPACES	 TO TRL-DET.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO XA999-EXIT.
018320	     GO TO XA52.

018290 XA65.
	     MOVE "S"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
018340       PERFORM XA20.
018350	     MOVE "DEBTORS ACCOUNTS SUMMARY"
				 TO TRL-HEADER.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
018370	     MOVE SPACES	 TO TRL-DET.
018380       MOVE "GRAND TOTALS" TO TRL-SUM.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
           IF WS-AGE = "Y"
013310	       MOVE "30 DAYS"	 TO TRL-SUM
           ELSE
013280	       MOVE "CURRENT"	 TO TRL-SUM.
013290	     MOVE W90-CUR	 TO TRL-VAL.
	     PERFORM CALL-PRINTUTL.
	   IF WS-AGE = "Y"
013340	       MOVE "60 DAYS"	 TO TRL-SUM
           ELSE
013310	       MOVE "30 DAYS"	 TO TRL-SUM.
013320	     MOVE W90-30	 TO TRL-VAL.
	     PERFORM CALL-PRINTUTL.
	   IF WS-AGE = "Y"
013370	       MOVE "90 DAYS"	 TO TRL-SUM
           ELSE
013340	       MOVE "60 DAYS"	 TO TRL-SUM.
013350	     MOVE W90-60	 TO TRL-VAL.
	     PERFORM CALL-PRINTUTL.
	   IF WS-AGE = "Y"
013370	       MOVE "120 DAYS"	 TO TRL-SUM
           ELSE
013370	       MOVE "90 DAYS"	 TO TRL-SUM.
013380	     MOVE W90-90	 TO TRL-VAL.
	     PERFORM CALL-PRINTUTL.
018520	     MOVE "INTEREST"	 TO TRL-SUM.
018530	     MOVE W90-INT	 TO TRL-VAL.
	     PERFORM CALL-PRINTUTL.
018550	     MOVE "CREDIT BALANCES"
				 TO TRL-SUM.
018560	     MOVE W90-BCR	 TO TRL-VAL.
	     PERFORM CALL-PRINTUTL.
018580	     MOVE "DEBIT BALANCES"
				 TO TRL-SUM.
018590	     MOVE W90-BDT	 TO TRL-VAL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
018610	     MOVE "DEBTOR TOTAL" TO TRL-SUM.
018620	     MOVE W90-DEBT-TOTAL TO TRL-VAL.
	     MOVE "E"		 TO WS-COMMAND.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF WS-BDT = 1
	       GO TO XA70.
	   IF WS-BAD = "N"
	       GO TO XA70.
	     MOVE 1		 TO WS-BDT.
016570	     MOVE 0		 TO W90-BAL W90-CUR W90-30  W90-60
016580				    W90-90  W90-120 W90-BDT W90-BCR
				    W90-INT.
	     GO TO XA20.

018660 XA70.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
018670	     GO TO XA999-EXIT.

019000 XA999-EXIT.
019010       EXIT.
      /
019020 XF000		      SECTION 6.
019030 XF00-INIT.
      *
      *    ****    1 2	 C P I
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
016620	     MOVE SPACES	 TO REP-DETAIL1.

       XF05.
	     MOVE "   Date        Tran-Type      Reference      Tran.Val
      -		  "ue      Allocation    Tran.Balance" TO ST7-HD1
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
016620	     MOVE SPACES	 TO REP-DETAIL1.

022810 XF75-BFWD.
022820       MOVE 0              TO W25-WHOLE W90-BAL.
	   IF OPEN-ITEM
	       GO TO XF80-TRAN.
022830	   IF DEB-STM = 0
022840         GO TO XF80-TRAN.
022850     IF DEB-LANG = "A"
022860         MOVE "REK GELEWER" 
				 TO ST7-BFWD
022870     ELSE
022880         MOVE "A/C RENDERED" 
				 TO ST7-BFWD.
022900	     MOVE DEB-STM	 TO ST7-TBAL W90-BAL.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017950	       PERFORM XA20 THRU XA25
	       PERFORM XF05.
016620	     MOVE SPACES	 TO REP-DETAIL1.

022960 XF80-TRAN.
	   IF PREVIOUS-PERIOD
022970	       MOVE DEB-ACNO	 TO TRH-KEY
	       PERFORM START-AT-DTRH-KEY THRU READ-DTHIST-EXIT
	   ELSE
022970	       MOVE DEB-ACNO	 TO TRA-KEY
	       PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
	   IF WS-F-ERROR = 5 OR 55
022980         GO TO XF90-SEND.

023000 XF85-TLOOP.
	   IF PREVIOUS-PERIOD
	       PERFORM READ-DTHIST-NEXT THRU READ-DTHIST-EXIT
	       MOVE TRH-RECORD1  TO TRA-RECORD1
	   ELSE
023010	       PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
	   IF (WS-F-ERROR = 5 OR 55) OR
              (TRA-AC NOT = DEB-ACNO)
               GO TO XF90-SEND.
023020     IF TRA-VALUE = ZERO
023030         GO TO XF85-TLOOP.
023040	     MOVE TRA-DATE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO ST7-TDTE.
	   IF WS-REF = "O"
	      IF NOT (TRA-REG = SPACES)
		  MOVE TRA-REG	 TO ST7-TREF
	      ELSE
		  MOVE TRA-REF	    TO ST7-TREF
	      END-IF
	   ELSE
023050	   IF NOT (TRA-REF = "00000000" OR SPACES)
023130	       MOVE TRA-REF	 TO ST7-TREF.
023070     IF DEB-LANG = "A"
023080	       MOVE W00-A-DESC (TRA-CODE) TO ST7-TDESC
023090     ELSE
023100	       MOVE W00-E-DESC (TRA-CODE) TO ST7-TDESC.
023220	     MOVE TRA-VALUE	 TO ST7-TDBT.
	   IF NOT (TRA-ALLOCATED = ZERO)
	       IF TRA-VALUE > ZERO
		   MULTIPLY -1 BY TRA-ALLOCATED
				 GIVING ST7-TCRD
		   SUBTRACT TRA-ALLOCATED FROM TRA-VALUE
				 GIVING ST7-TBAL
	       ELSE
		   MOVE TRA-ALLOCATED
				 TO ST7-TCRD
		   ADD TRA-VALUE TRA-PAID
				 GIVING ST7-TBAL
	       END-IF
           ELSE
023220	       MOVE TRA-VALUE	 TO ST7-TBAL.
             ADD TRA-VALUE       TO W90-BAL.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017950	       PERFORM XA20 THRU XA25
	       PERFORM XF05.
016620	     MOVE SPACES	 TO REP-DETAIL1.
023310	     GO TO XF85-TLOOP.

023320 XF90-SEND.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017950	       PERFORM XA20 THRU XA25.

025000 XF999-EXIT.
025010       EXIT.
      /
       YA000		      SECTION 80.
       YA00.
019040       DISPLAY CLR-SCREEN.
	     DISPLAY "DEBTORS (ACCOUNTS RECEIVABLE) DETAILED LIST"
		      AT 0319 WITH FOREGROUND-COLOR 7 HIGHLIGHT.

       YA05.
	     MOVE "'P'revious or 'C'urrent period  [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-PERIOD.
020250	   IF NOT (WS-PERIOD = "C" OR "P")
	       GO TO YA05.
	   IF NOT PREVIOUS-PERIOD
	       GO TO YA999.

       YA10.
019040	     DISPLAY CLR-SCREEN.
	     DISPLAY "DEBTORS (A/R) PREVIOUS DETAILED LIST" AT 0323
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
      *
      *			      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *			      ³ PREVIOUS PERIOD	 ³²²
      *			      ³ Year : YY (00-99)³²²
      *			      ³ Month: MM (01-12)³²²
      *			      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ²²
      *				²²²²²²²²²²²²²²²²²²²²
      *

	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0631
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 0731 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "PREVIOUS PERIOD  "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 0831 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "Year :    (00-99)" WITH FOREGROUND-COLOR 2
					      BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 0931 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "Month:    (01-12)" WITH FOREGROUND-COLOR 2
					      BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "À" AT 1031 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" WITH FOREGROUND-COLOR 0
						BACKGROUND-COLOR 5.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 33		 TO SHADE-COL.
	     MOVE 18		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     MOVE W12-YEAR	 TO W05-DYY.
	     MOVE W12-MONTH	 TO W05-DMM.
      *    ************************************************************
      *    ****    Y E A R   O F   P R E V I O U S   P E R I O D
      *    ************************************************************
       YA15.
	     ACCEPT S80-YEAR.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "C"	 TO WS-PERIOD
		     GO TO YA00
		 WHEN OTHER
		     CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
		     GO TO YA15
	       END-EVALUATE.
	   IF W05-DYY > W12-YEAR
	       MOVE "Year can not be > Current Year"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO YA15.
      *    ************************************************************
      *    ***	   M O N T H   O F   P R E V I O U S   P E R I O D
      *    ************************************************************
       YA20.
	     ACCEPT S80-MONTH.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA15
		 WHEN OTHER
		     CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
		     GO TO YA20
	       END-EVALUATE.
	   IF W05-DYY > W12-YEAR
	       MOVE "Month can not be < 01 or > 12"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO YA20.
019040	     DISPLAY CLR-SCREEN.
	     DISPLAY "DEBTORS (A/R) PREVIOUS DETAILED LIST" AT 0322
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
      *    *************************************************************
      *    ****    Create paths and file names for history files    ****
      *    *************************************************************
	     MOVE W05-DYY	 TO W05-TYY.
	     MOVE W05-DMM	 TO W05-TMM.
	     INSPECT W02-DBHIST REPLACING FIRST "AR_ACNT.DAT"
			       BY "           ".
	     MOVE W02-DBHIST	 TO W60-ADDRESS.
	     STRING W60-ADDRESS DELIMITED BY " "
		    W05-DEB-HIS DELIMITED SIZE
		    INTO W02-DBHIST.
	     INSPECT W02-DTHIST REPLACING FIRST "AR_TRAN.DAT"
			       BY "           ".
	     MOVE W02-DTHIST	 TO W60-ADDRESS.
	     STRING W60-ADDRESS DELIMITED BY " "
		    W05-DBT-HIS DELIMITED SIZE
		    INTO W02-DTHIST.
	     OPEN I-O DBHIST.
000040	   IF NOT(WS-STATUS = "00")
	       PERFORM YA200
	       GO TO YA00.
	     OPEN I-O DTHIST.
000040	   IF NOT(WS-STATUS = "00")
	       PERFORM YA200
	       GO TO YA00.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Set up the run / age dates applicable to the history files³
      *    ³ when the statements and ageing of balances was executed by³
      *    ³ the system. These dates are stored in the first record of ³
      *    ³ the history account file, which is a dummy account with   ³
      *    ³ an account number of "     !" and a name "  !".	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE "     !"	 TO DBH-ACNO.
	     PERFORM READ-DBHIST THRU READ-DBHIST-EXIT.
	   IF WS-F-ERROR = 56
	       GO TO YA200.
	     MOVE DBH-STM-DTE	 TO W12-TODAY.
	     GO TO YA999.

       YA200.
	     MOVE "Details for period requested not available"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     CLOSE DBHIST
		   DTHIST.

       YA999.
	       EXIT.

      /
025020 ZA00-INIT	      SECTION 90.
025030 ZA00-OPEN.
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
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *    ****    P A R A M E T E R   F I L E
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
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E    ****
      * 		Setup the Transaction History path
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN W02-DTHIST.
      *    *************************************************************
      *    **** 	  D E B T O R	M E M O	  F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      * 		  Setup the Account History path
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR W02-DBHIST.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
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

       ZA02.
025430       MOVE 11 TO WS-PARKEY.
025440       MOVE 1 TO WS-SUB-2.

025450 ZA15-READ-TRANS.
025460       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025490       MOVE 1 TO WS-SUB-1.

025500 ZA20-TRAN-LOOP.
025510       MOVE PAR-T-CODE (WS-SUB-1) TO W00-TRAN-CODE (WS-SUB-2).
025520       MOVE PAR-E-DESC (WS-SUB-1) TO W00-E-DESC (WS-SUB-2).
025530       MOVE PAR-A-DESC (WS-SUB-1) TO W00-A-DESC (WS-SUB-2).
025540       MOVE PAR-ACTION (WS-SUB-1) TO W00-ACTION (WS-SUB-2).
025550       ADD 1 TO WS-SUB-2.
025560     IF WS-SUB-1 < 2
025570         ADD 1 TO WS-SUB-1
025580         GO TO ZA20-TRAN-LOOP.
025590     IF WS-PARKEY < 60
025600         ADD 1 TO WS-PARKEY
025610         GO TO ZA15-READ-TRANS.
025620 ZA25.
	     MOVE 4		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-AGE	 TO WS-AGE.
025630	     MOVE 1		 TO WS-PARKEY.
025640       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025650	     MOVE PAR-COMPANY	 TO W95-COMP.
025660	     MOVE PAR-DMY	 TO W12-TODAY.
025670	     MOVE PAR-YMD	 TO W12-T-YMD.
025730	     MOVE 1		 TO WS-NETKEY.
025740       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
025830       GO TO ZA999-EXIT.
      *    *************************************************************
      *    ****   F I L E   L I M I T	E X C E E D E D
      * 	  (Config.sys Dos/Win/Windows 95/98/NT/2000)
      * 	  (Config.nt Windows XP)
      * 	  (System.ini Windows.me)
      *    *************************************************************
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
      *    *************************************************************
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *    *************************************************************
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
025940 ZB000		     SECTION 91.
      
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
041730     IF WS-F-ERROR = 5
041740         MOVE W02-DBTRAN   TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
041760     ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041640     ELSE
041650     IF WS-F-ERROR = 55
041660	       MOVE W02-DTHIST	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE TRH-KEY	 TO WS-KEYX
041760     ELSE
041770	   IF WS-F-ERROR = 56
041780	       MOVE W02-DBHIST	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE DBH-ACNO	 TO WS-KEYX.
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
