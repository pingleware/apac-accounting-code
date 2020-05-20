      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******    ******    *
      *   **	**     **     **    **	 **  **   **	**  **	  **   *
      *   **	**     **     **    **	**    **       **   **	       *
      *   **	**     **     *******	**    **     **     *******    *
      *   **	**     **     **	**    **   **	    **	  **   *
      *   **	**     **     **	 **  **   **	    **	  **   *
      *   *******      **     **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R	  A G E   A N A L Y S I S   P R O G R A M      *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *    May 2006	- Include option to print only Accounts with   *
      * 		  payment terms as selected by the User.       *
      * 		- See also DTP025: Transaction Journal.        *
      * 							       *
      * 		- New field included in Accounts Receivable    *
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
      *  April 2012	- Changes to handle USB/WINDOWS printer.       *
      * 		  Include WS-PRINT-PRG, which is set when the  *
      * 		  OPEN-PRINTER routine is perform and then it  *
      * 		  is used to call the correct print program.   *
      * 		  SEE - the PRINTUTL.AS?/AP? routines.	       *							       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP026.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   September 1986.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1986 - 2011
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

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000250 COPY PARAM.FDE.

       COPY DBHIST.FDE.

000270 COPY DEBTOR.FDE.

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
002960 77  WS-NTP	   PIC 9(01).
002970 77  WS-LINES        PIC 9(02).
002980 77  WS-PAGE         PIC 9(04)     COMP-5.
002990 77  WS-OPTION       PIC X(01).
002990 77  WS-AGE          PIC X(01).
002990 77  WS-SEQ          PIC X(01).
003000 77  WS-TERMS	   PIC X(01).
002990 77  WS-ZERO	   PIC X(01).
       77  WS-NUM          PIC Z9.
003020 77  WS-ADDR         PIC X(01).
003030 77  WS-ERROR        PIC 9(01).
003040 77  WS-ERROR1       PIC 9(01).
003110 77  WS-EXPAND	   PIC X(01)	 VALUE " ".
003150 77  WS-CHAR1        PIC X(01).
003160 77  WS-CHAR2        PIC X(01).
003180 77  WS-COMP         PIC 9(01).
003180 77  WS-STM          PIC 9(01).
003190 77  WS-TYPE         PIC X(01).
003200 77  WS-HEAD         PIC X(01).
       77  WS-PERIOD	   PIC	X(01).
       88  PREVIOUS-PERIOD		 VALUE "P".
003230 77  WS-BATCH	   PIC 9(05).
003240 77  WS-MESS         PIC X(02).
003250 77  WS-ACNO         PIC X(06).
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
003260 77  WS-S1	   PIC 9(04)	 COMP-5.
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

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DBHIST.ID.

       COPY DEBTOR.ID.

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
000940	   03  COM-LINE1 REDEFINES W02-PRN-LINE.
000950	       05  COM-PAGE    PIC  X(06).
000960	       05  COM-P-NO    PIC  Z(03)9.
000970	       05  FILLER      PIC  X(04).
000980	       05  COM-COMPANY PIC  X(30).
000990	       05  COM-HEAD    PIC  X(18).
001000	       05  FILLER      PIC  X(02).
001010	       05  COM-DATE    PIC  X(06).
001020	       05  COM-DTE     PIC  Z9/99/9999.
001030	   03  LAB-L1 REDEFINES W02-PRN-LINE.
001040	       05  LAB-DETAIL OCCURS 2.
001050		   07  FILLER  PIC  X(02).
001060		   07  LAB-DET PIC  X(36).
001070		   07  FILLER  PIC  X(02).
001080	   03  TRL-LINE1 REDEFINES W02-PRN-LINE.
001090	       05  TRL-DET     PIC  X(132).
001100	   03  TRL-LINE2 REDEFINES W02-PRN-LINE.
001110	       05  TRL-HEAD1   PIC  X(07).
001120	       05  TRL-HEAD2   PIC  X(34).
001130	       05  TRL-HEAD3   PIC  X(08).
001140	       05  TRL-HEAD4   PIC  X(06).
001150	       05  TRL-HEAD5   PIC  X(11).
001160	       05  TRL-HEAD6   PIC  X(11).
001170	       05  TRL-HEAD7   PIC  X(09).
001180	       05  TRL-HEAD8   PIC  X(09).
001190	       05  TRL-HEAD9   PIC  X(09).
001200	       05  TRL-HEAD10  PIC  X(10).
001210	       05  TRL-HEAD11  PIC  X(10).
001220	       05  TRL-HEAD12  PIC  X(08).
001230	   03  TRL-LINE3 REDEFINES W02-PRN-LINE.
001240	       05  TRL-AC      PIC  X(06).
001250	       05  FILLER      PIC  X(01).
001260	       05  TRL-NME     PIC  X(31).
001270	       05  FILLER      PIC  X(01).
001280	       05  TRL-ACT     PIC  Z9/99/9999.
001290	       05  FILLER      PIC  X(01).
001300	       05  TRL-MTH     PIC  9(01).
001310	       05  FILLER      PIC  X(02).
001320	       05  TRL-CUR     PIC  Z(05)9.99-.
001330	       05  FILLER      PIC  X(01).
001340	       05  TRL-30      PIC  Z(05)9.99-.
001350	       05  FILLER      PIC  X(01).
001360	       05  TRL-60      PIC  Z(05)9.99.
001370	       05  FILLER      PIC  X(01).
001380	       05  TRL-90      PIC  Z(05)9.99.
001390	       05  FILLER      PIC  X(01).
001400	       05  TRL-INT     PIC  Z(03)9.99.
001410	       05  FILLER      PIC  X(01).
001420	       05  TRL-PD      PIC  Z(05)9.99-.
001430	       05  FILLER      PIC  X(01).
001440	       05  TRL-LIMIT   PIC  Z(05)9.
001450	       05  TRL-EXL     PIC  X(01).
001460	       05  TRL-BAL     PIC  Z(06)9.99-.
001470	   03  TRL-LINE4 REDEFINES W02-PRN-LINE.
001480	       05  FILLER      PIC  X(33).
001490	       05  TRL-HEADER  PIC  X(13).
001500	       05  FILLER      PIC  X(34).
001510	   03  TRL-LINE5 REDEFINES W02-PRN-LINE.
001520	       05  FILLER      PIC  X(10).
001530	       05  TRL-SUM     PIC  X(20).
001540	       05  TRL-VAL     PIC  Z(06)9.99-.
001550	       05  FILLER      PIC  X(39).
001560	   03  TRL-LINE6 REDEFINES W02-PRN-LINE.
001570	       05  TRL-PAGE    PIC  X(06).
001580	       05  TRL-P-NO    PIC  Z(03)9.
001590	       05  FILLER      PIC  X(24).
001600	       05  TRL-RESTART.
001610		   07  TRL-COMPANY  PIC X(40).
001620		   07  TRL-HEAD     PIC X(24).
001630	       05  FILLER      PIC  X(17).
001640	       05  TRL-DATE    PIC  X(06).
001650	       05  TRL-DTE     PIC  Z9/99/9999.
	   03  TRL-LINE6A REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(03).
	       05  TRL-CO      PIC  X(30).
	       05  TRL-EXHEAD.
		   07 FILLER   PIC  X(15).
		   07 TRL-ACS  PIC  X(17).
	       05  FILLER      PIC  X(03).
001660	   03  TRL-LINE7 REDEFINES W02-PRN-LINE.
001670	       05  FILLER      PIC  X(07).
001680	       05  TRL-PCDE    PIC  9(04).
001690	       05  FILLER      PIC  X(02).
001700	       05  TRL-HEAD13  PIC  X(10).
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

001270 01  W08-EXPD.
	   03  W08-CHAR        PIC  X(06).
001280	   03  W08-DEC REDEFINES W08-CHAR
			       PIC  X(01) COMP-X OCCURS 6.

       COPY W12.WS.

       01  W24-MONTHS.
           03  W24-MONTH-NAMES.
               05  FILLER      PIC  X(09) VALUE " January ".
               05  FILLER      PIC  X(09) VALUE " February".
               05  FILLER      PIC  X(09) VALUE "  March  ". 
               05  FILLER      PIC  X(09) VALUE "  April  ". 
               05  FILLER      PIC  X(09) VALUE "   May   ".
               05  FILLER      PIC  X(09) VALUE "   June  ".
               05  FILLER      PIC  X(09) VALUE "   July  ".
               05  FILLER      PIC  X(09) VALUE "  August ".
               05  FILLER      PIC  X(09) VALUE "September".
               05  FILLER      PIC  X(09) VALUE " October ".
               05  FILLER      PIC  X(09) VALUE " November".
               05  FILLER      PIC  X(09) VALUE " December".
           03  W24-EACH-MONTH REDEFINES W24-MONTH-NAMES.
               05  W24-MONTH   PIC  X(09) OCCURS 12.

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

       COPY W40.DBT.

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

005560 01  W90-TRAN.
005570	   03  W90-DATE        PIC X(10).
005580     03  W90-DTE REDEFINES W90-DATE
005590			       PIC 99/99/9999.
005600	   03  W90-CODE        PIC 9(02).
005610     03  W90-REF         PIC X(08).
005620     03  W90-DB          PIC S9(07)V99 COMP-3.
005630     03  W90-CR          PIC S9(07)V99 COMP-3.
005640     03  W90-BDT         PIC S9(07)V99 COMP-3.
005650     03  W90-BCR         PIC S9(07)V99 COMP-3.
005660     03  W90-DESC        PIC X(12).
005670     03  W90-120         PIC S9(07)V99 COMP-3.
005670     03  W90-90          PIC S9(07)V99 COMP-3.
005680     03  W90-60          PIC S9(07)V99 COMP-3.
005690     03  W90-30          PIC S9(07)V99 COMP-3.
005700     03  W90-CUR         PIC S9(07)V99 COMP-3.
005700     03  W90-MTD         PIC S9(07)V99 COMP-3.
005710     03  W90-BAL         PIC S9(07)V99 COMP-3.
005720     03  W90-IN          PIC 9(04)     COMP-5.
005730     03  W90-IV          PIC S9(07)V99 COMP-3.
005740     03  W90-RN          PIC 9(04)     COMP-5.
005750     03  W90-RV          PIC S9(07)V99 COMP-3.
005760     03  W90-DN          PIC 9(04)     COMP-5.
005770     03  W90-DV          PIC S9(07)V99 COMP-3.
005780     03  W90-CN          PIC 9(04)     COMP-5.
005790     03  W90-CV          PIC S9(07)V99 COMP-3.
005800     03  W90-INT         PIC S9(07)V99 COMP-3.
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-CATKEY      PIC  9(04)    COMP-5.
004020     03  W90-CS          PIC  9(02)    COMP-5.

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

005950 01  W100-HEAD.
005960     03  FILLER          PIC X(26)    VALUE
005970                       "DEBTOR ACCOUNTS LISTING -".
005980     03  W100-DESC       PIC X(11).

004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99-.
004500     03  W100-BAL        PIC Z(08)9.99-.
004510     03  W100-CUR        PIC Z(08)9.99-.
004520     03  W100-30         PIC Z(08)9.99-.
004530     03  W100-60         PIC Z(08)9.99-.
004540     03  W100-90         PIC Z(08)9.99-.
004540     03  W100-120        PIC Z(08)9.99-.
004550     03  W100-INT        PIC Z(08)9.99.
004560     03  W100-CRL        PIC Z(06)9.  
004570     03  W100-KEY        PIC Z(05)9.

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

006370 01  S12.
006380	   03  LINE 13 COLUMN 12 VALUE "For Terms: ".
	   03	       COLUMN 23 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 24 VALUE "ll, ".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 29 VALUE "-Days, ".
	   03	       COLUMN 36 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 37 VALUE "0-Days, ".
	   03	       COLUMN 45 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 46 VALUE "0-Days, ".
	   03	       COLUMN 54 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "C".
	   03	       COLUMN 55 VALUE ".O.D.".

006580 01  S15.
006590	   03  LINE  9 COLUMN 12 VALUE
006600		       "Do you want the Debtor address printed (".
	   03	       COLUMN 52 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 53 VALUE "/".
	   03	       COLUMN 54 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 55 VALUE ")".

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
	     PERFORM XC000.
	   IF PREVIOUS-PERIOD
	       CLOSE DBHIST.

       AA49.
006950	     EXIT PROGRAM.

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
016170 XC000		      SECTION 5.
016180 XC00-INIT.
016190       DISPLAY CLR-SCREEN.
016200       DISPLAY "AGE ANALYSIS" AT 0334 WITH FOREGROUND-COLOR 15.
             DISPLAY "Printing for" AT 0512.

016210 XC05-ACCEPT.
016220	     DISPLAY W95-COMP AT 0527 WITH FOREGROUND-COLOR 3 HIGHLIGHT.
016230       DISPLAY S07.
016240	     ACCEPT WS-OPTION AT 0731
016250		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016310	   IF WS-OPTION = "N"
016320         GO TO XC999-EXIT.
016330       DISPLAY S15.
016340	     ACCEPT WS-ADDR AT 0957
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-ADDR
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.

016540 XC08.
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
016500	       GO TO XC08.
016550	     MOVE 1		 TO WS-SUB-2.
016570       MOVE 0 TO WS-PAGE W90-BAL W90-CUR W90-30 W90-60
016580                         W90-90 W90-120 W90-MTD W90-INT.
	     PERFORM SAVE-SCREEN-3.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     PERFORM RESTORE-SCREEN-3.
	     DISPLAY S12.
	     MOVE "A"		 TO WS-TERMS.

       XC07.
013760	     ACCEPT WS-TERMS AT 1361
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-TERMS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT(WS-TERMS = "A" OR "C" OR "3" OR "6" OR "7")
	       GO TO XC07.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Convert user friendly options as accepted on screen to    ³
      *    ³ code as stored on the account record (DEB-TERMS).	       ³
      *    ³ 0 = 7 Days, 1 = 30 Days, 2 = 60 Days and 3 = C.O.D.       ³
      *    ³ For 'A'll leave the option as A.			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WS-TERMS = "7"
	       MOVE "0" 	TO WS-TERMS
	   ELSE
	   IF WS-TERMS = "3"
	       MOVE "1" 	TO WS-TERMS
	   ELSE
	   IF WS-TERMS = "6"
	       MOVE "2" 	TO WS-TERMS
	   ELSE
	   IF WS-TERMS = "C"
	       MOVE "3" 	TO WS-TERMS.
	     PERFORM CHECK-CORRECT.
013920	   IF WS-OPTION = "N"
013930	       GO TO XC07.

016610 XC10-HEADING.
	     MOVE "H"		 TO W02-PRN-TYPE.
016620	     MOVE SPACES	 TO REP-DETAIL1.
016630	     MOVE "PAGE:"	 TO TRL-PAGE.
016640	     MOVE "DATE:"	 TO TRL-DATE.
016650	     ADD 1		 TO WS-PAGE.
016660	     MOVE W12-TODAY	 TO TRL-DTE.
016670	     MOVE WS-PAGE	 TO TRL-P-NO.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO TRL-LINE1
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO TRL-LINE1.
016680	     MOVE W95-COMP	 TO TRL-CO.
016690	     MOVE " -AGE ANALYSIS"
				 TO TRL-EXHEAD.
	   IF WS-TERMS = "0"
	       MOVE "(7 Day Accounts)"
				 TO TRL-ACS
	   ELSE
	   IF WS-TERMS = "1"
	       MOVE "(30 Day Accounts)"
				 TO TRL-ACS
	   ELSE
	   IF WS-TERMS = "2"
	       MOVE "(60 Day Accounts)"
				 TO TRL-ACS
	   ELSE
	   IF WS-TERMS = "3"
	       MOVE "(C.O.D. Accounts)"
				 TO TRL-ACS
	   ELSE
	       MOVE "(ALL Accounts)"
				 TO TRL-ACS.
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
016710	     MOVE SPACES	 TO TRL-DET.

016720 XC12-HEADING.
016730	     MOVE "AC-NO."	 TO AGE-HEAD1.
016740	     MOVE "NAME"	 TO AGE-HEAD2.
016750	     MOVE "BALANCE"	 TO AGE-HEAD3.
           IF WS-AGE = "Y"
016760	       MOVE "M.T.D."	 TO AGE-HEAD4
016770	       MOVE "CURRENT"	 TO AGE-HEAD5
016780	       MOVE "30 DAYS"	 TO AGE-HEAD6
016790	       MOVE "60 DAYS"	 TO AGE-HEAD7
016800	       MOVE " 90 DAYS"	 TO AGE-HEAD8
           ELSE
016760	       MOVE "M.T.D."	 TO AGE-HEAD4
016780	       MOVE "30 DAYS"	 TO AGE-HEAD5
016790	       MOVE "60 DAYS"	 TO AGE-HEAD6
016800	       MOVE "90 DAYS"	 TO AGE-HEAD7
	       MOVE "120 DAYS"	 TO AGE-HEAD8.
016810	     MOVE "INTEREST"	 TO AGE-HEAD9.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
016860	     MOVE SPACES	 TO TRL-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

016870 XC13.
	     PERFORM RESTORE-SCREEN-3.
016880	     DISPLAY "Is this a restart (" AT 1312
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".

016890 XC14.
016900	     ACCEPT WS-OPTION AT 1336
016910		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016970	   IF NOT (WS-OPTION = "N" OR "Y")
016980         GO TO XC14.
016990       MOVE SPACES         TO WS-ACNO.
017000     IF WS-OPTION = "N"
017010         GO TO XC15-INDEX.

017020 XC14A.
017030	     DISPLAY WS-BLNK78 AT 1802 WITH FOREGROUND-COLOR 3.
017040	     DISPLAY "Account No:" AT 1812.
017050	     ACCEPT W80-ACNO AT 1824
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
017060     IF W80-ACNO = SPACES
017070         GO TO XC15-INDEX.
017080       PERFORM CA155-GET-DEBTOR.
010410	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
017100     IF WS-ERROR NOT = ZERO
017110	       MOVE "Invalid account number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017130         GO TO XC14A.
017140	     MOVE DEB-NAME	 TO W40-NAME.
017150     IF DEB-TYPE = 1
017160         INSPECT W40-NAME REPLACING ALL "*" BY "."
017170         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
017180	     DISPLAY W40-NAME AT 1831
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
017270	   IF WS-OPTION = "N"
017280         GO TO XC14A.
017290	     MOVE W80-ACNO	 TO WS-ACNO.
017300       MOVE "RESTART  RESTART  RESTART  RESTART RESTART RESTART"
017310				 TO TRL-RESTART.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017330	     MOVE SPACES	 TO TRL-DET.

017340 XC15-INDEX.
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
017370	       GO TO XC990-CLOSE.

017380 XC20.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO XC990-CLOSE.
	   IF PREVIOUS-PERIOD
021090	       PERFORM READ-DBHIST-NEXT THRU READ-DBHIST-EXIT
	       MOVE DBH-RECORD1  TO  DEB-RECORD1
	   ELSE
021090	       PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
010780	   IF WS-F-ERROR = 6 OR 56
	       GO TO XC25.
      *    **************************************************************
      *     Only print transactions for accounts with the User selected
      *     payment terms.
      *    **************************************************************
	   IF NOT(WS-TERMS = "A")
	       IF NOT(WS-TERMS = DEB-TERMS)
		   GO TO XC20.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY DEB-ACNO AT 2228
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
017400     IF WS-ACNO = DEB-ACNO
017410         MOVE SPACES       TO WS-ACNO.
	   IF WS-ZERO = "N"
               IF DEB-BAL = ZERO
		   GO TO XC20.
017450	     MOVE DEB-NAME	 TO W40-NAME.
017460     IF DEB-TYPE = 1
017470         INSPECT W40-NAME REPLACING ALL "*" BY "."
017480         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
017490       MOVE DEB-ACNO       TO AGE-AC.
017500       MOVE W40-NAME       TO AGE-NME.
017520       COMPUTE W25-VALUE = DEB-BAL - (DEB-CUR + DEB-30 +
                                 DEB-60 + DEB-90 + DEB-120 + DEB-INT).
017530	     ADD DEB-BAL	 TO W90-BAL.
	     ADD W25-VALUE	 TO W90-MTD.
017540	     ADD DEB-CUR	 TO W90-CUR.
017550	     ADD DEB-30 	 TO W90-30.
017560	     ADD DEB-60 	 TO W90-60.
017570	     ADD DEB-90 	 TO W90-90.
017570	     ADD DEB-120	 TO W90-90.
017580	     ADD DEB-INT	 TO W90-INT.
017680	     MOVE DEB-BAL	 TO AGE-BAL.
017670	     MOVE W25-VALUE	 TO AGE-MTD.
017790	     MOVE DEB-CUR	 TO AGE-CUR.
017790	     MOVE DEB-30	 TO AGE-30.
017800	     MOVE DEB-60	 TO AGE-60.
017810	     ADD DEB-90 DEB-120  GIVING AGE-90.
017820	     MOVE DEB-INT	 TO AGE-INT.
017870     IF WS-ACNO NOT = SPACES
017880         GO TO XC20.
017890     IF WS-ADDR = "Y"
017900         GO TO XC22.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017950	       PERFORM XC10-HEADING THRU XC12-HEADING.
017960	     MOVE SPACES	 TO TRL-DET.
017970       GO TO XC20.

017980 XC22.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
018000	     MOVE SPACES	 TO TRL-DET.
018010	     MOVE DEB-ADDRESS	 TO W60-ADDRESS.
018020       PERFORM CA175-ADDRESS.
018030	     MOVE W40-ADD1	 TO AGE-NME.
	     PERFORM CALL-PRINTUTL.
018050	   IF W40-ADD2 = SPACES
018060         GO TO XC22-PCDE.
018070	     MOVE W40-ADD2	 TO AGE-NME.
	     PERFORM CALL-PRINTUTL.
018090	   IF W40-ADD3 = SPACES
018100         GO TO XC22-PCDE.
018110	     MOVE W40-ADD3	 TO AGE-NME.
	     PERFORM CALL-PRINTUTL.
018130	   IF W40-ADD4 = SPACES
018140         GO TO XC22-PCDE.
018150	     MOVE W40-ADD4	 TO AGE-NME.
	     PERFORM CALL-PRINTUTL.

018170 XC22-PCDE.
018180	     MOVE SPACES	 TO TRL-DET.
018190	     MOVE DEB-POST	 TO TRL-PCDE.
018200	     MOVE "TEL NO'S:"	 TO TRL-HEAD13.
018210	     MOVE DEB-H-TEL	 TO TRL-HTEL.
018220	     MOVE DEB-B-TEL	 TO TRL-BTEL.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 4)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
018250	   ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
018270	       PERFORM XC10-HEADING THRU XC12-HEADING.
018280	     MOVE SPACES	 TO TRL-DET.
             GO TO XC20.

018290 XC25.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
018350	     MOVE "****  T O T A L S  ****"
				 TO AGE-NME.
018620	     MOVE W90-BAL	 TO AGE-BAL.
018590	     MOVE W90-MTD	 TO AGE-MTD.
018410	     MOVE W90-CUR	 TO AGE-CUR.
018440	     MOVE W90-30	 TO AGE-30.
018470	     MOVE W90-60	 TO AGE-60.
018500	     MOVE W90-90	 TO AGE-90.
018530	     MOVE W90-INT	 TO AGE-INT.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.

       XC990-CLOSE.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

019000 XC999-EXIT.
019010       EXIT.

      /
       YA000		      SECTION 80.
       YA00.
019040       DISPLAY CLR-SCREEN.
	     DISPLAY "DEBTORS (ACCOUNTS RECEIVABLE) AGE ANALYSIS"
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
	     DISPLAY "DEBTORS (A/R) PREVIOUS AGE ANALYSIS" AT 0324
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
	     DISPLAY "DEBTORS (A/R) PREVIOUS AGE ANALYSIS" AT 0324
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
	     OPEN I-O DBHIST.
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
	     CLOSE DBHIST.

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
	     MOVE 4		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-AGE	 TO WS-AGE.
025630	     MOVE 1		 TO WS-PARKEY.
025640       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025650	     MOVE PAR-COMPANY	 TO W95-COMP.
025660	     MOVE PAR-DMY	 TO W12-TODAY.
025670	     MOVE PAR-YMD	 TO W12-T-YMD.
025680     IF PAR-STMINV = "S" OR "X"
025690	       MOVE "N" 	 TO WS-HEAD
025700     ELSE
025710	       MOVE "Y" 	 TO WS-HEAD.
025730	     MOVE 1		 TO WS-NETKEY.
025740       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
025830	     GO TO ZA999-EXIT.
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
041760	   ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041760	   ELSE
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
