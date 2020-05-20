      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******   ********   *
      *   **	**     **     **    **	 **  **   **	**  **	       *
      *   **	**     **     **    **	**    **       **   *******    *
      *   **	**     **     *******	**    **     ** 	  **   *
      *   **	**     **     **	**    **   **		  **   *
      *   **	**     **     **	 **  **   **	    **	  **   *
      *   *******      **     **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R	  T R A N S A C T I O N   J O U R N A L        *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *    May 2006	- Include option to print only Accounts with   *
      * 		  payment terms as selected by the User.       *
      * 		- See also DTP026: Age analysis.	       *
      * 							       *
      * 		- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP025.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   July 1983.

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

000140 COPY DEBTOR.ISL.

000160 COPY DBTRAN.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000250 COPY PARAM.FDE.

000270 COPY DEBTOR.FDE.

000290 COPY DBTRAN.FDE.

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
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002990 77  WS-OPTION	   PIC X(01).
002990 77  WS-AGE          PIC X(01).
002990 77  WS-SEQ          PIC X(01).
003000 77  WS-TERMS	   PIC X(01).
002990 77  WS-ZERO         PIC X(01).
       77  WS-NUM          PIC Z9.
003020 77  WS-ADDR         PIC X(01).
003030 77  WS-ERROR        PIC 9(01).
003040 77  WS-ERROR1       PIC 9(01).
003110 77  WS-EXPAND	   PIC X(01)	 VALUE " ".
003150 77  WS-CHAR1	   PIC X(01).
003160 77  WS-CHAR2        PIC X(01).
003180 77  WS-COMP         PIC 9(01).
003180 77  WS-STM          PIC 9(01).
003190 77  WS-TYPE         PIC X(01).
003200 77  WS-HEAD         PIC X(01).
003230 77  WS-REF	   PIC 9(08).
003240 77  WS-MESS         PIC X(02).
003250 77  WS-ACNO         PIC X(06).
003260 77  WS-S1           PIC 9(04)     COMP-5.
003270 77  WS-S2           PIC 9(04)     COMP-5.
003280 77  WS-S3           PIC 9(04)     COMP-5.
003290 77  WS-S4           PIC 9(04)     COMP-5.
003300 77  WS-S5           PIC 9(04)     COMP-5.
003310 77  WS-S6           PIC 9(04)     COMP-5.
003320 77  WS-S7           PIC 9(04)     COMP-5.
003330 77  WS-S8           PIC 9(04)     COMP-5.
003360 77  WS-ERR1	   PIC X(22)	  VALUE
003370                     "Invalid account number".
003380 77  WS-ERR2         PIC X(09) VALUE "No record".
003390 77  WS-A            PIC X(19) VALUE "Any key to continue".
003400 77  WS-C            PIC X(07) VALUE "COMPANY".
003410 77  WS-O            PIC X(08) VALUE "INTERNAL".
003420 77  WS-AC           PIC X(07) VALUE "Account".
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

       COPY DEBTOR.ID.

       COPY DBTRAN.ID.

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
000880	       05  FILLER      PIC  X(01).
	       05  REP-SEQ     PIC  X(01).
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
001110	       05  TRL-HEAD1   PIC  X(06).
001120	       05  TRL-HEAD2   PIC  X(35).
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
	       05  FILLER      PIC  X(06).
	       05  TRL-CO      PIC  X(30).
	       05  TRL-EXHEAD  PIC  X(24).
	       05  FILLER      PIC  X(08).
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
001790	       05  CAT-DESC.
		   07  FILLER  PIC  X(13).
		   07  CAT-ACS PIC  X(17).
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

       COPY W15.WS.

       COPY W20.WS.

       01  W23-PERIOD.
           03  FILLER          PIC X(17) VALUE "For the period : ".
	   03  W23-DTE1        PIC Z9/99/9999.
           03  FILLER          PIC X(04) VALUE " to ".
	   03  W23-DTE2        PIC Z9/99/9999.

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
005240 01  W65-ENTRY.
005250     03  W65-FOUR.
005260         05  W65-CHAR OCCURS 8 PIC X(01).
005270     03  W65-PREFIX REDEFINES W65-FOUR.
005280         05  W65-PREF    PIC X(02).
005290         05  FILLER      PIC X(06).
003670 01  W65-INDEX.
003680     03  W65-INPUT.
003690         05  W65-ICHAR OCCURS 8 PIC X(01).
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

006330 01  S10.
006340	   03  LINE  3 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
006350                           VALUE "TRANSACTION JOURNAL".
006360     03  LINE  5 COLUMN 12 VALUE "Printing for :".

006370 01  S11.
006380	   03  LINE 12 COLUMN 12 VALUE "Key in Transaction code".

006370 01  S12.
006380	   03  LINE 14 COLUMN 12 VALUE "For Terms: ".
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

       01  S18.
	   03  LINE  8 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE  9 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

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
	     PERFORM EA000.

       AA49.
006950	     EXIT PROGRAM.

       COPY DATE.CHK.

       COPY DATE.CNV.

       COPY FUNCTION.CRT.

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
      
       COPY DEBTOR.RD1.
      
       COPY DBTRAN.RD.
      
      /
008930 CA155-GET-DEBTOR        SECTION 2.
008940 CA155-ACCOUNT.
012010       MOVE W80-ACNO       TO DEB-ACNO.
014330       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
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
009140	     MOVE ZERO		 TO WS-ERROR.
009150       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
009160				 INTO W40-SURNAME W40-FNAME W40-SNAME
009170         OVERFLOW
009180		 MOVE 1 	 TO WS-ERROR.
009190 CA170-EXIT.
009200       EXIT.

009210 CA175-ADDRESS           SECTION 2.
009220 CA175-EXTRACT.
009230	     MOVE 0		 TO WS-ERROR.
009240       UNSTRING W60-ADDRESS DELIMITED BY "*"
009250				 INTO W40-ADD1 W40-ADD2
				      W40-ADD3 W40-ADD4
009260         OVERFLOW
009270		 MOVE 1 	 TO WS-ERROR.
009280 CA180-EXIT.
009290       EXIT.

      /
013580 EA000		       SECTION 5.
013590 EA00-INIT.
013600       DISPLAY CLR-SCREEN.
013610       DISPLAY S10.

013620 EA05-ACCEPT.
013630	     DISPLAY W95-COMP AT 0527
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
013640	     DISPLAY S07.
	     PERFORM UNTIL WS-OPTION = "Y" OR "N"
013650	       ACCEPT WS-OPTION AT 0731
013660		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
013720	   IF WS-OPTION = "N"
013730         GO TO EA999-EXIT.
	     DISPLAY WS-BLNK78 AT 0702
		     WITH BACKGROUND-COLOR 1 FOREGROUND-COLOR 3.
             DISPLAY "For the period." AT 0712.
             DISPLAY "Start date :" AT 0812.
             DISPLAY "End date   :" AT 0912.
	     DISPLAY "Dates to be keyed in as DDMMCCYY" AT 1012
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
             MOVE ZERO           TO W15-START W15-END.

       EA06.
             ACCEPT S18.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
018530         GO TO EA06.
	     MOVE W15-START	 TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO EA06.
             MOVE W20-DTE        TO W15-S-YMD.
             MOVE W15-END        TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO EA06.
             MOVE W20-DTE        TO W15-E-YMD.
	     MOVE W15-START	 TO W23-DTE1.
	     MOVE W15-END	 TO W23-DTE2.
013740       DISPLAY S11.
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

013750 EA07.
013760	     ACCEPT W45-CODE AT 1236
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
013770     IF W45-CODE = ZERO
013780	       GO TO EA990-CLOSE.
      *
      *    ****   Additional security to block the Invoices from
      * 	  being printed for anyone with a security level
      * 	  for accounts receivable (debtors) less than 3.
      *
003240	   IF (LS0-DBLEV < 3) AND (W45-CODE = 01)
	       MOVE "Not Authorised for this code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013810         GO TO EA07.
013790	   IF (W45-CODE > 99) OR (W00-E-DESC(W45-CODE) = SPACES)
013800	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013810         GO TO EA07.
013820	     DISPLAY W00-E-DESC(W45-CODE) AT 1244
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
013920	   IF WS-OPTION = "N"
013930         GO TO EA07.
	     DISPLAY S12.
	     MOVE "A"		 TO WS-TERMS.

       EA07A.
	     PERFORM TEST AFTER
		     UNTIL WS-TERMS = "A" OR "C" OR "3" OR "6" OR "7"
013760	       ACCEPT WS-TERMS AT 1461
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	       CALL "CBL_TOUPPER" USING WS-TERMS
				  BY VALUE WS-LENGTH
				 RETURNING WS-STATUS
	     END-PERFORM.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Convert user friendly options as accepted on screen to    ³
      *    ³ code as stored on the account record (DEB-TERMS).	       ³
      *    ³ 0 = & Days, 1 = 30 Days, 2 = 60 Days and 3 = C.O.D.       ³
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
013930         GO TO EA07.
	   IF W45-CODE = 02
	       MOVE ZERO	 TO WS-REF.

013940 EA08.
013950       MOVE 1              TO WS-SUB-2.
013970	     MOVE 0		 TO WS-PAGE W90-BAL W90-BDT W90-BCR.

013980 EA10-HEADING.
	     MOVE "H"		 TO W02-PRN-TYPE.
013990	     MOVE SPACES	 TO REP-DETAIL1.
014000	     MOVE "PAGE:"	 TO COM-PAGE.
014010	     MOVE "DATE:"	 TO COM-DATE.
014020	     ADD 1		 TO WS-PAGE.
014030	     MOVE W12-TODAY	 TO COM-DTE.
014040	     MOVE WS-PAGE	 TO COM-P-NO.
014050	     MOVE W95-COMP	 TO COM-COMPANY.
014060	     MOVE "-TRANSACTION JOURNAL"
				 TO COM-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
014080	     MOVE SPACES	 TO COM-LINE1.

014090 EA12-HEADING.
014100	     MOVE "TRN TYPE:"	 TO CAT-H1.
014110	     MOVE W45-CODE	 TO CAT-CODE.
014120	     MOVE W00-E-DESC(W45-CODE)
				 TO CAT-DESC.
	   IF WS-TERMS = "0"
	       MOVE "(7 Day Accounts)"
				 TO CAT-ACS
	   ELSE
	   IF WS-TERMS = "1"
	       MOVE "(30 Day Accounts)"
				 TO CAT-ACS
	   ELSE
	   IF WS-TERMS = "2"
	       MOVE "(60 Day Accounts)"
				 TO CAT-ACS
	   ELSE
	   IF WS-TERMS = "3"
	       MOVE "(C.O.D. Accounts)"
				 TO CAT-ACS
	   ELSE
	       MOVE "(ALL Accounts)"
				 TO CAT-ACS.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
014140	     MOVE SPACES	 TO REP-LINE7.
014150	     MOVE "AC-NO"	 TO REP-HEAD7.
014160	     MOVE "NAME"	 TO REP-HEAD8.
014170	     MOVE "DATE"	 TO REP-HEAD9.
014180	     MOVE "REFER"	 TO REP-HEAD10.
014190	     MOVE "VALUE"	 TO REP-HEAD11.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
014210	     MOVE SPACES	 TO TRL-DET.

014220 EA15-INDEX.
014230       MOVE W45-CODE       TO TRA-CODE.
             MOVE W15-S-YMD      TO TRA-DATE.
             PERFORM START-AT-DTRN-CODE THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
014240       GO TO EA25-CHR.

014250 EA22.
014260       PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
014270        (TRA-CODE NOT = W45-CODE) OR 
              (TRA-DATE > W15-E-YMD)
014280         GO TO EA25-CHR.
      *    **************************************************************
      *      The Account record is now read prior to any processing of
      *      the transaction record.
      *    **************************************************************
014350	     MOVE TRA-AC	 TO W80-ACNO DEB-ACNO.
014370       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
      *    **************************************************************
      *     Only print transactions for accounts with the User selected
      *     payment terms.
      *    **************************************************************
	   IF NOT(WS-TERMS = "A")
	       IF NOT(WS-TERMS = DEB-TERMS)
		   GO TO EA22.
	   IF TRA-CODE = 02
	       IF WS-REF = ZERO
		   MOVE TRA-REF  TO WS-REF
	       ELSE
		   ADD 1	 TO WS-REF.
014290	   IF TRA-VALUE = ZERO
014300         GO TO EA22.
014310	     MOVE TRA-DATE	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY.
	     MOVE W22-DTE3	 TO REP-TDTE.
014320	     MOVE TRA-REF	 TO REP-TREF.
014330	     MOVE TRA-VALUE	 TO REP-TVAL.
014340	     ADD TRA-VALUE	 TO W90-BDT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ May 2006 - As the new option to selectively print for a   ³
      *    ³ specific or all Payment terms, the account details will   ³
      *    ³ now be read before any processing of the transaction      ³
      *    ³ record is performed.				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
014350*	     MOVE TRA-AC	 TO W80-ACNO DEB-ACNO.
014370*       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
014380	     MOVE DEB-NAME	 TO W40-NAME.
014390     IF DEB-TYPE = 1
014400         INSPECT W40-NAME REPLACING ALL "*" BY "."
014410         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
014420	     MOVE DEB-ACNO	 TO REP-TAC.
014430	     MOVE W40-NAME	 TO REP-TNAME.
	   IF TRA-CODE = 02
	       IF NOT(WS-REF = TRA-REF)
		   MOVE "*"	 TO REP-SEQ
		   IF WS-REF < TRA-REF
		       MOVE TRA-REF
				 TO WS-REF
		   ELSE
		       SUBTRACT 1
				 FROM WS-REF.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014480	       PERFORM EA10-HEADING THRU EA12-HEADING.
014490	     MOVE SPACES	 TO REP-LINE7.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO EA990-CLOSE.
014530       GO TO EA22.

014540 EA25-CHR.
014550	     MOVE "TRANSACTION TOTAL"
				 TO REP-TNAME.
014560	     MOVE W90-BDT	 TO REP-TVAL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.

014580 EA26.
	   IF WS-PRINTER = "S"
	       PERFORM RESTORE-SCREEN-3.
014590       GO TO EA07.

       EA990-CLOSE.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

014600 EA999-EXIT.
014610       EXIT.

      /
025020 ZA00-INIT	      SECTION 9.
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
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     COPY FUNCTION.PRO.
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
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.

       ZA02.
025430	     MOVE 11		 TO WS-PARKEY.
025440	     MOVE 1		 TO WS-SUB-2.

025450 ZA15-READ-TRANS.
025460       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025490	     MOVE 1		 TO WS-SUB-1.

025500 ZA20-TRAN-LOOP.
025510	     MOVE PAR-T-CODE(WS-SUB-1)
				 TO W00-TRAN-CODE(WS-SUB-2).
025520	     MOVE PAR-E-DESC(WS-SUB-1)
				 TO W00-E-DESC(WS-SUB-2).
025530	     MOVE PAR-A-DESC(WS-SUB-1)
				 TO W00-A-DESC(WS-SUB-2).
025540	     MOVE PAR-ACTION(WS-SUB-1)
				 TO W00-ACTION(WS-SUB-2).
025550	     ADD 1		 TO WS-SUB-2.
025560     IF WS-SUB-1 < 2
025570	       ADD 1		 TO WS-SUB-1
025580         GO TO ZA20-TRAN-LOOP.
025590     IF WS-PARKEY < 60
025600	       ADD 1		 TO WS-PARKEY
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
025680     IF PAR-STMINV = "S" OR "X"
025690	       MOVE "N" 	 TO WS-HEAD
025700     ELSE
025710	       MOVE "Y" 	 TO WS-HEAD.
025730	     MOVE 1		 TO WS-NETKEY.
025740       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
025830	     GO TO ZA999-EXIT.

       COPY ZA49.PRO.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(8)	WS-TCHR(49)
021940				    WS-TCHR(61) WS-TCHR(72).
021950	     MOVE WS-G3 	 TO WS-MCHR(8)	WS-MCHR(49)
021960				    WS-MCHR(61) WS-MCHR(72)
021960				    WS-MCHR(1)	WS-MCHR(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(8)	WS-BCHR(49)
021980				    WS-BCHR(61) WS-BCHR(72).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
025840 ZA200.

       COPY LOCKED.PRO.

       ZA205.
             EXIT PROGRAM.

025920 ZA999-EXIT.
025930       EXIT.
      /
025940 ZB000		     SECTION 9.

       COPY ERRORS.PRO.

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
041870	       MOVE WS-PARKEY	 TO WS-KEY.

       COPY DISPERR.PRO.
