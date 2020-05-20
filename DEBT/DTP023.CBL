      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******    ******    *
      *   **	**     **     **    **	 **  **   **	**  **	  **   *
      *   **	**     **     **    **	**    **       **	  **   *
      *   **	**     **     *******	**    **     ** 	***    *
      *   **	**     **     **	**    **   **		  **   *
      *   **	**     **     **	 **  **   **	    **	  **   *
      *   *******      **     **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R	  A D D R E S S   L A B E L S	P R O G R A M  *
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
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP023.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   November 1983.

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

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000250 COPY PARAM.FDE.

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
002790 77  WS-SUB-1	   PIC	9(04)	 COMP-5.
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
002930 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
002940 77  WS-WRKEY	   PIC	9(04)	  COMP-5.
002960 77  WS-NTP	   PIC	9(01).
002970 77  WS-LINES	   PIC	9(02).
002980 77  WS-PAGE	   PIC	9(04)	  COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002990 77  WS-OPTION	   PIC	X(01).
002990 77  WS-SEQ	   PIC	X(01).
003030 77  WS-ERROR	   PIC	9(01).
003040 77  WS-ERROR1	   PIC	9(01).
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
003420 77  WS-AC	   PIC X(07) VALUE "Account".
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
	       05  TRL-EXPP    PIC  X(06).
	       05  FILLER      PIC  X(06).
	       05  TRL-CO      PIC  X(30).
	       05  TRL-EXHEAD  PIC  X(24).
	       05  TRL-EXPC    PIC  X(06).
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

003650 01  W05-LIST.
003660     03  W05-OPT         PIC 9(03).
003670     03  W05-TYPES REDEFINES W05-OPT.
003680         05  W05-COMP    PIC 9(01).
003690         05  W05-INDV    PIC 9(01).
003700         05  W05-INT     PIC 9(01).

003710* 01  W10-SPARES.
003720*	   03  W10-DATE        PIC X(10).
      *     03	W10-RDATE REDEFINES W10-DATE.
      *	       05  W10-DTE     PIC 99/99/9999.

       COPY W12.WS.

       01  W15-OPTIONS.
	   03  W15-OPTS.
003200	       05  W15-CAT     PIC  X(01).
	       05  W15-ALL     PIC  X(01).
002990	       05  W15-ZERO    PIC  X(01).
002990	       05  W15-L30     PIC  X(01).
002990	       05  W15-L60     PIC  X(01).
002990	       05  W15-L90     PIC  X(01).
002990	       05  W15-30      PIC  X(01).
002990	       05  W15-60      PIC  X(01).
002990	       05  W15-90      PIC  X(01).
002990	       05  W15-90P     PIC  X(01).
	   03  W15-REPEAT REDEFINES W15-OPTS.
	       05  W15-OPT     PIC  X(01) OCCURS 10.

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

006120 01  S03.
	   02  S03-1.
	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "DEBTORS - ADDRESS LABELS".
	   02  S03-2.
006130	   03  LINE  4 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
006140                           VALUE "Printing Client labels".
006150	   03  LINE  6 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")".
      *
      * 	   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * 	   ³	 F I L T E R   O P T I O N S	  ³°°
      * 	   ³				      Y/N ³°°
      * 	   ³ Selective per Category/Area      [ ] ³°°
      * 	   ³					  ³°°
      * 	   ³ Include: 1 All accounts	      [ ] ³°°
      * 	   ³	      2 Zero balance accounts [ ] ³°°
      * 	   ³	      3 Accounts < 30 days    [ ] ³°°
      * 	   ³	      4 Accounts < 60 days    [ ] ³°°
      * 	   ³	      5 Accounts < 90 days    [ ] ³°°
      * 	   ³	      6 Only 30 day accounts  [ ] ³°°
      * 	   ³	      7 Only 60 day accounts  [ ] ³°°
      * 	   ³	      8 Only 90 day accounts  [ ] ³°°
      * 	   ³	      9 Only 90+ day ccounts  [ ] ³°°
      * 	   ³ 1 & 2 = Y, include all accounts, 2 = ³°°
      * 	   ³ Y, only accounts with zero balances. ³°°
      * 	   ³ If any of 3 - 9 = Y, then all others ³°°
      * 	   ³ from 1 - 9 must contain an N.	  ³°°
      * 	   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      * 	     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
006160 01  S04.
	   03  LINE  4 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
		       VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "¿".
	   03  LINE  5 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
		       VALUE "³     F I L T E R   O P T I O N S      ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE  6 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
		       VALUE "³                                  Y/N ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE  7 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³ ".
	   03	       COLUMN 23 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		       VALUE "Selective per Category/Area      [ ] ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE  8 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
		       VALUE "³                                      ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE  9 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³ ".
	   03	       COLUMN 23 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		       VALUE "Include: 1 All accounts          [ ] ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 10 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³          ".
	   03	       COLUMN 32 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "2 Zero balance accounts [ ] ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 11 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³          ".
	   03	       COLUMN 32 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "3 Accounts < 30 days    [ ] ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 12 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³          ".
	   03	       COLUMN 32 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "4 Accounts < 60 days    [ ] ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 13 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³          ".
	   03	       COLUMN 32 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "5 Accounts < 90 days    [ ] ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 14 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³          ".
	   03	       COLUMN 32 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "6 Only 30 day accounts  [ ] ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 15 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³          ".
	   03	       COLUMN 32 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "7 Only 60 day accounts  [ ] ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 16 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³          ".
	   03	       COLUMN 32 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "8 Only 90 day accounts  [ ] ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 17 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³          ".
	   03	       COLUMN 32 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "9 Only 90+ day accounts [ ] ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 18 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³ ".
	   03	       COLUMN 23 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
		       VALUE "1 & 2 = Y, include all accounts, 2 = ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 19 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³ ".
	   03	       COLUMN 23 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
		       VALUE "Y, only accounts with zero balances. ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 20 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³ ".
	   03	       COLUMN 23 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
		       VALUE "If any of 3 - 9 = Y, then all others ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 21 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "³ ".
	   03	       COLUMN 23 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
		       VALUE "from 1 - 9 must contain an N.        ".
	   03	       COLUMN 60 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 22 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "À".
	   03	       COLUMN 22 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		       VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S04-A.
	   03  LINE  7 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(01) USING W15-CAT AUTO.
	   03  LINE  9 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(01) USING W15-ALL AUTO.
	   03  LINE 10 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(01) USING W15-ZERO AUTO.
	   03  LINE 11 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(01) USING W15-L30 AUTO.
	   03  LINE 12 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(01) USING W15-L60 AUTO.
	   03  LINE 13 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(01) USING W15-L90 AUTO.
	   03  LINE 14 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(01) USING W15-30 AUTO.
	   03  LINE 15 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(01) USING W15-60 AUTO.
	   03  LINE 16 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(01) USING W15-90 AUTO.
	   03  LINE 17 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(01) USING W15-90P AUTO.

006190 01  S05.
006200	   03  LINE 10 COLUMN 12 VALUE "Key in Category code".

       COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
003240	   IF LS0-DBLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	     PERFORM CH000.

       AA49.
006950	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP8.

      /
      *    ****    READ FILES ROUTINES
      *
007340 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.
      
       COPY DEBTOR.RD1.
      
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
009180		   MOVE 1	 TO WS-ERROR.
009190 CA170-EXIT.
009200       EXIT.

009210 CA175-ADDRESS           SECTION 2.
009220 CA175-EXTRACT.
009230	     MOVE 0		 TO WS-ERROR.
009240       UNSTRING W60-ADDRESS DELIMITED BY "*"
009250				 INTO W40-ADD1 W40-ADD2
				      W40-ADD3 W40-ADD4
009260         OVERFLOW
009270		   MOVE 1	 TO WS-ERROR.
009280 CA180-EXIT.
009290       EXIT.
      /
      *    ****    C L I E N T   L A B E L S
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
009670	     DISPLAY CLR-SCREEN.
	     DISPLAY S03-1.
009760	   IF WS-OPTION = "N"
009770         GO TO CH999.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE "D"		 TO W02-PRN-TYPE.
009780	     MOVE SPACES	 TO REP-DETAIL1.
	   IF WS-PRINTER = "D" OR "S"
	       MOVE 1		 TO WS-S6
	       GO TO CH30.
      *    *************************************************************
      *    ****   S E T U P   T E S T	L A B E L S   T O   B E
      * 	  P R I N T E D
      *    *************************************************************
009790	     MOVE ALL "X"	 TO W46-LABELS.
009800	     MOVE "9999"	 TO W46-PC (1) W46-PC (2).
      *    *************************************************************
      *    ****   S E T U P   L A B E L - 1   ( L E F T )  T O	 B E
      * 	  P R I N T E D
      *    *************************************************************
009810 CH05.
009820     IF W46-ADD2 (1) = SPACES
009830         MOVE W46-POST (1) TO W46-ADD2 (1)
009840	       MOVE SPACES	 TO W46-POST (1)
009850	       GO TO CH10.
009860     IF W46-ADD3 (1) = SPACES
009870         MOVE W46-POST (1) TO W46-ADD3 (1)
009880	       MOVE SPACES	 TO W46-POST (1)
009890	       GO TO CH10.
009900     IF W46-ADD4 (1) = SPACES
009910         MOVE W46-POST (1) TO W46-ADD4 (1)
009920	       MOVE SPACES	 TO W46-POST (1).
      *    *************************************************************
      *    ****   S E T U P   L A B E L - 2   ( R I G H T )  T O   B E
      * 	  P R I N T E D
      *    *************************************************************
009930 CH10.
009940     IF W46-ADD2 (2) = SPACES
009950         MOVE W46-POST (2) TO W46-ADD2 (2)
009960	       MOVE SPACES	 TO W46-POST (2)
009970	       GO TO CH15.
009980     IF W46-ADD3 (2) = SPACES
009990         MOVE W46-POST (2) TO W46-ADD3 (2)
010000	       MOVE SPACES	 TO W46-POST (2)
010010	       GO TO CH15.
010020     IF W46-ADD4 (2) = SPACES
010030         MOVE W46-POST (2) TO W46-ADD4 (2)
010040	       MOVE SPACES	 TO W46-POST (2).
      *    *************************************************************
      *    **** 	  P R I N T   L A B E L / S
      *    *************************************************************
010050 CH15.
010060	     MOVE SPACES	 TO LAB-L1.
010070	     MOVE W46-NAME (1)	 TO LAB-DET (1).
010080	     MOVE W46-NAME (2)	 TO LAB-DET (2).
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      * 	  P R O G R A M   ?
      *    *************************************************************
	   IF WS-COMMAND = "Z"
	       GO TO CH20.
010100	     MOVE W46-ADD1 (1)	 TO LAB-DET (1).
010110	     MOVE W46-ADD1 (2)	 TO LAB-DET (2).
	     PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      * 	  P R O G R A M   ?
      *    *************************************************************
	   IF WS-COMMAND = "Z"
	       GO TO CH20.
010130	     MOVE W46-ADD2 (1)	 TO LAB-DET (1).
010140	     MOVE W46-ADD2 (2)	 TO LAB-DET (2).
	     PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      * 	  P R O G R A M   ?
      *    *************************************************************
	   IF WS-COMMAND = "Z"
	       GO TO CH20.
010160	     MOVE W46-ADD3 (1)	 TO LAB-DET (1).
010170	     MOVE W46-ADD3 (2)	 TO LAB-DET (2).
	     PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      * 	  P R O G R A M   ?
      *    *************************************************************
	   IF WS-COMMAND = "Z"
	       GO TO CH20.
010190	     MOVE W46-ADD4 (1)	 TO LAB-DET (1).
010200	     MOVE W46-ADD4 (2)	 TO LAB-DET (2).
	     PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      * 	  P R O G R A M   ?
      *    *************************************************************
	   IF WS-COMMAND = "Z"
	       GO TO CH20.
010220	     MOVE W46-POST (1)	 TO LAB-DET (1).
010230	     MOVE W46-POST (2)	 TO LAB-DET (2).
	     PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      * 	  P R O G R A M   ?
      *    *************************************************************
	   IF WS-COMMAND = "Z"
	       GO TO CH20.
010250	     MOVE SPACES	 TO LAB-L1.
	     MOVE 3		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      * 	  P R O G R A M   ?
      *    *************************************************************
	   IF WS-COMMAND = "Z"
	       GO TO CH20.
010290	     MOVE 1		 TO WS-S6.

       CH20.
	     EXIT.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      * 	  P R O G R A M   ?
      *    *************************************************************
010300 CH25.
	   IF WS-COMMAND = "Z"
	       GO TO CH130.
	     PERFORM CHECK-CORRECT.
010390	   IF WS-OPTION = "N"
010400	       GO TO CH15.
      *    *************************************************************
      *    ****    D I S P L A Y   O P T I O N S
      *    *************************************************************
       CH30.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 23		 TO SHADE-COL.
	     MOVE 38		 TO SHADE-WIDTH.
	     MOVE 18		 TO SHADE-LINES.
010420	     DISPLAY S04.
             PERFORM SCREEN-SHADOW.
	     MOVE "Y"		 TO W15-ALL W15-ZERO.
	     MOVE "N"		 TO W15-L30 W15-L60 W15-L90
				    W15-30  W15-60  W15-90
				    W15-90P W15-CAT.
      *    *************************************************************
      *    ****    G E T   U S E R   O P T I O N S
      *    *************************************************************
       CH35.
010430	     ACCEPT S04-A.
	     MOVE 1		 TO WS-S5.
	     MOVE ZERO		 TO WS-S4.
      *    *************************************************************
      *     C O N V E R T   A L L   I N P U T	T O   U P P E R C A S E
      *    *************************************************************
       CH40.
	     CALL "CBL_TOUPPER" USING W15-OPT(WS-S5)
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
      *    *************************************************************
      *    ****  I S   T H E   O P T I O N   V A L I D	 ( Y OR N ) ?
      *    *************************************************************
	   IF NOT (W15-OPT(WS-S5) = "Y" OR "N")
	       GO TO CH45.
      *    *************************************************************
      *    **** A C C U M U L A T E   N U M B E R   O F   O P T I O N S
      * 	( 2  T O  1 0 ) T H A T	  A R E	  ' Y ' .
      *    *************************************************************
	   IF (WS-S5 > 1) AND (W15-OPT(WS-S5) = "Y")
	       ADD 1		 TO WS-S4.
	   IF WS-S5 < 10
	       ADD 1		 TO WS-S5
	       GO TO CH40.
      *    *************************************************************
      *    ****  M O R E   T H A N   T W O   O P T I O N S   =	 Y ?
      *    *************************************************************
	   IF WS-S4 > 2
	       GO TO CH40-ERR.
      *    *************************************************************
      *    ****      O N L Y   T W O   O P T I O N S   =   Y ?
      *    *************************************************************
	   IF WS-S4 = 2
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The system only allows for option W15-ALL and W15-ZERO to ³
      *    ³ both contain a 'Y' to make a selection. For all other     ³
      *    ³ selections, only one option may contain a 'Y'.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       IF (W15-ALL = "Y") AND
		  (W15-ZERO = "Y")
		   GO TO CH50
	       ELSE
		  GO TO CH40-ERR.
	     GO TO CH50.
      *    *************************************************************
      *    **** 	 I N V A L I D	 S E L E C T I O N
      *    *************************************************************
       CH40-ERR.
	     MOVE "Invalid selection"
				 TO WS-ERR-STRING
003260	     PERFORM ERROR-MESSAGE.
	     GO TO CH35.
      *    *************************************************************
      *    **** A   C H A R A C T E R	O T H E R   T H A N   A N  'N'
      * 	O R   A   'Y'	H A S	B E E N   K E Y E D.
      *    *************************************************************
       CH45.
	     MOVE "Selections must be Y or N"
				 TO WS-ERR-STRING
003260	     PERFORM ERROR-MESSAGE.
	     GO TO CH35.

       CH50.
009670	     DISPLAY CLR-SCREEN.
009680	     DISPLAY S03-1.
      *    *************************************************************
      *    ****  H A S	 T H E	 P E R	 C A T E G O R Y   O P T I O N
      * 	 B E E N   S E L E C T E D ?
      *    *************************************************************
010500	   IF NOT (W15-CAT = "Y")
010510         MOVE ZERO TO W45-CODE
010520	       GO TO CH60.
010530       DISPLAY S05.

010540 CH55.
010550	     ACCEPT W45-CODE AT 1034
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
010560     IF W45-CODE = ZERO
010570	       GO TO CH115.
010580     IF W45-CODE > 80
010590	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
010600	       GO TO CH55.
010410	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
             ADD 121 W45-CODE    GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2
                                 GIVING W90-CATKEY
                                 REMAINDER W90-CS.
             ADD 1               TO W90-CS.
040190       MOVE W90-CATKEY     TO WS-PARKEY.
011780       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
010640	     DISPLAY PAR-CDESC(W90-CS) AT 1039
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
010740	   IF WS-OPTION = "N"
010750	       GO TO CH55.

       CH60.
             DISPLAY "A" AT 1212 WITH FOREGROUND-COLOR 11
                     "ccount or " WITH FOREGROUND-COLOR 3
                     "N" WITH FOREGROUND-COLOR 11
                     "ame sequence" WITH FOREGROUND-COLOR 3.
             MOVE "N"            TO WS-SEQ.

       CH65.
	     ACCEPT WS-SEQ AT 1237
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-SEQ
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-SEQ = "A" OR "N")
	       GO TO CH65.
	     DISPLAY "Is this a restart " AT 1412
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-OPTION.

       CH70.
	     ACCEPT WS-OPTION AT 1434
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO CH70.
           IF WS-OPTION = "Y"
	       GO TO CH120.
             MOVE " !"           TO DEB-NAME DEB-ACNO.

       CH75.
             DISPLAY "Debtor account:" AT 1612.
           IF WS-SEQ = "A"
               PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT
           ELSE
               PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
010780     IF WS-F-ERROR = 6
010790	       GO TO CH135.

010800 CH80.
010810       PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
010780     IF WS-F-ERROR = 6
010790	       GO TO CH115.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the account is an internal account then read the next  ³
      *    ³ account. (Do not print labels for internal accounts).     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF DEB-TYPE = 3
	       GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the option for the output is the screen then do not    ³
      *    ³ display the Debtor account number on the screen.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       CH85.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY DEB-ACNO AT 1628
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the selection is per category code then if the account ³
      *    ³ is not for the selected category then read next account.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
010820	   IF W15-CAT = "Y"
010830         IF W45-CODE NOT = DEB-CAT
010840		   GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the account balance is zero and the user has selected  ³
      *    ³ not to include zero accounts then read next account.      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF DEB-BAL = ZERO
	       IF W15-ZERO = "N"
		   GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only zero balance accounts and   ³
      *    ³ the balance is not zero then read next account.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF (W15-ALL = "N")
	       IF (W15-ZERO = "Y")
		   IF NOT (DEB-BAL = ZERO)
		       GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only accounts with a balance less³
      *    ³ than 30 days and the account has an amount outstanding for³
      *    ³ more than 30 days then read the next account.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF (W15-L30 = "Y") AND
	      ((DEB-30 > ZERO) OR (DEB-60 > ZERO) OR
	       (DEB-90 > ZERO) OR (DEB-120 > ZERO))
	       GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only accounts with a balance less³
      *    ³ than 60 days and the account has an amount outstanding for³
      *    ³ more than 60 days then read the next account.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF (W15-L60 = "Y") AND
	      ((DEB-60 > ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO))
	       GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only accounts with a balance less³
      *    ³ than 90 days and the account has an amount outstanding for³
      *    ³ more than 90 days then read the next account.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF (W15-L90 = "Y") AND
	      ((DEB-90 > ZERO) OR (DEB-120 > ZERO))
	       GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only 30 day accounts and the     ³
      *    ³ account is not a 30 day account or has amounts outstanding³
      *    ³ for more than 30 days then read the next account.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF (W15-30 = "Y")
	       IF (DEB-30 = ZERO) OR (DEB-60 > ZERO) OR (DEB-90 > ZERO)
		   OR (DEB-120 > ZERO)
		   GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only 60 day accounts and the     ³
      *    ³ account is not a 60 day account or has amounts outstanding³
      *    ³ for more than 60 days then read the next account.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF (W15-60 = "Y")
	       IF (DEB-60 = ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO)
		   GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only 90 day accounts and the     ³
      *    ³ account is not a 90 day account or has amounts outstanding³
      *    ³ for more than 60 days then read the next account.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF (W15-90 = "Y")
	       IF (DEB-90 = ZERO) OR (DEB-120 > ZERO)
		   GO TO CH80.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the user has selected only 90+ day accounts and the    ³
      *    ³ account is not a 90+ day account then read the next       ³
      *    ³ account.						       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF (W15-90P = "Y")
	       IF (DEB-120 = ZERO)
		   GO TO CH80.
010850	   IF NOT (DEB-TYPE = 1)
010860	       MOVE DEB-NAME	 TO W40-NAME
010870	       GO TO CH110.
010880	     MOVE DEB-NAME	 TO W60-NAME.
010890       PERFORM CA165-NAME.
010900     IF DEB-LANG = "A"
010910	       MOVE W75-ATITLE (DEB-TITLE)
				 TO W60-NAME
010920     ELSE
010930	       MOVE W75-ETITLE (DEB-TITLE)
				 TO W60-NAME.
010940	     MOVE 7		 TO WS-S8.

010950 CH90.
010960     IF W60-NCHAR (WS-S8) NOT = SPACE
010970	       GO TO CH95.
010980     IF WS-S8 > 0
010990	       SUBTRACT 1	 FROM WS-S8
011000	       GO TO CH90.

011010 CH95.
011020	     MOVE 1		 TO WS-S7.
011030     IF WS-S8 > 0
011040	       ADD 2		 TO WS-S8
011050     ELSE
011060	       MOVE 1		 TO WS-S8.
011070       MOVE W40-NCHAR (25) TO W60-NCHAR (WS-S8).
011080	     ADD 1		 TO WS-S8.
011090	     MOVE "."		 TO W60-NCHAR (WS-S8).
011100     IF W40-NCHAR (49) = SPACE
011110	       GO TO CH100.
011120	     ADD 1		 TO WS-S8.
011130       MOVE W40-NCHAR (49) TO W60-NCHAR (WS-S8).
011140	     ADD 1		 TO WS-S8.
011150	     MOVE "."		 TO W60-NCHAR (WS-S8).

011160 CH100.
011170	     ADD 2		 TO WS-S8.
011180 CH105.
011190	     MOVE W40-NCHAR (WS-S7)
				 TO W60-NCHAR (WS-S8).
011200     IF WS-S7 < 24
011210	       ADD 1		 TO WS-S7
011220	       ADD 1		 TO WS-S8
011230	       GO TO CH105.
011240	     MOVE W60-NAME	 TO W40-NAME.
011250 CH110.
011260	     MOVE DEB-ADDRESS	 TO W60-ADDRESS.
011270       PERFORM CA175-ADDRESS.
011280	     MOVE W40-NAME	 TO W46-NAME (WS-S6).
011290	     MOVE W40-ADD1	 TO W46-ADD1 (WS-S6).
011300	     MOVE W40-ADD2	 TO W46-ADD2 (WS-S6).
011310	     MOVE W40-ADD3	 TO W46-ADD3 (WS-S6).
011320	     MOVE W40-ADD4	 TO W46-ADD4 (WS-S6).
011330	     MOVE DEB-POST	 TO W46-PC (WS-S6).
011340	     ADD 1		 TO WS-S6.
011350     IF WS-S6 > 2
011360	       PERFORM CH05 THRU CH20
011370	       MOVE SPACES	 TO W46-LABELS.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO CH130.
	     GO TO CH80.
       CH115.
011420     IF WS-S6 > 1
011430	       PERFORM CH05 THRU CH20.
      *    *************************************************************
      *    ****   H A S   T H E   U S E R   A B O R T E D   T H E
      * 	  P R O G R A M   ?
      *    *************************************************************
	   IF WS-COMMAND = "Z"
	       GO TO CH130.
011440	   IF W45-CODE NOT = ZERO
011450	       GO TO CH55.
	     GO TO CH135.

       CH120.
             MOVE SPACES         TO W80-ACNO.
	     MOVE 1801		 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
017040	     DISPLAY "Account No:" AT 1812.
017050	     ACCEPT W80-ACNO AT 1824
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF W80-ACNO = SPACES
	       GO TO CH70.
             MOVE W80-ACNO       TO DEB-ACNO.
           IF WS-SEQ = "A"
	       GO TO CH75.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
             PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.

       CH125.
             PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF W80-ACNO NOT = DEB-ACNO
	       GO TO CH125.
             DISPLAY "Debtor account:" AT 1612.
	     GO TO CH85.

       CH130.
014970	     MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO REP-DETAIL1.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       CH135.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

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
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
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
025510	     MOVE PAR-T-CODE (WS-SUB-1)
				 TO W00-TRAN-CODE (WS-SUB-2).
025520	     MOVE PAR-E-DESC (WS-SUB-1)
				 TO W00-E-DESC (WS-SUB-2).
025530	     MOVE PAR-A-DESC (WS-SUB-1)
				 TO W00-A-DESC (WS-SUB-2).
025540	     MOVE PAR-ACTION (WS-SUB-1)
				 TO W00-ACTION (WS-SUB-2).
025550	     ADD 1		 TO WS-SUB-2.
025560     IF WS-SUB-1 < 2
025570	       ADD 1		 TO WS-SUB-1
025580         GO TO ZA20-TRAN-LOOP.
025590     IF WS-PARKEY < 60
025600	       ADD 1		 TO WS-PARKEY
025610         GO TO ZA15-READ-TRANS.

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
041770	   IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
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
