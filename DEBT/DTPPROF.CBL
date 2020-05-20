      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *  *******  ******** *******  *******  *******   ******  ******* *
      *  **    **    **    **	 ** **	  ** **    ** **    ** **      *
      *  **    **    **    **	 ** **	  ** **    ** **    ** **      *
      *  **    **    **    *******  *******  *******  **    ** *****   *
      *  **    **    **    **	    **	     **    ** **    ** **      *
      *  **    **    **    **	    **	     **    ** **    ** **      *
      *  *******     **    **	    **	     **    **  ******  **      *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R	  S A L E S   P E R   A C C O U N T	       *
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
000020 PROGRAM-ID.     DTPPROF.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   June 1992.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2011
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

       COPY CARDEX.SL.

       COPY CONTROL.SL.

000120 COPY PARAM.SL.

000140 COPY DEBTOR.ISL.

       COPY DEBMEM.SL.

000140 COPY DEBTOT.SL.

000140 COPY DEPART.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FDE.

       COPY CONTROL.FDE.

000250 COPY PARAM.FDE.

000270 COPY DEBTOR.FDE.

       COPY DEBMEM.FDE.

000310 COPY DEBTOT.FDE.

000140 COPY DEPART.FDE.

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
003260 77  WS-S1	   PIC	9(04)	  COMP-5.
003270 77  WS-S2	   PIC	9(04)	  COMP-5.
003280 77  WS-S3	   PIC	9(04)	  COMP-5.
003290 77  WS-S4	   PIC	9(04)	  COMP-5.
003300 77  WS-S5	   PIC	9(04)	  COMP-5.
003310 77  WS-S6	   PIC	9(04)	  COMP-5.
003320 77  WS-S7	   PIC	9(04)	  COMP-5.
003330 77  WS-S8	   PIC	9(04)	  COMP-5.
002850 77  WS-IXS1	   PIC	9(04)	  COMP-5.
002860 77  WS-IXS2	   PIC	9(04)	  COMP-5.
002870 77  WS-IXS3	   PIC	9(04)	  COMP-5.
002880 77  WS-IXS4	   PIC	9(04)	  COMP-5.
002890 77  WS-MSUB	   PIC	9(04)	  COMP-5.
002900 77  WS-PARKEY	   PIC	9(04)	  COMP-5.
002910 77  WS-DEPKEY	   PIC	9(04)	  COMP-5.
002910 77  WS-DEBKEY	   PIC	9(06)	  COMP-5.
002930 77  WS-NETKEY	   PIC	9(06)	  COMP-5.
002940 77  WS-WRKEY	   PIC	9(04)	  COMP-5.
       77  WS-ETYPE	   PIC	X(01).
       77  WS-CARDEX	   PIC	X(01).
002960 77  WS-NTP	   PIC	9(01).
002970 77  WS-LINES	   PIC	9(02).
002980 77  WS-PAGE	   PIC	9(04)	  COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002990 77  WS-OPTION	   PIC	X(01).
       77  WS-EXTR	   PIC	X(01).
002990 77  WS-ACT	   PIC	X(01).
002990 77  WS-STK	   PIC	X(01).
002990 77  WS-DEB	   PIC	X(01).
003000 77  WS-CREDIT	   PIC	X(01).
002990 77  WS-SKIP	   PIC	X(01).
       77  WS-NUM	   PIC	Z9.
003020 77  WS-ADDR	   PIC	X(01).
000500 77  WS-ERROR	   PIC	9(02)	 COMP-X VALUE 0.
003040 77  WS-ERROR1	   PIC	9(01).
003110 77  WS-EXPAND	   PIC	X(01)	 VALUE " ".
003150 77  WS-CHAR1	   PIC	X(01).
003160 77  WS-CHAR2	   PIC	X(01).
003180 77  WS-COMP	   PIC	9(01).
003180 77  WS-STM	   PIC	9(01).
003190 77  WS-TYPE	   PIC	X(01).
003200 77  WS-HEAD	   PIC	X(01).
003230 77  WS-BATCH	   PIC	9(05).
003240 77  WS-MESS	   PIC	X(02).
003250 77  WS-ACNO	   PIC	X(06).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440	       05  WS-TCHR PIC	X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
	       05  FILLER  PIC	X(01).
               05  WS-H-LINE
			   PIC	X(78).
	       05  FILLER  PIC	X(01).
002430     03  WS-TOP-LNE2.
002440	       05  WS-TCH  PIC	X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460	       05  WS-MCHR PIC	X(01) OCCURS 80.
	   03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480	       05  WS-BCHR PIC	X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
	       05  FILLER  PIC	X(01).
               05  WS-F-LINE
			   PIC	X(78).
	       05  FILLER  PIC	X(01).
002470     03  WS-BOT-LNE2.
002480	       05  WS-BCH  PIC	X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250	       05  FILLER  PIC	X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270			   PIC	X(50).
                  07  FILLER
			   PIC	X(10).
003280	       05  FILLER  PIC	X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

003440 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "PRF".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

       COPY CONTROL.ID.

       COPY DEBTOR.ID.

       COPY DEBMEM.ID.

       COPY DEBTOT.ID.

       COPY DEPART.ID.

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
000940	   03  COM-LINE1 REDEFINES W02-PRN-LINE.
000950	       05  COM-PAGE    PIC  X(06).
000960	       05  COM-P-NO    PIC  Z(03)9.
000970	       05  FILLER      PIC  X(04).
000980	       05  COM-COMPANY PIC  X(30).
000990	       05  COM-HEAD    PIC  X(18).
001000	       05  FILLER      PIC  X(02).
001010	       05  COM-DATE    PIC  X(06).
001020	       05  COM-DTE     PIC  Z9/99/9999.

001080	   03  SLD-LINE1 REDEFINES W02-PRN-LINE.
001090	       05  SLD-DET     PIC  X(132).

001560	   03  SLD-LINE6 REDEFINES W02-PRN-LINE.
001570	       05  SLD-PAGE    PIC  X(06).
001580	       05  SLD-P-NO    PIC  Z(03)9.
001590	       05  FILLER      PIC  X(24).
001600	       05  SLD-RESTART.
001610		   07  SLD-COMPANY  PIC  X(40).
001620		   07  SLD-HEAD     PIC  X(24).
001630	       05  FILLER      PIC  X(17).
001640	       05  SLD-DATE    PIC  X(06).
001650	       05  SLD-DTE     PIC  Z9/99/9999.

	   03  SLD-LINE7 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(06).
	       05  SLD-CO      PIC  X(30).
	       05  SLD-EXHEAD  PIC  X(24).
	       05  FILLER      PIC  X(08).

	   03  SLD-LINE8 REDEFINES W02-PRN-LINE.
	       05  SLD-H1      PIC  X(08).
	       05  SLD-H2      PIC  X(88).

	   03  SLD-LINE8B REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(24).
	       05  SLD-H3      PIC  X(19).
	       05  SLD-H4      PIC  X(13).
	       05  SLD-H5      PIC  X(17).
	       05  SLD-H6      PIC  X(17).
	       05  SLD-H7      PIC  X(06).

	   03  SLD-LINE9 REDEFINES W02-PRN-LINE.
	       05  SLD-AC      PIC  X(06).
	       05  FILLER      PIC  X(02).
	       05  SLD-NME     PIC  X(40).
	       05  FILLER      PIC  X(48).

	   03  SLD-LINE10 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(24).
	       05  SLD-MONTH.
		   07  SLD-MNTH	 PIC  X(10).
		   07  SLD-YEAR  PIC  X(04).
	       05  FILLER      PIC  X(02).
	       05  SLD-COST    PIC  Z(09)9.99-.
	       05  FILLER      PIC  X(02).
	       05  SLD-SELL    PIC  Z(09)9.99-.
	       05  FILLER      PIC  X(02).
	       05  SLD-PROF    PIC  Z(09)9.99-.
	       05  FILLER      PIC  X(02).
	       05  SLD-GP      PIC  Z(03)9.99-.

003650 01  W05-LIST.
003660	   03  W05-OPT	       PIC  9(03).
003670     03  W05-TYPES REDEFINES W05-OPT.
003680	       05  W05-COMP    PIC  9(01).
003690	       05  W05-INDV    PIC  9(01).
003700	       05  W05-INT     PIC  9(01).

       COPY W05.VAT.

001270 01  W08-EXPD.
	   03  W08-CHAR        PIC  X(06).
001280	   03  W08-DEC REDEFINES W08-CHAR
			       PIC  X(01) COMP-X OCCURS 6.

       COPY W10.STK.

       COPY W12.WS.

       COPY W15.WS.

       COPY W20.WS.

       01  W23-PERIOD.
           03  FILLER          PIC X(17) VALUE "For the period : ".
	   03  W23-DTE1        PIC Z9/99/9999.
           03  FILLER          PIC X(04) VALUE " to ".
	   03  W23-DTE2        PIC Z9/99/9999.

003930 01  W25-CALCS.
002860	   03  W25-RESULT      PIC  9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880	       05  W25-WHOLE   PIC  9(05).
002890	       05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
003980	   03  W25-TOTAL       PIC S9(09)V99 COMP-3.
003990	   03  W25-VALUE       PIC S9(09)V99 COMP-3.
	   03  W25-TRANVAL     PIC S9(09)V99 COMP-3.
003980	   03  W25-G-TOTAL     PIC S9(09)V99 COMP-3.
003990	   03  W25-G-VALUE     PIC S9(09)V99 COMP-3.

000550 01  W30-MONTHS.
	   03  W30-CURR        PIC  9(02).
	   03  W30-START       PIC  9(02).
	   03  W30-END	       PIC  9(02).
000560     03  W30-MONTH-NAMES.
000570	       05  W30-ENG.
000580		   07  FILLER  PIC  X(09) VALUE "January  ".
		   07  FILLER  PIC  X(09) VALUE "February ".
		   07  FILLER  PIC  X(09) VALUE "March	  ".
		   07  FILLER  PIC  X(09) VALUE "April    ".
		   07  FILLER  PIC  X(09) VALUE "May      ".
		   07  FILLER  PIC  X(09) VALUE "June     ".
		   07  FILLER  PIC  X(09) VALUE "July     ".
		   07  FILLER  PIC  X(09) VALUE "August   ".
		   07  FILLER  PIC  X(09) VALUE "September".
		   07  FILLER  PIC  X(09) VALUE "October  ".
		   07  FILLER  PIC  X(09) VALUE "November ".
		   07  FILLER  PIC  X(09) VALUE "December ".
000590     03  W30-MONTH REDEFINES W30-MONTH-NAMES
000600			       PIC  X(09)  OCCURS 12.

001700 01  W32-MONTHS.
001710	   03  W32-MONTH-NAMES.
001720	       05  W32-ENG     PIC X(36)     VALUE
001730                         "JanFebMarAprMayJunJulAugSepOctNovDec".
001740	   03  W32-MONTH REDEFINES W32-MONTH-NAMES
001750                         PIC X(03)     OCCURS 12.

       01  W35-EDIT.
	   03  W35-COST        PIC  Z(08)9.99- BLANK WHEN ZERO.
	   03  W35-SELL        PIC  Z(08)9.99- BLANK WHEN ZERO.
	   03  W35-PROF        PIC  Z(08)9.99- BLANK WHEN ZERO.
	   03  W35-GP	       PIC  Z(02)9.99- BLANK WHEN ZERO.

       COPY W40.DBT.

       COPY W40.WS.

005020 01  W45-TRAN.
005030	   03  W45-CODE        PIC  9(02).
005040	   03  W45-ENG	       PIC  X(12).
005050	   03  W45-AFR	       PIC  X(12).
005060	   03  W45-ACT	       PIC  X(01).

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
005210	       05  W60-NCHAR OCCURS 40 PIC  X(01).
005220     03  W60-ADDRESS.
005230	       05  W60-ACHAR OCCURS 60 PIC  X(01).

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
	   03  W75-DNO	      PIC  X(06) OCCURS 18.
	   03  W75-ITEMS OCCURS 18.
	       05  W75-CODE   PIC  X(14).
	       05  W75-ITM    PIC  X(03).
	       05  W75-SELL   PIC  9(07)V99 COMP-3.
	       05  W75-VSELL  PIC  9(07)V99 COMP-3.
	       05  W75-CASH   PIC  9(07)V99 COMP-3.
	       05  W75-VCASH  PIC  9(07)V99 COMP-3.
	       05  W75-WSALE  PIC  9(07)V99 COMP-3.
	       05  W75-VWSALE PIC  9(07)V99 COMP-3.

005490 01  W80-ACCOUNT.
005500	   03  W80-ACNO        PIC  X(06).
005510     03  W80-NUMD REDEFINES W80-ACNO.
005530	       05  W80-SERIAL  PIC  9(06).
005520	   03  W80-TYPE        PIC  9(01).

005810 01  W95-STM.
005820	   03  W95-COMP        PIC  X(40).
005830	   03  W95-ADD1        PIC  X(30).
005840	   03  W95-ADD2        PIC  X(30).
005850	   03  W95-ADD3        PIC  X(30).
005860	   03  W95-ADD4        PIC  X(30).
005870     03  W95-EDIT        REDEFINES W95-ADD4.
005880         05  W95-V1.
005890		   07  W95-T   PIC  Z(04)9.99.
005900         05  W95-V2.
005910		   07  W95-B   PIC  Z(06)9.99-.
005920	       05  FILLER      PIC  X(11).
005930	   03  W95-POST        PIC  9(04).
005940	   03  W95-TEL	       PIC  X(14).

005950 01  W100-HEAD.
005960	   03  FILLER	       PIC  X(29)    VALUE
005970			     "ACCOUNTS RECEIVABLE LISTING -".
005980	   03  W100-DESC       PIC  X(11).

004490 01  W100-EDIT.
004500	   03  W100-LBAL       PIC  Z(06)9.99-.
004500	   03  W100-LARR       PIC  Z(06)9.99-.
004500	   03  W100-BAL        PIC  Z(08)9.99-.
004510	   03  W100-CUR        PIC  Z(08)9.99-.
004520	   03  W100-30	       PIC  Z(08)9.99-.
004530	   03  W100-60	       PIC  Z(08)9.99-.
004540	   03  W100-90	       PIC  Z(08)9.99-.
004540	   03  W100-120        PIC  Z(08)9.99-.
004550	   03  W100-INT        PIC  Z(08)9.99.
004560	   03  W100-CRL        PIC  Z(06)9.
004570	   03  W100-KEY        PIC  Z(05)9.
003970	   03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

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

000850 01  S-PROG.
000860	   03  LINE  2 COLUMN 22 FOREGROUND-COLOR 7
				 HIGHLIGHT VALUE
009360		       "DEBTORS - PROFIT FOR SELECTED PERIOD".

       01  S04.
	   03  LINE  6 COLUMN 12 VALUE "Accounts Required:".
	   03  LINE  8 COLUMN 12 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 13 VALUE "ll or ".
	   03	       COLUMN 19 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "S".
	   03	       COLUMN 20 VALUE "elective accounts - press ".
	   03	       COLUMN 46 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Esc".
	   03	       COLUMN 49 VALUE "ape to exit".

009340 01  S05D.
	   03  LINE  4 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³     Purchases for Period.    ³".
	   03  LINE  6 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  7 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³ Starting Month:              ³".
	   03  LINE  8 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³ Ending Month  :              ³".
	   03  LINE  9 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
	   03  LINE  7 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99 USING W30-START AUTO.
	   03  LINE  8 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99 USING W30-END AUTO.

009340 01  S10D.
	   03  LINE  4 COLUMN  3 VALUE "Account No.".

009930 01  S10.
009940	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

       COPY DEBLUP.CRT.

       COPY ERROR.CRT.

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

       01  S-PROF.
	   03  LINE  4 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 7 VALUE
	       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄ
      -	       "ÄÂÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 7 VALUE
	       "³    Month     ³  Total Cost ³Total Selling³   Total GP
      -	       " ³  GP % ³".
	   03  LINE  6 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 7 VALUE
	       "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ
      -	       "ÄÅÄÄÄÄÄÄÄ´".
	   03  LINE 19 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 7 VALUE
	       "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ
      -	       "ÄÅÄÄÄÄÄÄÄ´".
	   03  LINE 20 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 7 VALUE
	       "³    TOTAL     ³             ³             ³
      -	       " ³       ³".
	   03  LINE 21 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 7 VALUE
	       "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄ
      -	       "ÄÁÄÄÄÄÄÄÄÙ".

       COPY S99.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
      *    ************************************************************
      *    ****	   Debtor security level must be greater than 3	   ****
      *    ************************************************************
003240	   IF LS0-DBLEV < 4
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	   IF NOT (WS-CARDEX = "Y")
003250	       MOVE "Sales logging not selected"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
	     PERFORM BA000.
	     CLOSE CARDEX
		   DEBTOT.
      *    ************************************************************
      *    **  M U S T	 T H E	 P R I N T E R	 B E   C L O S E D ? **
      *    ************************************************************
	   IF W02-PRN-STATUS = "O"
	       MOVE "C"		 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL.

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

       COPY DATE.CHK.

       COPY DATE.CNV.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP5.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
007340 AC000-READ	       SECTION.

       COPY CARDEX.RD.

       COPY CONTROL.RD.

       COPY PARAM.RD.
      
       COPY DEBTOR.RD1.

011710 COPY DEBTOT.RD.

       COPY DEBMEM.RD.

       COPY DEPART.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
010170 AD000		 SECTION.

       COPY DEBTOT.WR.

011630 COPY DEBTOR.ILU.

       COPY APAC.HLP.

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
009160				 INTO W40-SURNAME
				      W40-FNAME
				      W40-SNAME
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
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  P R O F I T   F O R   S P E C I F I E D	A C C O U N T  ³
      *    ³  F O R   T H E   S E L E C T E D	P E R I O D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
021000 BA000		       SECTION 58.
       BA0.
	   IF WS-EXTR = "Y"
	       GO TO BA15.

021010 BA00.
021020	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.

       BA05.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY S05D.
	     PERFORM SCREEN-SHADOW.
	     MOVE W12-MONTH	 TO W30-END W30-CURR.
	   IF W12-MONTH = 12
	       MOVE 1		 TO W30-START
	   ELSE
	       ADD 1 W12-MONTH	 GIVING W30-START.

       BA10.
	     DISPLAY "Press " AT 2552
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Exit"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     ACCEPT S05A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA499
		 WHEN OTHER	 CALL X"E5"
		     GO TO BA10
	       END-EVALUATE.
	   IF (W30-START < 01 OR > 12) OR
	      (W30-END	< 01 OR > 12)
	       MOVE "Months must be 01 to 12"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019000	       GO TO BA10.
	     DISPLAY W30-MONTH(W30-START) AT 0747
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W30-MONTH(W30-END) AT 0847
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA10.
	     DISPLAY CLEAR-L25.
	     MOVE W12-TODAY	 TO W15-START W15-END.
	     MOVE W30-START	 TO W15-SMNTH.
	     MOVE W30-END	 TO W15-EMNTH.
	     MOVE 01		 TO W15-SDAY.
	     MOVE 31		 TO W15-EDAY.
      *    ************************************************************
      *    ****   I S	S T A R T   M O N T H	-   PREVIOUS YEAR  ****
      *    ************************************************************
	   IF W30-START > W30-CURR
	       SUBTRACT 1	 FROM W15-SCCYY
	       IF W30-END > W30-CURR
		   SUBTRACT 1	 FROM W15-ECCYY
	       END-IF
	   ELSE
	   IF W30-END < W30-START
	       SUBTRACT 1	 FROM W15-SCCYY.
	   IF W15-EMNTH = 04 OR 06 OR 09 OR 11
	       MOVE 30		 TO W15-EDAY
	   ELSE
	   IF W15-EMNTH = 02
      *    ************************************************************
      *    ****    C H E C K   F O R   L E A P	 Y E A R
      *    ************************************************************
008610	       DIVIDE W15-ECCYY BY 4 GIVING W25-RESULT
008620	       IF W25-DECIMAL = 0
		   MOVE 29	 TO W15-EDAY
	       ELSE
		   MOVE 28	 TO W15-EDAY.
      *    ************************************************************
      *    ****    SWITCH START DATE TO YEAR/MONTH/DAY
      *    ************************************************************
	     MOVE W15-SDAY	 TO W15-SDD.
	     MOVE W15-SMNTH	 TO W15-SMM.
	     MOVE W15-SCENT	 TO W15-SCC.
	     MOVE W15-SYEAR	 TO W15-SYY.
      *    ************************************************************
      *    ****    SWITCH END DATE TO YEAR/MONTH/DAY
      *    ************************************************************
	     MOVE W15-EDAY	 TO W15-EDD.
	     MOVE W15-EMNTH	 TO W15-EMM.
	     MOVE W15-ECENT	 TO W15-ECC.
	     MOVE W15-EYEAR	 TO W15-EYY.

       BA15.
	     PERFORM BA00.
	     DISPLAY S04.

       BA20.
011560	     ACCEPT WS-OPTION AT 0861
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BA499
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA20
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "A" OR "S")
	       GO TO BA20.
	     MOVE WS-OPTION	 TO WS-ACT.
	     MOVE ZERO		 TO WS-PAGE
				    W25-VALUE
				    W25-TOTAL.
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
      *    ************************************************************
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L   F I L E
      *    ************************************************************
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       DISPLAY "Debtor account:" AT 1412
	       PERFORM CALL-PRINTUTL.
	     PERFORM RESTORE-SCREEN-3.
	   IF WS-ACT = "A"
	       GO TO BA90.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  P R O F I T   F O R   S P E C I F I E D	A C C O U N T  ³
      *    ³  F O R   T H E   S E L E C T E D	P E R I O D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA25.
021020	     PERFORM RESTORE-SCREEN-3.
	     DISPLAY S10D.

       BA30.
018790	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018800		     " to do Account Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BA35.
018810	     ACCEPT S10 AT 0415.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO BA40
016550		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA30
	       END-EVALUATE
018840	       DISPLAY S10 AT 0415
018850         IF W80-ACNO = SPACES
018860		   GO TO BA30.

       BA40.
018870	     DISPLAY CLEAR-L25.
018880	   IF W80-ACNO = SPACES
018900	       GO TO BA499.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

018910 BA45.
018920	     DISPLAY CLEAR-L25.
018930       PERFORM CA155-GET-DEBTOR.
018940     IF WS-ERROR = 1
018950	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
018960	       GO TO BA30.
018970     IF WS-ERROR = 2
018980	       MOVE "Invalid account"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019000	       GO TO BA30.
      *    ************************************************************
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *    ************************************************************
019020	     MOVE DEB-NAME	 TO W40-NAME.
019030     IF W80-TYPE = 1
019040         INSPECT W40-NAME REPLACING ALL "*" BY " "
019050         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
019060	     DISPLAY W40-NAME AT 0315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       DISPLAY CLEAR-L25
	       GO TO BA30.

       BA50.
	     MOVE W80-ACNO	 TO CRD-AC.
	     MOVE W15-S-YMD	 TO CRD-DTE.
	     PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       MOVE "No details for period requested"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA499.
	     CLOSE DEBTOT.
	     OPEN OUTPUT DEBTOT.
	     CLOSE DEBTOT.
	     OPEN I-O DEBTOT.
	     DISPLAY "Extracting Data:" AT 1112.
	     MOVE SPACES	 TO TOT-AC   TOT-MTH.
	     MOVE ZERO		 TO TOT-YEAR TOT-TCOST
				    TOT-MNTH TOT-TSELL
				    TOT-EMTH.

       BA55.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO BA60.
	   IF NOT (CRD-AC = W80-ACNO)
	       GO TO BA60.
	   IF CRD-DTE < W15-S-YMD
	       GO TO BA55.
	   IF CRD-DTE > W15-E-YMD
	       GO TO BA60.
	     DISPLAY CRD-AC AT 1129
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     CRD-DTE AT 1136
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE CRD-AC	 TO TOT-AC.
	     MOVE CRD-DTE	 TO W20-DTE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   Move 4 digit year CCYY to totals file record        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE W20-CNYR	 TO TOT-YEAR.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	     Move 2 digit month to totals file record	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE W20-MONTH	 TO TOT-MNTH.
	     PERFORM READ-DEBTOT THRU READ-DEBTOT-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check if there has been a record created for this account ³
      *    ³ for the selected year and month.			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WS-F-ERROR = "55"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		 C R E A T E   N E W   R E C O R D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE W30-MONTH(TOT-MNTH)
				 TO TOT-MTH
	       MOVE W15-EMM	 TO TOT-EMTH
	       IF (CRD-QNT < ZERO)
		   COMPUTE TOT-TCOST = (CRD-COST * CRD-QNT) * -1
		   COMPUTE TOT-TSELL = (CRD-SELL * CRD-QNT) * -1
	       ELSE
		   MULTIPLY CRD-COST BY CRD-QNT
				     GIVING TOT-TCOST
		   MULTIPLY CRD-SELL BY CRD-QNT
				     GIVING TOT-TSELL
	       END-IF
	       PERFORM WRITE-DEBTOT THRU WRITE-DEBTOT-EXIT
	   ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    U P D A T E   E X I S T I N G   R E C O R D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       IF (CRD-QNT < ZERO)
		   COMPUTE TOT-TCOST = TOT-TCOST +
				     ((CRD-COST * CRD-QNT) * -1)
		   COMPUTE TOT-TSELL = TOT-TSELL +
				     ((CRD-SELL * CRD-QNT) * -1)
	       ELSE
		   COMPUTE TOT-TCOST = TOT-TCOST + (CRD-COST * CRD-QNT)
		   COMPUTE TOT-TSELL = TOT-TSELL + (CRD-SELL * CRD-QNT)
	       END-IF
	       PERFORM REWRITE-DEBTOT THRU WRITE-DEBTOT-EXIT.
	     GO TO BA55.

       BA60.
	     EXIT.

       BA62.
	     DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
	     CLOSE DEBTOT.
	     OPEN I-O DEBTOT.
	     DISPLAY S-PROF.
	     MOVE 0707		 TO CPOS.
	     PERFORM BA70-CRT-EDGE UNTIL CLIN = 19.
	     MOVE W80-ACNO	 TO TOT-AC.
	     MOVE ZERO		 TO TOT-YEAR  TOT-MNTH
				    W25-VALUE W25-TOTAL.
	     PERFORM START-AT-DEBTOT-AC THRU READ-DEBTOT-EXIT.
000880	   IF WS-F-ERROR = 55
	       GO TO BA75.
	     MOVE 0700		 TO CPOS.

       BA65.
	     PERFORM READ-DEBTOT-NEXT THRU READ-DEBTOT-EXIT.
000880	   IF WS-F-ERROR = 55
	       GO TO BA75.
	     MOVE TOT-TCOST	 TO W35-COST.
	     MOVE TOT-TSELL	 TO W35-SELL.
	     SUBTRACT TOT-TCOST FROM TOT-TSELL
				 GIVING W10-VSELL.
	     COMPUTE W10-ADJUST ROUNDED = (W10-VSELL / TOT-TSELL) *
					   100.000.
	     MOVE W10-VSELL	 TO W35-PROF.
	     MOVE W10-ADJUST	 TO W35-GP.
	     ADD TOT-TCOST	 TO W25-VALUE.
	     ADD TOT-TSELL	 TO W25-TOTAL.
	     MOVE 08		 TO CCOL.
	     DISPLAY TOT-MTH AT CPOS
		     WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
	     MOVE 18		 TO CCOL.
	     DISPLAY TOT-YEAR AT CPOS
		     WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
	     MOVE 23		 TO CCOL.
	     DISPLAY W35-COST AT CPOS
		     WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
	     MOVE 37		 TO CCOL.
	     DISPLAY W35-SELL AT CPOS
		     WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
	     MOVE 51		 TO CCOL.
	     DISPLAY W35-PROF AT CPOS
		     WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
	     MOVE 65		 TO CCOL.
	     DISPLAY W35-GP AT CPOS
		     WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
	     ADD 1		 TO CLIN.
	     GO TO BA65.

       BA70-CRT-EDGE.
	     DISPLAY "³              ³             ³             ³
      -		     "       ³       ³" AT CPOS
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
002090	     ADD 1		 TO CLIN.

       BA75.
	     MOVE 2023		 TO CPOS.
	     MOVE W25-VALUE	 TO W35-COST.
	     MOVE W25-TOTAL	 TO W35-SELL.
	     SUBTRACT W25-VALUE FROM W25-TOTAL
				 GIVING W10-VSELL.
	     COMPUTE W10-ADJUST ROUNDED = (W10-VSELL / W25-TOTAL) *
					   100.000.
	     MOVE W10-VSELL	 TO W35-PROF.
	     MOVE W10-ADJUST	 TO W35-GP.
	     DISPLAY W35-COST AT CPOS
		     WITH BACKGROUND-COLOR 7
			  FOREGROUND-COLOR 5.
	     MOVE 37		 TO CCOL.
	     DISPLAY W35-SELL AT CPOS
		     WITH BACKGROUND-COLOR 7
			  FOREGROUND-COLOR 5.
	     MOVE 51		 TO CCOL.
	     DISPLAY W35-PROF AT CPOS
		     WITH BACKGROUND-COLOR 7
			  FOREGROUND-COLOR 5.
	     MOVE 65		 TO CCOL.
	     DISPLAY W35-GP AT CPOS
		     WITH BACKGROUND-COLOR 7
			  FOREGROUND-COLOR 5.

       BA80.
	     DISPLAY "Press any key to continue" AT 2322
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA85.
032920	     MOVE "Change 'A'ccount,'P'eriod or 'E'xit"
				 TO WS-ERR-MES.
	     MOVE "A"		 TO WS-OPTION.
033910	     PERFORM OPT-MESSAGE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "A" OR "E" OR "P")
	       GO TO BA85.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA499
	       END-EVALUATE.
	   IF WS-OPTION = "A"
	       GO TO BA15.
	   IF WS-OPTION = "P"
	       GO TO BA00.
	     GO TO BA499.

       BA90.
	     MOVE " !"		 TO DEB-NAME.
             PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
	   IF WS-F-ERROR = 6
	       MOVE "No DEBTORS on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
021070	       GO TO BA499.
	     DISPLAY "Extracting Data:" AT 1112.

       BA95.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BA499.
021090	     PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
010780     IF WS-F-ERROR = 6
	       GO TO BA100.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY DEB-ACNO AT 1428
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	     MOVE DEB-ACNO	 TO W80-ACNO
				    CRD-AC.
	     MOVE W15-S-YMD	 TO CRD-DTE.
	     PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       GO TO BA95.
	     MOVE SPACES	 TO TOT-AC   TOT-MTH.
	     MOVE ZERO		 TO TOT-YEAR TOT-TCOST
				    TOT-MNTH TOT-TSELL
				    TOT-EMTH.
	     PERFORM BA55 THRU BA60.
	     GO TO BA95.
      *    ************************************************************
      *    ****    E X T R A C T   C O M P L E T E
      *    ************************************************************
       BA100.
	     CLOSE DEBTOT.
	     OPEN I-O DEBTOT.
	     MOVE "     !"	 TO TOT-AC.
	     MOVE ZERO		 TO TOT-YEAR
				    TOT-MNTH
				    WS-PAGE
				    W25-G-TOTAL
				    W25-G-VALUE.
	     MOVE SPACES	 TO W80-ACNO.
	     PERFORM START-AT-DEBTOT-AC THRU READ-DEBTOT-EXIT.
000880	   IF WS-F-ERROR = 55
	       MOVE "No data extracted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA499.

       BA105.
	     MOVE "H"		 TO W02-PRN-TYPE.
016620       MOVE SPACES         TO REP-DETAIL1.
016630	     MOVE "PAGE:"	 TO SLD-PAGE.
016640	     MOVE "DATE:"	 TO SLD-DATE.
016650	     ADD 1		 TO WS-PAGE.
016660	     MOVE W12-TODAY	 TO SLD-DTE.
016670	     MOVE WS-PAGE	 TO SLD-P-NO.
      *    ************************************************************
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L   F I L E
      *    ************************************************************
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO SLD-LINE1.
      *    ************************************************************
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L   F I L E
      *    ************************************************************
	   IF NOT (WS-PRINTER = "S")
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO SLD-LINE1.
	     MOVE SPACES	 TO SLD-LINE1.
016680	     MOVE W95-COMP	 TO SLD-CO.
016690	     MOVE " - G.PROF. PER ACCOUNT"
				 TO SLD-EXHEAD.
	     MOVE "H"		 TO W02-PRN-TYPE.
      *    ************************************************************
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L   F I L E
      *    ************************************************************
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO SLD-LINE1.
      *    ************************************************************
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L   F I L E
      *    ************************************************************
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "X"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012760	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
016620	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "Ac.No."	 TO SLD-H1.
	     MOVE "Account Name" TO SLD-H2.
	     PERFORM CALL-PRINTUTL.
016620	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "Month     Year"
				 TO SLD-H3.
	     MOVE "Total Cost"	 TO SLD-H4.
	     MOVE "Total Selling"
				 TO SLD-H5.
	     MOVE "Gross Profit" TO SLD-H6.
	     MOVE "G.P.%"	 TO SLD-H7.
	     PERFORM CALL-PRINTUTL.
	     MOVE ALL "="	 TO SLD-LINE8.
	     PERFORM CALL-PRINTUTL.
016620	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

       BA110.
	     PERFORM READ-DEBTOT-NEXT THRU READ-DEBTOT-EXIT.
000880	   IF WS-F-ERROR = 55
	       PERFORM BA501
	       GO TO BA115.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BA499.
	   IF NOT (TOT-AC = W80-ACNO)
	       PERFORM BA500.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       GO TO BA999.
	     ADD TOT-TCOST	 TO W25-VALUE.
	     ADD TOT-TSELL	 TO W25-TOTAL.
	     MOVE TOT-MTH	 TO SLD-MNTH.
	     MOVE TOT-YEAR	 TO SLD-YEAR.
	     MOVE TOT-TCOST	 TO SLD-COST.
	     MOVE TOT-TSELL	 TO SLD-SELL.
	     SUBTRACT TOT-TCOST FROM TOT-TSELL
				 GIVING W25-TRANVAL.
	     MOVE W25-TRANVAL	 TO SLD-PROF.
	     COMPUTE SLD-GP ROUNDED = (W25-TRANVAL / TOT-TSELL) *
					   100.000.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM BA105.
	     GO TO BA110.
      *    ************************************************************
      *    ****   P R I N T   G R A N D   T O T A L S
      *    ************************************************************
       BA115.
	     MOVE "GRAND TOTAL"  TO SLD-MONTH.
	     MOVE W25-G-VALUE	 TO SLD-COST.
	     MOVE W25-G-TOTAL	 TO SLD-SELL.
	     SUBTRACT W25-G-VALUE FROM W25-G-TOTAL
				 GIVING W25-TRANVAL.
	     MOVE W25-TRANVAL	 TO SLD-PROF.
	     COMPUTE SLD-GP ROUNDED = (W25-TRANVAL / W25-G-TOTAL) *
					   100.000.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       BA499.
	     EXIT.
      *    ************************************************************
      *    ****   I S	T H I S   T H E   F I R S T   A C C O U N T
      *    ************************************************************
       BA500	       SECTION 58.
       BA501.
	   IF W80-ACNO = SPACES
	       GO TO BA505.
	     ADD W25-VALUE	 TO W25-G-VALUE.
	     ADD W25-TOTAL	 TO W25-G-TOTAL.
      *    ************************************************************
      *    ****   P R I N T   A C C O U N T   T O T A L S
      *    ************************************************************
	     MOVE "Account Total"
				 TO SLD-MONTH.
	     MOVE W25-VALUE	 TO SLD-COST.
	     MOVE W25-TOTAL	 TO SLD-SELL.
	     SUBTRACT W25-VALUE FROM W25-TOTAL
				 GIVING W25-TRANVAL.
	     MOVE W25-TRANVAL	 TO SLD-PROF.
	     COMPUTE SLD-GP ROUNDED = (W25-TRANVAL / W25-TOTAL) *
					   100.000.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 3)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM BA105.

       BA505.
016620	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE ZERO		 TO W25-VALUE W25-TOTAL.
	     MOVE TOT-AC	 TO W80-ACNO SLD-AC.
	     PERFORM CA155-GET-DEBTOR.
	   IF NOT (WS-ERROR = 0)
	       MOVE "INVALID ACCOUNT NUMBER"
				 TO SLD-NME
	   ELSE
	       MOVE DEB-NAME	 TO SLD-NME
	       IF DEB-TYPE = 1
015630		   INSPECT SLD-NME REPLACING FIRST "*" BY ","
015640		   INSPECT SLD-NME REPLACING FIRST "*" BY " "
015650		   INSPECT SLD-NME REPLACING FIRST "!" BY " ".
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
016620	     MOVE SPACES	 TO REP-DETAIL1.

       BA999.
	     EXIT.

      /
025020 ZA00-INIT	      SECTION 90.
025030 ZA00-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W12-TODAY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ D o   n o t   D I S P L A Y   E R R O R   M E S S A G E S ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ****       S A L E S	  -   C A R D E X   F I L E	    ****
      *    *************************************************************
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
      *    *************************************************************
      *    **** 	  D E B T O R	M E M O	  F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
      *    *************************************************************
      *    ***	D E B T O R   T O T A L S   E X T R A C T - F I L E  ***
      *    *************************************************************
	     MOVE "DEBTOT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-DEBTOT.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
025110	     MOVE 3		 TO WS-PARKEY.
025120       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-CRDX	 TO WS-CARDEX.
053830	     OPEN I-O CARDEX.
	   IF WS-STATUS = "05"
	       CLOSE CARDEX
	       OPEN OUTPUT CARDEX
	       CLOSE CARDEX
	       OPEN I-O CARDEX.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 43	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 43		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
053920		   PERFORM OPEN-ERROR.
053830	     OPEN I-O DEBTOT.
	   IF WS-STATUS = "00"
	       GO TO ZA30.
	   IF WS-STATUS = "05"
	       CLOSE DEBTOT
	       OPEN OUTPUT DEBTOT
	       CLOSE DEBTOT
	       OPEN I-O DEBTOT.

       ZA01.
053840	   IF RUNTIME-ERROR
053850	       IF FLE-LOCKED
053860		   GO TO ZA200
053870	       ELSE
053880	       IF FLE-LIMIT
053890		   GO TO ZA49
	       ELSE
	       IF IDX-CORRUPT
		   CLOSE DEBTOT
		   OPEN OUTPUT DEBTOT
		   CLOSE DEBTOT
		   OPEN I-O DEBTOT.
053900	   IF WS-STATUS NOT = "00"
053910	       MOVE 55		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   CLOSE DEBTOT
		   OPEN OUTPUT DEBTOT
		   CLOSE DEBTOT
		   OPEN I-O DEBTOT
	       ELSE
053920		   PERFORM OPEN-ERROR.

       ZA02.
054710	     MOVE 5		 TO WS-PARKEY.
054720       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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

025620 ZA25.
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

       ZA30.
	     MOVE "Use the existing extracted data Y/N? [ ]"
				 TO WS-ERR-MES.
033910	     PERFORM OPT-MESSAGE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO ZA999-EXIT
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO ZA30.
	     MOVE WS-OPTION	 TO WS-EXTR.
	   IF WS-OPTION = "Y"
	       GO TO ZA02.
038770	     CLOSE DEBTOT.
	     OPEN OUTPUT DEBTOT.
	     CLOSE DEBTOT.
	     OPEN I-O DEBTOT.
	     GO TO ZA01.
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

039850 ZA50.
041850	   IF WS-F-ERROR = 43
041860	       MOVE "CARDEX file"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

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
041840	   ELSE
041810	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX.
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
