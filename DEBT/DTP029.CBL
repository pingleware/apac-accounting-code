      $SET LINKCOUNT"576"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******    ******    *
      *   **	**     **     **    **	 **  **   **	**  **	  **   *
      *   **	**     **     **    **	**    **       **   **	  **   *
      *   **	**     **     *******	**    **     **      *******   *
      *   **	**     **     **	**    **   **		  **   *
      *   **	**     **     **	 **  **   **	    **	  **   *
      *   *******      **     **	  ****	  ********   ******    *
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
      *  Jan 2008	- New file (DEBALT) - Debtor alternate index   *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Debtor name.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP029.
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

000140 COPY DEBALT.SL.

000140 COPY DEBTOR.ISL.

       COPY DEBMEM.SL.

000140 COPY DEPART.SL.

000120 COPY PARAM.SL.

000140 COPY STOCK.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FDE.

       COPY CONTROL.FDE.

000310 COPY DEBALT.FDE.

000270 COPY DEBTOR.FDE.

       COPY DEBMEM.FDE.

000140 COPY DEPART.FDE.

000250 COPY PARAM.FDE.

000540 COPY STOCK.FDE.

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
002910 77  WS-DEBKEY	   PIC	9(06)	  COMP-5.
002930 77  WS-NETKEY	   PIC	9(06)	  COMP-5.
002940 77  WS-WRKEY	   PIC	9(04)	  COMP-5.
       77  WS-ETYPE	    PIC X(01).
002780 77  WS-LOOK         PIC  X(01) VALUE "S".
       77  WS-CARDEX	   PIC	X(01).
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
002960 77  WS-NTP	   PIC	9(01).
002970 77  WS-LINES	   PIC	9(02).
002980 77  WS-PAGE	   PIC	9(04)	  COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002990 77  WS-OPTION	   PIC	X(01).
002990 77  WS-ACT	   PIC	X(01).
002990 77  WS-STK	   PIC	X(01).
002990 77  WS-DEB	   PIC	X(01).
003000 77  WS-CREDIT	   PIC	X(01).
002990 77  WS-SKIP	   PIC	X(01).
       77  WS-NUM	   PIC	Z9.
003020 77  WS-ADDR	   PIC	X(01).
003030 77  WS-ERROR	   PIC	9(01).
003040 77  WS-ERROR1	   PIC	9(01).
003110 77  WS-EXPAND	   PIC	X(01)	 VALUE " ".
003150 77  WS-CHAR1	   PIC	X(01).
003160 77  WS-CHAR2	   PIC	X(01).
003180 77  WS-COMP	   PIC	9(01).
003180 77  WS-STM	   PIC	9(01).
003190 77  WS-TYP	   PIC	X(01).
003190 77  WS-TYPE	   PIC	X(01).
003200 77  WS-HEAD	   PIC	X(01).
003230 77  WS-BATCH	   PIC	9(05).
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
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
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
	       05  FILLER      PIC  X(01) VALUE "�".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "�".
	       05  FILLER      PIC  X(01) VALUE "�".
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
	   03  WS-PROG	       PIC  X(03) VALUE "029".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

       COPY CONTROL.ID.

       COPY DEBALT.ID.

       COPY DEBTOR.ID.

       COPY DEBMEM.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000140 COPY STOCK.ID.

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
	       05  SLD-H2      PIC  X(18).
	       05  SLD-H3      PIC  X(08).
	       05  SLD-H4      PIC  X(03).
	       05  SLD-H5      PIC  X(10).
	       05  SLD-H6      PIC  X(16).
	       05  SLD-H7      PIC  X(34).
	       05  SLD-H8      PIC  X(11).
	       05  SLD-H9      PIC  X(15).
	       05  SLD-H10     PIC  X(09).

	   03  SLD-LINE9 REDEFINES W02-PRN-LINE.
	       05  SLD-AC      PIC  X(06).
	       05  FILLER      PIC  X(02).
	       05  SLD-NME     PIC  X(40).
	       05  FILLER      PIC  X(84).

	   03  SLD-LINE10 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(23).
	       05  SLD-DATE1   PIC  Z9/99/9999.
	       05  FILLER      PIC  X(01).
	       05  SLD-DOC     PIC  X(01).
	       05  FILLER      PIC  X(02).
	       05  SLD-REF     PIC  X(08).
	       05  FILLER      PIC  X(02).
	       05  SLD-ITEM    PIC  X(14).
	       05  FILLER      PIC  X(02).
	       05  SLD-DESC    PIC  X(30).
	       05  FILLER      PIC  X(01).
	       05  SLD-QTY     PIC  Z(08)9.99-.
	       05  FILLER      PIC  X(01).
	       05  SLD-SELL    PIC  Z(06)9.99-.
	       05  FILLER      PIC  X(01).
	       05  SLD-TOTAL   PIC  Z(08)9.99-.

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
	   03  W25-QNT	       PIC S9(09)V99 COMP-3.
	   03  W25-TOTQNT      PIC S9(09)V99 COMP-3.

       COPY W40.DBT.

       COPY W40.WS.

005020 01  W45-TRAN.
005030	   03  W45-CODE        PIC  9(02).
005040	   03  W45-ENG	       PIC  X(12).
005050	   03  W45-AFR	       PIC  X(12).
005060	   03  W45-ACT	       PIC  X(01).
      *
      *    STORAGE FOR PRINTING OF LABELS
      *
005070 01  W46-LABELS.
005080     03  W46-LABEL OCCURS 2.
005090	       05  W46-NAME    PIC  X(40).
005100	       05  W46-ADD1    PIC  X(30).
005110	       05  W46-ADD2    PIC  X(30).
005120	       05  W46-ADD3    PIC  X(30).
005130	       05  W46-ADD4    PIC  X(30).
005140         05  W46-POST.
005150		   07  W46-PC  PIC  9(04).

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
005960	   03  FILLER	       PIC  X(26)    VALUE
005970                       "DEBTOR ACCOUNTS LISTING -".
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

       01  S04.
	   03  LINE  6 COLUMN 12 VALUE "Debtors Required:".
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
	   03  LINE  7 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴커".
	   03  LINE  8 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "�   Transactions for Period.   �".
	   03  LINE  9 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑".
	   03  LINE 10 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "�  Starting Date :             �".
	   03  LINE 11 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "�  Ending Date   :             �".
	   03  LINE 12 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑".
	   03  LINE 13 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "쿏ates to be keyed in: ".
	   03	       COLUMN 48 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "DDMMCCYY".
	   03	       COLUMN 56 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "�".
	   03  LINE 14 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸".
       01  S05A.
	   03  LINE 10 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE 11 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.

009340 01  S10D.
	   03  LINE 10 COLUMN 12 VALUE "Account No.".

009930 01  S10.
009940	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

       01  S11.
	   03  LINE 12 COLUMN 12 VALUE "Details Required:".
	   03  LINE 14 COLUMN 12 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 13 VALUE "ll or ".
	   03	       COLUMN 19 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "S".
	   03	       COLUMN 20 VALUE "elective Items - press ".
	   03	       COLUMN 43 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Esc".
	   03	       COLUMN 46 VALUE "ape to exit".

009340 01  S11D.
	   03  LINE 16 COLUMN 12 VALUE "Item Code".

       01  S19.
006650	   03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
006660				 VALUE "PRINTING SALES STATISTICS".
006260	   03  LINE  4 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")".

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(14) USING W10-ITEM AUTO.

005530 01  S36-3.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  S36-4.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S36-5.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S36-6.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S36-7.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S36-8.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S36-9.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S36-10.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S36-11.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S36-12.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S36-13.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S36-14.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(14) USING W10-I14 AUTO.

       COPY DEBLUP.CRT.

       COPY STKLUP.CRT.

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

       COPY S99.CRT.

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
	   IF NOT (WS-CARDEX = "Y")
003250	       MOVE "Sales logging not selected"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
	     PERFORM FA000.
	     CLOSE CARDEX
		   STOCK.
      *
      *    ****    M U S T   T H E   P R I N T E R   B E   C L O S E D	?
      *
	   IF W02-PRN-STATUS = "O"
	       MOVE "C"		 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL.

       AA49.
006950	     EXIT PROGRAM.

       COPY AA50DS.LUP.

       COPY DATE.CHK.

       COPY DATE.CNV.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP5.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
010990 AA300              SECTION.
011000 AA301.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     " Item/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011040		     " Alt/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     " Desc/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011080		     " X/Ref Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011090 AA305.
011100       EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
007340 AC000-READ              SECTION.

       COPY CARDEX.RD.

       COPY CONTROL.RD.

       COPY DEBALT.RD.

       COPY DEBTOR.RD1.

       COPY DEBMEM.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.
      
       COPY STOCK.RD.

011630 COPY DEBTOR.ILU.

       COPY DEBTOR.ALU.

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
      /
013580 FA000		       SECTION 5.
013590 FA00-INIT.
013600       DISPLAY CLR-SCREEN.
             DISPLAY S19.

011530 FA05-ACCEPT.
011560	     ACCEPT WS-OPTION AT 0431
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO FA999-EXIT
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA05-ACCEPT
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO FA05-ACCEPT.
011630	   IF WS-OPTION = "N"
011640         GO TO FA999-EXIT.
	     DISPLAY S04.

011530 FA10-ACCEPT.
011560	     ACCEPT WS-OPTION AT 0861
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO FA999-EXIT
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA10-ACCEPT
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "A" OR "S")
	       GO TO FA10-ACCEPT.
	     MOVE WS-OPTION	 TO WS-ACT.
	     MOVE ZERO		 TO WS-PAGE
				    W25-QNT
				    W25-TOTQNT
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
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     PERFORM RESTORE-SCREEN-3.

       FA15.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S05D.
	     PERFORM SCREEN-SHADOW.
	     MOVE W12-TODAY	 TO W15-START W15-END.

       FA20.
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
		     GO TO FA999-EXIT
		 WHEN OTHER	 CALL X"E5"
		     GO TO FA20
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO FA20.
	     DISPLAY CLEAR-L25.
	     MOVE W15-START	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO FA20.
             MOVE W20-DTE        TO W15-S-YMD.
	     MOVE W15-END	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO FA20.
             MOVE W20-DTE        TO W15-E-YMD.
	   IF W15-E-YMD < W15-S-YMD
	       MOVE "End date is before start date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO FA20.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE SPACES	 TO W80-ACNO.

       FA25.
	   IF WS-ACT = "A"
	       GO TO FA40.
	     DISPLAY S10D.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F9" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " Debtor Lookup (Any Word),"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       FA30.
	     ACCEPT S10 AT 1024.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     IF WS-PAGE > 0
			 GO TO FA110
		     END-IF
		     GO TO FA35
016550		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE SPACE  TO WS-TYP
016560		     PERFORM AA50D THRU AA59
		 WHEN F9-KEY
		     MOVE "A"	 TO WS-TYP
		     PERFORM AA50D THRU AA59
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA30
	       END-EVALUATE
018840	       DISPLAY S10 AT 1024
018850         IF W80-ACNO = SPACES
018860		   GO TO FA30.

       FA35.
018870	     DISPLAY CLEAR-L25.
018880	   IF W80-ACNO = SPACES
018900	       GO TO FA999-EXIT.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     PERFORM CA155-GET-DEBTOR.
018940	   IF WS-ERROR = 1
032780	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
018960	       GO TO FA30.
018970     IF WS-ERROR = 2
032810	       MOVE "Invalid account"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019000	       GO TO FA30.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
019020	     MOVE DEB-NAME	 TO W40-NAME.
019030     IF W80-TYPE = 1
019040         INSPECT W40-NAME REPLACING ALL "*" BY " "
019050         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
019060	     DISPLAY W40-NAME AT 1031
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       MOVE SPACES	 TO W40-NAME
	       DISPLAY W40-NAME AT 1031
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       GO TO FA25.

       FA40.
	     DISPLAY S11.

       FA45.
011560	     ACCEPT WS-OPTION AT 1458
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO FA999-EXIT
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA45
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "A" OR "S")
	       GO TO FA45.
	     MOVE WS-OPTION	 TO WS-STK.
	     MOVE SPACES	 TO W10-EXT-ITEM.
	     MOVE ZERO		 TO W25-TOTAL W25-TOTQNT.

       FA50.
	   IF WS-STK = "A"
	       GO TO FA60.
	     DISPLAY S11D.
020960	     PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
	   IF W10-STCK = "A"
021070	       ACCEPT S36 AT 1622
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S36-3 AT 1622
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S36-4 AT 1622
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S36-5 AT 1622
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S36-6 AT 1622
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S36-7 AT 1622
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S36-8 AT 1622
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S36-9 AT 1622
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S36-10 AT 1622
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S36-11 AT 1622
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S36-12 AT 1622
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S36-13 AT 1622
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S36-14 AT 1622.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
018870		     DISPLAY CLEAR-L25
		     GO TO FA15
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN F5-KEY
		     MOVE "X"	 TO WS-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA50
	       END-EVALUATE
	       MOVE "S" 	 TO WS-LOOK
016560	       PERFORM AA50 THRU AA59
	       IF W10-EXT-ITEM = SPACES
		   GO TO FA50
	       END-IF
021010	       DISPLAY S36 AT 1622.
021040	   IF W10-EXT-ITEM = SPACES
021060	       GO TO FA15.

       FA55.
021070	     PERFORM GA000.
021080     IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50 THRU AA59
018630	       DISPLAY S36 AT 1622
018640	       IF W10-EXT-ITEM = SPACES
018720		   GO TO FA50
               ELSE
		   GO TO FA55.
	     DISPLAY STK-DESC AT 1637
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
      *
      *    ****    D I S P L A Y   C O N F I R M A T I O N   W I N D O W
      * 	   B E L O W   I T E M	 D E S C R I P T I O N
      *
	     MOVE 18		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
020250	   IF WS-OPTION = "N"
	       GO TO FA50.

       FA60.
	   IF WS-ACT = "S"
	       MOVE W80-ACNO	 TO CRD-AC
	   ELSE
	       MOVE "!" 	 TO CRD-AC.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY "Reading" AT 1812.
	     MOVE ZERO		 TO W25-VALUE W25-QNT.

       FA63.
	     MOVE W15-S-YMD	 TO CRD-DTE.
	     PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       GO TO FA85.
	     MOVE "N"		 TO WS-DEB.

       FA65.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       IF (WS-ACT = "S") AND (WS-DEB = "N")
		   GO TO FA85
	       END-IF
	       MOVE "Y" 	 TO WS-SKIP
	       GO TO FA90.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY CRD-KEY AT 1820
		       WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	   IF WS-ACT = "S"
	       IF NOT (CRD-AC = W80-ACNO)
		   GO TO FA100.
	   IF WS-ACT = "A"
	       IF W80-ACNO = SPACES
		   MOVE CRD-AC	 TO W80-ACNO
		   MOVE "N"	 TO WS-DEB
		   GO TO FA63.
      *
      *    ****    H A S   A C C O U N T   C H A N G E D   ?
      *
	   IF NOT (CRD-AC = W80-ACNO)
	       GO TO FA90.
      *
      *    ****    E N D   O F	 E X T R A C T I O N   P E R I O D   ?
      *
	   IF CRD-DTE > W15-E-YMD
	       GO TO FA90.
      *
      *    ****    O N L Y   P R I N T	 S E L E C T E D   I T E M   ?
      *
	   IF WS-STK = "S"
	       IF NOT (CRD-CODE = W10-EXT-ITEM)
		   GO TO FA65.
      *
      *    ****    M U S T   F I R S T	 P A G E   H E A D E R	 B E
      * 	   P R I N T E D   ?
      *
	   IF WS-PAGE > ZERO
	       GO TO FA80.

       FA70.
	     MOVE "H"		 TO W02-PRN-TYPE.
016620       MOVE SPACES         TO REP-DETAIL1.
016630	     MOVE "PAGE:"	 TO SLD-PAGE.
016640	     MOVE "DATE:"	 TO SLD-DATE.
016650	     ADD 1		 TO WS-PAGE.
016660	     MOVE W12-TODAY	 TO SLD-DTE.
016670	     MOVE WS-PAGE	 TO SLD-P-NO.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO SLD-LINE1.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO SLD-LINE1.
016680	     MOVE W95-COMP	 TO SLD-CO.
016690	     MOVE " - SALES PER DEBTOR"
				 TO SLD-EXHEAD.
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
	     MOVE SPACES	 TO SLD-LINE1.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "X"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012760	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
016620	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "Ac.No."	 TO SLD-H1.
	     MOVE "Debtors Name" TO SLD-H2.
	     MOVE "Date"	 TO SLD-H3.
	     MOVE "T"		 TO SLD-H4.
	     MOVE "Refer"	 TO SLD-H5.
	     MOVE "Item code"	 TO SLD-H6.
	     MOVE "Description"  TO SLD-H7.
	     MOVE "Quantity"	 TO SLD-H8.
	     MOVE "Sell price"	 TO SLD-H9.
	     MOVE "Extended"	 TO SLD-H10.
	     PERFORM CALL-PRINTUTL.
	     MOVE ALL "="	 TO SLD-LINE1.
	     PERFORM CALL-PRINTUTL.
016620	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.
      *
      *    ****    M U S T   D E B T O R   D E T A I L S   B E
      * 	   P R I N T E D   ?
      *
       FA80.
	   IF WS-DEB = "N"
      *
      *    ****    N E W   P A G E   ?
      *
	       IF W02-LINAGE > (W02-PRN-LENGTH - 3)
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
016060		   PERFORM FA70
	       END-IF
	       MOVE "Y" 	 TO WS-DEB
	       MOVE CRD-AC	 TO SLD-AC
	       MOVE CRD-NAME	 TO SLD-NME
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
016620	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE CRD-DTE	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY.
	     MOVE W22-DTE3	 TO SLD-DATE1.
	     COMPUTE W25-TRANVAL = CRD-QNT * CRD-SELL.
	   IF CRD-SELL < ZERO
	       MOVE "C" 	 TO SLD-DOC
	       COMPUTE W25-TRANVAL = W25-TRANVAL * -1
	   ELSE
	       MOVE "I" 	 TO SLD-DOC.
	     MOVE CRD-REF	 TO SLD-REF.
	     MOVE CRD-CODE	 TO SLD-ITEM STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     MOVE STK-DESC	 TO SLD-DESC.
	     MOVE CRD-QNT	 TO SLD-QTY.
	     MOVE CRD-SELL	 TO SLD-SELL.
	     MOVE W25-TRANVAL	 TO SLD-TOTAL.
	     COMPUTE W25-VALUE = W25-VALUE + W25-TRANVAL.
	     COMPUTE W25-QNT = W25-QNT + CRD-QNT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
016060	       PERFORM FA70
	       MOVE "N" 	 TO WS-DEB.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       GO TO FA115.
016620       MOVE SPACES         TO REP-DETAIL1.
	     GO TO FA65.

       FA85.
	   IF WS-ACT = "S"
	       MOVE "No data to report"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE.

       FA88.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE 0901		 TO CPOS.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 17.
	     PERFORM SAVE-SCREEN-3.
	     GO TO FA25.

       FA90.
	   IF WS-DEB = "Y"
	       ADD W25-QNT	 TO W25-TOTQNT
	       ADD W25-VALUE	 TO W25-TOTAL
	       MOVE "Account Total"
				 TO SLD-DESC
	       MOVE W25-QNT	 TO SLD-QTY
	       MOVE W25-VALUE	 TO SLD-TOTAL
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE ZERO	 TO W25-VALUE
	       MOVE ZERO	 TO W25-QNT
	       MOVE ALL "="	 TO SLD-LINE1
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
016620	     MOVE SPACES	 TO REP-DETAIL1.

       FA95.
	   IF WS-ACT = "S"
	       MOVE "N" 	 TO WS-SKIP
	       GO TO FA88.
	   IF WS-SKIP = "Y"
	       GO TO FA110.
	     GO TO FA105.

       FA100.
	     PERFORM FA90.
	   IF WS-ACT = "S"
	       GO TO FA88.
      *
      *    ****    F I N D   N E X T   A C C O U N T
      *
       FA105.
	   IF NOT (CRD-AC = W80-ACNO)
	       MOVE CRD-AC	 TO W80-ACNO
	       GO TO FA63.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO FA110.
	     GO TO FA105.

       FA110.
	     MOVE "Total for all Accounts"
				 TO SLD-DESC.
	     MOVE 2		 TO WS-ADVANCE.
	     MOVE W25-TOTQNT	 TO SLD-QTY.
	     MOVE W25-TOTAL	 TO SLD-TOTAL.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "****    E N D   O F   R E P O R T    ****"
				 TO SLD-NME.

       FA115.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

	FA999-EXIT.
             EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
013300 GA000	     SECTION 50.
013310 GA00.
013310	     MOVE 18		 TO W50-LENGTH.
013330 GA05.
	   IF WS-EXT-STK = "Y"
013320	       MOVE W10-EXT-ITEM TO W50-DATA
	   ELSE
	       MOVE W10-ITEM	 TO W50-DATA
	       MOVE W10-ITM	 TO W50-ITM.
	   IF W50-1 = SPACE
	       IF WS-EXT-STK = "Y"
		   MOVE W50-17	 TO W10-EXT-ITEM
	       ELSE
		   MOVE W50-13	 TO W10-ITEM
	       END-IF
	       GO TO GA05.
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-EXT-ITEM TO W10-EXT-ITEM.

       GA06.
013380	     MOVE W50-EXT-ITEM	 TO STK-CODE.

       GA10.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-STATUS = "23"
	       MOVE W50-EXT-ITEM TO W10-EXT-CODE2
	       PERFORM GA200
	       IF NOT (STK-XREF = W10-EXT-ITEM)
		   MOVE 22	 TO WS-F-ERROR
	       ELSE
		   MOVE STK-CODE TO W10-EXT-ITEM.
           IF WS-F-ERROR = 0
	       PERFORM GA500.

       GA99.
             EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   
      *           A L T E R N A T E   C O D E
      *
013520 GA100	     SECTION 50.
013530 GA101.
013540       MOVE 10             TO W50-LENGTH.
013550 GA105.
013560       MOVE W10-ACODE      TO W50-DATA.
013570     IF W50-1 = SPACE
013580         MOVE W50-9        TO W10-ACODE
013590	       GO TO GA105.
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
013650	     MOVE W50-ACODE	 TO STK-ACODE W10-ACODE.
013660 GA110.
013670       PERFORM START-AT-ALT-CODE THRU READ-STOCK-EXIT.
013680     IF WS-F-ERROR = 22
013690	       GO TO GA119.
013700       PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
013710     IF WS-F-ERROR = 0
013720	       PERFORM GA500.
013730 GA119.
013740       EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   X R E F
      *
015170 GA200	     SECTION 50.
015180 GA201.
013310	     MOVE 18		 TO W50-LENGTH.

       GA205.
013320	     MOVE W10-EXT-CODE2	 TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-17	 TO W10-EXT-CODE2
	       GO TO GA205.
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-EXT-ITEM	 TO W10-EXT-CODE2.

       GA206.
013380	     MOVE W50-EXT-ITEM	  TO STK-XREF.

       GA210.
             PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
	       GO TO GA299.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
	   IF NOT (STK-XREF = W50-EXT-ITEM)
	       GO TO GA299.
	     PERFORM GA500.

       GA299.
             EXIT.
      *
      *    ****    Calculate the inclusive prices
      *
       GA500	     SECTION 50.
       GA505.
      * 			 Adjust the average cost for negative
      * 			 or zero quantity
	   IF STK-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
	     MOVE 1		 TO W50-S1.

       GA510.
	   IF STK-WSL(W50-S1) = ZERO
	       IF STK-CSH(W50-S1) NOT = ZERO
		   MOVE STK-CSH(W50-S1)
				 TO STK-WSL(W50-S1)
               ELSE
		   MOVE STK-SL(W50-S1)
				 TO STK-WSL(W50-S1).
015630	   IF STK-CSH(W50-S1) = ZERO
015640	       MOVE STK-SL(W50-S1)
				 TO STK-CSH(W50-S1).
	     MOVE STK-SL(W50-S1) TO W10-SL(W50-S1).
	     MOVE STK-CSH(W50-S1)
				 TO W10-CSH(W50-S1).
	     MOVE STK-WSL(W50-S1)
				 TO W10-WSL(W50-S1).
	     MOVE ZERO		 TO W05-RATE.
           IF STK-TAX > 0
	       MOVE W05-VAT(STK-TAX)
				 TO W05-RATE.
	     COMPUTE W10-VCSH(W50-S1) ROUNDED
		   = W10-CSH(W50-S1) + (W10-CSH(W50-S1) * W05-RTE).
	     COMPUTE W10-VWSL(W50-S1) ROUNDED
		   = W10-WSL(W50-S1) + (W10-WSL(W50-S1) * W05-RTE).
	     COMPUTE W10-VSL(W50-S1) ROUNDED
		   = W10-SL(W50-S1) + (W10-SL(W50-S1) * W05-RTE).
	   IF W50-S1 < 4
	       ADD 1		 TO W50-S1
	       GO TO GA510.
	   IF STK-PRC = ZERO
	       MOVE 1		 TO STK-PRC.

015650 GA599.
015660       EXIT.

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
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
      *    *************************************************************
      *    *** D E B T O R   A L T E R N A T E	 I N D E X   F I L E ***
      *    *************************************************************
	     MOVE "DEBALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBALT.
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
025110	     MOVE 3		 TO WS-PARKEY.
025120       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
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
054300	     OPEN I-O STOCK.
054310     IF RUNTIME-ERROR
054320         IF FLE-LOCKED
054330             GO TO ZA200
054340         ELSE
054350         IF FLE-LIMIT
054360             GO TO ZA49
054370         ELSE
054380	       IF IDX-CORRUPT
054390             MOVE 22       TO WS-F-ERROR
054400             GO TO ZA50.
054410     IF WS-STATUS NOT = "00"
054420         MOVE 22           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054430		   PERFORM OPEN-ERROR.

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
020890	     MOVE 3		 TO WS-PARKEY.
020900       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
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

039850 ZA50.
	   IF WS-F-ERROR = 22
               MOVE "STOCK file" TO WS-FILE
           ELSE
           IF WS-F-ERROR = 25
               MOVE "STOCK MEMO" TO WS-FILE
	   ELSE
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
	       MOVE "Supervisor�"  TO WS-WRKHD
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
056620	   ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 47
041860	       MOVE W02-DEBALT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DAL-KEY	 TO WS-KEYX.
041880	   IF WS-STATUS = "10"
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
