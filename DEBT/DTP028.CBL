      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******    ******    *
      *   **	**     **     **    **	 **  **   **	**  **	  **   *
      *   **	**     **     **    **	**    **       **   **	  **   *
      *   **	**     **     *******	**    **     **      ******    *
      *   **	**     **     **	**    **   **	    **	  **   *
      *   **	**     **     **	 **  **   **	    **	  **   *
      *   *******      **     **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R	  P U R C H A S E S   S T A T I S T I C S      *
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
000020 PROGRAM-ID.     DTP028.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   July 1990.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2011
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
002970 77  WS-SMAN	   PIC 9(02).
002980 77  WS-PAGE         PIC 9(04)     COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002990 77  WS-OPTION	   PIC X(01).
002990 77  WS-AGE          PIC X(01).
002990 77  WS-SEQ          PIC X(01).
002990 77  WS-PSEQ	   PIC X(01).
003000 77  WS-CREDIT       PIC X(01).
002990 77  WS-ZERO         PIC X(01).
       77  WS-NUM          PIC Z9.
003020 77  WS-ADDR         PIC X(01).
003030 77  WS-ERROR        PIC 9(01).
003040 77  WS-ERROR1       PIC 9(01).
003110 77  WS-EXPAND	   PIC X(01)	 VALUE " ".
003180 77  WS-COMP         PIC 9(01).
003180 77  WS-STM          PIC 9(01).
003190 77  WS-TYPE         PIC X(01).
003200 77  WS-HEAD         PIC X(01).
003230 77  WS-BATCH        PIC 9(05).
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

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "028".

003440 COPY WS.WS.

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
001080	   03  TRL-LINE1 REDEFINES W02-PRN-LINE.
001090	       05  TRL-DET     PIC  X(132).
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
001790	       05  CAT-DESC    PIC  X(30).
001800	       05  FILLER      PIC  X(88).
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
	   03  PUR-L3 REDEFINES W02-PRN-LINE.
	       05  PUR-H10     PIC  X(14).
	       05  PUR-SMAN    PIC  Z9.
	       05  FILLER      PIC  X(01).
	       05  PUR-SALESM  PIC  X(30).
	       05  FILLER      PIC  X(89).

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

       01  W35-SALES-STAFF.
	   03  W35-SMNO        PIC  9(02) COMP.
	   03  W35-SALESMAN    PIC  X(30).

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
	   03  W55-TOTALS.
	       05  W55-TOT1   PIC S9(09)V99 COMP-3.
	       05  W55-TOT2   PIC S9(09)V99 COMP-3.
	       05  W55-TOT3   PIC S9(09)V99 COMP-3.
	       05  W55-TOT4   PIC S9(09)V99 COMP-3.
	       05  W55-TOT5   PIC S9(09)V99 COMP-3.
	       05  W55-TOT6   PIC S9(09)V99 COMP-3.

005190 01  W60-NME-ADD.
005200     03  W60-NAME.
005210	       05  W60-NCHAR OCCURS 40 PIC X(01).
005220     03  W60-ADDRESS.
005230	       05  W60-ACHAR OCCURS 60 PIC X(01).

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-DNO	      PIC  X(06) OCCURS 18.
	   03  W75-SM	      PIC  9(02) OCCURS 10.

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

       01  S19.
006650	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT
006660                           VALUE "PRINTING PURCHASE STATISTICS".
006260	   03  LINE  5 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")".

       01  S20.
	   03  LINE 12 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z9 USING WS-SMAN AUTO.

       COPY SALESMAN.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

004870 COPY S99.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
003240	   IF LS0-DBLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	     PERFORM FA000.

       AA49.
006950	     EXIT PROGRAM.

       COPY APAC.HLP.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP5.

       COPY SALESMAN.LUP.

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
013580 FA000		       SECTION 50.
013590 FA00-INIT.
013600       DISPLAY CLR-SCREEN.
             DISPLAY S19.

011530 FA05-ACCEPT.
011560	     ACCEPT WS-OPTION AT 0531
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
011630	   IF WS-OPTION = "N"
011640         GO TO FA999-EXIT.
	     DISPLAY WS-BLNK78 AT 0702 WITH FOREGROUND-COLOR 3.
             INITIALIZE W55-MONTHS.
             MOVE W12-MONTH      TO W55-MTH6.
           IF W55-MTH6 > 1
               SUBTRACT 1 FROM W55-MTH6
                                 GIVING W55-MTH5
           ELSE
               MOVE 12           TO W55-MTH5.             
           IF W55-MTH5 > 1
               SUBTRACT 1 FROM W55-MTH5
                                 GIVING W55-MTH4
           ELSE
               MOVE 12           TO W55-MTH4.             
           IF W55-MTH4 > 1
               SUBTRACT 1 FROM W55-MTH4
                                 GIVING W55-MTH3
           ELSE
               MOVE 12           TO W55-MTH3.             
           IF W55-MTH3 > 1
               SUBTRACT 1 FROM W55-MTH3
                                 GIVING W55-MTH2
           ELSE
               MOVE 12           TO W55-MTH2.             
           IF W55-MTH2 > 1
               SUBTRACT 1 FROM W55-MTH2
                                 GIVING W55-MTH1
           ELSE
               MOVE 12           TO W55-MTH1.             
             MOVE W24-MONTH (W55-MTH1)
                                 TO W55-H1.
             MOVE W24-MONTH (W55-MTH2)
                                 TO W55-H2.
             MOVE W24-MONTH (W55-MTH3)
                                 TO W55-H3.
             MOVE W24-MONTH (W55-MTH4)
                                 TO W55-H4.
             MOVE W24-MONTH (W55-MTH5)
                                 TO W55-H5.
             MOVE W24-MONTH (W55-MTH6)
                                 TO W55-H6.
             MOVE ZERO           TO WS-PAGE.
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

       FA08.
	     MOVE "'D'ebtor Seq or per 'S'ales person  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "D" OR "S")
	       GO TO FA08.
	     MOVE WS-OPTION	 TO WS-PSEQ.
	   IF WS-PSEQ = "S"
	       GO TO FA50.

        FA10.
	     MOVE "H"		 TO W02-PRN-TYPE.
016620       MOVE SPACES         TO REP-DETAIL1.
016630       MOVE "PAGE:"        TO TRL-PAGE.
016640       MOVE "DATE:"        TO TRL-DATE.
016650	     ADD 1		 TO WS-PAGE.
016660       MOVE W12-TODAY      TO TRL-DTE.
016670       MOVE WS-PAGE        TO TRL-P-NO.
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
016690       MOVE " - PURCHASE STATISTICS" 
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
	   IF WS-PSEQ = "S"
	       MOVE "Sales Person:"
				 TO PUR-H10
	       MOVE W35-SMNO	 TO PUR-SMAN
	       MOVE W35-SALESMAN TO PUR-SALESM
	       IF NOT (WS-PRINTER = "S")
		   MOVE 0	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
	       END-IF
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO TRL-LINE1.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "X"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012760	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
016710	     MOVE SPACES	 TO TRL-DET.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
016620	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE "Ac.No."       TO PUR-H1.
             MOVE "Debtors Name" TO PUR-H2.
             MOVE W55-H1         TO PUR-H3.
             MOVE W55-H2         TO PUR-H4.
             MOVE W55-H3         TO PUR-H5.
             MOVE W55-H4         TO PUR-H6.
             MOVE W55-H5         TO PUR-H7.
             MOVE W55-H6         TO PUR-H8.
             MOVE "Total"        TO PUR-H9.
	     PERFORM CALL-PRINTUTL.
	     MOVE ALL "="	 TO PUR-L1.
	     PERFORM CALL-PRINTUTL.
016620	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

       FA15.
	     PERFORM RESTORE-SCREEN-3.
	     DISPLAY "A" AT 0712 WITH FOREGROUND-COLOR 11
                     "ccount or " WITH FOREGROUND-COLOR 3
                     "N" WITH FOREGROUND-COLOR 11
                     "ame sequence" WITH FOREGROUND-COLOR 3.
             MOVE "N"            TO WS-SEQ.

       FA20.
	     ACCEPT WS-SEQ AT 0737
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-SEQ
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-SEQ = "A" OR "N")
	       GO TO FA20.
	     MOVE " !"		 TO DEB-NAME DEB-ACNO.

       FA25.
             DISPLAY "Debtor account:" AT 0912.
           IF WS-SEQ = "A"
               PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT
           ELSE
               PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
010780     IF WS-F-ERROR = 6
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE " N O   D E T A I L S   T O   R E P O R T"
                                 TO PUR-H2
010790         GO TO FA40.

010800 FA30.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       GO TO FA40.

       FA32.
010810	     PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
010780     IF WS-F-ERROR = 6
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
010790	       GO TO FA35.
	   IF WS-PSEQ = "S"
	       IF NOT (W35-SMNO = DEB-SMAN)
		   GO TO FA32.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY DEB-ACNO AT 0928
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
017450	     MOVE DEB-NAME	 TO W40-NAME.
017460     IF DEB-TYPE = 1
017470         INSPECT W40-NAME REPLACING ALL "*" BY "."
017480         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
017490       MOVE DEB-ACNO       TO PUR-AC.
017500       MOVE W40-NAME       TO PUR-NME.
             MOVE DEB-MTHPUR (W55-MTH1)
                                 TO W55-VAL1.
             MOVE DEB-MTHPUR (W55-MTH2)
                                 TO W55-VAL2.
             MOVE DEB-MTHPUR (W55-MTH3)
                                 TO W55-VAL3.
             MOVE DEB-MTHPUR (W55-MTH4)
                                 TO W55-VAL4.
             MOVE DEB-MTHPUR (W55-MTH5)
                                 TO W55-VAL5.
             MOVE DEB-MTHPUR (W55-MTH6)
                                 TO W55-VAL6.
             MOVE W55-VAL1       TO PUR-VAL1.
             MOVE W55-VAL2       TO PUR-VAL2.
             MOVE W55-VAL3       TO PUR-VAL3.
             MOVE W55-VAL4       TO PUR-VAL4.
             MOVE W55-VAL5       TO PUR-VAL5.
             MOVE W55-VAL6       TO PUR-VAL6.
             COMPUTE PUR-TOTAL = W55-VAL1 + W55-VAL2 + W55-VAL3
                               + W55-VAL4 + W55-VAL5 + W55-VAL6.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 4)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
016060	       PERFORM FA10.
016070       MOVE SPACES         TO REP-DETAIL1.
             GO TO FA30.

       FA35.
	   IF WS-PSEQ = "S"
	       MOVE "****    END OF SALES-PERSON / REP    ****"
				 TO PUR-H2
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO FA50.
             MOVE "****    E N D   O F   R E P O R T    ****"
                                 TO PUR-H2.

       FA40.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     GO TO FA999-EXIT.

       FA50.
	     DISPLAY "Sales Person No." AT 1204.
	     MOVE ZERO		 TO WS-SMAN.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to view Sales Staff "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       FA55.
020380	     ACCEPT S20.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO WS-SMAN
		     GO TO FA60
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM SAVE-SCREEN-2
016560		     PERFORM SALESMAN-VIEW
		     PERFORM RESTORE-SCREEN-2
		 WHEN OTHER
		     CALL X"E5"
		     GO TO FA55
	       END-EVALUATE
016590	       GO TO FA55.
	   IF WS-SMAN = ZERO
	       GO TO FA60.
	   IF WS-SMAN > 50
033530	       MOVE "Invalid Salesman Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
010840	       GO TO FA55.
010850	     ADD 100 WS-SMAN	 GIVING WS-PARKEY.
010860	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
020110	   IF PAR-SALESMAN = SPACES
033530	       MOVE "Invalid Salesman Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
010840	       GO TO FA55.
020160	     DISPLAY PAR-SALESMAN AT 1224
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 19		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
020030	   IF WS-OPTION = "N"
010840	       GO TO FA55.
	     MOVE WS-SMAN	 TO W35-SMNO.
	     MOVE PAR-SALESMAN	 TO W35-SALESMAN.
	     PERFORM RESTORE-SCREEN-3.
	     GO TO FA10.

       FA60.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       FA999-EXIT.
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
