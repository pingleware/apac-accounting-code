      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******    ******    *
      *   **	**     **     **    **	 **  **   **	**  **	  **   *
      *   **	**     **     **    **	**    **	**  **	       *
      *   **	**     **     *******	**    **     ****   *******    *
      *   **	**     **     **	**    **	**  **	  **   *
      *   **	**     **     **	 **  **   **	**  **	  **   *
      *   *******      **     **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R	  C O N T R A C T   D E L E T E 	       *
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
000020 PROGRAM-ID.     DTP036.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   February 1985.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1985 - 2011
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

000160 COPY DBFOTO.SL.

000140 COPY DEBMEM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000250 COPY PARAM.FDE.

000270 COPY DEBTOR.FDE.

000290 COPY DBFOTO.FDE.

000140 COPY DEBMEM.FDE.

000390 COPY RECOVER.FDE.

       COPY SHARED.FDE.

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
002790 77  WS-SUB	   PIC 9(04)	 COMP-5.
002790 77  WS-SUB-1        PIC 9(04)     COMP-5.
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
002930 77  WS-NETKEY	   PIC 9(06)	 COMP-5.
002930 77  WS-RECKEY	   PIC 9(06)	 COMP-5.
002940 77  WS-WRKEY        PIC 9(04)     COMP-5.
001720 77  WS-RECOVER	   PIC	9(06)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
001730 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
002960 77  WS-NTP	   PIC 9(01).
002970 77  WS-LINES        PIC 9(02).
002980 77  WS-PAGE         PIC 9(04)     COMP-5.
002990 77  WS-OPTION       PIC X(01).
002990 77  WS-AGE          PIC X(01).
002990 77  WS-SEQ          PIC X(01).
003000 77  WS-CREDIT       PIC X(01).
002990 77  WS-ZERO         PIC X(01).
001820 77  WS-SKIP         PIC  X(01) VALUE "N".
       77  WS-NUM          PIC Z9.
003020 77  WS-ADDR         PIC X(01).
003030 77  WS-ERROR        PIC 9(01).
003040 77  WS-ERROR1       PIC 9(01).
003110 77  WS-EXPAND	   PIC X(01)	 VALUE " ".
003180 77  WS-COMP         PIC 9(01).
003180 77  WS-IND	   PIC 9(01).
003190 77  WS-TYPE         PIC X(01).
003200 77  WS-HEAD         PIC X(01).
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
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "031".

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

       COPY DBFOTO.ID.

000140 COPY DEBMEM.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY SHARED.ID.

003650 01  W05-LIST.
003660     03  W05-OPT         PIC 9(03).
003670     03  W05-TYPES REDEFINES W05-OPT.
003680         05  W05-COMP    PIC 9(01).
003690         05  W05-INDV    PIC 9(01).
003700         05  W05-INT     PIC 9(01).

003710 01  W10-SPARES.
003720	   03  W10-DATE	    PIC X(10).
           03  W10-RDATE REDEFINES W10-DATE.
	       05  W10-DTE     PIC 99/99/9999.

       COPY W12.WS.

       COPY W15.WS.

       COPY W20.WS.

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

000030 01  W30-DBFOTO.
	   03  W30-KEY.
000170	       05  W30-AC      PIC  X(06).
	       05  W30-CONT    PIC  X(08).
	   03  W30-DESC	       PIC  X(30).
000060	   03  W30-DATE        PIC  9(08)     COMP-3.
	   03  W30-RENTAL      PIC S9(07)V99  COMP-3.
	   03  W30-TYPE        PIC  X(01).
	   03  W30-FREQ        PIC  X(01).
	   03  W30-ADATE       PIC  9(08)     COMP-3.
	   03  W30-ESC	       PIC  9(03)V99  COMP-3.
	   03  W30-RATE        PIC S9(06)V999 COMP-3.
000110	   03  W30-RDTE        PIC  9(08)     COMP-3.
000100	   03  W30-STRDNG      PIC  9(08)     COMP-5.
000100	   03  W30-LSTRDNG     PIC  9(08)     COMP-5.
000100	   03  W30-READING     PIC  9(08)     COMP-5.
000120	   03  W30-VALUE       PIC S9(07)V99  COMP-3.
000120	   03  W30-TAX	       PIC S9(05)V99  COMP-3.
	   03  W30-TCODE       PIC  X(01)     COMP-X.
	   03  W30-VG.
	       05  W30-VAT     PIC  9(01).
	   03  FILLER	       PIC  X(15).

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

       01  S01.
	   03  LINE  2 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
				"DELETE RENTAL AGREEMENT DETAILS".
	   03  LINE  4 COLUMN 10 VALUE "Account No     :".
	   03  LINE  5 COLUMN 10 VALUE "Contract No    :".
	   03  LINE  6 COLUMN 10 VALUE "Description    :".
	   03  LINE  7 COLUMN 10 VALUE "Contract Date  :".
	   03	       COLUMN 38 VALUE "(".
	   03	       COLUMN 39 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "DD".
	   03	       COLUMN 41 VALUE "/".
	   03	       COLUMN 42 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "MM".
	   03	       COLUMN 44 VALUE "/".
	   03	       COLUMN 45 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "CCYY".
	   03	       COLUMN 49 VALUE ")".
	   03  LINE  8 COLUMN 10 VALUE "Payment Amount :".
	   03	       COLUMN 38 VALUE "(Basic)".
005040	   03	       COLUMN 46 VALUE "Tax :".
	   03	       COLUMN 54 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 55 VALUE "=No VAT, ".
	   03	       COLUMN 64 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 65 VALUE "=Apply VAT".
	   03  LINE  9 COLUMN 10 VALUE "Transact. Type :".
	   03	       COLUMN 38 VALUE "(".
	   03	       COLUMN 39 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "R".
	   03	       COLUMN 40 VALUE "ental/".
	   03	       COLUMN 46 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "S".
	   03	       COLUMN 47 VALUE "ervice Contract/".
	   03	       COLUMN 63 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "O".
	   03	       COLUMN 64 VALUE "ther)".
	   03  LINE 10 COLUMN 10 VALUE "Frequency      :".
	   03	       COLUMN 38 VALUE "(".
	   03	       COLUMN 39 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 40 VALUE "/".
	   03	       COLUMN 41 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 42 VALUE "/".
	   03	       COLUMN 43 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 44 VALUE "/".
	   03	       COLUMN 45 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 46 VALUE "/".
	   03	       COLUMN 47 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 48 VALUE " monthly/".
	   03	       COLUMN 57 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 58 VALUE "nnual)".
	   03  LINE 11 COLUMN 10 VALUE "Escalation Rate:".
	   03	       COLUMN 38 VALUE "(Annual escalation factor %)".
	   03  LINE 12 COLUMN 10 VALUE "Reading        :".
	   03	       COLUMN 38 VALUE "(Reading at start of contract)".
	   03  LINE 13 COLUMN 10 VALUE "Unit Rate      :".
	   03	       COLUMN 38 VALUE
				 "(Monthly usage - unit/page rate)".
	   03  LINE 14 COLUMN 10 VALUE "Transaction Cde:".
	   03	       COLUMN 38 VALUE
				 "(Debtor transaction code to use)".
	   03  LINE 15 COLUMN 10 VALUE "Next Payment   :".
	   03	       COLUMN 38 VALUE
				 "(The next payment date)".

       01  S02.
	   02  S02A.
	   03  LINE  4 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(06) USING W30-AC AUTO.
	   02  S02B.
	   03  LINE  5 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(08) USING W30-CONT AUTO.
	   02  S02C.
	   03  LINE  6 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(30) USING W30-DESC AUTO.
	   03  LINE  7 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC 99/99/9999 USING W30-DATE AUTO.
	   03  LINE  8 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(06)9.99 USING W30-RENTAL AUTO.
	   03	       COLUMN 52 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC 9(01) USING W30-VAT AUTO.
	   03  LINE  9 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(01) USING W30-TYPE AUTO.
	   03  LINE 10 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(01) USING W30-FREQ AUTO.
	   03  LINE 11 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC ZZ9.99 USING W30-ESC
				 BLANK ZERO AUTO.
	   03  LINE 12 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9 USING W30-STRDNG
				 BLANK ZERO AUTO.
	   03  LINE 13 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(04)9.999 USING W30-RATE
				 BLANK ZERO AUTO.
	   03  LINE 14 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z9 USING W30-TCODE
				 BLANK ZERO AUTO.
	   03  LINE 15 COLUMN 27 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC 99/99/9999 USING W30-RDTE AUTO.

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

       COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
003240	   IF LS0-DBLEV < 4
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	     PERFORM DA000.
	     CLOSE DBFOTO.
013040	     CLOSE RECOVER.

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

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
007340 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.
      
       COPY DEBTOR.RD1.
      
       COPY DBFOTO.RD.

000140 COPY DEBMEM.RD.

       COPY SHARED.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY DBFOTO.WR.

       COPY DEBTOR.WR.

       COPY PARAM.WR.

       COPY DEBTOR.LUP.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
010910 AY000           SECTION.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
010970 AY42.
010980	     MOVE 42		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011000	     MOVE DBF-RECORD1	 TO REC-DBFOTO.
011010       GO TO AY50.
011120 AY49.
011130       MOVE 99             TO REC-FILE.
011140       MOVE ZERO           TO REC-KEY.
011150       MOVE SPACES         TO REC-DETAIL.
011160       PERFORM AY50 THRU AY59.
011170       ADD 1               TO WS-TRANS.
011180     IF WS-RECOVER > 95
011190         CLOSE RECOVER
011200         OPEN OUTPUT RECOVER
011210         CLOSE RECOVER
011220         OPEN I-O RECOVER
011230         MOVE ZERO         TO WS-RECOVER.
011240       GO TO AY59.
011250 AY50.
011260	     ADD 1		 TO WS-RECOVER.
011270       MOVE WS-RECOVER     TO WS-RECKEY.
011280       MOVE WS-TRANS       TO REC-TRAN.
011290       WRITE REC-RECORD.
011300     IF WS-STATUS NOT = "00"
011310         DISPLAY "Write error Recovery file - Status " AT 2212
011320			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			WS-STATUS
011330			WITH FOREGROUND-COLOR 7 HIGHLIGHT
011340         STOP RUN.
011370 AY59.
011380       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *	     MOVE SHR-STOCK	 TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
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
012620	     MOVE "DB"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010	     MOVE 1		 TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
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
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
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
013020	     PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       IF WS-USUB < 24
		   ADD 1	 TO WS-USUB
		   GO TO AY72
	       ELSE
		   GO TO AY85.

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

       AY85.
013090	     COMMIT.

011860 AY999.
011870       EXIT.
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
      *    ****    D E B T O R	 R E N T A L   M A I N T E N A N C E
      *
037050 DA000	       SECTION 5.
037060 DA00.
020320       DISPLAY CLR-SCREEN.
020330	     DISPLAY S01.
	     INITIALIZE W30-DBFOTO.
	     DISPLAY S02.
020370       MOVE SPACES         TO W80-ACNO.
020360 DA05.
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
020380	     ACCEPT S02A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W30-AC
		     GO TO DA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		     MOVE W80-ACNO TO W30-AC
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA05
	       END-EVALUATE
016570	       DISPLAY S02A
016580	       IF W30-AC = SPACES
016590		   GO TO DA05.
	     DISPLAY CLEAR-L25.
020390	   IF W30-AC = SPACES
020400	       GO TO DA999.
             MOVE 6              TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-AC
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W30-AC	 TO DEB-ACNO W80-ACNO.
020410       PERFORM CA155-GET-DEBTOR.
020420     IF WS-ERROR = 1
026950	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020440	       GO TO DA05.
020450     IF WS-ERROR = 2
026950	       MOVE WS-ERR1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020470	       GO TO DA05.
032840	     MOVE DEB-NAME	 TO W40-NAME.
032850     IF W80-TYPE = 1
032860         INSPECT W40-NAME REPLACING ALL "*" BY " "
032870         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
019060	     DISPLAY W40-NAME AT 0434
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
	       GO TO DA05.

       DA10.
	     ACCEPT S02B.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W30-AC
		     GO TO DA00
	       END-EVALUATE.
	   IF W30-CONT = SPACES
	       GO TO DA00.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-CONT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W30-AC	 TO DBF-AC.
	     MOVE W30-CONT	 TO DBF-CONT.
	     PERFORM READ-DBFOTO THRU READ-DBFOTO-EXIT.
	   IF WS-F-ERROR = 42
	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA10.
	     MOVE DBF-RECORD1	 TO W30-DBFOTO.

       DA15.
	     DISPLAY S02C.
012180	     MOVE "Delete this contract Y/N?  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO DA15.
018520	   IF WS-OPTION = "N"
	       GO TO DA10.
	     PERFORM AY60 THRU AY999.
	     MOVE 2		 TO WS-ACTION.
	     PERFORM AY42 THRU AY59.
	     PERFORM DELETE-DBFOTO-REC THRU WRITE-DBFOTO-EXIT.
	     PERFORM AY70 THRU AY999.
	     GO TO DA00.

       DA999.
	     EXIT.

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
	     MOVE "DBFOTO"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBFOTO.
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.
044300	     OPEN I-O DBFOTO.
           IF WS-STATUS = "05"
	       CLOSE DBFOTO
	       OPEN OUTPUT DBFOTO
	       CLOSE DBFOTO
	       OPEN I-O DBFOTO.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
		   MOVE 42	 TO WS-F-ERROR
                   GO TO ZA50.
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
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

039850 ZA50.
041850	   IF WS-F-ERROR = 42
041860	       MOVE "RENTAL file"
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
041760	   ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 42
041860	       MOVE W02-DBFOTO	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DBF-KEY	 TO WS-KEYX.
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
