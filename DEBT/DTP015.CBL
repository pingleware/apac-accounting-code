      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	     **     ********   *
      *   **	**     **     **    **	 **  **     ***     **	       *
      *   **	**     **     **    **	**    **     **     *******    *
      *   **	**     **     *******	**    **     ** 	  **   *
      *   **	**     **     **	**    **     ** 	  **   *
      *   **	**     **     **	 **  **      **     **	  **   *
      *   *******      **     **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  D E B T O R   U N A L L O C A T E D   C R E D I T S   P R O G *
      * 							       *
      *  This program finds the unallocated credits in the Accounts    *
      *  Receivable transaction file. Accounts that have unallocated   *
      *  credits are displayed and the User is then able to have these *
      *  transactions allocated.				       *
      * 							       *
      *  STILL TO BE INCLUDED.					       *
      *  If the balance is zero, the program will allow the User to    *
      *  delete the account.					       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP015.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   December 2009.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2009 - 2011
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

000160 COPY DBTRAN.SL.

000140 COPY DEBMEM.SL.

000140 COPY DEBTOR.ISL.

       COPY DEBTRN.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000330 COPY DBTRAN.FDE.

000140 COPY DEBMEM.FDE.

000310 COPY DEBTOR.FDE.

       COPY DEBTRN.GFD.

000290 COPY PARAM.FDE.

000390 COPY RECOVER.GFD.

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
001560 77  WS-SUB	   PIC	9(04)	 COMP-5.
002790 77  WS-SUB-1	   PIC	9(04)	 COMP-5.
002800 77  WS-SUB-2	   PIC	9(04)	 COMP-5.
002810 77  WS-SUB-3	   PIC	9(04)	 COMP-5.
002820 77  WS-SUB-4	   PIC	9(04)	 COMP-5.
002830 77  WS-SUB-5	   PIC	9(04)	 COMP-5.
002840 77  WS-SUB-6	   PIC	9(04)	 COMP-5.
002850 77  WS-IXS1	   PIC	9(04)	 COMP-5.
002860 77  WS-IXS2	   PIC	9(04)	 COMP-5.
002870 77  WS-IXS3	   PIC	9(04)	 COMP-5.
002880 77  WS-IXS4	   PIC	9(04)	 COMP-5.
002890 77  WS-MSUB	   PIC	9(04)	 COMP-5.
001680 77  WS-DBTKEY	   PIC	9(06)	 COMP-5.
002930 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
002900 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
001710 77  WS-RECKEY	   PIC	9(06)	 COMP-5.
001720 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002960 77  WS-SEL	   PIC	X(01)	 VALUE "N".
001730 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
002970 77  WS-LINES	   PIC	9(02).
002980 77  WS-PAGE         PIC 9(04)     COMP-5.
002990 77  WS-OPTION       PIC X(01).
002990 77  WS-ACT	   PIC X(01)	 VALUE SPACE.
001820 77  WS-ALLOCATE	   PIC	X(01)	 VALUE "M".
002990 77  WS-SEQ          PIC X(01).
003000 77  WS-CREDIT       PIC X(01).
003030 77  WS-ERROR        PIC 9(01).
002010 77  WS-ERR1	   PIC	X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
003180 77  WS-COMP	   PIC 9(01).
003190 77  WS-TYPE         PIC X(01).
003200 77  WS-HEAD         PIC X(01).
003240 77  WS-MESS	   PIC X(02).
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
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "╟".
	       05  FILLER      PIC  X(01) VALUE "Ё".
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
	   03  WS-PROG	       PIC  X(03) VALUE "015".

002250 01  W00-TRAN-CODES.
           03  W00-T-VAT   PIC  9(09)V99 COMP-3.
001410     03  W00-E-DESC  PIC  X(12).
001420     03  W00-A-DESC  PIC  X(12).
001430     03  W00-ACTION  PIC  9(01).
001440     03  W00-T-CODE  PIC  9(02)    COMP-5.
001450     03  W00-T-GL    PIC  9(06)    COMP-3.
001460     03  W00-T-DAY   PIC  9(06)    COMP-5.
001470     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
001480     03  W00-T-MTD   PIC  9(06)    COMP-5.
001490     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
001500     03  W00-T-YTD   PIC  9(06)    COMP-5.
001510     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DBTRAN.ID.

000140 COPY DEBMEM.ID.

       COPY DEBTOR.ID.

       COPY DEBTRN.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY SHARED.ID.

       01  W10-TEMP.
004300	   03  W10-DATE	       PIC  X(10).
	   03  W10-RDATE REDEFINES W10-DATE.
	       05  W10-DTE     PIC  99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

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
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
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

       COPY W90.DBT.

000030 01  W91-TRAN.
           03  W91-KEY.
000170         05  W91-AC      PIC  X(06).
000060	       05  W91-DATE    PIC  9(08)    COMP.
000040         05  W91-CODE    PIC  9(02).
000070         05  W91-REF     PIC  X(08).
      *    
      *    TYPE: "U" = Unpaid.         \
      *          "P" = Paid.            >  DEBIT TRANSACTIONS
      *          "X" = Partial payment./
      *
      *          "U" = Unallocated.    \
      *          "A" = Allocated.       >  CREDIT TRANSACTIONS
      *          "X" = Partial allocat./
      *
000050     03  W91-TYPE        PIC  X(01).
000080     03  W91-REG         PIC  X(09).
000090     03  W91-ORD         PIC  X(12).
000100     03  W91-VALUE       PIC S9(09)V99 COMP-3.
000110     03  W91-TAXFREE     PIC S9(09)V99 COMP-3.
000120     03  W91-TAX         PIC S9(07)V99 COMP-3.
000130     03  W91-DISC        PIC S9(03)V99 COMP-3.
           03  W91-PAID        PIC S9(09)V99 COMP-3.
           03  W91-ALLOCATED REDEFINES W91-PAID
                               PIC S9(09)V99 COMP-3.
           03  W91-REC         PIC  X(08).
           03  W91-AGE         PIC  9(02)    COMP-5.
           03  FILLER          PIC  X(01).

005810 01  W95-STM.
005820     03  W95-COMP        PIC X(40).
005880	   03  W95-V1.
005890	       05  W95-T   PIC Z(04)9.99.
005900	   03  W95-V2.
005910	       05  W95-B   PIC Z(06)9.99-.
005920	   03  FILLER	   PIC X(11).
005940	   03  W95-TEL	       PIC X(14).

005950 01  W100-HEAD.
005960	   03  FILLER	       PIC X(40)    VALUE
005970			   "ACCOUNTS RECEIVABLE- UNALLOCATED CREDITS".

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

       COPY DEBLUP.CRT.

000850 01  S-PROG.
000860	   03  LINE  2 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
		       VALUE "ACCOUNTS RECEIVABLE UNALLOCATED CREDITS".

       01  S18.
	   03  LINE  8 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE  9 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC 99/99/9999 USING W15-END AUTO.

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

       01  SD-TRAN.
	   03  COLUMN 15 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z9/99/9999 FROM W22-DTE3.
	   03  COLUMN 27 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC X(08) FROM TRA-REF.
	   03  COLUMN 39 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC 99 FROM TRA-CODE.
	   03  COLUMN 44 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC X(12) FROM W00-E-DESC.
	   03  COLUMN 58 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z(07)9.99CR FROM W25-VALUE.

       01  S08A.
	   03  LINE 16 COLUMN  6 VALUE "Allocation:".
           03  LINE 17 COLUMN  6 VALUE "Ref. No.".
	   03	       COLUMN 18 VALUE "Date".
	   03	       COLUMN 26 VALUE "Transaction".
	   03	       COLUMN 46 VALUE "Value".
	   03	       COLUMN 57 VALUE "Balance".
	   03	       COLUMN 69 VALUE "Allocate".

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

       01  S-ALLOC.
	   03  LINE 21 COLUMN 14 VALUE "Un-allocated balance : ".
	   03	       COLUMN 37 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(08)9.99 FROM W75-BALANCE.
	   03	       COLUMN 51 VALUE "Allocated : ".
	   03	       COLUMN 63 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(08)9.99 FROM W75-ALLOCATED.

       01  S-DTRN.
	   03  COLUMN  6 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC X(08) FROM DBT-REF.
	   03  COLUMN 15 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				   PIC 9999/99/99 FROM DBT-DATE.
	   03  COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC X(12) FROM W00-E-DESC.
	   03  COLUMN 39 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 FROM DBT-VALUE.
	   03  COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 FROM DBT-OUTSTD.
	   03  COLUMN 65 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 USING DBT-ALLOC AUTO.

       COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
003240	   IF LS0-DBLEV < 2
	       DISPLAY CLEAR-L25
003250	       DISPLAY "Not Authorised - Press any key" AT 2512
003260			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	     PERFORM BA000.
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

       COPY APAC.HLP.

       COPY CLEAR.CRT.

       COPY DATE.CHK.

      * здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      * Ё G E T   A   D E B T O R   T R A N S A C T I O N   R E C O R DЁ
      * Ё							       Ё
      * Ё  Perform this routine with the transaction code in W90-CODE  Ё
      * Ё							       Ё
      * Ё  On exiting, the transaction code details are in W00-......  Ё
      * Ё  while W90-TRNKEY contains the key to the PARAMETER file     Ё
      * Ё  where this transaction code is found and W90-TS is the      Ё
      * Ё  transaction code subscript in the record.		       Ё
      * юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
000010 GET-DEBTOR-TRAN-RECORD SECTION.

000030 GET-DEBTOR-TRAN.
000040       ADD 21 W90-CODE     GIVING W90-RESULT.
000050       DIVIDE W90-RESULT BY 2
000060                           GIVING W90-TRNKEY
000070                           REMAINDER W90-TS.
000080       ADD 1               TO W90-TS.

000100 GET-DEBTOR-TRAN-CODE.
000110       MOVE W90-TRNKEY     TO WS-PARKEY.
000120       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.

000140 STORE-DEBTOR-TRAN-DESC.
000150	     MOVE PAR-E-DESC (W90-TS)
000160                           TO W00-E-DESC.
000170       MOVE PAR-A-DESC (W90-TS)
000180                           TO W00-A-DESC.
000190       MOVE PAR-ACTION (W90-TS)
000200                           TO W00-ACTION.
000210       MOVE PAR-T-CODE (W90-TS)
000220                           TO W00-T-CODE.
000230       MOVE PAR-T-GL   (W90-TS)
000240                           TO W00-T-GL.

000260 STORE-DEBTOR-TRAN-STATS.
000270       MOVE PAR-T-VAT  (W90-TS)
000280                           TO W00-T-VAT.
000290       MOVE PAR-T-DAY  (W90-TS)
000300                           TO W00-T-DAY.
000310       MOVE PAR-T-VAL  (W90-TS)
000320                           TO W00-T-VAL.
000330       MOVE PAR-T-MTD  (W90-TS)
000340                           TO W00-T-MTD.
000350       MOVE PAR-T-MTDV (W90-TS)
000360                           TO W00-T-MTDV.
000370       MOVE PAR-T-YTD  (W90-TS)
000380                           TO W00-T-YTD.
000390       MOVE PAR-T-YTDV (W90-TS)
000400                           TO W00-T-YTDV.

000420 GET-DEBTOR-TRAN-EXIT.
000430       EXIT.

000450 GET-DEBTOR-TRAN-DESC SECTION.

000470 GET-DEBT-TRAN-DESC.
000480       PERFORM GET-DEBTOR-TRAN.
000490       MOVE W90-TRNKEY     TO WS-PARKEY.
000500       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
000510       PERFORM STORE-DEBTOR-TRAN-DESC.

000530 GET-DEBT-TRAN-DESC-EXIT.
000540       EXIT.
000550

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY OPTION.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
007340 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DBTRAN.RD1.

000140 COPY DEBMEM.RD.

       COPY DEBTOR.RD.

000140 COPY DEBTRN.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY DBTRAN.WR.

       COPY DEBTOR.WR.

000140 COPY DEBTRN.WR.

       COPY PARAM.WR.

       COPY DEBTOR.LUP.

       COPY DEBTOR.ALL.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
010910 AY000           SECTION.
010920 AY01.
010930       MOVE 01             TO REC-FILE.
010940       MOVE WS-PARKEY      TO REC-KEY.
010950       MOVE PAR-RECORD1    TO REC-PARAM.
010960       GO TO AY50.

010970 AY02.
010980       MOVE 02             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
011000       MOVE DEB-RECORD1    TO REC-DEBTOR.
011010       GO TO AY50.

011070 AY04.
011080       MOVE 04             TO REC-FILE.
011090       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
011100       MOVE TRA-RECORD1    TO REC-DBTRAN.
011110       GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

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
011320                  WITH FOREGROUND-COLOR 14 WS-STATUS 
011330                  WITH FOREGROUND-COLOR 15
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

009120 CA165-NAME	       SECTION 2.
009130 CA165-EXTRACT.
009140	     MOVE ZERO		 TO WS-ERROR.
009150       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
009160         INTO W40-SURNAME W40-FNAME W40-SNAME
009170         OVERFLOW
009180		 MOVE 1 	 TO WS-ERROR.

009190 CA170-EXIT.
009200       EXIT.

      /
019020 BA000	       SECTION 50.
019030 BA00.
019040	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE "View and allocate Credits Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.

       BA05.
	     PERFORM OPT-MESSAGE.
	   IF NOT(WS-OPTION = "Y" OR "N")
	       GO TO BA05.
	   IF WS-OPTION = "N"
	       GO TO BA999.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	 Start at the beginning of the Debtor account file     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       BA10.
	     MOVE " !"		 TO DEB-ACNO.
	     PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
010780     IF WS-F-ERROR = 6
015530	       GO TO BA999.
      *
      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    ЁAccount No XXXXXX NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNЁ╟╟
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддды╟╟
      *      ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟
      *
	     MOVE 19		 TO SHADE-ROW.
	     MOVE 12		 TO SHADE-COL.
	     MOVE 58		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "зддддддддддддддддддддддддддддддддддддддддддддддддд
      -		     "ддддддддд" AT 1810 WITH FOREGROUND-COLOR 0
					      BACKGROUND-COLOR 7
		     "©" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "ЁAccount No" AT 1910 WITH FOREGROUND-COLOR 0
						BACKGROUND-COLOR 7
		     "                                                Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "ю" AT 2010 WITH FOREGROUND-COLOR 0
				      BACKGROUND-COLOR 7
		     "дддддддддддддддддддддддддддддддддддддддддддддддддд
      -		     "дддддддды" WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.

020440 BA15.
	     DISPLAY "Ac.No.  Name" AT 0402
			     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "дддддддддддддддддд UNALLOCATED CREDITS ддддддддддд
      -		     "дддддд" AT 0715
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "   Date     Reference  Code  Description         V
      -		     "alue  " AT 0915
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE 10		 TO CLIN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      G E T   T H E   N E X T	A C C O U N T	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       BA20.
015570	     PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      E N D   O F   F I L E ?		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
010780     IF WS-F-ERROR = 6
	       GO TO BA980.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Display the account number and name of each account read. Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
024100	     DISPLAY DEB-ACNO AT 1922
		     WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
019020	     MOVE DEB-NAME	 TO W40-NAME.
019030     IF W80-TYPE = 1
019040         INSPECT W40-NAME REPLACING ALL "*" BY " "
019050         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
019060	     DISPLAY W40-NAME AT 1929
		     WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	   IF NOT OPEN-ITEM
	       GO TO BA20.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	No & Name NOT displayed for unallocated credits yet!   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE SPACE 	 TO WS-ACT.
023350	     MOVE DEB-ACNO	 TO TRA-KEY.

       BA25.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		     No transactions on file?		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF WS-F-ERROR = 5
	       GO TO BA20.

023380 BA30.
023390       PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё   End of transaction file or account number has changed?  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF (WS-F-ERROR = 5) OR
	      (TRA-AC NOT = DEB-ACNO)
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Have credits details been displayed for the previous      Ё
      *    Ё account? Allow User to allocate credits before continuing.Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       IF WS-ACT = "Y"
		   PERFORM CL000
		   DISPLAY WS-BLNK78 AT 0502
			   WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
		   MOVE 1001	 TO CPOS
		   PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 18
		   IF WS-ERROR = 9
		       GO TO BA999
		   END-IF
	       END-IF
	       GO TO BA20.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Is this transaction a credit? If not then go and read the Ё
      *    Ё next transaction.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
           IF TRA-VALUE NOT < ZERO
	       GO TO BA30.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Has the credit value been allocated yet? If yes then go   Ё
      *    Ё and read the next transaction.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF TRA-VALUE < ZERO
               IF (TRA-VALUE + TRA-ALLOCATED) = ZERO
		   GO TO BA30.
	   IF WS-ACT = SPACE
	       MOVE "Y" 	 TO WS-ACT
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Display the account number and name of the account with   Ё
      *    Ё unallocated credit transactions.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       DISPLAY DEB-ACNO	AT 0503
		       WITH FOREGROUND-COLOR 0
			    BACKGROUND-COLOR 7
	       PERFORM BA35 THRU BA60
	       DISPLAY W40-NAME AT 0511
		       WITH FOREGROUND-COLOR 0
			    BACKGROUND-COLOR 7
	       MOVE 10		 TO CLIN.
	     MOVE TRA-CODE	 TO W90-CODE.
	     PERFORM GET-DEBTOR-TRAN-DESC.
023400	     MOVE TRA-DATE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     COMPUTE W25-VALUE = (TRA-VALUE + TRA-ALLOCATED).
	     DISPLAY SD-TRAN AT LINE CLIN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Limit the number of lines used to display unallocated     Ё
      *    Ё credit transactions.				       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF CLIN < 16
	       ADD 1		 TO CLIN.
	     GO TO BA30.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё If the account is for an individual EXTRACT the initials  Ё
      *    Ё and surname and add the title.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       BA35.
021240	   IF DEB-TYPE NOT = 1
021250	       MOVE DEB-NAME	 TO W40-NAME
021260	       GO TO BA60.
021270	     MOVE DEB-NAME	 TO W60-NAME.
021280       PERFORM CA165-NAME.
021320	     MOVE W75-ETITLE (DEB-TITLE)
				 TO W60-NAME.
021330	     MOVE 7		 TO WS-SUB-6.

021340 BA40-TITLE.
021350     IF W60-NCHAR (WS-SUB-6) NOT = SPACE
021360	       GO TO BA45-INIT.
021370	     SUBTRACT 1 	 FROM WS-SUB-6.
021380     IF WS-SUB-6 > 0
021390	       GO TO BA40-TITLE.

021400 BA45-INIT.
021410	     MOVE 1		 TO WS-SUB-5.
021420	     ADD 2		 TO WS-SUB-6.
021430       MOVE W40-NCHAR (25) TO W60-NCHAR (WS-SUB-6).
021440	     ADD 1		 TO WS-SUB-6.
021450	     MOVE "."		 TO W60-NCHAR (WS-SUB-6).
021460     IF W40-NCHAR (49) = SPACE
021470	       GO TO BA50-SURNAME.
021480	     ADD 1		 TO WS-SUB-6.
021490       MOVE W40-NCHAR (49) TO W60-NCHAR (WS-SUB-6).
021500	     ADD 1		 TO WS-SUB-6.
021510	     MOVE "."		 TO W60-NCHAR (WS-SUB-6).

021520 BA50-SURNAME.
021530	     ADD 2		 TO WS-SUB-6.

021540 BA55-LOOP.
021550	     MOVE W40-NCHAR (WS-SUB-5)
				 TO W60-NCHAR (WS-SUB-6).
021560     IF WS-SUB-5 < 24
021570	       ADD 1		 TO WS-SUB-5
021580	       ADD 1		 TO WS-SUB-6
021590	       GO TO BA55-LOOP.
021600	     MOVE W60-NAME	 TO W40-NAME.

       BA60.
	     EXIT.

016080 BA980.
	     MOVE "End of List reached"
				 TO WS-ERR-STRING.
	     PERFORM ERROR-MESSAGE.

016150 BA999.
016160       EXIT.
      /
      *    ****    A L L O C A T E   C R E D I T S 
      *
037050 CL000	       SECTION 51.
037060 CL00.
	     PERFORM SAVE-SCREEN-3.
	     MOVE "Y"		 TO WS-OPTION.

020360 CL05.
	     MOVE "Allocate credits for this Account Y/N? [Y]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	   IF NOT(WS-OPTION = "Y" OR "N")
	       GO TO CL05.
	   IF WS-OPTION = "N"
	       GO TO CL998.
037070       DISPLAY CLR-SCREEN.
             DISPLAY "CREDIT TRANSACTION ALLOCATION" AT 0226
                      WITH FOREGROUND-COLOR 15.
020350	     DISPLAY "Account :" AT 0302.
020370	     MOVE DEB-ACNO	 TO W80-ACNO.
020410	     PERFORM CA155-GET-DEBTOR.
020420     IF WS-ERROR = 1
026950	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020440	       GO TO CL998.
020450     IF WS-ERROR = 2
026980	       MOVE WS-ERR1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020470	       GO TO CL998.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
027020	     MOVE DEB-NAME	 TO W60-NAME.
027030       INSPECT W60-NAME REPLACING FIRST "*" BY ".".
027040       INSPECT W60-NAME REPLACING FIRST "*" BY " ".
027050       INSPECT W60-NAME REPLACING FIRST "!" BY " ".
027060	     DISPLAY W60-NAME AT 0319
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
007520	     DISPLAY "Date" AT 0505
007530		     "Cde/description" AT 0513
007550		     "Ref. No." AT 0529
007560		     "Value" AT 0543.
023350	     MOVE DEB-ACNO	 TO TRA-KEY.

       CL08.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
	       GO TO CL35.

023380 CL10.
023390       PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
              (TRA-AC NOT = DEB-ACNO)
               GO TO CL35.
           IF TRA-VALUE NOT < ZERO
               GO TO CL10.
           IF TRA-VALUE < ZERO
               IF (TRA-VALUE + TRA-ALLOCATED) = ZERO
                   GO TO CL10.
             MOVE TRA-CODE       TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-DESC.
023400	     MOVE TRA-DATE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO W90-DTE.
             MOVE TRA-REF        TO W90-REF.
023410	     DISPLAY W90-DTE AT 0702
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W90-CODE AT 0713
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023420	     DISPLAY W00-E-DESC AT 0716
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023440	     DISPLAY TRA-REF AT 0729
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023450       MOVE TRA-VALUE      TO W95-B.
023460	     DISPLAY W95-V2 AT 0738
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             COMPUTE W90-CR = (TRA-VALUE + TRA-ALLOCATED) * -1.
             MOVE TRA-RECORD1    TO W91-TRAN.
	     MOVE ZERO		 TO W90-ALLOCATED W90-DISCOUNT.
             DISPLAY S08A.
             MOVE 19             TO CLIN.
             PERFORM ALLOCATE-CREDIT.
           IF (WS-ERROR = 9) OR
              (W75-ALLOCATED = W91-ALLOCATED)
               GO TO CL30.
021960       PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
021970       PERFORM AY02 THRU AY59.
029570       MOVE ZERO           TO W90-90 W90-120 W90-CUR 
029580                              W90-60 W90-INT W90-30.

       CL15.
           IF W75-KEY < WS-DBTKEY
               GO TO CL25.
             PERFORM READ-DEBTRN THRU READ-DEBTRN-EXIT.
           IF DBT-UPDATE = SPACE
               GO TO CL20.
             ADD DBT-ALLOC       TO DBT-PAID.
           IF DBT-VALUE > DBT-PAID
               MOVE "X"          TO DBT-TYPE
           ELSE
               MOVE "P"          TO DBT-TYPE.
           IF DBT-CODE = 20
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-INT
               ADD DBT-ALLOC     TO W90-INT
           ELSE
           IF DBT-AGE = 5
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-120
               ADD DBT-ALLOC     TO W90-120
           ELSE
           IF DBT-AGE = 4
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-90
               ADD DBT-ALLOC     TO W90-90
           ELSE
           IF DBT-AGE = 3
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-60
               ADD DBT-ALLOC     TO W90-60
           ELSE
           IF DBT-AGE = 2
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-30
               ADD DBT-ALLOC     TO W90-30
           ELSE
           IF DBT-AGE = 1
001200         SUBTRACT DBT-ALLOC
001210                           FROM DEB-CUR
               ADD DBT-ALLOC     TO W90-CUR.
             MOVE DBT-KEY        TO TRA-KEY.
             PERFORM READ-DBTRAN THRU READ-DBTRAN-EXIT.
             MOVE 0              TO WS-ACTION.
             PERFORM AY04 THRU AY59.
             MOVE DBT-TRA        TO TRA-RECORD1.
             MOVE W90-REF        TO TRA-REC.
             PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
       CL20.
             ADD 1               TO WS-DBTKEY.
             GO TO CL15.
       CL25.
             MOVE W91-TRAN       TO TRA-RECORD1.
             MOVE 0              TO WS-ACTION.
             PERFORM AY04 THRU AY59.
             ADD W75-ALLOCATED   TO TRA-ALLOCATED.
             PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
040490       PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
031380       SUBTRACT W90-120    FROM DEB-FOUR.
032240       SUBTRACT W90-90     FROM DEB-THREE.
032250       SUBTRACT W90-60     FROM DEB-TWO.
032260       SUBTRACT W90-30     FROM DEB-ONE.
032270       SUBTRACT W90-CUR    FROM DEB-MTD.
031830       SUBTRACT W90-INT    FROM DEB-INTEREST.
040600       PERFORM AY70 THRU AY999.
       CL30.
             CLOSE DEBTRN.

       CL32.
	     MOVE "'C'ontinue or 'E'xit  [C]"
				 TO WS-ERR-MES.
             MOVE "C"            TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "C" OR "E")
               GO TO CL32.
           IF WS-OPTION = "E"
	       COMMIT
               GO TO CL00.
             MOVE W91-KEY        TO TRA-KEY.             
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CL35.
023390       PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
             GO TO CL10.

       CL35.
	     COMMIT.
	     MOVE "No credits to allocate"
				 TO WS-ERR-STRING.
	     PERFORM ERROR-MESSAGE.
	     PERFORM RESTORE-SCREEN-3.

       CL998.
	     MOVE "Continue with Allocate credits Y/N? [Y]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	   IF NOT(WS-OPTION = "Y" OR "N")
	       GO TO CL998.
	   IF WS-OPTION = "N"
	       MOVE 9		 TO WS-ERROR.

       CL999.
             EXIT.
      /
025020 ZA00-INIT              SECTION 90.
025030 ZA00-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
042000	     DISPLAY CLR-SCREEN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё D o   n o t   d i s p l a y   E R R O R   m e s s a g e s Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *	     MOVE 44		 TO USER-NUMBER.
      *	     MOVE 1		 TO USER-SETTING.
      *	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   S e t   u p	 t h e	 F U N C T I O N   k e y s     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		       ESC, F1 to F10 keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		    PAGE-UP and PAGE-DOWN keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      UP and DOWN ARROW keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 2		 TO USER-ACTION.
	     MOVE 3		 TO USER-SETTING.
	     MOVE 5		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	    Pre-display ACCEPT fields before DATA-ENTRY	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *      MOVE 76		 TO USER-NUMBER.
      *      MOVE 3		 TO USER-SETTING.
      *      MOVE 1		 TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   A C T I V A T E   M O U S E		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
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
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       Set MOUSE key to act as FUNCTION key	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	    Setup the file ID's including path names           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
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
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E    ****
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
      *    *************************************************************
      *    *  D E B T O R   T R A N S A C T I O N   W O R K   F I L E  *
      *    *************************************************************
	     MOVE "DEBTRN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-DEBTRN.
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

       ZA02.
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.

025620 ZA25.
025630	     MOVE 1		 TO WS-PARKEY.
025640       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025650	     MOVE PAR-COMPANY	 TO W95-COMP.
020490	     MOVE PAR-TELEPHONE  TO W95-TEL.
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
039790	     DISPLAY "Too many files OPEN" AT 0708
039800                WITH FOREGROUND-COLOR 14.
039810	     DISPLAY "Check the FILES option in CONFIG.SYS (Dos,Windows
      -		     "3/95/98/NT/2000)" AT 0908.
	     DISPLAY "CONFIG.NT  (Windows XP)" AT 1034.
	     DISPLAY "SYSTEM.INI (Windows ME)" AT 1134.
039820	     DISPLAY "Press any key to continue" AT 1308.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
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
	       MOVE "Supervisorд"  TO WS-WRKHD
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
025940 ZB000                 SECTION 91.
      
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
041850	   IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 31
041860         MOVE W02-DEBTRN   TO WS-FILE
041870	       MOVE WS-DBTKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.
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
