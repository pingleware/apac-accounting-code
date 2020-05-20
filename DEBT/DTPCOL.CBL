      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	 ******    ******   **	       *
      *   **	**     **     **    **	**    **  **	**  **	       *
      *   **	**     **     **    **	**	  **	**  **	       *
      *   **	**     **     *******	**	  **	**  **	       *
      *   **	**     **     **	**	  **	**  **	       *
      *   **	**     **     **	**    **  **	**  **	       *
      *   *******      **     **	 ******	   ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R ' S   C O L L E C T I O N S   P R O G R A M    *
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
000020 PROGRAM-ID.     DTPENQ.
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
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY CONTROL.SL.

000160 COPY DBTRAN.SL.

000140 COPY DEBCOL.SL.

000140 COPY DEBTOR.ISL.

000140 COPY DEBMEM.SL.

000120 COPY PARAM.SL.

000240     SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000330 COPY DBTRAN.FDE.

000310 COPY DEBCOL.FDE.

000310 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

000290 COPY PARAM.FDE.

      /
000410 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
000430 01  R-L1.
000440     03  R-DET1          PIC  X(132).

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
001550 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001560 77  WS-SUB	   PIC	9(04)	 COMP-5.
001560 77  WS-S            PIC  9(04)    COMP-5.
001570 77  WS-S1           PIC  9(04)    COMP-5.
001580 77  WS-S2           PIC  9(04)    COMP-5.
001590 77  WS-S3           PIC  9(04)    COMP-5.
001600 77  WS-S4           PIC  9(04)    COMP-5.
001610 77  WS-S5           PIC  9(04)    COMP-5.
001620 77  WS-S6           PIC  9(04)    COMP-5.
001630 77  WS-S7           PIC  9(04)    COMP-5.
001640 77  WS-S8           PIC  9(04)    COMP-5.
001650 77  WS-S9           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001800 77  WS-PAGE	   PIC	9(04)	 COMP-5.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003650 77  WS-COND	   PIC	X(06).
003660 77  WS-NORM	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
001810 77  WS-KEYSTR	   PIC	9(06)	 COMP-5.
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-ETYPE	   PIC	X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
002010 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
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

002230 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "ENQ".

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

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DBTRAN.ID.

       COPY DEBCOL.ID.

       COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

       COPY PARAM.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "DTPCOL.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY W12.WS.

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
             05  W15-TRAN OCCURS 15.
               07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
               07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
	       07  W15-REC     PIC  X(08).
           03  W15-DISPLAY REDEFINES W15-DETAIL.
               05  W15-BAL1    PIC  X(11).
               05  W15-ALL1    PIC  X(11).
	       05  W15-REC1    PIC  X(08).
               05  W15-BAL2    PIC  X(11).
               05  W15-ALL2    PIC  X(11).
	       05  W15-REC2    PIC  X(08).
               05  W15-BAL3    PIC  X(11).
               05  W15-ALL3    PIC  X(11).
	       05  W15-REC3    PIC  X(08).
               05  W15-BAL4    PIC  X(11).
               05  W15-ALL4    PIC  X(11).
	       05  W15-REC4    PIC  X(08).
               05  W15-BAL5    PIC  X(11).
               05  W15-ALL5    PIC  X(11).
	       05  W15-REC5    PIC  X(08).
               05  W15-BAL6    PIC  X(11).
               05  W15-ALL6    PIC  X(11).
	       05  W15-REC6    PIC  X(08).
               05  W15-BAL7    PIC  X(11).
               05  W15-ALL7    PIC  X(11).
	       05  W15-REC7    PIC  X(08).
               05  W15-BAL8    PIC  X(11).
               05  W15-ALL8    PIC  X(11).
	       05  W15-REC8    PIC  X(08).
               05  W15-BAL9    PIC  X(11).
               05  W15-ALL9    PIC  X(11).
	       05  W15-REC9    PIC  X(08).
               05  W15-BAL10   PIC  X(11).
               05  W15-ALL10   PIC  X(11).
	       05  W15-REC10   PIC  X(08).
               05  W15-BAL11   PIC  X(11).
               05  W15-ALL11   PIC  X(11).
	       05  W15-REC11   PIC  X(08).
               05  W15-BAL12   PIC  X(11).
               05  W15-ALL12   PIC  X(11).
	       05  W15-REC12   PIC  X(08).
               05  W15-BAL13   PIC  X(11).
               05  W15-ALL13   PIC  X(11).
	       05  W15-REC13   PIC  X(08).
               05  W15-BAL14   PIC  X(11).
               05  W15-ALL14   PIC  X(11).
	       05  W15-REC14   PIC  X(08).
               05  W15-BAL15   PIC  X(11).
               05  W15-ALL15   PIC  X(11).
	       05  W15-REC15   PIC  X(08).

002730 01  W22-DATES.
002740	   03  W22-DTE1        PIC  9(08).
002760	   03  W22-DATE1 REDEFINES W22-DTE1.
	       05  W22-CC1     PIC  9(02).
002770	       05  W22-YY1     PIC  9(02).
002780	       05  W22-MM1     PIC  9(02).
002790	       05  W22-DD1     PIC  9(02).
002760	   03  W22-DATE1B REDEFINES W22-DTE1.
	       05  W22-CY2     PIC  9(04).
	       05  FILLER      PIC  9(04).
002800	   03  W22-DTE2        PIC  9(08).
002810	   03  W22-DATE2 REDEFINES W22-DTE2.
002820	       05  W22-DD2     PIC  9(02).
002830	       05  W22-MM2     PIC  9(02).
	       05  W22-CC2     PIC  9(02).
002840	       05  W22-YY2     PIC  9(02).

002850 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.

       COPY W40.DBT.

       01  W40-BALANCES.
           03  W40-A-MTD       PIC S9(09)V99 COMP-3.
003390     03  W40-A-CUR       PIC S9(09)V99 COMP-3.
003400     03  W40-A-30        PIC S9(09)V99 COMP-3.
003410     03  W40-A-60        PIC S9(09)V99 COMP-3.
003420     03  W40-A-90        PIC S9(09)V99 COMP-3.
003420     03  W40-A-120       PIC S9(09)V99 COMP-3.
003430     03  W40-A-INT       PIC S9(09)V99 COMP-3.   
           03  W40-D-MTD       PIC S9(09)V99 COMP-3.
003390     03  W40-D-CUR       PIC S9(09)V99 COMP-3.
003400     03  W40-D-30        PIC S9(09)V99 COMP-3.
003410     03  W40-D-60        PIC S9(09)V99 COMP-3.
003420     03  W40-D-90        PIC S9(09)V99 COMP-3.
003420     03  W40-D-120       PIC S9(09)V99 COMP-3.
003430     03  W40-D-INT       PIC S9(09)V99 COMP-3.   
           03  W40-T-MTD       PIC S9(09)V99 COMP-3.
003390     03  W40-T-CUR       PIC S9(09)V99 COMP-3.
003400     03  W40-T-30        PIC S9(09)V99 COMP-3.
003410     03  W40-T-60        PIC S9(09)V99 COMP-3.
003420     03  W40-T-90        PIC S9(09)V99 COMP-3.
003420     03  W40-T-120       PIC S9(09)V99 COMP-3.
003430     03  W40-T-INT       PIC S9(09)V99 COMP-3.   

       COPY W40.WS.

003890 01  W45-TRAN.
003900     03  W45-CODE        PIC 9(02).
003910     03  W45-ENG         PIC X(12).
003920     03  W45-AFR         PIC X(12).
003930     03  W45-ACT         PIC X(01).

       COPY FUNCTION.WS.

003530 COPY W50.WS.

003550 01  W60-NME-ADD.
003560     03  W60-NAME.
003570         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.

003750 01  W75-TITLES.
003760     03  W75-DESCRIP.
003770         05  FILLER      PIC X(28)      VALUE
003780                     "Mr     Mnr    Mrs    Mev    ".
003790         05  FILLER      PIC X(28)      VALUE
003800                     "Miss   Mej    Ms     Ms     ".
003810         05  FILLER      PIC X(28)      VALUE
003820                     "Dr     Dr     Rev    Ds     ".
003830         05  FILLER      PIC X(28)      VALUE
003840                     "Prof   Prof   The HonSy Edel".
003850         05  FILLER      PIC X(28)      VALUE
003860                     "                            ".
003870     03  W75-CODES REDEFINES W75-DESCRIP.
003880         05  W75-TITLE  OCCURS 10.
003890             07  W75-ETITLE  PIC X(07).
003900             07  W75-ATITLE  PIC X(07).

003910 01  W80-ACCOUNT.
003920     03  W80-ACNO        PIC X(06).
003940     03  W80-TYPE        PIC 9(01).

003980 01  W90-TRAN.
004000	   03  W90-DTE	       PIC  99/99/9999.
004020     03  W90-CODE        PIC  9(02).
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.
004020     03  W90-CATKEY      PIC  9(04)    COMP-5.
004020     03  W90-CS          PIC  9(02)    COMP-5.
004030	   03  W90-CDESC       PIC  X(30).
004080	   03  W90-SMAN        PIC  X(30).
	   03  W90-TERMS       PIC  X(12).
	   03  W90-TYPE        PIC  X(12).
	   03  W90-PRICE       PIC  X(12).
	   03  W90-120	       PIC S9(09)V99 COMP-3.
004090     03  W90-90          PIC S9(09)V99 COMP-3.
004100     03  W90-60          PIC S9(09)V99 COMP-3.
004110     03  W90-30          PIC S9(09)V99 COMP-3.
004120     03  W90-CUR         PIC S9(09)V99 COMP-3.
004130     03  W90-BAL         PIC S9(09)V99 COMP-3.
004380     03  W90-INT         PIC S9(09)V99 COMP-3.

004390 01  W95-STM.
004400     03  W95-COMP        PIC X(40).
004410     03  W95-V1.
004420         05  W95-T       PIC Z(04)9.99.
004430     03  W95-V2.
004440         05  W95-B       PIC Z(06)9.99-.
004450     03  W95-D.
004460	       05  W95-DTE     PIC Z9/99/9999.
004470     03  W95-POST        PIC 9(04).
004480     03  W95-TEL         PIC X(08).

004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99.
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

       01  W200-KEYS.
	   03  W200-KEY        PIC  X(10) OCCURS 100.

       01  W205-MEMO.
	   03  W205-MEM-DET.
	       05  W205-MEM    PIC  X(60) OCCURS 100.
	   03  W205-S	       PIC  9(04) COMP-5.
	   03  W205-S1	       PIC  9(04) COMP-5.
	   03  W205-S2	       PIC  9(04) COMP-5.
	   03  W205-LAST       PIC  9(04) COMP-5.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

005070 01  S08.
005080	   03  LINE  2 COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
005090				 VALUE "DEBTOR ENQUIRY".
005100     03  LINE  3 COLUMN  2 VALUE "Account No:".
005110     03          COLUMN 21 VALUE "Name:".
005120	   03	       COLUMN 66 VALUE "°°Language:°°°".
005130     03  LINE  4 COLUMN  2 VALUE "Address   :".
005140     03  LINE  5 COLUMN  2 VALUE "Telephone :".
           03          COLUMN 33 VALUE "Contact :".
005150     03  LINE  6 COLUMN  2 VALUE "Fax number:".
005160     03          COLUMN 33 VALUE "Purchases  YTD :".
005170     03          COLUMN 62 VALUE "Prev :".
005180	   03  LINE  7 COLUMN  5 VALUE "Date".
005190	   03	       COLUMN 13 VALUE "Description".
005200	   03	       COLUMN 26 VALUE "Ref No".
005210	   03	       COLUMN 40 VALUE "Value".

005220 01  S09.
005230     03  LINE  7 COLUMN 48 PIC X(01) USING WS-G3.
005240     03          COLUMN 51 VALUE "Opening balance :".
005250     03  LINE  8 COLUMN 48 PIC X(01) USING WS-G3.
005260     03          COLUMN 51 VALUE "Debits          :".
005270     03  LINE  9 COLUMN 48 PIC X(01) USING WS-G3.
005280     03          COLUMN 51 VALUE "Credits         :".
005290     03  LINE 10 COLUMN 48 PIC X(01) USING WS-G3.
005300     03          COLUMN 51 VALUE "Closing balance :".
005310     03  LINE 11 COLUMN 48 PIC X(01) USING WS-G3.
005320     03  LINE 12 COLUMN 48 PIC X(01) USING WS-G3.
005330     03          COLUMN 51 VALUE "Last active     :".
005340     03  LINE 13 COLUMN 48 PIC X(01) USING WS-G3.
005330     03          COLUMN 51 VALUE "Last payment    :".
005350     03  LINE 14 COLUMN 48 PIC X(01) USING WS-G3.
005360	   03	       COLUMN 51 VALUE "Age Analysis     ".
005370     03  LINE 15 COLUMN 48 PIC X(01) USING WS-G3.
005380     03          COLUMN 51 VALUE "    Current     :".
005390     03  LINE 16 COLUMN 48 PIC X(01) USING WS-G3.
005400     03          COLUMN 51 VALUE "    30 days     :".
005410     03  LINE 17 COLUMN 48 PIC X(01) USING WS-G3.
005420     03          COLUMN 51 VALUE "    60 days     :".
005430     03  LINE 18 COLUMN 48 PIC X(01) USING WS-G3.
005440     03          COLUMN 51 VALUE "    90 days     :".
005450     03  LINE 19 COLUMN 48 PIC X(01) USING WS-G3.
005460     03          COLUMN 51 VALUE "    120 days +  :".
005470     03  LINE 20 COLUMN 48 PIC X(01) USING WS-G3.
005460     03          COLUMN 51 VALUE "    Interest    :".
005480     03  LINE 21 COLUMN 48 PIC X(01) USING WS-G3.
005490     03          COLUMN 51 VALUE "Arrears         :".
005500     03  LINE 22 COLUMN 48 PIC X(01) USING WS-G3.
005510     03          COLUMN 51 VALUE "Credit limit    :".

       01  S09A.
           03  LINE  7 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE  8 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE  9 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 10 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 11 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 12 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 13 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 14 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 15 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 16 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 17 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 18 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 19 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 20 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 21 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE 22 COLUMN 45 BACKGROUND-COLOR 7
				 PIC  X(34) FROM W40-SPACES.
           03  LINE  7 COLUMN 47 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0 VALUE "Allocated".
           03          COLUMN 61 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0 VALUE "Balance".
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE "Ref. No.".
           03  LINE  8 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL1.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL1.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC1.
           03  LINE  9 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL2.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL2.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC2.
	   03  LINE 10 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL3.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL3.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC3.
	   03  LINE 11 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL4.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL4.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC4.
	   03  LINE 12 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL5.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL5.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC5.
	   03  LINE 13 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL6.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL6.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC6.
	   03  LINE 14 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL7.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL7.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC7.
	   03  LINE 15 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL8.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL8.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC8.
	   03  LINE 16 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL9.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL9.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC9.
	   03  LINE 17 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL10.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL10.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC10.
	   03  LINE 18 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL11.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL11.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC11.
	   03  LINE 19 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL12.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL12.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC12.
	   03  LINE 20 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL13.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL13.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC13.
	   03  LINE 21 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL14.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL14.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC14.
	   03  LINE 22 COLUMN 46 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-ALL15.
           03          COLUMN 58 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 PIC  X(11) FROM W15-BAL15.
	   03	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0
				 PIC  X(08) FROM W15-REC15.

       01  S09B.
           03  LINE  7 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-TOP.
           03  LINE  8 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE  9 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 10 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 11 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 12 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 13 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 14 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 15 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 16 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 17 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 18 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 19 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 20 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-MID.
           03  LINE 21 COLUMN 51 BACKGROUND-COLOR 7
                                 PIC  X(28) FROM W40-BOT.
           03  LINE  8 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "PURCHASES LAST 12 MONTHS".
           03  LINE  9 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "January   :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(1).
           03  LINE 10 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "February  :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(2).
           03  LINE 11 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "March     :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(3).
           03  LINE 12 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "April     :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(4).
           03  LINE 13 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "May       :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(5).
           03  LINE 14 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "June      :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(6).
           03  LINE 15 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "July      :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(7).
           03  LINE 16 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "August    :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(8).
           03  LINE 17 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "September :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(9).
           03  LINE 18 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "October   :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(10).
           03  LINE 19 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "November  :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(11).
           03  LINE 20 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "December  :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DEB-MTHPUR(12).
       01  S10.
	   03  LINE  8 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  9 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ Delivery Address : ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(30) USING W40-PADD1.
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE " ³".
	   03  LINE 10 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³                    ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(30) USING W40-PADD2.
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE " ³".
	   03  LINE 11 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³                    ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(30) USING W40-PADD3.
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE " ³".
	   03  LINE 12 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³                    ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(30) USING W40-PADD4.
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE " ³".
	   03  LINE 13 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ Postal Code        ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  9(04) USING W40-PPOST.
	   03	       COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				  "                           ³".
	   03  LINE 14 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ Category       ".
	   03	       COLUMN 22 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 PIC  99B USING DEB-CAT.
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(30) USING W90-CDESC.
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE " ³".
	   03  LINE 15 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ Terms          ".
	   03	       COLUMN 22 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 PIC  X(03) USING DEB-TERMS.
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(12) USING W90-TERMS.
	   03	       COLUMN 37 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				 "                   ³".
	   03  LINE 16 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ Price Code     ".
	   03	       COLUMN 22 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 PIC  X(03) USING DEB-PRICE.
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(12) USING W90-PRICE.
	   03	       COLUMN 37 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				 "                   ³".
	   03  LINE 17 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ Sales Person   ".
	   03	       COLUMN 22 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 PIC  99B USING DEB-SMAN.
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(30) USING W90-SMAN.
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE " ³".
	   03  LINE 18 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ Invoice Type   ".
	   03	       COLUMN 22 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 PIC  X(03) USING DEB-INV-TYPE.
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(12) USING W90-TYPE.
	   03	       COLUMN 37 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				 "                   ³".
	   03  LINE 19 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

009340 01  S-DEB.
009350	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
009360				 VALUE "DEBTOR COLLECTION LOOKUP".
009370	   03  LINE  3 COLUMN  2 VALUE "Start No/Name:".
009380	   03	       COLUMN 23 VALUE	"/".
009390     03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE 
009400	       "Ac/No  Customer name                            Tot.Bala
009410-        "nce  Past Due  T  Days".
009160	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÑÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÏÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009440     03  LINE 23 COLUMN  2 FOREGROUND-COLOR 7 VALUE 
009450       "Key in at least the first character of the name or press".
009460	   03	       COLUMN 59 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Esc".
009470     03          COLUMN 62 FOREGROUND-COLOR 7 VALUE " to exit".
009480	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1 VALUE "Pressing '".
009490	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "ENTER".
009500	   03	       COLUMN 17 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1 VALUE
009510                           "' key only, will start from the beginn
009520-                          "ing of the debtor file".
009530 01  SA-ACT.
009540	   03  LINE  3 COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009550				 PIC  X(06) USING W80-ACNO AUTO.
009530 01  SA-DEB.
009540	   03  LINE  3 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009550                           PIC  X(40) USING W40-NAME AUTO.

       01  S-COL.
	   03  LINE  3 COLUMN  2 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "ACNO " .
	   03	       COLUMN  7 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(06) FROM DEB-ACNO.
	   03	       COLUMN 13 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE " NAME ".
	   03	       COLUMN 19 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(40) FROM W40-NAME.
	   03	       COLUMN 60 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "  TEL ".
	   03	       COLUMN 66 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(14) FROM W40-TEL.
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE " MTD ".
	   03	       COLUMN  7 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99- FROM W40-A-MTD.
	   03	       COLUMN 20 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "   CUR ".
	   03	       COLUMN 27 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99- FROM W40-A-CUR.
	   03	       COLUMN 40 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "   30D ".
	   03	       COLUMN 47 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99- FROM W40-A-30.
	   03	       COLUMN 60 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "  60D ".
	   03	       COLUMN 66 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZZ9.99- FROM W40-A-60.
	   03  LINE  5 COLUMN  2 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE " 90D ".
	   03	       COLUMN  7 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99- FROM W40-A-90.
	   03	       COLUMN 20 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "  120D ".
	   03	       COLUMN 27 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99- FROM W40-A-120.
	   03	       COLUMN 40 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "   INT ".
	   03	       COLUMN 47 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9.99- FROM W40-A-INT.
	   03	       COLUMN 60 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2 VALUE "  DUE ".
	   03	       COLUMN 66 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZZ9.99- FROM W40-D-MTD.
	   03  LINE  6 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
	   03	       COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
	   03	       COLUMN 78 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
	   03  LINE  7 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
	   03	       COLUMN 18 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	   "   C O L L E C T I O N   N O T E S   A M E N D / V I E W
      -    "".
	   03	       COLUMN 78 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
	   03  LINE  8 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
	   03	       COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
	   03	       COLUMN 78 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
	   03  LINE  9 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
	   03  LINE 10 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
				 PIC X(62) USING WS-MM-LINE.
	   03  LINE 11 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
	   03  LINE 12 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
	   03  LINE 13 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
	   03  LINE 14 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
	   03  LINE 15 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
	   03  LINE 16 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
	   03  LINE 17 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
	   03  LINE 18 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
	   03  LINE 19 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
	   03  LINE 20 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
	   03  LINE 21 COLUMN 17 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
	   03	       COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
	   03	       COLUMN 78 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.
       01  SA-COL.
	   03  LINE  9 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
	   03  LINE 10 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
	   03  LINE 11 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
	   03  LINE 12 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
	   03  LINE 13 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
	   03  LINE 14 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
	   03  LINE 15 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
	   03  LINE 16 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
	   03  LINE 17 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
	   03  LINE 18 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
	   03  LINE 19 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
	   03  LINE 20 COLUMN 18 FOREGROUND-COLOR 5
				 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.


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

004870 COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
003240	   IF LS0-DBLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010270	     PERFORM ZA000-INIT.
	     PERFORM BA000.
	     CLOSE DEBCOL.

       AA49.
013150	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

011670 COPY CONTROL.RD.

       COPY DBTRAN.RD.

011710 COPY DEBCOL.RD.

011710 COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

011750 COPY PARAM.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY DEBCOL.WR.

      *
      *    G E T   A   D E B T O R   T R A N S A C T I O N   R E C O R D
      *
      *    PERFORM THIS ROUTINE WITH THE TRANSACTION CODE IN W90-CODE
      *
      *    ON EXITING THE TRANSACTION CODE DETAILS ARE IN W00-
      *    WHILE W90-TRNKEY CONTAINS THE KEY TO THE PARAMETER FILE
      *    WHERE THIS TRANSACTION CODE IS FOUND AND W90-TS IS THE
      *    TRANSACTION CODE SUBSCRIPT IN THE RECORD.
      *
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
000150	     MOVE PAR-E-DESC(W90-TS)
000160                           TO W00-E-DESC.
000170	     MOVE PAR-A-DESC(W90-TS)
000180                           TO W00-A-DESC.
000190	     MOVE PAR-ACTION(W90-TS)
000200                           TO W00-ACTION.
000210	     MOVE PAR-T-CODE(W90-TS)
000220                           TO W00-T-CODE.
000230	     MOVE PAR-T-GL(W90-TS)
000240                           TO W00-T-GL.

000260 STORE-DEBTOR-TRAN-STATS.
000270	     MOVE PAR-T-VAT(W90-TS)
000280                           TO W00-T-VAT.
000290	     MOVE PAR-T-DAY(W90-TS)
000300                           TO W00-T-DAY.
000310	     MOVE PAR-T-VAL(W90-TS)
000320                           TO W00-T-VAL.
000330	     MOVE PAR-T-MTD(W90-TS)
000340                           TO W00-T-MTD.
000350	     MOVE PAR-T-MTDV(W90-TS)
000360                           TO W00-T-MTDV.
000370	     MOVE PAR-T-YTD(W90-TS)
000380                           TO W00-T-YTD.
000390	     MOVE PAR-T-YTDV(W90-TS)
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

      /
      *    ****    D E B T O R	 C O L L E C T I O N   L O O K U P
      *
000010 DEBTOR-LOOKUP           SECTION.

000030 DEBT-LUP-SCREEN.
000040       DISPLAY CLR-SCREEN. 
000050	     DISPLAY S-DEB.
             MOVE SPACE          TO WS-OPTION.

       DEBT-LUP-CLR-SCREEN.
000060       MOVE 0601           TO CPOS.
000080       PERFORM DEBT-CRT-EDGE UNTIL CLIN = 22.
000090	     MOVE SPACES	 TO W40-NAME W80-ACNO.
	     MOVE "N"		 TO WS-ETYPE.

000110 DEBT-LUP-NAME.
000120	     ACCEPT SA-ACT.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
000140		     MOVE SPACES TO W80-ACNO
000150		     GO TO DEBTOR-LOOKUP-EXIT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DEBT-LUP-NAME.
	   IF NOT (W80-ACNO = SPACES)
	       GO TO DEBT-LUP-CONTINUE.
000120	     ACCEPT SA-DEB.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
000140		     MOVE SPACES TO W80-ACNO
000150		     GO TO DEBTOR-LOOKUP-EXIT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DEBT-LUP-NAME.

       DEBT-LUP-CONTINUE.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
000170	     DISPLAY CLEAR-L25.
000180       MOVE 6              TO CLIN.
000200       MOVE 1              TO W75-S W75-S1.

       DEBT-LUP-TAB-LOOP.
	     MOVE SPACES	 TO W75-DNO(W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
               GO TO DEBT-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       DEBT-LUP-ENTRY-TYPE.
000470	   IF (W40-NAME = SPACES) AND (W80-ACNO = SPACES)
	       MOVE " !"	 TO W40-NAME
	   ELSE
	   IF W40-NAME = SPACES
000640	       MOVE "A" 	 TO WS-ETYPE
000660	       MOVE 6		 TO WS-LENGTH
	       CALL "CBL_TOUPPER" USING W80-ACNO
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       MOVE 1		 TO WS-LENGTH
	   ELSE
	       MOVE 40		 TO WS-LENGTH
	       CALL "CBL_TOUPPER" USING W40-NAME
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       MOVE 1		 TO WS-LENGTH.
	   IF WS-ETYPE = "N"
	       MOVE W40-NAME	 TO DEB-NAME
	       PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT
	   ELSE
	       MOVE W80-ACNO	 TO DEB-ACNO
	       PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO DEBT-NO-RECORD.

000810 DEBT-GET-DEBTOR-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
               GO TO DEBT-SKIP-DEBT-READ.
000840       PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               SUBTRACT 1        FROM W75-S
               GO TO DEBT-FULL-SCREEN.

       DEBT-SKIP-DEBT-READ.
	     MOVE DEB-ACNO	 TO W75-DNO(W75-S).
000850       MOVE DEB-NAME       TO W40-NAME.
000860     IF DEB-TYPE = 1
000870         INSPECT W40-NAME REPLACING ALL "*" BY " "
000880         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
000890	     DISPLAY DEB-ACNO AT LINE CLIN COLUMN 2
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
000900	     DISPLAY W40-NAME AT LINE CLIN COLUMN 9
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
000910       MOVE DEB-BAL        TO W100-LBAL.
000920	     DISPLAY W100-LBAL AT LINE CLIN COLUMN 50
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	   IF DEB-TERMS = 0 OR 3
000930	       COMPUTE W100-LARR = DEB-30 + DEB-60 + DEB-90 + DEB-120
				 + DEB-INT
	   ELSE
	   IF DEB-TERMS = 1
000930	       COMPUTE W100-LARR = DEB-60 + DEB-90 + DEB-120 + DEB-INT
	   ELSE
	   IF DEB-TERMS = 2
000930	       COMPUTE W100-LARR = DEB-90 + DEB-120 + DEB-INT.
000940	     DISPLAY W100-LARR AT LINE CLIN COLUMN 62
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
000960	     DISPLAY DEB-TERMS AT LINE CLIN COLUMN 73
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	   IF DEB-120 > ZERO
	       DISPLAY "120" AT LINE CLIN COLUMN 76
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
	   ELSE
	   IF DEB-90 > ZERO
	       DISPLAY " 90" AT LINE CLIN COLUMN 76
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
	   ELSE
	   IF DEB-60 > ZERO
	       DISPLAY " 60" AT LINE CLIN COLUMN 76
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
	   ELSE
	   IF DEB-30 > ZERO
	       DISPLAY " 30" AT LINE CLIN COLUMN 76
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
	   ELSE
	       DISPLAY "CUR" AT LINE CLIN COLUMN 76
			WITH FOREGROUND-COLOR 3 HIGHLIGHT.
000970     IF CLIN < 21
000980         ADD 1             TO CLIN W75-S
000990         GO TO DEBT-GET-DEBTOR-REC.
001000
001010 DEBT-FULL-SCREEN.
001020	     DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 6 HIGHLIGHT
001030		     " - " WITH FOREGROUND-COLOR 7
		     "PgUp" WITH FOREGROUND-COLOR 6 HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " - " WITH FOREGROUND-COLOR 7
001040		     "ENTER" WITH FOREGROUND-COLOR 6 HIGHLIGHT
001050		     " view - " WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape - " WITH FOREGROUND-COLOR 7
		     "F8" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " Collect.Notes - "
		      WITH FOREGROUND-COLOR 7
		     "F9" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " memo/remarks"
		      WITH FOREGROUND-COLOR 7.

       DEBT-SAVE-FULL-SCREEN.
	     PERFORM SAVE-SCREEN-2.
001140	     MOVE 0		 TO W75-S1.
001150       MOVE 0601           TO CPOS.

001180 DEBT-CRT-ATTRIB.
	     MOVE 1920		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
001220	     MOVE 2000		 TO W41-LENGTH.

001240 DEBT-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
001320		     GO TO DEBT-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO DEBT-CHK-NO-KEYS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     MOVE WS1-CURS
				 TO CRT-LINE
		     PERFORM CD000
		     MOVE CRT-LINE
				 TO WS1-CURS
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DEBT-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO DEBTOR-LOOKUP-EXIT
		 WHEN PAGE-DOWN
001340		     GO TO DEBT-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO DEBT-GET-PREV-PAGE
		 WHEN F8-KEY
		     PERFORM DEBT-COLLECTIONS THRU DEBT-COL-EXIT
		     PERFORM DEBT-FULL-SCREEN
001510		     GO TO DEBT-GET-USER-OPT
		 WHEN F9-KEY
		     PERFORM DEBT-ADD-SPECS THRU DEBT-ADD-EXIT
		     PERFORM DEBT-FULL-SCREEN
001510		     GO TO DEBT-GET-USER-OPT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DEBT-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO DEBT-GET-USER-OPT.

001240 DEBT-GET-NEXT-PAGE.
001480     IF W75-S < 16
001490         GO TO DEBT-GET-USER-OPT.

       DEBT-NEW-PAGE.
001500       MOVE 0601           TO CPOS.
001520       PERFORM DEBT-CRT-EDGE UNTIL CLIN = 22.
001530       MOVE 6              TO CLIN.
001540       MOVE LOW-VALUES     TO W75-KEYS.
001550       MOVE 1              TO W75-S W75-S1.
             PERFORM DEBT-LUP-TAB-LOOP.
001560       GO TO DEBT-GET-DEBTOR-REC.

       DEBT-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       DEBT-GET-PREV-LOOP.
             PERFORM READ-DEBTOR-PREV THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
	       MOVE SPACE	 TO WS-OPTION
               GO TO DEBT-NEW-PAGE.
           IF WS-IXS1 < 32
               ADD 1             TO WS-IXS1
               GO TO DEBT-GET-PREV-LOOP.
             GO TO DEBT-NEW-PAGE.

001580 DEBT-NO-RECORD.
001590	     DISPLAY "No record - " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001600		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001610		     "ext or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001620		     "Q" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001630		     "uit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
001640
001650 DEBT-GET-USER-OPT-2.
001660	     ACCEPT WS-OPTION AT 2538
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
001720	   IF NOT (WS-OPTION = "N" OR "Q")
001730         GO TO DEBT-GET-USER-OPT-2.
001740     IF WS-OPTION = "N"
001750         GO TO DEBT-LUP-SCREEN.
001760       MOVE SPACES         TO W80-ACNO.
001770	     GO TO DEBTOR-LOOKUP-EXIT.

001790 DEBT-ADJ-NO-KEYS.
001800     IF W75-S1 > 1
001810         SUBTRACT 1        FROM W75-S1 CLIN.
001820       GO TO DEBT-HIGHLIGHT.

001840 DEBT-CHK-NO-KEYS.
001850     IF W75-S1 = ZERO
001860         MOVE 1            TO W75-S1
001870         GO TO DEBT-HIGHLIGHT.
001880     IF W75-S1 < W75-S
001890         ADD 1             TO W75-S1 CLIN.

001910 DEBT-HIGHLIGHT.
001920       PERFORM DEBT-CRT-ATTRIB.
001930	     DISPLAY W43-DET(CLIN) AT CPOS WITH REVERSE-VIDEO.
001940       GO TO DEBT-GET-USER-OPT.

       DEBT-ADD-SPECS.
002040     IF W75-S1 = ZERO
002050	       GO TO DEBT-ADD-EXIT.
002000	     MOVE W75-DNO(W75-S1)
                                 TO DME-AC W80-ACNO.
             MOVE 1              TO DME-SEQ.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
	       DISPLAY "No Memo or remarks" AT 2502
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
               GO TO DEBT-ADD-EXIT.
             INITIALIZE W200-MEM-FLDS.
	     MOVE 1		 TO WS-S1.

       DEBT-ADD-LOOP.
	     MOVE DME-DATA	 TO W200-MEM(WS-S1).
	   IF WS-S1 > 11
	       GO TO DEBT-DISP-ADD.

       DEBT-ADD-READ.
	     PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
		   ADD 1	 TO WS-S1
                   GO TO DEBT-ADD-LOOP.
	   IF WS-S1 = ZERO
	       GO TO DEBT-ADD-EXIT.
	     MOVE 99		 TO WS-S1.

       DEBT-DISP-ADD.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY S-DEB-MEM.
             DISPLAY SA-MEM.
021280	     DISPLAY "ENTER to continue or ESCape" AT 2512
                      WITH FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      PERFORM RESTORE-SCREEN-3
	       END-EVALUATE
	       GO TO DEBT-ADD-EXIT.
	     PERFORM RESTORE-SCREEN-3.
	   IF NOT (WS-S1 = 99)
	       MOVE 0		 TO WS-S1
	       GO TO DEBT-ADD-READ.

       DEBT-ADD-EXIT.
             EXIT.

       DEBT-COLLECTIONS.
002040	   IF W75-S1 = ZERO
002050	       GO TO DEBT-COL-EXIT.
002000	     MOVE W75-DNO(W75-S1)
				 TO DMC-AC W80-ACNO.
	     PERFORM CA155-GET-DEBTOR.
	   IF WS-ERROR NOT = ZERO
	       GO TO DEBT-COL-RESET.
	     UNLOCK DEBTOR.
022760	     MOVE DEB-NAME	 TO W40-NAME.
022770	     MOVE DEB-ADDRESS	 TO W60-ADDRESS.
	   IF W80-TYPE > 1
	       MOVE DEB-B-TEL	 TO W40-TEL
	   ELSE
	       MOVE DEB-H-TEL	 TO W40-TEL.
022790       INSPECT W60-ADDRESS REPLACING ALL "*" BY ".".
022810     IF W80-TYPE > 1
022820	       GO TO DEBT-COL-DUE.
022830	     INSPECT W40-NAME REPLACING ALL "*" BY " ".
022840	     INSPECT W40-NAME REPLACING FIRST "!" BY " ".

       DEBT-COL-DUE.
	   IF DEB-TERMS = 0 OR 3
000930	       COMPUTE W40-D-MTD = DEB-30 + DEB-60 + DEB-90 + DEB-120
				 + DEB-INT
	   ELSE
	   IF DEB-TERMS = 1
000930	       COMPUTE W40-D-MTD = DEB-60 + DEB-90 + DEB-120 + DEB-INT
	   ELSE
	   IF DEB-TERMS = 2
000930	       COMPUTE W40-D-MTD = DEB-90 + DEB-120 + DEB-INT.
019070	     COMPUTE W40-A-MTD = DEB-BAL - (DEB-CUR + DEB-30 +
019080				 DEB-60 + DEB-90 + DEB-120 + DEB-INT).
	     MOVE DEB-CUR	 TO W40-A-CUR.
	     MOVE DEB-30	 TO W40-A-30.
	     MOVE DEB-60	 TO W40-A-60.
	     MOVE DEB-90	 TO W40-A-90.
	     MOVE DEB-120	 TO W40-A-120.
	     MOVE DEB-INT	 TO W40-A-INT.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-COL.
	     PERFORM SCREEN-SHADOW.
             INITIALIZE W200-MEM-FLDS.
	     MOVE W80-ACNO	 TO DMC-AC.
	     MOVE 1		 TO DMC-SEQ.
	     PERFORM READ-DEBCOL-LOCK THRU READ-DEBCOL-EXIT.
           IF WS-F-ERROR NOT = ZERO
	       GO TO DEBT-COL-ACCEPT.

       DEBT-COL-DELETE.
	     MOVE DMC-SEQ	 TO WS-S1.
	     MOVE DMC-DATA	 TO W200-MEM(WS-S1).
	     PERFORM DELETE-DEBCOL-REC THRU WRITE-DEBCOL-EXIT.
	     PERFORM READ-DEBCOL-NEXT-LOCK THRU READ-DEBCOL-EXIT.
           IF WS-F-ERROR = ZERO
	       IF DMC-AC = W80-ACNO
		   GO TO DEBT-COL-DELETE.

       DEBT-COL-ACCEPT.
	     ACCEPT SA-COL.
012180	     DISPLAY S99.
012190	     ACCEPT WS-OPTION AT 2560
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
017890	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
015980     IF WS-OPTION = "N"
	       GO TO DEBT-COL-ACCEPT.
             MOVE 1              TO WS-S1.
	     INITIALIZE DMC-RECORD1.
	     MOVE W80-ACNO	 TO DMC-AC.
	     MOVE 1		 TO DMC-SEQ.

       DEBT-COL-UPDATE.
	   IF W200-MEM(WS-S1) NOT = SPACES
	       MOVE W200-MEM(WS-S1)
				 TO DMC-DATA
	       PERFORM WRITE-DEBCOL THRU WRITE-DEBCOL-EXIT
	       ADD 1		 TO DMC-SEQ.
           IF WS-S1 < 12
               ADD 1             TO WS-S1
	       GO TO DEBT-COL-UPDATE.

       DEBT-COL-RESET.
	     MOVE 18		 TO W75-S.

       DEBT-COL-FIND-LAST.
	   IF W75-DNO(W75-S) = SPACES
	       SUBTRACT 1	 FROM W75-S
	       GO TO DEBT-COL-FIND-LAST.
002000	     MOVE W75-DNO(W75-S)
002010				 TO W80-ACNO.
022690	     PERFORM CA155-GET-DEBTOR.
	   IF WS-ETYPE = "A"
	       GO TO DEBT-COL-FIND-EXIT.
	     PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.

       DEBT-COL-READ-LOOP.
000840       PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
	   IF NOT (W80-ACNO = DEB-ACNO)
	       GO TO DEBT-COL-READ-LOOP.

       DEBT-COL-FIND-EXIT.
011320	     PERFORM RESTORE-SCREEN-3.

       DEBT-COL-EXIT.
	     EXIT.

002070 DEBT-CRT-EDGE.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     "      ³                                        ³
      - 	     "         ³          ³ ³     "
		      WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
002090	     ADD 1		 TO CLIN.


002110 DEBTOR-LOOKUP-EXIT.
002120       EXIT.

       COPY APAC.HLP.

      /
      *  ****  E N Q U I R Y   ( C O L L E C T I O N S )
      *
021000 BA000		       SECTION 5.
021010 BA000-ENQUIRY.
021020       DISPLAY CLR-SCREEN.
	     PERFORM DEBTOR-LOOKUP.

021720 BA999.
021730       EXIT.

      *
      *    ****    Read debtor record - using account number.
      *
013610 CA155-GET-DEBTOR  SECTION 2.
013620 CA155.
013630       MOVE W80-ACNO       TO DEB-ACNO.
013640       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
013650     IF WS-F-ERROR = 6
013660         MOVE 1            TO WS-ERROR
013670     ELSE
013680     IF DEB-ACNO NOT = W80-ACNO
013690         MOVE 2            TO WS-ERROR
013700     ELSE
013710         MOVE 0            TO WS-ERROR
013720         MOVE DEB-TYPE     TO W80-TYPE.
013730 CA160-EXIT.
013740       EXIT.
      *
013750 CA165-NAME        SECTION 2.
013760 CA165.
013770       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
013780                           INTO W40-SURNAME 
013790                                W40-FNAME 
013800                                W40-SNAME
013810       OVERFLOW
013820           MOVE 1          TO WS-ERROR.
013830 CA170-EXIT.
013840       EXIT.
      *
014530 CA175-ADDRESS	 SECTION 2.
014540 CA175.
014550       UNSTRING W60-ADDRESS DELIMITED BY "*"
014560				 INTO W40-ADD1
				      W40-ADD2
				      W40-ADD3
				      W40-ADD4
014570       OVERFLOW
014580           MOVE 1          TO WS-ERROR.
014590 CA175-EXIT.
014600       EXIT.
      /
      *    ****    D E B T O R   E N Q U I R Y
      *   
022580 CD000		 SECTION 5.
022590 CD00.
002040	   IF W75-S1 = ZERO
002050	       GO TO CD999.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090       PERFORM SCREEN-CONTENTS.
021020	     DISPLAY CLR-SCREEN.
022610	     DISPLAY S08.
002000	     MOVE W75-DNO(W75-S1)
002010				 TO W80-ACNO.

022620 CD02.
022630       DISPLAY S09.

       CD03.
022690       PERFORM CA155-GET-DEBTOR.
022700     IF WS-ERROR = 2
022710	       MOVE WS-ERR1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
022720	       GO TO CD60.
022730     IF WS-ERROR = 1
022740	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
022750	       GO TO CD60.
             UNLOCK DEBTOR.
022760       MOVE DEB-NAME       TO W60-NAME.
022770	     MOVE DEB-ADDRESS	 TO W60-ADDRESS.
	   IF W80-TYPE > 1
	       MOVE DEB-B-TEL	 TO W40-TEL
	   ELSE
	       MOVE DEB-H-TEL	 TO W40-TEL.
022790       INSPECT W60-ADDRESS REPLACING ALL "*" BY ".".
022810     IF W80-TYPE > 1
022820         GO TO CD010.
022830       INSPECT W60-NAME REPLACING ALL "*" BY " ".
022840       INSPECT W60-NAME REPLACING FIRST "!" BY " ".

022850 CD010.
	     ADD 121 DEB-CAT	 GIVING W90-RESULT.
	     DIVIDE W90-RESULT BY 2
                                 GIVING W90-CATKEY
				 REMAINDER W90-CS.
	     ADD 1		 TO W90-CS.
040190	     MOVE W90-CATKEY	 TO WS-PARKEY.
028310	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-CDESC(W90-CS)
				 TO W90-CDESC.
	   IF DEB-CAT = 80
	       MOVE "Account Suspended"
				 TO W90-CDESC.
	   IF W90-CDESC = SPACES
	       MOVE "** I N V A L I D   C O D E **"
				 TO W90-CDESC.
	   IF DEB-SMAN >  ZERO AND < 51
020090	       ADD 100 DEB-SMAN	 GIVING WS-PARKEY
020100	       PERFORM READ-PARAM THRU READ-PARAM-EXIT
	       MOVE PAR-SALESMAN TO W90-SMAN
	   ELSE
	       MOVE SPACES	 TO W90-SMAN.
014630	     UNSTRING DEB-PADDR DELIMITED BY "*"
014640                           INTO W40-PADD1 
                                      W40-PADD2 
                                      W40-PADD3 
                                      W40-PADD4
014650       OVERFLOW
014660           MOVE 1          TO WS-ERROR.
	     MOVE DEB-PPOST	 TO W40-PPOST.
	   IF DEB-TERMS = "1"
	       MOVE "30 Days"	 TO W90-TERMS
	   ELSE
	   IF DEB-TERMS = "2"
	       MOVE "60 Days"	 TO W90-TERMS
	   ELSE
	   IF DEB-TERMS = "3"
	       MOVE "C.O.D."	 TO W90-TERMS
	   ELSE
	       MOVE "7 Days"	 TO W90-TERMS.
	   IF DEB-INV-TYPE = "I"
	       MOVE "Inclusive"  TO W90-TYPE
	   ELSE
	   IF DEB-INV-TYPE = "E"
	       MOVE "Exclusive"  TO W90-TYPE
	   ELSE
	       MOVE "Export"	 TO W90-TYPE.
	   IF DEB-PRICE = "R"
	       MOVE "Retail"	 TO W90-PRICE
	   ELSE
	   IF DEB-PRICE = "C"
	       MOVE "Cash"	 TO W90-PRICE
	   ELSE
	       MOVE "Wholesale"  TO W90-PRICE.
022860	     DISPLAY W60-NAME AT 0327
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022870	     DISPLAY DEB-LANG AT 0378
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022880	     DISPLAY W60-ADD AT 0414
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022890	     DISPLAY W40-TEL AT 0514
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022890	     DISPLAY DEB-CONTACT AT 0544
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022900       MOVE DEB-POST       TO W95-POST.
022910	     DISPLAY W95-POST AT 0476
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022940       COMPUTE W90-CUR = DEB-BAL - (DEB-30 + DEB-60 +
022950                         DEB-90 + DEB-120 + DEB-INT).
022960     IF W90-CUR < ZERO
022970         MOVE ZERO         TO W90-CUR.
022980       COMPUTE W90-90 = DEB-BAL - (W90-CUR + DEB-INT).
022990     IF W90-90 < ZERO
023000         MOVE ZERO         TO W90-90.
023010       MOVE DEB-FAX        TO W40-FAX.
023020	     DISPLAY W40-FAX AT 0614
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023030       MOVE DEB-PURYTD     TO W95-B.
023040	     DISPLAY W95-V2 AT 0650
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023050       MOVE DEB-PURPRV     TO W95-B.
023060	     DISPLAY W95-V2 AT 0669
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             MOVE ZERO           TO WS-ENQEND WS-ENQPOS.

023070 CD12.
023080       MOVE DEB-STM        TO W95-B.
023090	     DISPLAY W95-V2 AT 0769
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023100       MOVE DEB-DEBT       TO W95-B.
023110	     DISPLAY W95-V2 AT 0869
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023120       MOVE DEB-CRED       TO W95-B.
023130	     DISPLAY W95-V2 AT 0969
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023140       MOVE DEB-BAL        TO W95-B.
023150	     DISPLAY W95-V2 AT 1069
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023160	     MOVE DEB-ACTIVE	 TO W22-DTE1.
	     MOVE W22-CC1	 TO W22-CC2.
	     MOVE W22-YY1	 TO W22-YY2.
	     MOVE W22-MM1	 TO W22-MM2.
	     MOVE W22-DD1	 TO W22-DD2.
	     MOVE W22-DTE2	 TO W95-DTE.
023170	     DISPLAY W95-D AT 1269
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023160	     MOVE DEB-PMT	 TO W22-DTE1.
	     MOVE W22-CC1	 TO W22-CC2.
	     MOVE W22-YY1	 TO W22-YY2.
	     MOVE W22-MM1	 TO W22-MM2.
	     MOVE W22-DD1	 TO W22-DD2.
	     MOVE W22-DTE2	 TO W95-DTE.
023170	     DISPLAY W95-D AT 1369
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023180       MOVE W90-CUR        TO W95-B.
023190	     DISPLAY W95-V2 AT 1569
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023200       MOVE DEB-30         TO W95-B.
023210	     DISPLAY W95-V2 AT 1669
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023220       MOVE DEB-60         TO W95-B.
023230	     DISPLAY W95-V2 AT 1769
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023240       MOVE DEB-90         TO W95-B.
023250	     DISPLAY W95-V2 AT 1869
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023240       MOVE DEB-120        TO W95-B.
023250	     DISPLAY W95-V2 AT 1969
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023260       MOVE DEB-INT        TO W95-B.
023270	     DISPLAY W95-V2 AT 2069
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023280       MOVE W90-90         TO W95-B.
023290	     DISPLAY W95-V2 AT 2169
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023300       MOVE DEB-CRL        TO W95-B.
023310	     DISPLAY W95-V2 AT 2269
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

023320 CD14.
023350       MOVE DEB-ACNO       TO TRA-KEY.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
	   IF WS-F-ERROR = 5
	       GO TO CD21.
             MOVE SPACES         TO W40-SPACES.

023360 CD015.
023370       MOVE 8              TO CLIN.
             INITIALIZE W15-ALLOCATIONS.
             MOVE ZERO           TO WS-S9.

023380 CD020.
023390       PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
              (TRA-AC NOT = DEB-ACNO)
               MOVE WS-ENQPOS    TO WS-ENQEND
               GO TO CD21.
             ADD 1               TO WS-ENQPOS.

       CD20A.
             ADD 1               TO WS-S9.
             MOVE TRA-CODE       TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN-DESC.
023400	     MOVE TRA-DATE	 TO W22-DTE1.
	     MOVE W22-CC1	 TO W22-CC2.
	     MOVE W22-YY1	 TO W22-YY2.
	     MOVE W22-MM1	 TO W22-MM2.
	     MOVE W22-DD1	 TO W22-DD2.
	     MOVE W22-DTE2	 TO W90-DTE.
             MOVE 02             TO CCOL.
023410	     DISPLAY W90-DTE AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 13		 TO CCOL.
023420	     DISPLAY W00-E-DESC AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 26		 TO CCOL.
023440	     DISPLAY TRA-REF AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023450       MOVE TRA-VALUE      TO W95-B.
           IF TRA-VALUE > ZERO
               MULTIPLY TRA-ALLOCATED BY -1
				 GIVING W15-ALLOC(WS-S9)
               SUBTRACT TRA-ALLOCATED FROM TRA-VALUE
				 GIVING W15-VALUE(WS-S9)
	       MOVE TRA-REC	 TO W15-REC(WS-S9)
           ELSE
               MOVE TRA-ALLOCATED 
				 TO W15-ALLOC(WS-S9)
               ADD TRA-VALUE TRA-PAID
				 GIVING W15-VALUE(WS-S9)
	       MOVE SPACES	 TO W15-REC(WS-S9).
	     MOVE 35		 TO CCOL.
023460	     DISPLAY W95-V2 AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023500     IF CLIN < 22
023510         ADD 1             TO CLIN
023520         GO TO CD020.
023530       COMMIT.

       CD21.
023540	     DISPLAY "PgDn" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     ", " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "PgUp" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     ", " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape, " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " purchases, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F7" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " allocations, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " screen 2, "
		     "F9" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " Memo/Remarks"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       CD022.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CD30
		 WHEN F6-KEY	 GO TO CD40
		 WHEN F7-KEY
		     IF OPEN-ITEM
			 GO TO CD35
		     ELSE
			 GO TO CD022
		     END-IF
		 WHEN F8-KEY	 GO TO CD50
		 WHEN F9-KEY	 GO TO CD22
		 WHEN PAGE-UP
		     IF WS-ENQPOS = 0 OR 1 OR 15
			 GO TO CD21
		     ELSE
			 GO TO CD025
		     END-IF
		 WHEN PAGE-DOWN  GO TO CD023
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CD022.
	     CALL X"E5".
	     GO TO CD022.

       CD22.
019250	     DISPLAY CLEAR-L25.
             MOVE W80-ACNO       TO DME-AC.
             MOVE 1              TO DME-SEQ.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO CD21.
             INITIALIZE W200-MEM-FLDS.

       CD23. 
             MOVE DME-SEQ        TO WS-S1.
	     MOVE DME-DATA	 TO W200-MEM(WS-S1).
             PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
		   GO TO CD23.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.
             DISPLAY SA-MEM.
019250	     DISPLAY CLEAR-L25.
021280       DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN-3.

       CD23-E.
             GO TO CD21.

       CD023.
           IF WS-ENQPOS = WS-ENQEND
               GO TO CD022.
             MOVE 08             TO CLIN.
             MOVE 01             TO CCOL.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
023570       PERFORM CD02.
023580       PERFORM CD12.
023590       GO TO CD015.

023600 CD025.
             MOVE 1              TO WS-IXS1.

       CD25.
             PERFORM READ-DBTRAN-PREV THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CD26.
	   IF TRA-AC NOT = DEB-ACNO
023390         PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT
               GO TO CD26.
	     SUBTRACT 1 	 FROM WS-ENQPOS.
             ADD 1               TO WS-IXS1.
           IF WS-IXS1 < 30
               IF WS-ENQPOS > 0
                   GO TO CD25.

       CD26.
             MOVE 08             TO CLIN.
             MOVE 01             TO CCOL.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
023570       PERFORM CD02.
023580       PERFORM CD12.
             PERFORM CD015.
             ADD 1               TO WS-ENQPOS.
             GO TO CD20A.

       CD30.
023630       COMMIT.
023640	     GO TO CD60.

       CD35.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 47		 TO SHADE-COL.
	     MOVE 32		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S09A.
	     PERFORM SCREEN-SHADOW.
019250	     DISPLAY CLEAR-L25.
021280       DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN-3.

       CD35-E.
             GO TO CD21.

       CD40.
	     MOVE "Ú"		 TO W40-T1.
	     MOVE "¿"		 TO W40-T28.
	     MOVE ALL "Ä"	 TO W40-T2-27
                                    W40-B2-27.
	     MOVE "³"		 TO W40-M1 W40-M28
             MOVE SPACES         TO W40-M2-27.
	     MOVE "À"		 TO W40-B1.
	     MOVE "Ù"		 TO W40-B28.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 53		 TO SHADE-COL.
	     MOVE 26		 TO SHADE-WIDTH.
	     MOVE 14		 TO SHADE-LINES.
	     DISPLAY S09B.
	     PERFORM SCREEN-SHADOW.
019250	     DISPLAY CLEAR-L25.
021280       DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN-3.

       CD45-E.
             GO TO CD21.

       CD50.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 7		 TO SHADE-COL.
	     MOVE 50		 TO SHADE-WIDTH.
	     MOVE 11		 TO SHADE-LINES.
	     DISPLAY S10.
	     PERFORM SCREEN-SHADOW.
019250	     DISPLAY CLEAR-L25.
021280       DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN-3.

       CD50-E.
             GO TO CD21.

       CD60.
	     MOVE 18		 TO W75-S.

       CD65.
	   IF W75-DNO(W75-S) = SPACES
	       SUBTRACT 1	 FROM W75-S
	       GO TO CD65.
002000	     MOVE W75-DNO(W75-S)
002010				 TO W80-ACNO.
022690	     PERFORM CA155-GET-DEBTOR.
	   IF WS-ETYPE = "A"
	       GO TO CD75.
	     PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.

       CD70.
000840       PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
	   IF NOT (W80-ACNO = DEB-ACNO)
	       GO TO CD70.

       CD75.
011320	     PERFORM RESTORE-SCREEN.

023650 CD999.
023660       EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 8.
041990 ZA000-OPEN.
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
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
	     MOVE "DEBCOL"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBCOL.
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
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
	   IF TRM-STDP(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
	   IF TRM-STDP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
	   IF TRM-STDP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
	   IF TRM-STDP(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF TRM-STDP(WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
	   IF TRM-STDP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN3(WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE TRM-CONP(WS-S6)
                                 TO WS-COND.
	     MOVE TRM-NORP(WS-S6)
                                 TO WS-NORM.
	     MOVE TRM-EXPP(WS-S6)
                                 TO WS-EXPP.
	     MOVE TRM-ECAN(WS-S6)
                                 TO WS-ECAN.
	     MOVE TRM-8LPI(WS-S6)
                                 TO WS-8LPI.
	     MOVE TRM-6LPI(WS-S6)
                                 TO WS-6LPI.
	     MOVE TRM-10CPI(WS-S6)
                                 TO WS-10CPI.
	     MOVE TRM-12CPI(WS-S6)
                                 TO WS-12CPI.
	     MOVE TRM-17CPI(WS-S6)
                                 TO WS-17CPI.
	     MOVE TRM-LENGTH(WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE(WS-S6)
				 TO WS-PGE-LENGTH.
042170 ZA02.
044300	     OPEN I-O DEBCOL.
           IF WS-STATUS = "05"
	       CLOSE DEBCOL
	       OPEN OUTPUT DEBCOL
	       CLOSE DEBCOL
	       OPEN I-O DEBCOL.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
		   MOVE 49	 TO WS-F-ERROR
                   GO TO ZA50.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
      *       DISPLAY "Waiting for printer response" AT 1412
      * 	       WITH FOREGROUND-COLOR 14 BLINK.
042780*       OPEN OUTPUT PRNREP.
044780	     GO TO ZA999-EXIT.

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.
039850 ZA50.
041850	   IF WS-F-ERROR = 49
041860	       MOVE "DEBTOR COLL"
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
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1) WS-BCH(WS-S1).
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
042800 ZA200.
042810       DISPLAY "Files locked - Try later" AT 2312
042820                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
042830       DISPLAY "Press any key" AT 2512 
042840		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
042850	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 8.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041640     ELSE
041730     IF WS-F-ERROR = 5
041740         MOVE W02-DBTRAN TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
041760     ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX.
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
