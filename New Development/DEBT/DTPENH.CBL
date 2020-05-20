      $SET LINKCOUNT"586"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	********  **	**  **	  **   *
      *   **	**     **     **    **	**	  ***	**  **	  **   *
      *   **	**     **     **    **	**	  ****	**  **	  **   *
      *   **	**     **     *******	****	  ** **	**  ********   *
      *   **	**     **     **	**	  **  ****  **	  **   *
      *   **	**     **     **	**	  **   ***  **	  **   *
      *   *******      **     **	********  **	**  **	  **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R ' S   E N Q U I R Y   P R O G R A M            *
      * 							       *
      *     Debtors history enquiry - Allows enquiry of a previous     *
      *     period.						       *
      * 	   DBHIST and DTHIST - Account and Transaction files   *
      * 	   created by the program DTP006 (Age accounts).       *
      * 							       *
      *     DEBALT - Debtors alternate index (any word in the name) is *
      * 	     not available in this program as it is not saved  *
      * 	     for history purposes.			       *
      *                                                                *
      *     VERSION 9.00.00 - August 2012			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  October 2008	- Amended Accounts receivable ageing to create *
      * 		  duplicate files of the accounts and trans-   *
      * 		  actions for printing of copy satements when  *
      * 		  and if required. The file names are AR_Ayymm *
      * 		  and AR_Tyymm, where yymm is replaced with    *
      * 		  the year and month.			       *
      * 							       *
      *  November 2008	- Allow for enquiry into a previous period.    *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPENH.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 2008.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2008 - 2012
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

000160 COPY DTHIST.SL.

000160 COPY DEBDEP.SL.

000140 COPY DBHIST.SL.

000140 COPY DEBMEM.SL.

000210 COPY DEPART.SL.

000250 COPY INVOIC.SL.

000120 COPY PARAM.SL.

000240     SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

       COPY SALE.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000330 COPY DTHIST.FDE.

000330 COPY DEBDEP.FDE.

000310 COPY DBHIST.FDE.

000140 COPY DEBMEM.FDE.

000500 COPY DEPART.FDE.

000540 COPY INVOIC.FDE.

000290 COPY PARAM.FDE.

      /
000410 FD  PRNREP    LABEL RECORD OMITTED
000420               LINAGE 66.
000430 01  R-L1.
000440     03  R-DET1          PIC  X(132).

000600 COPY SALE.FDE.

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
       77  WS-CHECK	   PIC	X(18)	 VALUE
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
002390 77  WS-SALKEY       PIC  9(04)    COMP-5.
001800 77  WS-PAGE	   PIC	9(04)	 COMP-5.
003050 77  WS-NORM         PIC  X(04).
003060 77  WS-COND         PIC  X(04).
003070 77  WS-EXPP         PIC  X(04).
003080 77  WS-ECAN         PIC  X(04).
003090 77  WS-8LPI	   PIC	X(04).
003100 77  WS-6LPI	   PIC	X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
001810 77  WS-KEYSTR       PIC  9(06)    COMP-5.
002570 77  WS-LINES	   PIC	9(02) VALUE 0.
002580 77  WS-FIRST	   PIC	9(03) VALUE 0.
002590 77  WS-LAST	   PIC	9(03) VALUE 0.
002600 77  WS-LASTINV      PIC  9(04)    COMP-5.
002610 77  WS-LINE         PIC  9(04)    COMP-5.
002630 77  WS-L1	   PIC	9(04)	 COMP-5.
001820 77  WS-OPTION	   PIC	X(01).
       77  WS-PERIOD	   PIC	X(01).
002920 77  WS-PRC          PIC  X(01).
       77  WS-PROF	   PIC	X(01).
       88  PROF-INV	   VALUE "Y".
000750 77  WS-TYP	   PIC	X(01).
000750 77  WS-SEQ	   PIC	X(01).
001850 77  WS-ERROR	   PIC	9(01) VALUE ZERO.
002010 77  WS-ERR1	   PIC	X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
002100 77  WS-A 	   PIC	X(19) VALUE "Any key to continue".
002110 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
003030 77  WS-CR	   PIC	X(01).
       77  WS-HIST-DATE    PIC	9(08).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  TODAY-YYMMDD	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

002420 01  WS-DB-LINE.
003060	   03  WS-TOP-LINE.
003070	       05  WS-TCHAR PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍµ".
002430	   03  WS-TOP-LNE.
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
003140	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "³              ³   ³
      -        "                       ³        ³         ³          ³".
	   03  WS-MINV-LINE REDEFINES WS-MID-LINE.
	       05  WS-MIV1     PIC  X(01).
	       05  WS-MIV78    PIC  X(78).
	       05  WS-MIV80    PIC  X(01).
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
003180	   03  WS-BOT-LINE.
003190	       05  WS-BCHAR PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍµ".
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
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".

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

       COPY DTHIST.ID.

000330 COPY DEBDEP.ID.

       COPY DBHIST.ID.

      *
      *    ****   Use DBTRAN.ID to store Path, which will be used for
      * 	  DTHIST path.
      *
       COPY DBTRAN.ID.

000140 COPY DEBMEM.ID.

      *
      *    ****   Use DEBTOR.ID to store Path, which will be used for
      * 	  DBHIST path.
      *
       COPY DEBTOR.ID.

003760 COPY DEPART.ID.

003800 COPY INVOIC.ID.

       COPY PARAM.ID.

003840 COPY SALE.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "DTPENQ.".
               05  W02-USER   PIC X(05) VALUE SPACES.

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

       COPY W05.VAT.

       COPY W10.SAL.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

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

002640 01  W20-DATE.
	   03  W20-CCYY.
	       05  W20-CC.
		   07  W20-CENT	PIC  9(02).
002650	       05  W20-YY.
002660		   07  W20-YEAR	PIC  9(02).
	   03  W20-CNYR REDEFINES W20-CCYY
			       PIC  9(04).
002670     03  W20-MM.
002680	       05  W20-MONTH   PIC  9(02).
002690     03  W20-DD.
002700	       05  W20-DAY     PIC  9(02).
002710 01  W20-DTE1 REDEFINES W20-DATE.
002720	   03  W20-DTE	       PIC  9(08).

       01  W21-DATES.
	   03  W21-DATE        PIC  9(08).
	   03  W21-DATE2 REDEFINES W21-DATE.
	       05  W21-CCYY    PIC  9(04).
	       05  W21-MM      PIC  9(02).
	       05  W21-DD      PIC  9(02).
	   03  W21-DTE1 REDEFINES W21-DATE.
	       05  W21-DTE     PIC  9(08).
	   03  W22-DATE        PIC  9(08).

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

       01  W30-KEY.
000170	   03  W30-AC	     PIC  X(06).
000060	   03  W30-DATE      PIC  9(08)    COMP.
000040	   03  W30-CODE      PIC  9(02).
000070	   03  W30-REF	     PIC  X(08).

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

       COPY W90.DBT.
004030	   03  W90-CDESC       PIC  X(30).
004080	   03  W90-SMAN        PIC  X(30).
	   03  W90-TRMS        PIC  X(12).
	   03  W90-TYPE        PIC  X(12).
	   03  W90-PRICE       PIC  X(12).

004390 01  W95-STM.
004400     03  W95-COMP        PIC X(40).
004410     03  W95-V1.
004420         05  W95-T       PIC Z(04)9.99.
004430     03  W95-V2.
004440	       05  W95-B       PIC Z(06)9.99- BLANK WHEN ZERO.
004450     03  W95-D.
004460	       05  W95-DTE     PIC Z9/99/9999.
004470	   03  W95-POST        PIC X(08).
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
007100	   03  W100-VAL        PIC Z(08)9.99-.
007120	   03  W100-V2.
007130	       05  W100-S6V2   PIC Z(05)9.99-.
007140	   03  W100-V3 REDEFINES W100-V2.
007150         05  W100-PRICE  PIC X(09).
007160         05  FILLER      PIC X(01).
007170	   03  W100-V4 REDEFINES W100-V2.
007180         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
004560	   03  W100-CRL        PIC Z(06)9.
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
004580 SCREEN SECTION.

005070 01  S08.
005080	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT
005090				 VALUE "DEBTOR ENQUIRY (".
	   03	       COLUMN 43 FOREGROUND-COLOR 5 HIGHLIGHT
				 PIC 99/99/9999 FROM WS-HIST-DATE.
	   03	       COLUMN 53 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE ")".
005100	   03  LINE  3 COLUMN  2 VALUE "Account ".
005110	   03	       COLUMN 19 VALUE "Name ".
005120	   03	       COLUMN 66 VALUE "°°Language°°°°".
005130	   03  LINE  4 COLUMN  2 VALUE "Address ".
005140	   03  LINE  5 COLUMN  2 VALUE "Phone No".
	   03	       COLUMN 35 VALUE "Contact  ".
005150	   03  LINE  6 COLUMN  2 VALUE "Fax     ".
005160	   03	       COLUMN 35 VALUE "Purchases  YTD".
005170	   03	       COLUMN 62 VALUE "Prev ".
005180	   03  LINE  7 COLUMN  5 VALUE "Date".
005190	   03	       COLUMN 13 VALUE "Description".
005200	   03	       COLUMN 26 VALUE "Ref No".
005210	   03	       COLUMN 40 VALUE "Value".

005220 01  S09.
005230     03  LINE  7 COLUMN 48 PIC X(01) USING WS-G3.
005240	   03	       COLUMN 51 VALUE "Opening balance ".
005250     03  LINE  8 COLUMN 48 PIC X(01) USING WS-G3.
005260	   03	       COLUMN 51 VALUE "Debits          ".
005270     03  LINE  9 COLUMN 48 PIC X(01) USING WS-G3.
005280	   03	       COLUMN 51 VALUE "Credits         ".
005290     03  LINE 10 COLUMN 48 PIC X(01) USING WS-G3.
005300	   03	       COLUMN 51 VALUE "Closing balance ".
005310     03  LINE 11 COLUMN 48 PIC X(01) USING WS-G3.
005320     03  LINE 12 COLUMN 48 PIC X(01) USING WS-G3.
005330	   03	       COLUMN 51 VALUE "Last active     ".
005340     03  LINE 13 COLUMN 48 PIC X(01) USING WS-G3.
005330	   03	       COLUMN 51 VALUE "Last payment    ".
005350     03  LINE 14 COLUMN 48 PIC X(01) USING WS-G3.
005360	   03	       COLUMN 51 VALUE "Age Analysis    ".
005370     03  LINE 15 COLUMN 48 PIC X(01) USING WS-G3.
005380	   03	       COLUMN 51 VALUE "    Current     ".
005390     03  LINE 16 COLUMN 48 PIC X(01) USING WS-G3.
005400	   03	       COLUMN 51 VALUE "    30 days     ".
005410     03  LINE 17 COLUMN 48 PIC X(01) USING WS-G3.
005420	   03	       COLUMN 51 VALUE "    60 days     ".
005430     03  LINE 18 COLUMN 48 PIC X(01) USING WS-G3.
005440	   03	       COLUMN 51 VALUE "    90 days     ".
005450     03  LINE 19 COLUMN 48 PIC X(01) USING WS-G3.
005460	   03	       COLUMN 51 VALUE "    120 days +  ".
005470     03  LINE 20 COLUMN 48 PIC X(01) USING WS-G3.
005460	   03	       COLUMN 51 VALUE "    Interest    ".
005480     03  LINE 21 COLUMN 48 PIC X(01) USING WS-G3.
005490	   03	       COLUMN 51 VALUE "Arrears         ".
005500     03  LINE 22 COLUMN 48 PIC X(01) USING WS-G3.
005510	   03	       COLUMN 51 VALUE "Credit limit    ".

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
	   02  S09B-1.
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
	   02  S09B-2.
           03  LINE  9 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "January   :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(1).
           03  LINE 10 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "February  :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(2).
           03  LINE 11 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "March     :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(3).
           03  LINE 12 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "April     :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(4).
           03  LINE 13 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "May       :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(5).
           03  LINE 14 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "June      :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(6).
           03  LINE 15 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "July      :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(7).
           03  LINE 16 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "August    :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(8).
           03  LINE 17 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "September :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(9).
           03  LINE 18 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "October   :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(10).
           03  LINE 19 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "November  :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(11).
           03  LINE 20 COLUMN 53 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
                                 VALUE "December  :".
           03          COLUMN 65 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 0
				 PIC  Z(08)9.99- FROM DBH-MTHPUR(12).

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
				 PIC  X(08) USING W40-PPOST.
	   03	       COLUMN 33 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				  "  Jobs outstanding ".
	   03	       COLUMN 52 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  ZZZ USING DBH-JOBS.
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE " ³".
	   03  LINE 14 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ Category       ".
	   03	       COLUMN 22 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 PIC  99B USING DBH-CAT.
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
				 PIC  X(03) USING DBH-TERMS.
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(12) USING W90-TRMS.
	   03	       COLUMN 37 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				 "                   ³".
	   03  LINE 16 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ Price Code     ".
	   03	       COLUMN 22 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 PIC  X(03) USING DBH-PRICE.
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
				 PIC  99B USING DBH-SMAN.
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
				 PIC  X(03) USING DBH-INV-TYPE.
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(12) USING W90-TYPE.
	   03	       COLUMN 37 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				 "                   ³".
	   03  LINE 19 COLUMN  5 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       COPY INVOICE.CRT.

       01  S26-REF.
	   03  LINE 13 COLUMN 69 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(08) USING W10-NUM AUTO.

009600 01  S26-QNT.
009610	   03	       COLUMN 51 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 0
009620                           PIC Z(04)9.99 USING W10-QUANT AUTO.

009630 01  S26-PRC.
009640	   03	       COLUMN 60 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 0
009650                           PIC Z(05)9.99 USING W10-SELL AUTO.

009840 01  S027-CNOTE.
009850	   03  LINE  2 COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
009860                           VALUE "CREDIT NOTE".
	   03  LINE  4 COLUMN 61 VALUE " Inv. Date".

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

       COPY DEBLUH.CRT.

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

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
010270       PERFORM ZA000-INIT.
	     MOVE "P"		 TO WS-PERIOD.

       AA05.
	     PERFORM YA000.
	   IF WS-PERIOD = "P"
	       PERFORM CD000
	       CLOSE DBHIST
		     DTHIST.

       AA10.
	     MOVE "'C'hange period or 'E'xit  [ ]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
024860	   IF NOT (WS-OPTION = "C" OR "E")
024870	       GO TO AA10.
	   IF WS-OPTION = "C"
	       GO TO AA05.
	     CLOSE DEBDEP
		   SALE.

       AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010910 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
011180	     PERFORM DBHIST-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
011360 AA99.
011370       EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011650 AC000		  SECTION.

011670 COPY CONTROL.RD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Special start for displaying transactions descending Seq. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
000020 START-AT-DTRH-KEY-DESC.
000030	     START DTHIST
	       KEY < TRH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
           IF WS-STATUS = "23"
	       MOVE 55		 TO WS-F-ERROR
000050	       GO TO READ-DTHIST-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 55		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-DTHIST-EXIT.

       COPY DTHIST.RD.

011190 COPY DEBDEP.RD.

011710 COPY DBHIST.RD.

000140 COPY DEBMEM.RD.

011210 COPY DEPART.RD.

011250 COPY INVOIC.RD.

011750 COPY PARAM.RD.

011290 COPY SALE.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011350 AD000-WRITE	       SECTION.

011530 COPY SALE.WR.

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
000010 GET-DBHIST-TRAN-RECORD SECTION.

000030 GET-DBHIST-TRAN.
000040       ADD 21 W90-CODE     GIVING W90-RESULT.
000050       DIVIDE W90-RESULT BY 2
000060                           GIVING W90-TRNKEY
000070                           REMAINDER W90-TS.
000080       ADD 1               TO W90-TS.

000100 GET-DBHIST-TRAN-CODE.
000110       MOVE W90-TRNKEY     TO WS-PARKEY.
000120       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.

000140 STORE-DBHIST-TRAN-DESC.
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

000260 STORE-DBHIST-TRAN-STATS.
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

000420 GET-DBHIST-TRAN-EXIT.
000430       EXIT.

000450 GET-DBHIST-TRAN-DESC SECTION.

000470 GET-DEBT-TRAN-DESC.
000480	     PERFORM GET-DBHIST-TRAN.
000490       MOVE W90-TRNKEY     TO WS-PARKEY.
000500       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
000510	     PERFORM STORE-DBHIST-TRAN-DESC.

000530 GET-DEBT-TRAN-DESC-EXIT.
000540       EXIT.

011970 COPY DBHIST.LUP.

       COPY APAC.HLP.

      *
      *    ****    Read debtor record - using account number.
      *
013610 CA155-GET-DBHIST  SECTION 2.
013620 CA155.
013630	     MOVE W80-ACNO	 TO DBH-ACNO.
013640	     PERFORM READ-DBHIST THRU READ-DBHIST-EXIT.
013650	   IF WS-F-ERROR = 56
013660         MOVE 1            TO WS-ERROR
013670     ELSE
013680	   IF DBH-ACNO NOT = W80-ACNO
013690         MOVE 2            TO WS-ERROR
013700     ELSE
013710         MOVE 0            TO WS-ERROR
013720	       MOVE DBH-TYPE	 TO W80-TYPE.
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
022600       DISPLAY CLR-SCREEN.
022610	     DISPLAY S08.

022620 CD02.
022630       DISPLAY S09.

       CD03.
             MOVE SPACES         TO W80-ACNO.

022640 CD005.
	     DISPLAY "F1" AT 5002
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Debtor Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F9" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " Debtor Lookup (Any Word),"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
022650	     ACCEPT S36 AT 0311.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO CD999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE SPACE  TO WS-TYP
		     PERFORM AA50
		 WHEN F9-KEY
		     MOVE "A"	 TO WS-TYP
		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CD005
	       END-EVALUATE
	       DISPLAY S36 AT 0311
               IF W80-ACNO = SPACES
015990             GO TO CD005.
022680	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	     PERFORM CLEAR-L50.
022660     IF W80-ACNO = SPACES
022670         GO TO CD999.
             MOVE 6              TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W80-ACNO	 TO DBH-ACNO.
022690	     PERFORM CA155-GET-DBHIST.
022700     IF WS-ERROR = 2
020460	       MOVE WS-ERR1	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
022720	       GO TO CD005.
022730     IF WS-ERROR = 1
020430	       MOVE WS-ERR2	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
022750	       GO TO CD005.
	     UNLOCK DBHIST.
022760	     MOVE DBH-NAME	 TO W60-NAME.
022770	     MOVE DBH-ADDRESS	 TO W60-ADDRESS.
	   IF W80-TYPE > 1
	       MOVE DBH-B-TEL	 TO W40-TEL
	   ELSE
	       MOVE DBH-H-TEL	 TO W40-TEL.
022790       INSPECT W60-ADDRESS REPLACING ALL "*" BY ".".
022810     IF W80-TYPE > 1
022820	       GO TO CD10.
022830       INSPECT W60-NAME REPLACING ALL "*" BY " ".
022840       INSPECT W60-NAME REPLACING FIRST "!" BY " ".

022850 CD10.
	     ADD 121 DBH-CAT	 GIVING W90-RESULT.
	     DIVIDE W90-RESULT BY 2
                                 GIVING W90-CATKEY
				 REMAINDER W90-CS.
	     ADD 1		 TO W90-CS.
040190	     MOVE W90-CATKEY	 TO WS-PARKEY.
028310	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-CDESC(W90-CS)
				 TO W90-CDESC.
	   IF DBH-CAT = 80
	       MOVE "Account Suspended"
				 TO W90-CDESC.
	   IF W90-CDESC = SPACES
	       MOVE "** I N V A L I D   C O D E **"
				 TO W90-CDESC.
	   IF DBH-SMAN >  ZERO AND < 51
020090	       ADD 100 DBH-SMAN	 GIVING WS-PARKEY
020100	       PERFORM READ-PARAM THRU READ-PARAM-EXIT
	       MOVE PAR-SALESMAN TO W90-SMAN
	   ELSE
	       MOVE SPACES	 TO W90-SMAN.
014630	     UNSTRING DBH-PADDR DELIMITED BY "*"
014640                           INTO W40-PADD1
                                      W40-PADD2
                                      W40-PADD3
                                      W40-PADD4
014650       OVERFLOW
014660           MOVE 1          TO WS-ERROR.
	     MOVE DBH-PPOST	 TO W40-PPOST.
	   IF DBH-TERMS = "1"
	       MOVE "30 Days"	 TO W90-TRMS
	   ELSE
	   IF DBH-TERMS = "2"
	       MOVE "60 Days"	 TO W90-TRMS
	   ELSE
	   IF DBH-TERMS = "3"
	       MOVE "C.O.D."	 TO W90-TRMS
	   ELSE
	       MOVE "7 Days"	 TO W90-TRMS.
	   IF DBH-INV-TYPE = "I"
	       MOVE "Inclusive"  TO W90-TYPE
	   ELSE
	   IF DBH-INV-TYPE = "E"
	       MOVE "Exclusive"  TO W90-TYPE
	   ELSE
	       MOVE "Export"	 TO W90-TYPE.
	   IF DBH-PRICE = "R"
	       MOVE "Retail"	 TO W90-PRICE
	   ELSE
	   IF DBH-PRICE = "C"
	       MOVE "Cash"	 TO W90-PRICE
	   ELSE
	       MOVE "Wholesale"  TO W90-PRICE.
022860	     DISPLAY W60-NAME AT 0325
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022870	     DISPLAY DBH-LANG AT 0378
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022880	     DISPLAY W60-ADD AT 0411
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022890	     DISPLAY W40-TEL AT 0511
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022890	     DISPLAY DBH-CONTACT AT 0546
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022900	     MOVE DBH-POST	 TO W95-POST.
022910	     DISPLAY W95-POST AT 0472
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022940	     COMPUTE W90-CUR = DBH-BAL - (DBH-30 + DBH-60 +
022950			       DBH-90 + DBH-120 + DBH-INT).
022960     IF W90-CUR < ZERO
022970         MOVE ZERO         TO W90-CUR.
022980	     COMPUTE W90-90 = DBH-BAL - (W90-CUR + DBH-INT).
022990     IF W90-90 < ZERO
023000         MOVE ZERO         TO W90-90.
023010	     MOVE DBH-FAX	 TO W40-FAX.
023020	     DISPLAY W40-FAX AT 0611
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF LS0-DBLEV < 3
	       MOVE ZERO	 TO W95-B
	   ELSE
023030	       MOVE DBH-PURYTD	 TO W95-B.
023040	     DISPLAY W95-V2 AT 0650
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023050	     MOVE DBH-PURPRV	 TO W95-B.
023060	     DISPLAY W95-V2 AT 0668
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             MOVE ZERO           TO WS-ENQEND WS-ENQPOS.

023070 CD12.
023080	     MOVE DBH-STM	 TO W95-B.
023090	     DISPLAY W95-V2 AT 0768
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023100	     MOVE DBH-DEBT	 TO W95-B.
023110	     DISPLAY W95-V2 AT 0868
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023120	     MOVE DBH-CRED	 TO W95-B.
023130	     DISPLAY W95-V2 AT 0968
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023140	     MOVE DBH-BAL	 TO W95-B.
023150	     DISPLAY W95-V2 AT 1068
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023160	     MOVE DBH-ACTIVE	 TO W22-DTE1.
	     MOVE W22-CC1	 TO W22-CC3.
	     MOVE W22-YY1	 TO W22-YY3.
	     MOVE W22-MM1	 TO W22-MM3.
	     MOVE W22-DD1	 TO W22-DD3.
	     MOVE W22-DTE3	 TO W95-DTE.
023170	     DISPLAY W95-D AT 1268
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023160	     MOVE DBH-PMT	 TO W22-DTE1.
	     MOVE W22-CC1	 TO W22-CC3.
	     MOVE W22-YY1	 TO W22-YY3.
	     MOVE W22-MM1	 TO W22-MM3.
	     MOVE W22-DD1	 TO W22-DD3.
	     MOVE W22-DTE3	 TO W95-DTE.
023170	     DISPLAY W95-D AT 1368
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023180       MOVE W90-CUR        TO W95-B.
023190	     DISPLAY W95-V2 AT 1568
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023200	     MOVE DBH-30	 TO W95-B.
023210	     DISPLAY W95-V2 AT 1668
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023220	     MOVE DBH-60	 TO W95-B.
023230	     DISPLAY W95-V2 AT 1768
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023240	     MOVE DBH-90	 TO W95-B.
023250	     DISPLAY W95-V2 AT 1868
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023240	     MOVE DBH-120	 TO W95-B.
023250	     DISPLAY W95-V2 AT 1968
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023260	     MOVE DBH-INT	 TO W95-B.
023270	     DISPLAY W95-V2 AT 2068
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023280       MOVE W90-90         TO W95-B.
023290	     DISPLAY W95-V2 AT 2168
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023300	     MOVE DBH-CRL	 TO W95-B.
023310	     DISPLAY W95-V2 AT 2268
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       CD13.
	     MOVE "Ascending/Descending Tran Date seq (A/D)  [A]"
				 TO WS-ERR-MES.
	     MOVE "A"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     GO TO CD00
               END-EVALUATE.
	   IF NOT(WS-OPTION = "A" OR "D")
	       GO TO CD13.
	     MOVE WS-OPTION	 TO WS-SEQ.

023320 CD14.
023350	     MOVE DBH-ACNO	 TO TRH-AC.
	   IF WS-SEQ = "A"
	       MOVE ZERO	 TO TRH-DATE
				    TRH-CODE
	       MOVE SPACES	 TO TRH-REF
	       PERFORM START-AT-DTRH-KEY THRU READ-DTHIST-EXIT
	   ELSE
	       MOVE TODAY-YYMMDD TO TRH-DATE
	       MOVE 99		 TO TRH-CODE
	       MOVE "ZZZZZZZZ"	 TO TRH-REF
	       PERFORM START-AT-DTRH-KEY-DESC THRU READ-DTHIST-EXIT.
	   IF WS-F-ERROR = 55
	       GO TO CD21.
             MOVE SPACES         TO W40-SPACES.

023360 CD015.
023370       MOVE 8              TO CLIN.
             INITIALIZE W15-ALLOCATIONS.
             MOVE ZERO           TO WS-S9.

023380 CD020.
	   IF WS-SEQ = "A"
023390	       PERFORM READ-DTHIST-NEXT THRU READ-DTHIST-EXIT
	   ELSE
	       PERFORM READ-DTHIST-PREV	THRU READ-DTHIST-EXIT.
	   IF (WS-F-ERROR = 55) OR
	      (TRH-AC NOT = DBH-ACNO)
               MOVE WS-ENQPOS    TO WS-ENQEND
	       MOVE W30-KEY	 TO TRH-KEY
	       PERFORM START-AT-DTRH-KEY THRU READ-DTHIST-EXIT
	       GO TO CD21.
	     MOVE TRH-KEY	 TO W30-KEY.
             ADD 1               TO WS-ENQPOS.

       CD20A.
             ADD 1               TO WS-S9.
	     MOVE TRH-CODE	 TO W90-CODE.
	     PERFORM GET-DBHIST-TRAN-DESC.
      *
      *    ****    S W I T C H	 D A T E   T O	 D D M M C C Y Y
      *
023400	     MOVE TRH-DATE	 TO W22-DTE1.
	     MOVE W22-CC1	 TO W22-CC3.
	     MOVE W22-YY1	 TO W22-YY3.
	     MOVE W22-MM1	 TO W22-MM3.
	     MOVE W22-DD1	 TO W22-DD3.
	     MOVE W22-DTE3	 TO W90-DTE.
             MOVE 02             TO CCOL.
023410	     DISPLAY W90-DTE AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 13		 TO CCOL.
023420	     DISPLAY W00-E-DESC AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 26		 TO CCOL.
023440	     DISPLAY TRH-REF AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023450	     MOVE TRH-VALUE	 TO W95-B.
	   IF TRH-VALUE > ZERO
	       MULTIPLY TRH-ALLOCATED BY -1
				 GIVING W15-ALLOC(WS-S9)
	       SUBTRACT TRH-ALLOCATED FROM TRH-VALUE
				 GIVING W15-VALUE(WS-S9)
	       MOVE TRH-REC	 TO W15-REC(WS-S9)
           ELSE
	       MOVE TRH-ALLOCATED
				 TO W15-ALLOC(WS-S9)
	       ADD TRH-VALUE TRH-PAID
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
023540	     DISPLAY "PgDn" AT 5002
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "," WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "PgUp" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "," WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "," WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " S-Rest,"
		     "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " Inv/Crn,"
		     "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " purch,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F7" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " alloc,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " screen 2,"
		     "F9" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " Memo/Rmks"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       CD022.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CD30
		 WHEN F4-KEY
		      PERFORM EA000
		      GO TO CD022
		 WHEN F5-KEY
		      PERFORM DA000
		      PERFORM RESTORE-SCREEN-3
		      GO TO CD022
		 WHEN F6-KEY	 GO TO CD40
		 WHEN F7-KEY
		     IF H-OPEN-ITEM
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      M E M O	D E T A I L S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       CD22.
019250	     PERFORM CLEAR-L50.
             MOVE W80-ACNO       TO DME-AC.
             MOVE 1              TO DME-SEQ.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO CD21.
             INITIALIZE W200-MEM-FLDS.
	     MOVE 1		 TO WS-S1.

       CD23.
	     MOVE DME-DATA	 TO W200-MEM(WS-S1).
	   IF WS-S1 > 11
	       GO TO CD23B.

       CD23A.
	     PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
		   ADD 1	 TO WS-S1
                   GO TO CD23.
	   IF WS-S1 = ZERO
	       GO TO CD23-E.
	     MOVE 99		 TO WS-S1.

       CD23B.
	     PERFORM SAVE-SCREEN.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.
             DISPLAY SA-MEM.
019250	     PERFORM CLEAR-L50.
021280	     DISPLAY "ENTER to continue or ESCape" AT 5024
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      PERFORM RESTORE-SCREEN
	       END-EVALUATE
	       GO TO CD23-E.
	     PERFORM RESTORE-SCREEN.
	   IF NOT (WS-S1 = 99)
	       MOVE 0		 TO WS-S1
	       GO TO CD23B.

       CD23-E.
             GO TO CD21.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			 P A G E - D O W N		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       CD023.
           IF WS-ENQPOS = WS-ENQEND
               GO TO CD022.
             MOVE 08             TO CLIN.
             MOVE 01             TO CCOL.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
023570       PERFORM CD02.
023580       PERFORM CD12.
023590       GO TO CD015.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			   P A G E - U P		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
023600 CD025.
             MOVE 1              TO WS-IXS1.

       CD25.
	   IF WS-SEQ = "A"
	       PERFORM READ-DTHIST-PREV THRU READ-DTHIST-EXIT
	   ELSE
	       PERFORM READ-DTHIST-NEXT THRU READ-DTHIST-EXIT.
	   IF WS-F-ERROR = 55
               GO TO CD26.
	   IF TRH-AC NOT = DBH-ACNO
	       IF WS-SEQ = "A"
023390		   PERFORM READ-DTHIST-NEXT THRU READ-DTHIST-EXIT
	       ELSE
023390		   PERFORM READ-DTHIST-PREV THRU READ-DTHIST-EXIT
	       END-IF
               GO TO CD26.
	     SUBTRACT 1 	 FROM WS-ENQPOS.
	     MOVE TRH-KEY	 TO W30-KEY.
	     ADD 1		 TO WS-IXS1.
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
023640       GO TO CD00.

       CD35.
	     PERFORM SAVE-SCREEN.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 47		 TO SHADE-COL.
	     MOVE 32		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S09A.
	     PERFORM SCREEN-SHADOW.
019250	     PERFORM CLEAR-L50.
021280	     DISPLAY "ENTER to continue" AT 5024
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN.

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
	     PERFORM SAVE-SCREEN.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 53		 TO SHADE-COL.
	     MOVE 26		 TO SHADE-WIDTH.
	     MOVE 14		 TO SHADE-LINES.
	   IF LS0-DBLEV < 2
	       DISPLAY S09B-1
	       DISPLAY "Not Authorised" AT 1459
			WITH FOREGROUND-COLOR 1
			     BACKGROUND-COLOR 7
	   ELSE
	       DISPLAY S09B.
	     PERFORM SCREEN-SHADOW.
019250	     PERFORM CLEAR-L50.
021280	     DISPLAY "ENTER to continue" AT 5024
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN.

       CD45-E.
             GO TO CD21.

       CD50.
	     PERFORM SAVE-SCREEN.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 7		 TO SHADE-COL.
	     MOVE 50		 TO SHADE-WIDTH.
	     MOVE 11		 TO SHADE-LINES.
	     DISPLAY S10.
	     PERFORM SCREEN-SHADOW.
019250	     PERFORM CLEAR-L50.
021280	     DISPLAY "ENTER to continue" AT 5024
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN.
       CD50-E.
             GO TO CD21.

023650 CD999.
023660	     EXIT.
      /    *************************************************************
      *    * L O O K U P   I N V O I C E   O R	 C R E D I T   N O T E *
      *    *************************************************************
       DA000		       SECTION 50.
       DA00.
	     PERFORM SAVE-SCREEN-3.
	     MOVE ZERO		 TO W10-NUM.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 52		 TO SHADE-COL.
	     MOVE 27		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1150
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1250
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "    INVOICE/CREDIT NOTE    ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
	     DISPLAY "³ Reference Number          " AT 1350
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1450
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.

       DA05.
	     ACCEPT S26-REF.
	     PERFORM RESTORE-SCREEN-3.
	   IF W10-NUM = ZERO
	       GO TO DA199.
	     MOVE W10-NUM	 TO DOC-REF.
	     MOVE ZERO		 TO DOC-SQN DOC-TYPE.
	     PERFORM START-AT-INVOIC-REF THRU READ-INVOIC-EXIT.
	   IF WS-F-ERROR = 12
045270	       GO TO DA15.

       DA10.
045330	     PERFORM READ-INVOIC-NEXT THRU READ-INVOIC-EXIT.
	   IF WS-F-ERROR = 12
045270	       GO TO DA15.
045340	   IF (WS-F-ERROR NOT = ZERO) OR
045350	      (DOC-REF NOT = W10-NUM)
045270	       GO TO DA15.
045370	   IF DOC-TYPE NOT = 0
045380	       GO TO DA10.
	     GO TO DA100.

       DA15.
045240	     MOVE "Document not on File"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO DA00.

       DA100.
012010	     MOVE DOC-AC	 TO W80-ACNO
				    W40-ACNO.
045080	     PERFORM CA155-GET-DBHIST.
	     MOVE 1		 TO WS-S9.

       DA108.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO DA108.
018660	     MOVE ZERO		 TO W90-BDT W90-SALES
				    W90-BAL W90-LDISC
018670				    W90-CUR W90-NONTAX
				    W90-EXCTOT.
	     PERFORM DA200.
018580	     DISPLAY CLR-SCREEN.
018590       DISPLAY S26.
           IF WS-CR = "C"
               DISPLAY S027-CNOTE.
018620       MOVE 8              TO LIN.
018630	     PERFORM DA180 UNTIL LIN > 18.
025520	     DISPLAY W90-REF AT 0510
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
018640	     MOVE W20-DTE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO W90-DTE.
018650	     DISPLAY W90-DTE AT 0570
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
018680       MOVE 1              TO WS-FIRST WS-SALKEY.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
             DISPLAY S36 AT 0308.
019060	     DISPLAY W40-NAME AT 0320
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
019960	     DISPLAY W90-ORD AT 0368
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
020010     IF W10-SMAN NOT = ZERO
020030         ADD 100 W10-SMAN    GIVING WS-PARKEY
020040         PERFORM READ-PARAM THRU READ-PARAM-EXIT
020030	       DISPLAY W10-SMAN AT 0411
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0
020160	       DISPLAY PAR-SALESMAN AT 0420
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0
               MOVE PAR-SALESMAN TO W40-ASSIST
           ELSE
               MOVE SPACES       TO W40-ASSIST.
	   IF DBH-B-TEL = SPACES
	       MOVE DBH-H-TEL	 TO W40-TEL
	   ELSE
	       MOVE DBH-B-TEL	 TO W40-TEL.
	    DISPLAY W40-TEL AT 0466
		    WITH FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 0.
020660	     DISPLAY W90-CONTACT AT 0529
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
020760	     DISPLAY W90-REG AT 0554
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
             MOVE 1              TO WS-SALKEY.
022610       MOVE W90-DB         TO W90-TOTAL.
022620       SUBTRACT W90-LDISC  FROM W90-TOTAL.
022980	     MOVE W90-DB	 TO W100-VAL.
022990	     DISPLAY W100-VAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
023000       MULTIPLY -1 BY W90-LDISC
023010				 GIVING W100-VAL.
023020	     DISPLAY W100-VAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   C A L C U L A T E   T A X   ( RUNNING TOTAL )       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
022690	   IF W90-TAX = SPACES
022700	       GO TO DA110.
022710	   IF W90-TTAX NOT = ZERO
               MOVE W90-TTAX     TO W25-TOTAL
023100	       MOVE W25-TOTAL	 TO W100-VAL
023110	       DISPLAY W100-VAL AT 2215
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0
022760         ADD W25-TOTAL     TO W90-TOTAL.

022770 DA110.
023140	     MOVE W90-TOTAL	 TO W100-VAL.
023150	     DISPLAY W100-VAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	     D I S P L A Y   S A L E S	 D E T A I L S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029120 DA115.
029130       MOVE 8              TO LIN.
029140	     PERFORM DA180 UNTIL LIN > 18.
029150       MOVE 8              TO LIN.
029160       MOVE 1              TO WS-LINES WS-FIRST.

029170 DA120.
029180	     MOVE WS-FIRST	 TO WS-SALKEY.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Have all the entries been displayed?	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029190 DA125.
029200	   IF WS-SALKEY > WS-LAST
029210	       GO TO DA135.
029220       PERFORM READ-SALE THRU READ-SALE-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    D I S P L A Y   E N T R Y	D E T A I L S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029240	   IF SAL-ITEM = SPACES
	       IF SAL-TYPE = "R"
029250		   DISPLAY SAL-MSER AT LINE LIN COLUMN 10
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0
	       ELSE
		   DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0
	       END-IF
029260	       GO TO DA130.
029270	     DISPLAY SAL-ITEM AT LINE LIN COLUMN 2
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
030090	     DISPLAY "   " AT LINE LIN COLUMN 17
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
029280	     DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
029290       MOVE SAL-QNT        TO W10-QUANT.
029300       DISPLAY S26-QNT AT LINE LIN.
029310       MOVE SAL-SELL       TO W10-SELL.
029320       DISPLAY S26-PRC AT LINE LIN.
029330       MOVE SAL-VAL        TO W100-EXT.
029340	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		I S   T H E   S C R E E N   F U L L	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029350 DA130.
029360     IF LIN < 18
029370         ADD 1             TO LIN WS-SALKEY WS-LINES
029380	       GO TO DA125.

       DA132.
	     MOVE "'N'ext page or 'E'xit [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
029730	   IF NOT (WS-OPTION = "E" OR "N")
	       GO TO DA132.
	   IF WS-OPTION = "E"
	       GO TO DA199.
	     PERFORM DA115.
	     GO TO DA125.

       DA135.
             EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   D I S P L A Y   O P T I O N S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029390 DA140.
029700	     MOVE "Press ANY key to continue"
				 TO WS-ERR-MES.

029660 DA145.
	     PERFORM OPT-MESSAGE.
	     GO TO DA199.

       DA180.
023890	     DISPLAY WS-MIV1 AT LINE LIN COLUMN 1
		     WITH BACKGROUND-COLOR 0
			  FOREGROUND-COLOR 3
		     WS-MIV78 WITH FOREGROUND-COLOR 3
		     WS-MIV80 WITH BACKGROUND-COLOR 0
				   FOREGROUND-COLOR 3.
023900	     ADD 1		 TO LIN.

       DA199.
	     EXIT.

       DA200		 SECTION 51.
       DA201.
	     MOVE ZERO		 TO DOC-TYPE.
	     MOVE 100		 TO DOC-SQN.
046270       PERFORM READ-INVOIC THRU READ-INVOIC-EXIT.
           IF WS-F-ERROR = 12
	       GO TO DA295.
             MOVE 1              TO WS-LAST WS-SALKEY.
	     GO TO DA210.

       DA205.
             PERFORM READ-INVOIC-NEXT THRU READ-INVOIC-EXIT.
           IF WS-F-ERROR = 12
	       GO TO DA295.

       DA210.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		     H E A D E R   R E C O R D		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
014780     IF DOC-TYPE = 0
               MOVE DOC-REF      TO W90-REF
014860         MOVE DOC-NAME     TO W40-NAME
	       MOVE DOC-DTE	 TO W20-DTE
014870         MOVE DOC-ORD      TO W90-ORD
014880         MOVE DOC-SMAN     TO W10-SMAN
014850         MOVE DOC-DOC      TO WS-CR
014850         MOVE DOC-OREF     TO W90-REG
014890         MOVE DOC-EXP      TO W90-EXP
014910	       MOVE DOC-PRC	 TO WS-PRC
	       MOVE DOC-PROF	 TO WS-PROF
014920     ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      I T E M	D E T A I L S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
014930     IF DOC-TYPE = 1
014940	       INITIALIZE SAL-RECORD1
	       IF PROF-INV
		   MOVE DOC-REMKS    TO SAP-DESC
		   MOVE DOC-PSELL    TO SAP-SELL
		   MOVE DOC-PVAT     TO SAP-VAT
		   MOVE DOC-PVAL     TO SAP-VAL
		   MOVE DOC-PINCTAX  TO SAP-INCTAX
		   MOVE DOC-PNONTAX  TO SAP-NONTAX
		   MOVE DOC-PTAXVAL  TO SAP-TAXVAL
		   MOVE DOC-LEDG     TO SAP-LEDG
015140		   MOVE DOC-SQN	     TO SAL-SQN
015080		   PERFORM DA215
	       ELSE
		   MOVE "S"	     TO SAL-TYPE
014950		   MOVE DOC-EXT-ITEM TO SAL-EXT-ITEM
014960		   MOVE DOC-DESC     TO SAL-DESC
014970		   MOVE DOC-QNT	     TO SAL-QNT
		   MOVE DOC-CASES    TO SAL-CASES
014980		   MOVE DOC-COST     TO SAL-COST
014990		   MOVE DOC-SELL     TO SAL-SELL
015000		   MOVE DOC-VAT	     TO SAL-VAT
015010		   MOVE DOC-VAL	     TO SAL-VAL
		   MOVE DOC-VAT-SUB  TO SAL-VAT-SUB
015020		   MOVE DOC-INCTAX   TO SAL-INCTAX
015030		   MOVE DOC-NONTAX   TO SAL-NONTAX
015040		   MOVE DOC-TAXVAL   TO SAL-TAXVAL
015050		   MOVE DOC-DSC	     TO SAL-DSC
015060		   MOVE DOC-DISC     TO SAL-DISC
015140		   MOVE DOC-SQN	     TO SAL-SQN
		   IF NOT (SAL-VAT-SUB = ZERO)
		       ADD SAL-VAT   TO W90-VAT(SAL-VAT-SUB)
		       ADD SAL-TAXVAL
				     TO W90-TAXVAL(SAL-VAT-SUB)
		   END-IF
015080		   PERFORM DA215
	       END-IF
015090     ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		D I S C O U N T   D E T A I L S 	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015100     IF DOC-TYPE = 2
015110         INITIALIZE SAL-RECORD1
015120         MOVE DOC-DSC      TO SAL-DSC
015130         MOVE DOC-DISC     TO SAL-DISC
015140         MOVE DOC-SQN      TO SAL-SQN
015150	       PERFORM DA215
015160     ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			 R E M A R K S			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015170     IF DOC-TYPE = 3
015180         INITIALIZE SAL-RECORD1
015190         MOVE DOC-MSER     TO SAL-MSER
015140         MOVE DOC-SQN      TO SAL-SQN
015210	       PERFORM DA215
015220	   ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   D E L I V E R Y   A D D R E S S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015170	   IF DOC-TYPE = 5
	       MOVE DOC-AD1	 TO W40-PADD1
	       MOVE DOC-AD2	 TO W40-PADD2
	       MOVE DOC-AD3	 TO W40-PADD3
	       MOVE DOC-AD4	 TO W40-PADD4
	       MOVE DOC-PC	 TO W40-PC2
	   ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		 R E M A R K S	 ( A D V A N C E D )	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015170	   IF DOC-TYPE = 6
015180         INITIALIZE SAL-RECORD1
	       MOVE "R" 	 TO SAL-TYPE
015190         MOVE DOC-MSER     TO SAL-MSER
015140         MOVE DOC-SQN      TO SAL-SQN
015210	       PERFORM DA215
015220	   ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			  T R A I L E R 		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015170	   IF DOC-TYPE = 9
015230         MOVE DOC-SUBTOT   TO W90-DB
015240         MULTIPLY -1 BY DOC-DSCNT
015250                           GIVING W90-TDISC
015260         MOVE W90-TDISC    TO W90-LDISC
015270         MOVE DOC-NON-TXBL TO W90-NONTAX
015280	       MOVE DOC-TAXABLE  TO W90-EXCTOT
015290         MOVE DOC-TAX      TO W90-TTAX
015300         MOVE DOC-TOT      TO W90-TOTAL
015310         MOVE DOC-BAL      TO W90-BAL
	       MOVE DOC-TERMS	 TO W90-TERMS
	       MOVE DOC-CONTACT  TO W90-CONTACT
               SUBTRACT 1        FROM WS-LAST
015330	       GO TO DA299.
015340	     GO TO DA205.

015360 DA215.
015370       MOVE WS-LAST        TO SAL-CODE.
015380       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
015390       ADD 1               TO WS-LAST WS-SALKEY.

       DA295.
	     MOVE "Data base corrupted"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
             MOVE 1              TO WS-ERROR.

       DA299.
	     EXIT.
      /    *************************************************************
      *    *  V I E W	D E P A R T M E N T   R E S T R I C T I O N S  *
      *    *************************************************************
       EA000		       SECTION 52.
       EA00.
	   IF NO-SALES-RESTRICTIONS
	       GO TO EA200.
      *
      * 		   DEPARTMENTS EXCLUDED
      * 	   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * 	   ³	   DEPARTMENTS ALLOWED	       ³°°
      * 	   ÃÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      * 	   ³Code³      Description	       ³°°
      * 	   ÃÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ÀÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      * 	     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
	     PERFORM SAVE-SCREEN-3.
	     MOVE 08		 TO SHADE-ROW.
	     MOVE 42		 TO SHADE-COL.
	     MOVE 35		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0740
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 0840
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	   IF SALES-ALLOWED
	       DISPLAY "        DEPARTMENTS ALLOWED        ³" AT 0841
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 7
	   ELSE
	       DISPLAY "        DEPARTMENTS EXCLUDED       ³" AT 0841
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 7.
	     DISPLAY "ÃÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0940
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "´" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³Code³	    Description           " AT 1040
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "ÃÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1140
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "´" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
018620	     MOVE 1240		 TO CPOS.
018630	     PERFORM EA100 UNTIL CLIN > 21.
	     DISPLAY "À" AT 2240
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "ÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
	     MOVE DBH-ACNO	 TO DBD-ACNO.
	     PERFORM READ-DEBDEP THRU READ-DEBDEP-EXIT.
	   IF WS-F-ERROR = 33
	       PERFORM RESTORE-SCREEN-3
	       GO TO EA200.
	     MOVE 1		 TO WS-S.
	     MOVE 1241		 TO CPOS.

       EA05.
	   IF DBD-DEPT-CDE(WS-S) = SPACES
	       GO TO EA15.
	     MOVE DBD-DEPT-CDE(WS-S)
				 TO DPT-CODE.
022380	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO EA10.
	     DISPLAY DPT-CODE AT CPOS
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 7.
	     MOVE 46	      TO CCOL.
	     DISPLAY DPT-DESC AT CPOS
		     WITH FOREGROUND-COLOR 5 BACKGROUND-COLOR 7.
	     ADD 1	      TO CLIN.
	     MOVE 41	      TO CCOL.

       EA10.
	   IF WS-S < 10
	       ADD 1	      TO WS-S
	       GO TO EA05.

       EA15.
019250	     PERFORM CLEAR-L50.
021280	     DISPLAY "ENTER to continue" AT 5024
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN-3.
	     GO TO EA999.

       EA100.
	     DISPLAY "³    ³                              " AT CPOS
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     ADD 1		 TO CLIN.

       EA199.
	     EXIT.

       EA200.
	     MOVE "No Sales Restrictions for account"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.

       EA999.
	     EXIT.

      /
       YA000		      SECTION 80.
       YA00.
019040       DISPLAY CLR-SCREEN.
	     DISPLAY "DEBTORS (ACCOUNTS RECEIVABLE) ENQUIRY"
		      AT 0322 WITH FOREGROUND-COLOR 7 HIGHLIGHT.

       YA05.
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
       YA10.
	     ACCEPT S80-YEAR.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "C"	 TO WS-PERIOD
		     GO TO YA999
		 WHEN OTHER
		     CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
		     GO TO YA10
	       END-EVALUATE.
	   IF W05-DYY > W12-YEAR
	       MOVE "Year can not be > Current Year"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO YA10.
      *    ************************************************************
      *    ***	   M O N T H   O F   P R E V I O U S   P E R I O D
      *    ************************************************************
       YA15.
	     ACCEPT S80-MONTH.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA10
		 WHEN OTHER
		     CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
		     GO TO YA15
	       END-EVALUATE.
	   IF W05-DYY > W12-YEAR
	       MOVE "Month can not be < 01 or > 12"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO YA15.
019040	     DISPLAY CLR-SCREEN.
	     DISPLAY "DEBTORS (ACCOUNTS RECEIVABLE) ENQUIRY" AT 0322
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
      *    *************************************************************
      *    ****    Create paths and file names for history files    ****
      *    *************************************************************
	     MOVE W05-DYY	 TO W05-TYY.
	     MOVE W05-DMM	 TO W05-TMM.
	     MOVE W02-DEBTOR	 TO W02-DBHIST.
	     MOVE W02-DBTRAN	 TO W02-DTHIST.
	     STRING W02-DEBTOR DELIMITED BY " "
		    W05-DEB-HIS DELIMITED SIZE
		    INTO W02-DBHIST.
	     STRING W02-DBTRAN DELIMITED BY " "
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
	     MOVE DBH-STM-DTE	 TO WS-HIST-DATE.
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
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
041980 ZA000-INIT	       SECTION 90.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W22-DTE3.
	     MOVE W22-DD3	 TO W22-DD2.
	     MOVE W22-MM3	 TO W22-MM2.
	     MOVE W22-CC3	 TO W22-CC2.
	     MOVE W22-YY3	 TO W22-YY2.
	     MOVE W22-DTE2	 TO TODAY-YYMMDD.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
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
      * 	  Use DBTRAN.ID to store Path, which will be used for
      * 	  DTHIST path.
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
	     INSPECT W02-DBTRAN REPLACING FIRST "AR_TRAN.DAT"
			       BY "           ".
      *    *************************************************************
      *    ** D E B T O R   S A L E S	R E S T R I T I O N   F I L E **
      *    *************************************************************
	     MOVE "DEBDEP"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBDEP.
      *    *************************************************************
      *    **** 	  D E B T O R	M E M O	  F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
      *    *************************************************************
      *    **** 	       D E B T O R   F I L E		    ****
      * 	  Use W02-DEBTOR to store Path, which will be used for
      * 	  DBHIST path.
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     INSPECT W02-DEBTOR REPLACING FIRST "AR_ACNT.DAT"
			       BY "           ".
      *    *************************************************************
      *    ****        D E B T O R   I N V O I C E   F I L E	    ****
      *    *************************************************************
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 		S A L E S   F I L E		    ****
      *    *************************************************************
	     MOVE "SALE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
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
042170 ZA02.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
      *       DISPLAY "Waiting for printer response" AT 1412
      * 	       WITH FOREGROUND-COLOR 14 BLINK.
042780*       OPEN OUTPUT PRNREP.
054270	     OPEN OUTPUT SALE.
054280       CLOSE SALE.
054290	     OPEN I-O SALE.
	     OPEN INPUT DEBDEP.
042220	   IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 33	 TO WS-F-ERROR
                   GO TO ZA50.
042240     IF WS-STATUS NOT = "00"
042250	       MOVE 33		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
044780	     GO TO ZA999-EXIT.

       COPY ZA49.PRO.

039850 ZA50.
055350	   IF WS-F-ERROR = 33
055360	       MOVE "DEBDEP file"
055370				 TO WS-FILE
	   END-IF.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Setup the Screen Graphics Characters: ³ Ä Ú ¿ À Ù Â Á etc ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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

       COPY LOCKED.PRO.

       ZA205.
             EXIT PROGRAM.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
042900 ZB000-ERROR	     SECTION 91.

       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041110	   ELSE
040960	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041760	   ELSE
041770	   IF WS-F-ERROR = 33
041780	       MOVE W02-DEBDEP	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE DBD-ACNO	 TO WS-KEYX
041640	   ELSE
041730	   IF WS-F-ERROR = 55
041740	       MOVE W02-DTHIST	 TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
	       MOVE TRH-KEY	 TO WS-KEYX
041760     ELSE
041770	   IF WS-F-ERROR = 56
041780	       MOVE W02-DBHIST	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE DBH-ACNO	 TO WS-KEYX.

       COPY DISPERR.PRO.
