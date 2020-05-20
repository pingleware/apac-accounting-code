      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	    ****     ******    *
      *   **	**     **     **    **	 **  **    **  **   **	  **   *
      *   **	**     **     **    **	**    **  **	**  **	       *
      *   **	**     **     *******	**    **  **	**  *******    *
      *   **	**     **     **	**    **  **	**  **	  **   *
      *   **	**     **     **	 **  **    **  **   **	  **   *
      *   *******      **     **	  ****	    ****     ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      * 	A C C O U N T S	  R E C E I V A B L E	/	       *
      *                                                                *
      *       D E B T O R   A G E   A C C O U N T S   P R O G R A M    *
      *                                                                *
      *     VERSION 8.15.02 - December 2011			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      *  October 2006	- Amended Accounts receivable ageing from a    *
      * 		  specific date to the actual date on which    *
      * 		  statements and month-end is run.	       *
      * 							       *
      *  October 2008	- Amended Accounts receivable ageing to create *
      * 		  duplicate files of the accounts and trans-   *
      * 		  actions for printing of copy satements when  *
      * 		  and if required. The file names are AR_Ayymm *
      * 		  and AR_Tyymm, where yymm is replaced with    *
      * 		  the year and month.			       *
      * 							       *
      *  December 2011	- Amended creation of history file ID's as it  *
      * 		  was possible to crerate a history file at    *
      * 		  the beginning of a month which then resulted *
      * 		  in no history file being created at month-   *
      * 		  end. See SECTION ZA000.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP006.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   March 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2012
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

       COPY "APACFIDS.SL".

000120 COPY CONTROL.SL.

000180     SELECT DBTOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS SEQUENTIAL
000210                    ORGANIZATION SEQUENTIAL.

000160 COPY DBTRAN.SL.

       COPY DTHIST.SL.

       COPY DBHIST.SL.

000140 COPY DEBTOR.ISL.

000140 COPY DEBMEM.SL.

000200 COPY INVOIC.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000160 COPY DBTOLD.GFD.

000330 COPY DBTRAN.FDE.

       COPY DTHIST.FDE.

       COPY DBHIST.FDE.

000310 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

000370 COPY INVOIC.FDE.

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
002860 77  WS-IXS2         PIC  9(04)    COMP-5.
002870 77  WS-IXS3         PIC  9(04)    COMP-5.
002880 77  WS-IXS4         PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001710 77  WS-RECKEY       PIC  9(06)    COMP-5.
001720 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
001730 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
001740 77  WS-GST-RATE     PIC S9(03)V99 COMP-3.
001750 77  WS-FIN-RATE     PIC S9(03)V99 COMP-3.
001760 77  WS-PERIOD       PIC  9(02).
001770 77  WS-NTP          PIC  X(01).
001810 77  WS-KEYSTR	   PIC	9(06)	 COMP-5.
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-SKIP         PIC  X(01) VALUE "N".
001820 77  WS-TAX          PIC  X(01).
001820 77  WS-REPLY	   PIC	X(01).
001820 77  WS-REST	   PIC	X(01).
001820 77  WS-TYPE         PIC  X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
001870 77  WS-EKEY         PIC  9(06)    COMP-5.
001880 77  WS-CHAR1        PIC  X(01).
001890 77  WS-CHAR2        PIC  X(01).
001900 77  WS-COMP         PIC  9(01).
001980 77  WS-BATCH        PIC  9(06) COMP-5.
002010 77  WS-ERR1	   PIC	X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
002100 77  WS-A            PIC  X(19) VALUE "Any key to continue".
002110 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
002130 77  WS-AC           PIC  X(07) VALUE "ACCOUNT".
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

002230 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "006".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000160 COPY DBTOLD.ID.

       COPY DBTRAN.ID.

       COPY DTHIST.ID.

       COPY DBHIST.ID.

       COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

       COPY INVOIC.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY SHARED.ID.

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

002500 01  W10-SPARES.
002510	   03  W10-DATE	    PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

       COPY W20.WS.

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

       01  W30-AGE-DATES.
	   03  W30-120	    PIC  9(08) COMP-3.
	   03  W30-90	    PIC  9(08) COMP-3.
	   03  W30-60	    PIC  9(08) COMP-3.
	   03  W30-30	    PIC  9(08) COMP-3.
	   03  W30-CUR	    PIC  9(08) COMP-3.

       COPY W40.DBT.

       COPY W40.WS.

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

006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.

003980 01  W90-TRAN.
004090     03  W90-120         PIC S9(09)V99 COMP-3.
004090     03  W90-90          PIC S9(09)V99 COMP-3.
004100     03  W90-60          PIC S9(09)V99 COMP-3.
004110     03  W90-30          PIC S9(09)V99 COMP-3.
004120     03  W90-CUR         PIC S9(09)V99 COMP-3.
004130     03  W90-BAL         PIC S9(09)V99 COMP-3.
004370	   03  W90-IDTE        PIC  9(08)    COMP-3.
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

       LINKAGE SECTION.

       COPY CHAIN.LS.

004580 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY DEBLUP.CRT.

007640 01  S27.
007650	   03  LINE  4 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007660                           VALUE "This routine does not include a
007670-                          "recovery procedure. Security".
007680	   03  LINE  5 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007690                           VALUE "copies  of the data files should
007700-                          " be made as a power  failure".
007710	   03  LINE  6 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007720                           VALUE "or any other problem during this
007730-                          " procedure would require the".
007740	   03  LINE  7 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007750                           VALUE "DATA FILES from the previous bac
007760-                          "kup to be restored !!!      ".
007770	   03  LINE  9 COLUMN 12 VALUE "Do you wish to continue (Y/N)?".
007780 01  S28.
007790	   03  LINE  7 COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
007800				 PIC Z9/99/9999 USING W90-IDTE AUTO.

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

       COPY OPT.CRT.

       COPY ERROR.CRT.

       COPY UPDATE.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008230 AA000		       SECTION.
008240 AA00.
003240	   IF LS0-DBLEV < 4
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
008250	     PERFORM ZA000-INIT.
	     PERFORM CI000.
013040	     CLOSE RECOVER.
013140 AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM DEBTOR-LOOKUP.
	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY OPTION.CRT.

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY DBTRAN.RD1.

       COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

       COPY INVOIC.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY DBTRAN.WR.

       COPY DEBTOR.WR.

000140 COPY DEBMEM.WR.

       COPY INVOIC.WR.

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
      * 	   NETWORK record 1.
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
      *    ****    A G E   A C C O U N T S
      *   
034100 CI000		       SECTION 5.
034110 CI00.
034120       DISPLAY CLR-SCREEN.
             ADD 1 W12-MONTH     GIVING WS-S8.
           IF WS-S8 > 12
               MOVE 1            TO WS-S8.
             MOVE WS-OPTION      TO WS-NTP.
034130	     DISPLAY "AGEING ACCOUNTS" AT 0233
		      WITH FOREGROUND-COLOR 11.
034140	     DISPLAY S27.

024790 CI-CHK.
024800	     ACCEPT WS-OPTION AT 0943
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
024860	   IF NOT (WS-OPTION = "Y" OR "N")
024870         GO TO CI-CHK.
024880     IF WS-OPTION = "N"
024890         GO TO CI999.
034160       DISPLAY "Have BACKUP copies of files been made (Y/N)"
		      AT 1112.

       CI05.
034180	     ACCEPT WS-OPTION AT 1156
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO CI05.
034240     IF WS-OPTION = "N"
034250         GO TO CI999.
034260	     DISPLAY "Have Statements been printed (Y/N)" AT 1312.

       CI10.
034270	     ACCEPT WS-OPTION AT 1347
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO CI10.
034330     IF WS-OPTION = "N"
034340         GO TO CI999.
034350	     DISPLAY "Are you sure (Y/N)" AT 1512.

       CI12.
034360	     ACCEPT WS-OPTION AT 1531
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO CI12.
034420     IF WS-OPTION = "N"
034430         GO TO CI999.
034440	     DISPLAY "Is this a year end (Y/N)" AT 1712.
	     MOVE "N"		 TO WS-OPTION.

034450 CI13.
034460	     ACCEPT WS-OPTION AT 1737
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
034520	   IF NOT (WS-OPTION = "Y" OR "N")
034530	       GO TO CI13.
	     DISPLAY "Is this a restart (Y/N)" AT 1912.
	     MOVE "N"		 TO WS-REST.
       CI14.
034460	     ACCEPT WS-REST AT 1936
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-REST
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
034520	   IF NOT (WS-REST = "Y" OR "N")
034530	       GO TO CI14.
	   IF WS-REST = "Y"
	       PERFORM CI90 THRU CI100.

       CI15A.
             MOVE ZERO           TO W90-CUR W90-BAL W90-30 W90-60
				    W90-120 W90-INT W90-90.
034540       MOVE 1              TO WS-S3.
034550	     DISPLAY "Account :" AT 2012.
034580       PERFORM AY60 THRU AY999.
             MOVE "     !"       TO DEB-ACNO.
             PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO CI025.

034560 CI20.
034780       PERFORM READ-DEBTOR-NEXT-LOCK THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO CI025.
034790	     DISPLAY DEB-ACNO AT 2022
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
		     " " DEB-NAME WITH FOREGROUND-COLOR 6 HIGHLIGHT.
035660	     DISPLAY WS-BLNK78 AT 1902 WITH FOREGROUND-COLOR 3.
	   IF WS-REST = "Y"
	       IF DEB-ACNO < W80-ACNO
		   ADD DEB-BAL	 TO W90-BAL
		   ADD DEB-CUR	 TO W90-CUR
		   ADD DEB-30	 TO W90-30
		   ADD DEB-60	 TO W90-60
		   ADD DEB-90	 TO W90-90
		   ADD DEB-120	 TO W90-120
		   ADD DEB-INT	 TO W90-INT
		   GO TO CI20X
	       ELSE
		   MOVE "N"	 TO WS-REST.
	   IF OPEN-ITEM OF DEB-RECORD1
	       GO TO CI065.
034800       ADD DEB-90          TO DEB-120.
034800       MOVE DEB-60         TO DEB-90.
034810       MOVE DEB-30         TO DEB-60.
034820       MOVE DEB-CUR        TO DEB-30.
034830     IF DEB-BAL NOT > ZERO
034840         MOVE ZERO         TO DEB-CUR DEB-30 DEB-60 
                                    DEB-120 DEB-90 
034850     ELSE
034860         COMPUTE DEB-CUR = DEB-BAL - (DEB-30 + DEB-60 + DEB-90
034870                                      + DEB-120 + DEB-INT).
034880     IF DEB-CUR < ZERO
034890         ADD DEB-CUR       TO DEB-30
034900         MOVE ZERO         TO DEB-CUR.
034910     IF DEB-30 < ZERO
034920         ADD DEB-30        TO DEB-60
034930         MOVE ZERO         TO DEB-30.
034940     IF DEB-60 < ZERO
034950         ADD DEB-60        TO DEB-90
034960         MOVE ZERO         TO DEB-60.
034940     IF DEB-90 < ZERO
034950         ADD DEB-90        TO DEB-120
034960         MOVE ZERO         TO DEB-90.
034970     IF DEB-120 < ZERO
034980         MOVE ZERO         TO DEB-120.

       CI20-AGE.
034990       MOVE ZERO           TO DEB-DEBT DEB-CRED.
035000       MOVE DEB-BAL        TO DEB-STM.
             ADD DEB-BAL         TO W90-BAL.
             ADD DEB-CUR         TO W90-CUR.
             ADD DEB-30          TO W90-30.
             ADD DEB-60          TO W90-60.
             ADD DEB-90          TO W90-90.
             ADD DEB-120         TO W90-120.
             ADD DEB-INT         TO W90-INT.
035010     IF WS-OPTION = "Y"
035020         MOVE DEB-PURYTD   TO DEB-PURPRV
035030         MOVE ZERO         TO DEB-PURYTD.

       CI20A.
             MOVE DEB-ACNO       TO TRA-AC.
             MOVE ZERO           TO TRA-DATE TRA-CODE.
             MOVE SPACES         TO TRA-REF.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CI20X.

       CI20B.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
              (DEB-ACNO NOT = TRA-AC)
               GO TO CI20X.
             PERFORM DELETE-DBTRAN-REC THRU WRITE-DBTRAN-EXIT.
             GO TO CI20B.    

       CI20X.
             MOVE ZERO           TO DEB-MTHPUR (WS-S8).
035040       PERFORM REWRITE-DEBTOR-UNLOCK THRU WRITE-DEBTOR-EXIT.
035190       GO TO CI20.

035200 CI025.
             MOVE W90-BAL        TO DEB-OUT.
             MOVE W90-CUR        TO DEB-MTD.
             MOVE W90-30         TO DEB-ONE.
             MOVE W90-60         TO DEB-TWO.
             MOVE W90-90         TO DEB-THREE.
             MOVE W90-120        TO DEB-FOUR.
             MOVE W90-INT        TO DEB-INTEREST.
035300       MOVE DEB-OUT        TO DEB-OPEN.
035310       MOVE ZERO           TO DEB-DT DEB-CR.
	     MOVE W30-120	 TO DEB-120-DTE.
	     MOVE W30-90	 TO DEB-90D-DTE.
	     MOVE W30-60	 TO DEB-60D-DTE.
	     MOVE W30-30	 TO DEB-30D-DTE.
	     MOVE W30-CUR	 TO DEB-CUR-DTE.
	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
035320	     DISPLAY CLR-SCREEN.
035330	     DISPLAY "Transaction file will now be rebuilt" AT 0412.
             DISPLAY "NB" AT 0612 WITH FOREGROUND-COLOR 15
                     " - ALL other workstations must EXIT from ".
             DISPLAY "DEBTORS before continuing " AT 0812
                     "C" WITH FOREGROUND-COLOR 15 "ontinue > <". 

       CI26.
             ACCEPT WS-OPTION AT 0848 WITH FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "C")
               GO TO CI26.             
035350       CLOSE DBTRAN.
             DISPLAY "Reading :" AT 1012.
             DISPLAY "Writing :" AT 1212.
      *
      *    ****    R E N A M E   D B T R A N   F I L E
      *
004990	     CALL "CBL_RENAME_FILE" USING W02-DBTRAN
005000					  W02-DBTOLD
005030				    RETURNING WS-STATUS.
	   IF NOT (WS-STATUS = X"0000")
	       MOVE "Unable to Rebuild TRANSACTION file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CI999.

       CI28.
042240       OPEN INPUT DBTOLD.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   DISPLAY "Waiting" AT 2562 WITH
			    BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK
042260             GO TO CI28.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 55		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
	     DISPLAY CLEAR-L25.

       CI29.
042240       OPEN OUTPUT DBTRAN.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   DISPLAY "Waiting" AT 2562 WITH
			    BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK
042260             GO TO CI29.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 5 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
	     DISPLAY CLEAR-L25.

001340 CI30.
001350       READ DBTOLD
               AT END
               GO TO CI32.
             DISPLAY TRO-AC AT 1022 WITH FOREGROUND-COLOR 11
                     " " TRO-DATE WITH FOREGROUND-COLOR 11.
      *
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
011870       MOVE TRO-RECORD1    TO TRA-RECORD1.
             DISPLAY TRA-AC AT 1222 WITH FOREGROUND-COLOR 14
                     " " TRA-DATE WITH FOREGROUND-COLOR 14.
002310       PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
             GO TO CI30.

001450 CI32.
001830       CLOSE DBTRAN
                   DBTOLD.
      *
      *    ****    D E L E T E   O L D   D B T R A N   F I L E
      *
004990	     CALL "CBL_DELETE_FILE" USING W02-DBTOLD
005030				    RETURNING WS-STATUS.

       CI33.
035380       OPEN I-O DBTRAN. 
035410       PERFORM AY70 THRU AY999.
035420       DISPLAY CLR-SCREEN.
035430	     DISPLAY "Delete entries from INVOICE Log file Y/N > <"
035440		      AT 0510.
035450       MOVE "N"            TO WS-OPTION.

035460 CI040.
035470	     ACCEPT WS-OPTION AT 0552 WITH FOREGROUND-COLOR 11 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
035530	   IF NOT (WS-OPTION = "Y" OR "N")
035540         GO TO CI040.
035550     IF WS-OPTION = "N"
035560         GO TO CI999.
035570       DISPLAY "Enter Date from which INVOICES are to be kept DD/M
035580- 	     "M/CCYY" AT 0710.

035590 CI045.
035600       ACCEPT S28.
035610       MOVE W90-IDTE       TO W10-EDTE.
035620       PERFORM CHECK-DATE.
035630     IF (WS-ERROR = 1) OR (W22-DTE1 > W12-T-YMD)
035640	       MOVE "Invalid Date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
035650         GO TO CI045.
035670	     DISPLAY "Invoice :" AT 0910.
035680	     DISPLAY "Deleting Invoices no longer required" AT 2310
035690                WITH FOREGROUND-COLOR 14 BLINK.
035700       PERFORM AY60 THRU AY999.
035710       OPEN I-O INVOICE.       
	     MOVE 00000001	 TO DOC-DTE.
035820       PERFORM START-AT-INVOIC-DATE THRU READ-INVOIC-EXIT.
           IF WS-F-ERROR = 12
               GO TO CI060.

035850 CI055.
035870       PERFORM READ-INVOIC-NEXT THRU READ-INVOIC-EXIT.
           IF WS-F-ERROR = 12
               GO TO CI060.
035920	   IF NOT (DOC-DTE < W22-DTE1)
035930         GO TO CI060.
	     DISPLAY DOC-REF AT 0920 WITH FOREGROUND-COLOR 11.
035970       PERFORM DELETE-INVOIC-REC THRU WRITE-INVOIC-EXIT.
036010       GO TO CI055.

036020 CI060.
036360       PERFORM AY70 THRU AY999.
             GO TO CI999.

       CI065.
	     DISPLAY "Transaction :" AT 1912.
             MOVE ZERO           TO DEB-CUR
                                    DEB-30 
                                    DEB-60 
                                    DEB-90 
                                    DEB-120
                                    DEB-INT.
             MOVE DEB-ACNO       TO TRA-AC.
             MOVE ZERO           TO TRA-DATE TRA-CODE.
             MOVE SPACES         TO TRA-REF.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
               GO TO CI80.

       CI70.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
              (DEB-ACNO NOT = TRA-AC)
               GO TO CI80.
	     DISPLAY TRA-DATE AT 1926
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
		     " " TRA-CODE WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " " TRA-REF WITH FOREGROUND-COLOR 7 HIGHLIGHT.
           IF TRA-VALUE < ZERO
               IF (TRA-VALUE + TRA-ALLOCATED) = ZERO
                   GO TO CI75
               ELSE
                   ADD TRA-ALLOCATED 
                                 TO TRA-VALUE
                   MOVE ZERO     TO TRA-ALLOCATED
           ELSE
               IF TRA-VALUE = TRA-PAID
                   GO TO CI75
               ELSE
                   SUBTRACT TRA-PAID
                                 FROM TRA-VALUE
                   MOVE ZERO     TO TRA-PAID.
           IF TRA-VALUE > ZERO
	       IF NOT (TRA-DATE > W30-120)
		   MOVE 5	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-90)
		   MOVE 4	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-60)
		   MOVE 3	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-30)
		   MOVE 2	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-CUR)
		   MOVE 1	 TO TRA-AGE
	       ELSE
		   MOVE ZERO	 TO TRA-AGE.
           IF TRA-CODE = 17 OR 20
               ADD TRA-VALUE     TO DEB-INT
           ELSE
           IF TRA-AGE = 5
               ADD TRA-VALUE     TO DEB-120
           ELSE
           IF TRA-AGE = 4
               ADD TRA-VALUE     TO DEB-90
           ELSE
           IF TRA-AGE = 3
               ADD TRA-VALUE     TO DEB-60
           ELSE
           IF TRA-AGE = 2
               ADD TRA-VALUE     TO DEB-30
           ELSE
           IF TRA-AGE = 1
               ADD TRA-VALUE     TO DEB-CUR.
             PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
             GO TO CI70.

       CI75.
             PERFORM DELETE-DBTRAN-REC THRU WRITE-DBTRAN-EXIT.
             GO TO CI70.

       CI80.
             PERFORM CI20-AGE.
             GO TO CI20X.

       CI90.
	     DISPLAY "Restart at A/C No." AT 2012.
	     MOVE SPACES	 TO W80-ACNO.

       CI95.
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
	     ACCEPT W80-ACNO AT 2031
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO CI100
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CI95
	       END-EVALUATE
016570	       DISPLAY W80-ACNO AT 2031
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
016580	       IF W80-ACNO = SPACES
016590		   GO TO CI95.
	   IF W80-ACNO = SPACES
	       GO TO CI100.
	     DISPLAY CLEAR-L25.
             MOVE 6              TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
012010	     MOVE W80-ACNO	 TO DEB-ACNO.
012020       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
	       GO TO CI95.
014380	     DISPLAY DEB-NAME AT 2038
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       CI96.
032920	     MOVE "'C'ontinue or 'R'ekey  [ ]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
024860	   IF NOT (WS-OPTION = "C" OR "R")
024870	       GO TO CI96.
	   IF WS-OPTION = "R"
	       GO TO CI95.

       CI100.
	     EXIT.

036380 CI999.
036390       EXIT.
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
	     MOVE "DBTOLD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTOLD.
      *    *************************************************************
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E    ****
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN W02-DTHIST.
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
	     MOVE AFID-PATH	 TO W02-DEBTOR W02-DBHIST.
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
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.
042720       MOVE 1              TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760	     MOVE PAR-YMD	 TO W12-T-YMD.
      *    *************************************************************
      *    ****    Create paths and file names for history files    ****
      *    *************************************************************
	     MOVE W12-YEAR	 TO W05-TYY W05-DYY.
	   IF W12-DAY < 16
	       IF W12-MONTH = 1
		   SUBTRACT 1 FROM W12-YEAR
				 GIVING W05-TYY W05-DYY
		   MOVE 12	 TO W05-TMM W05-DMM
	       ELSE
		   SUBTRACT 1 FROM W12-MONTH
				 GIVING W05-TMM W05-DMM
	       END-IF
	   ELSE
	       MOVE W12-MONTH	 TO W05-TMM W05-DMM.
	     INSPECT W02-DBHIST REPLACING FIRST "AR_ACNT.DAT"
			       BY "           ".
	     MOVE W02-DBHIST	 TO W60-ADD.
	     STRING W60-ADD DELIMITED BY " "
		    W05-DEB-HIS DELIMITED SIZE
		    INTO W02-DBHIST.
	     INSPECT W02-DTHIST REPLACING FIRST "AR_TRAN.DAT"
			       BY "           ".
	     MOVE W02-DTHIST	 TO W60-ADD.
	     STRING W60-ADD DELIMITED BY " "
		    W05-DBT-HIS DELIMITED SIZE
		    INTO W02-DTHIST.
      *
      *    ****    G E T   D E B T O R	 M O N T H - E N D   D A Y
      *
	     MOVE 1		 TO WS-NETKEY.
	     PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The month end dates are stored in the Control file in the ³
      *    ³ first record as Cur, 30d, 60d, 90d and 120d and the next  ³
      *    ³ routine is used since Ver 8.11.06. 		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *      Check the Current age date to see if it has already been
      *      updated.
      *    *************************************************************
	   IF DEB-CUR-DTE = W12-T-YMD
	       MOVE DEB-CUR-DTE  TO W30-CUR
	       MOVE DEB-30D-DTE  TO W30-30
	       MOVE DEB-60D-DTE  TO W30-60
	       MOVE DEB-90D-DTE  TO W30-90
	       MOVE DEB-120-DTE  TO W30-120
	   ELSE
	       MOVE DEB-90D-DTE	 TO W30-120
	       MOVE DEB-60D-DTE	 TO W30-90
	       MOVE DEB-30D-DTE	 TO W30-60
	       MOVE DEB-CUR-DTE	 TO W30-30
	       MOVE W12-T-YMD	 TO W30-CUR.
	     PERFORM ZA10 THRU ZA30.
044780	     GO TO ZA999-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check if the history files were created on a previous     ³
      *    ³ execution of this program. The details will have been     ³
      *    ³ changed and so can not be created again.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       ZA10.
	     MOVE 2		 TO WS-PARKEY.
042730	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF PAR-HIST-ID = W05-HIST-ID
	       GO TO ZA30.
	     OPEN OUTPUT DBHIST.
	     CLOSE DBHIST.
	     OPEN I-O DBHIST.
	     OPEN OUTPUT DTHIST.
	     CLOSE DTHIST.
	     OPEN I-O DTHIST.
	     MOVE " !"		 TO DEB-NAME.
             PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
010780     IF WS-F-ERROR = 6
021070	       GO TO ZA30.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 16		 TO SHADE-COL.
	     MOVE 52		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     DISPLAY
	       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
		     AT 0914 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				  BACKGROUND-COLOR 5
	       "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³               " AT 1014
		     WITH FOREGROUND-COLOR 5 HIGHLIGHT
			  BACKGROUND-COLOR 5
		     "CREATING HISTORY FILES"
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5
		     "		     ³"
		     WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 1114 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "Account: " WITH FOREGROUND-COLOR 2 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "                                           ³"
		     WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 1214 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "Transaction: " WITH FOREGROUND-COLOR 2 HIGHLIGHT
					  BACKGROUND-COLOR 5
		     "           			     ³"
		     WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "À" AT 1314 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
	       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		     WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     PERFORM SCREEN-SHADOW.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Ceate a dummy account on history file with details for   ³
      *    ³  printing copy statements and other reports. This will be ³
      *    ³  first record in the history account file. 	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     INITIALIZE DBH-RECORD1.
	     MOVE "  !" 	 TO DBH-NAME.
	     MOVE "     !"	 TO DBH-ACNO.
	     MOVE W30-AGE-DATES  TO DBH-W30-AGE-DATES.
	     MOVE W12-TODAY	 TO DBH-STM-DTE.
	     WRITE DBH-RECORD1.

       ZA15.
021090	     PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
010780     IF WS-F-ERROR = 6
	       GO TO ZA25.
	     DISPLAY DEB-ACNO AT 1124
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY DEB-NAME AT 1131
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE DEB-RECORD1	 TO DBH-RECORD1.
	     WRITE DBH-RECORD1.
013740	   IF WS-STAT1 NOT = "0"
013750	       MOVE 56		 TO WS-F-ERROR
013760         PERFORM WRITE-ERROR.
	     MOVE DEB-ACNO	 TO TRA-AC.
             MOVE ZERO           TO TRA-DATE TRA-CODE.
             MOVE SPACES         TO TRA-REF.
	     PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5
	       GO TO ZA15.

       ZA20.
023010	     PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5) OR
              (TRA-AC NOT = DEB-ACNO)
	       GO TO ZA15.
	     DISPLAY TRA-DATE AT 1228
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY TRA-CODE AT 1237
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY TRA-REF AT 1240
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE TRA-RECORD1	 TO TRH-RECORD1
	     WRITE TRH-RECORD1.
013740	   IF WS-STAT1 NOT = "0"
013750	       MOVE 55		 TO WS-F-ERROR
013760         PERFORM WRITE-ERROR.
	     GO TO ZA20.

       ZA25.
	     MOVE W05-HIST-ID	 TO PAR-HIST-ID.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     CLOSE DBHIST
		   DTHIST.
	     PERFORM RESTORE-SCREEN-3.

       ZA30.
	     EXIT.
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
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041640     ELSE
041730     IF WS-F-ERROR = 5
041740	       MOVE W02-DBTRAN	 TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
041760     ELSE
041770     IF WS-F-ERROR = 6
041780	       MOVE W02-DEBTOR	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 12
041860	       MOVE W02-INVOIC	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 55
041660	       MOVE W02-DBTOLD	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE TRO-KEY	 TO WS-KEYX
041760     ELSE
041770	   IF WS-F-ERROR = 56
041780	       MOVE W02-DBHIST	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE DBH-ACNO	 TO WS-KEYX.
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
