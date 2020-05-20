      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	    ****    ********   *
      *   **	**     **     **    **	 **  **    **  **   **	       *
      *   **	**     **     **    **	**    **  **	**  *******    *
      *   **	**     **     *******	**    **  **	**	  **   *
      *   **	**     **     **	**    **  **	**	  **   *
      *   **	**     **     **	 **  **    **  **   **	  **   *
      *   *******      **     **          ****      ****     ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      * 	A C C O U N T S	  R E C E I V A B L E	/	       *
      *                                                                *
      *       D E B T O R   M E M O / R E M A R K S   M A I N T        *
      *                                                                *
      *     VERSION 8.15.04 - March 2012			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      *  March 2012	- Include Option to delete the Entire MEMO for *
      * 		  a Debtor Account.			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP005.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   February 1990.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2012
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

000140 COPY DEBTOR.ISL.

000140 COPY DEBMEM.SL.

000140 COPY MEMDEB.SL.

000120 COPY PARAM.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000310 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

       COPY MEMDEB.FDE.

000290 COPY PARAM.FDE.

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
001560 77  WS-SUB          PIC  9(04)    COMP-5.
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
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001710 77  WS-RECKEY       PIC  9(06)    COMP-5.
001730 77  WS-TRANS	   PIC	9(04)	 COMP-5 VALUE 1.
001730 77  WS-FIRST	   PIC	9(04)	 COMP-5 VALUE 0.
001730 77  WS-LAST	   PIC	9(04)	 COMP-5 VALUE 0.
       77  WS-MEMPOS	   PIC	9(04)	 COMP-5 VALUE 0.
001750 77  WS-FIN-RATE	   PIC S9(03)V99 COMP-3.
001760 77  WS-PERIOD       PIC  9(02).
001770 77  WS-NTP          PIC  X(01).
001810 77  WS-KEYSTR       PIC  9(06)    COMP-5.
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-SKIP         PIC  X(01) VALUE "N".
001820 77  WS-TAX          PIC  X(01).
001820 77  WS-TYPE         PIC  X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
001870 77  WS-EKEY         PIC  9(06)    COMP-5.
001900 77  WS-COMP	   PIC	9(01).
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
002010 77  WS-ERR1	   PIC	X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
002110 77  WS-E 	   PIC	X(15) VALUE "**END** - ENTER".
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
	   03  WS-PROG	       PIC  X(03) VALUE "005".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

000140 COPY MEMDEB.ID.

       COPY PARAM.ID.

002500 01  W10-SPARES.
002510	   03  W10-DATE	    PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

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

005000 01  S05.
005010	   03  LINE  3 COLUMN 37 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "COMPANY".

005020 01  S06.
005030	   03  LINE  3 COLUMN 37 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "INTERNAL".

       COPY DEBLUP.CRT.

       COPY DEBTOR.CRT.

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W80-ACNO AUTO.

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
	   02  SA-MEM-1.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
	   02  SA-MEM-2.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
	   02  SA-MEM-3.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
	   02  SA-MEM-4.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
	   02  SA-MEM-5.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
	   02  SA-MEM-6.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
	   02  SA-MEM-7.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
	   02  SA-MEM-8.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
	   02  SA-MEM-9.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
	   02  SA-MEM-10.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
	   02  SA-MEM-11.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
	   02  SA-MEM-12.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008230 AA000		       SECTION.
008240 AA00.
003240	   IF LS0-DBLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
008250	     PERFORM ZA000-INIT.
	     PERFORM CJ000.

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

       COPY FUNCTION.CRT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			  OPT-MESSAGE			       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'. 			       ³
      *    ³							       ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³							       ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE (Use this format if function or any ³
      *    ³	       other keys are to be tested on exit)	       ³
      *    ³			     OR 			       ³
      *    ³   PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".       ³
      *    ³   (Use TEST AFTER if WS-OPTION is preloaded with a        ³
      *    ³   default option)					       ³
      *    ³							       ³
      *    ³ This would be displayed as:			       ³
      *    ³	ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³	³	   Print transactions (Y/N) [ ] 	 ³°°   ³
      *    ³	ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³	  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 13) move the line number to be used to SLIN and   ³
      *    ³ then PERFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ³ (The UNTIL and TEST AFTER options also apply here)        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

000140 COPY MEMDEB.RD.

       COPY PARAM.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY DEBTOR.WR.

       COPY DEBMEM.WR.

000140 COPY MEMDEB.WR.

       COPY PARAM.WR.

       COPY DEBTOR.LUP.

       COPY APAC.HLP.

      *
      *    ****    Read debtor record - using account number.
      *
014260 CA155-GET-DEBTOR  SECTION 2.
014270 CA155.
012010       MOVE W80-ACNO       TO DEB-ACNO.
012020       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
014380     ELSE
014390     IF DEB-ACNO NOT = W80-ACNO
014400         MOVE 2            TO WS-ERROR
014410     ELSE
014420         MOVE 0            TO WS-ERROR
               MOVE DEB-TYPE     TO W80-TYPE.
014430 CA160-EXIT.
014440       EXIT.
      *
014450 CA165-NAME        SECTION 2.
014460 CA165.
014470       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
014480                           INTO W40-SURNAME 
                                      W40-FNAME 
                                      W40-SNAME
014490       OVERFLOW
014500           MOVE 1          TO WS-ERROR.
014510 CA170-EXIT.
014520       EXIT.
      *
014530 CA175-ADDRESS     SECTION 2.
014540 CA175.
014550       UNSTRING W60-ADDRESS DELIMITED BY "*"
014560                           INTO W40-ADD1 
                                      W40-ADD2 
                                      W40-ADD3 
                                      W40-ADD4
014570       OVERFLOW
014580           MOVE 1          TO WS-ERROR.
014590 CA175-EXIT.
014600       EXIT.
      *
014610 CA180-ADDRESS     SECTION 2.
014620 CA180.
014630       UNSTRING W60-ADDRESS1 DELIMITED BY "*"
014640                           INTO W40-PADD1 
                                      W40-PADD2 
                                      W40-PADD3 
                                      W40-PADD4
014650       OVERFLOW
014660           MOVE 1          TO WS-ERROR.
014670 CA180-EXIT.
014680       EXIT.
      /
      *    ****    D E B T O R   M E M O   M A I N T E N A N C E
      *
037050 CJ000	       SECTION 50.
037060 CJ00.
020320       DISPLAY CLR-SCREEN.
020330       DISPLAY "MEMO/REMARKS MAINTENANCE" AT 0229
                      WITH FOREGROUND-COLOR 15.
020350       DISPLAY "Account :" AT 0302.
020370       MOVE SPACES         TO W80-ACNO.

020360 CJ05.
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
020380       ACCEPT S36 AT 0312.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO CJ999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CJ05
	       END-EVALUATE
016570	       DISPLAY S36 AT 0312
016580	       IF W80-ACNO = SPACES
016590		   GO TO CJ05.
	     DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.
	     DISPLAY CLEAR-L25.
020390     IF W80-ACNO = SPACES
020400         GO TO CJ999.
             MOVE 6              TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
             MOVE W80-ACNO       TO DEB-ACNO.
020410       PERFORM CA155-GET-DEBTOR.
020420     IF WS-ERROR = 1
020430	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020440         GO TO CJ05.
020450     IF WS-ERROR = 2
020460	       MOVE WS-ERR1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020470         GO TO CJ05.
	     DISPLAY WS-BLNK78 AT 0302 WITH FOREGROUND-COLOR 3.
020480       MOVE DEB-NAME       TO W60-NAME.
             MOVE DEB-CONTACT    TO W40-CONTACT.
020490       MOVE DEB-ADDRESS    TO W60-ADDRESS.
020500       MOVE DEB-PADDR      TO W60-ADDRESS1.
020510       MOVE DEB-POST       TO W40-POST.
020520       MOVE DEB-PPOST      TO W40-PPOST.
020530	     MOVE DEB-LANG	 TO W40-LANG.
020540	     MOVE SPACES	 TO W40-NAME1 W40-ADDRESS1 W40-PADDRESS.
020550       MOVE DEB-H-TEL      TO W40-HTEL.
020560       MOVE DEB-B-TEL      TO W40-TEL.
020560       MOVE DEB-FAX        TO W40-FAX.
020570       MOVE DEB-TITLE      TO W40-TITLE.
020580	     MOVE DEB-ID	 TO W40-ID.
019900	     MOVE DEB-TAX	 TO W40-TAX.
020590       MOVE DEB-CAT        TO W40-CAT.
020600       MOVE DEB-CRL        TO W40-CRL.
020600       MOVE DEB-OI         TO W40-OI.
	     MOVE DEB-ART11	 TO W40-EXEMPT.
020600       MOVE DEB-LAB        TO W40-LAB.
	   IF NOT VALID-PRICE
	       MOVE "R" 	 TO DEB-PRICE.
	     MOVE DEB-PRICE	 TO W40-PRICE.
020610     IF DEB-FIN = 1
020620         MOVE "N"          TO W40-FIN
020630     ELSE
020640         MOVE "Y"          TO W40-FIN.
020650       MOVE DEB-TERMS      TO W40-TERMS.
             MOVE DEB-INV-TYPE   TO W40-INV-TYPE.
020660       MOVE DEB-SMAN       TO W40-SMAN.
020670       MOVE DEB-DISC       TO W40-DISC.
020670       MOVE DEB-SDISC      TO W40-SDISC.
	     MOVE DEB-CONAC	 TO W40-CONAC.
020680       PERFORM CA175-ADDRESS.
020690       PERFORM CA180-ADDRESS.
	     DISPLAY WS-BLNK78 AT 0302 WITH FOREGROUND-COLOR 3.
020710     IF W80-TYPE NOT = 1
020720	       GO TO CJ15.
020730       PERFORM CA165-NAME.
020760       DISPLAY S22.
             DISPLAY S36 AT 0415.

020770 CJ10.
020780       DISPLAY S25.
	     PERFORM CHECK-CORRECT.
020860	   IF WS-OPTION = "N"
020870         GO TO CJ00.
	     GO TO CJ25.

021360 CJ15.
021370	   IF W80-TYPE = 3
021380         DISPLAY S06
021390     ELSE
021400         DISPLAY S05.
021410       DISPLAY S23.
             DISPLAY S36 AT 0415.
021420       MOVE W60-NAME       TO W40-NAME.

021450 CJ20.
021460       DISPLAY S24.
	     PERFORM CHECK-CORRECT.
021540	   IF WS-OPTION = "N"
021550         GO TO CJ00.
      *
      *    ****   M E M O   W I N D O W
      *
       CJ25.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.
      *
      *    ****   C H E C K   I F   T H E R E	I S   A N Y   D A T A
      *
	     INITIALIZE W200-MEM-FLDS.
             MOVE W80-ACNO       TO DME-AC.
             MOVE 1              TO DME-SEQ.
018690	     MOVE 1		 TO WS-FIRST WS-MEMPOS WS-LAST.
	     PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
      *
      *    ****    I F	 T H E R E   I S   N O	 D A T A   B Y P A S S
      *
           IF WS-F-ERROR NOT = ZERO
	       OPEN OUTPUT MEMDEB
	       CLOSE MEMDEB
	       OPEN I-O MEMDEB
	       MOVE ZERO	 TO WS-S9
	       MOVE SPACES	 TO W205-MEM-DET W200-KEYS
	       MOVE 1		 TO W205-S
	       GO TO CJ40.
      *
      *    ****   G E T   E X I S T I N G   M E M O   D A T A
      *
	     PERFORM GET-DEBMEM-DETAILS.
	     MOVE 1		 TO W205-S.
      *
      *    ****   S E T   S T A R T   P O S I T I O N	F O R
      * 	  D I S P L A Y   A N D   A C C E P T
      *
       CJ28.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     MOVE 1		 TO WS-S1.
	     MOVE W205-S	 TO W205-S1 W205-S2.
	     INITIALIZE W200-MEM-FLDS.
      *
      *    ****   S T O R E   M E M O	D E T A I L S	I N
      * 	  D I S P L A Y   A N D   A C C E P T	A R E A
      *
       CJ30.
	     MOVE W205-MEM(W205-S1)
				 TO W200-MEM(WS-S1).
	   IF WS-S1 > 11
	       GO TO CJ40.
      *
      *    ****   C H E C K   F O R   E N D   O F   M E M O   D A T A
      *
       CJ35.
	   IF W205-S1 < W205-LAST
	       ADD 1		 TO WS-S1 W205-S1
	       GO TO CJ30.

       CJ40.
	     DISPLAY SA-MEM.
001020	     DISPLAY " " AT 2503 WITH BACKGROUND-COLOR 3
					FOREGROUND-COLOR 6 HIGHLIGHT
001030		     "," WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "PgUp" WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "PgDn" WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
		     "," WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001170		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape," WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Insert line," WITH BACKGROUND-COLOR 3
				     FOREGROUND-COLOR 1
		     "F3" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Delete line," WITH BACKGROUND-COLOR 3
					  FOREGROUND-COLOR 1
		     "F4" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Delete All," WITH BACKGROUND-COLOR 3
					  FOREGROUND-COLOR 1
		     "F10" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Update" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 1.

       CJ45.
	     ACCEPT SA-MEM-1.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     IF W205-S = 1
			 GO TO CJ45
		     ELSE
			 SUBTRACT 1
				 FROM W205-S
001320			 GO TO CJ28
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ50
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ45
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ45
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM1
		     PERFORM CJ130
		     GO TO CJ45
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ45.
	     ADD 1		 TO W205-S2.

       CJ50.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-2.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ45
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ55
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ50
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ50
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM2
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ45
		     ELSE
			 GO TO CJ50
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ50.
	     ADD 1		 TO W205-S2.

       CJ55.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-3.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ50
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ60
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ55
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ55
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM3
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ50
		     ELSE
			 GO TO CJ55
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ55.
	     ADD 1		 TO W205-S2.

       CJ60.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-4.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ55
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ65
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ60
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ60
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM4
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ55
		     ELSE
			 GO TO CJ60
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ60.
	     ADD 1		 TO W205-S2.

       CJ65.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-5.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ60
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ70
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ65
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ65
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM5
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ60
		     ELSE
			 GO TO CJ65
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ65.
	     ADD 1		 TO W205-S2.

       CJ70.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-6.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ65
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ75
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ70
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ70
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM6
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ65
		     ELSE
			 GO TO CJ70
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ70.
	     ADD 1		 TO W205-S2.

       CJ75.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-7.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ70
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ80
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ75
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ75
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM7
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ70
		     ELSE
			 GO TO CJ75
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ75.
	     ADD 1		 TO W205-S2.

       CJ80.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-8.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ75
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ85
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ80
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ80
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM8
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ75
		     ELSE
			 GO TO CJ80
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ80.
	     ADD 1		 TO W205-S2.

       CJ85.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-9.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ80
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ90
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ85
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ85
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM9
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ80
		     ELSE
			 GO TO CJ85
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ85.
	     ADD 1		 TO W205-S2.

       CJ90.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ85
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ95
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ90
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ90
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM10
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ85
		     ELSE
			 GO TO CJ90
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ90.
	     ADD 1		 TO W205-S2.

       CJ95.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-11.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ90
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ100
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ95
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ95
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM11
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ90
		     ELSE
			 GO TO CJ95
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ95.
	     ADD 1		 TO W205-S2.

       CJ100.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-12.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    M O V E   U P   O N E   L I N E
      *
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ95
      *
      *    M O V E   D O W N   O N E	L I N E
      *
		 WHEN DOWN-KEY
		     IF W205-S > 88
001340			 GO TO CJ100
		     ELSE
			 PERFORM CK000
			 ADD 1	 TO W205-S
			 ADD 11 W205-S
				 GIVING W205-S2
			 PERFORM CJ28 THRU CJ40
			 ADD 11  TO W205-S2
			 GO TO CJ100
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ100
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY
		     GO TO CJ110
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ100
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM12
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ95
		     ELSE
			 GO TO CJ100
      *
      *	   D E L E T E	 A L L
      *
		 WHEN F4-KEY
		     PERFORM CJ140
		     GO TO CJ110
      *
      *    E N D   I N P U T
      *
		 WHEN F10-KEY
		     GO TO CJ105
      *
      *    I N V A L I D   K E Y   S T R O K E
      *
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ100.
	     PERFORM CK000.
	   IF W205-S2 = 100
	       GO TO CJ105.
	   IF W205-S2 = W205-LAST
	       ADD 1		 TO W205-LAST.
	     ADD 1		 TO W205-S.
	     ADD 11 W205-S	 GIVING W205-S2.
	     PERFORM CJ28 THRU CJ40.
	     ADD 11		 TO W205-S2.
	     GO TO CJ100.

       CJ105.
	     PERFORM CK000.
	     ADD 11 W205-S	 GIVING W205-S2.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
	       GO TO CJ45.
	     PERFORM CN000.

       CJ110.
	     PERFORM RESTORE-SCREEN-3.
	     CLOSE MEMDEB.
	     GO TO CJ00.
      *
      *    P A G E   D O W N
      *
       CJ115.
	     PERFORM CK000.
	   IF W205-S > 75
	       MOVE 88		 TO W205-S
	   ELSE
	       ADD 12		 TO W205-S.
	     GO TO CJ28.
      *
      *    P A G E   U P
      *
       CJ120.
	     PERFORM CK000.
	   IF W205-S < 13
	       MOVE 1		 TO W205-S
	   ELSE
	       SUBTRACT 12	 FROM W205-S.
	     GO TO CJ28.
      *
      *    I N S E R T	 A   L I N E
      *
       CJ125.
	     PERFORM CK000.
	  IF WS-LAST < 100
	      PERFORM CL000.
	     PERFORM CJ28 THRU CJ40.
      *
      *    D E L E T E	 A   L I N E
      *
       CJ130.
	     PERFORM CK000.
	     MOVE W205-S2	 TO WS-S6 WS-S5.
	     PERFORM CM000.
	     PERFORM CJ28 THRU CJ40.
	     MOVE WS-S6 	 TO W205-S2.
      *
      *    D E L E T E	 A L L	 F O R	 T H I S   A C O U N T
      *
       CJ140.
	     PERFORM CP000.

       CJ999.
	     EXIT.
      *
      *    ****   M O V E   D A T A   F R O M	A C C E P T   A R E A
      * 	  T O	W 2 0 5   A N D   W R I T E   T H E S E
      * 	  D E T A I L S   T O	T H E	W O R K   F I L E
      *
       CK000	       SECTION 51.
       CK00.
	     MOVE W205-S	 TO W205-S1.
	     MOVE 1		 TO WS-S1.
	     MOVE DEB-ACNO	 TO MEM-AC.

       CK05.
	     MOVE W205-S1	 TO MEM-SEQ.
	     MOVE W200-MEM(WS-S1)
				 TO W205-MEM(W205-S1)
				    MEM-DATA.
	     PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
	   IF WS-F-ERROR = 62
	       MOVE ZERO	 TO WS-F-ERROR
	       IF NOT (MEM-DATA = SPACES)
		   PERFORM WRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
	   IF WS-S1 < 12
	       ADD 1		 TO WS-S1 W205-S1
	       GO TO CK05.

       CK999.
	     EXIT.
      *
      *    ****   I N S E R T	A   B L A N K	L I N E
      *
       CL000	       SECTION 52.
       CL00.
	     MOVE W205-S1	 TO WS-S7.
	     MOVE W205-S2	 TO WS-S8.
	     MOVE W205-LAST	 TO W205-S1.
	     ADD 1		 TO W205-LAST.
	     MOVE W205-LAST	 TO W205-S2.
	     MOVE DEB-ACNO	 TO MEM-AC.
      *
      *    M O V E   D A T A   U P   I N   W O R K   A R E A   A N D
      *    I N	 T H E	 W O R K   F I L E
      *
       CL05.
	     MOVE W205-S2	 TO MEM-SEQ.
	     MOVE W205-MEM(W205-S1)
				 TO W205-MEM(W205-S2)
				    MEM-DATA.
	   IF W205-S2 = W205-LAST
	       PERFORM WRITE-MEMDEB THRU WRITE-MEMDEB-EXIT
	       IF WS-F-ERROR = 62
		   PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT
		   MOVE ZERO	 TO WS-F-ERROR
	       END-IF
	   ELSE
	       PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
	   IF W205-S1 > WS-S8
	       SUBTRACT 1	 FROM W205-S1 W205-S2
	       GO TO CL05.
	     MOVE SPACES	 TO W205-MEM(W205-S1)
				    MEM-DATA.
	     MOVE W205-S1	 TO MEM-SEQ.

       CL10.
	     PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
	     MOVE WS-S7		 TO W205-S1.
	     MOVE WS-S8		 TO W205-S2.

       CL999.
	     EXIT.
      *
      *    ****   D E L E T E	T H E	L I N E   B E I N G
      * 	  E D I T E D
      *
       CM000	       SECTION 53.
       CM00.
	     MOVE W205-S1	 TO WS-S7.
	     MOVE W205-S2	 TO WS-S8.
	     MOVE DEB-ACNO	 TO MEM-AC.

       CM02.
	   IF W205-MEM(W205-LAST) = SPACES
	       IF W205-S2 = W205-LAST
		   GO TO CM999
	       ELSE
	       IF W205-S2 < W205-LAST
		   SUBTRACT 1	 FROM W205-LAST
		   GO TO CM02.
	   IF W205-S2 = W205-LAST
	       MOVE SPACES	 TO W205-MEM(W205-S2)
	       GO TO CM10.
	     ADD 1 W205-S2	 GIVING	W205-S1.
      *
      *    M O V E   D A T A   D O W N	 I N   W O R K	 A R E A   A N D
      *    I N	 T H E	 W O R K   F I L E
      *
       CM05.
	     MOVE W205-S2	 TO MEM-SEQ.
	     MOVE W205-MEM(W205-S1)
				 TO W205-MEM(W205-S2)
				    MEM-DATA.
	     PERFORM REWRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
	     MOVE SPACES	 TO W205-MEM(W205-S1).
	   IF W205-S1 < W205-LAST
	       ADD 1		 TO W205-S1 W205-S2
	       GO TO CM05.

       CM10.
	     MOVE SPACES	 TO MEM-DATA.
	     MOVE W205-LAST	 TO MEM-SEQ.
	     PERFORM READ-MEMDEB THRU READ-MEMDEB-EXIT.
	     PERFORM DELETE-MEMDEB-REC THRU WRITE-MEMDEB-EXIT.
	     SUBTRACT 1 	 FROM W205-LAST.
	     MOVE WS-S7		 TO W205-S1.
	     MOVE WS-S8		 TO W205-S2.
	   IF W205-S2 > W205-LAST
	       MOVE W205-LAST	 TO W205-S2 WS-S6.

       CM999.
	     EXIT.
      *
      *    ****   U P D A T E	T H E	M E M O   F I L E
      *
       CN000	       SECTION 54.
       CN00.
	     MOVE 1		 TO W205-S1 WS-S6.
      *
      *    F I N D   L A S T   L I N E	 W I T H   D A T A
      *
       CN02.
	   IF W205-MEM(W205-LAST) = SPACES
	       SUBTRACT 1	 FROM W205-LAST
	       GO TO CN02.
      *
      *    R E M O V E	 B L A N K   L I N E S
      *
       CN05.
	   IF W205-MEM(W205-S1) = SPACES
	       PERFORM CO000
	       GO TO CN05.
	   IF W205-S1 < W205-LAST
	       ADD 1		 TO W205-S1
	       GO TO CN05.
	     MOVE 1		 TO W205-S1.
	     MOVE DEB-ACNO	 TO DME-AC.
      *
      *    U P D A T E	 M E M O   F I L E
      *
       CN10.
	     MOVE W205-S1	 TO DME-SEQ.
	     MOVE W205-MEM(W205-S1)
				 TO DME-DATA.
      *
      *    F I N D   K E Y   I N   K E Y   T A B L E
      *
       CN15.
	   IF DME-KEY < W200-KEY(WS-S6)
	       GO TO CN20.
	   IF DME-KEY = W200-KEY(WS-S6)
	       GO TO CN25.
	   IF WS-S6 < W205-LAST
	       ADD 1		 TO WS-S6
	       GO TO CN15.
      *
      *    A D D   N E W   M E M O   R E C O R D
      *
       CN20.
	     PERFORM WRITE-DEBMEM THRU WRITE-DEBMEM-EXIT.
	     GO TO CN30.
      *
      *    U P D A T E	 E X I S T I N G   M E M O   R E C O R D
      *
       CN25.
	     PERFORM REWRITE-DEBMEM THRU WRITE-DEBMEM-EXIT.
      *
      *    C L E A R   K E Y S	 O F   U S E D	 R E C O R D S
      *
	     MOVE SPACES	 TO W200-KEY(WS-S6).

       CN30.
	   IF W205-S1 < W205-LAST
	       ADD 1		 TO W205-S1
	       GO TO CN10.
      *
      *    D E L E T E	 R E D U N D A N T   R E C O R D S   F R O M
      *    M E M O   F I L E
      *
	     MOVE 1		 TO WS-S6.
      *
      *    F I N D   R E D U N D A N T	 K E Y S
      *
       CN35.
	   IF NOT (W200-KEY(WS-S6) = SPACES)
	       MOVE W200-KEY(WS-S6)
				 TO DME-KEY
	       PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT
	       PERFORM DELETE-DEBMEM-REC THRU WRITE-DEBMEM-EXIT.
	   IF WS-S6 < WS-S9
	       ADD 1		 TO WS-S6
	       GO TO CN35.

       CN999.
	     EXIT.
      *
      *    M O V E   D A T A   D O W N	 I N   W O R K	 A R E A
      *
       CO000	       SECTION 54.
       CO00.
	     MOVE W205-S1	 TO WS-S7.
	     ADD 1 WS-S7	 GIVING WS-S8.

       CO05.
	     MOVE W205-MEM(WS-S8)
				 TO W205-MEM(WS-S7).
	     MOVE SPACES	 TO W205-MEM(WS-S8).
	   IF WS-S8 < W205-LAST
	       ADD 1		 TO WS-S8 WS-S7
	       GO TO CO05.
	     SUBTRACT 1 	 FROM W205-LAST.

       CO999.
	     EXIT.

      *
      *    D E L E T E	 M E M O   D E T A I L S   F O R   T H I S
      *    A C C O U N T
      *
       CP000	       SECTION 55.
       CP00.
	     MOVE W80-ACNO	 TO DME-AC.
             MOVE 1              TO DME-SEQ.
018690	     MOVE 1		 TO WS-FIRST WS-MEMPOS WS-LAST.
	     PERFORM START-AT-DEBTOR THRU READ-DEBMEM-EXIT.
	   IF WS-F-ERROR = 27
	       MOVE "No MEMO details for account"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CP999
	   END-IF.

       CP05.
	     PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
	   IF WS-F-ERROR = 0
	       IF DME-AC = W80-ACNO
		   PERFORM DELETE-DEBMEM-REC THRU WRITE-DEBMEM-EXIT
		   GO TO CP05
	       END-IF
	   END-IF.
	     MOVE SPACES	 TO WS-ERR-MES.
	     STRING "MEMO deleted for " DELIMITED BY SIZE
		     W80-ACNO DELIMITED BY SIZE
		    " - Press ANY key" DELIMITED SIZE
				 INTO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.

       CP999.
	      EXIT.

       GET-DEBMEM-DETAILS SECTION 56.
       OPEN-MEMDEB.
	     OPEN OUTPUT MEMDEB.
	     CLOSE MEMDEB.
	     OPEN I-O MEMDEB.
	     MOVE ZERO		 TO WS-S9.
	     MOVE SPACES	 TO W205-MEM-DET W200-KEYS.
	     MOVE DME-AC	 TO MEM-AC.

       WRITE-MEMO-DETAILS.
	     MOVE DME-SEQ	 TO MEM-SEQ WS-LAST.
	     ADD 1		 TO WS-S9.
	     MOVE DME-DATA	 TO MEM-DATA W205-MEM(WS-S9).
	     MOVE DME-KEY	 TO W200-KEY(WS-S9).
	     PERFORM WRITE-MEMDEB THRU WRITE-MEMDEB-EXIT.
	     PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
	   IF WS-F-ERROR = ZERO
	       IF DME-AC = W80-ACNO
		   GO TO WRITE-MEMO-DETAILS.
	     MOVE WS-S9 	 TO W205-LAST.

       GET-DEBMEM-DETAILS-EXIT.
	     EXIT.

      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 80.
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
      *    UP AND DOWN ARROW KEYS
      *
	     MOVE 2		 TO USER-ACTION.
	     MOVE 3		 TO USER-SETTING.
	     MOVE 5		 TO USER-NUMBER.
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
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "MEMDEB"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-MEMDEB.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT (PAR-RT-HD = X"00000000000000000000")
	       MOVE PAR-RT-HD	 TO WS-RT-HD.
044780	     GO TO ZA999-EXIT.
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
042900 ZB000-ERROR	     SECTION 81.
      
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
041760	   ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 62
041860	       MOVE W02-MEMDEB	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE MEM-KEY	 TO WS-KEYX.
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
