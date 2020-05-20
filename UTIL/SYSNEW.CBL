      ******************************************************************
      *                                                                *
      *    ******   **	  **   ******	**    **  ********  **	  **   *
      *   **	**  **	  **  **    **	***   **  **	    **	  **   *
      *   **	     **  **   **	****  **  **	    **	  **   *
      *    ******     ****     ******	** ** **  *****     **	  **   *
      * 	**     **	    **	**  ****  **	    ** ** **   *
      *   **	**     **     **    **	**   ***  **	    ***  ***   *
      *    ******      **      ******	**    **  ********  **	  **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     B U S I N E S S  S Y S T E M  N E W   C O M P A N Y        *
      *                                                                *
      *     VERSION 8.15.01 - August 2011			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Feb 2007	- New file (DEBDEP/AR_REST.DAT) - Debtor sales *
      * 		  restrictions file has been included which    *
      * 		  will allow for departments to de excluded or *
      * 		  only selected departments allowed. This will *
      * 		  allow the user the abillity to allow or bar  *
      * 		  sales of selected departments.	       *
      * 							       *
      *  Jan 2008	- New file (DEBALT) - Debtor alternate index   *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Debtor name.		       *
      * 							       *
      * August 2008    - New file (STKALT) - Stock alternate index     *
      * 		 included for lookups, using any word con-     *
      * 		 tained in the Stock description.	       *
      * 							       *
      *  March 2010	- Include facility to print to a USB printer   *
      * 		  via the Windows print using the relavent     *
      * 		  print API's supplied with MF COBOL. Allow    *
      * 		  BMP files in APACFILE (INVBMP & STMBMP).     *
      * 							       *
      * August 2011 - Amended MEMO file to have 6 character Account    *
      * 	      Reference code.Increased sequence to 4 digits.   *
      * 	      Included Recovery routines and file locking for  *
      * 	      this program. (Creditors)			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     SYSNEW.
000030 AUTHOR.         J W LEMMON  (APAC).
000040 DATE-WRITTEN.   OCTOBER 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2010
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

       COPY APACFILE.SL.

000120 COPY SYSTEM.SL.

000170	   SELECT NEWSYS  ASSIGN DISK
                          LOCK AUTOMATIC
000180                    STATUS WS-STATUS
000190                    ACCESS RANDOM
000200                    ORGANIZATION RELATIVE
000210                    RELATIVE WS-SECKEY.

000120 COPY APCCOM.SL.

000140 DATA DIVISION.
000150 FILE SECTION.

       COPY APACFIDS.FD.

       COPY APACFILE.FD.

000170 COPY SYSTEM.FD.

      /*****************************************************************
      *                                                                *
      *    THIS FILE CONTAINS:                                         *
      *                                                                *
      *       RECORD 1:                                                *
      *       ========                                                 *
      *                THE 256 ASCII CODES WHICH ARE ACCESSED BY       *
      *        USING THE DECIMAL CODE.                                 *
      *                                                                *
      *                THESE CODES ARE USED TO ENABLE THE SYSTEM TO    *
      *        PERFORM SPECIAL FUNCTIONS WHICH REQUIRE CHARACTERS THAT *
      *        ARE UNPRINTABLE AND ARE THEREFOR UNABLE TO BE CODED     *
      *        INTO A COBOL PROGRAM.                                   *
      *                                                                *
      *       RECORD 2:                                                *
      *       ========                                                 *
      *                VARIOUS DETAILS OF THE SYSTEM.                  *
      *                 1) WHICH PROGRAMS ARE AVAILABLE.               *
      *                 2) WHICH FILES ARE USED BY THE SYSTEM.         *
      *                 3) WHERE THE FILES ARE LOCATED.                *
      *                 4) SECURITY DETAILS OF THE SYSTEM.             *
      *                                                                *
      ******************************************************************
000240 FD  NEWSYS      LABEL RECORD STANDARD
000250		       VALUE OF FILE-ID W02-NEWSYS.
000260 01  NEW-REC1.
000280	   03  NEW-ASCII	   PIC	X(01) OCCURS 256.
000290 01  NEW-REC2.
000310	   03  NEW-NUM		   PIC	X(07).
000320	   03  NEW-EXT		   PIC	X(03).
000330	   03  NEW-DISK 	   PIC	X(01).
	   03  NEW-INITIAL	   PIC	9(06)	  COMP-5.
	   03  NEW-JUNIOR	   PIC	X(01).
	   03  NEW-DEBTORS	   PIC	9(06)	  COMP-5.
	   03  NEW-STOCK	   PIC	9(06)	  COMP-5.
	   03  NEW-TERMS	   PIC	9(04)	  COMP-5.
	   03  NEW-VER		   PIC	9(02)V999 COMP-3.
	   03  NEW-VDATE	   PIC	9(06)	  COMP-3.
000850	   03  FILLER		   PIC	X(66).
	   03  NEW-DEM		   PIC	9(02)	  COMP-5.
	   03  NEW-COUNT	   PIC	9(02)	  COMP-5.
000880	   03  NEW-PRNCOND.
000890	       05  NEW-CHAR1	   PIC	X(01) OCCURS 4.
000900	   03  NEW-PRNNORM.
000910	       05  NEW-CHAR2	   PIC	X(01) OCCURS 4.
000740	   03  NEW-PRNEXPD.
000750	       05  NEW-CHAR3	   PIC	X(01) OCCURS 4.
000760	   03  NEW-PRNECAN.
000770	       05  NEW-CHAR4	   PIC	X(01) OCCURS 4.
	   03  NEW-PAGE48.
	       05  NEW-CHAR5	   PIC	X(01) OCCURS 4.
	   03  NEW-PAGE66.
	       05  NEW-CHAR6	   PIC	X(01) OCCURS 4.
	   03  NEW-10CPI.
	       05  NEW-CHAR7	   PIC	X(01) OCCURS 4.
	   03  NEW-12CPI.
	       05  NEW-CHAR8	   PIC	X(01) OCCURS 4.
	   03  NEW-17CPI.
	       05  NEW-CHAR9	   PIC	X(01) OCCURS 4.
	   03  NEW-DBLP.
	       05  NEW-CHAR10	   PIC	X(01) OCCURS 4.
	   03  NEW-DCAN.
	       05  NEW-CHAR11	   PIC	X(01) OCCURS 4.
	   03  NEW-P2-CON.
	       05  NEW-CHAR12	   PIC	X(01) OCCURS 4.
	   03  NEW-P2-CCON.
	       05  NEW-CHAR13	   PIC	X(01) OCCURS 4.
	   03  NEW-P2-EXP.
	       05  NEW-CHAR14	   PIC	X(01) OCCURS 4.
	   03  NEW-P2-CEXP.
	       05  NEW-CHAR15	   PIC	X(01) OCCURS 4.
	   03  NEW-P2-PGE48.
	       05  NEW-CHAR16	   PIC	X(01) OCCURS 4.
	   03  NEW-P2-PGE66.
	       05  NEW-CHAR17	   PIC	X(01) OCCURS 4.
	   03  NEW-P2-10CPI.
	       05  NEW-CHAR18	   PIC	X(01) OCCURS 4.
	   03  NEW-P2-12CPI.
	       05  NEW-CHAR19	   PIC	X(01) OCCURS 4.
	   03  NEW-P2-17CPI.
	       05  NEW-CHAR20	   PIC	X(01) OCCURS 4.
	   03  NEW-P2-DBLP.
	       05  NEW-CHAR21	   PIC	X(01) OCCURS 4.
	   03  NEW-P2-DCAN.
	       05  NEW-CHAR22	   PIC	X(01) OCCURS 4.
000860	   03  NEW-DATE 	   PIC	9(06)	 COMP-3.
000920	   03  NEW-PASS 	   PIC	9(06).
000930	   03  NEW-COMP 	   PIC	X(40).
000940	   03  NEW-MODULES.
000950	       05  NEW-DTP	   PIC	9(01).
000960	       05  NEW-STP	   PIC	9(01).
000970	       05  NEW-HPD	   PIC	9(01).
000980	       05  NEW-CRP	   PIC	9(01).
000990	       05  NEW-GLP	   PIC	9(01).
001000	       05  NEW-POS	   PIC	9(01).
001010	       05  NEW-JCP	   PIC	9(01).
001020	       05  NEW-VHP	   PIC	9(01).
	       05  NEW-WAG	   PIC	9(01).
	       05  NEW-SAL	   PIC	9(01).
               05  FILLER          PIC  X(10).
001030	   03  NEW-PRGM  REDEFINES NEW-MODULES.
001040	       05  NEW-PMODS	   PIC	X(20).

000170 COPY APCCOM.FD.

      /
000190 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000200 77  WS-S1	  PIC 9(04)	COMP-5.
000210 77  WS-S2	  PIC 9(04)	COMP-5.
000220 77  WS-S3	  PIC 9(04)	COMP-5.
000230 77  WS-S4	  PIC 9(04)	COMP-5.
000240 77  WS-C1           PIC 9(04)     COMP-5.
000250 77  WS-C2           PIC 9(04)     COMP-5.
000260 77  WS-C3           PIC 9(04)     COMP-5.
000270 77  WS-C4           PIC 9(04)     COMP-5.
000710 77  WS-IXS1	       PIC 9(04)     COMP-5.
000280 77  WS-SECKEY       PIC 9(04)     COMP-5.
000280 77  WS-PARKEY       PIC 9(04)     COMP-5.
000290 77  WS-SERIAL       PIC 9(07)     VALUE ZERO.
000300 77  WS-NUM          PIC Z(04)9.
000310 77  WS-MES          PIC X(11) VALUE "INITIALIZED".
000330 77  WS-ERROR        PIC 9(01)     VALUE ZERO.
000340 77  WS-CHAR         PIC X(01).
000350 77  WS-STOCK        PIC X(01)     VALUE "Y".
000360 77  WS-DEBTOR       PIC X(01)     VALUE "Y".
000370 77  WS-CREDITOR     PIC X(01)     VALUE "Y".
000380 77  WS-GLEDGER      PIC X(01)     VALUE "Y".
000390 77  WS-HPDEBT       PIC X(01)     VALUE "Y".
000400 77  WS-POSALE       PIC X(01)     VALUE "Y".
000410 77  WS-VEHCLE       PIC X(01)     VALUE "Y".
000420 77  WS-OPTION       PIC X(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.

000980 01  W00-ALPHA-SER.
000990     03  W00-SERIAL      PIC X(07).

      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC  X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC  X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002450	   03  WS-MID-LNE.
002460	       05  WS-MCHR PIC	X(01) OCCURS 80.
002470     03  WS-BOT-LNE.
002480         05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
001380	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
001400	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
001410         05  FILLER      PIC  X(01).
001420         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC  X(01) OCCURS 80.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "SYS".
	   03  WS-PROG	       PIC  X(03) VALUE "NEW".

       COPY WS.WS.

000430 01  W00-FILES.
000440     03  W00-FREC            OCCURS 3.
000450         05  W00-FID         OCCURS 5.
000460             07  W00-DISC    PIC X(01).
000470             07  W00-FNME    PIC X(08).
000480             07  W00-SUFF    PIC X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY APACFILE.ID.

000930	   03  W02-SYSTEM.
000950         05  FILLER      PIC  X(05) VALUE "SYST_".
000960	       05  W02-EXT     PIC  X(03) VALUE "NEW".
               05  FILLER      PIC  X(04) VALUE ".APC".

	   03  W02-NEWSYS.
000020	       05  W02-PTH23   PIC  X(03).
000030	       05  FILLER      PIC  X(16) VALUE "_DATA\SYSTEM.DAT".
	       05  FILLER      PIC  X(01) VALUE X"00".

	   03  W02-COMP-PTH.
	       05  W02-PATH    PIC  X(03).
	       05  FILLER      PIC  X(05) VALUE "_DATA".
	       05  FILLER      PIC  X(01) VALUE X"00".

	   03  W02-WORK-PTH.
	       05  W02-PATH2   PIC  X(03).
	       05  FILLER      PIC  X(05) VALUE "_WORK".
	       05  FILLER      PIC  X(01) VALUE X"00".

	   03  W02-OFFLINE.
	       05  FILLER      PIC  X(07) VALUE "OFFLINE".
	       05  FILLER      PIC  X(01) VALUE X"00".

	   03  W02-INVBMP.
	       05  W02-INV-ID  PIC  X(03).
	       05  FILLER      PIC  X(08) VALUE "_INV.BMP".

	   03  W02-STMBMP.
	       05  W02-STM-ID  PIC  X(03).
	       05  FILLER      PIC  X(08) VALUE "_STM.BMP".

000490 01  W05-PRINT.
000500     03  W05-CONTRACT.
000510         05  W05-CHAR1       PIC X(01) OCCURS 4.
000520     03  W05-NORMAL.
000530         05  W05-CHAR2       PIC X(01) OCCURS 4.
000570 01  W15-HEX.
000580     03  W15-ASCII           PIC X(01) OCCURS 256.
000590 01  W20-COND.
000600     03  W20-DEC             PIC 9(03) OCCURS 4.
000610 01  W25-NORM.
000620     03  W25-DEC             PIC 9(03) OCCURS 4.
000630 01  W30-ASCII.
000640     03  W30-GRP.
000650         05  W30-DEC         PIC 9(05) COMP-0.
000660 01  W30-GRP1           REDEFINES  W30-ASCII.
000670         05  W30-1           PIC X(01). 
000680         05  W30-CHAR        PIC X(01).

       COPY W40.WS.

       COPY FUNCTION.WS.

       01  W75-KEYS.
	   03  W75-S	       PIC 9(02) COMP-5.
	   03  W75-S1	       PIC 9(02) COMP-5.
	   03  W75-S2	       PIC 9(02) COMP-5.
	   03  W75-COM	       PIC X(03) OCCURS 18.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
      *   ******    ******   *******   ********  ********  **     ** 
      *  **    **  **    **  **    **  **        **        ***    ** 
      *  **        **        **   **   **        **        ** *   ** 
      *   ******   **        ******    *****     *****     **  *  ** 
      *        **  **        **   **   **        **        **   * ** 
      *  **    **  **    **  **    **  **        **        **    *** 
      *   ******    ******   **    **  ********  ********  **     ** 
      *                                                              
001940 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S-COM.
	   03  LINE  6 COLUMN 29 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  7 COLUMN 29 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³    C O M P A N Y   C O D E
      -	       "L O O K U P   ³".
	   03  LINE  8 COLUMN 29 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÃÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 21 COLUMN 29 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÀÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  SD-COM.
	   03  COLUMN 30 FOREGROUND-COLOR 5
			 BACKGROUND-COLOR 7 PIC X(03) FROM COM-CODE.
	   03  COLUMN 34 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
			 PIC X(40) FROM COM-NAME.

       01  SA-COM.
	   03  LINE  4 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(40) USING COM-NAME AUTO.

004870 01  S99.
004880	   03  LINE 25 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
000830				 VALUE "Correct ".
	   03	       COLUMN 37 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
004890				 VALUE "ENTER".
	   03	       COLUMN 42 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " - ".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 46 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " if incorrect".

       COPY ERROR.CRT.

      /
000690 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
000700 AA000-MAIN              SECTION.
000710 AA00.
003240	   IF (LS0-DBLEV < 3) AND (LS0-STLEV < 3) AND
	      (LS0-CRLEV < 3) AND (LS0-GLLEV < 3)
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA999.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
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
	       GO TO AA00A.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.

       AA00A.
	     OPEN I-O APCCOM.
	   IF WS-STATUS NOT = "00"
	       OPEN OUTPUT APCCOM
	       INITIALIZE COM-RECORD
	       MOVE "APC"	 TO COM-CODE
	       WRITE COM-RECORD
	       CLOSE APCCOM
	       OPEN I-O APCCOM.

       AA02.
000720	     DISPLAY CLR-SCREEN.
000730	     DISPLAY "CREATE SYSTEM FILE FOR NEW COMPANY" AT 0225
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
003340	     DISPLAY "Company Code" AT 0412.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 17		 TO SHADE-COL.
	     MOVE 47		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
		     AT 0615 WITH BACKGROUND-COLOR 2 FOREGROUND-COLOR 0.
	     DISPLAY "³ The code is used to identify the company. The ³"
		     AT 0715 WITH BACKGROUND-COLOR 2 FOREGROUND-COLOR 0.
	     DISPLAY "³ code is made up of three characters and must  ³"
		     AT 0815 WITH BACKGROUND-COLOR 2 FOREGROUND-COLOR 0.
	     DISPLAY "³ contain at least 1 (one) alphabetic character ³"
		     AT 0915 WITH BACKGROUND-COLOR 2 FOREGROUND-COLOR 0.
	     DISPLAY "³ Use a meaningfull code. eg JWL for a company  ³"
		     AT 1015 WITH BACKGROUND-COLOR 2 FOREGROUND-COLOR 0.
	     DISPLAY "³ with the name JWL Software or CSS for a name  ³"
		     AT 1115 WITH BACKGROUND-COLOR 2 FOREGROUND-COLOR 0.
	     DISPLAY "³ Computer Sales and Software.                  ³"
		     AT 1215 WITH BACKGROUND-COLOR 2 FOREGROUND-COLOR 0.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		     AT 1315 WITH BACKGROUND-COLOR 2 FOREGROUND-COLOR 0.
	     PERFORM SCREEN-SHADOW.
	     MOVE SPACES	 TO W02-PTH23.
	     DISPLAY "F2" AT 2502 WITH FOREGROUND-COLOR 6 HIGHLIGHT
				       BACKGROUND-COLOR 3
		     " to view existing codes, "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "F3" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     " to amend, "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to exit" WITH FOREGROUND-COLOR 1
					BACKGROUND-COLOR 3.

       AA05.
003380	     ACCEPT W02-PTH23 AT 0425
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO AA30
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO AA05
		 WHEN F2-KEY
018810		     PERFORM COMPANY-LOOKUP
		     GO TO AA05
		 WHEN F3-KEY
018810		     PERFORM COMPANY-AMEND
		     GO TO AA02
		 WHEN OTHER
		     CALL X"E5"
		     GO TO AA05
	       END-EVALUATE.
	     MOVE 3		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W02-PTH23
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     DISPLAY CLEAR-L25.

       AA10.
012180	     DISPLAY S99.
012190	     ACCEPT WS-OPTION AT 2560
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     DISPLAY CLEAR-L25.
	   IF WS-OPTION = "N"
               GO TO AA05.
	   IF W02-PTH23 NUMERIC
	       DISPLAY "Company code may not be numeric" AT 2512
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			BACKGROUND-COLOR 3
	       GO TO AA05.
001060	     OPEN INPUT SECUR.
006200     IF RUNTIME-ERROR
               IF FLE-LOCKED
006210         DISPLAY "Files locked - try later - Press ENTER" AT 2412
006220			WITH FOREGROUND-COLOR 6 HIGHLIGHT
006230	       ACCEPT WS-OPTION AT 2451
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO
006240	       GO TO AA99.
006250     IF WS-STATUS = "30"
006260	       DISPLAY "Invalid or file not on disk" AT 2512
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
006270	       GO TO AA99.
	     INITIALIZE COM-RECORD.
	     MOVE W02-PTH23	 TO COM-CODE.
	     READ APCCOM.
	   IF WS-STATUS = "00"
	       DISPLAY "Existing Company code" AT 2504
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
	       GO TO AA05.
	     MOVE W02-PTH23	 TO W02-PATH   W02-PATH2
				    W02-INV-ID W02-STM-ID.
	     CALL "CBL_CREATE_DIR" USING W02-COMP-PTH
				   RETURNING WS-STATUS.
	     CALL "CBL_CREATE_DIR" USING W02-WORK-PTH
				   RETURNING WS-STATUS.
	     OPEN OUTPUT NEWSYS.
	   IF NOT (WS-STATUS = "00")
006260	       DISPLAY "Disk problem or invalid file name " AT 2504
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
			W02-NEWSYS
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 3
006270	       GO TO AA99.
006280	     MOVE 1		 TO WS-SECKEY.
000860 AA15.
006300       READ SECUR.
006310     IF WS-STATUS = "00"
006320         GO TO AA18.
006330     IF RUNTIME-ERROR
006340         IF RECORD-LOCKED
006350             GO TO AA15.
006360	     DISPLAY "Disk error - AA15" AT 2512
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 3.
006370	     GO TO AA99.
       AA18.
000750	     MOVE SEC-REC1	 TO NEW-REC1.
006400	     WRITE NEW-REC1.
	   IF NOT (WS-STATUS = "00")
006260	       DISPLAY "Disk problem " AT 2512
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
			W02-NEWSYS
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 3
006270	       GO TO AA99.
006410	     ADD 1		 TO WS-SECKEY.
       AA20.
006300	     READ SECUR.
006310     IF WS-STATUS = "00"
006320	       GO TO AA25.
006330     IF RUNTIME-ERROR
006340         IF RECORD-LOCKED
006350		   GO TO AA20.
006360	     DISPLAY "Disk error - AA20" AT 2512
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 3.
006370	     GO TO AA99.

       AA25.
000750	     MOVE SEC-REC1	 TO NEW-REC1.
	     MOVE W02-PTH23	 TO NEW-EXT.
006400	     WRITE NEW-REC1.
	   IF NOT (WS-STATUS = "00")
006260	       DISPLAY "Disk problem " AT 2512
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
			W02-NEWSYS
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 3
006270	       GO TO AA99.
	     WRITE COM-RECORD.
      *    *************************************************************
      *       N E T W O R K   O R   S T A N D - A L O N E   F I L E S
      *     File names and paths of data files for the system. These
      *     could be on a Network or Local drive.
      *    *************************************************************
	     MOVE "SYSTEMS\"	 TO W02-L-OR-N.
	     PERFORM APACFILES.
	     PERFORM APAC-SETUP-PATHS.
      *    *************************************************************
      * 	 O F F - L I N E   F I L E S   ( N E T W O R K S)
      *     File names and paths of data files for the system. These
      *     will be on the local drive. For work stations with no hard
      *     drive the system will not have any off-line files.
      *    *************************************************************
	     MOVE "OFFLINE\"	 TO W02-L-OR-N.
	     PERFORM APACFILES.
	     PERFORM APAC-SETUP-PATHS.
001550	     DISPLAY "New Code added to Company file" AT 2504
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3.
	     CLOSE APCCOM
		   SECUR
		   NEWSYS.
001560	     GO TO AA99.

       AA30.
	     DISPLAY "Program terminated by Operator" AT 2512
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3.

       AA99.
	     DISPLAY "Press any key" AT 2566
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       AA999.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

011433 COPY CLEAR.CRT.

       COPY APAC.HLP.

       COMPANY-AMEND   SECTION 50.
       COMP-AMEND-SCREEN.
000720	     DISPLAY CLR-SCREEN.
000730	     DISPLAY "AMEND COMPANY DETAILS" AT 0230
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
003340	     DISPLAY "Company Code" AT 0412.
	     MOVE SPACES	 TO W02-PTH23.

       COMP-GET-EXT.
003380	     ACCEPT W02-PTH23 AT 0425
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO COMPANY-AMEND-EXIT
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO COMP-GET-EXT
		 WHEN OTHER
		     CALL X"E5"
		     GO TO COMP-GET-EXT
	       END-EVALUATE.
	     MOVE 3		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W02-PTH23
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     DISPLAY CLEAR-L25.
	   IF W02-PTH23 NUMERIC
	       MOVE "Company code may not be numeric"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO COMP-GET-EXT.
	     INITIALIZE COM-RECORD.
	     MOVE W02-PTH23	 TO COM-CODE.
	     READ APCCOM.
	   IF NOT (WS-STATUS = "00")
	       MOVE "Company code not on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO COMP-GET-EXT.
	     DISPLAY "F9" AT 2502 WITH FOREGROUND-COLOR 6 HIGHLIGHT
				       BACKGROUND-COLOR 3
		     " to delete company code"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       COMP-AMEND-NAME.
	     ACCEPT SA-COM.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO COMP-AMEND-SCREEN
		 WHEN F9-KEY
000180		     DELETE APCCOM
		     GO TO COMP-AMEND-SCREEN
		 WHEN OTHER
		     CALL X"E5"
		     GO TO COMP-AMEND-NAME
	       END-EVALUATE.
	     REWRITE COM-RECORD.
	     GO TO COMP-AMEND-SCREEN.

       COMPANY-AMEND-EXIT.
	     EXIT.

       COMPANY-LOOKUP  SECTION 51.
012520 COMPANY-LUP-SCREEN.
011050	     PERFORM SAVE-SCREEN.
	     DISPLAY S-COM.

       COMPANY-LUP-CLR-SCREEN.
	     MOVE 0929		 TO CPOS.
	     PERFORM COMPANY-CRT-EDGE UNTIL CLIN = 21.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 44		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
000340	     DISPLAY CLEAR-L25.
000350	     MOVE 9		 TO CLIN.
000360	     MOVE 1		 TO W75-S W75-S1.

       COMPANY-LUP-TAB-LOOP.
	     MOVE SPACES	 TO W75-COM (W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
	       GO TO COMPANY-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.
	     MOVE "  !" 	 TO COM-CODE.
006850	     START APCCOM
	       KEY >= COM-CODE.
006860	   IF NOT (WS-STATUS = "00")
	       GO TO COMPANY-NO-RECORD.

       COMPANY-GET-COM-RECORD.
	   IF WS-OPTION = "J"
	       MOVE SPACE	 TO WS-OPTION
	       GO TO COMPANY-SKIP-COM-READ.
006850	     READ APCCOM NEXT	 WITH IGNORE LOCK.
009900	   IF NOT (WS-STATUS = "00")
	       SUBTRACT 1	 FROM W75-S
	       GO TO COMPANY-FULL-SCREEN.

       COMPANY-SKIP-COM-READ.
	     MOVE COM-CODE	 TO W75-COM (W75-S).
	     DISPLAY SD-COM AT LINE CLIN.
027700	   IF CLIN < 20
027710	       ADD 1		 TO CLIN W75-S
027720	       GO TO COMPANY-GET-COM-RECORD.

       COMPANY-FULL-SCREEN.
001020	     DISPLAY " " AT 2503 WITH FOREGROUND-COLOR 6 HIGHLIGHT
					BACKGROUND-COLOR 3
001030		     " highlight - " WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "PgUp" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     " - " WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       COMPANY-SAVE-FULL-SCREEN.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 0		 TO W75-S1.
	     MOVE 0930		 TO CPOS.

       COMPANY-CRT-ATTRIB.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.

       COMPANY-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO COMPANY-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO COMPANY-CHK-NO-KEYS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO COMPANY-RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO COMPANY-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO COMPANY-RESTORE-SCREEN
		 WHEN PAGE-DOWN
001340		     GO TO COMPANY-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO COMPANY-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO COMPANY-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO COMPANY-GET-USER-OPT.

001240 COMPANY-GET-NEXT-PAGE.
	   IF W75-S < 12
	       GO TO COMPANY-GET-USER-OPT.

       COMPANY-NEW-PAGE.
	     MOVE 0929		 TO CPOS.
	     PERFORM COMPANY-CRT-EDGE UNTIL CLIN = 21.
	     MOVE 9		 TO CLIN.
	     MOVE LOW-VALUES	 TO W75-KEYS.
	     MOVE 1		 TO W75-S W75-S1.
	     PERFORM COMPANY-LUP-TAB-LOOP.
027830	     GO TO COMPANY-GET-COM-RECORD.

       COMPANY-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
	     MOVE "J"		 TO WS-OPTION.

       COMPANY-GET-PREV-LOOP.
006850	     READ APCCOM PREVIOUS
				 WITH IGNORE LOCK.
009900	   IF NOT (WS-STATUS = "00")
	       MOVE SPACE	 TO WS-OPTION
	       GO TO COMPANY-NEW-PAGE.
	   IF WS-IXS1 < 24
               ADD 1             TO WS-IXS1
	       GO TO COMPANY-GET-PREV-LOOP.
	     GO TO COMPANY-NEW-PAGE.

027970 COMPANY-NO-RECORD.
027730	     DISPLAY "No company codes - Press " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "ANY" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     " key"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     GO TO COMPANY-RESTORE-SCREEN.

       COMPANY-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1 FROM W75-S1 CLIN.
	     GO TO COMPANY-HIGHLIGHT.

       COMPANY-CHK-NO-KEYS.
	   IF W75-S1 = ZERO
	       MOVE 1		 TO W75-S1
	       GO TO COMPANY-HIGHLIGHT.
           IF W75-S1 < W75-S
	       ADD 1		 TO W75-S1 CLIN.

       COMPANY-HIGHLIGHT.
	     PERFORM COMPANY-CRT-ATTRIB.
	     DISPLAY W43-DET3 (CLIN) AT CPOS
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 7.
	     GO TO COMPANY-GET-USER-OPT.

       COMPANY-CRT-EDGE.
012890	     DISPLAY "³   ³                                        ³"
		      AT CPOS WITH BACKGROUND-COLOR 7
				   FOREGROUND-COLOR 0
019260	     ADD 1		 TO CLIN.

       COMPANY-RESTORE-SCREEN.
011320	     PERFORM RESTORE-SCREEN.

       COMPANY-LOOKUP-EXIT.
             EXIT.

       APACFILES       SECTION 52.
       APAC-FILES-INIT.
	     MOVE W02-PTH23	 TO W02-SYSPTH.
	     OPEN OUTPUT APACFILE.
	     CLOSE APACFILE.
	     PERFORM APAC-CREATE-NEW THRU APAC-CREATE-NEW-EXIT.
	     CLOSE APACFILE.
	     GO TO APACFILES-EXIT.

       APAC-CREATE-NEW.
	     OPEN I-O APACFILE.
	     MOVE "ASSETT"	 TO APAC-KEY.
	     MOVE "ASSETT.DAT"	 TO APAC-NAME.
	     MOVE 11		 TO APAC-FILE.
	     MOVE SPACES	 TO APAC-PATH.
	     STRING W02-PTH23 DELIMITED SIZE
		    "_DATA\" DELIMITED SIZE
		    INTO APAC-PATH.

       APAC-WRITE-APACFILE.
	     WRITE APAC-RECORD1.

       APAC-DATA-FILES.
	     MOVE "AUDITF"	 TO APAC-KEY.
	     MOVE "AUDIT.DAT"	 TO APAC-NAME.
	     MOVE 16		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "BARCOD"	 TO APAC-KEY.
	     MOVE "BARCODE.DAT"	 TO APAC-NAME.
	     MOVE 57		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PURDEX"	 TO APAC-KEY.
	     MOVE "CARDEXPR.DAT" TO APAC-NAME.
	     MOVE 56		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CARDEX"	 TO APAC-KEY.
	     MOVE "CARDEXSL.DAT" TO APAC-NAME.
	     MOVE 43		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CATITM"	 TO APAC-KEY.
	     MOVE "CATITM.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CATLOG"	 TO APAC-KEY.
	     MOVE "CATLOG.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CHEQUE"	 TO APAC-KEY.
	     MOVE "CHEQUE.DAT"	 TO APAC-NAME.
	     MOVE 36		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CHQBATCH"	 TO APAC-KEY.
	     MOVE "CHQBCH.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CHQBCH"	 TO APAC-KEY.
	     MOVE "CHQBCH_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "NETWORK"	 TO APAC-KEY.
	     MOVE "CONTROL.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CRDMEM"	 TO APAC-KEY.
	     MOVE "AP_MEMO.DAT"	 TO APAC-NAME.
	     MOVE 6		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CREDIT"	 TO APAC-KEY.
	     MOVE "AP_ACNT.DAT"	 TO APAC-NAME.
	     MOVE 7		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CRTRAN"	 TO APAC-KEY.
	     MOVE "AP_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 8		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CSBOOK"	 TO APAC-KEY.
	     MOVE "CSBOOK.DAT"	 TO APAC-NAME.
	     MOVE 29		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CSTRAN"	 TO APAC-KEY.
	     MOVE "CSTRAN.DAT"	 TO APAC-NAME.
	     MOVE 30		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DBFOTO"	 TO APAC-KEY.
	     MOVE "AR_CONT.DAT"	 TO APAC-NAME.
	     MOVE 3		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DBTRAN"	 TO APAC-KEY.
	     MOVE "AR_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 4		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBALT"	 TO APAC-KEY.
	     MOVE "AR_NAME.DAT"	 TO APAC-NAME.
	     MOVE 47		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBCOL"	 TO APAC-KEY.
	     MOVE "AR_COL.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBDEP"	 TO APAC-KEY.
	     MOVE "AR_REST.DAT"  TO APAC-NAME.
	     MOVE 33		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBHIS"	 TO APAC-KEY.
	     MOVE "AR_HIST.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBMEM"	 TO APAC-KEY.
	     MOVE "AR_MEMO.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBORD"	 TO APAC-KEY.
	     MOVE "AR_ORD.DAT"	 TO APAC-NAME.
	     MOVE 53		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBPRF"	 TO APAC-KEY.
	     MOVE "DEBPRF.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBTOR"	 TO APAC-KEY.
	     MOVE "AR_ACNT.DAT"	 TO APAC-NAME.
	     MOVE 2		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEPART"	 TO APAC-KEY.
	     MOVE "DEPART.DAT"	 TO APAC-NAME.
	     MOVE 38		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "EGGSLE"	 TO APAC-KEY.
	     MOVE "EGGSLE.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GACCNT"	 TO APAC-KEY.
	     MOVE "GL_ACNT.DAT"	 TO APAC-NAME.
	     MOVE 12		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GARTEE"	 TO APAC-KEY.
	     MOVE "GARTEE.DAT"	 TO APAC-NAME.
	     MOVE 9		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLDALT"	 TO APAC-KEY.
	     MOVE "GL_NAME.DAT"	 TO APAC-NAME.
	     MOVE 50		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLBHDR"	 TO APAC-KEY.
	     MOVE "GL_BHDR.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLBTCH"	 TO APAC-KEY.
	     MOVE "GLBTCH_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLCHEQ"	 TO APAC-KEY.
	     MOVE "GLCHEQ_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLDEPO"	 TO APAC-KEY.
	     MOVE "GLDEPO_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLINST"	 TO APAC-KEY.
	     MOVE "GL_INST.DAT"	 TO APAC-NAME.
	     MOVE 15		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLTRAN"	 TO APAC-KEY.
	     MOVE "GL_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 13		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GPDCHQ"	 TO APAC-KEY.
	     MOVE "GL_PDCHQ.DAT" TO APAC-NAME.
	     MOVE 45		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "INVBMP"	 TO APAC-KEY.
	     MOVE W02-INVBMP	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "INVOIC"	 TO APAC-KEY.
	     MOVE "INVOICE.DAT"	 TO APAC-NAME.
	     MOVE 22		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "ITEMS"	 TO APAC-KEY.
	     MOVE "ITEMS.DAT"	 TO APAC-NAME.
	     MOVE 57		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JCHIST"	 TO APAC-KEY.
	     MOVE "JCHIST.DAT"	 TO APAC-NAME.
	     MOVE 58		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBCST"	 TO APAC-KEY.
	     MOVE "JOBLIN.DAT"	 TO APAC-NAME.
	     MOVE 25		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBAUD"	 TO APAC-KEY.
	     MOVE "JOBAUD.DAT"	 TO APAC-NAME.
	     MOVE 24		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JCLINE"	 TO APAC-KEY.
	     MOVE "JCLINE.DAT"	 TO APAC-NAME.
	     MOVE 59		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBHDR"	 TO APAC-KEY.
	     MOVE "JOBHDR.DAT"	 TO APAC-NAME.
	     MOVE 21		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBREQ"	 TO APAC-KEY.
	     MOVE "JOBREQ.DAT"	 TO APAC-NAME.
	     MOVE 26		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBRMK"	 TO APAC-KEY.
	     MOVE "JOBRMK.DAT"	 TO APAC-NAME.
	     MOVE 27		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBSTAT"	 TO APAC-KEY.
	     MOVE "JOBSTAT.DAT"	 TO APAC-NAME.
	     MOVE 60		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBTECH"	 TO APAC-KEY.
	     MOVE "JOBTECH.DAT"	 TO APAC-NAME.
	     MOVE 61		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBWAR"	 TO APAC-KEY.
	     MOVE "JOBWAR.DAT"	 TO APAC-NAME.
	     MOVE 28		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "LEDTRF"	 TO APAC-KEY.
	     MOVE "LEDTRF.DAT"	 TO APAC-NAME.
	     MOVE 40		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "LEDTRG"	 TO APAC-KEY.
	     MOVE "LTRG_XXX.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PARAM"	 TO APAC-KEY.
	     MOVE "PARAM.DAT"	 TO APAC-NAME.
	     MOVE 1		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PDCHEQ"	 TO APAC-KEY.
	     MOVE "PDCHEQ.DAT"	 TO APAC-NAME.
	     MOVE 44		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PICSLIP"	 TO APAC-KEY.
	     MOVE "PICSLP.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PORDER"	 TO APAC-KEY.
	     MOVE "PORDER.DAT"	 TO APAC-NAME.
	     MOVE 10		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "POSPLU"	 TO APAC-KEY.
	     MOVE "POSPLU.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "ROUTE"	 TO APAC-KEY.
	     MOVE "ROUTE.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PSLIPS"	 TO APAC-KEY.
	     MOVE "PSLIPS.DAT"	 TO APAC-NAME.
	     MOVE 35		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SHARED"	 TO APAC-KEY.
	     MOVE "SHARED.DAT"	 TO APAC-NAME.
	     MOVE 33		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SORDER"	 TO APAC-KEY.
	     MOVE "SORDER.DAT"	 TO APAC-NAME.
	     MOVE 17		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SPARTS"	 TO APAC-KEY.
	     MOVE "IN_PARTS.DAT" TO APAC-NAME.
	     MOVE 20		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STKALT"	 TO APAC-KEY.
	     MOVE "IN_DESC.DAT"	 TO APAC-NAME.
	     MOVE 49		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STKDEX"	 TO APAC-KEY.
	     MOVE "IN_ADJ.DAT"	 TO APAC-NAME.
	     MOVE 46		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STKMEM"	 TO APAC-KEY.
	     MOVE "IN_MEMO.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STMBMP"	 TO APAC-KEY.
	     MOVE W02-STMBMP	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STOCKF"	 TO APAC-KEY.
	     MOVE "INVENT.DAT"	 TO APAC-NAME.
	     MOVE 5		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SYSTEM"	 TO APAC-KEY.
	     MOVE "SYSTEM.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SYSUSE"	 TO APAC-KEY.
	     MOVE "USERS.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "TERMINAL"	 TO APAC-KEY.
	     MOVE "TRMNAL.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "TRFRIN"	 TO APAC-KEY.
	     MOVE "TRFRIN.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "TRFOUT"	 TO APAC-KEY.
	     MOVE "TRFOUT.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "TXTRAN"	 TO APAC-KEY.
	     MOVE "TX_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 19		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "UNTSLE"	 TO APAC-KEY.
	     MOVE "UNTSLE.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "VEHICLE"	 TO APAC-KEY.
	     MOVE "VEHICLE.DAT"	 TO APAC-NAME.
	     MOVE 54		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "VSTOCK"	 TO APAC-KEY.
	     MOVE "V_INVENT.DAT" TO APAC-NAME.
	     MOVE 55		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "WARHSE"	 TO APAC-KEY.
	     MOVE "WARHSE.DAT"	 TO APAC-NAME.
	     MOVE 51		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "WSTOCK"	 TO APAC-KEY.
	     MOVE "W_INVENT.DAT" TO APAC-NAME.
	     MOVE 52		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.

       APAC-WORK-FILES.
	     MOVE "BULKST"	 TO APAC-KEY.
	     MOVE "BULK_XXX.WRK" TO APAC-NAME.
	     MOVE 59		 TO APAC-FILE.
	     MOVE SPACES	 TO APAC-PATH.
	     STRING W02-PTH23 DELIMITED SIZE
		    "_WORK\" DELIMITED SIZE
		    INTO APAC-PATH.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CORDER"	 TO APAC-KEY.
	     MOVE "CORD_XXX.WRK" TO APAC-NAME.
	     MOVE 48		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CRETRN"	 TO APAC-KEY.
	     MOVE "CTRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DBTCON"	 TO APAC-KEY.
	     MOVE "DCON_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEALER"	 TO APAC-KEY.
	     MOVE "DEAL_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEATOT"	 TO APAC-KEY.
	     MOVE "DEAT_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DPTSALES"	 TO APAC-KEY.
	     MOVE "DSAL_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBTEX"	 TO APAC-KEY.
	     MOVE "DBEX_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBTOT"	 TO APAC-KEY.
	     MOVE "DTOT_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBTRN"	 TO APAC-KEY.
	     MOVE "DTRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DRBTCH"	 TO APAC-KEY.
	     MOVE "DBCH_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLBATCH"	 TO APAC-KEY.
	     MOVE "GBAL_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLJRN"	 TO APAC-KEY.
	     MOVE "GJRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLTRNB"	 TO APAC-KEY.
	     MOVE "GTRB_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "INVSAL"	 TO APAC-KEY.
	     MOVE "INVS_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "LEDTRN"	 TO APAC-KEY.
	     MOVE "LTRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "MEMCRD"	 TO APAC-KEY.
	     MOVE "CRDM_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "MEMDEB"	 TO APAC-KEY.
	     MOVE "DEBM_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "MOVEMT"	 TO APAC-KEY.
	     MOVE "MOVM_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PERFRM"	 TO APAC-KEY.
	     MOVE "PERF_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PRCLAB"	 TO APAC-KEY.
	     MOVE "PLAB_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PRNDISK"	 TO APAC-KEY.
	     MOVE "PRNT_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PURCH"	 TO APAC-KEY.
	     MOVE "PURC_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "RECEIPTS"	 TO APAC-KEY.
	     MOVE "RECP_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "RECOVER"	 TO APAC-KEY.
	     MOVE "RECV_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SALE"	 TO APAC-KEY.
	     MOVE "SALE_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SALEJ"	 TO APAC-KEY.
	     MOVE "SALJ_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SALESMAN"	 TO APAC-KEY.
	     MOVE "SMAN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SALESTRK"	 TO APAC-KEY.
	     MOVE "STRK_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STKORD"	 TO APAC-KEY.
	     MOVE "STKO_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STKSEL"	 TO APAC-KEY.
	     MOVE "STKS_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STKWRK"	 TO APAC-KEY.
	     MOVE "STOT_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.

       APAC-OLD-FILES.
	     MOVE SPACES	 TO APAC-PATH.
	     STRING W02-PTH23 DELIMITED SIZE
		    "_DATA\" DELIMITED SIZE
		    INTO APAC-PATH.
	     MOVE "ASSOLD"	 TO APAC-KEY.
	     MOVE "ASSETT.OLD"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CAROLD"	 TO APAC-KEY.
	     MOVE "CARDEXSL.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CHQBHOLD"	 TO APAC-KEY.
	     MOVE "CHQBCH_1.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CHQOLD"	 TO APAC-KEY.
	     MOVE "CHEQUE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CREOLD"	 TO APAC-KEY.
	     MOVE "AP_ACNT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CRMOLD"	 TO APAC-KEY.
	     MOVE "AP_MEMO.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "CRTOLD"	 TO APAC-KEY.
	     MOVE "AP_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DBFOLD"	 TO APAC-KEY.
	     MOVE "AR_CONT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DBMOLD"	 TO APAC-KEY.
	     MOVE "AR_MEMO.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DBTOLD"	 TO APAC-KEY.
	     MOVE "AR_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEBOLD"	 TO APAC-KEY.
	     MOVE "AR_ACNT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "DEPOLD"	 TO APAC-KEY.
	     MOVE "DEPART.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GACOLD"	 TO APAC-KEY.
	     MOVE "GL_ACNT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GAROLD"	 TO APAC-KEY.
	     MOVE "GARTEE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GBHOLD"	 TO APAC-KEY.
	     MOVE "GL_BHDR.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GLBOLD"	 TO APAC-KEY.
	     MOVE "GLBTCH_1.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GPDOLD"	 TO APAC-KEY.
	     MOVE "GL_PDCHQ.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "GTROLD"	 TO APAC-KEY.
	     MOVE "GL_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "INVOLD"	 TO APAC-KEY.
	     MOVE "INVOICE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "ITMOLD"	 TO APAC-KEY.
	     MOVE "ITEMS.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JAUOLD"	 TO APAC-KEY.
	     MOVE "JOBAUD.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JHDOLD"	 TO APAC-KEY.
	     MOVE "JOBHDR.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JHSOLD"	 TO APAC-KEY.
	     MOVE "JCHIST.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JOBOLD"	 TO APAC-KEY.
	     MOVE "JOBLIN.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JRMOLD"	 TO APAC-KEY.
	     MOVE "JOBRMK.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JRQOLD"	 TO APAC-KEY.
	     MOVE "JOBREQ.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JSTOLD"	 TO APAC-KEY.
	     MOVE "JOBSTAT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JTCOLD"	 TO APAC-KEY.
	     MOVE "JOBTECH.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "JWAOLD"	 TO APAC-KEY.
	     MOVE "JOBWAR.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "LEDOLD"	 TO APAC-KEY.
	     MOVE "LEDTRF.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PDCOLD"	 TO APAC-KEY.
	     MOVE "PDCHEQ.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PRDOLD"	 TO APAC-KEY.
	     MOVE "CARDEXPR.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "PUROLD"	 TO APAC-KEY.
	     MOVE "PORDER.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SLPOLD"	 TO APAC-KEY.
	     MOVE "PSLIPS.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SOROLD"	 TO APAC-KEY.
	     MOVE "SORDER.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "SPAOLD"	 TO APAC-KEY.
	     MOVE "IN_PARTS.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STKOLD"	 TO APAC-KEY.
	     MOVE "INVENT.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "STMOLD"	 TO APAC-KEY.
	     MOVE "IN_MEMO.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "TXTOLD"	 TO APAC-KEY.
	     MOVE "TX_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "UNTOLD"	 TO APAC-KEY.
	     MOVE "UNTSLE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "VEHOLD"	 TO APAC-KEY.
	     MOVE "VEHICLE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "VSTOLD"	 TO APAC-KEY.
	     MOVE "V_INVENT.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "WAROLD"	 TO APAC-KEY.
	     MOVE "WARHSE.OLD"	 TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     MOVE "WSTOLD"	 TO APAC-KEY.
	     MOVE "W_INVENT.OLD" TO APAC-NAME.
	     PERFORM APAC-WRITE-APACFILE.
	     CLOSE APACFILE.

       APAC-CREATE-NEW-EXIT.
	     EXIT.

       APACFILES-EXIT.
	     EXIT.

       APAC-SETUP-PATHS        SECTION 53.
       APAC-SETUP.
	     MOVE W02-PTH23	 TO W02-SYSPTH W02-SYSID.
	     OPEN INPUT APACFILE.
	     OPEN OUTPUT APACFIDS.
	     CLOSE APACFIDS.
	     OPEN I-O APACFIDS.
	     MOVE SPACES	 TO APAC-KEY.
000030	     START APACFILE
	       KEY >= APAC-KEY.

       APAC-GET-NEXT-FILE-ID.
000320	     READ APACFILE NEXT WITH IGNORE LOCK.
000350	   IF NOT (WS-STAT1 = "0")
000370	       GO TO APAC-SETUP-END.
	     MOVE APAC-KEY	 TO AFID-KEY.
	     MOVE SPACES	 TO AFID-PATH.
	     STRING APAC-PATH DELIMITED BY " "
		    APAC-NAME DELIMITED BY " "
		    INTO AFID-PATH.
	     WRITE AFID-RECORD1.
	     GO TO APAC-GET-NEXT-FILE-ID.

       APAC-SETUP-END.
	     CLOSE APACFILE
		   APACFIDS.

       APAC-SETUP-EXIT.
	     EXIT.
