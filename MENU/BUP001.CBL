      ******************************************************************
      *                                                                *
      *    *******   **    **  *******     ****      ****      **      *
      *    **    **  **    **  **    **   **  **    **  **    ***      *
      *    **    **  **    **  **    **  **    **  **    **    **      *
      *    *******   **    **  *******   **    **  **    **    **      *
      *    **    **  **    **  **        **    **  **    **    **      *
      *    **    **  **    **  **         **  **    **  **     **      *
      *    *******    ******   **          ****      ****    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     ( A P A C )   T E R M I N A L   S T A R T   &   I N I T    *
      *                                                                *
      *     VERSION 8.15.01 - August 2011			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2010	- Include the updating of new files that are   *
      * 		  added to the system to the Work stations as  *
      * 		  the file containing the paths could be on a  *
      * 		  local drive and not the Network drive.       *
      * 							       *
      ******************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		    VERSIONS OF THE APAC SYSTEM		       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё While developing this system the requirements and budgets Ё
      *    Ё (originally this was a propriety package) for various     Ё
      *    Ё businesses necessitated that at least 4 separate systems  Ё
      *    Ё be produced. As the maintenance and amendments to the 4   Ё
      *    Ё different systems would require man hours that would not  Ё
      *    Ё enable me to do much development, it was decided to use   Ё
      *    Ё one system with the various requirements controlled by    Ё
      *    Ё the various menu programs using a version indicator. This Ё
      *    Ё indicator is located in the second record of the SYSTEM   Ё
      *    Ё file SEC-LEV, which is in the SEC-MODULES. This indicator Ё
      *    Ё together with all the other indicators in SEC-MODULES, is Ё
      *    Ё moved to WS0-MODULES in BUP000 or BUP001 and then passed  Ё
      *    Ё via the LINKAGE SECTION from program to program in  the   Ё
      *    Ё LS0-MODULES group. The other indicators in this group are Ё
      *    Ё used to show which modules of the system are installed.   Ё
      *    фмммммммммммммммммммямммммммммммммммммммяммммммммммммммммммм╣
      *    Ё	SYSTEM FILE    Ё      CHAIN.WS	   Ё	 CHAIN.LS      Ё
      *    цдддддддддддддддддддедддддддддддддддддддеддддддддддддддддддд╢
      *	   Ё  03  SEC-MODULES. Ё  03  WS0-MODULES. Ё  03  LS0-MODULES. Ё
      *    Ё	  05  SEC-DTP  Ё      05  WS0-DTP  Ё	  05  LS0-DTP  Ё
      *    Ё	  05  SEC-STP  Ё      05  WS0-STP  Ё	  05  LS0-STP  Ё
      *    Ё	  05  SEC-HPD  Ё      05  WS0-HPD  Ё	  05  LS0-HPD  Ё
      *    Ё	  05  SEC-CRP  Ё      05  WS0-CRP  Ё	  05  LS0-CRP  Ё
      *    Ё	  05  SEC-GLP  Ё      05  WS0-GLP  Ё	  05  LS0-GLP  Ё
      *    Ё	  05  SEC-POS  Ё      05  WS0-POS  Ё	  05  LS0-POS  Ё
      *    Ё	  05  SEC-JCP  Ё      05  WS0-JCP  Ё	  05  LS0-JCP  Ё
      *    Ё	  05  SEC-VHP  Ё      05  WS0-VHP  Ё	  05  LS0-VHP  Ё
      *    Ё	  05  SEC-WAG  Ё      05  WS0-WAG  Ё	  05  LS0-WAG  Ё
      *    Ё	  05  SEC-SAL  Ё      05  WS0-SAL  Ё	  05  LS0-SAL  Ё
      *    Ё	  05  SEC-LEV  Ё      05  WS0-LEV  Ё	  05  LS0-LEV  Ё
      *    Ё	  05  SEC-WHS  Ё      05  WS0-WHS  Ё	  05  LS0-WHS  Ё
      *    Ё	  05  SEC-RTE  Ё      05  WS0-RTE  Ё	  05  LS0-RTE  Ё
      *	   Ё	  05  FILLER   Ё		   Ё		       Ё
      *    юдддддддддддддддддддадддддддддддддддддддаддддддддддддддддддды
      *
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     BUP001.
000030 AUTHOR.         J W LEMMON  (APAC).
000040 DATE-WRITTEN.   JUNE 1990.

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
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY APACFILE.SL.

       COPY APCCOM.SL.

000120 COPY SYSTEM.SL.

000140 COPY PARAM.SL.

000160 DATA DIVISION.
000170 FILE SECTION.

       COPY APACFIDS.FD.

       COPY APACFILE.FD.

       COPY APCCOM.FD.

000190 COPY SYSTEM.FD.

000210 COPY PARAM.IFD.

000230 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000240 77  WS-S1	       PIC 9(04)     COMP-5.
000250 77  WS-S2               PIC 9(04)     COMP-5.
000260 77  WS-S3               PIC 9(04)     COMP-5.
000270 77  WS-S4               PIC 9(04)     COMP-5.
000710 77  WS-IXS1	       PIC 9(04)     COMP-5.
       77  WS-TERMS	       PIC 9(04)     COMP-5.
000280 77  WS-PARKEY           PIC 9(04)     COMP-5.
000290 77  WS-SECKEY           PIC 9(04)     COMP-5.
000310 77  WS-START            PIC 9(08).
000320 77  WS-END              PIC 9(08).
       77  WS-DATE	       PIC  Z9/99/9999.
000330 77  WS-OPTION           PIC  X(01).
000330 77  WS-REPLY	       PIC  X(01).
000340 77  WS-PAR              PIC 9(01) VALUE 0.
000350 77  WS-ERROR            PIC 9(01) VALUE 0.

000360 01  WS-GRAPHICS.
000370     03  WS-GCHAR        PIC X(01) OCCURS 12.
002060 01  WS-DB-LINE.
002070     03  WS-TOP-LINE.
002080         05  WS-TCHAR    PIC  X(01) OCCURS 80.
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH      PIC  X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LINE.
002460         05  WS-MCHR     PIC  X(01) OCCURS 80.
	   03  WS-MID78 REDEFINES WS-MID-LINE.
	       05  FILLER      PIC  X(01).
	       05  WS-BLNK78   PIC  X(78).
	       05  FILLER      PIC  X(01).
002110	   03  WS-BOT-LINE.
002120         05  WS-BCHAR    PIC  X(01) OCCURS 80.
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH      PIC  X(01) OCCURS 80.

000410 COPY WS.WS.

000410 COPY CHAIN.WS.

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY APACFILE.ID.

       COPY PARAM.ID.

       COPY SYSTEM.ID.

000430 01  W05-COMMAND.
	   03  W05-COM-LINE.
000440	       05  W05-CHAR    PIC  X(01) OCCURS 128.
	   03  W05-COM-L REDEFINES W05-COM-LINE.
	       05  W05-SHORT   PIC  X(28).
	       05  FILLER      PIC  X(100).

000450 01  W05-DETAIL.
000460     03  W05-DC          PIC  X(01) OCCURS 15.

       01  W05-STRINGS.
000480     03  W05-DSK         PIC  X(01).
000490     03  W05-EX          PIC  X(03).
           03  W05-TR          PIC  X(03).
000500     03  W05-DR          PIC  X(02).
000520     03  W05-CS          PIC  X(01).

000470 01  W05-COM.
000480     03  W05-DISK        PIC  X(01).
000490     03  W05-EXT         PIC  X(03).
           03  W05-TRM         PIC  X(03).
000500     03  W05-DRW.
000510         05  W05-DRAW    PIC  9(02).
000520     03  W05-CSS         PIC  X(01).

000530 01  W05-TEMP.
000540     03  W05-NUM         PIC  X(03).
           03  W05-NCHR REDEFINES W05-NUM
                               PIC  X(01) OCCURS 3.
000540     03  W05-NUM2        PIC  X(03).
           03  W05-NCHR2 REDEFINES W05-NUM2
                               PIC  X(01) OCCURS 3.
           03  W05-RESULT REDEFINES W05-NUM2.
               05  W05-NTRM    PIC  9(03).
           03  W05-REST REDEFINES W05-NUM2.
               05  FILLER      PIC  X(01).
               05  W05-NDRW    PIC  9(02).

       01  W10-FILE-PATHS.
	   03  W10-CURRENT-PATH  PIC  X(52).
	   03  W10-CURR-PATH	 REDEFINES W10-CURRENT-PATH.
	       05  W10-CURR-CHAR PIC  X(01) OCCURS 52.
	   03  W10-WRK-PATH	 PIC  X(52).
	   03  W10-WK-PATH	 REDEFINES W10-WRK-PATH.
	       05  W10-WRK-CHAR  PIC  X(01) OCCURS 52.
	   03  W10-PATH 	 PIC  X(09).
	   03  W10-COM-PATH	 REDEFINES W10-PATH.
	       05  W10-COM-CHAR  PIC  X(01) OCCURS 9.

       01  W10-USER-FIELDS.
	   03  W10-USER        PIC  9(03) VALUE ZERO.
	   03  W10-PASS        PIC  X(08) VALUE SPACES.

       COPY W12.WS.

001510 01  W20-DATE.
001520     03  W20-DAY         PIC 9(02).
001530     03  W20-MONTH       PIC 9(02).
	   03  W20-CENT        PIC 9(02).
001540     03  W20-YEAR        PIC 9(02).
001550 01  W20-DTE1 REDEFINES W20-DATE.
001560	   03  W20-DTE	       PIC 9(08).
001550 01  W20-DTE2 REDEFINES W20-DATE.
	   03  W20-CC	       PIC 9(02).
001520     03  W20-YY          PIC 9(02).
001530     03  W20-MM          PIC 9(02).
001540     03  W20-DD          PIC 9(02).
       01  W20-LEAP REDEFINES W20-DATE.
	   03  FILLER	       PIC 9(04).
	   03  W20-CY	       PIC 9(04).

001570 01  W25-CALCS.
001580	   03  W25-RESULT      PIC  9(09)V99.
001590     03  W25-RESULT1 REDEFINES W25-RESULT.
001600	       05  FILLER      PIC  9(03).
001610	       05  W25-WHOLE   PIC  9(06).
001620	       05  W25-DECIMAL PIC  9(02).
	   03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  FILLER      PIC  9(02).
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
001630	   03  W25-PASS.
001640	       05  W25-PASS1   PIC  9(03).
001650	       05  W25-PASS2   PIC  9(03).
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680	       05  FILLER      PIC  9(02).
001690	       05  W25-FACT    PIC  9(06).
	       05  W25-DATE REDEFINES W25-FACT.
		   07  W25-YY  PIC  9(02).
		   07  W25-MM  PIC  9(02).
		   07  W25-DD  PIC  9(02).
	   03  W25-CUR-CC      PIC  9(02) COMP-5.
	   03  W25-PRV-CC      PIC  9(02) COMP-5.
	   03  W25-NXT-CC      PIC  9(02) COMP-5.

001700 01  W30-MONTHS.
001710     03  W30-MONTH-NAMES.
001720         05  W30-ENG     PIC X(36)     VALUE
001730                         "JanFebMarAprMayJunJulAugSepOctNovDec".
001740     03  W30-MONTH REDEFINES W30-MONTH-NAMES
001750                         PIC X(03)     OCCURS 12.

	COPY W40.WS.

       COPY FUNCTION.WS.

       01  W75-KEYS.
	   03  W75-S	       PIC 9(02) COMP-5.
	   03  W75-S1	       PIC 9(02) COMP-5.
	   03  W75-S2	       PIC 9(02) COMP-5.
	   03  W75-COM	       PIC X(03) OCCURS 18.

       LINKAGE SECTION.

       COPY CHAIN.LS.

001160 01  L05-COMMAND.
001170	   03  L05-CHAR        PIC  X(128).

000830 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY LOGO.CRT.

004830 01  S18.
	   03  LINE  4 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
	       "здддддддддддддддддддддддддддддддддддддддддддддддд".
	   03	       COLUMN 65 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "©".
	   03  LINE  5 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03	       COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
	       "Packaging and Marketing:                        ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
004850	   03  LINE  6 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03	       COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
004860	       "       WORKGROUP DATA SOLUTIONS.                ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03  LINE  7 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03	       COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "       18 Milford Crest, North Riding, JHB.     ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
004880	   03  LINE  8 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03	       COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "       PO Box 68416,                            ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
004890	   03  LINE  9 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03	       COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "       BRYANSTON. 2021                          ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
004900	   03  LINE 10 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03	       COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
	       "Support:                                        ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
004910	   03  LINE 11 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03	       COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "  See the registration card, supplied with the  ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03  LINE 12 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03	       COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "  system, for details of the telephone and fax  ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
004910	   03  LINE 13 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03	       COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "  numbers of the APAC support line.             ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03  LINE 14 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03	       COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
	       "  For local support in your area please contact ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03  LINE 15 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
004910	   03	       COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
	       "  the support line, who will in turn give you   ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03  LINE 16 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03	       COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE
	       "  details of your local support centre.         ".
	   03	       COLUMN 65 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ё".
	   03  LINE 17 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "ю".
	   03	       COLUMN 17 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "дддддддддддддддддддддддддддддддддддддддддддддддды".
001470	   03  LINE 19 COLUMN 16 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
001480	       "Press any KEY to Continue".

       01  S-COM.
	   03  LINE  6 COLUMN 29 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "здддддддддддддддддддддддддддддд
      -        "дддддддддддддд©".
	   03  LINE  7 COLUMN 29 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "Ё    C O M P A N Y   C O D E
      -	       "L O O K U P   Ё".
	   03  LINE  8 COLUMN 29 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "цдддбдддддддддддддддддддддддддд
      -        "дддддддддддддд╢".
	   03  LINE 21 COLUMN 29 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "юдддадддддддддддддддддддддддддд
      -        "дддддддддддддды".

       01  SD-COM.
	   03  COLUMN 30 FOREGROUND-COLOR 5
			 BACKGROUND-COLOR 7 PIC X(03) FROM COM-CODE.
	   03  COLUMN 34 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
			 PIC X(40) FROM COM-NAME.

003700 01  S19.
004970	   03  LINE 16 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
004980		       VALUE "Please key in todays date (DDMMCCYY)".

004990 01  S20.
005000	   03  LINE 16 COLUMN 54 FOREGROUND-COLOR 7 HIGHLIGHT
005010				 PIC Z9/99/9999 USING W20-DTE AUTO.


       COPY ERROR.CRT.

010570 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS L05-COMMAND.
001500 AA000-MAIN	       SECTION.
001510 AA00.
             DISPLAY SPACE AT 0101.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-USER-ID	 TO WS-USER-ID.
	     MOVE LS0-PROGRAMS	 TO WS0-PROGRAMS.
001520	     PERFORM AL000.
001530       DISPLAY CLR-SCREEN.
	     MOVE W05-EXT	 TO WS-SYS-ID W02-SYSID W02-SYSPTH.

       AA00-APAC.
	     OPEN INPUT APACFIDS.
      *
      *	     DISPLAY WS-STAT1 AT 2404.
      *	     DISPLAY WS-STAT2H AT 2407.
      *	     DISPLAY WS-DSKDRV AT 2412.
      *	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      *
006200	   IF RUNTIME-ERROR
	       IF FLE-LOCKED
006210		   MOVE "Files locked - try later - Press ENTER"
006220				 TO WS-ERR-MES
006230		   PERFORM ERROR-LENGTH THRU ERROR-EXIT
002570		   STOP RUN.
	   IF (((WS-STAT1 = "9") AND
	      (WS-STAT2H = 009 OR 199)) OR
	      ((WS-STAT1 = "3") AND
	      (WS-STAT2H = 053))) AND
	      (WS-DSKDRV = "N")
	       PERFORM AA200
	       IF WS-ERROR = 0
		   PERFORM AA100
		   IF WS-OPTION = "Y"
		       CLOSE APACFIDS
		       MOVE "OFFLINE\"
				 TO W02-L-OR-N
				    WS-L-OR-N
		       MOVE "L"      TO WS-DSKDRV
		       GO TO AA00-APAC
		   ELSE
		       MOVE "System Aborted- No network drive"
				 TO WS-ERR-STRING
003260		       PERFORM ERROR-MESSAGE
		       STOP RUN.
002490	   IF NOT(WS-STATUS = "00")
002500	       MOVE "Invalid or file not on disk"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
002520         STOP RUN.
	     MOVE "PARAM"	 TO AFID-KEY.

       AA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO AA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       AA00-READ-APACFIDS-EXIT.
	     EXIT.

       AA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
	     MOVE "SYSTEM"	 TO AFID-KEY.
	     PERFORM AA00-READ-APACFIDS THRU AA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SYSTEM.
002480	     OPEN INPUT SECUR.
002490	   IF WS-STATUS = "30"
002500	       MOVE "Invalid or file not on disk"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
002520	       STOP RUN.
001540	     PERFORM AD000.
001550       PERFORM AE000.
014760	     MOVE 2		 TO WS-SECKEY.

       AA02.
014780	     READ SECUR.
014790     IF RUNTIME-ERROR
014800         IF RECORD-LOCKED
014810		   GO TO AA02.
	   IF SEC-AGREE = "R" AND
	      SEC-DAYS = ZERO
	       MOVE "Contract must be renewed!"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
006370	       STOP RUN.
	   IF WS-TERMS NOT > 0
	       MOVE "Networking not installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       STOP RUN.
	   IF WS-USE > WS-TERMS
	       MOVE "Workstation number exceeds users allowed"
				 TO WS-ERR-MES
003260	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
	       STOP RUN.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 18		 TO SHADE-COL.
	     MOVE 48		 TO SHADE-WIDTH.
	     MOVE 13		 TO SHADE-LINES.
001560	     DISPLAY S18.
	     PERFORM SCREEN-SHADOW.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
003410*      ACCEPT WS-OPTION AT 1942 WITH AUTO-SKIP.
	     MOVE "C"		 TO LS-INSTR.
	     MOVE SPACES	 TO LS-NEXT-PRG.
	     MOVE WS-PARID	 TO LS-PARID.
	     MOVE WS-USER-ID	 TO LS-USER-ID.
	     MOVE WS0-PROGRAMS	 TO LS0-PROGRAMS.

       AA05.
	     EXIT PROGRAM.
      *    *************************************************************
      *    ***	N E T W O R K	D R I V E   N O T   A V A I L A B L E
      * 	M U S T   S Y S T E M	R U N	O F F - L I N E
      *    *************************************************************
       AA100	       SECTION.
       AA105.
	     PERFORM SAVE-SCREEN-3.
	     DISPLAY "зддддддддддддддддддддддддддддддддддддддд"
		     AT 1120 WITH FOREGROUND-COLOR 0
				  BACKGROUND-COLOR 5
		     "©" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "Ё" AT 1220 WITH FOREGROUND-COLOR 0
				      BACKGROUND-COLOR 5
		     "Network files not available at present!"
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5
		     "Ё" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "Ё" AT 1320 WITH FOREGROUND-COLOR 0
				      BACKGROUND-COLOR 5
		     "Work off-line (Y/N)                    "
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "Ё" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "ю" AT 1420 WITH FOREGROUND-COLOR 0
				      BACKGROUND-COLOR 5
		     "ддддддддддддддддддддддддддддддддддддддды"
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 22		 TO SHADE-COL.
	     MOVE 40		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     MOVE "N"		 TO WS-OPTION.

       AA110.
	     PERFORM WITH TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y"
	       ACCEPT WS-OPTION AT 1341
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
	     PERFORM RESTORE-SCREEN-3.

007710 AA199.
007720       EXIT.
      *    *************************************************************
      *    ***	F I L E S   N O T   A V A I L A B L E
      * 	Check if this is a Network and if off-line files exist.
      *    *************************************************************
       AA200	       SECTION.
       AA205.
	     MOVE ZERO		 TO WS-ERROR.
	     MOVE "OFFLINE\"     TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     CALL "CBL_CHECK_FILE_EXIST" USING W02-APACFIDS
					       FILE-DETAILS
				     RETURNING WS-STATUS.
	   IF WS-STAT1H = 1
	       MOVE 9		 TO WS-ERROR.
	     MOVE "SYSTEMS\"     TO W02-L-OR-N.

007710 AA999.
007720       EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

      /
      *    *****    DATE VALIDATION ROUTINE    *****
      *
005630 AB000-DATE              SECTION.
005640 AB000-VALID-DATE.
005650	     MOVE ZERO		 TO WS-ERROR.
005660     IF W20-DAY < 01 OR > 31
005670         GO TO AB010-ERROR.
005680     IF W20-MONTH < 01 OR > 12
005690         GO TO AB010-ERROR.
005700     IF W20-MONTH = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
005710         GO TO AB999-EXIT.
005720     IF W20-MONTH = 02 AND W20-DAY > 28
005730         GO TO AB005-FEB.
005740     IF W20-DAY > 30
005750         GO TO AB010-ERROR.
005760       GO TO AB999-EXIT.

005770 AB005-FEB.
005780     IF W20-DAY > 29
005790         GO TO AB010-ERROR.
005800	     DIVIDE W20-CY BY 4  GIVING W25-RESULT.
005810     IF W25-DECIMAL = 0
005820         GO TO AB999-EXIT.

005830 AB010-ERROR.
005840	     MOVE 1		 TO WS-ERROR.

005850 AB999-EXIT.
005860       EXIT.

001640 AD000		       SECTION.
001650 AD00.
	   IF LS-INSTR = "X"
001680	       MOVE W05-EXT	 TO WS-SYS-ID
	       MOVE "X" 	 TO W05-CSS
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE WS-USER	 TO WS-WRKST
	       GO TO AD01.
001660	     MOVE W05-DRAW	 TO WS-DRAW.
	     MOVE W05-TRM	 TO WS-USER.
	     MOVE "Workstation"  TO WS-WRKHD.
	     MOVE WS-USER	 TO WS-WRKST.
001680	     MOVE W05-EXT	 TO WS-SYS-ID.
001690     IF W05-CSS = "X"
001700	       MOVE "Y" 	 TO WS-CSS
001710     ELSE
001720     IF W05-CSS = "Z"
001730	       MOVE "N" 	 TO WS-CSS
001740     ELSE
001750	       MOVE W05-CSS	 TO WS-CSS.

       AD01.
001760       OPEN I-O PARAM.
001770     IF WS-STAT1 = "9"
006210	       MOVE "Files locked - try later - Press ENTER"
				 TO WS-ERR-MES
003260	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
001810         STOP RUN.
001820     IF WS-STATUS NOT = "00"
001830         GO TO AD06.
014670	     MOVE 3		 TO WS-PARKEY.

014680 AD01-DATE.
014690	     READ PARAM LOCK.
014700     IF RUNTIME-ERROR
014710         IF RECORD-LOCKED
014720		   GO TO AD01-DATE.
014730     IF WS-STATUS NOT = "00"
014740	       MOVE "Disk error - AD01 (PARAM)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014750         STOP RUN.
	     MOVE PAR-CUR-YR	 TO WS-CUR-YR.
	     MOVE PAR-CUR-CEN	 TO WS-CUR-CEN.
	     MOVE PAR-PRV-CEN	 TO WS-PRV-CEN.
	     MOVE PAR-NXT-CEN	 TO WS-NXT-CEN.
      *
      *    ****    G E T   R U N   D A T E
      *
042720	     MOVE 1		 TO WS-PARKEY.
001860	     READ PARAM IGNORE LOCK.
042750	     MOVE PAR-DMY	 TO W12-TODAY
				    WS-DATE
				    TODAY-DDMMYY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
      *
      *    ****    CHECK IF LAST RUN ENDED CORRECTLY
      *
001840	     MOVE 4		 TO WS-PARKEY.
001850 AD02.
001860       READ PARAM LOCK.
001870     IF WS-STAT1 = "9"
001880         IF RECORD-LOCKED
001890             GO TO AD02.
001910	   IF PAR-LOGON NOT = 1
001920	       DISPLAY "Supervisor not signed on" AT 1004
001930			WITH FOREGROUND-COLOR 6 HIGHLIGHT
	       DISPLAY "Run Date is " AT 1204
			WS-DATE WITH FOREGROUND-COLOR 6 HIGHLIGHT
	       DISPLAY "C" AT 1404 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		       "ontinue,"
		       "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		       "ew date or "
		       "E" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		       "xit"
	       PERFORM AD10 THRU AD99
001530	       DISPLAY CLR-SCREEN
	       IF WS-OPTION = "E"
001940		   UNLOCK PARAM
001950		   STOP RUN.
001960     IF PAR-USERS = 0
001990	       MOVE 1		 TO WS-PAR
002000         GO TO AD07U.
002010	     MOVE 1		 TO WS-S1.
002020	     MOVE 0		 TO WS-S2 WS-S3.

002030 AD05-CHECK.
002040	   IF PAR-USR(WS-S1) = WS-USER
002050	       IF PAR-PROG(WS-S1) = SPACES
		   MOVE SPACES	 TO PAR-USR(WS-S1)
		   SUBTRACT 1	 FROM PAR-USERS
002100         ELSE
002120             GO TO AD06.
002160     IF WS-S1 < 24
002170	       ADD 1		 TO WS-S1
002180         GO TO AD05-CHECK.
	   IF NOT(LS-INSTR = "X")
002280	       GO TO AD07U.
	   IF PAR-LOGON = 1
	       MOVE "Y" 	 TO WS-REPLY
	   ELSE
	       MOVE "N" 	 TO WS-REPLY.
015200       REWRITE PAR-RECORD1.
002280	     GO TO AD07U.

002290 AD06.
002300       DISPLAY "Last run not ended off correctly" AT 2014 
002310		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
002320       DISPLAY 
002330             "Recovery procedure must be initiated by Supervisor"
002340		    AT 2114 WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	       DISPLAY WS-STATUS AT 2502
		       WITH BACKGROUND-COLOR 3
			    FOREGROUND-COLOR 7 HIGHLIGHT
		       " " WITH BACKGROUND-COLOR 3
		       WS-PARID WITH BACKGROUND-COLOR 3
				     FOREGROUND-COLOR 6 HIGHLIGHT.

002350 AD07.
002360	     MOVE 9		 TO WS-PAR.
002370       UNLOCK PARAM.
002380       STOP RUN.

002390 AD07U.
002400       REWRITE PAR-RECORD1.
002410       UNLOCK PARAM.
002420       CLOSE PARAM.
001870	   IF NOT WS-STAT1 = "0"
001890	       MOVE "User file error"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       STOP RUN.
	     GO TO AD999.

       AD10.
	     PERFORM WITH TEST AFTER
		     UNTIL WS-OPTION = "C" OR "N" OR "E"
	       ACCEPT WS-OPTION AT 1430
		      WITH BACKGROUND-COLOR 5
			   FOREGROUND-COLOR 6 HIGHLIGHT AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
	   IF NOT(WS-OPTION = "N")
	       GO TO AD99.
014670	     MOVE 1		 TO WS-PARKEY.

       AD12.
014690	     READ PARAM.
014700	   IF RUNTIME-ERROR
014710         IF RECORD-LOCKED
014720		   GO TO AD12.
014760	     MOVE 2		 TO WS-SECKEY.

014770 AD14.
014780       READ SECUR.
014790     IF RUNTIME-ERROR
014800         IF RECORD-LOCKED
014810		   GO TO AD14.

       AD15.
	     ACCEPT W25-FACT FROM DATE.
	     MOVE W25-YY	 TO W20-YEAR  W12-YEAR.
	     MOVE W25-MM	 TO W20-MONTH W12-MONTH.
	     MOVE W25-DD	 TO W20-DAY   W12-DAY.
	     MOVE WS-CUR-CEN	 TO W20-CENT  W12-CENT.
	   IF W25-YY < (WS-CUR-YR - 70)
	       MOVE WS-NXT-CEN	 TO W20-CENT W12-CENT
	   ELSE
	   IF W25-YY > (WS-CUR-YR + 70)
	       MOVE WS-PRV-CEN	 TO W20-CENT W12-CENT.
	     DISPLAY S19.

014970 AD20.
014980       ACCEPT S20.
006180	     DISPLAY CLEAR-L25.
014990       PERFORM AB000-DATE.
015000     IF WS-ERROR = 1
015010	       MOVE "Invalid date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015020	       GO TO AD20.
015030	     MOVE W20-DAY	 TO W12-DAY W12-DD.
015040	     MOVE W20-MONTH	 TO W12-MONTH W12-MM.
015050	     MOVE W20-YEAR	 TO W12-YEAR W12-YY.
	     MOVE W20-CENT	 TO W12-CENT W12-CC.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY SEC-DATE.
015060	     DISPLAY WS-BLNK78 AT 1802 WITH FOREGROUND-COLOR 3.
015060	     DISPLAY WS-BLNK78 AT 2002 WITH FOREGROUND-COLOR 3.
015070	     DISPLAY "Todays date - " AT 1829 W12-DD " "
015080		      W30-MONTH(W12-MM) "," W12-CC W12-YY.
015090	     DISPLAY "Is date correct (Y/N) ?" AT 2029
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "Y"		 TO WS-OPTION.

       AD25.
	     PERFORM WITH TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y"
015100	       ACCEPT WS-OPTION AT 2053
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
015160	   IF WS-OPTION = "N"
015170	       GO TO AD20.
015180	     MOVE W12-TODAY	 TO PAR-DMY.
015190	     MOVE W12-T-YMD	 TO PAR-YMD.
015200       REWRITE PAR-RECORD1.

002430 AD99.
002440       EXIT.

002430 AD999.
002440       EXIT.

002450 AE000           SECTION.
002460 AE00.
002580	     MOVE 2		 TO WS-SECKEY.

002590 AE02.
002600       READ SECUR.
002610     IF WS-STAT1 = "9"
002620         IF RECORD-LOCKED
002630             GO TO AE02.
002640     IF WS-STATUS NOT = "00"
002650	       MOVE "Disk error - AE00"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
002670         STOP RUN.
	     MOVE SEC-COMP	 TO WS-COMPANY.
	     MOVE SEC-SYS	 TO APAC-SYSTEM WS-APAC-SYSTEM.
	   IF NOT(SEC-VER = "2010" OR "2011")
	       PERFORM AF000
	       MOVE "2011"	 TO SEC-VER.
	     MOVE SEC-VER	 TO APAC-VERSION WS-APAC-VERSION.
	     MOVE 40		 TO WS-S1.

       AE05.
	   IF WS-COMPCHR(WS-S1) = SPACE
	       MOVE WS-G8	  TO WS-COMPCHR(WS-S1)
               IF WS-S1 > 1
                   SUBTRACT 1    FROM WS-S1
                   GO TO AE05.
             MOVE WS-COMPANY     TO WS-TOP-COMP.
	   IF WS-USER = WS-SYS-ID
	       MOVE "Supervisorд"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE WS-USER	   TO WS-WRKST.
002680	     MOVE SEC-PMODS	 TO WS0-PMODS.
             MOVE SEC-DEM        TO WS-DEM.
	     MOVE SEC-TERMS	 TO WS-TERMS.
002690       CLOSE SECUR.

002700 AE999.
002710	     EXIT.

       AF000	       SECTION.
       AF00.
	     OPEN I-O APACFILE.
	     CLOSE APACFIDS.
      *
      * 	Get the path used for the existing files.
      *
	     MOVE "PARAM"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	     MOVE APAC-PATH	 TO W10-CURRENT-PATH.
	     MOVE "CORDER"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	     MOVE APAC-PATH	 TO W10-WRK-PATH.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё CHECK FOR THE PRESENCE OF NEW FILES THAT ARE ADDED TO THE Ё
      *    Ё SYSTEM AND UPDATE THE PATH FILE WITH MISSING FILE NAMES.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *    *************************************************************
      *    ****   C R E D I T O R S   M E M O	W O R K   F I L E   ****
      * 	       Version 8.15.01
      *    *************************************************************
	     MOVE "MEMCRD"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT(WS-STATUS = "00")
	       MOVE "MEMCRD"	 TO APAC-KEY
	       MOVE "CRDM_XXX.WRK"
				 TO APAC-NAME
	       MOVE ZERO	 TO APAC-FILE
	       MOVE W10-WRK-PATH TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      *    ****        I N V O I C E   B I T M A P   F I L E	    ****
      * 	       Version 8.14.03
      *    *************************************************************
	     MOVE "INVBMP"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT(WS-STATUS = "00")
	       STRING WS-SYS-ID DELIMITED SIZE
		      "_INV.BMP" DELIMITED SIZE
		      INTO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      *    ****      S T A T E M E N T	 B I T M A P   F I L E	    ****
      * 	     Version 8.14.03
      *    *************************************************************
	     MOVE "STMBMP"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT(WS-STATUS = "00")
	       MOVE "STMBMP"	 TO APAC-KEY
	       MOVE SPACES	 TO APAC-NAME
	       STRING WS-SYS-ID DELIMITED SIZE
		      "_STM.BMP" DELIMITED SIZE
		      INTO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      * 	   Version 8.14.03
      *    *************************************************************
	     MOVE "DEALER"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT(WS-STATUS = "00")
	       MOVE "DEALER"	 TO APAC-KEY
	       MOVE "DEAL_XXX.WRK"
				 TO APAC-NAME
	       MOVE W10-WRK-PATH TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
	     MOVE "DEATOT"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT(WS-STATUS = "00")
	       MOVE "DEATOT"	 TO APAC-KEY
	       MOVE "DEAT_XXX.WRK"
				 TO APAC-NAME
	       MOVE W10-WRK-PATH TO APAC-PATH
	       WRITE APAC-RECORD1
	       MOVE WS-SYS-ID	 TO W02-SYSID
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
      *    *************************************************************
      *    ****    D E B T O R	 N A M E   I N D E X   F I L E	    ****
      * 	   Version 8.13.8
      *    *************************************************************
	     MOVE "DEBALT"	 TO APAC-KEY.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF NOT(WS-STATUS = "00")
	       MOVE "DEBALT"	 TO APAC-KEY
	       MOVE "AR_NAME.DAT"
				 TO APAC-NAME
	       MOVE W10-CURRENT-PATH
				 TO APAC-PATH
	       WRITE APAC-RECORD1
	       OPEN I-O APACFIDS
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       CLOSE APACFIDS.
	     CLOSE APACFILE.
	     OPEN INPUT APACFIDS.
       AF999.
	     EXIT.

002720 AL000           SECTION.
002730 AL00.
002740       ACCEPT WS-START FROM TIME.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	       GO TO AL02.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       Set MOUSE key to act as FUNCTION key	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.

       AL02.
002890	     MOVE 223		 TO W40-DEC.
002900	     MOVE 1		 TO WS-S1.

002910 AL05.
002920	     MOVE W40-CHAR	 TO WS-GCHAR(WS-S1).
002930     IF WS-S1 < 20
002940	       ADD 1		 TO WS-S1
002950         GO TO AL05.
014170	     MOVE 2		 TO WS-S1.

014180 AL05A.
014190	     MOVE WS-G1 	 TO WS-TCHAR(WS-S1) WS-BCHAR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1) WS-BCH(WS-S1).
014200     IF WS-S1 < 79
014210	       ADD 1		 TO WS-S1
014220         GO TO AL05A.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
014230	     MOVE WS-G15	 TO WS-TCHAR(1).
014240	     MOVE WS-G16	 TO WS-TCHAR(80).
014250	     MOVE WS-G17	 TO WS-BCHAR(1).
014260	     MOVE WS-G18	 TO WS-BCHAR(80).
	   IF NOT(LS-INSTR = "X")
002960	       DISPLAY APAC.
021870	     MOVE ALL "╟"	 TO WS-MID-LINE.
	     MOVE WS-G3 	 TO WS-MCHR(1) WS-MCHR(80).

002990 AL05B.
	   IF LS-INSTR = "X"
	       GO TO AL45.
	     MOVE L05-COMMAND	 TO W05-COMMAND.
003060	     MOVE 1		 TO WS-S1 WS-S2.

003070 AL10.
003080	   IF W05-CHAR(WS-S1) = "*"
003090         GO TO AL15.
003100     IF WS-S1 < 117
003110	       ADD 1		 TO WS-S1
003120         GO TO AL10.
003130	     MOVE 1		 TO WS-ERROR.
003140       GO TO AL30.

003150 AL15.
003160	     ADD 2		 TO WS-S1.

003170 AL20.
003180	   IF W05-CHAR(WS-S1) = " "
003190         GO TO AL25.
003200	     MOVE W05-CHAR(WS-S1)
				 TO W05-DC(WS-S2).
003210     IF WS-S2 < 15
003220	       ADD 1		 TO WS-S2
003230     ELSE
003240         GO TO AL26.

003250 AL25.
003260     IF WS-S1 < 128
003270	       ADD 1		 TO WS-S1
003280         GO TO AL20.

       AL26.
             MOVE SPACES         TO W05-STRINGS.
	     MOVE ","		 TO W05-DC(WS-S2).
             UNSTRING W05-DETAIL DELIMITED BY ","
                                 INTO W05-DSK
                                      W05-EX
                                      W05-TR 
                                      W05-DR 
                                      W05-CS
             OVERFLOW
                 MOVE 0          TO WS-ERROR.
	     MOVE W05-TR	 TO W05-NUM.
             PERFORM AL27 THRU AL28.
             MOVE W05-NTRM       TO W05-TRM.
           IF W05-TRM = ZERO 
               MOVE 2            TO WS-ERROR
               GO TO AL30.
             MOVE W05-DR         TO W05-NUM.
             PERFORM AL27 THRU AL28.
             MOVE W05-NDRW       TO W05-DRAW.
             MOVE W05-DSK        TO W05-DISK.
             MOVE W05-EX         TO W05-EXT.
	     CALL "CBL_TOUPPER" USING W05-CS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE W05-CS	 TO W05-CSS.
	     GO TO AL30.

       AL27.
003370       MOVE 3              TO WS-S1 WS-S2.
             MOVE ZERO           TO W05-NUM2.

       AL28.
	   IF W05-NCHR(WS-S1) NUMERIC
	       MOVE W05-NCHR(WS-S1)
				 TO W05-NCHR2(WS-S2)
               SUBTRACT 1        FROM WS-S1 WS-S2
           ELSE
               SUBTRACT 1        FROM WS-S1.
           IF WS-S1 > ZERO
               GO TO AL28.

003300 AL30.
003310     IF WS-ERROR = ZERO
003320         IF W05-DRW NOT NUMERIC
003330             MOVE 2        TO WS-ERROR
003340         ELSE
003350         IF W05-DRAW > 39
003360             MOVE 3        TO WS-ERROR
               ELSE
               IF W05-TRM NOT NUMERIC
                   MOVE 4        TO WS-ERROR
               ELSE
               IF W05-TRM > 100
                   MOVE 5        TO WS-ERROR.
003370	     MOVE ZERO		 TO WS-S1 WS-S2.

003380 AL35.
003390       CALL X"D9" USING WS-CONSOLE.
003400     IF WS-CONSOLE NOT = ZERO
003410         ACCEPT WS-OPTION WITH AUTO-SKIP
003420         GO TO AL40.
003430	     ADD 1		 TO WS-S1 WS-S2.
003440	     ACCEPT WS-END FROM TIME.
003450	     SUBTRACT WS-START	 FROM WS-END.
003460     IF WS-END < 400
003470         GO TO AL35.
003480 AL40.
003490     IF WS-ERROR = 0
003500         GO TO AL999.
             DISPLAY CLR-SCREEN.
           IF WS-ERROR = 2 OR 3
	       MOVE "Cash drawer not in range 00 to 39"
006220				 TO WS-ERR-MES
006230	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
           ELSE
           IF WS-ERROR = 4 OR 5
	       MOVE "Terminal not in range 001 to 100"
006220				 TO WS-ERR-MES
006230	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
           ELSE
003510	       MOVE "Invalid Parameters in command line"
006220				 TO WS-ERR-MES
006230	       PERFORM ERROR-LENGTH THRU ERROR-EXIT.
003530       STOP RUN.

       AL45.
	     DISPLAY CLR-SCREEN.
	     DISPLAY "Key in Company code" AT 1212.
	     DISPLAY "F2" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     " to view existing codes"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1.
	     MOVE WS-SYS-ID	 TO W05-EXT.

       AL50.
	     ACCEPT W05-EXT AT 1232
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN F2-KEY
		     OPEN INPUT APCCOM
018810		     PERFORM COMPANY-LOOKUP
		     CLOSE APCCOM
		     GO TO AL50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO AL50
	       END-EVALUATE.
006020	     MOVE 3		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W05-EXT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
006080	     MOVE 1		 TO WS-LENGTH.
	   IF W05-EXT = SPACES
	       MOVE WS-SYS-ID	 TO W05-EXT.

003540 AL999.
003550       EXIT.

       COMPANY-LOOKUP  SECTION.
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
	     MOVE SPACES	 TO W75-COM(W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
	       GO TO COMPANY-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.
	     MOVE "  !" 	 TO COM-CODE.
006850	     START APCCOM
	       KEY >= COM-CODE.
006860	   IF NOT(WS-STATUS = "00")
	       GO TO COMPANY-NO-RECORD.

       COMPANY-GET-COM-RECORD.
	   IF WS-REPLY = "J"
	       MOVE SPACE	 TO WS-REPLY
	       GO TO COMPANY-SKIP-COM-READ.
006850	     READ APCCOM NEXT	 WITH IGNORE LOCK.
009900	   IF NOT(WS-STATUS = "00")
	       SUBTRACT 1	 FROM W75-S
	       GO TO COMPANY-FULL-SCREEN.

       COMPANY-SKIP-COM-READ.
	     MOVE COM-CODE	 TO W75-COM(W75-S).
	     DISPLAY SD-COM AT LINE CLIN.
027700	   IF CLIN < 20
027710         ADD 1 TO CLIN W75-S
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
	     MOVE "J"		 TO WS-REPLY.

       COMPANY-GET-PREV-LOOP.
006850	     READ APCCOM PREVIOUS
				 WITH IGNORE LOCK.
009900	   IF NOT(WS-STATUS = "00")
	       MOVE SPACE	 TO WS-REPLY
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
	     DISPLAY W43-DET3(CLIN) AT CPOS
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 7.
	     GO TO COMPANY-GET-USER-OPT.

       COMPANY-CRT-EDGE.
012890	     DISPLAY "Ё   Ё                                        Ё"
		      AT CPOS WITH BACKGROUND-COLOR 7
				   FOREGROUND-COLOR 0
019260	     ADD 1		 TO CLIN.

       COMPANY-RESTORE-SCREEN.
	   IF NOT(W75-S1 = ZERO)
	       MOVE W75-COM(W75-S1) TO W05-EXT.
011320	     PERFORM RESTORE-SCREEN.

       COMPANY-LOOKUP-EXIT.
             EXIT.
