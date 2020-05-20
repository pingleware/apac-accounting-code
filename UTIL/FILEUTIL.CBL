      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *  *******  ******  **	  *******  **	**  ********  **       *
      *  **	    **	  **	  **	   **	**     **     **       *
      *  **	    **	  **	  **	   **	**     **     **       *
      *  *****	    **	  **	  *****    **	**     **     **       *
      *  **	    **	  **	  **	   **	**     **     **       *
      *  **	    **	  **	  **	   **	**     **     **       *
      *  **	  ******  ******* *******   *****      **     *******  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     U T I L I T I E S	F I L E   P A T H S		       *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     FILEUTIL.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 2000.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2000 - 2008
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
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY APACFILE.SL.

       COPY CONTROL.SL.

       COPY SHARED.SL.

       COPY SYSTEM.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FD.

       COPY APACFILE.FD.

       COPY CONTROL.FD.

       COPY SHARED.FD.

       COPY SYSTEM.FD.

000700 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000710 77  WS-S1	   PIC	9(04) COMP-5.
000720 77  WS-S2	   PIC	9(04) COMP-5.
000730 77  WS-S3	   PIC	9(04) COMP-5.
000740 77  WS-S4	   PIC	9(04) COMP-5.
000740 77  WS-S6	   PIC	9(04) COMP-5.
000740 77  WS-S7	   PIC	9(04) COMP-5.
000740 77  WS-S8	   PIC	9(04) COMP-5.
000720 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
000720 77  WS-SECKEY	   PIC	9(04)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
000960 77  WS-ERROR	   PIC	9(01).
000960 77  WS-ERRLOC	   PIC	9(01).
000930 77  WS-OPTION	   PIC	X(01).
000930 77  WS-LOCAL-DRIVE  PIC	X(01).
       77  WS-SPACE	   PIC	X(54) VALUE SPACES.
       77  WS-SYS-ID	   PIC	X(03) VALUE SPACES.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  APAC-VERSION    PIC	X(04) VALUE "2000".
       77  APAC-SYSTEM	   PIC	X(12) VALUE "- File Setup".

000960 01  WS-GRAPHICS.
000970     03  WS-GCHAR        PIC X(01) OCCURS 20.
002060 01  WS-DB-LINE.
002070     03  WS-TOP-LINE.
002080         05  WS-TCHAR    PIC  X(01) OCCURS 80.
002430     03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "зддддддддддддддддддддддддд
      -        "ддддддддддддддддддддддддддддддддддддддддддддддддддддд©".
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

000010 01  WS-FILE-STATUS.
000020     03  WS-STATUS       PIC  X(02).
000030     88  RECORD-LOCKED        VALUE "94" "9D".
000040 01  WS-RED-STAT REDEFINES WS-FILE-STATUS.
000050     03  WS-STAT1        PIC  X(01).
000060         88  RUNTIME-ERROR    VALUE "9".
	   03  WS-STAT1H REDEFINES WS-STAT1
			       PIC  9(02) COMP-X.
000070     03  WS-STAT2        PIC  9(02) COMP-X.
000080         88  FLE-LIMIT        VALUE 14.
000090	       88  IDX-CORRUPT	    VALUE 41 43.
000100         88  FLE-LOCKED       VALUE 65.
000110         88  REC-LOCKED       VALUE 68.
000120         88  TOO-MANY-LOCKS   VALUE 213.

000130 01  WS-FILE-ERRORS.
000140     03  WS-ACTION       PIC  9(01) VALUE ZERO.
000150     03  WS-F-ERROR      PIC  9(02) COMP-5. 
000160	   03  WS-WAIT	       PIC S9(08) COMP-5.
000160     03  WS-COUNT        PIC  9(02) COMP-5 VALUE 1.
000170     03  WS-LOCKED       PIC  9(01) VALUE 0.
000940	   03  WS-STIME	       PIC  9(08).
000940	   03  WS-TIME	       PIC  9(08).
000180	   03  WS-FILE.
	       05  WS-SHRT-FN  PIC  X(06).
	       05  FILLER      PIC  X(18).
000190     03  WS-KEY          PIC  Z(05)9 BLANK WHEN ZERO.
	   03  WS-ERR-MES.
	       05  FILLER      PIC  X(01).
	       05  WS-CONT.
000200		 07  WS-STAT-MESSAGE
			       PIC  X(23).
		 07  WS-KEYX.
		   09 FILLER   PIC X(23) VALUE SPACES.
		   09 WS-ERREND
			       PIC  X(01) VALUE SPACES.
	   03  WS-ERR-CHAR     REDEFINES WS-ERR-MES
			       PIC  X(01) OCCURS 48.
	   03  WS-ERR-STRING   PIC  X(32).

000930 01  WS-CONSOLE	       PIC 9(02) COMP-X.

       01  WS-LENGTH	       PIC 9(08) COMP-5 VALUE 1.

       01  STATUS-CODE	       PIC S9(04) COMP.

      *
      *  Previous versions of COBOL compiler had LIN as a special
      *  register. As the sytem has now advanced to include additional
      *  options that require the MF directive, LIN is no longer
      *  available as a special register. This is only required for the
      *  32 bit system.
      *
      * 01  WS-SPECIAL-REGISTER.
      *	   03  LIN	       PIC S9(04) COMP VALUE ZERO.

000210 01  WS1-CURSOR.
000220     03  CPOS            PIC  9(04) VALUE ZERO.
000230     03  WS1-CURS REDEFINES CPOS.
000240         05  CLIN        PIC  9(02).
000250         05  CCOL        PIC  9(02).

       01  SAVE-POS.
	   03  SAVE-LIN        PIC  9(02) COMP-X VALUE ZERO.
	   03  SAVE-COL        PIC  9(02) COMP-X VALUE ZERO.

000260 01  CSTART.
000270     03  COL-AND-LINE    PIC  9(04).

       01  CRT-LINE.
           03  STORE-LIN       PIC  9(02).
           03  STORE-COL       PIC  9(02).

       01  CRT-DETAIL.
	   03  TOP-ROW	       PIC  9(02) COMP-X.
	   03  BOTTOM-ROW      PIC  9(02) COMP-X.
	   03  STRING-LENGTH   PIC  9(04) COMP-X.
	   03  SCREEN-POSITION.
	       05  SCREEN-LIN  PIC  9(02) COMP-X.
	       05  SCREEN-COL  PIC  9(02) COMP-X.

       01  SCREEN-POS.
	   03  SLIN	       PIC  9(02).
	   03  SCOL	       PIC  9(02).

       01  CUR-CONT.
	   03  CUR-ROW	       PIC  X(01) COMP-X.
	   03  CUR-COL	       PIC  X(01) COMP-X.

       01  BLOCK-DETAIL.
	   03  ORIGINAL-VID.
	       05  ORIGINAL-CHAR PIC  X(01) OCCURS 80.
	   03  REVERSE-VID.
	       05  REVERSE-CHAR  PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL.
	   03  SHADE-ROW       PIC  9(02) COMP-X.
	   03  SHADE-COL       PIC  9(02) COMP-X.
	   03  SHADE-LINES     PIC  9(02) COMP-X.
	   03  SHADE-WIDTH     PIC  9(02) COMP-X.
	   03  SHADE-CHAR      PIC  X(01) VALUE X"08".

       01  KEY-STATUS.
	   03  KEY-TYPE        PIC  X(01).
	       88  NORM-END    VALUE "0".
	       88  USER-FUNC   VALUE "1".
	       88  ADIS-FUNC   VALUE "2".
	       88  DATA-8BIT   VALUE "3".
	       88  DATA-16BIT  VALUE "4".
	   03  KEY-CODE-1      PIC  9(02) COMP-X.
	   03  KEY-CODE-1X REDEFINES KEY-CODE-1
			       PIC  X(01).
	   03  KEY-CODE-2      PIC  9(02) COMP-X.
      *
      *    S C R E E N	 C O L O U R S
      *
      * 		 0 = BLACK
      * 		 1 = BLUE
      * 		 2 = GREEN
      * 		 3 = CYAN
      * 		 4 = RED
      * 		 5 = MAGENTA
      * 		 6 = BROWN / YELLOW
      * 		 7 = WHITE
      *
       01  CRT-COLOURS.
	   03  WS-BGRND		   PIC	9(01) VALUE 1.
	   03  WS-FGRND		   PIC	9(01) VALUE 3.
	   03  WS-BGRND-1	   PIC	9(01) VALUE 3.
	   03  WS-FGRND-1	   PIC	9(01) VALUE 1.
	   03  WS-BGRND-2	   PIC	9(01) VALUE 1.
	   03  WS-FGRND-2	   PIC	9(01) VALUE 7.
	   03  WS-BGRND-3	   PIC	9(01) VALUE 7.
	   03  WS-FGRND-3	   PIC	9(01) VALUE 6.
	   03  WS-BGRND-4	   PIC	9(01) VALUE 3.
	   03  WS-FGRND-4	   PIC	9(01) VALUE 4.
	   03  WS-BGRND-5	   PIC	9(01) VALUE 6.
	   03  WS-FGRND-5	   PIC	9(01) VALUE 2.
	   03  WS-BGRND-6	   PIC	9(01) VALUE 7.
	   03  WS-FGRND-6	   PIC	9(01) VALUE 4.
	   03  WS-BGRND-7	   PIC	9(01) VALUE 0.
	   03  WS-FGRND-7	   PIC	9(01) VALUE 1.
	   03  WS-BGRND-8	   PIC	9(01) VALUE 3.
	   03  WS-FGRND-8	   PIC	9(01) VALUE 4.
	   03  WS-BGRND-9	   PIC	9(01) VALUE 2.
	   03  WS-FGRND-9	   PIC	9(01) VALUE 5.
	   03  WS-CCHNG 	   PIC	9(01) VALUE 0.
	   03  WS-TEMPFG	   PIC	9(01) VALUE 0.
	   03  WS-TEMPBG	   PIC	9(01) VALUE 7.

       01  X91-CALL.
	   03  X91-RES		   PIC	9(02) COMP-X.
      *
      *	   INTERPROGRAM - USING CALL X"91"
      *
      *	   11 = SET COBOL PROGRAM SWITCHES
      *	   12 = READ COBOL PROGRAM SWITCHES
      *	   13 = SET RUN-TIME SWITCHES
      *	   14 = READ RUN-TIME SWITCHES
      *	   15 = CHECK IF A PROGRAM EXISTS
      *	   16 = GET NUMBER OF LINKAGE PARAMETERS
      *	   35 = CALL PROGRAM UNDER DOS ("4B" CALL)
      *	   46 = ENABLE INSERTION OF NULL CHARACTERS
      *	   47 = DISABLE INSERTION OF NULL CHARACTERS
      *	   48 = ENABLE TAB INSERTION
      *	   49 = DISABLE TAB INSERTION
      *
	   03  X91-FUN		   PIC	9(02) COMP-X VALUE 47.

       01  FILE-DETAILS.
	   03  FILE-SIZE	   PIC	X(08) COMP-X.
	   03  FILE-DATE.
	       05  FILE-DAY	   PIC	X(01) COMP-X.
	       05  FILE-MONTH	   PIC	X(01) COMP-X.
	       05  FILE-YEAR	   PIC	X(02) COMP-X.
	   03  FILE-TIME.
	       05  FILE-HOURS	   PIC	X(01) COMP-X.
	       05  FILE-MINUTES    PIC	X(01) COMP-X.
	       05  FILE-SECONDS    PIC	X(01) COMP-X.
	       05  FILE-HUND-SECS  PIC	X(01) COMP-X.

000210 01  WS-MOUSE.
      *
      *    MOUSE - USING CALL X"AF".
      *
      *    64 = ACTIVATE/TERMINATE MOUSE
      * 	PARAM: 0 = Terminate, 1 = Activate.
      *    66 = ENABLE/DISABLE MOUSE
      * 	PARAM: 0 = Disable, 1 = Enable.
      *    67 = GET MOUSE DETAILS.
      * 	Returns X and Y positions
      * 	Sets status.
      *
000220	   03  MOUSE-FUNC      PIC  9(02) COMP-X.
000230	   03  MOUSE-PARAM     PIC  9(02) COMP-X.
000240	   03  MOUSE-DETAILS.
000250	       05  MOUSE-X     PIC  9(04) COMP-X.
000250	       05  MOUSE-Y     PIC  9(04) COMP-X.
000250	       05  MOUSE-STAT  PIC  9(04) COMP-X.
	   03  MOUSE-ENTER     PIC  X(01).

       01  WS-MOUSE2.
      *
      *    MOUSE - USING CALL BY NAME
      *
      *    The mouse status will be non zero if any call made to the
      *    mouse functions is unsuccessfull.
      *
      *    CBL_INIT_MOUSE USING MOUSE-HANDLE MOUSE-BUTTONS
      * 		  RETURNING MOUSE-STATUS.
      *    The mouse must be initialized before any other mouse functions
      *    can be performed. The handle is returned in MOUSE-HANDLE while
      *    the number of buttons on the mouse is returned in MOUSE-BUTTONS.
      *
      *    CBL_GET_MOUSE_MASK USING MOUSE-HANDLE EVENT-MASK
      * 		      RETURNING MOUSE-STATUS.
      *    This call gets the event mask -
      * 	     bit 1 to 3 = buttons 1 to 3
      * 	     bit 0	= mouse moved
      *
      *    CBL_GET_MOUSE_POSITION USING MOUSE-HANDLE MOUSE-POS
      * 			  RETURNING MOUSE-STATUS.
      *    This call is used to get the row and column location of
      *    the mouse.
      *
      *    CBL_GET_MOUSE_STATUS USING MOUSE-HANDLE MOUSE-EVENT
      * 			RETURNING MOUSE-STATUS.
      *    This function is used to find out the number of events in the
      *    queue.
      *
      *    CBL_HIDE_MOUSE USING MOUSE-HANDLE
      * 		  RETURNING MOUSE-STATUS.
      *    Makes the mouse pointer invisible.
      *
      *    CBL_READ_MOUSE_EVENT USING MOUSE-HANDLE EVENT-DATA READ-TYPE
      * 			RETURNING MOUSE-STATUS.
      *    Reads the mouse event queue and returns information about
      *    an event.
      *    READ-TYPE = 0. If no events, returns imediately with zero
      * 		  values.
      * 	     = 1. Return is delayed until an event has been
      * 		  queued.
      *
      *    CBL_SET_MOUSE_MASK USING MOUSE-HANDLE EVENT-MASK
      * 		      RETURNING MOUSE-STATUS.
      *    Set the mouse event mask.
      *
      *    CBL_SET_MOUSE_POSITION USING MOUSE-HANDLE MOUSE-POS
      * 			  RETURNING MOUSE-STATUS.
      *    Move mouse pointer to position specified.
      *
      *    CBL_SHOW_MOUSE USING MOUSE-HANDLE MOUSE-POS
      * 		  RETURNING MOUSE-STATUS.
      *    Make the mouse pointer visible.
      *
      *    CBL_TERM_MOUSE USING MOUSE-HANDLE
      * 		  RETURNING MOUSE-STATUS.
      *    Terminate mouse support.
      *
      *    PC_GET_MOUSE_SHAPE USING MOUSE-HANDLE
      * 			    RESEVERD-ITEM
      * 			    MOUSE-PTR-SHAPE
      * 		      RETURNING MOUSE-STATUS.
      *    Get the shape of the mouse pointer. (bit map)
      *
      *    PC_SET_MOUSE_HIDE_AREA USING MOUSE-HANDLE
      * 				COLLISION-AREA
      * 			  RETURNING MOUSE-STATUS.
      *    Defines the area where the mouse is to be invisible.
      *
      *    PC_SET_MOUSE_SHAPE USING MOUSE-HANDLE
      * 			    RESEVERD-ITEM
      * 			    MOUSE-PTR-SHAPE
      * 		      RETURNING MOUSE-STATUS.
      *    Set the shape of the mouse pointer.
      *
	   03  MOUSE-HANDLE    PIC  X(04) COMP-X.
	   03  MOUSE-EVENT     PIC  X(02) COMP-X.
	   03  MOUSE-POS.
	       05  MOUSE-ROW   PIC  X(02) COMP-X.
	       05  MOUSE-COL   PIC  X(02) COMP-X.
	   03  MOUSE-BUTTONS   PIC  X(02) COMP-X.
	   03  MOUSE-STATUS    PIC S9(04) COMP.
	   03  COLLISION-AREA.
	       05  TOP-LIN     PIC  X(02) COMP-X.
	       05  LEFT-COL    PIC  X(02) COMP-X.
	       05  BOTTOM-LIN  PIC  X(02) COMP-X.
	       05  RIGHT-COL   PIC  X(02) COMP-X.
	   03  MOUSE-PTR-SHAPE.
	       05  CHAR-AND-MASK PIC  X(01) COMP-X.
	       05  ATTR-AND-MASK PIC  X(01) COMP-X.
	       05  CHAR-XOR-MASK PIC  X(01) COMP-X.
	       05  ATTR-XOR-MASK PIC  X(01) COMP-X.
	   03  EVENT-DATA.
	       05  EVENT-TYPE	 PIC  X(02) COMP-X.
	       05  EVENT-TIME	 PIC  X(02) COMP-X.
	       05  EVENT-ROW	 PIC  X(02) COMP-X.
	       05  EVENT-COL	 PIC  X(02) COMP-X.
	   03  RESERVED-ITEM	 PIC  X(10).
	   03  READ-TYPE	 PIC  X(01) COMP-X.
	   03  MOUSE-ATTACHED	 PIC  X(01) VALUE "N".
	       88  MOUSE	 VALUE "Y".
	       88  NO-MOUSE	 VALUE "N".

       01  SCREEN-GRAPHICS.
      *
      *   DECIMAL CODES USING THE ALTERNATE KEY AND NUMERIC KEY PAD
      *  иммммкмммямммямммямммямммямммямммямммямммяммм╩
      *  ╨PREF╨ 0 Ё 1 Ё 2 Ё 3 Ё 4 Ё 5 Ё 6 Ё 7 Ё 8 Ё 9 ╨
      *  лммммнмммьмммьмммьмммьмммьмммьмммьмммьмммьммм╧
      *  ╨ 17 ╨   Ё   Ё   Ё   Ё   Ё   Ё ╟ Ё ╠ Ё ╡ Ё Ё ╨
      *  гддддвдддедддедддедддедддедддедддедддедддеддд╤
      *  ╨ 18 ╨ ╢ Ё ╣ Ё ╤ Ё ╥ Ё ╦ Ё ╧ Ё ╨ Ё ╩ Ё ╪ Ё ╫ ╨
      *  гддддвдддедддедддедддедддедддедддедддедддеддд╤
      *  ╨ 19 ╨ ╬ Ё © Ё ю Ё а Ё б Ё ц Ё д Ё е Ё ф Ё г ╨
      *  гддддвдддедддедддедддедддедддедддедддедддеддд╤
      *  ╨ 20 ╨ х Ё и Ё й Ё к Ё л Ё м Ё н Ё о Ё п Ё я ╨
      *  гддддвдддедддедддедддедддедддедддедддедддеддд╤
      *  ╨ 21 ╨ р Ё с Ё т Ё у Ё ж Ё в Ё ь Ё ы Ё з Ё ш ╨
      *  хммммймммомммомммомммомммомммомммомммомммоммм╪
000110	   03  WS-G1	       PIC  X(01) VALUE "м".
000120	   03  WS-G2	       PIC  X(01) VALUE "я".
000130	   03  WS-G3	       PIC  X(01) VALUE "Ё".
000140	   03  WS-G4	       PIC  X(01) VALUE "о".
000150	   03  WS-G5	       PIC  X(01) VALUE "к".
000160	   03  WS-G6	       PIC  X(01) VALUE "╨".
000170	   03  WS-G7	       PIC  X(01) VALUE "й".
000180	   03  WS-G8	       PIC  X(01) VALUE "д".
000190	   03  WS-G9	       PIC  X(01) VALUE "з".
000200	   03  WS-G10	       PIC  X(01) VALUE "©".
000210	   03  WS-G11	       PIC  X(01) VALUE "ю".
000220	   03  WS-G12	       PIC  X(01) VALUE "ы".
000230	   03  WS-G13	       PIC  X(01) VALUE "╣".
000240	   03  WS-G14	       PIC  X(01) VALUE "ф".
000250	   03  WS-G15	       PIC  X(01) VALUE "и".
000260	   03  WS-G16	       PIC  X(01) VALUE "╩".
000270	   03  WS-G17	       PIC  X(01) VALUE "х".
000280	   03  WS-G18	       PIC  X(01) VALUE "╪".
	   03  WS-G19	       PIC  X(01) VALUE "╟".
	   03  WS-G20	       PIC  X(01) VALUE "╠".
	   03  WS-G21	       PIC  X(01) VALUE "╡".
	   03  WS-G22	       PIC  X(01) VALUE "ш".
	   03  WS-BACKGROUND   PIC  X(80) VALUE ALL "╟".

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY APACFILE.ID.

       COPY CONTROL.ID.

       COPY SHARED.ID.

       COPY SYSTEM.ID.

       COPY W12.WS.

       01  W10-FILE-PATHS.
	   03  W10-CURRENT-PATH  PIC  X(52).
	   03  W10-CURR-PATH	 REDEFINES W10-CURRENT-PATH.
	       05  W10-CURR-CHAR PIC  X(01) OCCURS 52.
	   03  W10-NEW-PATH	 PIC  X(52).
	   03  W10-NW-PATH	 REDEFINES W10-NEW-PATH.
	       05  W10-NEW-CHAR  PIC  X(01) OCCURS 52.
	   03  W10-PATH 	 PIC  X(09).
	   03  W10-COM-PATH	 REDEFINES W10-PATH.
	       05  W10-COM-CHAR  PIC  X(01) OCCURS 9.

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
       01  W31-DAYS.
           03  W31-DAY-NUMBERS.
               05  W31-DAYNO   PIC  X(36)    VALUE 
                               "000031059090120151181212243273303334".
               05  W31-DN REDEFINES W31-DAYNO.
                   07  W31-NO  PIC 9(03) OCCURS 12.
	   03  W31-DTE1        PIC 9(06) COMP-5.
	   03  W31-DTE2        PIC 9(06) COMP-5.

       COPY W40.WS.

       COPY FUNCTION.WS.

       01  W75-LOOKUP.
	   03  W75-S	     PIC  9(04) COMP-5.
	   03  W75-S1	     PIC  9(04) COMP-5.
	   03  W75-S2	     PIC  9(04) COMP-5.
	   03  W75-POS	     PIC  9(04) COMP-5.
	   03  W75-FIRST     PIC  9(04) COMP-5.
	   03  W75-LINES     PIC  9(04) COMP-5.
	   03  W75-UPDATE    PIC  X(01).
	   03  W75-APATH     PIC  X(52).
	   03  W75-ENTRY     OCCURS 200.
	       05  W75-KEY   PIC  X(08).
	       05  W75-FILE  PIC  X(21).
	       05  W75-PATH  PIC  X(52).
	   03  W75-GLOBAL    PIC  X(40).
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

       01  S01.
	   03  LINE  3 COLUMN  2 REVERSE-VIDEO VALUE
		      "   File Description          File Path - (May inc
      - 	      "lude the drive location)     ".
	   03  LINE  4 COLUMN  1 BACKGROUND-COLOR 0 VALUE "ф".
	   03	       COLUMN  2 VALUE "мммммммммммммммммммммммямммммммм
      -		      "мммммммммммммммммммммммммммммммммммммммммммммм".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "╣".
	   03  LINE  5 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE  6 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE  7 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE  8 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE  9 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 10 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 11 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 12 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 13 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 14 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 15 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 16 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 17 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 18 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 19 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 20 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 21 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 22 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.
	   03  LINE 23 COLUMN  1 BACKGROUND-COLOR 0 VALUE "ф".
	   03	       COLUMN  2 VALUE "мммммммммммммммммммммммомммммммм
      -		      "мммммммммммммммммммммммммммммммммммммммммммммм".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "╣".

       01  S02.
	   03	       COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(23) FROM WS-SPACE.
	   03	       COLUMN 25 VALUE "Ё".
	   03	       COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(54) FROM WS-SPACE.

       01  S03.
	   03	       COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(52) USING W75-APATH AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
007730 PROCEDURE DIVISION.
007740 AA000-MAIN              SECTION.
007750 AA000-INIT.
	     MOVE ZERO		 TO WS-ERROR
				    WS-ERRLOC.
	     PERFORM AB000.
	   IF WS-ERROR = 9
	       GO TO AA99.

       AA05.
	     MOVE ZERO		 TO WS-ERROR.
	     PERFORM AC000.
	     PERFORM RESTORE-SCREEN-3.
	   IF WS-ERROR = 9
	       GO TO AA99.
	     PERFORM AD000.
	     GO TO AA05.

       AA90.
	     MOVE "Abnormal termination - Reload files"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.

       AA99.
	     STOP RUN.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  SCREEN-SHADOW 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:						       Ё
      *	   Ё	       SHADE-ROW   - Top line of the box + 1.	       Ё
      *	   Ё	       SHADE-COL   - Left line of box + 2.	       Ё
      *	   Ё	       SHADE-WIDTH - Width of the box - 2.	       Ё
      *	   Ё	       SHADE-LINES - Hight of box - 1.		       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  CHECK-CORRECT 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine displays a pop-up window with the message    Ё
      *    Ё	      "Correct ENTER - N if incorrect"		       Ё
      *    Ё							       Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё							       Ё
      *    Ё To start with the pop-up window higher or lower than row  Ё
      *    Ё 13 (default); move another value to SLIN and PERFORM      Ё
      *    Ё CHECK-SETUP THRU CHECK-EXIT.			       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  ERROR-MESSAGE 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display an error message with " - Press ANY key" at    Ё
      *    Ё the end; use PERFORM ERROR-MESSAGE.		       Ё
      *    Ё							       Ё
      *    Ё To display just the error message; use PERFORM	       Ё
      *    Ё ERROR-LENGTH THRU ERROR-EXIT.			       Ё
      *    Ё							       Ё
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   Ё
      *    Ё or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     Ё
      *    Ё number to be used to SLIN and then PERFORM ERROR-DISPLAY  Ё
      *    Ё THRU ERROR-EXIT.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY FUNCTION.CRT.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			  OPT-MESSAGE			       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine is used to allow the OPERATOR to respond to  Ё
      *    Ё a request for an option, so that the correct action can   Ё
      *    Ё be performed by the program. The routine will display the Ё
      *    Ё message in a pop-up window and allow the OPERATOR to      Ё
      *    Ё respond to the 'question'. 			       Ё
      *    Ё							       Ё
      *    Ё The option request must be placed in WS-ERR-MES and may   Ё
      *    Ё not exceed 48 characters. The message will be centred in  Ё
      *    Ё the window. An example of a message request follows:      Ё
      *    Ё							       Ё
      *    Ё   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      Ё
      *    Ё   PERFORM OPT-MESSAGE.				       Ё
      *    Ё							       Ё
      *    Ё This would be displayed as:			       Ё
      *    Ё	здддддддддддддддддддддддддддддддддддддддддддддддд©     Ё
      *    Ё	Ё	   Print transactions (Y/N) [ ] 	 Ё╟╟   Ё
      *    Ё	юдддддддддддддддддддддддддддддддддддддддддддддддды╟╟   Ё
      *    Ё	  ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟   Ё
      *    Ё							       Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё							       Ё
      *    Ё To display the request message higher or lower (default   Ё
      *    Ё is line 13) move the line number to be used to SLIN and   Ё
      *    Ё then PERFORM OPT-SETUP THRU OPT-EXIT.		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY OPTION.CRT.

      *    *************************************************************
      *    ****   G E T   D A T E   &	I N I T I A L I Z E   P R O G
      *    *************************************************************
       AB000		       SECTION 50.
       AB00.
005090       DISPLAY SPACE AT 0101.
	     ACCEPT W25-FACT FROM DATE.
	     MOVE W25-YY	 TO W12-YEAR.
	     MOVE W25-MM	 TO W12-MONTH.
	     MOVE W25-DD	 TO W12-DAY.
	   IF W25-YY < 50
	       MOVE 20		 TO W12-CENT
	   ELSE
	       MOVE 19		 TO W12-CENT.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
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
	       GO TO AB02.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       Set MOUSE key to act as FUNCTION key	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.

       AB02.
	     DISPLAY CLR-SCREEN.
	     DISPLAY "DATA FILES - PATH MAINTENANCE" AT 0228
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     PERFORM SAVE-SCREEN-3.

       AB04.
	     MOVE "Have backups been made 'Y/N'  [ ]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO AB04.
	   IF WS-OPTION = "N"
	       MOVE 9		 TO WS-ERROR.

       AB99.
	     EXIT.
      *    *************************************************************
      *    ****   G E T   S Y S T E M	I D
      *    *************************************************************
       AC000		       SECTION 51.
       AC00.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 15		 TO SHADE-COL.
	     MOVE 52		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY
	      "здддддддддддддддддддддддддддддддддддддддддддддддддддд"
	       AT 1013 WITH FOREGROUND-COLOR 0 HIGHLIGHT
			    BACKGROUND-COLOR 7
	      "©" WITH FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 7.
	     DISPLAY
	      "Ё" AT 1113 WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 7
	      " Key in the code for the set of files that must be  "
	       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	      "Ё" WITH FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 7.
	     DISPLAY
	      "Ё" AT 1213 WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 7
	      " updated.   "
	       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	      "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			 BACKGROUND-COLOR 7
	      "ape to exit. "
	       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	      "                        "
	       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	      "Ё" WITH FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 7.
	     DISPLAY
	      "ц" AT 1313 WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 7
	      "дддддддддддддддддддддддддддддддддддддддддддддддддддд"
	       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	      "╢" WITH FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 7.
	     DISPLAY
	      "Ё" AT 1413 WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 7
	      " CODE: "
	       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	      "   " WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5
	      " " WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	      "                                        "
	       WITH FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
	      " Ё" WITH FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 7.
	     DISPLAY
	      "ю" AT 1513 WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 7
	      "дддддддддддддддддддддддддддддддддддддддддддддддддддды"
	       WITH FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 7.
             PERFORM SCREEN-SHADOW.

       AC05.
	     MOVE SPACES	 TO WS-SYS-ID.

       AC10.
	     ACCEPT WS-SYS-ID AT 1421
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO AC99
		 WHEN OTHER
                     CALL X"E5"
		     GO TO AC10
	       END-EVALUATE.
	   IF WS-SYS-ID = SPACES
	       CALL X"E5"
	       GO TO AC10.
	     MOVE 3		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING WS-SYS-ID
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE WS-SYS-ID	 TO W02-SYSPTH.
	     MOVE ZERO		 TO RETURN-CODE.
	     CALL "CBL_CHECK_FILE_EXIST" USING W02-APACFILE
					       FILE-DETAILS.
	   IF NOT (RETURN-CODE = ZERO)
	       MOVE SPACES	 TO WS-ERR-MES
	       STRING "Code " DELIMITED SIZE
		       WS-SYS-ID DELIMITED SIZE
		      " is invalid" DELIMITED SIZE
		       INTO WS-ERR-MES
	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
	       GO TO AC10.
001060	     OPEN I-O APACFILE.
006200     IF RUNTIME-ERROR
               IF FLE-LOCKED
006210		   MOVE "Files locked - try later - Press ENTER"
				 TO WS-ERR-MES
		   PERFORM OPT-MESSAGE
006240		   GO TO AC10.
006250     IF WS-STATUS = "30"
006260	       MOVE "Invalid or file not on disk"
				 TO WS-ERR-MES
	       PERFORM OPT-MESSAGE
006270	       GO TO AC10.
	     MOVE W02-SYSPTH	 TO W02-SYSID.
001060	     OPEN I-O APACFIDS.
      *    *************************************************************
      *    ****    C O N T R O L   F I L E
      *    *************************************************************
       AC15.
	     MOVE "NETWORK"	 TO AFID-KEY.

       AC15-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO AC15-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       AC15-READ-APACFIDS-EXIT.
	     EXIT.

       AC15-CONTINUE.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM AC15-READ-APACFIDS THRU AC15-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "SYSTEM"	 TO AFID-KEY.
	     PERFORM AC15-READ-APACFIDS THRU AC15-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SYSTEM.
	     OPEN INPUT SECUR.
	   IF NOT (WS-STATUS = "00")
	       GO TO AC20.
	     MOVE 2		 TO WS-SECKEY.
006300       READ SECUR.
	   IF WS-STATUS = "00"
	       GO TO AC25.

       AC20.
      *	     MOVE SPACES	 TO WS-ERR-MES.
006260*	     STRING "Invalid or file not on disk - "
      *		     DELIMITED SIZE
      *		     WS-STATUS DELIMITED SIZE
      *		     INTO WS-ERR-MES.
      *	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370*	     STOP RUN.
	     MOVE "System files not at current Location"
				 TO SEC-COMP.
	     MOVE 1		 TO WS-ERRLOC.

       AC25.
	     DISPLAY SEC-COMP AT 1425
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 19		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
020250	   IF WS-OPTION = "N"
	       CLOSE APACFILE
		     APACFIDS
		     SECUR
	       GO TO AC00.
	   IF WS-ERRLOC = 1
	       GO TO AC99.
	     OPEN INPUT NETWORK.
	   IF NOT (WS-STATUS = "00")
	       GO TO AC20.
	     OPEN INPUT	SHARED.
	   IF NOT (WS-STATUS = "00")
	       GO TO AC20.

       AC99.
	     EXIT.
      *    *************************************************************
      *    ****   M A I N T A I N   F I L E   P A T H S
      *    *************************************************************
       AD000		       SECTION 52.
       AD00.
	     DISPLAY S01.
	     MOVE SPACES	 TO APAC-KEY.
000030	     START APACFILE
	       KEY >= APAC-KEY.

       AD05.
	     MOVE 1		 TO W75-S.
	     MOVE 5		 TO CLIN.

       AD10.
	     MOVE SPACES	 TO W75-KEY(W75-S)
				    W75-FILE(W75-S)
				    W75-PATH(W75-S).
	   IF W75-S < 200
	       ADD 1		 TO W75-S
	       GO TO AD10.
	     MOVE 0		 TO W75-S
				    W75-S2.
	     MOVE 1		 TO W75-S1.

       AD15.
000320	     READ APACFILE NEXT.
000350	   IF NOT (WS-STAT1 = "0")
	       MOVE W75-S	 TO W75-S2
	       MOVE 1		 TO W75-S
				    W75-POS
030020				    W75-LINES
				    W75-FIRST
	       MOVE "N" 	 TO W75-UPDATE
000370	       GO TO AD20.
	     ADD 1		 TO W75-S.
	     MOVE APAC-KEY	 TO W75-KEY(W75-S).
	     MOVE APAC-NAME	 TO W75-FILE(W75-S).
	     MOVE APAC-PATH	 TO W75-PATH(W75-S).
	     GO TO AD15.

       AD20.
	     MOVE W75-FIRST	 TO W75-POS W75-S.
	     MOVE 5		 TO CLIN.
      *    *************************************************************
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *    *************************************************************
       AD22.
	   IF W75-S > W75-S2
	       GO TO AD35.

       AD25.
	     MOVE 03		 TO CCOL.
	     DISPLAY W75-FILE(W75-S) AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 27		 TO CCOL.
	     DISPLAY W75-PATH(W75-S) AT CPOS
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       AD30.
	   IF CLIN < 22
	       IF W75-S < W75-S2
		   ADD 1	 TO CLIN
				    W75-S
		   GO TO AD22.

       AD35.
	     MOVE 4		 TO LIN.
	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    D I S P L A Y   O P T I O N S
      *    *************************************************************
       AD40.
001020	     DISPLAY " " AT 2503 WITH FOREGROUND-COLOR 14
					BACKGROUND-COLOR 3
001030		     " highlight," WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "PgUp" WITH FOREGROUND-COLOR 14
				 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1
			      BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 14
				 BACKGROUND-COLOR 3
		     "," WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3
001040		     "A" WITH FOREGROUND-COLOR 14
				  BACKGROUND-COLOR 3
001050		     "mend," WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
001040		     "G" WITH FOREGROUND-COLOR 14
				  BACKGROUND-COLOR 3
001050		     "lobal," WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 14
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3.
      *    *************************************************************
      *    ****    S A V E   C U R R E N T   P A G E
      *    *************************************************************
       AD42.
	     PERFORM SAVE-SCREEN-2.
      *    *************************************************************
      *    ****    S C R E E N	 A T T R I B U T E S
      *    *************************************************************
       AD45.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.
      *    *************************************************************
      *    ****    G E T   U S E R   O P T I O N
      *    *************************************************************
030520 AD50.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO AD55
		 WHEN DOWN-KEY
001340		     GO TO AD75
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO AD50
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO AD115
      *    *************************************************************
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
		 WHEN PAGE-DOWN
001340		     GO TO AD100
      *    *************************************************************
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *    *************************************************************
		 WHEN PAGE-UP
001340		     GO TO AD95
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO AD50
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION = "A") AND
		  (LIN > 4)
		   DISPLAY CLEAR-L25
		   GO TO AD105
	       ELSE
	       IF WS-OPTION = "G"
		   DISPLAY CLEAR-L25
		   PERFORM AD45
		   PERFORM AE000
		   GO TO AD00.
	     CALL X"E5".
001280	     GO TO AD50.
      *    *************************************************************
      *    ****    U P	 K E Y	 ( M O V E   H I G H L I G H T	 U P )
      *    *************************************************************
       AD55.
	   IF (W75-POS = 1) OR (LIN < 5)
	       GO TO AD85.
	     SUBTRACT 1 	 FROM W75-POS.
	   IF LIN > 5
	       SUBTRACT 1	 FROM LIN
	       GO TO AD85.
	     MOVE 22		 TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      * 	    O N E   L I N E
      *    *************************************************************
       AD60.
	     SUBTRACT 1 FROM WS-S7
				 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S7 > 6
	       SUBTRACT 1	 FROM WS-S7
	       GO TO AD60.
	     MOVE W75-POS	 TO W75-FIRST.
      *    *************************************************************
      *    ****    C L E A R   F I R S T   O R	 L A S T
      * 	   D E T A I L	 L I N E.
      *    *************************************************************
       AD65.
	     PERFORM RESTORE-SCREEN-2.
	     DISPLAY S02 AT LINE LIN.
      *    *************************************************************
      *    ****    G E T   N E X T   O R   P R E V I O U S
      * 	   D E T A I L	 L I N E.
      *    *************************************************************
030080	     MOVE W75-POS	 TO W75-S.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
	     PERFORM AD25.

       AD70.
	     PERFORM SAVE-SCREEN-2.
	     GO TO AD85.
      *    *************************************************************
      *    ****    D O W N   K E Y   ( M O V E	 H I G H L I G H T
      * 			       D O W N )
      *    *************************************************************
       AD75.
	   IF (W75-POS = W75-S2) AND (LIN > 4)
	       GO TO AD85.
      *    *************************************************************
      *    ****    W75-POS CONTAINS POSITION OF HIGHLIGHTED ITEM
      *    *************************************************************
	   IF LIN > 4
	       ADD 1		 TO W75-POS.
	   IF LIN < 22
	       ADD 1		 TO LIN
	       GO TO AD85.
	     MOVE 5		 TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      * 	    L I N E
      *    *************************************************************
       AD80.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 22
	       ADD 1		 TO WS-S7
	       GO TO AD80.
	     ADD 1		 TO W75-FIRST.
	     GO TO AD65.
      *    *************************************************************
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *    *************************************************************
001980 AD85.
001990	     PERFORM AD45.
	   IF LIN > 4
      *	       MOVE LIN 	 TO CLIN
      *	       MOVE 1		 TO CCOL
002000*	       DISPLAY W43-DET (LIN) AT CPOS
002000	       DISPLAY (LIN, 1) W43-DET (LIN)
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
002010	     GO TO AD50.
      *    *************************************************************
      *    ****    C L E A R   S C R E E N
      *    *************************************************************
030780 AD90.
	     DISPLAY S01.
030810	     MOVE 5		 TO LIN.
030820	     MOVE 1		 TO W75-LINES.
      *    *************************************************************
      *    ****   P R E V I O U S   S C R E E N	  R E Q U I R E D
      *    *************************************************************
       AD95.
	   IF W75-FIRST = 1
	       GO TO AD50.
	   IF W75-FIRST > 18
	       SUBTRACT 18	 FROM W75-FIRST
	   ELSE
	       MOVE 1		 TO W75-FIRST.
	     PERFORM AD90.
	     GO TO AD20.
      *    *************************************************************
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
       AD100.
	   IF (W75-FIRST + 18) > W75-S2
	       GO TO AD50.
	     ADD 18		 TO W75-FIRST.
	     PERFORM AD90.
	     GO TO AD20.
      *    *************************************************************
      *    ****    G E T   T H E   D E T A I L S   F O R   T H E
      * 	   F I L E   P A T H   T O   B E   A M E N D E D
      *    *************************************************************
       AD105.
031220	     MOVE W75-PATH(W75-POS)
				 TO W75-APATH.
	     PERFORM AD45.
	     DISPLAY CLEAR-L25.
001020	     DISPLAY "Esc" AT 2503 WITH FOREGROUND-COLOR 14
					BACKGROUND-COLOR 3
001030		     "ape to ignore amendment"
				   WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3.

       AD110.
	     ACCEPT S03 AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO AD85
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO AD110.
031220	     MOVE W75-APATH	 TO W75-PATH(W75-POS).
	     MOVE "Y"		 TO W75-UPDATE.
001990	     PERFORM AD45.
	     PERFORM SAVE-SCREEN-2.
	   IF LIN > 4
      *	       MOVE LIN 	 TO CLIN
      *	       MOVE 1		 TO CCOL
002000*	       DISPLAY W43-DET (LIN) AT CPOS
002000	       DISPLAY (LIN, 1) W43-DET (LIN)
		       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM AD40.
002010	     GO TO AD50.

       AD115.
	   IF W75-UPDATE = "N"
	       GO TO AD130.
	     MOVE 1		 TO W75-S.

       AD120.
	     MOVE W75-KEY(W75-S) TO APAC-KEY.
	     READ APACFILE.
	     MOVE W75-PATH(W75-S)
				 TO APAC-PATH.
	     REWRITE APAC-RECORD1.
	   IF W75-S < W75-S2
	       ADD 1		 TO W75-S
	       GO TO AD120.
	     CLOSE APACFIDS.
	     OPEN I-O APACFIDS.
	     MOVE SPACES	 TO APAC-KEY.
000030	     START APACFILE
	       KEY >= APAC-KEY.

       AD125.
000320	     READ APACFILE NEXT WITH IGNORE LOCK.
000350	   IF NOT (WS-STAT1 = "0")
000370	       GO TO AD130.
	     MOVE APAC-KEY	 TO AFID-KEY.
	     MOVE SPACES	 TO AFID-PATH.
	     STRING APAC-PATH DELIMITED BY " "
		    APAC-NAME DELIMITED BY " "
		    INTO AFID-PATH.
	     REWRITE AFID-RECORD1.
	     GO TO AD125.

       AD130.
	     CLOSE APACFILE
		   APACFIDS
		   SECUR.
	   IF NOT (WS-ERRLOC = 1)
	       CLOSE NETWORK
		     SHARED.

       AD999.
	     EXIT.
      *    *************************************************************
      *    **** 	      G L O B A L   C H A N G E		    ****
      *    *************************************************************
       AE000		       SECTION 52.
       AE00.
	     MOVE "N"		 TO WS-OPTION.

       AE05.
	     MOVE "Amend 'N'etwork or 'L'ocal path  [ ]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "L" OR "N")
	       GO TO AE05.
	     MOVE WS-OPTION	 TO WS-LOCAL-DRIVE.
	   IF WS-LOCAL-DRIVE = "L"
	       CLOSE APACFILE
		     APACFIDS
		     SECUR
	       IF NOT(WS-ERRLOC = 1)
		   CLOSE NETWORK
			 SHARED
	       END-IF
	       MOVE "OFFLINE\"   TO W02-L-OR-N
	       PERFORM AF000
	       IF WS-ERROR = 9
		   MOVE "Check local files (OFFLINE)"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   STOP RUN.
      *    *************************************************************
      *    ****    C O N T R O L   F I L E
      *    *************************************************************
       AE10.
	     MOVE "NETWORK"	 TO APAC-KEY.

       AE15-READ-APACFILE.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF WS-STATUS = "00"
	       GO TO AE15-READ-APACFILE-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     APAC-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
	     GO TO AE75.

       AE15-READ-APACFILE-EXIT.
	     EXIT.

       AE20-CONTINUE.
	     MOVE APAC-PATH	 TO W10-CURRENT-PATH.
	     MOVE 52		 TO WS-S8.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		     Find the end of the path.		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AE25-LOOP.
	   IF W10-CURR-CHAR(WS-S8) = SPACE
	       SUBTRACT 1	 FROM WS-S8
	       GO TO AE25-LOOP.
	     SUBTRACT 9 FROM WS-S8
				 GIVING WS-S7.
	   IF WS-S7 = ZERO
	       MOVE SPACES	 TO W10-CURRENT-PATH
	       GO TO AE35.
	     ADD 1		 TO WS-S7.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Clear the last nine characters of the path (???_DATA\) /  Ё
      *    Ё (???_WORK\) leaving only the USER portion to be edited.   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AE30-LOOP.
	     MOVE SPACE 	 TO W10-CURR-CHAR(WS-S8).
	   IF WS-S8 > WS-S7
	       SUBTRACT 1	 FROM WS-S8
	       GO TO AE30-LOOP.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		 Move the USER portion of the path.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AE35.
	     MOVE W10-CURRENT-PATH
				 TO W10-NEW-PATH.
      *
      * 	 здддддддддддддддддддддддддддддддддддддддддддддддд©
      * 	 Ё  ADD DRIVE LETTER/PATH/.... TO SYSTEM PATHS	  Ё╟╟
      * 	 цдддддддддддддддддддддддддддддддддддддддддддддддд╢╟╟
      * 	 ЁCurrent:					  Ё╟╟
      * 	 ЁNew	 :					  Ё╟╟
      * 	 юдддддддддддддддддддддддддддддддддддддддддддддддды╟╟
      * 	   ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟
      *
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 18		 TO SHADE-COL.
	     MOVE 48		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY "здддддддддддддддддддддддддддддддддддддддддддддддд"
		     AT 1016 WITH FOREGROUND-COLOR 0
				  BACKGROUND-COLOR 7
		     "©" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "Ё   " AT 1116 WITH FOREGROUND-COLOR 0
					 BACKGROUND-COLOR 7
		     "ADD DRIVE LETTER/PATH/.... TO SYSTEM PATHS"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "   Ё" WITH FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 7.
	     DISPLAY "ц" AT 1216 WITH FOREGROUND-COLOR 0
				      BACKGROUND-COLOR 7
		     "дддддддддддддддддддддддддддддддддддддддддддддддд"
		     WITH FOREGROUND-COLOR 0 HIGHLIGHT
			  BACKGROUND-COLOR 7
		     "╢" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "ЁCurrent:                                        "
		     AT 1316 WITH FOREGROUND-COLOR 0
				  BACKGROUND-COLOR 7
		     "Ё" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "ЁNew    : 				       "
		     AT 1416 WITH FOREGROUND-COLOR 0
				  BACKGROUND-COLOR 7
		     "Ё" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "ю" AT 1516 WITH FOREGROUND-COLOR 0
				      BACKGROUND-COLOR 7
		     "дддддддддддддддддддддддддддддддддддддддддддддддды"
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 7.
             PERFORM SCREEN-SHADOW.
	     MOVE W10-CURRENT-PATH
				 TO W75-GLOBAL.
	     DISPLAY W75-GLOBAL AT 1325
		     WITH FOREGROUND-COLOR 5 BACKGROUND-COLOR 7.

       AE40.
	     ACCEPT W75-GLOBAL AT 1425
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     MOVE 40		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W75-GLOBAL
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE "'C'ontinue or 'A'mend  [ ]"
				 TO WS-ERR-MES.
	     MOVE 17		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF WS-OPTION = "A"
	       GO TO AE40.
	     MOVE W75-GLOBAL	 TO W10-NEW-PATH.
	     MOVE 52		 TO WS-S8.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		     Find the end of the path.		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AE45-LOOP.
	   IF W10-NEW-CHAR(WS-S8) = SPACE
	       SUBTRACT 1	 FROM WS-S8
	       GO TO AE45-LOOP.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё If more than the drive ID is being added, the path should Ё
      *    Ё terminate with the '\' character to form a new path.      Ё
      *    Ё eg. F:\COMPANY\ which would then have the ???_DATA\       Ё
      *    Ё appended to create the new path F:\COMPANY\???_DATA\      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF NOT (W10-NEW-CHAR(WS-S8) = "\")
	       MOVE "Path does not end with \ Continue Y/N  [ ]"
				 TO WS-ERR-MES
	       MOVE 17		 TO SLIN
	       PERFORM OPT-SETUP THRU OPT-EXIT
	       IF NOT (WS-OPTION = "Y")
		   GO TO AE40.
      *    *************************************************************
      *    ****   S T A R T   F R O M	B E G I N N I N G   O F
      * 	  A P A C F I L E
      *    *************************************************************
	     MOVE SPACES	 TO APAC-KEY.
	     START APACFILE
	       KEY >= APAC-KEY.
      *    *************************************************************
      *       G E T   T H E   N E X T	A P A C F I L E   R E C O R D
      *    *************************************************************
       AE50.
	     READ APACFILE NEXT.
      *    *************************************************************
      *    ****		     E N D   O F   F I L E  ?
      *    *************************************************************
000350	   IF NOT (WS-STAT1 = "0")
	       GO TO AE80.
	     MOVE APAC-PATH	 TO W10-CURRENT-PATH.
	     MOVE 52		 TO WS-S8.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		     Find the end of the path.		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AE55-LOOP.
	   IF W10-CURR-CHAR(WS-S8) = SPACE
	       SUBTRACT 1	 FROM WS-S8
	       GO TO AE55-LOOP.
	     SUBTRACT 8 FROM WS-S8
				 GIVING WS-S7.
	     MOVE 1		 TO WS-S6.
	     MOVE SPACES	 TO W10-PATH.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Move the last nine characters of the path (???_DATA\) /   Ё
      *    Ё (???_WORK\) to W10-PATH PIC X(09).			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AE60-LOOP.
	     MOVE W10-CURR-CHAR(WS-S7)
				 TO W10-COM-CHAR(WS-S6).
	   IF WS-S7 < WS-S8
	       ADD 1		 TO WS-S6 WS-S7
	       GO TO AE60-LOOP.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Add the contents of W10-PATH to the end of the path that  Ё
      *    Ё has been keyed in by the USER to create the new path and  Ё
      *    Ё update the file (APACFILE) record. 		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE SPACES	 TO W10-CURR-PATH.
	   IF W10-NEW-PATH = SPACES
	       MOVE W10-PATH	 TO W10-CURR-PATH
	   ELSE
	       STRING W10-NEW-PATH DELIMITED BY " "
		      W10-PATH DELIMITED BY SIZE
				 INTO W10-CURR-PATH.
	     MOVE W10-CURR-PATH  TO APAC-PATH.
000030	     REWRITE APAC-RECORD1.
000040     IF WS-STAT1 NOT = "0"
041860	       MOVE W02-APACFILE TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE APAC-KEY	 TO WS-KEYX
	       GO TO AE900.
	     MOVE APAC-KEY	 TO AFID-KEY.

       AE65-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO AE65-READ-APACFIDS-EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё If there is no record in the APACFIDS file, for the recordЁ
      *    Ё being updated in APACFILE, then create and add the record Ё
      *    Ё to the APACFIDS file.				       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF WS-STATUS = "23"
	       MOVE APAC-KEY	 TO AFID-KEY
	       MOVE SPACES	 TO AFID-PATH
	       STRING APAC-PATH DELIMITED BY " "
		      APAC-NAME DELIMITED BY " "
		      INTO AFID-PATH
	       WRITE AFID-RECORD1
	       IF WS-STAT1 NOT = "0"
041860		   MOVE W02-APACFIDS
				 TO WS-FILE
041870		   MOVE ZERO	 TO WS-KEY
		   MOVE AFID-KEY TO WS-KEYX
		   GO TO AE900
	       ELSE
		   GO TO AE50.
006260	     STRING "I-O error " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       AE65-READ-APACFIDS-EXIT.
	     EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Set up the full path, including the file name in APACFIDS Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AE70.
	     MOVE SPACES	 TO AFID-PATH.
	     STRING APAC-PATH DELIMITED BY " "
		    APAC-NAME DELIMITED BY " "
		    INTO AFID-PATH.
	     REWRITE AFID-RECORD1.
000040	   IF WS-STAT1 NOT = "0"
041860	       MOVE W02-APACFIDS TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE AFID-KEY	 TO WS-KEYX
	       GO TO AE900.
	     GO TO AE50.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё As a fatal error has occured while trying to process a    Ё
      *    Ё global change. Display a message and terminate processing.Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *
      *        зддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *        Ё Reload the APACFILE (DAT & IDX) and the APACFIDS  Ё╟╟
      *        Ё (DAT & IDX) files for the local or network system,Ё╟╟
      *        Ё depending on which system was being updated.	   Ё╟╟
      *        юддддддддддддддддддддддддддддддддддддддддддддддддддды╟╟
      * 	 ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟
      *
       AE75.
	     MOVE 18		 TO SHADE-ROW.
	     MOVE 16		 TO SHADE-COL.
	     MOVE 51		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     DISPLAY
		 "зддддддддддддддддддддддддддддддддддддддддддддддддддд"
		  AT 1714 WITH FOREGROUND-COLOR 4 HIGHLIGHT
			       BACKGROUND-COLOR 4
		 "©" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     DISPLAY "Ё " AT 1814 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				       BACKGROUND-COLOR 4
		 "Reload the APACFILE (DAT & IDX) and the APACFIDS  "
		  WITH FOREGROUND-COLOR 7 HIGHLIGHT
		       BACKGROUND-COLOR 4
		 "Ё" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     DISPLAY "Ё " AT 1914 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				       BACKGROUND-COLOR 4
		 "(DAT & IDX) files for the local or network system,"
		  WITH FOREGROUND-COLOR 7 HIGHLIGHT
		       BACKGROUND-COLOR 4
		 "Ё" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     DISPLAY "Ё " AT 2014 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				       BACKGROUND-COLOR 4
		 "depending on which system was being updated.      "
		  WITH FOREGROUND-COLOR 7 HIGHLIGHT
		       BACKGROUND-COLOR 4
		 "Ё" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     DISPLAY "ю" AT 2114 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				      BACKGROUND-COLOR 4
		 "ддддддддддддддддддддддддддддддддддддддддддддддддддды"
		  WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     PERFORM SCREEN-SHADOW.
007380	     DISPLAY "Press ANY key" AT 2503
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     STOP RUN.
      *    *************************************************************
      *    ****   C H E C K   I F   T H E   O F F - L I N E   F I L E
      * 	  I D ' S   W E R E   U P D A T E D
      *    *************************************************************
       AE80.
	   IF WS-LOCAL-DRIVE = "L"
	       CLOSE APACFILE
		     APACFIDS
	       MOVE "SYSTEMS\"   TO W02-L-OR-N
				    W02-N-OR-L
	       OPEN I-O APACFILE
			APACFIDS
			SECUR
	       IF NOT(WS-ERRLOC = 1)
		   CLOSE NETWORK
			 SHARED.
	     GO TO AE999.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Display the I-O error description, status, the record key Ё
      *    Ё and a message for the operator.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AE900.
041350	   IF WS-STATUS = "10"
041360	       MOVE "End of FILE" TO WS-STAT-MESSAGE
041370	   ELSE
041380	   IF WS-STATUS = "22"
041390	       MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041400	   ELSE
041410	   IF WS-STATUS = "23"
041420	       MOVE "Invalid record number" TO WS-STAT-MESSAGE
041430	   ELSE
041440	   IF WS-STATUS = "24"
041450	       MOVE "DISK full" TO WS-STAT-MESSAGE
041460	   ELSE
041470	   IF WS-STATUS = "30"
041480	       MOVE "DISK error" TO WS-STAT-MESSAGE
041490	   ELSE
041500	   IF WS-STATUS = "94"
041510	       MOVE "FILE locked" TO WS-STAT-MESSAGE.
041520	     DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
041530	     DISPLAY "Status " AT 1212
041540		      WS-STATUS WITH FOREGROUND-COLOR 11
041550		     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
041560	   IF NOT (WS-STATUS = "94")
041570	       DISPLAY "Key    " AT 1412
041580			WS-KEY WITH FOREGROUND-COLOR 11
			" " WS-KEYX WITH FOREGROUND-COLOR 11
041590	       DISPLAY "Reverse backup or contact program Support"
041600			AT 1612.
041610	       DISPLAY "Please make a note of these details" AT 1812.
041620	     STOP RUN.

       AE999.
	     EXIT.
      *    *************************************************************
      *    **** 	      L O C A L   F I L E S		    ****
      *    *************************************************************
       AF000		       SECTION 52.
       AF00.
001060	     OPEN I-O APACFILE.
006200     IF RUNTIME-ERROR
               IF FLE-LOCKED
006210		   MOVE "Files locked - try later - Press ENTER"
				 TO WS-ERR-MES
		   PERFORM OPT-MESSAGE
006240		   GO TO AF20.
006250     IF WS-STATUS = "30"
006260	       MOVE "Invalid or file not on disk"
				 TO WS-ERR-MES
	       PERFORM OPT-MESSAGE
006270	       GO TO AF20.
      *   зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *   Ё Change APACFIDS file name to include offline path and the Ё
      *   Ё system identification for the set of files to be amended. Ё
      *   юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE W02-L-OR-N	 TO W02-N-OR-L.
	     MOVE W02-SYSPTH	 TO W02-SYSID.
      *    *************************************************************
      *    ****    C O N T R O L   F I L E
      *    *************************************************************
       AF05.
	     MOVE "NETWORK"	 TO APAC-KEY.

       AF10-READ-APACFILE.
000030	     READ APACFILE WITH IGNORE LOCK
	       KEY IS APAC-KEY.
	   IF WS-STATUS = "00"
	       GO TO AF10-READ-APACFILE-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     APAC-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
	     GO TO AF20.

       AF10-READ-APACFILE-EXIT.
	     EXIT.

       AF15.
	     MOVE ZERO		 TO WS-ERROR.
	     OPEN I-O APACFIDS.
006200     IF RUNTIME-ERROR
               IF FLE-LOCKED
006210		   MOVE "Files locked - try later - Press ENTER"
				 TO WS-ERR-MES
		   PERFORM OPT-MESSAGE
006240		   GO TO AF20.
006250	   IF WS-STATUS = "30"
006260	       MOVE "Invalid or file not on disk"
				 TO WS-ERR-MES
	       PERFORM OPT-MESSAGE
006270	       GO TO AF20.
	     GO TO AF999.

       AF20.
	     MOVE 9		 TO WS-ERROR.

       AF999.
	     EXIT.
