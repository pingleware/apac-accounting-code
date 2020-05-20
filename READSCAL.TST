      ******************************************************************
      *                                                                *
      * ******	 ******    *****    *****    *****   **       *******  *
      * **   **  **   **  **   **  **	**  **	 **  **       **       *
      * **   **  **   **  **	   **	    **	 **  **       **       *
      * *****	 **   **   *****   **	    *******  **       *****    *
      * **   **  **   **       **  **	    **	 **  **       **       *
      * **   **  **   **  **   **  **	**  **	 **  **       **       *
      * **   **  ******    *****    *****   **	 **  *******  *******  *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   R E A D   I N P U T	F R O M   S C A L E   P R O G R A M    *
      *                                                                *
      *     VERSION 6.65 - April 1996				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     RDSCALE.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1996.
000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

000170	   SELECT SCALES  ASSIGN W02-SCALES
000300                    ORGANIZATION LINE SEQUENTIAL
000180			  STATUS WS-STATUS
000310			  ACCESS SEQUENTIAL.

	   SELECT SCALES-DISK ASSIGN DISK
000300			      ORGANIZATION LINE SEQUENTIAL
000180			      STATUS WS-STATUS
000310			      ACCESS SEQUENTIAL.

      /
000260 DATA DIVISION.
000270 FILE SECTION.

000530 FD  SCALES    LABEL RECORD OMITTED.
000550 01  SCA-REC.
	   03  SCA-LINE.
	       05  SCA-DATA-1-40
			       PIC  X(40).
	       05  SCA-DATA-41-256
			       PIC  X(216).
       01  SCA-REC2.
	   03  SCA-CHAR1       PIC  X(01).
	   03  SCA-CHAR2-30    PIC  X(29).
	   03  FILLER	       PIC  X(226).
       01  SCA-REC3.
	   03  SCA-KG	       PIC  9(02).
      *	   03  SCA-DEC	       PIC  X(01).
	   03  SCA-GRAM        PIC  9(03).
	   03  FILLER	       PIC  X(24).
	   03  FILLER	       PIC  X(01).
	   03  FILLER	       PIC  X(226).
       01  SCA-REC4.
	   03  SCA-CHAR-1      PIC  X(01).
	   03  SCA-CHAR-2      PIC  X(01).
	   03  SCA-CHAR-3      PIC  X(01).
	   03  SCA-CHAR-4      PIC  X(01).
	   03  SCA-CHAR-5      PIC  X(01).
	   03  FILLER	       PIC  X(251).

000530 FD  SCALES-DISK LABEL RECORD STANDARD
000020		       VALUE OF FILE-ID "SCALDISK".

000550 01  SCD-REC.
	   03  SCD-LINE.
	       05  SCD-DATA-1-40
			       PIC  X(40).
	       05  SCD-DATA-41-256
			       PIC  X(216).

      /
000430 WORKING-STORAGE SECTION.
000440 77  WS-S1            PIC 9(04)     COMP-5.
000440 77  WS-SUB	    PIC 9(06)	  COMP-5.
000560 77  WS-ERROR         PIC 9(01)     VALUE ZERO.
       77  WS-OPTION        PIC X(01).
       77  TODAY-DDMMYY	   PIC	9(06) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC  X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC  X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC  X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC  X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC  X(01) OCCURS 80.

000590*COPY WS.WS.
000010 01  WS-FILE-STATUS.
000020     03  WS-STATUS       PIC  X(02).
000030     88  RECORD-LOCKED        VALUE "94" "9D".
000040 01  WS-RED-STAT REDEFINES WS-FILE-STATUS.
000050     03  WS-STAT1        PIC  X(01).
000060         88  RUNTIME-ERROR    VALUE "9".
000070     03  WS-STAT2        PIC  9(02) COMP-X.
000080         88  FLE-LIMIT        VALUE 14.
000090	       88  IDX-CORRUPT	    VALUE 41 43.
000100         88  FLE-LOCKED       VALUE 65.
000110         88  REC-LOCKED       VALUE 68.
000120         88  TOO-MANY-LOCKS   VALUE 213.

       01  WS-VAT-CHANGES.
	   03  WS-VAT-DATE     PIC  9(06).
	   03  WS-VAT-SUB      PIC  X(01) COMP-X.

000130 01  WS-FILE-ERRORS.
000140     03  WS-ACTION       PIC  9(01) VALUE ZERO.
000150     03  WS-F-ERROR      PIC  9(02) COMP-5. 
000160	   03  WS-WAIT	       PIC S9(06) COMP-5.
000160     03  WS-COUNT        PIC  9(02) COMP-5 VALUE 1.
000170     03  WS-LOCKED       PIC  9(01) VALUE 0.
000180	   03  WS-FILE.
	       05  WS-SHRT-FN  PIC  X(06).
	       05  FILLER      PIC  X(08).
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

       01  PORT-CALLS.
	   03  PORT-ADDRESS	   PIC	9(05).
	   03  PORT-DATA	   PIC	X(01).
	   03  PORT-COM1	   PIC	9(04) COMP VALUE 1016.
	   03  PORT-COM2	   PIC	9(04) COMP VALUE 760.
	   03  PORT-COM3	   PIC	9(04) COMP VALUE 1000.
	   03  PORT-COM4	   PIC	9(04) COMP VALUE 744.
	   03  PORT-INPUT.
	       05  PORT-WEIGHT	   PIC	X(05).
	       05  PORT-CHAR	   PIC	X(01).
	   03  PORT-INPUT-RED REDEFINES PORT-INPUT.
	       05  FILLER	   PIC	X(01).
	       05  PORT-WEIGHT-2-6 PIC	X(05).
	   03  PORT-COUNT	   PIC	9(02) COMP.

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
      *  ÉÍÍÍÍËÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍ»
      *  ºPREFº 0 ³ 1 ³ 2 ³ 3 ³ 4 ³ 5 ³ 6 ³ 7 ³ 8 ³ 9 º
      *  ÌÍÍÍÍÎÍÍÍØÍÍÍØÍÍÍØÍÍÍØÍÍÍØÍÍÍØÍÍÍØÍÍÍØÍÍÍØÍÍÍ¹
      *  º 17 º   ³   ³   ³   ³   ³   ³ ° ³ ± ³ ² ³ ³ º
      *  ÇÄÄÄÄ×ÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄ¶
      *  º 18 º ´ ³ µ ³ ¶ ³ · ³ ¸ ³ ¹ ³ º ³ » ³ ¼ ³ ½ º
      *  ÇÄÄÄÄ×ÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄ¶
      *  º 19 º ¾ ³ ¿ ³ À ³ Á ³ Â ³ Ã ³ Ä ³ Å ³ Æ ³ Ç º
      *  ÇÄÄÄÄ×ÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄ¶
      *  º 20 º È ³ É ³ Ê ³ Ë ³ Ì ³ Í ³ Î ³ Ï ³ Ð ³ Ñ º
      *  ÇÄÄÄÄ×ÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄ¶
      *  º 21 º Ò ³ Ó ³ Ô ³ Õ ³ Ö ³ × ³ Ø ³ Ù ³ Ú ³ Û º
      *  ÈÍÍÍÍÊÍÍÍÏÍÍÍÏÍÍÍÏÍÍÍÏÍÍÍÏÍÍÍÏÍÍÍÏÍÍÍÏÍÍÍÏÍÍÍ¼
000110	   03  WS-G1	       PIC  X(01) VALUE "Í".
000120	   03  WS-G2	       PIC  X(01) VALUE "Ñ".
000130	   03  WS-G3	       PIC  X(01) VALUE "³".
000140	   03  WS-G4	       PIC  X(01) VALUE "Ï".
000150	   03  WS-G5	       PIC  X(01) VALUE "Ë".
000160	   03  WS-G6	       PIC  X(01) VALUE "º".
000170	   03  WS-G7	       PIC  X(01) VALUE "Ê".
000180	   03  WS-G8	       PIC  X(01) VALUE "Ä".
000190	   03  WS-G9	       PIC  X(01) VALUE "Ú".
000200	   03  WS-G10	       PIC  X(01) VALUE "¿".
000210	   03  WS-G11	       PIC  X(01) VALUE "À".
000220	   03  WS-G12	       PIC  X(01) VALUE "Ù".
000230	   03  WS-G13	       PIC  X(01) VALUE "µ".
000240	   03  WS-G14	       PIC  X(01) VALUE "Æ".
000250	   03  WS-G15	       PIC  X(01) VALUE "É".
000260	   03  WS-G16	       PIC  X(01) VALUE "»".
000270	   03  WS-G17	       PIC  X(01) VALUE "È".
000280	   03  WS-G18	       PIC  X(01) VALUE "¼".
	   03  WS-G19	       PIC  X(01) VALUE "°".
	   03  WS-G20	       PIC  X(01) VALUE "±".
	   03  WS-G21	       PIC  X(01) VALUE "²".
	   03  WS-G22	       PIC  X(01) VALUE "Û".
	   03  WS-BACKGROUND   PIC  X(80) VALUE ALL "°".

      *COPY FUNCTION.WS.
      *
      *    T E R M I N A T O  R   K E Y S
      *
       78 NORM-TERM	     VALUE "0".
       78 USER-FN-KEY	     VALUE "1".
       78 ADIS-FN-KEY	     VALUE "2".
       78 8BIT-DATA	     VALUE "3".
       78 16BIT-DATA	     VALUE "4".
       78 ERROR-TERM	     VALUE "9".
      *
      *    A D I S   F U N C T I O N   K E Y S
      *
       78  ENTER-KEY	    VALUE 0.
       78  CR-KEY	    VALUE 2.
       78  LEFT-KEY	    VALUE 3.
       78  RIGHT-KEY	    VALUE 4.
       78  UP-KEY	    VALUE 5.
       78  DOWN-KEY	    VALUE 6.
       78  HOME-KEY	    VALUE 7.
       78  TAB-KEY	    VALUE 8.
       78  BACKTAB-KEY	    VALUE 9.
       78  END-KEY	    VALUE 10.
       78  BACK-SPACE	    VALUE 14.
       78  DEL-KEY	    VALUE 17.
       78  INS-KEY	    VALUE 23.
       78  MOUSE-KEY	    VALUE 27.
      *
      *    U S E R   F U N C T I O N   K E Y S
      *
       78  ESC-KEY	    VALUE 0.
       78  F1-KEY	    VALUE 1.
       78  F2-KEY	    VALUE 2.
       78  F3-KEY	    VALUE 3.
       78  F4-KEY	    VALUE 4.
       78  F5-KEY	    VALUE 5.
       78  F6-KEY	    VALUE 6.
       78  F7-KEY	    VALUE 7.
       78  F8-KEY	    VALUE 8.
       78  F9-KEY	    VALUE 9.
       78  F10-KEY	    VALUE 10.
       78  SF1-KEY	    VALUE 11.
       78  SF2-KEY	    VALUE 12.
       78  SF3-KEY	    VALUE 13.
       78  SF4-KEY	    VALUE 14.
       78  SF5-KEY	    VALUE 15.
       78  SF6-KEY	    VALUE 16.
       78  SF7-KEY	    VALUE 17.
       78  SF8-KEY	    VALUE 18.
       78  SF9-KEY	    VALUE 19.
       78  SF10-KEY	    VALUE 20.
       78  CF1-KEY	    VALUE 21.
       78  CF2-KEY	    VALUE 22.
       78  CF3-KEY	    VALUE 23.
       78  CF4-KEY	    VALUE 24.
       78  CF5-KEY	    VALUE 25.
       78  CF6-KEY	    VALUE 26.
       78  CF7-KEY	    VALUE 27.
       78  CF8-KEY	    VALUE 28.
       78  CF9-KEY	    VALUE 29.
       78  CF10-KEY	    VALUE 30.
       78  AF1-KEY	    VALUE 31.
       78  AF2-KEY	    VALUE 32.
       78  AF3-KEY	    VALUE 33.
       78  AF4-KEY	    VALUE 34.
       78  AF5-KEY	    VALUE 35.
       78  AF6-KEY	    VALUE 36.
       78  AF7-KEY	    VALUE 37.
       78  AF8-KEY	    VALUE 38.
       78  AF9-KEY	    VALUE 39.
       78  AF10-KEY	    VALUE 40.
       78  ALT-1	    VALUE 41.
       78  ALT-2	    VALUE 42.
       78  ALT-3	    VALUE 43.
       78  ALT-4	    VALUE 44.
       78  ALT-5	    VALUE 45.
       78  ALT-6	    VALUE 46.
       78  ALT-7	    VALUE 47.
       78  ALT-8	    VALUE 48.
       78  ALT-9	    VALUE 49.
       78  ALT-0	    VALUE 50.
       78  ALT-HYPH	    VALUE 51.
       78  ALT-EQUAL	    VALUE 52.
       78  PAGE-UP	    VALUE 53.
       78  PAGE-DOWN	    VALUE 54.
       78  ALT-A	    VALUE 65.
       78  ALT-B	    VALUE 66.
       78  ALT-C	    VALUE 67.
       78  ALT-D	    VALUE 68.
       78  ALT-E	    VALUE 69.
       78  ALT-F	    VALUE 70.
       78  ALT-G	    VALUE 71.
       78  ALT-H	    VALUE 72.
       78  ALT-I	    VALUE 73.
       78  ALT-J	    VALUE 74.
       78  ALT-K	    VALUE 75.
       78  ALT-L	    VALUE 76.
       78  ALT-M	    VALUE 77.
       78  ALT-N	    VALUE 78.
       78  ALT-O	    VALUE 79.
       78  ALT-P	    VALUE 80.
       78  ALT-Q	    VALUE 81.
       78  ALT-R	    VALUE 82.
       78  ALT-S	    VALUE 83.
       78  ALT-T	    VALUE 84.
       78  ALT-U	    VALUE 85.
       78  ALT-V	    VALUE 86.
       78  ALT-W	    VALUE 87.
       78  ALT-X	    VALUE 88.
       78  ALT-Y	    VALUE 89.
       78  ALT-Z	    VALUE 90.
       78  F11-KEY	    VALUE 91.
       78  F12-KEY	    VALUE 92.
       78  SF11-KEY	    VALUE 93.
       78  SF12-KEY	    VALUE 94.
       78  CF11-KEY	    VALUE 95.
       78  CF12-KEY	    VALUE 96.
       78  AF11-KEY	    VALUE 97.
       78  AF12-KEY	    VALUE 98.

       01  SET-BIT-PAIRS       PIC  9(02)    COMP-X VALUE 1.

       01  GET-SINGLE-CHAR     PIC  9(02)    COMP-X VALUE 26.

      *
      *    ACTION :  1 = CONTROL USER FUNCTION KEYS
      *
      * 		 SETTING 0 = DISABLE
      * 			 1 = ENABLE
      * 		 NUMBER    = NUMBER OF FIRST USER KEY
      * 		 KEYS	   = NUMBER OF CONSECUTIVE KEYS
      *
      * 		 SETTING 1 = STANDARD USER FUNCTION KEY LIST
      * 			 2 = COMPATIBILITY KEY LIST
      * 		 NUMBER    = 87
      * 		 KEYS	   = 1
      *
      * 	     2 = CONTROL ADIS KEY MAPPINGS
      *
      * 		 SETTING 0 = DISABLE KEYS
      * 			 1 = ENABLE
      * 			 2 = NORMAL ACTION
      * 			 3 = NORMAL ACTION UNLESS CURSOR
      * 			     LEAVES CURRENT FIELD
      * 		 NUMBER    = NUMBER OF FIRST ADIS KEY
      * 		 KEYS	   = NUMBER OF CONSECUTIVE KEYS
      *
      * 		 SETTING 0 = PRE-DISPLAY FIXED-FORMAT
      * 			     NUMERIC/NUM EDITED
      * 			 1 = NUMERIC AND FIXED-FORMAT
      * 			     NUM EDITED FIELDS PRE-DISPLAYED
      * 			 2 = PRE-DISPLAY FIELD WHEN CURSOR
      * 			     MOVED TO IT
      * 			 3 = ALL FIELDS IN ACCEPT ARE
      * 			     PRE-DISPLAYED.
      * 		 NUMBER    = 76
      * 		 KEYS	   = 1
      *
      * 		 SETTING 0 = INDICATOR DISPLAYED IF NECCESSARY
      * 			 3 = INDICATOR IS NEVER DISPLAYED
      * 		 NUMBER    = 56 INSERT/REPLACE IND
      * 			   = 57 END OF FIELD IND
      * 			   = 58 AUTOCLEAR IND
      * 		 KEYS	   = 1
      *
      * 		 SETTING 0 = ERROR MESSAGES NEVER DISPLAYED
      * 			     BELL IS RUNG
      * 			 1 = MESSAGES NEVER DISPLAYED
      * 			     BELL IS RUNG FOR INVALID NUMERIC
      * 			 2 = MESSAGES DISPLAYED
      * 			     INVALID NUMERIC NOT REPORTED
      * 			 3 = MESSAGES DISPLAYED
      * 			     BELL RUNG FOR INVALID NUMERIC
      * 		 NUMBER    = 44
      * 		 KEYS	   = 1
      *
       01  USER-KEY-CONTROL.
	   03  USER-SETTING    PIC  9(02)    COMP-X.
	   03  USER-ACTION     PIC  X(01)    VALUE "2".
	   03  USER-NUMBER     PIC  9(02)    COMP-X.
	   03  USER-KEYS       PIC  9(02)    COMP-X VALUE 1.

       01  W02-FILE-IDS.

	   03  W02-SCALES      PIC X(04) VALUE "COM1".

       01  W10-VALUES.
	   03  W10-WEIGHT.
	       05  W10-QNT     PIC 9(03)V999.
	   03  W10-WEIGHT2 REDEFINES W10-WEIGHT.
	       05  W10-KG      PIC  9(03).
	       05  W10-GR      PIC  9(03).
	   03  W10-DISP        PIC  ZZ9.999.

000320 01  W12-DATE.
000330     03  W12-TODAY       PIC  9(06).
000340     03  W12-DATE-DMY REDEFINES W12-TODAY.
000350         05  W12-DAY     PIC  9(02).
000360         05  W12-MONTH   PIC  9(02).  
000370         05  W12-YEAR    PIC  9(02).
000380     03  W12-T-YMD       PIC  9(06). 
000390     03  W12-DATE-YMD REDEFINES W12-T-YMD.
000400         05  W12-YY      PIC  9(02).
000410         05  W12-MM      PIC  9(02).
000420         05  W12-DD      PIC  9(02).

      /
000850 SCREEN SECTION.

      *COPY BLANK.CRT.
      *
      *    ****  THIS SCREEN - CLEARS THE DISPLAY AND SETS THE DEFAULT
      *          COLOUR TO 3 (LIGHT BLUE).
      *
001950 01  CLR-SCREEN.
003170     03  BLANK SCREEN FOREGROUND-COLOR 3
                            BACKGROUND-COLOR 1.
	   03  CLR-L1-2.
003860	   05  LINE  1 COLUMN  1 BACKGROUND-COLOR 0
				 PIC X(80) USING WS-TOP-LNE2.
003870	   05  LINE  2 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
004160	   05	       COLUMN  2 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
004170                           VALUE "APAC Accounting".
	   05	       COLUMN 17 PIC  X(50) FROM WS-BACKGROUND.
004180	   05	       COLUMN 66 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
004190				 VALUE "Version - 6.7 ".
003920	   05	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003930	   03  LINE  3 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(64) FROM WS-BACKGROUND.
	   03	       COLUMN 66 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Date: ".
	   03	       COLUMN 72 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 PIC Z9/99/99 USING TODAY-DDMMYY.
003940	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003950	   03  LINE  4 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
003960	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003970	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
003980	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003990	   03  LINE  6 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004000	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004010	   03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004020	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004030	   03  LINE  8 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004040	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004050	   03  LINE  9 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004060	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004070	   03  LINE 10 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004080	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004090	   03  LINE 11 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004100	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004110	   03  LINE 12 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004120	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004130	   03  LINE 13 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004140	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004150	   03  LINE 14 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004160	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004170	   03  LINE 15 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004180	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004190	   03  LINE 16 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004200	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004210	   03  LINE 17 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004220	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004230	   03  LINE 18 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004240	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004250	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004260	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004270	   03  LINE 20 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004280	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004290	   03  LINE 21 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004300	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004310	   03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004320	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004330	   03  LINE 23 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004340	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
006550	   03  LINE 24 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      - 	     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
	   03  CLEAR-L25.
	   05  LINE 25 BLANK LINE BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 1.

000860 01  S00.
000910	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT
000920		       VALUE "READ SCALE - TESTING PROGRAM".
000950	   03  LINE  6 COLUMN 12 VALUE "Data :".

      /
001230 PROCEDURE DIVISION.
001220 AA000	       SECTION.
001230 AA00.
041220       MOVE 1 TO WS-S1.
021870	     MOVE SPACES TO WS-MID-LNE.
041240 AA02.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO AA02.
             MOVE WS-G9  TO WS-TCH  (1).
             MOVE WS-G10 TO WS-TCH  (80).
             MOVE WS-G11 TO WS-BCH  (1).
             MOVE WS-G12 TO WS-BCH  (80).
             MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2  TO WS-TCHR (16) WS-TCHR (41)
021940                      WS-TCHR (64) WS-TCHR (71).
021950       MOVE WS-G3  TO WS-MCHR (16) WS-MCHR (41)
021960                      WS-MCHR (64) WS-MCHR (71)
			    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4  TO WS-BCHR (16) WS-BCHR (41)
021980                      WS-BCHR (64) WS-BCHR (71).
	     ACCEPT W12-T-YMD FROM DATE.
	     MOVE W12-YY	 TO W12-YEAR.
	     MOVE W12-MM	 TO W12-MONTH.
	     MOVE W12-DD	 TO W12-DAY.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
001250	     DISPLAY CLR-SCREEN.
001270       PERFORM ZA000.
001260       DISPLAY S00.

001340 AA05.
	     MOVE X"05" 	 TO SCA-LINE PORT-DATA.
	     DISPLAY "Writing ENQ to COM port" AT 0619
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     MOVE SCA-REC	 TO SCD-REC.
	     MOVE W02-SCALES	 TO SCD-DATA-41-256.
	     WRITE SCD-REC.
	     CLOSE SCALES-DISK.
	     OPEN EXTEND SCALES-DISK.
	     DISPLAY PORT-ADDRESS AT 0660.
	     CALL X"88" USING PORT-ADDRESS, PORT-DATA.
	     MOVE "      "	 TO PORT-INPUT.
	     MOVE ZERO		 TO WS-SUB PORT-COUNT.

       LOOP.
	   IF WS-SUB < 3300
	       ADD 1		 TO WS-SUB
	       GO TO LOOP.

       LOOP1.
	     CALL X"87" USING PORT-ADDRESS, PORT-CHAR.
	   IF PORT-CHAR = X"05" OR " " OR X"FC"
	       GO TO LOOP1.
	   IF (PORT-CHAR = X"1E") OR (PORT-COUNT > 25)
      *	       IF PORT-WEIGHT NUMERIC
		   MOVE PORT-WEIGHT  TO SCA-DATA-1-40
		   GO TO GOT-WEIGHT.
      *	      ELSE
      *		   GO TO AA05.
	     MOVE PORT-WEIGHT-2-6
				 TO PORT-WEIGHT.
	     MOVE " "		 TO PORT-CHAR.
	     MOVE ZERO		 TO WS-SUB.
	     ADD 1		 TO PORT-COUNT.
	     GO TO LOOP.

       GOT-WEIGHT.
	     DISPLAY SCA-DATA-1-40 AT 0819
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     DISPLAY "Unpacking data" AT 1019
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     MOVE SCA-REC	 TO SCD-REC.
	     WRITE SCD-REC.
	     CLOSE SCALES-DISK.
	     OPEN EXTEND SCALES-DISK.
	     PERFORM AB000.
	     MOVE W10-QNT	 TO W10-DISP.
	     DISPLAY W10-DISP AT 1219
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     DISPLAY "Press E (Caps lock on) to exit" AT 1419
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     ACCEPT WS-OPTION WITH AUTO.
	   IF NOT (WS-OPTION = "E")
	       GO TO AA05.

001450 AA10.
001830	     CLOSE SCALES SCALES-DISK.
001950	     STOP RUN.

       AB000	       SECTION.
       AB00.
	     MOVE ZERO		 TO W10-QNT.

       AB05.
      *	   IF NOT (SCA-CHAR1 = "+" OR "-")
      *	       MOVE SCA-CHAR2-79 TO SCA-REC
      *	       IF SCA-REC = SPACES
      *		   GO TO AB999
      *	       ELSE
      *		   GO TO AB05.
      *	     MOVE SCA-CHAR2-79	 TO SCA-REC.
	   IF SCA-REC = SPACES
	       GO TO AB999.
	     MOVE SCA-KG	 TO W10-KG.
	     MOVE SCA-GRAM	 TO W10-GR.

       AB999.
	     EXIT.

      /
002460 ZA000           SECTION.
002470 ZA00.
	     DISPLAY "COM PORT TO USE" AT 0312.
	     ACCEPT WS-OPTION AT 0328
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF WS-OPTION = "1"
	       MOVE "COM1"	 TO W02-SCALES
	       MOVE PORT-COM1	 TO PORT-ADDRESS
      *	       MOVE PORT-REG1	 TO PORT-REGISTER
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE "COM2"	 TO W02-SCALES
	       MOVE PORT-COM2	 TO PORT-ADDRESS
      *	       MOVE PORT-REG2	 TO PORT-REGISTER
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE "COM3"	 TO W02-SCALES
	       MOVE PORT-COM3	 TO PORT-ADDRESS
      *	       MOVE PORT-REG3	 TO PORT-REGISTER
	   ELSE
	       MOVE PORT-COM4	 TO PORT-ADDRESS
      *	       MOVE PORT-REG4	 TO PORT-REGISTER
	       MOVE "COM4"	 TO W02-SCALES.
042140	     OPEN I-O SCALES.
	     CALL X"91" USING X91-RES X91-FUN SCALES.
003050	   IF NOT (WS-STATUS = "00" OR "41")
003060	       MOVE 2		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
	     OPEN OUTPUT SCALES-DISK.
	     CALL X"91" USING X91-RES X91-FUN SCALES-DISK.

003080 ZA999-EXIT.
003090       EXIT.
      /
003100 ZB000-ERROR           SECTION.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500	     DISPLAY "Open error" AT 0812
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
041510       PERFORM DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500	     DISPLAY "Read error" AT 0812
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
041510       PERFORM DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540	     DISPLAY "Write error" AT 0812
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041620	     MOVE W02-SCALES	 TO WS-FILE.
	     MOVE ZERO		 TO WS-KEY.
041880	   IF WS-STATUS = "10"
041890         MOVE "End of FILE"
                                 TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920         MOVE "Duplicate record number" 
                                 TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950         MOVE "Invalid record number" 
                                 TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980         MOVE "DISK full"  TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040         MOVE "FILE locked" TO WS-STAT-MESSAGE.
045380	     DISPLAY "File - " AT 1012 WS-FILE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
045390       DISPLAY "Status " AT 1212 
		      WS-STATUS WITH FOREGROUND-COLOR 3 HIGHLIGHT
		     ": " WS-STAT-MESSAGE
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
			WS-KEY WITH FOREGROUND-COLOR 3 HIGHLIGHT
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
