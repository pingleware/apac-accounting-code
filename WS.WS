000010 01  WS-FILE-STATUS.
000020     03  WS-STATUS       PIC  X(02).
000030     88  RECORD-LOCKED        VALUE "94" "9D".
000040 01  WS-RED-STAT REDEFINES WS-FILE-STATUS.
000050     03  WS-STAT1        PIC  X(01).
000060         88  RUNTIME-ERROR    VALUE "9".
	   03  WS-STAT1H REDEFINES WS-STAT1
			       PIC  9(02) COMP-X.
	   03  WS-STAT2        PIC  X(01).
000070	   03  WS-STAT2H REDEFINES WS-STAT2
			       PIC  9(02) COMP-X.
	       88  FLE-NOT-OPEN     VALUE 2.
	       88  FLE-IN-ONLY	    VALUE 6.
	       88  DSK-SPACE	    VALUE 7.
	       88  FLE-OUT-ONLY     VALUE 8.
	       88  FLE-IS-OPEN	    VALUE 12.
	       88  FLE-NOT-FOUND    VALUE 13.
000080	       88  FLE-LIMIT	    VALUE 14.
	       88  DSK-ERROR	    VALUE 24.
000090	       88  IDX-CORRUPT	    VALUE 41 43.
000100         88  FLE-LOCKED       VALUE 65.
000110	       88  REC-LOCKED	    VALUE 68.
	       88  NET-ERROR	    VALUE 208.
	       88  COM-ERROR	    VALUE 209.
000120         88  TOO-MANY-LOCKS   VALUE 213.

       01  WS-VAT-CHANGES.
	   03  WS-VAT-DATE     PIC  9(08) COMP-5 OCCURS 6.
	   03  WS-VAT-SUB      PIC  X(01) COMP-X.

000130 01  WS-FILE-ERRORS.
000140     03  WS-ACTION       PIC  9(01) VALUE ZERO.
000150     03  WS-F-ERROR      PIC  9(02) COMP-5. 
000160	   03  WS-WAIT	       PIC S9(08) COMP-5.
000160     03  WS-COUNT        PIC  9(02) COMP-5 VALUE 1.
000170     03  WS-LOCKED       PIC  9(01) VALUE 0.
000940	   03  WS-STIME	       PIC  9(08).
000940	   03  WS-TIME	       PIC  9(08).
000180	   03  WS-FILE.
	       05  WS-SHRT-FN  PIC  X(08).
	       05  FILLER      PIC  X(56).
	   03  WS-FILE-CHAR REDEFINES WS-FILE
			       PIC  X(01) OCCURS 64.
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

       01  WS-SCREEN-PRINT.
      *
      *    COMMAND: C = Close printer
      * 	    E = End of report
      *		    O = Open printer
      * 	    P = Print a line
      * 	    S = Skip to new page
      *
	   03  WS-COMMAND      PIC  X(01).
      *
      *    PRINTER: D = Disk
      * 	    S = Screen
      * 	    1 = Lpt1
      * 	    2 = Lpt2
      * 	    3 = Lpt3
      * 	    4 = Com1
      * 	    5 = Com2
      *
	   03  WS-PRINTER.
	       05  WS-PRINT-NO PIC  9(01).
      *
      *    ADVANCE:   0 = Print - No advance
      * 	    1-3 = Print advance lines specified
      * 	    4-? = Print advance 1 line, then use a loop
      * 		  to advance the remaining lines.
      * 	     99 = Print and advance to a new page.
      *
	   03  WS-ADVANCE      PIC  9(02).
      *
      *    REPORT: 1 = Picking Slips
      * 	   2 = Invoices/Credit Notes
      * 	   3 = General Reports
      * 	   4 = Statements
      * 	   5 = Labels
      * 	   6 = Quotations
      *
	   03  WS-REPORT       PIC  9(01).
	   03  WS-SCREEN       PIC  9(01).
	   03  WS-SCREEN-LIN   PIC  9(02).
	   03  WS-LINAGE       PIC  9(02).
	   03  WS-PAPER        PIC  X(01).
	   03  FILLER	       PIC  X(01).

      *
      *  Previous versions of COBOL compiler had LIN as a special
      *  register. As the sytem has now advanced to include additional
      *  options that require the MF directive, LIN is no longer
      *  available as a special register. LIN will now be included in
      *  this copy book, which is used by almost all the programs in the
      *  system. This copy book is only required for the 32 bit system.
      *
       01  WS-SPECIAL-REGISTER.
	   03  LIN	       PIC S9(04) COMP VALUE ZERO.

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
000220	   03  SPOS	       PIC  9(04) VALUE ZERO.
000230	   03  WS2-CURS REDEFINES SPOS.
	       05  SLIN	       PIC  9(02).
	       05  SCOL	       PIC  9(02).

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
	   03  WS-BACKGRND REDEFINES WS-BACKGROUND.
	       05  WS-BGRND78  PIC  X(78).
	       05  FILLER      PIC  X(02).
