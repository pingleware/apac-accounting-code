      ******************************************************************
      *                                                                *
      *   *******   ********  *******    ******    *****   **    **    *
      *   **    **  **        **    **  **    **  **   **  ***   **    *
      *   **    **  **        **    **  **        **   **  ****  **    *
      *   **    **  *****     *******   **        **   **  ** ** **    *
      *   **    **  **        **    **  **        **   **  **  ****    *
      *   **    **  **        **    **  **    **  **   **  **   ***    *
      *   *******   ********  *******    ******    *****   **    **    *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   D E B T O R   F I L E   C O N V E R T   P R O G R A M        *
      *                                                                *
      *     VERSION 8.06 - August 2002				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DEBCON.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2002
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000110	   SELECT NETWORK ASSIGN DISK
                          LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000130                    ACCESS RANDOM
000140                    ORGANIZATION RELATIVE
000150                    RELATIVE WS-NETKEY.
000110     SELECT PARAM   ASSIGN DISK
                          LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000130                    ACCESS RANDOM
000140                    ORGANIZATION RELATIVE
000150                    RELATIVE WS-PARKEY.
000170     SELECT DEBTOR  ASSIGN DISK
                          LOCK EXCLUSIVE
000180                    STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210                    RECORD DEB-ACNO
                          ALTERNATE RECORD DEB-NKEY DUPLICATES.
000180     SELECT DEBOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS RANDOM
000210                    ORGANIZATION RELATIVE
                          RELATIVE WS-DEBKEY.
000010     SELECT DBTRAN  ASSIGN DISK
000020                    LOCK EXCLUSIVE
000030                    STATUS WS-STATUS
000040                    ACCESS DYNAMIC
000050                    ORGANIZATION INDEXED
000060                    RECORD TRA-KEY
                          ALTERNATE RECORD TRA-TRSEQ =
                            TRA-CODE TRA-DATE DUPLICATES.
000010     SELECT DBTOLD  ASSIGN DISK
000020                    LOCK EXCLUSIVE
000030                    STATUS WS-STATUS
000040                    ACCESS RANDOM
000050                    ORGANIZATION RELATIVE
000060                    RELATIVE WS-DBTKEY.
000230     SELECT DEBMEM  ASSIGN DISK
                          LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270                    RECORD DME-KEY.
           SELECT CHEQUE  ASSIGN DISK
000270                    LOCK EXCLUSIVE
000290                    STATUS WS-STATUS
000300                    ACCESS DYNAMIC
000310                    ORGANIZATION INDEXED
                          RECORD CHQ-TR-NUMBER
000420                    ALTERNATE RECORD CHQ-DKEY =
                            CHQ-YMD CHQ-TR-NUMBER.

       COPY GARTEE.SL.

       COPY INVOIC.SL.

       COPY SORDER.SL.

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000290 COPY CONTROL.FD.

000290 COPY PARAM.IFD.

000140 COPY DEBTOR.GFD.

      /
000010 FD  DEBOLD    LABEL RECORD STANDARD
000020               VALUE OF FILE-ID W02-DEBOLD.
000030 01  DBO-RECORD1.
000040     03  DBO-ACNO        PIC  9(05).
000050     03  DBO-TYPE        PIC  9(01).
000060     03  DBO-CATEGORY.
000070         05  DBO-CAT     PIC  9(02).
000080     03  DBO-NAME.
000090         05  DBO-NKEY    PIC  X(12).
000100         05  FILLER      PIC  X(28).
000110     03  DBO-ADDRESS     PIC  X(60).
000120     03  DBO-PADDR       PIC  X(60).
000130     03  DBO-POST        PIC  9(04)    COMP-5.
000140     03  DBO-PPOST       PIC  9(04)    COMP-5.
000150     03  DBO-TITLE       PIC  9(04)    COMP-5.
000160     03  DBO-H-TEL       PIC  X(14).
000170     03  DBO-B-TEL       PIC  X(14).
000180     03  DBO-ID          PIC  X(14).
000190     03  DBO-TAX REDEFINES DBO-ID
000200                         PIC  X(14).
000210     03  DBO-ACTIVE      PIC  9(06)    COMP-3.
000220     03  DBO-STM         PIC S9(09)V99 COMP-3.
000230     03  DBO-BAL         PIC S9(09)V99 COMP-3.
000240     03  DBO-CUR         PIC S9(09)V99 COMP-3.
000250     03  DBO-30          PIC S9(09)V99 COMP-3.
000260     03  DBO-60          PIC S9(09)V99 COMP-3.
000270     03  DBO-90          PIC S9(09)V99 COMP-3.
000280     03  DBO-120         PIC S9(09)V99 COMP-3.
000290     03  DBO-DEBT        PIC S9(09)V99 COMP-3.
000300     03  DBO-CRED        PIC S9(09)V99 COMP-3.
000310     03  DBO-PURYTD      PIC S9(09)V99 COMP-3.
000320     03  DBO-PURPRV      PIC S9(09)V99 COMP-3.
000330     03  DBO-INT         PIC S9(05)V99 COMP-3.
000340     03  DBO-VAT         PIC S9(05)V99 COMP-3.
000350     03  DBO-CRL         PIC  9(07)    COMP-3.
000360     03  DBO-FINANCE.
000370         05  DBO-FIN     PIC  9(01).
000380     03  DBO-TERMS       PIC  X(01).
000390     03  DBO-BDT         PIC  9(01).
000400     03  DBO-LANG        PIC  X(01).
000410     03  DBO-PMT         PIC  9(06)    COMP-3.
000420     03  DBO-INV-TYPE    PIC  X(01).
000430     03  FILLER          PIC  X(08).
000440     03  DBO-SDISC       PIC  9(02)    COMP-5.
000450     03  DBO-DISC        PIC  9(02)    COMP-5.
000460     03  DBO-SMAN        PIC  9(02)    COMP-5.
000470     03  DBO-IFIRST      PIC  9(06)    COMP-5.
000480     03  DBO-ILAST       PIC  9(06)    COMP-5.
000490     03  DBO-FIRST       PIC  9(06)    COMP-5.
000500     03  DBO-LAST        PIC  9(06)    COMP-5.
000510 01  DBO-HEADER.
000520     03  DBO-DBDTE       PIC  9(06).
000530     03  FILLER          PIC  X(324).

       COPY DBTRAN.GFD.

      /
000010 FD  DBTOLD    LABEL RECORD STANDARD
000020               VALUE OF FILE-ID W02-DBTOLD.
000030 01  TRO-RECORD1.
000040     03  TRO-CODE        PIC  9(02).
      *    
      *    TYPE: "N" = No discount
      *          "D" = Discount if paid within 30 days.
      *
000050     03  TRO-TYPE        PIC  X(01).
000060     03  TRO-DATE        PIC  9(06)    COMP-3.
000070     03  TRO-REF         PIC  X(06).
000080     03  TRO-REG         PIC  X(09).
000090     03  TRO-ORD         PIC  X(12).
000100     03  TRO-VALUE       PIC S9(09)V99 COMP-3.
000110     03  TRO-TAXFREE     PIC S9(09)V99 COMP-3.
000120     03  TRO-TAX         PIC S9(07)V99 COMP-3.
000130     03  TRO-DISC        PIC S9(03)V99 COMP-3.
000140     03  FILLER          PIC  X(01).
000150     03  TRO-PREV        PIC  9(06)    COMP-5.
000160     03  TRO-NEXT        PIC  9(06)    COMP-5.
000170     03  TRO-AC          PIC  9(05)    COMP-3.

       COPY DEBMEM.FD.

       COPY CHEQUE.FD.

       COPY GARTEE.FD.

       COPY INVOIC.FD.

       COPY SORDER.FD.

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000440 77  WS-S1	   PIC 9(04)	 COMP-5.
000440 77  WS-SUB          PIC 9(04)     COMP-5.
003290 77  WS-NETKEY       PIC 9(04)     COMP-5 VALUE 1.
003290 77  WS-DEBKEY       PIC 9(04)     COMP-5 VALUE 1.
003290 77  WS-DBTKEY       PIC 9(04)     COMP-5 VALUE 1.
003290 77  WS-DEPKEY       PIC 9(04)     COMP-5 VALUE 1.
003300 77  WS-CTRKEY       PIC 9(04)     COMP-5 VALUE 1.
003340 77  WS-PARKEY       PIC 9(04)     COMP-5 VALUE 1.
003360 77  WS-RECKEY       PIC 9(04)     COMP-5.
001830 77  WS-LENGTH       PIC  9(04) VALUE 1.
000520 77  WS-RECOVER      PIC 9(05)     COMP-0.
000530 77  WS-TRANS        PIC 9(05)     COMP-0 VALUE 1.
000540 77  WS-EKEY         PIC Z(04)9.
000560 77  WS-ERROR        PIC 9(01)     VALUE ZERO.
000570 77  WS-PROG         PIC X(02).
       77  WS-OPTION       PIC X(01).
       77  WS-JUST         PIC X(01) VALUE "N".
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
               05  FILLER      PIC  X(39).
002450     03  WS-MID-LNE.
002460         05  WS-MCHR PIC  X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
002460         05  WS-MCH  PIC  X(01) OCCURS 80.
002470     03  WS-BOT-LNE.
002480         05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC  X(01) OCCURS 80.

000010 01  WS-FILE-STATUS.
000020     03  WS-STATUS       PIC  X(02).
000030     88  RECORD-LOCKED        VALUE "94" "9D".
000040 01  WS-RED-STAT REDEFINES WS-FILE-STATUS.
000050     03  WS-STAT1        PIC  X(01).
000060         88  RUNTIME-ERROR    VALUE "9".
000070     03  WS-STAT2        PIC  9(02) COMP-X.
000080         88  FLE-LIMIT        VALUE 14.
000090         88  IDX-CORRUPT      VALUE 43.
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
000200	       05  WS-STAT-MESSAGE
			       PIC  X(23).
	       05  WS-KEYX     PIC  X(24) VALUE SPACES.
	   03  WS-ERR-STRING   PIC  X(32).

000930 01  WS-CONSOLE	       PIC 9(02) COMP-X.

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
	   03  MOUSE-BUTTONS   PIC  X(01) COMP-X.
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

000590 COPY CHAIN.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

004000 01  W02-FID.

       COPY APACFIDS.ID.

000290 COPY CONTROL.ID.

000140 COPY DEBTOR.ID.

000160 COPY DEBOLD.ID.

       COPY DBTRAN.ID.

       COPY CHEQUE.ID.

000010     03  W02-DBTOLD.
000020         05  W02-DSK5O   PIC  X(01).
000030         05  FILLER      PIC  X(06) VALUE ":DTRN_".
000040         05  W02-EXT5O   PIC  X(03).
000050         05  FILLER      PIC  X(04) VALUE ".OLD".

       COPY DEBMEM.ID.

       COPY GARTEE.ID.

       COPY INVOIC.ID.

       COPY PARAM.ID.

       COPY SORDER.ID.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

       01  W15-EDIT.
           03  W15-ACNO.
               05  W15-NO      PIC  Z(04)9.
           03  W15-AC REDEFINES W15-ACNO.
               05  W15-1ST     PIC  X(01).
               05  W15-2TO5    PIC  X(04).

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

000050 01  NTN-DEBTOR.
000060     03  DBN-ACCOUNTS    PIC  9(06)    COMP-5.
000070     03  DBN-INVOICE     PIC  9(08)    COMP-5.
000080     03  DBN-BATCH       PIC  9(06)    COMP-5.
000090     03  DBN-DEBTOR      PIC  9(06)    COMP-5.
000110     03  DBN-DINDEX      PIC  9(06)    COMP-5.
000120     03  DBN-DBTRAN      PIC  9(06)    COMP-5.
000130     03  DBN-INVOIC      PIC  9(06)    COMP-5.
000140     03  DBN-AUDIT       PIC  9(06)    COMP-5.
000150     03  DBN-SALORD      PIC  9(06)    COMP-5.
000160     03  DBN-SALIDX      PIC  9(06)    COMP-5.
000170     03  DBN-OPEN        PIC S9(09)V99 COMP-3.
000180     03  DBN-OUT         PIC S9(09)V99 COMP-3.
000190     03  DBN-MTD         PIC S9(09)V99 COMP-3.
000200     03  DBN-ONE         PIC S9(09)V99 COMP-3.
000210     03  DBN-TWO         PIC S9(09)V99 COMP-3.
000220     03  DBN-THREE       PIC S9(09)V99 COMP-3.
000230     03  DBN-FOUR        PIC S9(09)V99 COMP-3.
000240     03  DBN-DT          PIC S9(09)V99 COMP-3.
000250     03  DBN-CR          PIC S9(09)V99 COMP-3.
000260     03  DBN-INTEREST    PIC S9(09)V99 COMP-3.
000270     03  DBN-VAT-DUE     PIC S9(09)V99 COMP-3.
000280     03  DBN-BD          PIC S9(09)V99 COMP-3.
000290     03  DBN-BDRM        PIC S9(09)V99 COMP-3.
000300     03  DBN-BDRY        PIC S9(09)V99 COMP-3.
000310     03  DBN-ORDER       PIC  9(08)    COMP-5.
000100     03  DBN-JOBNO       PIC  9(08)    COMP-5.
000320     03  DBN-CHECK       PIC  9(06)    COMP-5.
      /
000850 SCREEN SECTION.
      *
      *    ****  THIS SCREEN - CLEARS THE DISPLAY AND SETS THE DEFAULT
      *          COLOUR TO 3 (LIGHT BLUE).
      *
003160 01  CLR-SCREEN.
003170     03  BLANK SCREEN FOREGROUND-COLOR 3.
003860     03  LINE  1 COLUMN  1 PIC  X(80) USING WS-TOP-LNE2.
003870     03  LINE  2 COLUMN  1 PIC  X(01) USING WS-G3.
004160     03          COLUMN  2 FOREGROUND-COLOR 12
004170                           VALUE "APAC Accounting".
004180     03          COLUMN 69 FOREGROUND-COLOR 13
004190                           VALUE "Version 6.1".
003920     03          COLUMN 80 PIC  X(01) USING WS-G3.
003930     03  LINE  3 COLUMN  1 PIC  X(01) USING WS-G3.
           03          COLUMN 66 VALUE "Date:".
           03          COLUMN 72 PIC  Z9/99/99 USING W12-TODAY.
003940     03          COLUMN 80 PIC  X(01) USING WS-G3.
003950     03  LINE  4 COLUMN  1 PIC  X(01) USING WS-G3.
003960     03          COLUMN 80 PIC  X(01) USING WS-G3.
003970     03  LINE  5 COLUMN  1 PIC  X(01) USING WS-G3.
003980     03          COLUMN 80 PIC  X(01) USING WS-G3.
003990     03  LINE  6 COLUMN  1 PIC  X(01) USING WS-G3.
004000     03          COLUMN 80 PIC  X(01) USING WS-G3.
004010     03  LINE  7 COLUMN  1 PIC  X(01) USING WS-G3.
004020     03          COLUMN 80 PIC  X(01) USING WS-G3.
004030     03  LINE  8 COLUMN  1 PIC  X(01) USING WS-G3.
004040     03          COLUMN 80 PIC  X(01) USING WS-G3.
004050     03  LINE  9 COLUMN  1 PIC  X(01) USING WS-G3.
004060     03          COLUMN 80 PIC  X(01) USING WS-G3.
004070     03  LINE 10 COLUMN  1 PIC  X(01) USING WS-G3.
004080     03          COLUMN 80 PIC  X(01) USING WS-G3.
004090     03  LINE 11 COLUMN  1 PIC  X(01) USING WS-G3.
004100     03          COLUMN 80 PIC  X(01) USING WS-G3.
004110     03  LINE 12 COLUMN  1 PIC  X(01) USING WS-G3.
004120     03          COLUMN 80 PIC  X(01) USING WS-G3.
004130     03  LINE 13 COLUMN  1 PIC  X(01) USING WS-G3.
004140     03          COLUMN 80 PIC  X(01) USING WS-G3.
004150     03  LINE 14 COLUMN  1 PIC  X(01) USING WS-G3.
004160     03          COLUMN 80 PIC  X(01) USING WS-G3.
004170     03  LINE 15 COLUMN  1 PIC  X(01) USING WS-G3.
004180     03          COLUMN 80 PIC  X(01) USING WS-G3.
004190     03  LINE 16 COLUMN  1 PIC  X(01) USING WS-G3.
004200     03          COLUMN 80 PIC  X(01) USING WS-G3.
004210     03  LINE 17 COLUMN  1 PIC  X(01) USING WS-G3.
004220     03          COLUMN 80 PIC  X(01) USING WS-G3.
004230     03  LINE 18 COLUMN  1 PIC  X(01) USING WS-G3.
004240     03          COLUMN 80 PIC  X(01) USING WS-G3.
004250     03  LINE 19 COLUMN  1 PIC  X(01) USING WS-G3.
004260     03          COLUMN 80 PIC  X(01) USING WS-G3.
004270     03  LINE 20 COLUMN  1 PIC  X(01) USING WS-G3.
004280     03          COLUMN 80 PIC  X(01) USING WS-G3.
004290     03  LINE 21 COLUMN  1 PIC  X(01) USING WS-G3.
004300     03          COLUMN 80 PIC  X(01) USING WS-G3.
004310     03  LINE 22 COLUMN  1 PIC  X(01) USING WS-G3.
004320     03          COLUMN 80 PIC  X(01) USING WS-G3.
004330     03  LINE 23 COLUMN  1 PIC  X(01) USING WS-G3.
004340     03          COLUMN 80 PIC  X(01) USING WS-G3.
004350     03  LINE 24 COLUMN  1 PIC  X(80) USING WS-BOT-LNE2.
000860 01  S00.
000910     03  LINE  2 COLUMN 27 FOREGROUND-COLOR 15
000920                 VALUE "DEBTOR FILE CONVERT PROGRAM".
000930     03  LINE  4 COLUMN  2 VALUE "Reading :".
000950     03  LINE  6 COLUMN  2 VALUE "Writing :".
000910     03  LINE  8 COLUMN  2 FOREGROUND-COLOR 15
000920                 VALUE "DEBTOR TRANSACTION FILE".
000930     03  LINE 10 COLUMN  2 VALUE "Reading :".
000950     03  LINE 12 COLUMN  2 VALUE "Writing :".
001080 01  S04.
001090     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001100                 VALUE "Convertion of DEBTOR file completed".
001110     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001120                 VALUE "Press any key to continue".
001130 01  S05.
001140     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001150                 VALUE "Convertion unsuccessfull !".
001160     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup".
      /
001210 PROCEDURE DIVISION.
001220 AA000           SECTION.
001230 AA00.
041220	     MOVE 1	       TO WS-S1.
021870       MOVE SPACES TO WS-MID-LNE WS-MID-LNE2.
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
                            WS-MCHR (1)  WS-MCHR (80) 
                            WS-MCH  (1)  WS-MCH  (80).
021970       MOVE WS-G4  TO WS-BCHR (16) WS-BCHR (41)
021980                      WS-BCHR (64) WS-BCHR (71).
             MOVE WS-COMPANY     TO WS-TOP-COMP.
001250       DISPLAY WS-MID-LNE2 AT 1201 WITH FOREGROUND-COLOR 3.
001270       PERFORM ZA000.
           IF WS-ERROR = 9
               GO TO AA50.
001260       DISPLAY S00.
             DISPLAY "Left justify account no. (Y/N)" AT 2502.
       AA04.
             ACCEPT WS-JUST AT 2533 
                    WITH FOREGROUND-COLOR 15 UPDATE AUTO.
011980       CALL "_MSUPCASE" USING WS-JUST
023170                        LENGTH WS-JUST
023180                               WS-LENGTH
023190                        LENGTH WS-LENGTH
023200                               WS-STATUS.
           IF NOT (WS-JUST = "N" OR "Y")
               GO TO AA04.
             MOVE 1              TO WS-DEBKEY.
001340 AA05.
             ADD 1               TO WS-DEBKEY.
001350       READ DEBOLD.
           IF WS-STATUS NOT = "00"
               GO TO AA10.
           IF DBO-ACNO = ZERO
               GO TO AA05.
             DISPLAY DBO-ACNO AT 0412 WITH FOREGROUND-COLOR 11
                     " " DBO-NAME WITH FOREGROUND-COLOR 11.
001600       PERFORM AW000.
             GO TO AA05.
001450 AA10.
             MOVE ZERO           TO WS-DBTKEY.
001340 AA15.
             ADD 1               TO WS-DBTKEY.
001350       READ DBTOLD.
           IF WS-STATUS NOT = "00"
               GO TO AA20.
             DISPLAY WS-DBTKEY AT 1012 WITH FOREGROUND-COLOR 11.
001600       PERFORM AX000.
             GO TO AA15.
       AA20.
             OPEN OUTPUT DEBMEM.
             CLOSE DEBMEM.
             OPEN OUTPUT CHEQUE.
             CLOSE CHEQUE.
             OPEN OUTPUT GARTEE.
             CLOSE GARTEE.
006070       OPEN OUTPUT INVOICE.
             CLOSE INVOICE.
             OPEN I-O INVOICE.
006070       OPEN OUTPUT SORDER.
             CLOSE SORDER.
             OPEN I-O SORDER.
             MOVE 1              TO WS-NETKEY.
             READ NETWORK.
000060       MOVE DEB-ACCOUNTS   TO DBN-ACCOUNTS
000070       MOVE DEB-INVOICE    TO DBN-INVOICE 
000080       MOVE DEB-BATCH      TO DBN-BATCH   
000090       MOVE DEB-DEBTOR     TO DBN-DEBTOR  
000110       MOVE DEB-DINDEX     TO DBN-DINDEX  
000120       MOVE DEB-DBTRAN     TO DBN-DBTRAN  
000130       MOVE DEB-INVOIC     TO DBN-INVOIC  
000140       MOVE DEB-AUDIT      TO DBN-AUDIT   
000150       MOVE DEB-SALORD     TO DBN-SALORD  
000160       MOVE DEB-SALIDX     TO DBN-SALIDX  
000170       MOVE DEB-OPEN       TO DBN-OPEN    
000180       MOVE DEB-OUT        TO DBN-OUT     
000190       MOVE DEB-MTD        TO DBN-MTD     
000200       MOVE DEB-ONE        TO DBN-ONE     
000210       MOVE DEB-TWO        TO DBN-TWO     
000220       MOVE DEB-THREE      TO DBN-THREE   
000230       MOVE DEB-FOUR       TO DBN-FOUR    
000240       MOVE DEB-DT         TO DBN-DT      
000250       MOVE DEB-CR         TO DBN-CR      
000260       MOVE DEB-INTEREST   TO DBN-INTEREST
000270       MOVE DEB-VAT-DUE    TO DBN-VAT-DUE 
000280       MOVE DEB-BD         TO DBN-BD      
000290       MOVE DEB-BDRM       TO DBN-BDRM    
000300       MOVE DEB-BDRY       TO DBN-BDRY    
000310       MOVE DEB-ORDER      TO DBN-ORDER   
000100       MOVE 1              TO DBN-JOBNO   
000320       MOVE DEB-CHECK      TO DBN-CHECK   
             MOVE NTN-DEBTOR     TO NET-RECORD.
             REWRITE NET-RECORD.
001830       CLOSE DEBTOR
                   DEBOLD
                   DBTRAN
                   DBTOLD
                   INVOICE
                   SORDER
                   PARAM
001870             NETWORK.
001910       DISPLAY S04.
             ACCEPT WS-OPTION AT 1639 WITH FOREGROUND-COLOR 15 AUTO.
004990       CALL "_MSREMOVE" USING BY REFERENCE W02-DEBOLD
005000                              BY CONTENT LENGTH OF W02-DEBOLD
005030                              BY REFERENCE WS-STATUS.
004990       CALL "_MSREMOVE" USING BY REFERENCE W02-DBTOLD
005000                              BY CONTENT LENGTH OF W02-DBTOLD
005030                              BY REFERENCE WS-STATUS.
       AA25.
001920       EXIT PROGRAM.
001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.
      *
      *    ****    L E F T   J U S T I F Y   A C C O U N T   N O.
      *
       AB000           SECTION.
       AB00.
             MOVE DBO-ACNO       TO W15-NO.
       AB05.
           IF W15-1ST = SPACE
               MOVE W15-2TO5     TO W15-ACNO
               GO TO AB05.
             MOVE W15-ACNO       TO DEB-ACNO.
       AB999.
             EXIT.
      *
      *    ****    L E F T   J U S T I F Y   A C C O U N T   N O.
      *
       AC000           SECTION.
       AC00.
             MOVE TRO-AC         TO W15-NO.
       AC05.
           IF W15-1ST = SPACE
               MOVE W15-2TO5     TO W15-ACNO
               GO TO AC05.
             MOVE W15-ACNO       TO TRA-AC.
       AC999.
             EXIT.
      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
001960 AW000           SECTION.
011840 AW01.
             INITIALIZE DEB-RECORD1.
           IF WS-JUST = "N"
000040         MOVE DBO-ACNO     TO DEB-ACNO
           ELSE
               PERFORM AB000.
000050       MOVE DBO-TYPE       TO DEB-TYPE.
000060       MOVE DBO-CATEGORY   TO DEB-CATEGORY.
000150       MOVE DBO-TITLE      TO DEB-TITLE.
000080       MOVE DBO-NAME       TO DEB-NAME.
000110       MOVE DBO-ADDRESS    TO DEB-ADDRESS.
000120       MOVE DBO-PADDR      TO DEB-PADDR.
000130       MOVE DBO-POST       TO DEB-POST.
000140       MOVE DBO-PPOST      TO DEB-PPOST.
000160       MOVE DBO-H-TEL      TO DEB-H-TEL
             MOVE DBO-B-TEL      TO DEB-B-TEL.
000180       MOVE DBO-ID         TO DEB-ID.
000200       MOVE DBO-ACTIVE     TO DEB-ACTIVE.
000210       MOVE DBO-STM        TO DEB-STM.
000220       MOVE DBO-BAL        TO DEB-BAL.
000230       MOVE DBO-CUR        TO DEB-CUR.
000240       MOVE DBO-30         TO DEB-30.
000250       MOVE DBO-60         TO DEB-60.
000260       MOVE DBO-90         TO DEB-90.
000270       MOVE DBO-120        TO DEB-120.
000280       MOVE DBO-DEBT       TO DEB-DEBT.
000290       MOVE DBO-CRED       TO DEB-CRED.
000300       MOVE DBO-PURYTD     TO DEB-PURYTD.
000310       MOVE DBO-PURPRV     TO DEB-PURPRV.
000320       MOVE DBO-INT        TO DEB-INT.
000320       MOVE DBO-VAT        TO DEB-VAT.
000330       MOVE DBO-CRL        TO DEB-CRL.
000340       MOVE DBO-FINANCE    TO DEB-FINANCE.
000360       MOVE DBO-TERMS      TO DEB-TERMS.
000370       MOVE DBO-BDT        TO DEB-BDT.
000380       MOVE DBO-LANG       TO DEB-LANG.
000390       MOVE DBO-PMT        TO DEB-PMT.
             MOVE DBO-INV-TYPE   TO DEB-INV-TYPE.
000400       MOVE DBO-SDISC      TO DEB-SDISC.
000410       MOVE DBO-DISC       TO DEB-DISC.
000420       MOVE DBO-SMAN       TO DEB-SMAN.
             MOVE "Y"            TO DEB-LAB.
             MOVE "B"            TO DEB-OI.
             MOVE "R"            TO DEB-PRICE.
000430*      MOVE DBO-IFIRST     TO DEB-IFIRST.
000440*      MOVE DBO-ILAST      TO DEB-ILAST.
000430*      MOVE DBO-FIRST      TO DEB-FIRST.
000440*      MOVE DBO-LAST       TO DEB-LAST.
             DISPLAY DEB-ACNO AT 0612 WITH FOREGROUND-COLOR 14
                     " " DEB-NAME WITH FOREGROUND-COLOR 14.
002310       WRITE DEB-RECORD1.
           IF WS-STAT1 NOT = "0"
               MOVE 6            TO WS-F-ERROR
               PERFORM WRITE-ERROR.
002440 AW999.
002450       EXIT.
      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
001960 AX000           SECTION.
011840 AX01.
             INITIALIZE TRA-RECORD1.
             MOVE TRO-CODE       TO TRA-CODE.   
             MOVE TRO-TYPE       TO TRA-TYPE.   
             MOVE TRO-DATE       TO TRA-DATE.   
             MOVE TRO-REF        TO TRA-REF.   
             MOVE TRO-REG        TO TRA-REG.                   
             MOVE TRO-ORD        TO TRA-ORD.    
             MOVE TRO-VALUE      TO TRA-VALUE.  
             MOVE TRO-TAXFREE    TO TRA-TAXFREE.
             MOVE TRO-TAX        TO TRA-TAX.    
             MOVE TRO-DISC       TO TRA-DISC.   
           IF WS-JUST = "N"
000040         MOVE TRO-AC       TO TRA-AC
           ELSE
               PERFORM AC000.
             DISPLAY WS-DBTKEY AT 1212 WITH FOREGROUND-COLOR 14.
       AX02.     
002310       WRITE TRA-RECORD1.
           IF WS-STATUS = "22"
               MOVE TRA-REF      TO W10-REF
               IF W10-REF NOT NUMERIC
                   MOVE 1        TO W10-NUM
                   MOVE W10-REF  TO TRA-REF
                   GO TO AX02
               ELSE
                   ADD 1         TO W10-NUM
                   MOVE W10-REF  TO TRA-REF
                   GO TO AX02.
           IF WS-STAT1 NOT = "0"
               MOVE 5            TO WS-F-ERROR
               PERFORM WRITE-ERROR.
       AX999.                    
             EXIT.               
      /                          
002460 ZA000           SECTION.  
002470 ZA00.
	     MOVE "CSS" 	 TO WS-EXT15.
	     MOVE "C"		 TO WS-DSK15.
	     DISPLAY CLR-SCREEN.
	     DISPLAY (4, 2) "Extension of files to convert".
005240	     ACCEPT (4, 32) WS-EXT15 WITH UPDATE AUTO-SKIP.
	     DISPLAY (5, 2) "Disk drive of files to convert".
005240	     ACCEPT (5, 33) WS-DSK15 WITH UPDATE AUTO-SKIP.


	     MOVE WS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.

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




002480	     OPEN I-O PARAM.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130             GO TO ZA200.
002490     IF WS-STATUS NOT = "00"
002500         MOVE 15 TO WS-F-ERROR
002510         PERFORM OPEN-ERROR.
002520       MOVE 1 TO WS-PARKEY.
             READ PARAM.
002540     IF WS-STATUS NOT = "00"
002550         MOVE 15 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
             MOVE PAR-DMY TO W12-TODAY.
002520       MOVE 3 TO WS-PARKEY.
002530       READ PARAM.
002540     IF WS-STATUS NOT = "00"
002550         MOVE 15 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
002660	     MOVE WS-EXT15	 TO W02-PTH2
				    W02-PTH5
				    W02-PTH6
				    W02-PTH9
				    W02-PTH5O
				    W02-PTH6O
				    W02-PTH12
				    W02-PTH20
				    W02-PTH27
				    W02-PTH36.
042140       OPEN I-O NETWORK.
042150     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042160             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 2 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
004990       CALL "_MSRENAME" USING BY REFERENCE W02-DEBTOR
005000                              BY CONTENT LENGTH OF W02-DEBTOR
004990                              BY REFERENCE W02-DEBOLD
005000                              BY CONTENT LENGTH OF W02-DEBOLD
005030                              BY REFERENCE WS-STATUS.
004990       CALL "_MSRENAME" USING BY REFERENCE W02-DBTRAN
005000                              BY CONTENT LENGTH OF W02-DBTRAN
004990                              BY REFERENCE W02-DBTOLD
005000                              BY CONTENT LENGTH OF W02-DBTOLD
005030                              BY REFERENCE WS-STATUS.
042240       OPEN INPUT DBTOLD.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 55 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240       OPEN INPUT DEBOLD.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 56 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240       OPEN OUTPUT DBTRAN.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 5 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240       OPEN OUTPUT DEBTOR.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 6 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
002960       GO TO ZA999-EXIT.
020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2412
020130                WITH FOREGROUND-COLOR 14.
020140       DISPLAY "Press any key" AT 2512 
020150                WITH FOREGROUND-COLOR 14.
020160       ACCEPT WS-OPTION AT 2526 WITH FOREGROUND-COLOR 15 AUTO.
             MOVE 9 TO WS-ERROR.
003080 ZA999-EXIT.
003090       EXIT.
      /
003100 ZB000-ERROR           SECTION.
      
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
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 5
041660         MOVE W02-DBTRAN TO WS-FILE
041670         MOVE WS-DBTKEY  TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 6
041660         MOVE W02-DEBTOR TO WS-FILE
041670         MOVE ZERO       TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 55
041660         MOVE W02-DBTOLD TO WS-FILE
041670         MOVE WS-DBTKEY  TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 56
041660         MOVE W02-DEBOLD TO WS-FILE
041670         MOVE ZERO       TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY.
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
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
