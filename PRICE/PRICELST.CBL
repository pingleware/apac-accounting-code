      ******************************************************************
      *                                                                *
      *   *******   *******    ******	**	  ******   ********    *
      *   **	**  **	  **  **    **	**	 **    **     **       *
      *   **	**  **	  **  **	**	 **	      **       *
      *   *******   *******   **	**	  ******      **       *
      *   **	    **	**    **	**	       **     **       *
      *   **	    **	 **   **    **	**	 **    **     **       *
      *   **	    **	  **   ******	*******   ******      **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   P R I C E   L I S T	P R O G R A M	F O R	C L I E N T S  *
      *                                                                *
      *     VERSION 8.13.09 - November 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRICELST.
000030 AUTHOR.	       J W LEMMON (APAC).
000040 DATE-WRITTEN.   NOVEMBER 2000.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2000 - 2009
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

      $SET KEYCOMPRESS "7"
000200	   SELECT PRICES  ASSIGN DISK
000020                    LOCK MANUAL
000030                    WITH LOCK ON MULTIPLE RECORDS
000040                    STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
000070			  RECORD PRC-CODE
000130			  ALTERNATE RECORD PRC-DKEY DUPLICATES
000130			  ALTERNATE RECORD PRC-DKEY2 DUPLICATES
000140			  ALTERNATE RECORD PRC-XREF DUPLICATES
000140			  ALTERNATE RECORD PRC-DEPART DUPLICATES.
      $SET NOKEYCOMPRESS

000200	   SELECT STKDET  ASSIGN W02-STKDET
000040                    STATUS WS-STATUS
                          ORGANIZATION LINE SEQUENTIAL.

000220 DATA DIVISION.
000230 FILE SECTION.

      /
      *      *******   *******	 ******   ******   *******   ******
      *      **    **  **    **    **	 **    **  **	    **	  **
      *      **    **  **    **    **	 **	   **	    **
      *      *******   *******	   **	 **	   *****     ******
      *      **        **   **	   **	 **	   **		  **
      *      **        **    **    **	 **    **  **	    **	  **
      *      **        **    **  ******   ******   *******   ******
      *
000010 FD  PRICES    LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID W02-PRICES.
000030 01  PRC-RECORD1.
	   03  PRC-CODE.
000040	       05  PRC-ITEM	 PIC  X(14).
	       05  PRC-PLU REDEFINES PRC-ITEM
				 PIC  9(14).
000060	       05  PRC-ITM	 PIC  X(03).
	       05  FILLER	 PIC  X(01).
	   03  PRC-LDG.
000070	       05  PRC-DEPART	 PIC  X(30).
000050	   03  PRC-ACODE	 PIC  X(10).
000080	   03  PRC-DESC.
	       05  PRC-DSC26.
000090		   07  PRC-DKEY	 PIC  X(14).
		   07  FILLER	 PIC  X(12).
000100	       05  FILLER	 PIC  X(04).
	   03  PRC-DESC2.
000090	       05  PRC-DKEY2	 PIC  X(14).
000100	       05  FILLER	 PIC  X(16).
000110	   03  PRC-QUANT	 PIC S9(09)V9(04) COMP-3.
	   03  PRC-SALES-FIELDS.
	       05  PRC-UNIT	 PIC S9(04)V999 COMP-3.
	       05  PRC-PER	 PIC  X(08).
000140	       05  PRC-PRICE	 PIC S9(08)V999 COMP-3.
	       05  PRC-UNIT-2	 PIC S9(04)V999 COMP-3.
	       05  PRC-PER-2	 PIC  X(08).
000140	       05  PRC-PRICE-2	 PIC S9(08)V999 COMP-3.
	       05  PRC-UNIT-3	 PIC S9(04)V999 COMP-3.
	       05  PRC-PER-3	 PIC  X(08).
000140	       05  PRC-PRICE-3	 PIC S9(08)V999 COMP-3.
	       05  PRC-UNIT-4	 PIC S9(04)V999 COMP-3.
	       05  PRC-PER-4	 PIC  X(08).
000140	       05  PRC-PRICE-4	 PIC S9(08)V999 COMP-3.
	   03  PRC-SELLING REDEFINES PRC-SALES-FIELDS.
	       05  PRC-BREAK-DOWNS OCCURS 4.
		   07  PRC-UNT	 PIC S9(04)V999 COMP-3.
		   07  PRC-PR	 PIC  X(08).
000140		   07  PRC-PRC	 PIC S9(08)V999 COMP-3.
000410	   03  PRC-TAX		 PIC  9(01).
	   03  PRC-VAT		 PIC  9(02)V99	COMP-3.
000420	   03  PRC-XREF 	 PIC  X(18).

000030 01  PRC-RECORD2.
	   03  FILLER		 PIC  X(18).
	   03  PRC-COMP 	 PIC  X(40).
	   03  PRC-LIST.
	       05  FILLER	 PIC  X(17).
	       05  PRC-DAY	 PIC  Z9.
	       05  FILLER	 PIC  X(01).
	       05  PRC-MONTH	 PIC  X(14).
	   03  PRC-PACKS	 PIC  X(01).
	   03  FILLER		 PIC  X(01).
	   03  PRC-ACNO 	 PIC  X(06).
	   03  PRC-DEBTOR	 PIC  X(40).
	   03  PRC-EXPIRE	 PIC  9(08)   COMP-5.
	   03  FILLER		 PIC  X(75).

       FD  STKDET    LABEL RECORD OMITTED.
       01  EXP-REC1.
	   03  EXP-DATA     PIC  X(650).

      /
000350 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001710 77  WS-SUB	   PIC 9(04)	 COMP-5.
001710 77  WS-S1	   PIC 9(04)	 COMP-5.
001720 77  WS-S2	   PIC 9(04)	 COMP-5.
001730 77  WS-S3	   PIC 9(04)	 COMP-5.
001740 77  WS-S4	   PIC 9(04)	 COMP-5.
001750 77  WS-S5	   PIC 9(04)	 COMP-5.
001760 77  WS-S6	   PIC 9(04)	 COMP-5.
001770 77  WS-S7	   PIC 9(04)	 COMP-5.
001780 77  WS-S8	   PIC 9(04)	 COMP-5.
001790 77  WS-IXS	   PIC 9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC 9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC 9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC 9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC 9(04)	 COMP-5.
002020 77  WS-OPTION	   PIC X(01).
002020 77  WS-PACKS	   PIC X(01).
002070 77  WS-ERROR	   PIC 9(01).
002790 77  WS-ETYPE	   PIC	X(01) VALUE "I".
002170 77  WS-HYPH	   PIC X(14)	 VALUE "-".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
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
	       05  FILLER    PIC  X(01) VALUE X"B3".
	       05  WS-SPACE  PIC  X(60) VALUE SPACES.
	       05  FILLER    PIC  X(01) VALUE X"B3".
	   03  WS-HME-LINE   PIC  X(60) VALUE ALL X"CD".
000040	   03  WS-COMPANY.
000050         05  WS-COMPCHR  PIC  X(01) OCCURS 40.

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

002350 01  W02-FID.

000010	   03  W02-PRICES.
000030	       05  FILLER      PIC  X(10) VALUE "PRICES.DAT".

	   03  W02-STKDET.
	       05  W02-EXPORT PIC X(32) VALUE "PRICES".

003920 01  W05-TAX-RATES.
003930     03  W05-VAT-RATES.
003940	       05  W05-VAT     PIC S9(03)V99 COMP-3 OCCURS 12.
003950     03  W05-TAX-RATES REDEFINES W05-VAT-RATES.
003960	       05  W05-VAT-R1  PIC S9(03)V99 COMP-3.
003970         05  W05-VAT-R2  PIC S9(03)V99 COMP-3.
003980         05  W05-VAT-R3  PIC S9(03)V99 COMP-3.
003990         05  W05-VAT-R4  PIC S9(03)V99 COMP-3.
004000         05  W05-VAT-R5  PIC S9(03)V99 COMP-3.
004010         05  W05-VAT-R6  PIC S9(03)V99 COMP-3.
003960	       05  W05-PVAT-R1 PIC S9(03)V99 COMP-3.
003970	       05  W05-PVAT-R2 PIC S9(03)V99 COMP-3.
003980	       05  W05-PVAT-R3 PIC S9(03)V99 COMP-3.
003990	       05  W05-PVAT-R4 PIC S9(03)V99 COMP-3.
004000	       05  W05-PVAT-R5 PIC S9(03)V99 COMP-3.
004010	       05  W05-PVAT-R6 PIC S9(03)V99 COMP-3.
004020     03  W05-RATE        PIC S9(03)V99 COMP-3.
004030     03  W05-RTE REDEFINES W05-RATE
004040                         PIC S9V9(04)  COMP-3.
004050     03  W05-VATNO       PIC  X(14).
	   03  W05-VAT-CODE.
	       05  FILLER      PIC  X(03) VALUE "VAT".
	       05  W05-V-RATE  PIC  9(01) VALUE 1.

004160 01  W10-STOCK.
	   03  W10-EXT-ENTRY.
002560	       05  W10-ENTRY	 PIC  X(14).
	       05  FILLER	 PIC  X(04).
	   03  W10-EXT-ITEM.
002560	       05  W10-ITEM.
		   07  W10-IC	 PIC  X(01) OCCURS 14.
	       05  W10-ITEM14 REDEFINES W10-ITEM.
		   07  W10-I14	 PIC  9(14).
	       05  W10-ITEM13 REDEFINES W10-ITEM.
		   07  W10-I13	 PIC  9(13).
		   07  FILLER	 PIC  X(01).
	       05  W10-ITEM12 REDEFINES W10-ITEM.
		   07  W10-I12	 PIC  9(12).
		   07  FILLER	 PIC  X(02).
	       05  W10-ITEM11 REDEFINES W10-ITEM.
		   07  W10-I11	 PIC  9(11).
		   07  FILLER	 PIC  X(03).
	       05  W10-ITEM10 REDEFINES W10-ITEM.
		   07  W10-I10	 PIC  9(10).
		   07  FILLER	 PIC  X(04).
	       05  W10-ITEM9 REDEFINES W10-ITEM.
		   07  W10-I9	 PIC  9(09).
		   07  FILLER	 PIC  X(05).
	       05  W10-ITEM8 REDEFINES W10-ITEM.
		   07  W10-I8	 PIC  9(08).
		   07  FILLER	 PIC  X(06).
	       05  W10-ITEM7 REDEFINES W10-ITEM.
		   07  W10-I7	 PIC  9(07).
		   07  W10-IQ	 PIC  9(02)V999.
		   07  FILLER	 PIC  X(02).
	       05  W10-ITEM6 REDEFINES W10-ITEM.
		   07  W10-I6	 PIC  9(06).
		   07  FILLER	 PIC  X(08).
	       05  W10-ITEM5 REDEFINES W10-ITEM.
		   07  W10-I5	 PIC  9(05).
		   07  FILLER	 PIC  X(09).
	       05  W10-ITEM4 REDEFINES W10-ITEM.
		   07  W10-I4	 PIC  9(04).
		   07  FILLER	 PIC  X(10).
	       05  W10-ITEM3 REDEFINES W10-ITEM.
		   07  W10-I3	 PIC  9(03).
		   07  FILLER	 PIC  X(11).
	       05  W10-ITM	 PIC  X(03).
	       05  FILLER	 PIC  X(01).
	   03  W10-EXT-CODE1.
002570	       05  W10-CODE1	 PIC  X(14).
002580	       05  W10-COD1 REDEFINES W10-CODE1.
002590		   07  W10-CD1	 PIC  X(02).
002600		   07  W10-CDE1	 PIC  X(12).
002620	       05  W10-COD1O REDEFINES W10-CODE1.
		   07  W10-CD1C	 PIC  X(01) OCCURS 14.
	       05  FILLER	 PIC  X(04).
	   03  W10-EXT-CODE2.
002610	       05  W10-CODE2	 PIC  X(14).
002620	       05  W10-COD2 REDEFINES W10-CODE2.
002630		   07  W10-CD2	 PIC  X(02).
002640		   07  W10-CDE2	 PIC  X(12).
002620	       05  W10-COD2O REDEFINES W10-CODE2.
		   07  W10-CD2C	 PIC  X(01) OCCURS 14.
	       05  W10-CODE2-EXT.
		   07  W10-CODE2-ITM
				 PIC  X(03).
		   07  FILLER	 PIC  X(01).
002650	   03  W10-ACODE	 PIC  X(10).
002660     03  W10-ACDE1 REDEFINES W10-ACODE.
002670	       05  W10-ACRSE	 PIC  X(01).
002680	       05  W10-ASFINE	 PIC  X(02).
002690	       05  FILLER	 PIC  X(07).
002760	   03  W10-BIN		 PIC  X(06).
002890	   03  W10-LEDG.
	       05  W10-DEPART	 PIC  X(30).
002770	   03  W10-DESC.
	     04  W10-SDSC.
	       05  W10-DENT	 PIC  X(14).
	       05  FILLER	 PIC  X(14).
	     04  FILLER 	 PIC  X(02).
002770	   03  W10-DESC2.
	     04  W10-SDSC2.
	       05  W10-DENT2	 PIC  X(14).
	       05  FILLER	 PIC  X(16).
	   03  W10-QUANT	 PIC S9(09)V9(04) COMP-3.
002770	   03  W10-DEPT 	 PIC  X(30).
	   03  W10-SALES-FIELDS.
	       05  W10-UNIT	 PIC S9(04)V999   COMP-3.
	       05  W10-PER	 PIC  X(08).
000140	       05  W10-SELL	 PIC S9(08)V999   COMP-3.
	       05  W10-UNIT-2	 PIC S9(04)V999   COMP-3.
	       05  W10-PER-2	 PIC  X(08).
000140	       05  W10-SELL-2	 PIC S9(08)V999   COMP-3.
	       05  W10-UNIT-3	 PIC S9(04)V999   COMP-3.
	       05  W10-PER-3	 PIC  X(08).
000140	       05  W10-SELL-3	 PIC S9(08)V999   COMP-3.
	       05  W10-UNIT-4	 PIC S9(04)V999   COMP-3.
	       05  W10-PER-4	 PIC  X(08).
000140	       05  W10-SELL-4	 PIC S9(08)V999   COMP-3.
	   03  W10-SELLING REDEFINES W10-SALES-FIELDS.
	       05  W10-BREAK-DOWNS OCCURS 4.
		   07  W10-UNT	 PIC S9(04)V999   COMP-3.
		   07  W10-PR	 PIC  X(08).
000140		   07  W10-SL	 PIC S9(08)V999   COMP-3.
002800	   03  W10-IND		 PIC  X(01).
	   03  W10-PRC		 PIC  9(03)V99	  COMP-3.
002810	   03  W10-TAX		 PIC  9(01).
	   03  W10-INCLUSIVE.
002850	       05  W10-VSELL	 PIC S9(09)V99	  COMP-3.
002850	       05  W10-VSELL-2	 PIC S9(09)V99	  COMP-3.
002850	       05  W10-VSELL-3	 PIC S9(09)V99	  COMP-3.
002850	       05  W10-VSELL-4	 PIC S9(09)V99	  COMP-3.
	   03  W10-INC-PRICES REDEFINES W10-INCLUSIVE.
	       05  W10-INC-PRC OCCURS 4.
000140		   07  W10-VSL	 PIC S9(09)V99	  COMP-3.
	   03  W10-VALUE-ADDED-TAX.
002850	       05  W10-VAT	 PIC S9(08)V999	  COMP-3.
002850	       05  W10-VAT-2	 PIC S9(08)V999	  COMP-3.
002850	       05  W10-VAT-3	 PIC S9(08)V999	  COMP-3.
002850	       05  W10-VAT-4	 PIC S9(08)V999	  COMP-3.
	   03  W10-V-A-T REDEFINES W10-VALUE-ADDED-TAX.
	       05  W10-VAT-VAL OCCURS 4.
		   07  W10-VT	 PIC S9(08)V999	  COMP-3.
	   03  W10-ETYPE	 PIC  X(01).
002910	   03  W10-DATE.
	       05  W10-EDTE	 PIC  99/99/9999.

003170 01  W12-DATE.
003180	   03  W12-TODAY      PIC 9(08).
003190     03  W12-DATE-DMY REDEFINES W12-TODAY.
003200         05  W12-DAY    PIC 9(02).
003210         05  W12-MONTH  PIC 9(02).
	       05  W12-CENT   PIC 9(02).
003220         05  W12-YEAR   PIC 9(02).
	   03  W12-LAB-DATE REDEFINES W12-TODAY.
               05  FILLER     PIC 9(02).
	       05  W12-MMYY   PIC 9(06).
	   03  W12-BAR-DATE REDEFINES W12-TODAY.
	       05  W12-DDMM   PIC 9(04).
	       05  W12-CCYY   PIC 9(04).
003230	   03  W12-T-YMD      PIC 9(08).
003240     03  W12-DATE-YMD REDEFINES W12-T-YMD.
003250	       05  W12-CC     PIC 9(02).
003250         05  W12-YY     PIC 9(02).
003260         05  W12-MM     PIC 9(02).
003270         05  W12-DD     PIC 9(02).
003240	   03  W12-DATE-CYMD REDEFINES W12-T-YMD.
	       05  W12-CY     PIC 9(04).
	       05  W12-MD     PIC 9(04).

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

001570 01  W25-CALCS.
001580	   03  W25-RESULT      PIC  9(09)V99.
001590     03  W25-RESULT1 REDEFINES W25-RESULT.
001600	       05  W25-DAYS    PIC  9(03).
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

001810 01  W40-BINARY.
001820     03  W40-BIN.
001830	       05  W40-DEC     PIC  9(02)    COMP-X.
001840     03  W40-HEX REDEFINES W40-BIN.
001850	       05  W40-CHAR    PIC  X(01).
	   03  W40-N1	       PIC  9(02)    COMP-X.
	   03  W40-N2	       PIC  9(02)    COMP-X.
	   03  W40-SWITCH      PIC  9(02)    COMP-X.
001860     03  W40-CSHPASS     PIC  X(06) VALUE SPACES.

       01  W41-SAVE.
           03  W41-LENGTH      PIC 9(04) COMP-X VALUE 2000.
           03  W41-START       PIC 9(04) COMP-X VALUE 1.
	   03  W41-BUFFER      PIC 9(04) COMP-X VALUE 1.

       01  W42-ATTRIB.
	   03  FILLER	       PIC X(2000).

       01  W42-ATTRIB2.
	   03  W42-LINES OCCURS 25.
	       05  W42-BYTE OCCURS 80.
		   07  W42-CHAR PIC 9(02) COMP-X.

       01  W42-ATTRIB3.
           03  FILLER          PIC X(2000).

       01  W42-ATTRIB4.
	   03  W42-LINE OCCURS 25.
	       05  W42-BTE OCCURS 80.
		   07  W42-CHR PIC 9(02) COMP-X.

       01  W43-SCREEN.
	   03  FILLER	       PIC X(2000).

004400 01  W43-SCREEN2.
004410	   03  W43-SCREEN2A.
004420	     05  W43-LINE    OCCURS 25.
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    C R E D I T O R S
      * 		    D E B T O R S
      * 		    G E N E R A L   L E D G E R
      * 		    G U A R A N T E E
      * 		    J O B   C O S T I N G
      * 		    J O B   R E M A R K S
      * 		    S A L E S	O R D E R
      * 		    S T O C K
      * 		    W A R E H O U S E
      *
004430	      07  W43-DET.
		09  FILLER     PIC  X(09).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    P U R C H A S E   O R D E R
      *
		09  W43-DET2.
		 11  FILLER    PIC  X(16).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    R O U T E
      *
		 11  W43-DET2A.
		  13  FILLER   PIC  X(04).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    C O M P A N Y   C O D E
      *
		  13  W43-DET3.
		    15 FILLER  PIC  X(02).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      *
		    15 W43-DET3A PIC X(42).
		  13  FILLER   PIC  X(02).
		09  FILLER     PIC  X(05).

004410	   03  W43-SCREEN2B  REDEFINES W43-SCREEN2A.
004420	     05  W43-LINE2   OCCURS 25.
004430	       07  FILLER      PIC  X(10).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    M O D E L	N U M B E R S
      *
	       07  W43-DET4A.
		 09  FILLER    PIC  X(28).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      *
		 09  W43-DET4.
		  11  FILLER   PIC  X(13).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    D E B T O R   S T A N D I N G   O R D E R S
      *
		  11  W43-DET5 PIC  X(24).
		 09  FILLER    PIC  X(02).
	       07  FILLER      PIC  X(03).

004410	   03  W43-SCREEN2C  REDEFINES W43-SCREEN2A.
004420	     05  W43-LINE3   OCCURS 25.
004430	       07  FILLER      PIC  X(41).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    T E C H N I C I A N S
      *
	       07  W43-DET6    PIC  X(34).
	       07  FILLER      PIC  X(05).
       01  W43-SERIALS REDEFINES W43-SCREEN2.
	   03  W43-SERNO       PIC  X(18) OCCURS 111.
	   03  FILLER	       PIC  X(01).

       01  W43-SCREEN3.
           03  FILLER          PIC X(2000).

       01  W44-FUNCTION        PIC 9(02)     COMP-X.

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


000010 01  W50-TABLE.
000020     03  W50-TDETAIL.
000030         05  FILLER      PIC  X(27)      VALUE
000040                         "!#$%&'()*+,-./0123456789:;<".
000050         05  FILLER      PIC  X(27)      VALUE
000060                         "=>?@ABCDEFGHIJKLMNOPQRSTUVW".
000070         05  FILLER      PIC  X(12)      VALUE
000080                         "XYZ[\]^`{|}~".
000090     03  W50-TAB1 REDEFINES W50-TDETAIL.
000100         05  W50-FCHAR  OCCURS 66.
000110             07  W50-CHAR PIC X(01).
000120     03  W50-SFKEY       PIC  9(04)  COMP-5.
000130     03  W50-RESULT      PIC  9(01).
000140         88  IX-FOUND    VALUE 0.
000150     03  W50-STATUS      PIC  X(02).
000160	   03  W50-LENGTH      PIC  9(08)  COMP-5.
000170	   03  W50-S1	       PIC  9(02)  COMP-5.
000180     03  W50-S2          PIC  9(02)  COMP-5.
           03  W50-EXP.
000190         05  W50-OPT     PIC  9(01).
000200     03  W50-END         PIC  9(02)  COMP-5.
000210     03  W50-DATA.
000220         05  W50-CCHAR   PIC  X(01)  OCCURS 40.
000230     03  W50-DATA-LEFT REDEFINES W50-DATA.
000240         05  FILLER      PIC  X(01).
000250         05  W50-D39     PIC  X(39).
000260     03  W50-DATA-THE REDEFINES W50-DATA.
000270         05  W50-THE     PIC  X(04).
000280         05  W50-D36.
000290             07  W50-D26 PIC  X(26).
000300             07  FIILER  PIC  X(10).
000310     03  W50-DATA-20 REDEFINES W50-DATA.
000320         05  W50-20.
000330           07  W50-ACODE PIC  X(10).
000340           07  FILLER    PIC  X(10).
000350         05  FILLER      PIC  X(20).
000360     03  W50-DATA-30 REDEFINES W50-DATA.
000370         05  W50-30.
000380           07  FILLER    PIC  X(01).
000390           07  W50-29    PIC  X(29).
000400         05  FILLER      PIC  X(10).
000410     03  W50-STOCK REDEFINES W50-DATA.
	       05  W50-EXT-ITEM.
000420		 07  W50-ITEM.
000430		   09  W50-1	     PIC  X(01).
000440		   09  W50-13.
000450		     11  W50-9.
		       13  W50-5.
000460			 15  W50-I23 PIC  X(02).
			 15  FILLER  PIC  X(03).
000470		       13  FILLER    PIC  X(04).
000480		     11  FILLER      PIC  X(04).
000490		 07  W50-ITM	     PIC  X(04).
	       05  W50-EX-CDE REDEFINES W50-EXT-ITEM.
		 07  FILLER	     PIC  X(01).
		 07  W50-17	     PIC  X(17).
000490	       05  FILLER	     PIC  X(26).
000500     03  W50-NAME REDEFINES W50-DATA.
000510         05  W50-ENTRY.
000520           07  W50-DENT.
000530             09  W50-N1  PIC  X(01).
000540             09  W50-N23 PIC  X(02).
000550             09  FILLER  PIC  X(04).
000560           07 FILLER     PIC  X(01).
000570         05  FILLER      PIC  X(32).
000580     03  W50-SNAME REDEFINES W50-DATA.
000590         05  W50-SURNAME.
000600           07  W50-SN1   PIC  X(01).
000610           07  W50-S23   PIC  X(23).
000620         05  FILLER      PIC  X(16).
000630     03  W50-BCODE REDEFINES W50-DATA.
000640         05  W50-BAR.
000650           07  W50-BENT.
000660             09  W50-B1  PIC  X(01).
000670             09  W50-B23 PIC  X(02).
000680             09  FILLER  PIC  X(13).
000690         05  FILLER      PIC  X(24).
000700     03  W50-LOOKUP      PIC  X(50).
000710     03  W50-ASCII       PIC  X(01).
000720     03  W50-BIN REDEFINES W50-ASCII
000730                         PIC  9(02) COMP-5.
	   03  W50-ALLOC-BC    PIC  X(01).
	   03  W50-BAR-CODE    PIC  X(01).
	   03  W50-BCLNGTH     PIC  9(02) COMP-5.
	   03  W50-BC.
	       05  W50-BCC     PIC  X(01) OCCURS 14.
	   03  W50-BCD14 REDEFINES W50-BC.
	       05  W50-BC14    PIC  9(14).
	   03  W50-BCD13 REDEFINES W50-BC.
	       05  W50-BC13    PIC  9(13).
	       05  FILLER      PIC  X(01).
	   03  W50-BCD12 REDEFINES W50-BC.
	       05  W50-BC12    PIC  9(12).
	       05  FILLER      PIC  X(02).
	   03  W50-BCD11 REDEFINES W50-BC.
	       05  W50-BC11    PIC  9(11).
	       05  FILLER      PIC  X(03).
	   03  W50-BCD10 REDEFINES W50-BC.
	       05  W50-BC10    PIC  9(10).
	       05  FILLER      PIC  X(04).
	   03  W50-BCD9 REDEFINES W50-BC.
	       05  W50-BC9     PIC  9(09).
	       05  FILLER      PIC  X(05).
	   03  W50-BCD8 REDEFINES W50-BC.
	       05  W50-BC8     PIC  9(08).
	       05  FILLER      PIC  X(06).
	   03  W50-BCD7 REDEFINES W50-BC.
	       05  W50-BC7     PIC  9(07).
	       05  W50-BCQ     PIC  9(02)V999.
	       05  FILLER      PIC  X(02).
	   03  W50-BCD6 REDEFINES W50-BC.
	       05  W50-BC6     PIC  9(06).
	       05  FILLER      PIC  X(08).

       01  W55-MONTHS.
	   03  W55-MONTH-NAMES.
	       05  FILLER      PIC  X(09) VALUE "January".
	       05  FILLER      PIC  X(09) VALUE "February".
	       05  FILLER      PIC  X(09) VALUE "March".
	       05  FILLER      PIC  X(09) VALUE "April".
	       05  FILLER      PIC  X(09) VALUE "May".
	       05  FILLER      PIC  X(09) VALUE "June".
	       05  FILLER      PIC  X(09) VALUE "July".
	       05  FILLER      PIC  X(09) VALUE "August".
               05  FILLER      PIC  X(09) VALUE "September".
	       05  FILLER      PIC  X(09) VALUE "October".
	       05  FILLER      PIC  X(09) VALUE "November".
	       05  FILLER      PIC  X(09) VALUE "December".
	   03  W55-EACH-MONTH REDEFINES W55-MONTH-NAMES.
	       05  W55-MONTH   PIC  X(09) OCCURS 12.

       01  W60-MTH-YR.
	   03  W60-MMYY.
	       05  W60-MY     PIC  X(01) OCCURS 14.
	   03  W60-CCYY.
	       05  W60-CY     PIC  X(01) OCCURS 4.

       01  W65-EXPIRY.
	   03  W65-DAY	      PIC  Z9.
	   03  FILLER	      PIC  X(01) VALUE SPACE.
	   03  W65-MMYY       PIC  X(14).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
	   03  W75-S2	      PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
	   03  W75-ITEMS OCCURS 18.
	     05  W75-EXT-CODE.
	       07  W75-CODE	 PIC  X(14).
	       07  W75-ITM	 PIC  X(03).
	       07  FILLER	 PIC  X(01).
	     05  W75-PRICES.
	       07  W75-PER	 PIC  X(08).
	       07  W75-SELL	 PIC  9(08)V999 COMP-3.
	       07  W75-VSELL	 PIC  9(09)V99	COMP-3.
	       07  W75-PER-2	 PIC  X(08).
	       07  W75-SELL-2	 PIC  9(08)V999 COMP-3.
	       07  W75-VSELL-2	 PIC  9(09)V99	COMP-3.
	       07  W75-PER-3	 PIC  X(08).
	       07  W75-SELL-3	 PIC  9(08)V999 COMP-3.
	       07  W75-VSELL-3	 PIC  9(09)V99	COMP-3.
	       07  W75-PER-4	 PIC  X(08).
	       07  W75-SELL-4	 PIC  9(08)V999 COMP-3.
	       07  W75-VSELL-4	 PIC  9(09)V99	COMP-3.
	     05  W75-DSP-PRICES REDEFINES W75-PRICES.
	       07  W75-DSP-PRC OCCURS 4.
		 09  W75-PR	 PIC  X(08).
		 09  W75-SL	 PIC  9(08)V999 COMP-3.
		 09  W75-VSL	 PIC  9(09)V99	COMP-3.
	   03  W75-SLEDG	 PIC  X(04) OCCURS 10.

       01  W78-EXPORT.
	   03  W78-S1	       PIC  9(04) COMP-5.
	   03  W78-EXP-DATA.
	       05  W78-EXP     PIC  X(01) OCCURS 650.

003820 01  W80-FIELDS.
	   03  W80-S	       PIC  9(02) COMP-5.
003830	   03  W80-FNO	       PIC  9(02) COMP-5 OCCURS 20.

003850 01  W85-FIELDS.
	   03  W85-S	       PIC  9(02) COMP-5.
	   03  W85-FIELD       PIC  X(22).
003860	   03  W85-FLDS.
      *    Field 01
003870         05  FILLER      PIC X(21) VALUE "Item Code".
      *    Field 02
003870	       05  FILLER      PIC X(21) VALUE "Description".
      *    Field 03
003870	       05  FILLER      PIC X(21) VALUE "Description 2".
      *    Field 04
003870         05  FILLER      PIC X(21) VALUE "Alternate Code".
      *    Field 05
003870	       05  FILLER      PIC X(21) VALUE "Bar/XRef Code".
      *    Field 06
	       05  FILLER      PIC X(21) VALUE "Department".
      *    Field 07
003870	       05  FILLER      PIC X(21) VALUE "Units".
      *    Field 08
003870	       05  FILLER      PIC X(21) VALUE "Priced Per".
      *    Field 09
003870	       05  FILLER      PIC X(21) VALUE "Exclusive Price".
      *    Field 10
003870	       05  FILLER      PIC X(21) VALUE "Tax Rate Percent".
      *    Field 11
003870	       05  FILLER      PIC X(21) VALUE "Value Added Tax".
      *    Field 12
003870	       05  FILLER      PIC X(21) VALUE "Inclusive Price".
      *    Field 13
003870	       05  FILLER      PIC X(21) VALUE "Pack 1 Units".
      *    Field 14
003870	       05  FILLER      PIC X(21) VALUE "Price per Pack 1".
      *    Field 15
003870	       05  FILLER      PIC X(21) VALUE "Pack 1 Price".
      *    Field 16
003870	       05  FILLER      PIC X(21) VALUE "Pack 2 Units".
      *    Field 17
003870	       05  FILLER      PIC X(21) VALUE "Price per Pack 2".
      *    Field 18
003870	       05  FILLER      PIC X(21) VALUE "Pack 2 Price".
      *    Field 19
003870	       05  FILLER      PIC X(21) VALUE "Pack 3 Units".
      *    Field 20
003870	       05  FILLER      PIC X(21) VALUE "Price per Pack 3".
      *    Field 21
003870	       05  FILLER      PIC X(21) VALUE "Pack 3 Price".
      *    Field 22
003870	       05  FILLER      PIC X(21) VALUE SPACES.
	   03  W85-RFLDS REDEFINES W85-FLDS.
	       05  W85-FLD     PIC  X(21) OCCURS 22.

007010 01  W100-EDIT.
007120	   03  W100-V2.
007130         05  W100-S6V2   PIC Z(05)9.99-.
007140	   03  W100-P1 REDEFINES W100-V2.
007150         05  W100-PRICE  PIC X(09).
007160         05  FILLER      PIC X(01).
	   03  W100-1.
               05  W100-N      PIC  9(01).
           03  W100-2.
               05  W100-N2     PIC  9(02).
           03  W100-3.
               05  W100-NO1    PIC  9(03).
           03  W100-9.
               05  W100-NO2    PIC  Z(08)9.
           03  W100-S9.
	       05  W100-NO3    PIC  -(07)9.
	   03  W100-S9Q.
	       05  W100-QNT    PIC  -(09)9.9999.
	   03  W100-S9V2.
	       05  W100-V1     PIC  -(09)9.99.
           03  W100-9V2.
               05  W100-V2     PIC  Z(08)9.99.
	   03  W100-9V3.
               05  W100-V3     PIC  Z(08)9.999.
	   03  W100-9V4.
	       05  W100-V3B    PIC  Z(08)9.9999.
           03  W100-3V2.
               05  W100-V4     PIC  Z(02)9.99.
           03  W100-DATE.
	       05  W100-DTE    PIC  9(08) BLANK WHEN ZERO.


      /
004130 SCREEN SECTION.
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
	   05  LINE  1 COLUMN 47 BACKGROUND-COLOR 0
				 VALUE "´".
	   05	       COLUMN 48 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC Z9/99/9999 USING TODAY-DDMMYY.
	   05	       COLUMN 58 BACKGROUND-COLOR 0
				 VALUE "Ã".
003870	   05  LINE  2 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
004160	   05	       COLUMN  2 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
004170				 VALUE "APAC Systems".
	   05	       COLUMN 14 PIC  X(57) FROM WS-BACKGROUND.
004180	   05	       COLUMN 70 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
004190				 VALUE "Price List".
003920	   05	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003930	   03  LINE  3 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
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

       01  S-PRICE.
008690	   03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15
008700				 VALUE "PRICE LOOKUP".
	   03  LINE  3 COLUMN  3 FOREGROUND-COLOR 3
				 VALUE "Item code  ".
	   03	       COLUMN 49 FOREGROUND-COLOR 3
				 VALUE "Alternate code".
	   03	       COLUMN 64 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(10) FROM WS-SPACE.
	   03  LINE  4 COLUMN  3 FOREGROUND-COLOR 3
				 VALUE "Bar code   ".
	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(18) FROM WS-SPACE.
	   03  LINE  6 COLUMN  3 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Description".
	   03  LINE  7 COLUMN  8 FOREGROUND-COLOR 3
				 VALUE "Line 1".
	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM WS-SPACE.
	   03  LINE  8 COLUMN  8 FOREGROUND-COLOR 3
				 VALUE "Line 2".
	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM WS-SPACE.
	   03  LINE 10 COLUMN  3 FOREGROUND-COLOR 3
				 VALUE "Department ".
	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM WS-SPACE.
	   03  LINE 11 COLUMN  3 FOREGROUND-COLOR 3
				 VALUE "Quantity   ".
	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(15) FROM WS-SPACE.
	   03  LINE 12 COLUMN  3 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Pricing    ".
	   03  LINE 13 COLUMN  7 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄ
      -				 "ÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE 14 COLUMN  7 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 VALUE "³   Pack   ³  Units   ³  Exclusi
      -				 "ve   ³     VAT      ³  Inclusive   ³".
	   03  LINE 15 COLUMN  7 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ
      -				 "ÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 16 COLUMN  7 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 VALUE "³          ³          ³
      -				 "     ³              ³              ³".
	   03  LINE 17 COLUMN  7 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 VALUE "³          ³          ³
      -				 "     ³              ³              ³".
	   03  LINE 18 COLUMN  7 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 VALUE "³          ³          ³
      -				 "     ³              ³              ³".
	   03  LINE 19 COLUMN  7 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 VALUE "³          ³          ³
      -				 "     ³              ³              ³".
	   03  LINE 20 COLUMN  7 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄ
      -				 "ÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
	   03  LINE 22 COLUMN 18 FOREGROUND-COLOR 3
				 VALUE "These prices are valid until ".
	   03	       COLUMN 47 FOREGROUND-COLOR 6 HIGHLIGHT
				 PIC  X(17) FROM W65-EXPIRY.

       01  SA-PRICE.
	   03  LINE  3 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(18) USING W10-EXT-ITEM AUTO.

       01  SD-PRICE.
	   03  LINE  3 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(18) FROM W10-EXT-ITEM.
	   03	       COLUMN 64 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(10) FROM W10-ACODE.
	   03  LINE  4 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(18) FROM W10-EXT-CODE1.
	   03  LINE  7 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM W10-DESC.
	   03  LINE  8 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM W10-DESC2.
	   03  LINE 10 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM W10-DEPT.
	   03  LINE 11 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(08)9.9(04)- FROM W10-QUANT.
	   03  LINE 16 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(08) FROM W10-PER.
	   03	       COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  ZZZ9.999 BLANK WHEN ZERO
				 FROM W10-UNIT.
	   03	       COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.999 BLANK WHEN ZERO
				 FROM W10-SELL.
	   03	       COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.999 BLANK WHEN ZERO
				 FROM W10-VAT.
	   03	       COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.99 BLANK WHEN ZERO
				 FROM W10-VSELL.
	   03  LINE 17 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(08) FROM W10-PER-2.
	   03	       COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  ZZZ9.999 BLANK WHEN ZERO
				 FROM W10-UNIT-2.
	   03	       COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.999 BLANK WHEN ZERO
				 FROM W10-SELL-2.
	   03	       COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.999 BLANK WHEN ZERO
				 FROM W10-VAT-2.
	   03	       COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.99 BLANK WHEN ZERO
				 FROM W10-VSELL-2.
	   03  LINE 18 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(08) FROM W10-PER-3.
	   03	       COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  ZZZ9.999 BLANK WHEN ZERO
				 FROM W10-UNIT-3.
	   03	       COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.999 BLANK WHEN ZERO
				 FROM W10-SELL-3.
	   03	       COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.999 BLANK WHEN ZERO
				 FROM W10-VAT-3.
	   03	       COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.99 BLANK WHEN ZERO
				 FROM W10-VSELL-3.
	   03  LINE 19 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(08) FROM W10-PER-4.
	   03	       COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  ZZZ9.999 BLANK WHEN ZERO
				 FROM W10-UNIT-4.
	   03	       COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.999 BLANK WHEN ZERO
				 FROM W10-SELL-4.
	   03	       COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.999 BLANK WHEN ZERO
				 FROM W10-VAT-4.
	   03	       COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(07)9.99 BLANK WHEN ZERO
				 FROM W10-VSELL-4.

008680 01  S-STK.
008690     03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15
008700				 VALUE "PRICE LOOKUP".
008720     03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE 
008730	       " Item Code         Description                     Unit
008740-        "   Cost excl Cost incl".
009160	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
008770     03  LINE 23 COLUMN  2 FOREGROUND-COLOR 7 VALUE 
008780       "Key in at least the first character of the code or press".
008790     03          COLUMN 59 FOREGROUND-COLOR 14 VALUE "Esc".
008800     03          COLUMN 62 FOREGROUND-COLOR 7 VALUE " to exit".
008810	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1 VALUE "Pressing '".
008820	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 14 VALUE "ENTER".
008830	   03	       COLUMN 17 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1 VALUE
008840                           "' key only, will start from the beginn
008850- 			 "ing of the price file".

       01  S-STKA.
008710     03  LINE  3 COLUMN  2 "Start Item Code :".

008880 01  S-STKB.
008890	   03  LINE  3 COLUMN  2 "Start Bar Code :".

008900 01  S-STKD.
008910	   03  LINE  3 COLUMN  2 "Start Description :".
008920     03  LINE 23 COLUMN  2 FOREGROUND-COLOR 7 VALUE 
008930       "Key in at least the first character of the description or 
008940-      "press".
008950     03          COLUMN 66 FOREGROUND-COLOR 14 VALUE "Esc".
008960     03          COLUMN 69 FOREGROUND-COLOR 7 VALUE " to exit".

008900 01  S-STKD2.
008910	   03  LINE  3 COLUMN  2 "Start Descript. 2 :".
008920	   03  LINE 23 COLUMN  2 FOREGROUND-COLOR 7 VALUE
008930       "Key in at least the first character of the description or 
008940-      "press".
008950     03          COLUMN 66 FOREGROUND-COLOR 14 VALUE "Esc".
008960     03          COLUMN 69 FOREGROUND-COLOR 7 VALUE " to exit".

008900 01  S-STKDP.
008910	   03  LINE  3 COLUMN  2 "Start Department  :".
008920	   03  LINE 23 COLUMN  2 FOREGROUND-COLOR 7 VALUE
008930	     "Key in at least the first character of the department or p
008940-      "ress".
008950	   03	       COLUMN 65 FOREGROUND-COLOR 14 VALUE "Esc".
008960	   03	       COLUMN 68 FOREGROUND-COLOR 7 VALUE " to exit".

008970 01  SA-STK.
008980	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
008990				 PIC  X(18) USING W10-EXT-ITEM AUTO.

009030 01  SA-STKB.
009040	   03  LINE  3 COLUMN 19 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009050				 PIC  X(18) USING W10-CODE2 AUTO.

009060 01  SA-STKD.
009070	   03  LINE  3 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009080                           PIC  X(30) USING W10-DESC AUTO.

009060 01  SA-STKD2.
009070	   03  LINE  3 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009080				 PIC  X(30) USING W10-DESC2 AUTO.

009060 01  SA-STKDP.
009070	   03  LINE  3 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009080				 PIC  X(30) USING W10-DEPART AUTO.

       01  ERROR-LINE.
	   03		COLUMN 16 BACKGROUND-COLOR 4
				  FOREGROUND-COLOR 4 HIGHLIGHT
	   VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03		COLUMN 65 BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
				  VALUE "¿".
004880	   03  LINE + 1 COLUMN 16 BACKGROUND-COLOR 4
				  FOREGROUND-COLOR 4 HIGHLIGHT
				  VALUE "³".
	   03		COLUMN 17 BACKGROUND-COLOR 4
				  FOREGROUND-COLOR 7 HIGHLIGHT
				  PIC  X(48) FROM WS-ERR-MES.
	   03		COLUMN 65 BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
				  VALUE "³".
	   03  LINE + 1	COLUMN 16 BACKGROUND-COLOR 4
				  FOREGROUND-COLOR 4 HIGHLIGHT
				  VALUE "À".
	   03		COLUMN 17 BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
	   VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  OPT-LINE.
	   03		COLUMN 16 BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 3 HIGHLIGHT
	   VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03		COLUMN 65 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				  VALUE "¿".
004880	   03  LINE + 1 COLUMN 16 BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 3 HIGHLIGHT
				  VALUE "³".
	   03		COLUMN 17 BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 6 HIGHLIGHT
				  PIC  X(48) FROM WS-ERR-MES.
	   03		COLUMN 65 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				  VALUE "³".
	   03  LINE + 1	COLUMN 16 BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 3 HIGHLIGHT
				  VALUE "À".
	   03		COLUMN 17 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
	   VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S99.
	   03	       COLUMN 24 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
		       VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 55 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "¿".
004880	   03  LINE + 1 COLUMN 24 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 25 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
000830				 VALUE "Correct ".
	   03	       COLUMN 33 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
004890				 VALUE "ENTER".
	   03	       COLUMN 38 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " - ".
	   03	       COLUMN 41 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 42 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " if incorrect".
	   03	       COLUMN 55 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
				 VALUE "³".
	   03  LINE + 1 COLUMN 24 BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 3 HIGHLIGHT
				  VALUE "À".
	   03		COLUMN 25 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
			VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

007730 PROCEDURE DIVISION.
007740 AA000-MAIN              SECTION.
007750 AA000-INIT.
007760	     PERFORM ZA000-INIT.
	     PERFORM AB000.
	     DISPLAY CLR-SCREEN.
	     STOP RUN.
      *
      *    ****    G E T   T H E   E X P O R T   F I L E   I D 
      *
       AA100           SECTION.
       AA101.
	     PERFORM SAVE-SCREEN-3.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0722
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³ E X P O R T   F I L E   N A M E	 " AT 0822
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³ " AT 0922 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				       BACKGROUND-COLOR 7
		     "Keyin the  <pathname>/<filename> ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³                                  " AT 1022
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1122 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     MOVE 08		 TO SHADE-ROW.
	     MOVE 24		 TO SHADE-COL.
	     MOVE 34		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
             MOVE ZERO           TO WS-ERROR.

       AA105.
	     ACCEPT W02-EXPORT AT 1024
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO AA199
		 WHEN OTHER
		     CALL X"E5"
		     GO TO AA105
	       END-EVALUATE.
	     MOVE 64		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W02-EXPORT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     DISPLAY W02-EXPORT AT 1024
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5 UPDATE AUTO.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO AA105.
	     OPEN OUTPUT STKDET.
           IF WS-STATUS = "00"
	       PERFORM RESTORE-SCREEN-3
	       PERFORM AA800
               GO TO AA199.
	     MOVE "Invalid file name"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
             GO TO AA105.

       AA199.
             EXIT.
      *
      *    ****    M O V E   F I E L D   T O   E X P O R T   A R E A
      *
      *            W50-DATA   : CONTAINS DATA TO BE INSERTED
      *            W50-LENGTH : LENGTH OF DATA 
      *            W50-EXP    : FIELD TERMINATION CHARACTER
      *
       AA200           SECTION.
       AA201.
           IF W50-DATA = SPACES
               GO TO AA225.
             MOVE W50-LENGTH     TO W50-S1.
      *
      *    ****    D E T E R M I N E   E N D   O F   F I E L D
      *
      *            THIS LOOP EXCLUDES THE TRAILING SPACES
      *
       AA205.
	   IF W50-CCHAR(W50-S1) = SPACE
               SUBTRACT 1        FROM W50-S1
               GO TO AA205.
      *
      *    ****    R E M O V E   L E A D I N G   S P A C E S
      *
       AA210.
           IF W50-1 = SPACE
               MOVE W50-D39      TO W50-DATA
               SUBTRACT 1        FROM W50-S1
               GO TO AA210.
             MOVE W50-S1         TO W50-S2.     
             MOVE 1              TO W50-S1.

       AA220.
	     MOVE W50-CCHAR(W50-S1)
				 TO W78-EXP(W78-S1).
             ADD 1               TO W50-S1
				    W78-S1.
           IF W50-S1 NOT > W50-S2
               GO TO AA220.

       AA225.
	     MOVE W50-EXP	 TO W78-EXP(W78-S1).
	     ADD 1		 TO W78-S1.

       AA299.
             EXIT.
      *
      *    ****    M O V E   I N D V I D U A L   F I E L D S
      *
      *            W50-BIN : FIELD NUMBER TO MOVE
      *
       AA300              SECTION.
       AA301.
	   IF W50-BIN < 01 OR > 12
               GO TO AA399.
      *
           IF W50-BIN = 01
	       MOVE PRC-CODE	 TO W50-DATA
	       MOVE 18		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 02
	       MOVE PRC-DESC	 TO W50-DATA
	       MOVE 30		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 03
	       MOVE PRC-DESC2	 TO W50-DATA
               MOVE 30           TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 04
	       MOVE PRC-ACODE	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 05
	       MOVE PRC-XREF	 TO W50-DATA
	       MOVE 18		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 06
	       MOVE PRC-DEPART	 TO W50-DATA
	       MOVE 30		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 07
	       MOVE PRC-UNIT	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 08
	       MOVE PRC-PER	 TO W50-DATA
	       MOVE 8		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 09
	       MOVE PRC-PRICE	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 10
	       MOVE PRC-VAT	 TO W100-V4
	       MOVE W100-3V2	 TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 11
	       MOVE W10-VAT	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
	   IF W50-BIN = 12
	       MOVE W10-VSELL	 TO W100-V3
	       MOVE W100-9V3	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200.

       AA399.
               EXIT.
      *
      *    ****    P R O C E S S   T H E   S E L E C T E D   F I E L D S
      *
       AA400           SECTION.
       AA401.
	     MOVE 1		 TO W80-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
	     MOVE SPACES	 TO W78-EXP-DATA.
	     MOVE 1		 TO W78-S1.

       AA405.
	   IF W80-FNO(W80-S) = ZERO
               GO TO AA499.
	     MOVE W80-FNO(W80-S) TO W50-BIN.
             PERFORM AA300.
	   IF W80-S < 12
	       ADD 1		 TO W80-S
               GO TO AA405.

       AA499.
             EXIT.
      *
      *    ****   H E A D E R S
      *
       AA800		  SECTION.
       AA801.
	     MOVE 1		 TO W80-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
	     MOVE SPACES	 TO W78-EXP-DATA.
	     MOVE 1		 TO W78-S1.
       AA805.
	   IF W80-FNO(W80-S) = ZERO
	       MOVE W78-EXP-DATA TO EXP-REC1
	       PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT
	       GO TO AA899.
	     MOVE W80-FNO(W80-S) TO W50-BIN.
	     MOVE 21		 TO W50-LENGTH.
	     MOVE W85-FLD(W50-BIN)
				 TO W50-DATA.
	     PERFORM AA200.
	   IF W80-S < 12
	       ADD 1		 TO W80-S
	       GO TO AA805.
       AA899.
             EXIT.

       AB000	       SECTION.
       AB00.
	     DISPLAY CLR-SCREEN.
	     MOVE SPACES	 TO W10-EXT-ITEM.
	     DISPLAY S-PRICE.
011010	     DISPLAY "F2" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
011020		     " Item code/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011040		     " Depart/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F4" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011060		     " Desc/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F5" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011060		     " Desc2/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F6" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011080		     " Bar code Lookup/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F9" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     " Export/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       AB05.
	     ACCEPT SA-PRICE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO AB999
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "P"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "B"	 TO W10-ETYPE
		 WHEN F9-KEY
		     PERFORM EXPORT-PRICES
		     GO TO AB05
		 WHEN OTHER
		     CALL X"E5"
		     GO TO AB05
	       END-EVALUATE
	       PERFORM SAVE-SCREEN-3
016560	       PERFORM PRICES-LOOKUP
	       PERFORM RESTORE-SCREEN-3
	       DISPLAY SA-PRICE.
021040	   IF W10-EXT-ITEM = SPACES
	       GO TO AB05.

       AB10.
	     PERFORM CA000.
021080	   IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM PRICES-LOOKUP
	       IF W10-EXT-ITEM = SPACES
		   GO TO AB05
	       ELSE
		   GO TO AB10.

       AB15.
021160	     DISPLAY CLEAR-L25.
	     MOVE PRC-CODE	 TO W10-EXT-ITEM.
	     MOVE PRC-ACODE	 TO W10-ACODE.
	     MOVE PRC-DEPART	 TO W10-DEPT.
	     MOVE PRC-DESC	 TO W10-DESC.
	     MOVE PRC-DESC2	 TO W10-DESC2.
	     MOVE PRC-QUANT	 TO W10-QUANT.
	     MOVE PRC-XREF	 TO W10-EXT-CODE1.
	     DISPLAY SD-PRICE.
	     DISPLAY "Press " AT 2502
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     "ext Item or "
		     "ANY " WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     "key to continue".
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION = "N"
		   GO TO AB20.
	     GO TO AB00.

       AB20.
	     PERFORM READ-PRICES-NEXT THRU READ-PRICES-EXIT.
021080	   IF WS-F-ERROR = 22
	       PERFORM READ-PRICES-PREV THRU READ-PRICES-EXIT.
	     PERFORM CA500.
	     DISPLAY S-PRICE.
	     GO TO AB15.

       AB999.
	     EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.
      *
      *    ****  ****  *****  ***  *****   ***** ***** *     *****
      *    *   * *   *	 *   *	 * *	   *	   *   *     *
      *    *   * *   *	 *   *	   *	   *	   *   *     *
      *    ****  ****	 *   *	   ***	   ***	   *   *     ***
      *    *	 *   *	 *   *	   *	   *	   *   *     *
      *    *	 *   *	 *   *	 * *	   *	   *   *     *
      *    *	 *   * *****  ***  *****   *	 ***** ***** *****
      *
000010 READ-PRICES.
000020	     READ PRICES	 WITH IGNORE LOCK
000030	       KEY IS PRC-CODE.
	     GO TO PRICES-CHECK-STATUS.

000380 READ-PRICES-NEXT.
000390	     READ PRICES NEXT	 WITH IGNORE LOCK.
	     GO TO PRICES-CHECK-STATUS.

000380 READ-PRICES-PREV.
000390	     READ PRICES PREVIOUS WITH IGNORE LOCK.
	     GO TO PRICES-CHECK-STATUS.

000750 START-AT-PRICES-CODE.
000760	     START PRICES
000770	       KEY >= PRC-CODE.
	     GO TO PRICES-CHECK-STATUS.

001310 START-AT-PRICES-DEPT.
001320	     START PRICES
001330	       KEY >= PRC-DEPART.
	     GO TO PRICES-CHECK-STATUS.

001310 START-AT-PRICES-DESC.
001320	     START PRICES
001330	       KEY >= PRC-DKEY.
	     GO TO PRICES-CHECK-STATUS.

001310 START-AT-PRICES-DESC2.
001320	     START PRICES
001330	       KEY >= PRC-DKEY2.
	     GO TO PRICES-CHECK-STATUS.

001310 START-AT-PRICES-XREF.
001320	     START PRICES
001330	       KEY >= PRC-XREF.

       PRICES-CHECK-STATUS.
001480	   IF WS-STATUS = "00"
001490         MOVE ZERO         TO WS-F-ERROR
001500	       GO TO READ-PRICES-EXIT.
000430	   IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
001520	       MOVE 22		 TO WS-F-ERROR
001530	       GO TO READ-PRICES-EXIT.
001540     IF WS-STAT1 = "2" OR "3" OR "4"
001550         MOVE 22           TO WS-F-ERROR
001560         PERFORM READ-ERROR.

001580 READ-PRICES-EXIT.
001590	     EXIT.
      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       WRITE-STKDET.
000250       WRITE EXP-REC1.
000260     IF WS-STATUS NOT = "00"
000270         MOVE 99           TO WS-F-ERROR
000280         PERFORM WRITE-ERROR.
000310
000320 WRITE-STKDET-EXIT.
000330       EXIT.

      /
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *
       CRT-KBD-MOUSE   SECTION.
      *
      *    ****    S C R E E N   A T T R I B U T E S
      *
       SAVE-ATTRIB.
	     CALL "CBL_READ_SCR_ATTRS" USING SAVE-POS
					     W42-ATTRIB
					     W41-LENGTH
				       RETURNING WS-STATUS.

       SCREEN-ATTRIBUTES-4.
	     CALL "CBL_READ_SCR_ATTRS" USING SAVE-POS
					     W42-ATTRIB4
					     W41-LENGTH
				       RETURNING WS-STATUS.

       RESTORE-ATTRIB.
	     MOVE 2000		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB4
					      W41-LENGTH
					RETURNING WS-STATUS.

       SCREEN-CONTENTS.
	    CALL "CBL_CLEAR_SCR" USING X"20" X"00"
				 RETURNING WS-STATUS.

       SAVE-SCREEN.
	     CALL "CBL_READ_SCR_CHATTRS" USING SAVE-POS
					       W43-SCREEN
					       W42-ATTRIB
					       W41-LENGTH
					 RETURNING WS-STATUS.

       SAVE-SCREEN-2.
	     CALL "CBL_READ_SCR_CHATTRS" USING SAVE-POS
					       W43-SCREEN2
					       W42-ATTRIB2
					       W41-LENGTH
					 RETURNING WS-STATUS.

       SAVE-SCREEN-3.
	     CALL "CBL_READ_SCR_CHATTRS" USING SAVE-POS
					       W43-SCREEN3
					       W42-ATTRIB3
					       W41-LENGTH
					 RETURNING WS-STATUS.

       RESTORE-SCREEN.
	     MOVE 2000		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_CHATTRS" USING SAVE-POS
						W43-SCREEN
						W42-ATTRIB
						W41-LENGTH
					  RETURNING WS-STATUS.

       RESTORE-SCREEN-2.
	     MOVE 2000		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_CHATTRS" USING SAVE-POS
						W43-SCREEN2
						W42-ATTRIB2
						W41-LENGTH
					  RETURNING WS-STATUS.

       RESTORE-SCREEN-3.
	     MOVE 2000		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_CHATTRS" USING SAVE-POS
						W43-SCREEN3
						W42-ATTRIB3
						W41-LENGTH
					  RETURNING WS-STATUS.

       POSITION-MOUSE.
	     MOVE 67		 TO MOUSE-FUNC.
	     CALL X"AF" USING MOUSE-FUNC MOUSE-DETAILS.
	     MOVE SCREEN-COL	 TO STORE-LIN
				    STORE-COL.
	     SUBTRACT 2 	 FROM STORE-LIN.
	     ADD STRING-LENGTH	 TO STORE-COL.
	   IF NOT (MOUSE-Y < TOP-ROW OR > BOTTOM-ROW)
	       IF NOT (MOUSE-X < STORE-LIN OR > STORE-COL)
		   IF MOUSE-Y = SCREEN-LIN
		       MOVE "Y"	 TO MOUSE-ENTER
		   ELSE
		       PERFORM CLEAR-BLOCK
		       MOVE MOUSE-Y
				 TO SCREEN-LIN
		       PERFORM SAVE-ATTR
		       PERFORM MARK-BLOCK.

       MOVE-BLOCK-UP.
	     PERFORM CLEAR-BLOCK.
	   IF SCREEN-LIN > TOP-ROW
	       SUBTRACT 1	 FROM SCREEN-LIN
	   ELSE
	       MOVE BOTTOM-ROW	 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       MOVE-BLOCK-DOWN.
	     PERFORM CLEAR-BLOCK.
	   IF SCREEN-LIN < BOTTOM-ROW
	       ADD 1		 TO SCREEN-LIN
	   ELSE
	       MOVE TOP-ROW	 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       CLEAR-BLOCK.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SCREEN-POSITION
					      ORIGINAL-VID
					      STRING-LENGTH.

       MARK-BLOCK.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SCREEN-POSITION
					      REVERSE-VID
					      STRING-LENGTH.

       SAVE-ATTR.
	     CALL "CBL_READ_SCR_ATTRS" USING SCREEN-POSITION
					     ORIGINAL-VID
					     STRING-LENGTH.
	     PERFORM SET-UP-REVERSE-VID
		     VARYING WS-S1 FROM 1 BY 1
		     UNTIL WS-S1 > STRING-LENGTH.

       SET-UP-REVERSE-VID.
	     MOVE ORIGINAL-CHAR (WS-S1)
				 TO W40-CHAR.
	     MOVE ZERO		 TO W40-N1 W40-N2.
	  IF W40-DEC > 127
	      SUBTRACT 128	 FROM W40-DEC
	      ADD 8		 TO W40-N2.
	  IF W40-DEC > 63
	      SUBTRACT 64	 FROM W40-DEC
	      ADD 4		 TO W40-N2.
	  IF W40-DEC > 31
	      SUBTRACT 32	 FROM W40-DEC
	      ADD 2		 TO W40-N2.
	  IF W40-DEC > 15
	      SUBTRACT 16	 FROM W40-DEC
	      ADD 1		 TO W40-N2.
	  IF W40-DEC > 7
	      SUBTRACT 8	 FROM W40-DEC
	      ADD 128		 TO W40-N1.
	  IF W40-DEC > 3
	      SUBTRACT 4	 FROM W40-DEC
	      ADD 64		 TO W40-N1.
	  IF W40-DEC > 1
	      SUBTRACT 2	 FROM W40-DEC
	      ADD 32		 TO W40-N1.
	  IF W40-DEC > 0
	      SUBTRACT 1	 FROM W40-DEC
	      ADD 16		 TO W40-N1.
	     ADD W40-N1 W40-N2	 GIVING W40-DEC.
	     MOVE W40-CHAR	 TO REVERSE-CHAR (WS-S1).

       SCREEN-SHADOW.
	     CALL "CBL_READ_SCR_ATTRS" USING SAVE-POS
					     W42-ATTRIB4
					     W41-LENGTH
				       RETURNING WS-STATUS.
	     ADD SHADE-COL SHADE-WIDTH
				 GIVING WS-S2.
	     ADD 1 WS-S2	 GIVING WS-S3.
	     ADD SHADE-LINES SHADE-ROW
				 GIVING WS-S4.
	     PERFORM SET-UP-SHADOW-VERT
		     VARYING WS-S1 FROM SHADE-ROW BY 1
		     UNTIL WS-S1 = WS-S4.
	     PERFORM SET-UP-SHADOW-HOR
		     VARYING WS-S2 FROM SHADE-COL BY 1
		     UNTIL WS-S2 > WS-S3.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB4
					      W41-LENGTH
					RETURNING WS-STATUS.

	SET-UP-SHADOW-VERT.
	     MOVE SHADE-CHAR	 TO W42-CHR (WS-S1, WS-S2)
				    W42-CHR (WS-S1, WS-S3).

	SET-UP-SHADOW-HOR.
	     MOVE SHADE-CHAR	 TO W42-CHR (WS-S1, WS-S2).

       MESSAGE-INST.
	     DISPLAY CLEAR-L25.
001020	     DISPLAY "Use " AT 2524 WITH BACKGROUND-COLOR 3
					 FOREGROUND-COLOR 1
		     " " WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
001030		     " to move the message window"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1.

       HIDE-THE-CURSOR.
	     MOVE 255		 TO CUR-ROW CUR-COL.
	     CALL "CBL_SET_CSR_POS" USING CUR-CONT
				    RETURNING WS-STATUS.

       DISPLAY-THE-CURSOR.
	     MOVE 25		 TO CUR-ROW.
	     MOVE 01		 TO CUR-COL.
	     CALL "CBL_SET_CSR_POS" USING CUR-CONT
				    RETURNING WS-STATUS.
      *
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      * 	  D I S P L A Y   E R R O R   M E S S A G E S
      *
       ERROR-MESSAGE   SECTION.
       ERROR-SETUP.
	     MOVE SPACES	 TO WS-ERR-MES.
	     STRING WS-ERR-STRING DELIMITED BY "  "
		    " - Press ANY key" DELIMITED SIZE
		    INTO WS-ERR-MES.

       ERROR-LENGTH.
	     MOVE SPACE 	 TO WS-OPTION
	     MOVE 48		 TO WS-S1.

      * 			 FIND THE LENGTH OF THE MESSAGE
       ERROR-LOOP.
	   IF WS-ERR-CHAR (WS-S1) = SPACE
	       SUBTRACT 1	 FROM WS-S1
	       GO TO ERROR-LOOP.
	     SUBTRACT WS-S1 FROM 48
				 GIVING WS-COUNT.
	   IF WS-COUNT < 3
	       GO TO ERROR-POS.
	     DIVIDE 2		 INTO WS-COUNT.
	     SUBTRACT WS-COUNT FROM 48
				 GIVING WS-S2.

      * 		   CENTRE THE MESSAGE IN THE DISPLAY WINDOW
       ERROR-CENTRE.
	     MOVE WS-ERR-CHAR (WS-S1)
				 TO WS-ERR-CHAR (WS-S2).
	     MOVE SPACE 	 TO WS-ERR-CHAR (WS-S1).
	   IF WS-S1 > 1
	       SUBTRACT 1	 FROM WS-S1 WS-S2
	       GO TO ERROR-CENTRE.

       ERROR-POS.
	     MOVE 13		 TO SLIN.
	     MOVE 64		 TO SCOL.
	     PERFORM SAVE-SCREEN.

       ERROR-DISPLAY.
	     ADD 1 SLIN 	 GIVING SHADE-ROW.
	     MOVE 18		 TO SHADE-COL.
	     MOVE 48		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY ERROR-LINE AT LINE SLIN.
	     PERFORM SCREEN-SHADOW.
	     PERFORM MESSAGE-INST.

       ERROR-REPLY.
	     ADD 1		 TO SLIN.
	     PERFORM HIDE-THE-CURSOR.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		      PERFORM RESTORE-SCREEN
		      IF SLIN > 3
			  SUBTRACT 2 FROM SLIN
		      ELSE
			  MOVE 2     TO SLIN
		      END-IF
		      GO TO ERROR-DISPLAY
		 WHEN DOWN-KEY
		      PERFORM RESTORE-SCREEN
		      IF SLIN > 21
			  MOVE 21    TO SLIN
		      END-IF
		      GO TO ERROR-DISPLAY
		 WHEN ENTER-KEY  GO TO ERROR-END
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO ERROR-REPLY
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.

       ERROR-END.
	     PERFORM DISPLAY-THE-CURSOR.
	     PERFORM RESTORE-SCREEN.

       ERROR-EXIT.
	     EXIT.
      *
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      * 	  D I S P L A Y   O P T I O N	M E S S A G E S
      *
       OPT-MESSAGE   SECTION.
       OPT-CURSOR.
	     MOVE 13		 TO SLIN.

       OPT-SETUP.
	     MOVE 64		 TO SCOL.
	     MOVE 48		 TO WS-S1.

      * 			 FIND THE LENGTH OF THE MESSAGE
       OPT-LOOP.
	   IF WS-ERR-CHAR (WS-S1) = SPACE
	       SUBTRACT 1	 FROM WS-S1
	       GO TO OPT-LOOP.
	     SUBTRACT WS-S1 FROM 48
				 GIVING WS-COUNT.
	   IF WS-COUNT < 3
	       MOVE WS-S1	 TO WS-COUNT
	       GO TO OPT-POS.
	     DIVIDE 2		 INTO WS-COUNT.
	     SUBTRACT WS-COUNT FROM 48
				 GIVING WS-S2.
	     MOVE WS-S2 	 TO WS-COUNT.

      * 		   CENTRE THE MESSAGE IN THE DISPLAY WINDOW
       OPT-CENTRE.
	     MOVE WS-ERR-CHAR (WS-S1)
				 TO WS-ERR-CHAR (WS-S2).
	     MOVE SPACE 	 TO WS-ERR-CHAR (WS-S1).
	   IF WS-S1 > 1
	       SUBTRACT 1	 FROM WS-S1 WS-S2
	       GO TO OPT-CENTRE.

       OPT-POS.
	     PERFORM SAVE-SCREEN.

       OPT-DISPLAY.
	     ADD 1 SLIN 	 GIVING SHADE-ROW.
	     MOVE 18		 TO SHADE-COL.
	     MOVE 48		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY OPT-LINE AT LINE SLIN.
	     PERFORM SCREEN-SHADOW.
	     PERFORM MESSAGE-INST.

       OPT-REPLY.
	     ADD 1		 TO SLIN.
	   IF WS-ERR-CHAR (WS-COUNT) = "]"
	       SUBTRACT 1	 FROM WS-COUNT
	       ADD 16 WS-COUNT	 GIVING SCOL
	       DISPLAY WS-ERR-CHAR (WS-COUNT) AT SCREEN-POS
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       ADD 1		 TO WS-COUNT.
	     PERFORM HIDE-THE-CURSOR.

       OPT-ACCEPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		      PERFORM RESTORE-SCREEN
		      IF SLIN > 3
			  SUBTRACT 2 FROM SLIN
		      ELSE
			  MOVE 2     TO SLIN
		      END-IF
		      GO TO OPT-DISPLAY
		 WHEN DOWN-KEY
		      PERFORM RESTORE-SCREEN
		      IF SLIN > 21
			  MOVE 21    TO SLIN
		      END-IF
		      GO TO OPT-DISPLAY
		 WHEN ENTER-KEY  GO TO OPT-END
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO OPT-REPLY
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.

       OPT-END.
	     PERFORM DISPLAY-THE-CURSOR.
	     PERFORM RESTORE-SCREEN.

       OPT-EXIT.
	     EXIT.

       CHECK-CORRECT   SECTION.
       CHECK-POS.
	     MOVE 13		 TO SLIN.

       CHECK-SETUP.
	     MOVE SPACE 	 TO WS-OPTION.
	     MOVE 54		 TO SCOL.
	     PERFORM SAVE-SCREEN.

       CHECK-DISPLAY.
	     ADD 1 SLIN 	 GIVING SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY S99 AT LINE SLIN.
	     PERFORM SCREEN-SHADOW.
	     PERFORM MESSAGE-INST.

       CHECK-REPLY.
	     ADD 1		 TO SLIN.
	     PERFORM HIDE-THE-CURSOR.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		      PERFORM RESTORE-SCREEN
		      IF SLIN > 3
			  SUBTRACT 2 FROM SLIN
		      ELSE
			  MOVE 2     TO SLIN
		      END-IF
		      GO TO CHECK-DISPLAY
		 WHEN DOWN-KEY
		      PERFORM RESTORE-SCREEN
		      IF SLIN > 21
			  MOVE 21    TO SLIN
		      END-IF
		      GO TO CHECK-DISPLAY
		 WHEN ENTER-KEY  GO TO CHECK-UPPER
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CHECK-REPLY
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION.

       CHECK-UPPER.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     PERFORM DISPLAY-THE-CURSOR.
	     PERFORM RESTORE-SCREEN.

       CHECK-EXIT.
	     EXIT.

      *
      *    ****   G E T   P R I C E   R E C O R D   U S I N G	C O D E
      *
015170 CA000         SECTION.
015180 CA00.
013310	     MOVE 18		 TO W50-LENGTH.

       CA05.
013320	     MOVE W10-EXT-ITEM	 TO W50-DATA.
	   IF W50-1 = SPACE
	       MOVE W50-17	 TO W10-EXT-ITEM
	       GO TO CA05.
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-EXT-ITEM	 TO W10-EXT-ITEM.

       CA06.
013380	     MOVE W50-EXT-ITEM	 TO PRC-CODE.

       CA10.
000920	     PERFORM READ-PRICES THRU READ-PRICES-EXIT.
	   IF WS-STATUS = "23"
	       MOVE W50-EXT-ITEM TO W10-EXT-CODE2
	       PERFORM CA200
	       IF NOT (PRC-XREF = W10-EXT-ITEM)
		   MOVE 22	 TO WS-F-ERROR
	       ELSE
		   MOVE PRC-CODE TO W10-EXT-ITEM.
	   IF WS-F-ERROR = 0
               PERFORM CA500.

       CA99.
             EXIT.
      *
      *    ****   G E T   P R I C E   R E C O R D   U S I N G	X R E F
      *
015170 CA200         SECTION.
015180 CA201.
013310	     MOVE 18		 TO W50-LENGTH.

       CA205.
013320	     MOVE W10-EXT-CODE2	 TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-17	 TO W10-EXT-CODE2
               GO TO CA205.      
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-EXT-ITEM	 TO W10-EXT-CODE2.

       CA206.
013380	     MOVE W50-EXT-ITEM	  TO PRC-XREF.

       CA210.
	     PERFORM START-AT-PRICES-XREF THRU READ-PRICES-EXIT.
           IF WS-F-ERROR = 22
               GO TO CA299.
	     PERFORM READ-PRICES-NEXT THRU READ-PRICES-EXIT.
	   IF NOT (PRC-XREF = W50-EXT-ITEM)
	       GO TO CA299.
             PERFORM CA500.

       CA299.
             EXIT.
      *
      *    ****    Calculate the inclusive prices
      *
       CA500         SECTION.
       CA505.
	     MOVE 1		 TO W50-S1.

       CA510.
	     MOVE PRC-UNT(W50-S1)
				 TO W10-UNT(W50-S1).
	     MOVE PRC-PR(W50-S1)
				 TO W10-PR(W50-S1).
	     MOVE PRC-PRC(W50-S1)
				 TO W10-SL(W50-S1).

	     MOVE ZERO		 TO W05-RATE.
	   IF PRC-TAX > 0
	       MOVE PRC-VAT	 TO W05-RATE.
	     COMPUTE W10-VSL(W50-S1) ROUNDED
		   = W10-SL(W50-S1) + (W10-SL(W50-S1) * W05-RTE).
	     COMPUTE W10-VT(W50-S1) = W10-VSL(W50-S1) - W10-SL(W50-S1).
	   IF W50-S1 < 4
	       ADD 1		 TO W50-S1
	       GO TO CA510.

015650 CA599.
015660       EXIT.
      /
      *    ****    S T O C K   L O O K U P   R O U T I N E
      *
      *            PERFORMED FROM AA50 
      *            (P R O C E S S   F U N C T I O N   K E Y S)
      *
000010 PRICES-LOOKUP		       SECTION.
000030 PRICES-LUP-SCREEN.
000040       DISPLAY CLR-SCREEN. 
000050	     DISPLAY S-STK.
000090	   IF W10-ETYPE = "B"
000100	       DISPLAY S-STKB
000080	   ELSE
000120	   IF W10-ETYPE = "D"
000130	       DISPLAY S-STKD
	   ELSE
	   IF W10-ETYPE = "2"
	       DISPLAY S-STKD2
	   ELSE
	   IF W10-ETYPE = "P"
	       DISPLAY S-STKDP
	   ELSE
	       DISPLAY S-STKA.
             MOVE SPACE          TO WS-OPTION.

       PRICES-LUP-CLR-SCREEN.
000140       MOVE 0601           TO CPOS.
000150	     PERFORM PRICES-CRT-EDGE UNTIL CLIN = 22.
000160       MOVE SPACES         TO W10-ACODE W10-DESC 
000170				    W10-CODE2 W10-CODE1
				    W10-DESC2 W10-DEPART.

000200 PRICES-LUP-DATA.
	   IF W10-ETYPE = "Z"
               NEXT SENTENCE
           ELSE
000210	   IF W10-ETYPE = "I"
000220         ACCEPT SA-STK
000260	   ELSE
000270	   IF W10-ETYPE = "B"
000280	       ACCEPT SA-STKB
000230	   ELSE
	   IF W10-ETYPE = "D"
000300	       ACCEPT SA-STKD
	   ELSE
	   IF W10-ETYPE = "2"
	       ACCEPT SA-STKD2
	   ELSE
	       ACCEPT SA-STKDP.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
000140		     MOVE SPACES TO W10-EXT-ITEM
000150		     GO TO PRICES-RESET-LIN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO PRICES-LUP-DATA.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
000340	     DISPLAY CLEAR-L25.
000350	     MOVE 6		 TO CLIN.
000360	     MOVE 1		 TO W75-S W75-S1.

000380 PRICES-LUP-TAB-LOOP.
000390	     MOVE SPACES	 TO W75-EXT-CODE(W75-S1).
000400     IF W75-S1 < 18
000410	       ADD 1		 TO W75-S1
000420	       GO TO PRICES-LUP-TAB-LOOP.
000430	     MOVE ZERO		 TO W75-S1.

000450 PRICES-LUP-ENTRY-TYPE.
000460	   IF W10-ETYPE = "I" OR "Z"
000470	       IF W10-EXT-ITEM = SPACES
000480		   MOVE X"1F"	 TO W10-EXT-ITEM.
000520	   IF W10-ETYPE = "B"
000530	       IF W10-EXT-CODE2 = SPACES
000540		   MOVE X"1F"	 TO W10-EXT-CODE2.
000550	   IF W10-ETYPE = "D"
000560         IF W10-DESC = SPACES
000570		   MOVE X"1F"	 TO W10-DESC.
000550	   IF W10-ETYPE = "2"
000560	       IF W10-DESC2 = SPACES
000570		   MOVE X"1F"	 TO W10-DESC2.
000550	   IF W10-ETYPE = "P"
000560	       IF W10-DEPART = SPACES
000570		   MOVE X"1F"	 TO W10-DEPART.
000580	   IF W10-ETYPE = "I" OR "Z"
000590         PERFORM CA00 THRU CA06.
000620	   IF W10-ETYPE = "B"
000630         PERFORM CA201 THRU CA205.
000640	   IF W10-ETYPE = "D"
000650	       MOVE W10-DESC	 TO W50-DATA
000660	       MOVE 30		 TO W50-LENGTH
	       CALL "CBL_TOUPPER" USING W50-30
				  BY VALUE W50-LENGTH
				  RETURNING WS-STATUS
000720	       MOVE W50-30	 TO W10-DESC.
000730	   IF W10-ETYPE = "I" OR "Z"
000740	       MOVE W10-EXT-ITEM TO PRC-CODE
000750	       PERFORM START-AT-PRICES-CODE THRU READ-PRICES-EXIT
000760     ELSE
000810	   IF W10-ETYPE = "B"
000820	       MOVE W10-CODE2	 TO PRC-XREF
000830	       PERFORM START-AT-PRICES-XREF THRU READ-PRICES-EXIT
000840     ELSE
000850	   IF W10-ETYPE = "D"
000860	       MOVE W10-DESC	 TO PRC-DESC
000870	       PERFORM START-AT-PRICES-DESC THRU READ-PRICES-EXIT
000840     ELSE
000850	   IF W10-ETYPE = "2"
000860	       MOVE W10-DESC2	 TO PRC-DESC2
000870	       PERFORM START-AT-PRICES-DESC2 THRU READ-PRICES-EXIT.
000880     IF WS-F-ERROR = 22
000890	       GO TO PRICES-NO-RECORD.

000910 PRICES-GET-PRICES-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO PRICES-SKIP-PRICES-READ.
000920	     PERFORM READ-PRICES-NEXT THRU READ-PRICES-EXIT.
	   IF PRC-CODE = SPACES
	       GO TO PRICES-GET-PRICES-REC.
000930     IF WS-F-ERROR = 22
000940	       SUBTRACT 1	 FROM W75-S
000950	       GO TO PRICES-FULL-SCREEN.

       PRICES-SKIP-PRICES-READ.
000960	     MOVE PRC-CODE	 TO W75-EXT-CODE(W75-S).
	     MOVE PRC-PER	 TO W10-PER.
	     MOVE PRC-PRICE	 TO W10-SELL W75-SELL(W75-S).
	     MOVE PRC-QUANT	 TO W10-QUANT.

       PRICES-DISP-DET.
000970	     DISPLAY (CLIN, 2) PRC-CODE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     MOVE PRC-DESC	 TO W10-DESC.
000980	     DISPLAY ( , 21) W10-DESC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     MOVE PRC-QUANT	 TO W100-NO3.
	     DISPLAY ( , 52) W100-S9
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
001010	     MOVE PRC-PRICE	 TO W100-S6V2.
001020	     DISPLAY ( , 61) W100-PRICE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	   IF PRC-TAX NOT = ZERO
	       MOVE PRC-VAT	 TO W05-RATE
	       COMPUTE W10-VSELL ROUNDED
		     = W10-SELL + (W10-SELL * W05-RTE)
	   END-IF
	     MOVE W10-VSELL	 TO W75-VSELL (W75-S)
001030	     MOVE W10-VSELL	 TO W100-S6V2
001040	     DISPLAY ( , 71) W100-PRICE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
001050     IF CLIN < 21
001060	       ADD 1		 TO CLIN W75-S
001070	       GO TO PRICES-GET-PRICES-REC.

001090 PRICES-FULL-SCREEN.
001020	     DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
001030		     " /" WITH FOREGROUND-COLOR 7
		     "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 14
		     "," WITH FOREGROUND-COLOR 7
001040		     "ENTER" WITH FOREGROUND-COLOR 14
001050		     " select," WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 14
		     "ape" WITH FOREGROUND-COLOR 7.

       PRICES-POSITION-CURSOR.
	     PERFORM SAVE-SCREEN-2.
001220	     MOVE 0		 TO W75-S1.
001230       MOVE 0601           TO CPOS.

001250 PRICES-CRT-ATTRIB.
	     MOVE 1920		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
001290	     MOVE 2000		 TO W41-LENGTH.

001310 PRICES-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY SPACE AT 2540
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
001320		     GO TO PRICES-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO PRICES-CHK-NO-KEYS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO PRICES-CHK-SELECT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO PRICES-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO PRICES-RESET-LIN
		 WHEN PAGE-DOWN
001340		     GO TO PRICES-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO PRICES-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO PRICES-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO PRICES-GET-USER-OPT.

       PRICES-GET-NEXT-PAGE.
001550     IF W75-S < 16
001560	       GO TO PRICES-GET-USER-OPT.

       PRICES-NEW-PAGE.
001570       MOVE 0601           TO CPOS.
001580	     PERFORM PRICES-CRT-EDGE UNTIL CLIN = 22.
001590       MOVE 6              TO CLIN.
001600       MOVE LOW-VALUES TO W75-KEYS.
001610	     MOVE 1		 TO W75-S W75-S1.
001620	     PERFORM PRICES-LUP-TAB-LOOP.
001630	     GO TO PRICES-GET-PRICES-REC.

       PRICES-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       PRICES-GET-PREV-LOOP.
	     PERFORM READ-PRICES-PREV THRU READ-PRICES-EXIT.
           IF WS-F-ERROR = 22
	       MOVE SPACE	 TO WS-OPTION
	       GO TO PRICES-NEW-PAGE.
           IF WS-IXS1 < 32
               ADD 1             TO WS-IXS1
	       GO TO PRICES-GET-PREV-LOOP.
	     GO TO PRICES-NEW-PAGE.

       PRICES-SAVE-FULL-SCREEN.
	     DISPLAY " " AT 2572 WITH BACKGROUND-COLOR 3
				      FOREGROUND-COLOR 1.
001990	     PERFORM PRICES-CRT-ATTRIB.
001180       MOVE 0 TO W44-FUNCTION.
001190	     CALL X"B7" USING W44-FUNCTION W41-SAVE W43-SCREEN2.
           IF W75-S1 > 0
	       GO TO PRICES-HIGHLIGHT.
	     GO TO PRICES-GET-USER-OPT.

001650 PRICES-NO-RECORD.
001660	     DISPLAY "No record - " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001670		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001680		     "ext or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001690		     "Q" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001700		     "uit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

001720 PRICES-GET-USER-OPT-2.
001730	     ACCEPT WS-OPTION AT 2538
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
001790	   IF NOT (WS-OPTION = "N" OR "Q")
001800	       GO TO PRICES-GET-USER-OPT-2.
001810     IF WS-OPTION = "N"
001820	       GO TO PRICES-LUP-SCREEN.
001830	     MOVE SPACES	 TO W10-EXT-ITEM.
001840	     GO TO PRICES-RESET-LIN.

001860 PRICES-ADJ-NO-KEYS.
001870     IF W75-S1 > 1
001880         SUBTRACT 1        FROM W75-S1 CLIN.
001890	     GO TO PRICES-HIGHLIGHT.

001910 PRICES-CHK-NO-KEYS.
001920     IF W75-S1 = ZERO
001930         MOVE 1            TO W75-S1
001940	       GO TO PRICES-HIGHLIGHT.
001950     IF W75-S1 < W75-S
001960         ADD 1             TO W75-S1 CLIN.

001980 PRICES-HIGHLIGHT.
001990	     PERFORM PRICES-CRT-ATTRIB.
002000       DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
002010	     GO TO PRICES-GET-USER-OPT.

002030 PRICES-CHK-SELECT.
002040     IF W75-S1 = ZERO
002050	       GO TO PRICES-GET-USER-OPT.
002060	     MOVE CSTART	 TO WS1-CURSOR.
002070	     MOVE W75-EXT-CODE(W75-S1)
				 TO W10-EXT-ITEM.
002080	     PERFORM READ-PRICES THRU READ-PRICES-EXIT.
002090	     MOVE ZERO		 TO COL-AND-LINE.
002100	     GO TO PRICES-RESET-LIN.

002120 PRICES-CRT-EDGE.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     "                  ³                              ³
      -		     "        ³         ³         "
		      WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
002140	     ADD 1		 TO CLIN.

002160 PRICES-RESET-LIN.
	     MOVE SLIN		 TO LIN.

002160 PRICES-LOOKUP-EXIT.
002170       EXIT.
      *
      *    ****    E X P O R T	 T H E	 D E T A I L S
      *
       EXPORT-PRICES	       SECTION.
       EXPORT-INIT.
	     PERFORM SAVE-SCREEN-2.
	     INITIALIZE W80-FIELDS.
	     MOVE 01		 TO W80-FNO(1).
	     MOVE 02		 TO W80-FNO(2).
	     MOVE 03		 TO W80-FNO(3).
	     MOVE 04		 TO W80-FNO(4).
	     MOVE 05		 TO W80-FNO(5).
	     MOVE 06		 TO W80-FNO(6).
	     MOVE 07		 TO W80-FNO(7).
	     MOVE 08		 TO W80-FNO(8).
	     MOVE 09		 TO W80-FNO(9).
	     MOVE 10		 TO W80-FNO(10).
	     MOVE 11		 TO W80-FNO(11).
	     MOVE 12		 TO W80-FNO(12).
	     PERFORM AA100.
	     MOVE X"1F" 	 TO PRC-CODE.
	     PERFORM START-AT-PRICES-CODE THRU READ-PRICES-EXIT.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1124
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "³        " AT 1224
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "EXPORTING DATA        "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "³ " AT 1324
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "Item code " WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
		     "                   ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     DISPLAY "À" AT 1424 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				      BACKGROUND-COLOR 3
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 3.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     PERFORM READ-PRICES-NEXT THRU READ-PRICES-EXIT.

       EXPORT-LOOP.
	     PERFORM READ-PRICES-NEXT THRU READ-PRICES-EXIT.
000930	   IF WS-F-ERROR = 22
	       PERFORM RESTORE-SCREEN-2
	       GO TO EXPORT-END.
	     DISPLAY PRC-CODE AT 1336
		     WITH FOREGROUND-COLOR 5 HIGHLIGHT
			  BACKGROUND-COLOR 3.
	   IF PRC-TAX = ZERO
	       MOVE ZERO	 TO W10-VAT
	   ELSE
	       COMPUTE W10-VAT = (PRC-PRICE * PRC-VAT) / 100.000.
	     ADD PRC-PRICE W10-VAT
				 GIVING W10-VSELL.
             PERFORM AA400.
	     MOVE W78-EXP-DATA	 TO EXP-REC1.
	     PERFORM WRITE-STKDET THRU WRITE-STKDET-EXIT.
	     GO TO EXPORT-LOOP.

       EXPORT-END.
             CLOSE STKDET.
029700	     MOVE "Export complete - Press ANY key to continue"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.

       EXPORT-EXIT.
	     EXIT.
      /
019370 ZA000-INIT	       SECTION 8.
019380 ZA000-OPEN.
	     ACCEPT W25-FACT FROM DATE.
	     MOVE W25-YY	 TO W12-YEAR.
	     MOVE W25-MM	 TO W12-MONTH.
	     MOVE W25-DD	 TO W12-DAY.
	   IF W25-YY < 50
	       MOVE 20		 TO W12-CENT
	   ELSE
	       MOVE 19		 TO W12-CENT.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
003200	     MOVE W12-DAY	 TO W12-DD.
003210	     MOVE W12-MONTH	 TO W12-MM.
	     MOVE W12-CENT	 TO W12-CC.
003220	     MOVE W12-YEAR	 TO W12-YY.
	     PERFORM ZA55 THRU ZA60.
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
	       GO TO ZA05.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    F I E L D   S E P A R A T O R
      *
	     MOVE ","		 TO W50-EXP.

       ZA05.
044300	     OPEN INPUT PRICES.
044330	   IF WS-STATUS NOT = "00"
050210	       PERFORM OPEN-ERROR.
	     MOVE SPACES	 TO PRC-CODE.
	     PERFORM READ-PRICES THRU READ-PRICES-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "Error on Prices file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       STOP RUN.
	   IF PRC-EXPIRE < W12-T-YMD
	       MOVE "Price list out of date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       STOP RUN.
	     MOVE PRC-COMP	 TO WS-COMPANY.
	     MOVE 40		 TO WS-S1.

       ZA10.
           IF WS-COMPCHR (WS-S1) = SPACE
               MOVE WS-G8        TO WS-COMPCHR (WS-S1)
               IF WS-S1 > 1
                   SUBTRACT 1    FROM WS-S1
		   GO TO ZA10.
             MOVE WS-COMPANY     TO WS-TOP-COMP.
	     MOVE PRC-EXPIRE	 TO W20-DTE.
	     MOVE W55-MONTH(W20-MONTH)
				 TO W60-MMYY.
	     MOVE W20-CCYY	 TO W60-CCYY.
	     MOVE 9		 TO WS-S1.
	     MOVE 1		 TO WS-S2.

       ZA15.
	   IF W60-MY(WS-S1) = SPACE
	       SUBTRACT 1	 FROM WS-S1
	       GO TO ZA15.
	     ADD 2		 TO WS-S1.

       ZA20.
	     MOVE W60-CY(WS-S2)  TO W60-MY(WS-S1).
	   IF WS-S2 < 4
	       ADD 1		 TO WS-S1 WS-S2
	       GO TO ZA20.

	     MOVE W20-DAY	 TO W65-DAY.
	     MOVE W60-MMYY	 TO W65-MMYY.
019900	     GO TO ZA999-EXIT.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870       MOVE SPACES         TO WS-MID-LNE.

041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
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

019990 ZA999-EXIT.
020000       EXIT.
      /
020010 ZB000-ERROR	       SECTION 8.
      
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
      
041560 DISPLAY-FILE-NAME.
041780	     MOVE W02-PRICES	 TO WS-FILE.
041790	     MOVE ZERO		 TO WS-KEY.
	     MOVE PRC-CODE	 TO WS-KEYX.
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
