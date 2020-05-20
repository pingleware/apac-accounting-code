      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *    *    ******     *     *****  ****** **   **  *****  ******  *
      *   ***   **   **   ***   **   **   **   ***  ** **   **   **    *
      *  ** **  **   **  ** **  **        **   **** ** **        **    *
      * **   ** ******  **   ** **        **   ** ****  *****    **    *
      * ******* **      ******* **        **   **  ***      **   **    *
      * **   ** **      **   ** **   **   **   **   ** **   **   **    *
      * **   ** **      **   **  *****  ****** **   **  *****    **    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     A P A C   I N I T I A L I Z E   P R O G R A M              *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
      *                                                                *
      *    Version 8 has introduced editable file paths and allows for *
      *    the sharing of files between two businesses.                *
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
      *  March 2010	- Include facility to print to a USB printer   *
      * 		  via the Windows print using the relavent     *
      * 		  print API's supplied with MF COBOL. Allow    *
      * 		  BMP files in APACFILE (INVBMP & STMBMP).     *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     APACINST.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   APRIL 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2011
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
000080                 CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY APCCOM.SL.

000220 COPY AUDIT.SL.

000240 COPY CONTROL.SL.

000180 COPY CHEQUE.SL.

000200 COPY DEPART.SL.

000120 COPY PARAM.SL.

000140 COPY SYSTEM.SL.

000140 COPY SYSUSE.SL.

       COPY TXTRAN.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY APCCOM.FD.

000390 COPY AUDIT.IFD.

000290 COPY CONTROL.IFD.

000180 COPY CHEQUE.FD.

000200 COPY DEPART.GFD.

000290 COPY PARAM.IFD.

000310 COPY SYSTEM.FD.

000310 COPY SYSUSE.FD.

000390 COPY TXTRAN.IFD.

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000440 77  WS-S1	    PIC  9(04)	   COMP-5.
000450 77  WS-S2	    PIC  9(04)	   COMP-5.
000460 77  WS-S3	    PIC  9(04)	   COMP-5.
000470 77  WS-S4	    PIC  9(04)	   COMP-5.
000480 77  WS-PARKEY	    PIC  9(04)	  COMP-5.
000520 77  WS-AUDKEY	    PIC  9(04)	  COMP-5.
000530 77  WS-NETKEY	    PIC  9(04)	  COMP-5.
000540 77  WS-SECKEY	    PIC  9(04)	  COMP-5.
000550 77  WS-GST-RATE	    PIC  9(03)V99  COMP-3.
000560 77  WS-LINES	    PIC  9(02).
000570 77  WS-PAGE	    PIC  9(04)	  COMP-5.
000580 77  WS-OPTION	    PIC  X(01).
000590 77  WS-ERROR	    PIC  9(01).
000600 77  WS-DEB	    PIC  9(05).
000610 77  WS-CON	    PIC  9(05).
000620 77  WS-NUM	    PIC  Z(04)9.
000630 77  WS-MES	    PIC  X(11) VALUE "Initialized".
       77  TODAY-DDMMYY     PIC  9(08) COMP-5.
      /
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
002450     03  WS-MID-LNE.
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

000670 COPY WS.WS.

000690 01  W01-CONTROL.
000700     03  W01-COMP.
000710         05  W01-DEC     PIC  9(02) COMP-X.
000720     03  W01-DISP REDEFINES W01-COMP.
000730         05  W01-CHAR    PIC  X(01).
000740     03  W01-NUL         PIC  X(01).
000750     03  W01-ESC         PIC  X(01).
000760     03  W01-EXP         PIC  X(01).
000770     03  W01-ECAN        PIC  X(01).
000780     03  W01-NORM        PIC  X(01).
000790     03  W01-COND        PIC  X(01).
000800     03  W01-DBLE        PIC  X(01).
000810     03  W01-DCAN        PIC  X(01).
000820     03  W01-8LIN        PIC  X(01).
000830     03  W01-6LIN        PIC  X(01).

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

000840 01  W02-FID.

       COPY APACFIDS.ID.

       COPY AUDIT.ID.

       COPY CONTROL.ID.

000180 COPY CHEQUE.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

       COPY SYSTEM.ID.

       COPY SYSUSE.ID.

000390 COPY TXTRAN.ID.

       01  W10-PRINTERS.
      * 				    CONDENSED PRINT
	   03  W09-CONP.
000230	       05  W10-CONP    PIC  X(04).
	       05  FILLER      PIC  X(02)   VALUE X"0000".
      *                                     CANCEL CONDENSED PRINT
	   03  W09-NORP.
000240	       05  W10-NORP    PIC  X(04).
	       05  FILLER      PIC  X(02)   VALUE X"0000".
      *                                     EXPANDED PRINT
	   03  W09-EXPP.
000250	       05  W10-EXPP    PIC  X(04).
	       05  FILLER      PIC  X(02)   VALUE X"0000".
      *                                     CANCEL EXPANDED PRINT
	   03  W09-ECAN.
000260	       05  W10-ECAN    PIC  X(04).
	       05  FILLER      PIC  X(02)   VALUE X"0000".
      *                                     SET SPACING TO 8 LPI 
	   03  W09-8LPI.
000270	       05  W10-8LPI    PIC  X(04).
	       05  FILLER      PIC  X(02)   VALUE X"0000".
      *                                     SET SPACING TO 6 LPI 
	   03  W09-6LPI.
000280	       05  W10-6LPI    PIC  X(04).
	       05  FILLER      PIC  X(02)   VALUE X"0000".
      *                                     PRINT 10 CHARS PER INCH
	   03  W09-10CPI.
000290	       05  W10-10CPI   PIC  X(04).
	       05  FILLER      PIC  X(02)   VALUE X"0000".
      *                                     PRINT 12 CHARS PER INCH
	   03  W09-12CPI.
000300	       05  W10-12CPI   PIC  X(04).
	       05  FILLER      PIC  X(02)   VALUE X"0000".
      *                                     PRINT 17 CHARS PER INCH
	   03  W09-17CPI.
000310	       05  W10-17CPI   PIC  X(04).
	       05  FILLER      PIC  X(02)   VALUE X"0000".

       01  W12-TODAY.
	   03  W12-TODAY-DDMMYY  PIC 9(08).
	   03  W12-DATE REDEFINES W12-TODAY-DDMMYY.
	       05  W12-DAY	 PIC 9(02).
	       05  W12-MONTH	 PIC 9(02).
	       05  W12-CENT	 PIC 9(02).
	       05  W12-YEAR	 PIC 9(02).

       COPY FUNCTION.WS.

       COPY W40.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
001070 SCREEN SECTION.

       COPY BLANK.CRT.

001140 01  S01.
	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
                                 "INITIALIZE APAC SYSTEM FILES".
001150	   03  LINE  4 COLUMN  4 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
001160			     " Please be patient - Initializing files ".
001170	   03  LINE  6 COLUMN  4 PIC X(18) USING WS-PARID.
001180	   03  LINE  8 COLUMN  4 PIC X(20) USING W02-NETWORK.
001190	   03  LINE 10 COLUMN  4 PIC X(19) USING W02-DEPART.
001200	   03  LINE 12 COLUMN  4 PIC X(19) USING W02-AUDITF.
	   03  LINE 14 COLUMN  4 PIC X(19) USING W02-TXTRAN.
	   03  LINE 16 COLUMN  4 PIC X(19) USING W02-CHEQUE.
	   03  LINE 18 COLUMN  4 PIC X(19) USING W02-SYSUSER.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001240 AA000-MAIN              SECTION.
001250 AA00.
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 AA02.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO AA02.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(16) WS-TCHR(41)
021940				    WS-TCHR(64) WS-TCHR(71).
021950	     MOVE WS-G3 	 TO WS-MCHR(16) WS-MCHR(41)
021960				    WS-MCHR(64) WS-MCHR(71)
				    WS-MCHR(1)	WS-MCHR(80) .
021970	     MOVE WS-G4 	 TO WS-BCHR(16) WS-BCHR(41)
021980				    WS-BCHR(64) WS-BCHR(71).
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY
				    W12-TODAY-DDMMYY.
001260       DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
	     MOVE "PARAM"	 TO AFID-KEY.

       AA03-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO AA03-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       AA03-READ-APACFIDS-EXIT.
	     EXIT.

       AA03A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
      *    *************************************************************
      *    **** 		A U D I T   F I L E		    ****
      *    *************************************************************
	     MOVE "AUDITF"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-AUDITF.
      *    *************************************************************
      *    **** 	       C H E Q U E   F I L E		    ****
      *    *************************************************************
	     MOVE "CHEQUE"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CHEQUE.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	       S Y S T E M   F I L E		    ****
      *    *************************************************************
	     MOVE "SYSTEM"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SYSTEM.
      *    *************************************************************
      *    **** 		U S E R S   F I L E		    ****
      *    *************************************************************
	     MOVE "SYSUSE"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SYSUSER.
      *    *************************************************************
      *    **** 	       V . A . T .   F I L E		    ****
      *    *************************************************************
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
001280	     OPEN I-O SECUR.
001290	     MOVE 2		 TO WS-SECKEY.
	     OPEN I-O APCCOM.
	   IF WS-STATUS NOT = "00"
	       OPEN OUTPUT APCCOM
	       INITIALIZE COM-RECORD
	       MOVE "APC"	 TO COM-CODE
	       WRITE COM-RECORD
	       CLOSE APCCOM
	       OPEN I-O APCCOM.
001300       READ SECUR WAIT.
	     MOVE WS-SYS-ID	 TO COM-CODE.
	     READ APCCOM.
	     MOVE SEC-COMP	 TO COM-NAME.
	     REWRITE COM-RECORD.
	     CLOSE APCCOM.
001390	     DISPLAY S01.
001420 AA05.
001430       PERFORM AC000-PARAM.
001460       PERFORM AD000-CONTROL.
001470       PERFORM AE000-DEPART.
001490       PERFORM AG000-AUDIT.
             PERFORM AF000-TXTRAN.
             PERFORM AH000-CHEQUE.
	     PERFORM AI000-SYSUSE.
001510       GO TO AZ000-EOJ.

       COPY FUNCTION.SCR.

       COPY OPTION.CRT.

       COPY ERROR.SCR.

      /
001520 AC000-PARAM             SECTION.
001530 AC000-INIT.
001540	   IF LS0-DTP = 2 OR LS0-STP = 2 OR LS0-CRP = 2 OR
001550	      LS0-GLP = 2 OR LS0-HPD = 2 OR
001560	      LS0-JCP = 2 OR LS0-VHP = 2
001570         OPEN I-O PARAM
001580	       DISPLAY "Updated" AT 0633
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 5
001590         GO TO AC999-EXIT.
      *
      *    ****   PRINTER CONTROL CHARACTERS
      *
001600       MOVE ZERO           TO W01-DEC.
001610       MOVE W01-CHAR       TO W01-NUL.
001620       MOVE 27             TO W01-DEC.
001630       MOVE W01-CHAR       TO W01-ESC.
001640       MOVE 15             TO W01-DEC.
001650       MOVE W01-CHAR       TO W01-COND.
001660       MOVE 18             TO W01-DEC.
001670       MOVE W01-CHAR       TO W01-NORM.
001680       MOVE 14             TO W01-DEC.
001690       MOVE W01-CHAR       TO W01-EXP.
001700       MOVE 20             TO W01-DEC.
001710       MOVE W01-CHAR       TO W01-ECAN.
001720       MOVE 69             TO W01-DEC.
001730       MOVE W01-CHAR       TO W01-DBLE.
001740       MOVE 70             TO W01-DEC.
001750       MOVE W01-CHAR       TO W01-DCAN.
001760       MOVE "0"            TO W01-8LIN.
001770       MOVE "2"            TO W01-6LIN.
001780       OPEN OUTPUT PARAM.
001790       MOVE 1              TO WS-PARKEY.
001800 AC00.
001810       MOVE WS-PARKEY      TO WS-NUM.
001820	     DISPLAY WS-NUM AT 0626
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
001830 AC005-LOOP.
001840	     DISPLAY "Company Details" AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
             INITIALIZE PAR-RECORD1.
001850       MOVE SEC-COMP       TO PAR-COMPANY PAR-CSHEAD.
001880       MOVE SEC-DATE       TO PAR-DMY.
001890     IF SEC-PASS = 999999
001900         MOVE 9            TO PAR-STAT
001910     ELSE
001920         MOVE 0            TO PAR-STAT.
             MOVE "A"            TO PAR-STMINV.
001930       WRITE PAR-RECORD1.
001940       PERFORM AC00.
001950       ADD 1               TO WS-PARKEY.
             INITIALIZE PAR-RECORD2.
001960       MOVE "( Reg. No.             )" 
                                 TO PAR-ADDRESS1.
001970       MOVE "PO Box/Posbus"
                                 TO PAR-ADDRESS2.
002010       MOVE "Street Address"
                                 TO PAR-ADDRESS3.
002020       MOVE "Town/City"    TO PAR-ADDRESS4.
002030       WRITE PAR-RECORD2.
002040       PERFORM AC00.
002050       ADD 1               TO WS-PARKEY.
002060	     DISPLAY "File Locations " AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
             INITIALIZE PAR-RECORD3.
002320*	     MOVE ZERO		 TO PAR-POS.
	     MOVE 7		 TO PAR-INV.
	     MOVE 1		 TO PAR-SLIP
				    PAR-CHQ-PRN
				    PAR-CHQ-SPC.
	     MOVE 6		 TO PAR-STM.
	     MOVE "8.15"	 TO PAR-VERSION.
	     MOVE "PASSWORD"	 TO PAR-PASSWORD.
	     MOVE "******"	 TO PAR-CSHCODE.
	     MOVE "IPRINT"	 TO PAR-INVPASS.
	     MOVE "Y"		 TO PAR-NEW-SYS
				    PAR-PPROMPT
				    PAR-SLIP-P.
	     MOVE "N"		 TO PAR-OVER-LIM
				    PAR-ARREARS
				    PAR-CASES
				    PAR-PARCELS
				    PAR-SPLIT-BC
				    PAR-ALLOC-BC
				    PAR-ROUND-TOT
				    PAR-USE-3-DEC
				    PAR-INV-CHECK.
	     MOVE "X"		 TO PAR-BAR-CODE.
	     MOVE 80		 TO PAR-BCSIZE.
	     MOVE "A"		 TO PAR-STOCK.
	     MOVE "S"		 TO PAR-PLU-D.
	     MOVE 14		 TO PAR-SLNGTH.
	     MOVE ZERO		 TO PAR-SLIP-ROUND
				    PAR-INV-AMT
				    PAR-HFEE.
	     MOVE SEC-NUM	 TO PAR-SERIAL.
	     MOVE SEC-LEV	 TO PAR-LEV.
	     MOVE SEC-VER	 TO PAR-VER.
	     MOVE W12-YEAR	 TO PAR-CUR-YR.
	     MOVE W12-CENT	 TO PAR-CUR-CEN
				    PAR-PRV-CEN
				    PAR-NXT-CEN.
	     ADD 1		 TO PAR-NXT-CEN.
	     SUBTRACT 1 	 FROM PAR-PRV-CEN.

       AC05A.
	     MOVE "Record stock sales on a CARDEX file Y/N  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y".
	     MOVE WS-OPTION	 TO PAR-CRDX.
       AC05B.
	     MOVE "Update quantities on a PRICED item Y/N  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y".
	     MOVE WS-OPTION	 TO PAR-PRICED-IND.
       AC05C.
	     MOVE "Use the BANK DEPOSIT module Y/N  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y".
	     MOVE WS-OPTION	 TO PAR-BANK-IND.
       AC05D.
	     MOVE "Use PROMPTS with sales Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y".
	     MOVE WS-OPTION	 TO PAR-PROMPT.
	     DISPLAY CLEAR-L25.
002380       WRITE PAR-RECORD3.
002390       PERFORM AC00.
002400       ADD 1               TO WS-PARKEY.
003320       INITIALIZE PAR-RECORD4.
003330	     DISPLAY "Operator Details " AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     MOVE SEC-TERMS	 TO PAR-TRMS.
             MOVE "N"            TO PAR-AGE.
003380       WRITE PAR-RECORD4.
003390       PERFORM AC00.
002400       ADD 1               TO WS-PARKEY.
002410       MOVE "DS"           TO PAR-SYS.
002420	     DISPLAY "Debtor Discount Codes" AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
002440 AC06.
002450       INITIALIZE PAR-RECORD5.
002570	     MOVE "APACPW"	 TO PAR-SUPER (1)
002580                              PAR-SUPER (2)
002590                              PAR-SUPER (3)
002600                              PAR-SUPER (4)
002610                              PAR-SUPER (5)
002620                              PAR-SUPER (6)
002630                              PAR-SUPER (7)
002640                              PAR-SUPER (8)
002650                              PAR-SUPER (9).
002670       MOVE 28.00          TO PAR-MARG.
	     MOVE 5.00		 TO PAR-ADV-RATE.
             MOVE 1.75           TO PAR-FIN-RATE.
             MOVE 30             TO PAR-PERIOD.
002500       WRITE PAR-RECORD5.
002510       PERFORM AC00.
002420	     DISPLAY "Purchase Journal     " AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
002520	     ADD 1		 TO WS-PARKEY.
003410       INITIALIZE PAR-RECORD6.
003490       MOVE "QWERTYUIOP"   TO PAR-COST-CODE.
             MOVE 62             TO PAR-LPP.
             MOVE "N"            TO PAR-WHOLESALE
                                    PAR-INTEGRATE.
             MOVE 1              TO PAR-DEBGL  PAR-CREGL
                                    PAR-INTGL  PAR-BNKGL
                                    PAR-UNPROF PAR-REDGL
                                    PAR-ADJGL  PAR-RLGL
                                    PAR-DSGL.
	     MOVE 1		 TO PAR-TRF-REF.
	     MOVE 34		 TO PAR-STM-DET.
003500       WRITE PAR-RECORD6.
003510       PERFORM AC00.
003520	     ADD 1		 TO WS-PARKEY.
002420	     DISPLAY "Cash Sale - control  " AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
003530       INITIALIZE PAR-RECORD7.
	     MOVE 1		 TO PAR-PIC-REF.
003500       WRITE PAR-RECORD7.
	     MOVE "Y"		 TO PAR-CSH-ADD PAR-CSH-TEL
				    PAR-CSH-VAT PAR-CSH-TIME
				    PAR-CSH-ASSIST.
	     MOVE "2"		 TO PAR-CSH-DETAIL.
003510	     PERFORM AC00.
003520       ADD 1               TO WS-PARKEY.
002420	     DISPLAY "Cost of Sales        " AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
             INITIALIZE PAR-RECORD8.
	     MOVE "Description"  TO PAR-DESC-H1.
	     MOVE "Description 2"
				 TO PAR-DESC-H2.
	     MOVE "-Wholesale"	 TO PAR-WS-HD.
	     MOVE "-Cash     "	 TO PAR-CS-HD.
	     MOVE "-Retail   "	 TO PAR-RT-HD.
003500       WRITE PAR-RECORD8.
003510       PERFORM AC00.
002700	     ADD 1		 TO WS-PARKEY.
002420	     DISPLAY "VAT Control Account  " AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
             INITIALIZE PAR-RECORD9.
             MOVE "V"            TO PAR-VAT-GST.
003500       WRITE PAR-RECORD9.
003510       PERFORM AC00.
002700	     ADD 1		 TO WS-PARKEY.
004790	     DISPLAY "°°°°°°°°°°°°°°°°°°°°°" AT 0645
		      WITH BACKGROUND-COLOR 1 FOREGROUND-COLOR 3.
             MOVE 1              TO WS-S1.

       AC08.       
             MOVE 1              TO PAR-T-PRN1 (WS-S1).
             MOVE 2              TO PAR-T-PRN2 (WS-S1).
           IF WS-S1 < 63
               ADD 1             TO WS-S1
               GO TO AC08.
003500       WRITE PAR-RECORD10.
003510       PERFORM AC00.
002700	     ADD 1		 TO WS-PARKEY.
002710	     DISPLAY "Transaction Codes" AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
002730 AC09.
             INITIALIZE PAR-RECORD11.

002780 AC010-LOOP.
002790     IF WS-PARKEY = 11
002810         MOVE 01           TO PAR-T-CODE (1)
002820         MOVE 02           TO PAR-T-CODE (2)
002830         MOVE "Invoice"    TO PAR-E-DESC (1)
002840         MOVE "Faktuur"    TO PAR-A-DESC (1)
002850         MOVE "Receipt"    TO PAR-E-DESC (2)
002860         MOVE "Kwitansie"  TO PAR-A-DESC (2)
002870         MOVE 0            TO PAR-ACTION (1)
002880         MOVE 1            TO PAR-ACTION (2).
002900     IF WS-PARKEY = 12
002910         MOVE 03           TO PAR-T-CODE (1)
002920         MOVE 04           TO PAR-T-CODE (2)
002930         MOVE "Debit Note "
                                 TO PAR-E-DESC (1)
002940         MOVE "Debiet Nota"
                                 TO PAR-A-DESC (1)
002950         MOVE "Credit Note" 
                                 TO PAR-E-DESC (2)
002960         MOVE "Krediet Nota" 
                                 TO PAR-A-DESC (2)
002970         MOVE 0            TO PAR-ACTION (1)
002980         MOVE 1            TO PAR-ACTION (2).
002900     IF WS-PARKEY = 13
002910         MOVE 05           TO PAR-T-CODE (1)
002920         MOVE 06           TO PAR-T-CODE (2)
002930         MOVE "Dt Journal" TO PAR-E-DESC (1)
002940         MOVE "Dt Joernaal" 
                                 TO PAR-A-DESC (1)
002950         MOVE "Cr Journal" TO PAR-E-DESC (2)
002960         MOVE "Kt Joernaal" 
                                 TO PAR-A-DESC (2)
002970         MOVE 0            TO PAR-ACTION (1)
002980         MOVE 1            TO PAR-ACTION (2).
           IF WS-PARKEY = 17
003000         MOVE 13           TO PAR-T-CODE (1)
003010         MOVE 14           TO PAR-T-CODE (2)
003020         MOVE "Tax Refund" TO PAR-E-DESC (1) 
003030         MOVE "Tax"        TO PAR-E-DESC (2)
003040         MOVE "Belasting"  TO PAR-A-DESC (1)
003050         MOVE "Belasting"  TO PAR-A-DESC (2)
003060         MOVE 0            TO PAR-ACTION (1)
003070         MOVE 1            TO PAR-ACTION (2).
002990     IF WS-PARKEY = 18
003000         MOVE 15           TO PAR-T-CODE (1)
003010         MOVE 16           TO PAR-T-CODE (2)
003020	       MOVE "Refund"	 TO PAR-E-DESC (1)
003030         MOVE "Chq Reversal" 
                                 TO PAR-E-DESC (2)
003040         MOVE "Tjek Betaal" 
                                 TO PAR-A-DESC (1)
003050         MOVE "Tjek Terug" TO PAR-A-DESC (2)
003060         MOVE 0            TO PAR-ACTION (1)
003070         MOVE 0            TO PAR-ACTION (2).
003080     IF WS-PARKEY = 19
003090         MOVE 17           TO PAR-T-CODE (1)
003100         MOVE 18           TO PAR-T-CODE (2)
003110         MOVE "Int Reversed" 
                                 TO PAR-E-DESC (1) 
003120         MOVE "Instalment" TO PAR-E-DESC (2)
003130         MOVE "Rente Terug" 
                                 TO PAR-A-DESC (1)
003140         MOVE "Paaiment"   TO PAR-A-DESC (2)
003150         MOVE 1            TO PAR-ACTION (1)
003160         MOVE 0            TO PAR-ACTION (2).
003170     IF WS-PARKEY = 20
003180         MOVE 19           TO PAR-T-CODE (1)
003190         MOVE 20           TO PAR-T-CODE (2)
003200         MOVE "Discount"   TO PAR-E-DESC (1)
003210         MOVE "Afslag"     TO PAR-A-DESC (1)
003220         MOVE "Int Arrears" 
                                 TO PAR-E-DESC (2)
003230         MOVE "Rnt Agterst" 
                                 TO PAR-A-DESC (2)
003240         MOVE 1            TO PAR-ACTION (1).
003250       WRITE PAR-RECORD11.
003260       PERFORM AC00.
003270       PERFORM AC09.
003290	     ADD 1		 TO WS-PARKEY
003280     IF WS-PARKEY < 61
003300         GO TO AC010-LOOP.
             INITIALIZE PAR-RECORD61.

003680 AC15.
003740       WRITE PAR-RECORD61.
003750       PERFORM AC00.
003690       ADD 1               TO WS-PARKEY.
003780     IF WS-PARKEY < 101
003790        GO TO AC15.

003680 AC20.
004380       INITIALIZE PAR-RECORD101.
004430	     DISPLAY "Salesman Details " AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.

004450 AC40.
004460       WRITE PAR-RECORD101.
004470       PERFORM AC00.
004490       ADD 1               TO WS-PARKEY
004480     IF WS-PARKEY < 151
004500         GO TO AC40.
004510	     DISPLAY "Cash Draw Details" AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
004540       INITIALIZE PAR-RECORD151.
	     MOVE ZERO		 TO PAR-CSHDTE
				    PAR-FLOAT
				    PAR-CASH
				    PAR-CHEQUES
				    PAR-CARDS
				    PAR-VATVAL
				    PAR-VOUCHERS
				    PAR-RECEIPTS
				    PAR-CSH
				    PAR-CSHMTD
				    PAR-CSHYTD
				    PAR-CPRN
				    PAR-ITMS
				    PAR-CUST
				    PAR-TAXVAL
				    PAR-PETTY
				    PAR-BANKTRF
				    PAR-SPEED.
	     MOVE "N"		 TO PAR-CUSDISP
				    PAR-SCALE
				    PAR-USE-3
				    PAR-KTCHPRN
				    PAR-DRAWER.
	     MOVE 1		 TO PAR-CUSPORT
				    PAR-SCALPORT
				    PAR-KCHPORT
				    PAR-PORT.
	     MOVE SPACES	 TO PAR-KICK
				    PAR-SCALE-TYPE.
004640	     MOVE W01-NUL	 TO PAR-NUL.
004640       MOVE W01-ESC        TO PAR-ESC.
004650       MOVE W01-DBLE       TO PAR-DBL.
004660       MOVE W01-DCAN       TO PAR-CDBL.
004670       MOVE W01-COND       TO PAR-SI.
004680       MOVE W01-NORM       TO PAR-CSI.
004690       MOVE W01-EXP        TO PAR-SO.
004700       MOVE W01-ECAN       TO PAR-CSO.
004710       MOVE W01-6LIN       TO PAR-6LPI.
004720       MOVE W01-8LIN       TO PAR-8LPI.
             MOVE 1              TO PAR-CPRN.
             MOVE 6              TO PAR-ADVANCE.

004730 AC45.
004740       WRITE PAR-RECORD151.
004750       PERFORM AC00.
004770       ADD 1               TO WS-PARKEY
004760     IF WS-PARKEY < 191
004780         GO TO AC45.
004510	     DISPLAY "Creditor Tr codes" AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.

       AC50.
             INITIALIZE PAR-RECORD191.

004730 AC55.
           IF WS-PARKEY = 191
               MOVE "Invoice"    TO PAR-C-DESC (1)
               MOVE "Inv"        TO PAR-C-ABR (1)
               MOVE 0            TO PAR-C-ACT (1)
               MOVE 01           TO PAR-C-CODE (1)
               MOVE "Debit Note" TO PAR-C-DESC (2)
               MOVE "D/N"        TO PAR-C-ABR (2)
               MOVE 1            TO PAR-C-ACT (2)
               MOVE 02           TO PAR-C-CODE (2).
           IF WS-PARKEY = 192
               MOVE "Cheque"     TO PAR-C-DESC (1)
               MOVE "Chq"        TO PAR-C-ABR (1)
               MOVE 1            TO PAR-C-ACT (1)
               MOVE 03           TO PAR-C-CODE (1)
               MOVE "Payment"    TO PAR-C-DESC (2)
               MOVE "Pmt"        TO PAR-C-ABR (2)
               MOVE 0            TO PAR-C-ACT (2)
               MOVE 04           TO PAR-C-CODE (2).
           IF WS-PARKEY = 193
               MOVE "Debit Transfer"
                                 TO PAR-C-DESC (1)
               MOVE "Trf"        TO PAR-C-ABR (1)
               MOVE 1            TO PAR-C-ACT (1)
               MOVE 05           TO PAR-C-CODE (1)
               MOVE "Credit Transfer"
                                 TO PAR-C-DESC (2)
               MOVE "Trf"        TO PAR-C-ABR (2)
               MOVE 0            TO PAR-C-ACT (2)
               MOVE 06           TO PAR-C-CODE (2).
           IF WS-PARKEY = 194
               MOVE "Discount"   TO PAR-C-DESC (2)
               MOVE "Dsc"        TO PAR-C-ABR (2)
	       MOVE 1		 TO PAR-C-ACT (2)
               MOVE 08           TO PAR-C-CODE (2).
           IF WS-PARKEY = 195
               MOVE "Debit Journal"
                                 TO PAR-C-DESC (1)
               MOVE "D/J"        TO PAR-C-ABR (1)
               MOVE 1            TO PAR-C-ACT (1)
               MOVE 09           TO PAR-C-CODE (1)
               MOVE "Credit Journal"
                                 TO PAR-C-DESC (2)
               MOVE "C/J"        TO PAR-C-ABR (2)
               MOVE 0            TO PAR-C-ACT (2)
               MOVE 10           TO PAR-C-CODE (2).
           IF WS-PARKEY = 196
               MOVE "Invoice (G/L)"
                                 TO PAR-C-DESC (1)
               MOVE "Inv"        TO PAR-C-ABR (1)
               MOVE 0            TO PAR-C-ACT (1)
               MOVE 11           TO PAR-C-CODE (1)
               MOVE "Debit Note (G/L)" 
                                 TO PAR-C-DESC (2)
               MOVE "D/N"        TO PAR-C-ABR (2)
               MOVE 1            TO PAR-C-ACT (2)
               MOVE 12           TO PAR-C-CODE (2).
           IF WS-PARKEY = 197
               MOVE "Payment (G/L)"
                                 TO PAR-C-DESC (2)
               MOVE "Pmt"        TO PAR-C-ABR (2)
               MOVE 0            TO PAR-C-ACT (2)
               MOVE 14           TO PAR-C-CODE (2).
004740       WRITE PAR-RECORD191.
004750       PERFORM AC00.
004770	     ADD 1		 TO WS-PARKEY
004760     IF WS-PARKEY < 201
               MOVE 1            TO WS-S1
004780         GO TO AC50.
004510	     DISPLAY "Technician details" AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
             INITIALIZE PAR-RECORD201.

004450 AC60.
004460       WRITE PAR-RECORD201.
004470       PERFORM AC00.
004490       ADD 1               TO WS-PARKEY
004480     IF WS-PARKEY < 251
004500         GO TO AC60.
004790	     DISPLAY "Pinter specifications" AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
             INITIALIZE PAR-RECORD251.
002340	     MOVE SEC-PRNCOND	 TO W10-CONP.
002350	     MOVE SEC-PRNNORM	 TO W10-NORP.
002360	     MOVE SEC-PRNEXPD	 TO W10-EXPP.
002370	     MOVE SEC-PRNECAN	 TO W10-ECAN.
	     MOVE SEC-PAGE48	 TO W10-8LPI.
	     MOVE SEC-PAGE66	 TO W10-6LPI.
	     MOVE SEC-10CPI	 TO W10-10CPI.
	     MOVE SEC-12CPI	 TO W10-12CPI.
	     MOVE SEC-17CPI	 TO W10-17CPI.
002340	     MOVE W09-CONP	 TO TRM-CONP (1)
                                    TRM-CONP (2).
002350	     MOVE W09-NORP	 TO TRM-NORP (1)
                                    TRM-NORP (2).
002360	     MOVE W09-EXPP	 TO TRM-EXPP (1)
                                    TRM-EXPP (2).
002370	     MOVE W09-ECAN	 TO TRM-ECAN (1)
                                    TRM-ECAN (2).
	     MOVE W09-8LPI	 TO TRM-8LPI (1)
                                    TRM-8LPI (2).
	     MOVE W09-6LPI	 TO TRM-6LPI (1)
                                    TRM-6LPI (2).
	     MOVE W09-10CPI	 TO TRM-10CPI (1)
                                    TRM-10CPI (2).
	     MOVE W09-12CPI	 TO TRM-12CPI (1)
                                    TRM-12CPI (2).
	     MOVE W09-17CPI	 TO TRM-17CPI (1)
                                    TRM-17CPI (2).
	     MOVE 66		 TO TRM-PAGE (1)
				    TRM-PAGE (2).
	     MOVE 62		 TO TRM-LENGTH (1)
				    TRM-LENGTH (2).
	     MOVE "6"		 TO TRM-VERSION.

004450 AC65.
004460       WRITE PAR-RECORD251.
004470       PERFORM AC00.
004490       ADD 1               TO WS-PARKEY
004480     IF WS-PARKEY < 255
004500         GO TO AC65.
004790	     DISPLAY "Terminal printers    " AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
             INITIALIZE PAR-RECORD255.
             MOVE 1              TO WS-S1.

       AC70.       
000470       MOVE 1              TO TRM-PICP (WS-S1)
                                    TRM-PRN1 (WS-S1)
000480                              TRM-INVP (WS-S1)
                                    TRM-PRN2 (WS-S1)
000490                              TRM-STDP (WS-S1)
                                    TRM-PRN3 (WS-S1)
000500                              TRM-STMP (WS-S1)
                                    TRM-PRN4 (WS-S1)
000510                              TRM-CSHP (WS-S1)
                                    TRM-PRN5 (WS-S1)
000520                              TRM-POS  (WS-S1)
                                    TRM-PRN6 (WS-S1).
           IF WS-S1 < 10
               ADD 1             TO WS-S1
               GO TO AC70.

004450 AC75.
004460       WRITE PAR-RECORD255.
004470       PERFORM AC00.
004490       ADD 1               TO WS-PARKEY
004480     IF WS-PARKEY < 266
004500         GO TO AC75.
004790	     DISPLAY "Quotation Remarks    " AT 0645
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
             INITIALIZE PAR-RECORD266.

004450 AC80.
004460       WRITE PAR-RECORD266.
004470       PERFORM AC00.
004490       ADD 1               TO WS-PARKEY
004480	   IF WS-PARKEY < 275
004500         GO TO AC80.
	     MOVE "C"		 TO PAR-QTE-FMT.
	     MOVE 6		 TO PAR-QTE-MRG.
	     MOVE "Dear Sir/Madam,"
				 TO PAR-QTE-SAL.
	     MOVE "Yours sincerely"
				 TO PAR-QTE-SIG.
	     WRITE PAR-RECORD275.
004490       ADD 1               TO WS-PARKEY
	     INITIALIZE PAR-RECORD276.

       AC81.
004460	     WRITE PAR-RECORD276.
004470       PERFORM AC00.
004490       ADD 1               TO WS-PARKEY
004480	   IF WS-PARKEY < 285
004500	       GO TO AC81.
004790	     DISPLAY "°°°°°°°°°°°°°°°°°°°°°" AT 0645
		      WITH BACKGROUND-COLOR 1 FOREGROUND-COLOR 3.
004800	     DISPLAY WS-MES AT 0633
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.

004810 AC999-EXIT.
004820       EXIT.
      /
004830 AD000-CONTROL           SECTION.
004840 AD000-INIT.
001540	   IF LS0-DTP = 2 OR LS0-STP = 2 OR LS0-CRP = 2 OR
001550	      LS0-GLP = 2
               OPEN I-O NETWORK
               GO TO AD30
           ELSE
004850         OPEN OUTPUT NETWORK.
004860       MOVE 1              TO WS-NETKEY.
004870       INITIALIZE NET-DEBTOR.
005550       MOVE 1              TO DEB-BATCH  
                                    DEB-INVOICE
                                    DEB-ORDER  
                                    DEB-RECEIPT 
				    DEB-VOUCH
                                    DEB-JOBNO.
005570       MOVE 2              TO DEB-AUDIT.
005580       WRITE NET-DEBTOR.
       AD05.
005590	     MOVE WS-NETKEY	 TO WS-NUM.
005600	     DISPLAY WS-NUM AT 0826
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5.
       AD10.
             ADD 1               TO WS-NETKEY.
004870       INITIALIZE NET-CREDITOR.
             MOVE 1              TO P-CRTRAN P-PURORD.
             MOVE 2              TO P-CREDIT P-CREMAX.
             MOVE 68             TO P-CINDEX P-PURIDX.
             WRITE NET-CREDITOR.
             PERFORM AD05.
       AD15.
             ADD 1               TO WS-NETKEY.
004870       INITIALIZE NET-STOCK.
             MOVE "N"            TO STK-CARDEX.
000400       MOVE "PASSWORD"     TO STK-PASSWORD.
             WRITE NET-STOCK.
             PERFORM AD05.
       AD20.
             ADD 1               TO WS-NETKEY.
004870       INITIALIZE NET-LEDGER.
000420	     MOVE 2		 TO G-OPEN.
000440	     MOVE 1		 TO G-GLINST G-BATCH.
             WRITE NET-LEDGER.
             PERFORM AD05.
       AD25.
             ADD 1               TO WS-NETKEY.
004870       INITIALIZE NET-LAYBUY.
000420       MOVE 2              TO LAY-LAYBUY LAY-LAYMAX.
000440       MOVE 68             TO LAY-AINDEX.  
             WRITE NET-LAYBUY.
             PERFORM AD05.
       AD30.
005610	     DISPLAY WS-MES AT 0833
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
005620 AD999-EXIT.
005630       EXIT.
      /
       AE000-DEPART    SECTION.
       AE00.
001540	   IF LS0-DTP = 2 OR LS0-STP = 2 OR LS0-CRP = 2 OR
001550	      LS0-GLP = 2
              OPEN I-O DEPART
              GO TO AE15.
             OPEN OUTPUT DEPART.
             INITIALIZE DPT-RECORD.
	     MOVE "VAT1"	 TO DPT-CODE.
	     MOVE "VALUE ADDED TAX"
				 TO DPT-DESC.
	     MOVE 19930407	 TO DPT-R-DATE.
	     MOVE 14.00 	 TO DPT-RATE.
	     MOVE 10.00 	 TO DPT-P-RATE.

       AE10.
001820	     DISPLAY DPT-CODE AT 1026
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5.
             WRITE DPT-RECORD.

       AE12.
	   IF DPT-CODE < "VAT2"
	       MOVE "VAT2"	 TO DPT-CODE
	       GO TO AE10
	   ELSE
	   IF DPT-CODE < "VAT3"
	       MOVE "VAT3"	 TO DPT-CODE
	       GO TO AE10
	   ELSE
	   IF DPT-CODE < "VAT4"
	       MOVE "VAT4"	 TO DPT-CODE
	       GO TO AE10
	   ELSE
	   IF DPT-CODE < "VAT5"
	       MOVE "VAT5"	 TO DPT-CODE
	       GO TO AE10
	   ELSE
	   IF DPT-CODE < "VAT6"
	       MOVE "VAT6"	 TO DPT-CODE
	       GO TO AE10.
	     MOVE ZERO		 TO DPT-R-DATE
				    DPT-RATE
				    DPT-P-RATE.
	     MOVE "XXXX"	 TO DPT-CODE.
	     MOVE "UNSPECIFIED SALES"
				 TO DPT-DESC.
	     PERFORM AE10.
	     MOVE "YYYY"	 TO DPT-CODE.
	     MOVE "SETTLEMENT DISCOUNT GIVEN"
				 TO DPT-DESC.
	     PERFORM AE10.
             CLOSE DEPART.
             OPEN I-O DEPART.

       AE15.
004800	     DISPLAY WS-MES AT 1033
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
       AE999.
             EXIT.
      /
005940 AF000-TXTRAN            SECTION.
005950 AF000-INIT.
001540	   IF LS0-DTP = 2 OR LS0-STP = 2 OR LS0-CRP = 2 OR
001550	      LS0-GLP = 2
              OPEN I-O TXTRAN
              GO TO AF05.
005960	     OPEN OUTPUT TXTRAN.
005980       INITIALIZE TAX-RECORD1.
             MOVE "0"            TO TAX-TYPE.
             MOVE 0              TO TAX-ACTYPE.
             MOVE ZERO           TO TAX-DATE.
006010       WRITE TAX-RECORD1.

       AF05.
006020	     DISPLAY WS-MES AT 1433
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.

006030 AF999-EXIT.
006040       EXIT.
      /
005940 AG000-AUDIT             SECTION.
005950 AG000-INIT.
001540	   IF LS0-DTP = 2 OR LS0-STP = 2 OR LS0-CRP = 2 OR
001550	      LS0-GLP = 2
              OPEN I-O AUDIT
              GO TO AG05.
005960       OPEN OUTPUT AUDIT.
005970       MOVE 1 TO WS-AUDKEY.
005980       INITIALIZE AUD-HDR.
006010       WRITE AUD-REC1.

       AG05.
006020	     DISPLAY WS-MES AT 1233
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.

006030 AG999-EXIT.
006040       EXIT.
      /
005940 AH000-CHEQUE            SECTION.
005950 AH000-INIT.
             OPEN OUTPUT CHEQUE.
005610	     DISPLAY WS-MES AT 1633
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.

       AH999-EXIT.
             EXIT.
      /
005940 AI000-SYSUSE	       SECTION.
005950 AI000-INIT.
	     OPEN OUTPUT SYSUSER.
005980	     INITIALIZE USE-RECORD.
	     MOVE 001		 TO USE-NO.
	     MOVE "MISTTO                R RANIAD"
				 TO USE-NAME.
	     MOVE "oNsjyzuy"	 TO USE-PASS.
	     MOVE "Y"		 TO USE-GL    USE-STOCK USE-DEBT
				    USE-CRED  USE-PARM	USE-SALES
				    USE-JOBS  USE-WAGES.
	     MOVE 9		 TO USE-GLLEV USE-STLEV USE-DBLEV
				    USE-CRLEV USE-PALEV USE-SALEV
				    USE-JBLEV USE-WGLEV.
006010	     WRITE USE-RECORD.
005610	     DISPLAY WS-MES AT 1833
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.

       AI999-EXIT.
             EXIT.
      /
006170 AZ000-END               SECTION.
006180 AZ000-EOJ.
006190     IF SEC-PASS = 999999
006200         MOVE 979695 TO SEC-PASS.
006210       REWRITE SEC-REC1.
006220       CLOSE SECUR.
006230       CLOSE CHEQUE.
006230       CLOSE PARAM.
006260       CLOSE NETWORK.
006270       CLOSE DEPART.
006290       CLOSE AUDIT.
	     CLOSE SYSUSER.
             CLOSE TXTRAN.

006310 AZ02.
006320       DISPLAY "***** Initialization Complete ****" AT 2212 
006330		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.

006350 AZ05.
006390       DISPLAY CLR-SCREEN.
006400	   IF LS0-DTP = 1
006410         DISPLAY "Loading Debtor initialize program" AT 1212
	       MOVE "UTP\DTP000" TO LS-NEXT-PRG
	       GO TO AZ999.
006480	   IF LS0-JCP = 1
006490         DISPLAY "Loading Job Costing initialize program" AT 1212
006500	       MOVE "UTP\JCP000" TO LS-NEXT-PRG
	       GO TO AZ999.
006480	   IF LS0-STP = 1
006490         DISPLAY "Loading Stock initialize program" AT 1212
006500	       MOVE "UTP\STP000" TO LS-NEXT-PRG
	       GO TO AZ999.
006520	   IF LS0-CRP = 1
006530         DISPLAY "Loading Creditors initialize program" AT 1212
006540	       MOVE "UTP\CRP000" TO LS-NEXT-PRG
	       GO TO AZ999.
006560	   IF LS0-GLP = 1
006570         DISPLAY "Loading G/Ledger initialize program" AT 1212
006580	       MOVE "UTP\GLP000" TO LS-NEXT-PRG
	       GO TO AZ999.
	     MOVE SPACES	 TO LS-NEXT-PRG.

       AZ999.
	     EXIT PROGRAM.
