      $set LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *    *****   *******  *******  **      ****  **    **  *******   *
      *   **   **  **       **       **       **   ***   **  **        *
      *   **   **  **	    **       **	    **   ****  **  **        *
      *   **   **  *****    *****    **	    **   ** ** **  *****     *
      *   **   **  **       **       **	    **   **  ****  **        *
      *   **   **  **       **       **       **   **   ***  **        *
      *    *****   **       **       ******  ****  **    **  *******   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     ( A P A C )   S E T U P   O F F L I N E   L O C A T I O N  *
      *                                                                *
      *     VERSION 9.00.00 - August 2012		                       *
      * 							                       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     OFFLFILE.
000030 AUTHOR.         J W LEMMON  (APAC).
000040 DATE-WRITTEN.   OCTOBER 1982.

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

       COPY "APACFILE.SL".

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY APACFILE.FD.

000700 WORKING-STORAGE SECTION.
       77  WS-CHECK	 PIC	X(18)	 VALUE "aeWlimemnomLalismJ".
000710 77  WS-S1	       PIC  9(04) COMP-5.
000720 77  WS-S2	       PIC  9(04) COMP-5.
000730 77  WS-S3	       PIC  9(04) COMP-5.
000740 77  WS-S4	       PIC  9(04) COMP-5.
       77  WS-ERROR	 PIC  9(01) VALUE ZERO.

001010 COPY "WS.WS".

       COPY "APACFIDS.ID".

       COPY "APACFILE.ID".

       COPY "SYSTEM.ID".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.	  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C =  Clear comment line (50)
      *         E =  Clear lines (any line or lines between 5 and 46)
      *         H =  Change heading
      *         S =  Clear the screen and display headings
      *
	   03  CRT-TYPE      PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
	   03  CRT-START     PIC  9(02).
	   03  CRT-END	   PIC  9(02).
      *
      *    Calling Program
      *
	   03  CRT-PROGRAM   PIC  X(15) VALUE "OFFLFILE".
      *
      *    Screen Heading
      *
	   03  CRT-HEADING   PIC  X(40) VALUE "OFFLINE FILE LOCATIONS".
      *
      *    Company Name
      *
	   03  CRT-COMPANY   PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
         03  CRT-WRKHD     PIC  X(11).
         03  CRT-WRKST     PIC  X(03) VALUE "ÄÄÄ".

       LINKAGE SECTION.

       COPY "CHAIN.LS".

      /
001940 SCREEN SECTION.

010570 PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
005070 AA000-MAIN              SECTION.
       AA00.
             MOVE LS-COMPANY         TO CRT-COMPANY.
           IF LS-USER = LS-SYS-ID
               MOVE "SupervisorÄ"    TO CRT-WRKHD
           ELSE
               MOVE "Workstation"    TO CRT-WRKHD
               MOVE LS-USER          TO CRT-WRKST.
             PERFORM AE000.

       AA99.
             EXIT PROGRAM.

       COPY "DISPMESS.SCR".

       COPY "ERROR.SCR".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		 O F F L I N E	 L O C A T I O N S	     ³
      *    ³This routine inserted from version 8.07 and sets up the    ³
      *    ³offline directory and locations if it has not been created.³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
007730 AE000                   SECTION 5.
007740 AE00.
             MOVE "S"                TO CRT-TYPE.
007750       CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.

008090 AE05.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             MOVE "OFFLINE\"                   TO W02-L-OR-N W02-N-OR-L.
             MOVE WS-SYS-ID                    TO W02-SYSPTH.
             MOVE ZERO                         TO WS-ERROR.
             CALL "CBL_CHECK_FILE_EXIST" USING W02-APACFILE FILE-DETAILS RETURNING WS-STATUS.
           IF WS-STATUS = ZERO
               MOVE "Off-line locations exist" TO WS-ERR-STRING
               PERFORM ERROR-MESSAGE
               GO TO AE999.
             CALL "CBL_CREATE_DIR" USING W02-OFFLINE RETURNING WS-STATUS.
             PERFORM OFFLFILES.
             PERFORM OFFL-SETUP-PATHS.
             MOVE "Off-line locations created"  TO WS-ERR-STRING.
             PERFORM DISP-MESSAGE.
             MOVE "SYSTEMS\"                    TO W02-L-OR-N W02-N-OR-L.
008620 AE999.
008630       EXIT.
      *    *************************************************************
      *     C R E A T E	  O F F L I N E   F I L E S   L O C A T I O N S
      *    *************************************************************
       OFFLFILES       SECTION 5.
       OFFL-FILES-INIT.
             MOVE WS-SYS-ID            TO W02-SYSPTH.
             OPEN OUTPUT APACFILE.
             CLOSE APACFILE.
             PERFORM OFFL-CREATE-NEW THRU OFFL-CREATE-NEW-EXIT.
             CLOSE APACFILE.
             GO TO OFFLFILES-EXIT.

       OFFL-CREATE-NEW.
             OPEN I-O APACFILE.
      *    *************************************************************
      *    A S S E T T	 F I L E  -  F U T U R E   D E V E L O P M E N T
      *    *************************************************************
             MOVE "ASSETT"           TO APAC-KEY.
             MOVE "ASSETT.DAT"	 TO APAC-NAME.
             MOVE 11                 TO APAC-FILE.
             MOVE SPACES             TO APAC-PATH.
             STRING WS-SYS-ID DELIMITED SIZE "_DATA\" DELIMITED SIZE INTO APAC-PATH.

       OFFL-WRITE-APACFILE.
             WRITE APAC-RECORD1.

       OFFL-DATA-FILES.
      *    *************************************************************
      *    ****          D A I L Y   -   A U D I T   F I L E        ****
      *    *************************************************************
             MOVE "AUDITF"           TO APAC-KEY.
             MOVE "AUDIT.DAT"        TO APAC-NAME.
             MOVE 16                 TO APAC-FILE.
             PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ** B A R C O D E   L A B E L S   T O	  B E	  P R I N T E D **
      *    *************************************************************
             MOVE "BARCOD"           TO APAC-KEY.
             MOVE "BARCODE.DAT"	 TO APAC-NAME.
             MOVE 57                 TO APAC-FILE.
             PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****    P U R C H A S E S   -   C A R D E X   F I L E    ****
      *    *************************************************************
	     MOVE "PURDEX"	 TO APAC-KEY.
	     MOVE "CARDEXPR.DAT" TO APAC-NAME.
	     MOVE 56		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****        S A L E S   -   C A R D E X   F I L E        ****
      *    *************************************************************
	     MOVE "CARDEX"	 TO APAC-KEY.
	     MOVE "CARDEXSL.DAT" TO APAC-NAME.
	     MOVE 43		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    C A T A L O G U E   I T E M S   F I L E - F U T U R E   D E V
      *    *************************************************************
	     MOVE "CATITM"	 TO APAC-KEY.
	     MOVE "CATITM.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    C A T A L O G U E   F I L E   -   F U T U R E   D E V E L O P
      *    *************************************************************
	     MOVE "CATLOG"	 TO APAC-KEY.
	     MOVE "CATLOG.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****      C H E Q U E S   R E C E I V E D   F I L E      ****
      *    *************************************************************
	     MOVE "CHEQUE"	 TO APAC-KEY.
	     MOVE "CHEQUE.DAT"	 TO APAC-NAME.
	     MOVE 36		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    C H E Q U E S   B A T C H   F I L E	-  C R E D I T O R S  AP
      *    *************************************************************
	     MOVE "CHQBATCH"	 TO APAC-KEY.
	     MOVE "CHQBCH.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    C H E Q U E S   B A T C H   F I L E	-  G / L  -  C U R R  D.
      *    *************************************************************
	     MOVE "CHQBCH"	 TO APAC-KEY.
	     MOVE "CHQBCH_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****               C O N T R O L   F I L E               ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO APAC-KEY.
	     MOVE "CONTROL.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****   ( A / P )   C R E D I T O R	M E M O   F I L E   ****
      *    *************************************************************
	     MOVE "CRDMEM"	 TO APAC-KEY.
	     MOVE "AP_MEMO.DAT"	 TO APAC-NAME.
	     MOVE 6		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    *** C R E D I T O R   A C C O U N T   ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO APAC-KEY.
	     MOVE "AP_ACNT.DAT"	 TO APAC-NAME.
	     MOVE 7		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ( A / P )   C R E D I T O R   T R A N S A C T I O N   F I L E
      *    *************************************************************
	     MOVE "CRTRAN"	 TO APAC-KEY.
	     MOVE "AP_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 8		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *     C A S H   B O O K	F I L E  -  F U T U R E   D E V E L O P
      *    *************************************************************
	     MOVE "CSBOOK"	 TO APAC-KEY.
	     MOVE "CSBOOK.DAT"	 TO APAC-NAME.
	     MOVE 29		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *     C A S H   B O O K	T R A N S A C T   F I L E - F U T U R E
      *    *************************************************************
	     MOVE "CSTRAN"	 TO APAC-KEY.
	     MOVE "CSTRAN.DAT"	 TO APAC-NAME.
	     MOVE 30		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****      D E B T O R   C O N T R A C T S   F I L E      ****
      *    *************************************************************
	     MOVE "DBFOTO"	 TO APAC-KEY.
	     MOVE "AR_CONT.DAT"	 TO APAC-NAME.
	     MOVE 3		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****    D E B T O R   T R A N S A C T I O N   F I L E    ****
      *    *************************************************************
	     MOVE "DBTRAN"	 TO APAC-KEY.
	     MOVE "AR_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 4		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****    D E B T O R   N A M E   I N D E X   F I L E      ****
      *    *************************************************************
	     MOVE "DEBALT"	 TO APAC-KEY.
	     MOVE "AR_NAME.DAT"	 TO APAC-NAME.
	     MOVE 47		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****    D E B T O R   C O L L E C T I O N S   F I L E    ****
      *    *************************************************************
	     MOVE "DEBCOL"	 TO APAC-KEY.
	     MOVE "AR_COL.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    **** D E B T O R   S A L E S   R E S T R I C T   F I L E ****
      *    *************************************************************
	     MOVE "DEBDEP"	 TO APAC-KEY.
	     MOVE "AR_REST.DAT"  TO APAC-NAME.
	     MOVE 33		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****        D E B T O R   H I S T O R Y   F I L E        ****
      *    *************************************************************
	     MOVE "DEBHIS"	 TO APAC-KEY.
	     MOVE "AR_HIST.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****     ( A / R )	D E B T O R   M E M O	F I L E       ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO APAC-KEY.
	     MOVE "AR_MEMO.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *     D E B T O R	  O R D E R   F I L E	-  C U R R E N T   D E V
      *    *************************************************************
	     MOVE "DEBORD"	 TO APAC-KEY.
	     MOVE "AR_ORD.DAT"	 TO APAC-NAME.
	     MOVE 53		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    **** 	          D E B T O R   G / P   F I L E           ****
      *    *************************************************************
	     MOVE "DEBPRF"	 TO APAC-KEY.
	     MOVE "DEBPRF.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
	     MOVE "DEBTOR"	 TO APAC-KEY.
	     MOVE "AR_ACNT.DAT"	 TO APAC-NAME.
	     MOVE 2		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    **** 	         D E P A R T M E N T   F I L E            ****
      *    *************************************************************
	     MOVE "DEPART"	 TO APAC-KEY.
	     MOVE "DEPART.DAT"	 TO APAC-NAME.
	     MOVE 38		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *     E G G   S A L E S	F I L E   -   E G G   P R O D U C E R S
      *    *************************************************************
	     MOVE "EGGSLE"	 TO APAC-KEY.
	     MOVE "EGGSLE.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    *** G E N E R A L   L E D G E R   A C C O U N T   F I L E ***
      *    *************************************************************
	     MOVE "GACCNT"	 TO APAC-KEY.
	     MOVE "GL_ACNT.DAT"	 TO APAC-NAME.
	     MOVE 12		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    **** 	         G U A R A N T E E   F I L E              ****
      *    *************************************************************
	     MOVE "GARTEE"	 TO APAC-KEY.
	     MOVE "GARTEE.DAT"	 TO APAC-NAME.
	     MOVE 9		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *     G E N E R A L   L E D G   B A T C H   H E A D E R   F I L E
      *    *************************************************************
	     MOVE "GLBHDR"	 TO APAC-KEY.
	     MOVE "GL_BHDR.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    G / L   J O U R N A L S   B A T C H	 T R A N S A C T I O N S
      *    *************************************************************
	     MOVE "GLBTCH"	 TO APAC-KEY.
	     MOVE "GLBTCH_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *     G / L   C H E Q U E S   B A T C H	T R A N S A C T I O N S
      *    *************************************************************
	     MOVE "GLCHEQ"	 TO APAC-KEY.
	     MOVE "GLCHEQ_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    G / L   D E P O S I T S   B A T C H	 T R A N S A C T I O N S
      *    *************************************************************
	     MOVE "GLDEPO"	 TO APAC-KEY.
	     MOVE "GLDEPO_1.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****	G / L	R E C U R R I N G   T R A N S A C T I O N S ****
      *    *************************************************************
	     MOVE "GLINST"	 TO APAC-KEY.
	     MOVE "GL_INST.DAT"	 TO APAC-NAME.
	     MOVE 15		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    G E N E R A L   L E D G E R   T R A N S A C T I O N   F I L E
      *    *************************************************************
	     MOVE "GLTRAN"	 TO APAC-KEY.
	     MOVE "GL_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 13		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    *** G / L   P O S T   D A T E D   C H E Q U E S   F I L E ***
      *    *************************************************************
	     MOVE "GPDCHQ"	 TO APAC-KEY.
	     MOVE "GL_PDCHQ.DAT" TO APAC-NAME.
	     MOVE 45		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****  ( A / R )   D E B T O R   I N V O I C E  F I L E   ****
      *    *************************************************************
	     MOVE "INVOIC"	 TO APAC-KEY.
	     MOVE "INVOICE.DAT"	 TO APAC-NAME.
	     MOVE 22		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "ITEMS"	 TO APAC-KEY.
	     MOVE "ITEMS.DAT"	 TO APAC-NAME.
	     MOVE 57		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JCHIST"	 TO APAC-KEY.
	     MOVE "JCHIST.DAT"	 TO APAC-NAME.
	     MOVE 58		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    **** J O B	C O S T I N G   -   D E T A I L S   F I L E ****
      *    *************************************************************
	     MOVE "JOBCST"	 TO APAC-KEY.
	     MOVE "JOBLIN.DAT"	 TO APAC-NAME.
	     MOVE 25		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****   J O B   C O S T I N G   -   A U D I T   F I L E   ****
      *    *************************************************************
	     MOVE "JOBAUD"	 TO APAC-KEY.
	     MOVE "JOBAUD.DAT"	 TO APAC-NAME.
	     MOVE 24		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JCLINE"	 TO APAC-KEY.
	     MOVE "JCLINE.DAT"	 TO APAC-NAME.
	     MOVE 59		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****  J O B   C O S T I N G   -   H E A D E R   F I L E  ****
      *    *************************************************************
	     MOVE "JOBHDR"	 TO APAC-KEY.
	     MOVE "JOBHDR.DAT"	 TO APAC-NAME.
	     MOVE 21		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *     J O B   C O S T I N G   -   R E Q U I S I T I O N   F I L E
      *    *************************************************************
	     MOVE "JOBREQ"	 TO APAC-KEY.
	     MOVE "JOBREQ.DAT"	 TO APAC-NAME.
	     MOVE 26		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ***  J O B   C O S T I N G   -   R E M A R K S   F I L E  ***
      *    *************************************************************
	     MOVE "JOBRMK"	 TO APAC-KEY.
	     MOVE "JOBRMK.DAT"	 TO APAC-NAME.
	     MOVE 27		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JOBSTAT"	 TO APAC-KEY.
	     MOVE "JOBSTAT.DAT"	 TO APAC-NAME.
	     MOVE 60		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JOBTECH"	 TO APAC-KEY.
	     MOVE "JOBTECH.DAT"	 TO APAC-NAME.
	     MOVE 61		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JOBWAR"	 TO APAC-KEY.
	     MOVE "JOBWAR.DAT"	 TO APAC-NAME.
	     MOVE 28		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****           I N T E G R A T I O N    F I L E          ****
      *    *************************************************************
	     MOVE "LEDTRF"	 TO APAC-KEY.
	     MOVE "LEDTRF.DAT"	 TO APAC-NAME.
	     MOVE 40		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    I N T E G R A T I O N   F I L E   (C R E D I T O R S   G / L)
      *    *************************************************************
	     MOVE "LEDTRG"	 TO APAC-KEY.
	     MOVE "LTRG_XXX.DAT" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    **** 	          P A R A M E T E R   F I L E             ****
      *    *************************************************************
	     MOVE "PARAM"	 TO APAC-KEY.
	     MOVE "PARAM.DAT"	 TO APAC-NAME.
	     MOVE 1		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "PDCHEQ"	 TO APAC-KEY.
	     MOVE "PDCHEQ.DAT"	 TO APAC-NAME.
	     MOVE 44		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****          P I C K I N G   S L I P   F I L E          ****
      *    *************************************************************
	     MOVE "PICSLIP"	 TO APAC-KEY.
	     MOVE "PICSLP.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****        P U R C H A S E   O R D E R   F I L E        ****
      *    *************************************************************
	     MOVE "PORDER"	 TO APAC-KEY.
	     MOVE "PORDER.DAT"	 TO APAC-NAME.
	     MOVE 10		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****              P L U   C O D E S   F I L E	        ****
      *    *************************************************************
	     MOVE "POSPLU"	 TO APAC-KEY.
	     MOVE "POSPLU.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    R O U T E   F I L E   -   C U R R E N T   D E V E L O P M E N
      *    *************************************************************
	     MOVE "ROUTE"	 TO APAC-KEY.
	     MOVE "ROUTE.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****              P O S   S L I P S   F I L E            ****
      *    *************************************************************
	     MOVE "PSLIPS"	 TO APAC-KEY.
	     MOVE "PSLIPS.DAT"	 TO APAC-NAME.
	     MOVE 35		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E        ****
      *    *************************************************************
	     MOVE "SHARED"	 TO APAC-KEY.
	     MOVE "SHARED.DAT"	 TO APAC-NAME.
	     MOVE 37		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    **** 	        S A L E S   O R D E R   F I L E           ****
      *    *************************************************************
	     MOVE "SORDER"	 TO APAC-KEY.
	     MOVE "SORDER.DAT"	 TO APAC-NAME.
	     MOVE 17		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    *** S T O C K   ( I V E N T O R Y )	 P A R T S   F I L E ***
      *    *************************************************************
	     MOVE "SPARTS"	 TO APAC-KEY.
	     MOVE "IN_PARTS.DAT" TO APAC-NAME.
	     MOVE 20		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****   S T O C K   D E S C R I P T I O N   INDEX   FILE  ****
      *    *************************************************************
	     MOVE "STKALT"	 TO APAC-KEY.
	     MOVE "IN_DESC.DAT"	 TO APAC-NAME.
	     MOVE 49		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****  A D J U S T M E N T   -   H I S T O R Y   F I L E  ****
      *    *************************************************************
	     MOVE "STKDEX"	 TO APAC-KEY.
	     MOVE "IN_ADJ.DAT"	 TO APAC-NAME.
	     MOVE 46		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ***  S T O C K   ( I V E N T O R Y )   M E M O   F I L E  ***
      *    *************************************************************
	     MOVE "STKMEM"	 TO APAC-KEY.
	     MOVE "IN_MEMO.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****      S T O C K   ( I V E N T O R Y )   F I L E      ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO APAC-KEY.
	     MOVE "INVENT.DAT"	 TO APAC-NAME.
	     MOVE 5		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "SYSTEM"	 TO APAC-KEY.
	     MOVE "SYSTEM.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    **** 	               U S E R S   F I L E                ****
      *    *************************************************************
	     MOVE "SYSUSE"	 TO APAC-KEY.
	     MOVE "USERS.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "TERMINAL"	 TO APAC-KEY.
	     MOVE "TRMNAL.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "TRFRIN"	 TO APAC-KEY.
	     MOVE "TRFRIN.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "TRFOUT"	 TO APAC-KEY.
	     MOVE "TRFOUT.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****       V A L U E  A D D E D   T A X   F I L E        ****
      *    *************************************************************
	     MOVE "TXTRAN"	 TO APAC-KEY.
	     MOVE "TX_TRAN.DAT"	 TO APAC-NAME.
	     MOVE 19		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "UNTSLE"	 TO APAC-KEY.
	     MOVE "UNTSLE.DAT"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "VEHICLE"	 TO APAC-KEY.
	     MOVE "VEHICLE.DAT"	 TO APAC-NAME.
	     MOVE 54		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "VSTOCK"	 TO APAC-KEY.
	     MOVE "V_INVENT.DAT" TO APAC-NAME.
	     MOVE 55		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****             W A R E H O U S E   F I L E             ****
      *    *************************************************************
	     MOVE "WARHSE"	 TO APAC-KEY.
	     MOVE "WARHSE.DAT"	 TO APAC-NAME.
	     MOVE 51		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    W A R E H O U S E  -  S T O C K  ( I V E N T O R Y )  F I L E
      *    *************************************************************
	     MOVE "WSTOCK"	 TO APAC-KEY.
	     MOVE "W_INVENT.DAT" TO APAC-NAME.
	     MOVE 52		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.

       OFFL-WORK-FILES.
	     MOVE "BULKST"	 TO APAC-KEY.
	     MOVE "BULK_XXX.WRK" TO APAC-NAME.
	     MOVE 59		 TO APAC-FILE.
	     MOVE SPACES	 TO APAC-PATH.
	     STRING WS-SYS-ID DELIMITED SIZE
		    "_WORK\" DELIMITED SIZE
		    INTO APAC-PATH.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****   P U R C H A S E   O R D E R	W O R K   F I L E   ****
      *    *************************************************************
	     MOVE "CORDER"	 TO APAC-KEY.
	     MOVE "CORD_XXX.WRK" TO APAC-NAME.
	     MOVE 48		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "CRETRN"	 TO APAC-KEY.
	     MOVE "CTRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DBTCON"	 TO APAC-KEY.
	     MOVE "DCON_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DEALER"	 TO APAC-KEY.
	     MOVE "DEAL_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DPTSALES"	 TO APAC-KEY.
	     MOVE "DSAL_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DEBTEX"	 TO APAC-KEY.
	     MOVE "DBEX_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DEBTOT"	 TO APAC-KEY.
	     MOVE "DTOT_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DEBTRN"	 TO APAC-KEY.
	     MOVE "DTRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DRBTCH"	 TO APAC-KEY.
	     MOVE "DBCH_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "GLBATCH"	 TO APAC-KEY.
	     MOVE "GBAL_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    **    G / L   T R A N S A C T I O N   W O R K   F I L E    **
      *    *************************************************************
	     MOVE "GLJRN"	 TO APAC-KEY.
	     MOVE "GJRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    **    G / L   T R A N S A C T I O N	 W O R K   F I L E    **
      *    *************************************************************
	     MOVE "GLTRNB"	 TO APAC-KEY.
	     MOVE "GTRB_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "INVSAL"	 TO APAC-KEY.
	     MOVE "INVS_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "LEDTRN"	 TO APAC-KEY.
	     MOVE "LTRN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "MEMDEB"	 TO APAC-KEY.
	     MOVE "DEBM_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "MOVEMT"	 TO APAC-KEY.
	     MOVE "MOVM_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "PERFRM"	 TO APAC-KEY.
	     MOVE "PERF_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "PRCLAB"	 TO APAC-KEY.
	     MOVE "PLAB_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "PRNDISK"	 TO APAC-KEY.
	     MOVE "PRNT_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "PURCH"	 TO APAC-KEY.
	     MOVE "PURC_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "RECEIPTS"	 TO APAC-KEY.
	     MOVE "RECP_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****              R E C O V E R Y   F I L E              ****
      *    *************************************************************
	     MOVE "RECOVER"	 TO APAC-KEY.
	     MOVE "RECV_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****                S A L E S   F I L E                  ****
      *    *************************************************************
	     MOVE "SALE"	 TO APAC-KEY.
	     MOVE "SALE_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
      *    *************************************************************
      *    ****    S A L E S   F I L E  -  J O B   C O S T I N G    ****
      *    *************************************************************
	     MOVE "SALEJ"	 TO APAC-KEY.
	     MOVE "SALJ_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "SALESMAN"	 TO APAC-KEY.
	     MOVE "SMAN_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "SALESTRK"	 TO APAC-KEY.
	     MOVE "STRK_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "STKORD"	 TO APAC-KEY.
	     MOVE "STKO_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "STKSEL"	 TO APAC-KEY.
	     MOVE "STKS_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "STKWRK"	 TO APAC-KEY.
	     MOVE "STOT_XXX.WRK" TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.

       OFFL-OLD-FILES.
	     MOVE SPACES	 TO APAC-PATH.
	     STRING WS-SYS-ID DELIMITED SIZE
		    "_DATA\" DELIMITED SIZE
		    INTO APAC-PATH.
	     MOVE "ASSOLD"	 TO APAC-KEY.
	     MOVE "ASSETT.OLD"	 TO APAC-NAME.
	     MOVE ZERO		 TO APAC-FILE.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "CAROLD"	 TO APAC-KEY.
	     MOVE "CARDEXSL.OLD" TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "CHQBHOLD"	 TO APAC-KEY.
	     MOVE "CHQBCH_1.OLD" TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "CHQOLD"	 TO APAC-KEY.
	     MOVE "CHEQUE.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "CREOLD"	 TO APAC-KEY.
	     MOVE "AP_ACNT.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "CRMOLD"	 TO APAC-KEY.
	     MOVE "AP_MEMO.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "CRTOLD"	 TO APAC-KEY.
	     MOVE "AP_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DBFOLD"	 TO APAC-KEY.
	     MOVE "AR_CONT.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DBMOLD"	 TO APAC-KEY.
	     MOVE "AR_MEMO.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DBTOLD"	 TO APAC-KEY.
	     MOVE "AR_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DEBOLD"	 TO APAC-KEY.
	     MOVE "AR_ACNT.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "DEPOLD"	 TO APAC-KEY.
	     MOVE "DEPART.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "GACOLD"	 TO APAC-KEY.
	     MOVE "GL_ACNT.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "GAROLD"	 TO APAC-KEY.
	     MOVE "GARTEE.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "GBHOLD"	 TO APAC-KEY.
	     MOVE "GL_BHDR.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "GLBOLD"	 TO APAC-KEY.
	     MOVE "GLBTCH_1.OLD" TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "GPDOLD"	 TO APAC-KEY.
	     MOVE "GL_PDCHQ.OLD" TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "GTROLD"	 TO APAC-KEY.
	     MOVE "GL_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "INVOLD"	 TO APAC-KEY.
	     MOVE "INVOICE.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "ITMOLD"	 TO APAC-KEY.
	     MOVE "ITEMS.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JAUOLD"	 TO APAC-KEY.
	     MOVE "JOBAUD.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JHDOLD"	 TO APAC-KEY.
	     MOVE "JOBHDR.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JHSOLD"	 TO APAC-KEY.
	     MOVE "JCHIST.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JOBOLD"	 TO APAC-KEY.
	     MOVE "JOBLIN.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JRMOLD"	 TO APAC-KEY.
	     MOVE "JOBRMK.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JRQOLD"	 TO APAC-KEY.
	     MOVE "JOBREQ.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JSTOLD"	 TO APAC-KEY.
	     MOVE "JOBSTAT.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JTCOLD"	 TO APAC-KEY.
	     MOVE "JOBTECH.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "JWAOLD"	 TO APAC-KEY.
	     MOVE "JOBWAR.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "LEDOLD"	 TO APAC-KEY.
	     MOVE "LEDTRF.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "PDCOLD"	 TO APAC-KEY.
	     MOVE "PDCHEQ.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "PRDOLD"	 TO APAC-KEY.
	     MOVE "CARDEXPR.OLD" TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "PUROLD"	 TO APAC-KEY.
	     MOVE "PORDER.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "SLPOLD"	 TO APAC-KEY.
	     MOVE "PSLIPS.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "SOROLD"	 TO APAC-KEY.
	     MOVE "SORDER.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "SPAOLD"	 TO APAC-KEY.
	     MOVE "IN_PARTS.OLD" TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "STKOLD"	 TO APAC-KEY.
	     MOVE "INVENT.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "STMOLD"	 TO APAC-KEY.
	     MOVE "IN_MEMO.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "STXOLD"	 TO APAC-KEY.
	     MOVE "IN_ADJ.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "TXTOLD"	 TO APAC-KEY.
	     MOVE "TX_TRAN.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "UNTOLD"	 TO APAC-KEY.
	     MOVE "UNTSLE.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "VEHOLD"	 TO APAC-KEY.
	     MOVE "VEHICLE.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "VSTOLD"	 TO APAC-KEY.
	     MOVE "V_INVENT.OLD" TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "WAROLD"	 TO APAC-KEY.
	     MOVE "WARHSE.OLD"	 TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     MOVE "WSTOLD"	 TO APAC-KEY.
	     MOVE "W_INVENT.OLD" TO APAC-NAME.
	     PERFORM OFFL-WRITE-APACFILE.
	     CLOSE APACFILE.

       OFFL-CREATE-NEW-EXIT.
	     EXIT.

       OFFLFILES-EXIT.
	     EXIT.

       OFFL-SETUP-PATHS        SECTION 5.
       OFFL-SETUP.
	     MOVE WS-SYS-ID	 TO W02-SYSPTH W02-SYSID.
	     OPEN INPUT APACFILE.
	     OPEN OUTPUT APACFIDS.
	     CLOSE APACFIDS.
	     OPEN I-O APACFIDS.
	     MOVE SPACES	 TO APAC-KEY.
000030	     START APACFILE
	       KEY >= APAC-KEY.

       OFFL-GET-NEXT-FILE-ID.
000320	     READ APACFILE NEXT WITH IGNORE LOCK.
000350	   IF NOT (WS-STAT1 = "0")
000370	       GO TO OFFL-SETUP-END.
	     MOVE APAC-KEY	 TO AFID-KEY.
	     MOVE SPACES	 TO AFID-PATH.
	     STRING APAC-PATH DELIMITED BY " "
		    APAC-NAME DELIMITED BY " "
		    INTO AFID-PATH.
	     WRITE AFID-RECORD1.
	     GO TO OFFL-GET-NEXT-FILE-ID.

       OFFL-SETUP-END.
	     CLOSE APACFILE
		   APACFIDS.

       OFFL-SETUP-EXIT.
	     EXIT.
