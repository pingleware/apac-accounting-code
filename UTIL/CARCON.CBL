      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******    ******   *******	 ******	   *****   **	 **    *
      *   **	**  **	  **  **    **	**    **  **   **  ***	 **    *
      *   **	    **	  **  **   **	**	  **   **  ****	 **    *
      *   **	    ********  ******	**	  **   **  ** ** **    *
      *   **	    **	  **  **   **	**	  **   **  **  ****    *
      *   **	**  **	  **  **    **	**    **  **   **  **	***    *
      *    ******   **	  **  **    **	 ******	   *****   **	 **    *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *  S A L E S   /	 R E T U R N S	 C A R D E X   C O N V E R T   *
      *                                                                *
      *     VERSION 8.14.03 - July 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CARCON.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   JULY 2010.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2010 - 2010
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

      $SET KEYCOMPRESS "7"
000010     SELECT CARDEX  ASSIGN DISK
000020			  LOCK EXCLUSIVE
000040                    STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
      *
      * 	Account Number / Stock Code / Date Sequence
      *
000070			  RECORD CRD-KEY
      *
      * 	Stock Code / Account Number Sequence
      *
			  ALTERNATE RECORD CRD-SKEY =
				    CRD-EXT-CODE CRD-AC DUPLICATES
      *
      * 	Stock Code / Reference (Invoice/Credit Note) Sequence
      *
			  ALTERNATE RECORD CRD-MKEY =
				    CRD-EXT-CODE CRD-REF DUPLICATES
      *
      * 	Account Number / Date Sequence
      *
			  ALTERNATE RECORD CRD-AKEY =
				    CRD-AC CRD-DTE DUPLICATES
      *
      * 	Date / Account Number Sequence
      *
			  ALTERNATE RECORD CRD-DKEY =
				    CRD-DTE CRD-AC DUPLICATES
      *
      * 	Date / Stock Code Sequence
      *
			  ALTERNATE RECORD CRD-DSKEY =
				    CRD-DTE CRD-EXT-CODE DUPLICATES
      *
      * 	Stock Code / Date Sequence
      *
			  ALTERNATE RECORD CRD-SDKEY =
				    CRD-EXT-CODE CRD-DTE DUPLICATES
      *
      * 	Sales Person / Date / Stock Code Sequence
      *
			  ALTERNATE RECORD CRD-SMKEY =
				    CRD-SMAN CRD-DTE CRD-EXT-CODE
				    DUPLICATES.
      $SET NOKEYCOMPRESS

000180	   SELECT CAROLD  ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS SEQUENTIAL
000210			  ORGANIZATION SEQUENTIAL.

       COPY CONTROL.SL.

000140 COPY PARAM.SL.

000140 COPY STOCK.SL.

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000140 COPY CARDEX.FD.

000160 COPY CAROLD.FD.

000290 COPY CONTROL.FDE.

000290 COPY PARAM.FDE.

000540 COPY STOCK.FDE.

      /
000430 WORKING-STORAGE SECTION.
000440 77  WS-S1	   PIC	9(04)	 COMP-5.
001890 77  WS-S2	   PIC	9(04)	 COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
000440 77  WS-SUB	   PIC	9(04)	 COMP-5.
003290 77  WS-NETKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003340 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
000520 77  WS-RECOVER	   PIC	9(05)	 COMP-0.
000530 77  WS-TRANS	   PIC	9(05)	 COMP-0 VALUE 1.
000540 77  WS-EKEY	   PIC	Z(04)9.
000560 77  WS-ERROR	   PIC	9(01)	 VALUE ZERO.
000570 77  WS-PROG	   PIC	X(02).
       77  WS-OPTION	   PIC	X(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440	       05  WS-TCHR     PIC  X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
	       05  FILLER      PIC  X(01).
	       05  WS-H-LINE   PIC  X(78).
	       05  FILLER      PIC  X(01).
002430     03  WS-TOP-LNE2.
002440	       05  WS-TCH      PIC  X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460	       05  WS-MCHR     PIC  X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480	       05  WS-BCHR     PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
	       05  FILLER      PIC  X(01).
	       05  WS-F-LINE   PIC  X(78).
	       05  FILLER      PIC  X(01).
002470     03  WS-BOT-LNE2.
002480	       05  WS-BCH      PIC  X(01) OCCURS 80.

000590 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

004000 01  W02-FID.

       COPY APACFIDS.ID.

000140 COPY CARDEX.ID.

000160 COPY CAROLD.ID.

000290 COPY CONTROL.ID.

       COPY PARAM.ID.

000140 COPY STOCK.ID.

       COPY W12.WS.

       COPY W40.WS.

       COPY FUNCTION.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
000850 SCREEN SECTION.

       COPY BLANK.CRT.

000860 01  S00.
000910	   03  LINE  2 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
000920		       VALUE "SALES CARDEX FILE CONVERTION PROGRAM".
000930     03  LINE  4 COLUMN  2 VALUE "Reading :".
000950	   03  LINE  6 COLUMN  2 VALUE "Writing :".
001080 01  S04.
001090	   03  LINE 12 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
001100		       VALUE "Convertion of CARDEX file completed".
001110	   03  LINE 14 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
001120                 VALUE "Press any key to continue".
001130 01  S05.
001140	   03  LINE 12 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
001150		       VALUE "Convertion unsuccessfull !".
001160	   03  LINE 14 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190	   03  LINE 16 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
001200                 VALUE "since creating the last backup disk/s".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
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
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
001250	     DISPLAY CLR-SCREEN.
001270       PERFORM ZA000.
           IF WS-ERROR = 9
               GO TO AA50.
001260       DISPLAY S00.

001340 AA05.
001350	     READ CAROLD
               AT END
               GO TO AA10.
	     DISPLAY CRO-KEY AT 0412
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
001600       PERFORM AW000.
             GO TO AA05.

001450 AA10.
044650	     MOVE 3		 TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	     MOVE "8.14"	 TO STK-VER.
	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
001830	     CLOSE CARDEX
		   CAROLD
		   STOCK
                   PARAM
001870             NETWORK.
001910	     DISPLAY S04.
	     ACCEPT WS-OPTION AT 1439
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
004990	     CALL "CBL_DELETE_FILE" USING W02-CAROLD
005030				    RETURNING WS-STATUS.

       AA15.
001920       EXIT PROGRAM.

001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.

       COPY LOCKED.RC2.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY STOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
001960 AW000           SECTION.
011840 AW01.
      *	     MOVE CRO-KEY	 TO CRD-KEY.
      *	     MOVE CRO-REF	 TO CRD-REF.
      *	     MOVE CRO-SMAN	 TO CRD-SMAN.
      *	     MOVE CRO-QNT	 TO CRD-QNT.
      *	     MOVE CRO-COST	 TO CRD-COST.
      *	     MOVE CRO-SELL	 TO CRD-SELL.
      *	     MOVE CRO-NAME	 TO CRD-NAME.
000250*	     MOVE CRO-SUPP	 TO CRD-SUPP.
      *	     MOVE CRO-WHS	 TO CRD-WHS.
      *	     MOVE CRO-TIME	 TO CRD-TIME.
	     MOVE CRO-REC1	 TO CRD-REC1.
	     MOVE CRD-EXT-CODE	 TO STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "ZZZZ"	 TO STK-LDG.
	     MOVE STK-LDG	 TO CRD-DEPT.
	     DISPLAY CRD-KEY AT 0612
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
002310	     WRITE CRD-REC1.
           IF WS-STAT1 NOT = "0"
	       MOVE 43		 TO WS-F-ERROR
               PERFORM WRITE-ERROR.

002440 AW999.
002450       EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

      /
002460 ZA000           SECTION.
002470 ZA00.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
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
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
	     MOVE "CAROLD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CAROLD.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
004990	     CALL "CBL_RENAME_FILE" USING W02-CARDEX
005000					  W02-CAROLD
005030				    RETURNING WS-STATUS.
	   IF NOT (WS-STATUS = X"0000")
	       MOVE "Unable to convert CARDEX file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       MOVE 9		 TO WS-ERROR
	       GO TO ZA999-EXIT.
042240	     OPEN OUTPUT CARDEX.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 43		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
054300	     OPEN INPUT STOCK.
054310     IF RUNTIME-ERROR
054320         IF FLE-LOCKED
054330             GO TO ZA200
054340         ELSE
054350         IF FLE-LIMIT
054360             GO TO ZA49
054370         ELSE
054380	       IF IDX-CORRUPT
054390             MOVE 22       TO WS-F-ERROR
054400             GO TO ZA50.
054410     IF WS-STATUS NOT = "00"
054420         MOVE 22           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054430		   PERFORM OPEN-ERROR.
042240	     OPEN INPUT CAROLD.
042250	   IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 93		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
002960	     GO TO ZA999-EXIT.

       COPY ZA49.PRO.

039850 ZA50.
	   IF WS-F-ERROR = 22
	       MOVE "STOCK file" TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990		      WS-FILE WITH FOREGROUND-COLOR 6 HIGHLIGHT.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

040020 ZA51.
040030	     EXIT PROGRAM.

020110 ZA200.

       COPY LOCKED.PRO.

056040 ZA205.
056050       EXIT PROGRAM.

003080 ZA999-EXIT.
003090       EXIT.
      /
003100 ZB000-ERROR           SECTION.
      
       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041640     ELSE
041650	   IF WS-F-ERROR = 43
041660	       MOVE W02-CARDEX TO WS-FILE
041670         MOVE ZERO       TO WS-KEY
041640     ELSE
041650	   IF WS-F-ERROR = 93
041660	       MOVE W02-CAROLD TO WS-FILE
041670         MOVE ZERO       TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY.

       COPY DISPERR.PRO.
