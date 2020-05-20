      ******************************************************************
      *                                                                *
      *    ******   *******   *******    ******    *****   **    **    *
      *   **    **  **    **  **    **  **    **  **   **  ***   **    *
      *   **        **   **   **    **  **        **   **  ****  **    *
      *   **        ******    *******   **        **   **  ** ** **    *
      *   **        **   **   **        **        **   **  **  ****    *
      *   **    **  **    **  **        **    **  **   **  **   ***    *
      *    ******   **    **  **         ******    *****   **    **    *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   C R E D I T O R   F I L E   C O N V E R T   P R O G R A M    *
      *                                                                *
      *     VERSION 8.06 - August 2002				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CRPCON.
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
000230     SELECT CREDIT  ASSIGN DISK
                          LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270                    RECORD P-NUMBER
                          ALTERNATE RECORD P-NKEY DUPLICATES.
000180     SELECT CREOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS SEQUENTIAL
000210                    ORGANIZATION SEQUENTIAL.
000180     SELECT CTRANS ASSIGN DISK
                         LOCK EXCLUSIVE
000190                   STATUS WS-STATUS
000200                   ACCESS RANDOM
000210                   ORGANIZATION RELATIVE
000220                   RELATIVE WS-CTRKEY.
000180     SELECT CRTOLD ASSIGN DISK
                         LOCK EXCLUSIVE
000190                   STATUS WS-STATUS
000200                   ACCESS RANDOM
000210                   ORGANIZATION RELATIVE
000220                   RELATIVE WS-CTRKEY.

       COPY PORDER.SL.

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FD.

000290 COPY CONTROL.IFD.

000290 COPY PARAM.IFD.

000140 COPY CREDIT.GFD.

000160 COPY CREOLD.FD.

       COPY CRTRAN.FD.

       COPY CRTOLD.FD.

       COPY PORDER.GFD.

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000440 77  WS-S1	    PIC 9(04)	  COMP-5.
000440 77  WS-SUB           PIC 9(04)     COMP-5.
003290 77  WS-NETKEY        PIC 9(04)     COMP-5 VALUE 1.
003290 77  WS-DEPKEY        PIC 9(04)     COMP-5 VALUE 1.
003300 77  WS-CTRKEY        PIC 9(04)     COMP-5 VALUE 1.
003340 77  WS-PARKEY        PIC 9(04)     COMP-5 VALUE 1.
003360 77  WS-RECKEY        PIC 9(04)     COMP-5.
000520 77  WS-RECOVER       PIC 9(05)     COMP-0.
000530 77  WS-TRANS         PIC 9(05)     COMP-0 VALUE 1.
000540 77  WS-EKEY          PIC Z(04)9.
000560 77  WS-ERROR         PIC 9(01)     VALUE ZERO.
000570 77  WS-PROG          PIC X(02).
       77  WS-OPTION        PIC X(01).
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

000590 COPY WS.WS.

000590 COPY CHAIN.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

004000 01  W02-FID.

       COPY APACFIDS.ID.

000290 COPY CONTROL.ID.

000140 COPY CREDIT.ID.

000160 COPY CREOLD.ID.

       COPY CRTRAN.ID.

       COPY CRTOLD.ID.

       COPY PARAM.ID.

       COPY PORDER.ID.

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
000910     03  LINE  2 COLUMN 26 FOREGROUND-COLOR 15
000920                 VALUE "CREDITOR FILE REBUILD PROGRAM".
000930     03  LINE  4 COLUMN  2 VALUE "Reading :".
000950     03  LINE  6 COLUMN  2 VALUE "Writing :".
000910     03  LINE  8 COLUMN  2 FOREGROUND-COLOR 15
000920                 VALUE "CREDITOR TRANSACTION FILE".
000930     03  LINE 10 COLUMN  2 VALUE "Reading :".
000950     03  LINE 12 COLUMN  2 VALUE "Writing :".
001080 01  S04.
001090     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001100                 VALUE "Rebuilt of CREDITOR file completed".
001110     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001120                 VALUE "Press any key to continue".
001130 01  S05.
001140     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001150                 VALUE "Rebuild unsuccessfull !".
001160     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001200                 VALUE "since creating the last backup disk/s".
      /
001210 PROCEDURE DIVISION.
001220 AA000           SECTION.
001230 AA00.
041220	     MOVE 1		 TO WS-S1.
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
001340 AA05.
001350       READ CREOLD
               AT END
               GO TO AA10.
             DISPLAY PO-NUMBER AT 0412 WITH FOREGROUND-COLOR 11
                     " " PO-NAME WITH FOREGROUND-COLOR 11.
001600       PERFORM AW000.
             GO TO AA05.
001450 AA10.
             MOVE ZERO           TO WS-CTRKEY.
001340 AA15.
             ADD 1               TO WS-CTRKEY.
001350       READ CRTOLD.
           IF WS-STATUS NOT = "00"
               GO TO AA20.
             DISPLAY WS-CTRKEY AT 1012 WITH FOREGROUND-COLOR 11.
001600       PERFORM AX000.
             GO TO AA15.
       AA20.
006070       OPEN OUTPUT PORDER.
             CLOSE PORDER.
             OPEN I-O PORDER.
001830       CLOSE CREDIT
                   CREOLD
                   CTRANS
                   CRTOLD
                   PARAM
                   PORDER
001870             NETWORK.
001910       DISPLAY S04.
             ACCEPT WS-OPTION AT 1439 WITH FOREGROUND-COLOR 15 AUTO.
004990       CALL "_MSREMOVE" USING BY REFERENCE W02-CREOLD
005000                              BY CONTENT LENGTH OF W02-CREOLD
005030                              BY REFERENCE WS-STATUS.
004990       CALL "_MSREMOVE" USING BY REFERENCE W02-CRTOLD
005000                              BY CONTENT LENGTH OF W02-CRTOLD
005030                              BY REFERENCE WS-STATUS.
       AA25.
001920	     STOP RUN.
001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.
      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
001960 AW000           SECTION.
011840 AW01.
             INITIALIZE P-REC.
000520       MOVE PO-NUMBER      TO P-NUMBER.
000610       MOVE PO-TLK         TO P-TLK.
000530       MOVE PO-NAME        TO P-NAME.
000540       MOVE PO-TERMS       TO P-TERMS.
000620       MOVE PO-ADDR        TO P-ADDR.
000630       MOVE PO-PC          TO P-PC.
000620       MOVE PO-CONTACT     TO P-CONTACT.
000620       MOVE PO-TEL         TO P-TEL.
000640       MOVE PO-BAL         TO P-BAL.
000650       MOVE PO-DT          TO P-DT.
000660       MOVE PO-CR          TO P-CR.
000670       MOVE PO-PBAL        TO P-PBAL.
000680       MOVE PO-FIRST       TO P-FIRST.
000690       MOVE PO-LAST        TO P-LAST.
000700       MOVE PO-30D         TO P-30D.
000710       MOVE PO-60D         TO P-60D.
000720       MOVE PO-90D         TO P-90D.
000730       MOVE PO-120D        TO P-120D.
000740       MOVE PO-150D        TO P-150D.
000750       MOVE PO-180D        TO P-180D.
000760       MOVE PO-OUT         TO P-OUT.
             MOVE PO-VAT         TO P-VAT.
             MOVE PO-TAX         TO P-TAX.
             DISPLAY P-NUMBER AT 0612 WITH FOREGROUND-COLOR 14
000010                      " " P-NAME WITH FOREGROUND-COLOR 14.
002310       WRITE P-REC.
           IF WS-STAT1 NOT = "0"
               MOVE 3            TO WS-F-ERROR
               PERFORM WRITE-ERROR.
002440 AW999.
002450       EXIT.
      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
001960 AX000           SECTION.
011840 AX01.
             INITIALIZE CT-REC.
             MOVE CTO-CODE       TO CT-CODE.
             MOVE CTO-DTE        TO CT-DTE.
             MOVE CTO-REF        TO CT-REF.
             MOVE CTO-SINV       TO CT-SINV.
             MOVE CTO-SDTE       TO CT-SDTE.
             MOVE CTO-ORD        TO CT-ORD.
             MOVE CTO-COST       TO CT-COST.
             MOVE CTO-TAXFREE    TO CT-TAXFREE.
             MOVE CTO-SELL       TO CT-SELL.
             MOVE CTO-TAX        TO CT-TAX.
             MOVE CTO-AGE        TO CT-AGE.
             MOVE CTO-LINK       TO CT-LINK.
             MOVE CTO-PJLNK      TO CT-PJLNK.
             MOVE CTO-CRED       TO CT-CRED.
             DISPLAY WS-CTRKEY AT 1212 WITH FOREGROUND-COLOR 14.
002310       WRITE CT-REC.
           IF WS-STAT1 NOT = "0"
               MOVE 4            TO WS-F-ERROR
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
002650       MOVE WS-DSK15       TO W02-DSK2 
                                    W02-DSK3 
                                    W02-DSK4 
                                    W02-DSK16
                                    W02-DSK3O
                                    W02-DSK4O.
002660	     MOVE WS-EXT15	 TO W02-PTH2
				    W02-PTH3
				    W02-PTH4
				    W02-PTH16
				    W02-PTH3O
				    W02-PTH4O.
042140       OPEN I-O NETWORK.
042150     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042160             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 2 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
004990       CALL "_MSRENAME" USING BY REFERENCE W02-CREDIT
005000                              BY CONTENT LENGTH OF W02-CREDIT
004990                              BY REFERENCE W02-CREOLD
005000                              BY CONTENT LENGTH OF W02-CREOLD
005030                              BY REFERENCE WS-STATUS.
004990       CALL "_MSRENAME" USING BY REFERENCE W02-CRTRAN
005000                              BY CONTENT LENGTH OF W02-CRTRAN
004990                              BY REFERENCE W02-CRTOLD
005000                              BY CONTENT LENGTH OF W02-CRTOLD
005030                              BY REFERENCE WS-STATUS.
042240       OPEN INPUT CREOLD.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 53 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240       OPEN OUTPUT CREDIT.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 3 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240       OPEN INPUT CRTOLD.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 54 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240       OPEN OUTPUT CTRANS.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 4 TO WS-F-ERROR
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
041650     IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT TO WS-FILE
041670         MOVE ZERO       TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 4
041660         MOVE W02-CRTRAN TO WS-FILE
041670         MOVE WS-CTRKEY  TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 53
041660         MOVE W02-CREOLD TO WS-FILE
041670         MOVE ZERO       TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 54
041660         MOVE W02-CRTOLD TO WS-FILE
041670         MOVE WS-CTRKEY  TO WS-KEY
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
