      $set LINKCOUNT"384"
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
      *     VERSION 8.09 - March 2004				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DEBCON.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2004
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

       COPY CONTROL.SL.

       COPY PARAM.SL.

       COPY DEBTOR.SL.

000180	   SELECT DEBOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS SEQUENTIAL
000210                    ORGANIZATION SEQUENTIAL.
      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000290 COPY CONTROL.FDE.

000290 COPY PARAM.FDE.

000140 COPY DEBTOR.FDE.

      /
000010 FD  DEBOLD    LABEL RECORD STANDARD
000020               VALUE OF FILE-ID W02-DEBOLD
000220               RECORDING MODE IS VARIABLE.
000030 01  DBO-RECORD1.
000040	   03  DBO-ACNO        PIC  X(06).
000050	   03  DBO-TYPE        PIC  9(01).
000060	   03  DBO-CATEGORY.
000070	       05  DBO-CAT     PIC  9(02).
000150	   03  DBO-TITLE       PIC  9(02)    COMP-5.
000080	   03  DBO-NAME.
000090	       05  DBO-NKEY.
		 07  DBO-NCASH PIC  X(09).
                 07  FILLER    PIC  X(03).
000100         05  FILLER      PIC  X(28).
000110	   03  DBO-ADDRESS.
	       05  DBO-ADD     PIC  X(60).
               05  FILLER      PIC  X(10).
000120	   03  DBO-PADDR.
	       05  DBO-PAD     PIC  X(60).
               05  FILLER      PIC  X(10).
000130	   03  DBO-POST        PIC  9(04)    COMP-5.
000140	   03  DBO-PPOST       PIC  9(04)    COMP-5.
000160	   03  DBO-H-TEL       PIC  X(14).
000170	   03  DBO-B-TEL       PIC  X(14).
000170	   03  DBO-FAX	       PIC  X(14).
000180	   03  DBO-ID	       PIC  X(14).
000190	   03  DBO-TAX REDEFINES DBO-ID
                               PIC  X(14).
	   03  DBO-CONTACT     PIC  X(30).
000200	   03  DBO-ACTIVE      PIC  9(08)    COMP-3.
000210	   03  DBO-STM	       PIC S9(09)V99 COMP-3.
000220	   03  DBO-BAL	       PIC S9(09)V99 COMP-3.
000230	   03  DBO-CUR	       PIC S9(09)V99 COMP-3.
000240	   03  DBO-30	       PIC S9(09)V99 COMP-3.
000250	   03  DBO-60	       PIC S9(09)V99 COMP-3.
000260	   03  DBO-90	       PIC S9(09)V99 COMP-3.
000270	   03  DBO-120	       PIC S9(09)V99 COMP-3.
000280	   03  DBO-DEBT        PIC S9(09)V99 COMP-3.
000290	   03  DBO-CRED        PIC S9(09)V99 COMP-3.
000300	   03  DBO-PURYTD      PIC S9(09)V99 COMP-3.
000310	   03  DBO-PURPRV      PIC S9(09)V99 COMP-3.
000320	   03  DBO-INT	       PIC S9(05)V99 COMP-3.
000320	   03  DBO-VAT	       PIC S9(05)V99 COMP-3.
000330	   03  DBO-CRL	       PIC  9(07)    COMP-3.
000340	   03  DBO-FINANCE.
000350	       05  DBO-FIN     PIC  9(01).
000360	   03  DBO-TERMS       PIC  X(01).
000370	   03  DBO-BDT	       PIC  9(01).
000380	   03  DBO-LANG        PIC  X(01).
000390	   03  DBO-PMT	       PIC  9(08)    COMP-3.
	   03  DBO-INV-TYPE    PIC  X(01).
	   03  DBO-LAST-PMT    PIC S9(09)V99 COMP-3.
	   03  DBO-LAB	       PIC  X(01).
	   03  DBO-OI	       PIC  X(01).
	   03  DBO-PRICE       PIC  X(01).
	   03  DBO-ART11       PIC  X(01).
000400	   03  DBO-SDISC       PIC  9(02)    COMP-5.
000410	   03  DBO-DISC        PIC  9(02)    COMP-5.
000420	   03  DBO-SMAN        PIC  9(02)    COMP-5.
	   03  DBO-CONAC       PIC  X(06).
	   03  DBO-PURMTH.
	       05  DBO-MTHPUR  PIC S9(09)V99 COMP-3 OCCURS 12.
	   03  DBO-AREA        PIC  X(08).
	   03  DBO-PMTMTH.
	       05  DBO-PMTS OCCURS 12.
		   07  DBO-MTHPMT  PIC S9(09)V99 COMP-3.
      *
      *    ****   INDICATOR
      * 	  C = STATEMENT BALANCE
      * 	  S = SHORT PAID
      * 	  1 = 30 DAYS
      * 	  2 = 60 DAYS
      * 	  3 = 90 DAYS
      * 	  4 = 120 + DAYS
      *
		   07  DBO-MTHPRF  PIC	X(01).
		   07  DBO-MTHDTE  PIC	9(08)	 COMP-5.

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000440 77  WS-S1	    PIC 9(04)	  COMP-5.
001890 77  WS-S2	   PIC	9(04)	 COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
000440 77  WS-SUB	    PIC 9(04)	  COMP-5.
003290 77  WS-NETKEY	   PIC 9(04)	 COMP-5 VALUE 1.
003340 77  WS-PARKEY       PIC 9(04)     COMP-5 VALUE 1.
003360 77  WS-RECKEY       PIC 9(04)     COMP-5.
000520 77  WS-RECOVER      PIC 9(05)     COMP-0.
000530 77  WS-TRANS        PIC 9(05)     COMP-0 VALUE 1.
000540 77  WS-EKEY         PIC Z(04)9.
000560 77  WS-ERROR        PIC 9(01)     VALUE ZERO.
000570 77  WS-PROG         PIC X(02).
       77  WS-OPTION       PIC X(01).
       77  WS-JUST         PIC X(01) VALUE "N".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
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

       COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

004000 01  W02-FID.

       COPY APACFIDS.ID.

000290 COPY CONTROL.ID.

000140 COPY DEBTOR.ID.

000160 COPY DEBOLD.ID.

       COPY PARAM.ID.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

       01  W15-EDIT.
           03  W15-ACNO.
               05  W15-NO      PIC  Z(04)9.
           03  W15-AC REDEFINES W15-ACNO.
               05  W15-1ST     PIC  X(01).
               05  W15-2TO5    PIC  X(04).

       COPY W40.WS.

       COPY FUNCTION.WS.

       LINKAGE SECTION.

000590 COPY CHAIN.LS.

      /
000850 SCREEN SECTION.

       COPY BLANK.CRT.

000860 01  S00.
000910     03  LINE  2 COLUMN 27 FOREGROUND-COLOR 15
000920		       VALUE "DEBTOR FILE CONVERT PROGRAM".
000930     03  LINE  4 COLUMN  2 VALUE "Reading :".
000950     03  LINE  6 COLUMN  2 VALUE "Writing :".
001080 01  S04.
001090     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001100		       VALUE "Convertion of DEBTOR file completed".
001110     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001120                 VALUE "Press any key to continue".
001130 01  S05.
001140     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001150                 VALUE "Convertion unsuccessfull !".
001160     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION USING LS-PARID LS-USER-ID.
001220 AA000	       SECTION.
001230 AA00.
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE WS-MID-LNE2.
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
				    WS-MCHR(1)	WS-MCHR(80)
				    WS-MCH(1)	WS-MCH(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(16) WS-BCHR(41)
021980				    WS-BCHR(64) WS-BCHR(71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
001250	     DISPLAY CLR-SCREEN.
001270	     PERFORM ZA000.
           IF WS-ERROR = 9
               GO TO AA50.
001260       DISPLAY S00.
001340 AA05.
001350       READ DEBOLD.
           IF WS-STATUS NOT = "00"
               GO TO AA10.
	     DISPLAY DBO-ACNO AT 0412 WITH FOREGROUND-COLOR 11
                     " " DBO-NAME WITH FOREGROUND-COLOR 11.
001600       PERFORM AW000.
             GO TO AA05.

001450 AA10.
044650	     MOVE 1		 TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	     MOVE "8.09"	 TO DEB-VER.
	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
001830	     CLOSE DEBTOR
		   DEBOLD.
001910       DISPLAY S04.
             ACCEPT WS-OPTION AT 1639 WITH FOREGROUND-COLOR 15 AUTO.
004990	     CALL "CBL_DELETE_FILE" USING W02-DEBOLD
005030				    RETURNING WS-STATUS.

       AA25.
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

       COPY PARAM.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
001960 AW000           SECTION.
011840 AW00.
	     INITIALIZE DEB-RECORD1.
000040	     MOVE DBO-ACNO	 TO DEB-ACNO.
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
	     MOVE DBO-FAX	 TO DEB-FAX.
000180	     MOVE DBO-ID	 TO DEB-ID.
	     MOVE SPACES	 TO DEB-TAX.
	     MOVE DBO-CONTACT	 TO DEB-CONTACT.
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
	     MOVE DBO-INV-TYPE	 TO DEB-INV-TYPE.
	     MOVE DBO-LAST-PMT	 TO DEB-LAST-PMT.
	     MOVE DBO-LAB	 TO DEB-LAB.
	     MOVE DBO-OI	 TO DEB-OI.
	     MOVE DBO-PRICE	 TO DEB-PRICE.
	     MOVE DBO-ART11	 TO DEB-ART11.
000400       MOVE DBO-SDISC      TO DEB-SDISC.
000410       MOVE DBO-DISC       TO DEB-DISC.
000420       MOVE DBO-SMAN       TO DEB-SMAN.
000430	     MOVE DBO-CONAC	 TO DEB-CONAC.
000440	     MOVE DBO-PURMTH	 TO DEB-PURMTH.
000430	     MOVE DBO-AREA	 TO DEB-AREA.
000440	     MOVE DBO-PMTMTH	 TO DEB-PMTMTH.
             DISPLAY DEB-ACNO AT 0612 WITH FOREGROUND-COLOR 14
                     " " DEB-NAME WITH FOREGROUND-COLOR 14.
002310       WRITE DEB-RECORD1.
           IF WS-STAT1 NOT = "0"
               MOVE 6            TO WS-F-ERROR
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
	     MOVE "DEBOLD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBOLD.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
002480	     OPEN I-O PARAM.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130             GO TO ZA200.
002490	   IF NOT (WS-STATUS = "00" OR "41")
002500	       MOVE 15		 TO WS-F-ERROR
002510         PERFORM OPEN-ERROR.
002520	     MOVE 1		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
002540	   IF WS-STATUS NOT = "00"
002550	       MOVE 15		 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
	     MOVE PAR-DMY	 TO W12-TODAY.
042140	     OPEN I-O NETWORK.
042150     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042160             GO TO ZA200.
003050	   IF NOT (WS-STATUS = "00" OR "41")
003060	       MOVE 2		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
004990	     CALL "CBL_RENAME_FILE" USING W02-DEBTOR
005000					  W02-DEBOLD
005030				    RETURNING WS-STATUS.
	   IF NOT (WS-STATUS = X"0000")
	       MOVE "Unable to Rebuild DEBTOR file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       MOVE 9		 TO WS-ERROR
	       GO TO ZA999-EXIT.
042240	     OPEN INPUT DEBOLD.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 56		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240       OPEN OUTPUT DEBTOR.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 6		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
002960	     GO TO ZA999-EXIT.
020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2412
020130                WITH FOREGROUND-COLOR 14.
020140       DISPLAY "Press any key" AT 2512 
020150                WITH FOREGROUND-COLOR 14.
020160       ACCEPT WS-OPTION AT 2526 WITH FOREGROUND-COLOR 15 AUTO.
	     MOVE 9		 TO WS-ERROR.
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
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041640	   ELSE
041650	   IF WS-F-ERROR = 6
041660	       MOVE W02-DEBTOR	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041640     ELSE
041650     IF WS-F-ERROR = 56
041660	       MOVE W02-DEBOLD	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE DBO-ACNO	 TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 15
041860	       MOVE W02-PARAM	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.
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
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
