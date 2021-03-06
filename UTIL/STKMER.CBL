      ******************************************************************
      *                                                                *
      *    ******   ********  **    **	**     **  *******  *******    *
      *   **	**     **     **   **	***   ***  **	    **	  **   *
      *   **	       **     **  **	** * * **  **	    **	 **    *
      *    ******      **     *****	**  *  **  *****    ******     *
      * 	**     **     **  **	**     **  **	    **	 **    *
      *   **	**     **     **   **	**     **  **	    **	  **   *
      *    ******      **     **    **	**     **  *******  **	  **   *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   S T O C K   F I L E	M E R G E   P R O G R A M	       *
      *                                                                *
      *     VERSION 8.06 - August 2002				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STKMER.
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

       COPY DEPART.SL.

	   SELECT DEPTADD ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS SEQUENTIAL
000210			  ORGANIZATION SEQUENTIAL.

000140 COPY PARAM.SL.

       COPY STOCK.SL.

000180	   SELECT STKADD  ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS SEQUENTIAL
000210                    ORGANIZATION SEQUENTIAL.
      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FD.

       COPY DEPART.GFD.

       COPY DEPTADD.FD.

000290 COPY PARAM.IFD.

000140 COPY STOCK.GFD.

000160 COPY STKADD.FD.

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
003290 77  WS-NETKEY        PIC 9(04)     COMP-5 VALUE 1.
003340 77  WS-PARKEY        PIC 9(04)     COMP-5 VALUE 1.
003360 77  WS-RECKEY        PIC 9(04)     COMP-5.
000520 77  WS-RECOVER       PIC 9(05)     COMP-0.
000530 77  WS-TRANS         PIC 9(05)     COMP-0 VALUE 1.
000540 77  WS-EKEY          PIC Z(04)9.
000560 77  WS-ERROR         PIC 9(01)     VALUE ZERO.
000570 77  WS-PROG          PIC X(02).
       77  WS-OPTION        PIC X(01).
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

000590 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

004000 01  W02-FID.

       COPY APACFIDS.ID.

       COPY DEPART.ID.

       COPY DEPTADD.ID.

       COPY PARAM.ID.

000140 COPY STOCK.ID.

000160 COPY STKADD.ID.

       COPY W12.WS.

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

       COPY W40.WS.

       COPY FUNCTION.WS.

       COPY CHAIN.WS.

      /
000850 SCREEN SECTION.

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
004190				 VALUE "Version - 7.00".
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
000910     03  LINE  2 COLUMN 28 FOREGROUND-COLOR 15
000920		       VALUE "STOCK FILE MERGE PROGRAM".
	   03  LINE  4 COLUMN  3 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "STOCK FILE".
000930	   03  LINE  6 COLUMN  3 VALUE "Reading :".
000950	   03  LINE  8 COLUMN  3 VALUE "Writing :".
	   03  LINE 10 COLUMN  3 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "DEPARTMENT FILE".
000930	   03  LINE 12 COLUMN  3 VALUE "Reading :".
000950	   03  LINE 14 COLUMN  3 VALUE "Writing :".

001080 01  S04.
001090	   03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001100		       VALUE "Merge of STOCK file completed".
001110	   03  LINE 18 COLUMN 12 FOREGROUND-COLOR 11
001120                 VALUE "Press any key to continue".

001130 01  S05.
001140	   03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001150		       VALUE "Merge unsuccessfull !".
001160	   03  LINE 18 COLUMN 12 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190	   03  LINE 20 COLUMN 12 FOREGROUND-COLOR 11
001200                 VALUE "since creating the last backup disk/s".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION.
001220 AA000	       SECTION.
001230 AA00.
001270	     PERFORM ZA000.
           IF WS-ERROR = 9
               GO TO AA50.
001260       DISPLAY S00.

001340 AA05.
001350	     READ STKADD
               AT END
               GO TO AA10.
	     DISPLAY STA-CODE AT 0613 WITH FOREGROUND-COLOR 11
		     " " STA-DESC WITH FOREGROUND-COLOR 11.
	     MOVE STA-CODE	 TO STK-CODE.
000020	     READ STOCK WITH IGNORE LOCK
000030			KEY IS STK-CODE.
000040	   IF WS-STATUS = "00"
000060	       GO TO AA05.
001600	     PERFORM AB000.
             GO TO AA05.

001450 AA10.
001350	     READ DEPTADD
               AT END
	       GO TO AA15.
	   IF DPA-CODE = "XXXX" OR "YYYY" OR "VAT1" OR
			 "VAT2" OR "VAT3" OR "VAT4" OR
			 "VAT5" OR "VAT6"
	       GO TO AA10.
	     DISPLAY DPA-CODE AT 1213 WITH FOREGROUND-COLOR 11
		     " " DPA-DESC WITH FOREGROUND-COLOR 11.
	     MOVE DPA-CODE	 TO DPT-CODE.
000020	     READ DEPART WITH IGNORE LOCK
000030			 KEY IS DPT-CODE.
000040	   IF WS-STATUS = "00"
000060	       GO TO AA10.
001600	     PERFORM AC000.
	     GO TO AA10.

       AA15.
001830	     CLOSE STOCK
		   STKADD
		   DEPART
001870		   DEPTADD.
001910       DISPLAY S04.
             ACCEPT WS-OPTION AT 1439 WITH FOREGROUND-COLOR 15 AUTO.

       AA45.
001920       EXIT PROGRAM.

001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.
      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE MASTER FILE.
      *
001960 AB000	       SECTION.
011840 AB01.
011870	     MOVE STA-RECORD1	 TO STK-RECORD1.
	     DISPLAY STK-CODE AT 0813 WITH FOREGROUND-COLOR 14
                     " " STK-DESC WITH FOREGROUND-COLOR 14.
015780	     MOVE ZERO		 TO STK-QUANT
000180				    STK-ORD
000200				    STK-MTD
000210				    STK-MTDC
000220				    STK-MTDV
000230				    STK-YTD
000240				    STK-YTDC
000250				    STK-YTDV
000260				    STK-WK1
000270				    STK-WK2
000280				    STK-WK3
000290				    STK-WK4
000260				    STK-MTH1
000270				    STK-MTH2
000280				    STK-MTH3
000290				    STK-MAVG
000330				    STK-ACT
000340				    STK-PUR
000350				    STK-DTE.
002310       WRITE STK-RECORD1.
	   IF WS-STATUS = "22"
	       GO TO AB999.
           IF WS-STAT1 NOT = "0"
               MOVE 22           TO WS-F-ERROR
               PERFORM WRITE-ERROR.

002440 AB999.
002450       EXIT.

      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE MASTER FILE.
      *
001960 AC000	       SECTION.
011840 AC01.
011870	     MOVE DPA-RECORD	 TO DPT-RECORD.
	     DISPLAY DPT-CODE AT 1413 WITH FOREGROUND-COLOR 14
		     " " DPT-DESC WITH FOREGROUND-COLOR 14.
	     MOVE ZERO		 TO DPT-SALES-DAY
				    DPT-SALES-MTD
				    DPT-SALES-YTD
				    DPT-RET-DAY
				    DPT-RET-MTD
				    DPT-RET-YTD
				    DPT-DISC-DAY
				    DPT-DISC-MTD
				    DPT-DISC-YTD
				    DPT-COST-DAY
				    DPT-COST-MTD
				    DPT-COST-YTD
				    DPT-CRET-DAY
				    DPT-CRET-MTD
				    DPT-CRET-YTD
				    DPT-PUR-DAY
				    DPT-PUR-MTD
				    DPT-PUR-YTD
				    DPT-PRET-DAY
				    DPT-PRET-MTD
				    DPT-PRET-YTD
				    DPT-ADJ-DAY
				    DPT-ADJ-MTD
				    DPT-ADJ-YTD
				    DPT-TRF-DAY
				    DPT-TRF-MTD
				    DPT-TRF-YTD
				    DPT-SALE-GL
				    DPT-DISC-GL
				    DPT-COST-GL
				    DPT-PUR-GL.
002310	     WRITE DPT-RECORD.
	   IF WS-STATUS = "22"
	       GO TO AC999.
           IF WS-STAT1 NOT = "0"
	       MOVE 7		 TO WS-F-ERROR
               PERFORM WRITE-ERROR.

002440 AC999.
002450       EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

      /
002460 ZA000           SECTION.
002470 ZA00.
	     ACCEPT W25-FACT FROM DATE.
	     MOVE W25-YY	 TO W12-YEAR.
	     MOVE W25-MM	 TO W12-MONTH.
	     MOVE W25-DD	 TO W12-DAY.
	   IF W25-YY < 50
	       MOVE 20		 TO W12-CENT
	   ELSE
	       MOVE 19		 TO W12-CENT.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
	     PERFORM ZA55 THRU ZA60.
044150	     DISPLAY CLR-SCREEN.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	       GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.




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




	     DISPLAY "System file identification" AT 1212.
	     ACCEPT W02-PTH7 AT 1239
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
002660	     MOVE W02-PTH7	 TO W02-PTH22
				    W02-PTH7A
				    W02-PTH22A.
      *
      *    ****    D E P A R T M E N T	 /   S A L E S	 L E D G E R
      * 	   F I L E
      *
038810	     OPEN I-O DEPART.
038820	   IF RUNTIME-ERROR
038830         IF FLE-LOCKED
038840		   GO TO ZA200.
038910     IF WS-STATUS NOT = "00"
038920	       MOVE 7		 TO WS-F-ERROR
038930         PERFORM OPEN-ERROR.
042240	     OPEN I-O STOCK.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 22		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042250	   IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
042240	     OPEN INPUT DEPTADD.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 57		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240	     OPEN INPUT STKADD.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 72		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
002960	     GO TO ZA999-EXIT.

039850 ZA50.
           IF WS-F-ERROR = 22
	       MOVE "STOCK file" TO WS-FILE.
039980	     DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870       MOVE SPACES         TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70).
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
      *      MOVE WS-COMPANY	 TO WS-TOP-COMP.
020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2412
020130                WITH FOREGROUND-COLOR 14.
020140       DISPLAY "Press any key" AT 2512 
020150		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
020160	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
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
051960	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
052070	   ELSE
041650	   IF WS-F-ERROR = 22
041660         MOVE W02-STOCKF   TO WS-FILE
052220	       MOVE ZERO	 TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041640	   ELSE
051960	   IF WS-F-ERROR = 57
041820	       MOVE W02-DEPTADD	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPA-CODE	 TO WS-KEYX
052070	   ELSE
041650	   IF WS-F-ERROR = 72
041660	       MOVE W02-STKADD	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY.
041880     IF WS-STATUS = "10"
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
052480	     DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
052490       DISPLAY "Status " AT 1212 
052500                WS-STATUS WITH FOREGROUND-COLOR 11
052510               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
052520     IF WS-STATUS NOT = "94" 
052530         DISPLAY "Key    " AT 1412 
052540                  WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
052550         DISPLAY "Reverse backup or contact program Support"
052560                  AT 1612.
052570         DISPLAY "Please make a note of these details" AT 1812.
042170	     STOP RUN.
