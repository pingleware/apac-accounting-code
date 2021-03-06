      $SET LINKCOUNT"458"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	   ******	**     *
      *   **	**  **	  **  **    **	 **  **   **	**     ***     *
      *   **	    **	  **  **    **	**    **  **	**    * **     *
      *   **	    ******    *******	**    **   *******   *	**     *
      *   **	    **	 **   **	**    **	**  *******    *
      *   **	**  **	  **  **	 **  **   **	**	**     *
      *    ******   **	  **  **	  ****	   ******      ****    *
      * 							       *
      *       ENGLISH                                                  *
      *                                                                *
      *     C R E D I T O R   D E L E T E   O R D E R S 	       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CRP094.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2011
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

       COPY APACFIDS.SL.

000120 COPY CONTROL.SL.

000160 COPY CREDIT.SL.

000120 COPY PARAM.SL.

000200 COPY PORDER.SL.

000140 COPY STOCK.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000310 COPY CREDIT.FDE.

000290 COPY PARAM.FDE.

000520 COPY PORDER.FDE.

000540 COPY STOCK.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
001550 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001560 77  WS-SUB	   PIC	9(04)	 COMP-5.
001560 77  WS-S            PIC  9(04)    COMP-5.
001570 77  WS-S1           PIC  9(04)    COMP-5.
001580 77  WS-S2           PIC  9(04)    COMP-5.
001590 77  WS-S3           PIC  9(04)    COMP-5.
001600 77  WS-S4           PIC  9(04)    COMP-5.
001610 77  WS-S5           PIC  9(04)    COMP-5.
001620 77  WS-S6           PIC  9(04)    COMP-5.
001630 77  WS-S7           PIC  9(04)    COMP-5.
001640 77  WS-S8           PIC  9(04)    COMP-5.
001650 77  WS-S9           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
002860 77  WS-IXS2         PIC  9(04)    COMP-5.
002870 77  WS-IXS3         PIC  9(04)    COMP-5.
002880 77  WS-IXS4         PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001710 77  WS-RECKEY       PIC  9(06)    COMP-5.
001720 77  WS-RECOVER      PIC  9(06)    COMP-5.
001730 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
001760 77  WS-PERIOD	   PIC	9(02).
001770 77  WS-NTP          PIC  X(01).
001810 77  WS-KEYSTR	   PIC	9(06)	 COMP-5.
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-SKIP         PIC  X(01) VALUE "N".
001820 77  WS-TAX          PIC  X(01).
001820 77  WS-TYPE         PIC  X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
001870 77  WS-EKEY         PIC  9(06)    COMP-5.
001900 77  WS-COMP	   PIC	9(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
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
	       05  FILLER      PIC  X(01) VALUE "│".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "░".
	       05  FILLER      PIC  X(01) VALUE "│".
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
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002230 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY CREDIT.ID.

       COPY PARAM.ID.

000360 COPY PORDER.ID.

000300 COPY STOCK.ID.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

001650	   03  W15-SETT      PIC  9(06).
001660     03  W15-SET       REDEFINES W15-SETT.
001670         05  W15-30    PIC  9(01).
001680         05  W15-60    PIC  9(01).
001690         05  W15-90    PIC  9(01).
001700         05  W15-120   PIC  9(01).
001710         05  W15-150   PIC  9(01).
001720         05  W15-180   PIC  9(01).
001730     03  W15-PAY       REDEFINES W15-SETT.
001740         05  W15-MONTH PIC  9(01)     OCCURS 6.

       COPY W20.WS.

002850 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC 9(04).
	       05  W25-SUB     PIC 9(01).
	       05  FILLER      PIC 9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.

001910 01  W25-VALUES.
001920     03  W25-CFLOW OCCURS 6 
                             PIC S9(07)V99 COMP-3.
001930     03  W25-V1        PIC S9(07)V99 COMP-3.
001940     03  W25-V2        PIC S9(07)V99 COMP-3.

       01  W30-AGE-DATES.
	   03  W30-120	       PIC  9(08) COMP-3.
	   03  W30-90	       PIC  9(08) COMP-3.
	   03  W30-60	       PIC  9(08) COMP-3.
	   03  W30-30	       PIC  9(08) COMP-3.
	   03  W30-CUR	       PIC  9(08) COMP-3.

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

003530 COPY W50.WS.

003550 01  W60-NME-ADD.
003560     03  W60-NAME.
003570         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
	   03  W75-S	       PIC  9(02) COMP-5.
	   03  W75-S1	       PIC  9(02) COMP-5.
	   03  W75-KEY	       PIC  9(06)    COMP-5.
	   03  W75-ALLOCATED   PIC S9(09)V99 COMP-3.
	   03  W75-BALANCE     PIC S9(09)V99 COMP-3.
	   03  W75-DNO	       PIC  X(06) OCCURS 18.

003910 01  W80-ACCOUNT.
003920	   03  W80-ACNO        PIC  X(06).
003940	   03  W80-TYPE        PIC  9(01).

006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.

003980 01  W90-TRAN.
004090     03  W90-120         PIC S9(09)V99 COMP-3.
004090     03  W90-90          PIC S9(09)V99 COMP-3.
004100     03  W90-60          PIC S9(09)V99 COMP-3.
004110     03  W90-30          PIC S9(09)V99 COMP-3.
004120     03  W90-CUR         PIC S9(09)V99 COMP-3.
004130     03  W90-BAL         PIC S9(09)V99 COMP-3.
004370     03  W90-IDTE        PIC 9(06)     COMP-3.
004380     03  W90-INT         PIC S9(09)V99 COMP-3.

004390 01  W95-STM.
004400     03  W95-COMP        PIC X(40).
004410     03  W95-V1.
004420         05  W95-T       PIC Z(04)9.99.
004430     03  W95-V2.
004440         05  W95-B       PIC Z(06)9.99-.
004450     03  W95-D.
004460	       05  W95-DTE     PIC Z9/99/9999.
004470     03  W95-POST        PIC 9(04).
004480     03  W95-TEL         PIC X(08).

004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99.
004500     03  W100-BAL        PIC Z(08)9.99-.
004510     03  W100-CUR        PIC Z(08)9.99-.
004520     03  W100-30         PIC Z(08)9.99-.
004530     03  W100-60         PIC Z(08)9.99-.
004540     03  W100-90         PIC Z(08)9.99-.
004540     03  W100-120        PIC Z(08)9.99-.
004550     03  W100-INT        PIC Z(08)9.99.
004560     03  W100-CRL        PIC Z(06)9.  
004570     03  W100-KEY        PIC Z(05)9.

       LINKAGE SECTION.

       COPY CHAIN.LS.

004580 SCREEN SECTION.

       COPY BLANK.CRT.

007640 01  S27.
007650	   03  LINE  5 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007660                           VALUE "This routine does not include a
007670-                          "recovery procedure. Security".
007680	   03  LINE  6 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007690                           VALUE "copies  of the data files should
007700-                          " be made as a power  failure".
007710	   03  LINE  7 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007720                           VALUE "or any other problem during this
007730-                          " procedure would require the".
007740	   03  LINE  8 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007750                           VALUE "DATA FILES from the previous bac
007760-                          "kup to be restored !!!      ".
007770	   03  LINE 10 COLUMN 12 VALUE "Do you wish to continue (".
	   03	       COLUMN 37 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 38 VALUE "/".
	   03	       COLUMN 39 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 40 VALUE ")?".

       COPY ERROR.CRT.

       COPY UPDATE.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008230 AA000		       SECTION.
008240 AA00.
003240	   IF LS0-CRLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
008250	     PERFORM ZA000-INIT.
	     PERFORM CM000.

013140 AA49.
013150       EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY CREDIT.RD.

       COPY PARAM.RD.

       COPY PORDER.RD.

000140 COPY STKMEM.RD.

       COPY STOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY CREDIT.WR.

       COPY PARAM.WR.

       COPY PORDER.WR.

       COPY STOCK.WR.

      /
      *    THIS ROUTINE LOCKS THE CONTROL FILE DEBTOR RECORD
      *
010910 AY000           SECTION.
011390 AY60.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
011960       MOVE 4              TO WS-PARKEY.
             MOVE 3              TO WS-COUNT.
011480       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
               GO TO AY62.
004870	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
             UNLOCK PARAM.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 200		 TO WS-WAIT.
	     PERFORM LOCK-REC-LOOP.
	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             GO TO AY60.
       AY61.
011990	     MOVE "CR"		 TO PAR-PROG (WS-USUB).
	     MOVE LS-USER	 TO PAR-USR (WS-USUB).
011460	     PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      * 	   and lock it.
      *
012010	     MOVE 2		 TO WS-NETKEY.
012020       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012170       GO TO AY999.
       AY62.
	   IF PAR-PROG (WS-SUB) = "CR" OR "CS" OR "CG"
      *
      *    ****    Test if this is a program error first
      *
	       IF LS-USER = PAR-USR (WS-USUB)
                   SUBTRACT 1    FROM PAR-USERS
                   MOVE SPACES   TO PAR-PROG (WS-USUB)
                                    PAR-USR (WS-USUB)
               ELSE
                   UNLOCK PARAM
		   DISPLAY "Waiting" AT 2572 WITH
			    BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK
		   ACCEPT WS-STIME FROM TIME
		   MOVE 200	 TO WS-WAIT
		   PERFORM LOCK-REC-LOOP
		   DISPLAY "Waiting" AT 2572 WITH
			    BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK
                   GO TO AY60.
           IF WS-SUB < 24
               ADD 1             TO WS-SUB
               GO TO AY62.
	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             MOVE 1              TO WS-SUB.
       AY63.
           IF PAR-USR (WS-SUB) = SPACES
               MOVE WS-SUB       TO WS-USUB
               ADD 1             TO PAR-USERS
               GO TO AY61.
             ADD 1               TO WS-SUB. 
             GO TO AY63.
012180 AY70.
012290	     MOVE 4		 TO WS-PARKEY.
             MOVE 1              TO WS-COUNT.
012300       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      * 	   NETWORK record 2.
      *
012270	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012310	     MOVE SPACES	 TO PAR-PROG (WS-USUB)
                                    PAR-USR (WS-USUB).
             SUBTRACT 1          FROM PAR-USERS.
012320	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
011850       COMMIT.
011860 AY999.
011870       EXIT.
      /
      *    ****    R E - C A L C U L A T E   B A L A N C E S
      *   
034100 CM000		       SECTION 5.
034110 CM00.
034120       DISPLAY CLR-SCREEN.
034130	     DISPLAY "DELETE PURCHASE ORDERS" AT 0229
                        WITH FOREGROUND-COLOR 11.
026030	     DISPLAY "This routine will delete orders prior to the date
      - 	     "keyed in" AT 0408.
026050       DISPLAY "Are you sure Y/N?" AT 0608
026060                WITH FOREGROUND-COLOR 14.

       CM05.
026080	     ACCEPT WS-OPTION AT 0626
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO CM05.
           IF WS-OPTION = "N"
	       GO TO CM999.


034140	     DISPLAY S27.

024790 CM-CHK.
024800	     ACCEPT WS-OPTION AT 1043
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
024860	   IF NOT (WS-OPTION = "Y" OR "N")
024870         GO TO CM-CHK.
024880     IF WS-OPTION = "N"
024890         GO TO CM999.
034540       MOVE 1              TO WS-S3.
034550       DISPLAY "Account :" AT 1812.
             MOVE "N"            TO WS-SKIP.
034580       PERFORM AY60 THRU AY999.
	     MOVE ZERO		 TO W90-CUR W90-BAL W90-30 W90-60
                                    W90-120 W90-INT W90-90.
	     MOVE "     !"	 TO P-NUMBER.
	     PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
	   IF WS-F-ERROR = 3
               GO TO CM85.
034560 CM20.
034780	     PERFORM READ-CREDIT-NEXT-LOCK THRU READ-CREDIT-EXIT.
	   IF WS-F-ERROR = 3
               GO TO CM85.
034790	     DISPLAY DEB-ACNO AT 1822
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
           IF OPEN-ITEM
034840	       MOVE ZERO	 TO DEB-BAL DEB-CRED DEB-DEBT
           ELSE
               MOVE DEB-STM      TO DEB-BAL.
             MOVE DEB-ACNO       TO TRA-AC.
             MOVE ZERO           TO TRA-DATE TRA-CODE.
             MOVE SPACES         TO TRA-REF.
	     PERFORM START-AT-DTRN-KEY THRU READ-CRTRAN-EXIT.
	   IF WS-F-ERROR = 4
               GO TO CM80.
       CM70.
	     PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
	   IF (WS-F-ERROR = 4) OR
              (DEB-ACNO NOT = TRA-AC)
               GO TO CM80.
             ADD TRA-VALUE       TO DEB-BAL.
           IF NOT OPEN-ITEM
               GO TO CM70.
           IF TRA-VALUE > ZERO
               MULTIPLY -1       BY TRA-ALLOCATED.
             ADD TRA-ALLOCATED   TO DEB-BAL.
             GO TO CM70.
       CM80.
           IF OPEN-ITEM
	       PERFORM CI000
               COMPUTE DEB-STM = DEB-BAL - DEB-DEBT + DEB-CRED.
	     ADD DEB-BAL	 TO W90-BAL.
             ADD DEB-CUR         TO W90-CUR.
             ADD DEB-30          TO W90-30.
             ADD DEB-60          TO W90-60.
             ADD DEB-90          TO W90-90.
             ADD DEB-120         TO W90-120.
             ADD DEB-INT         TO W90-INT.
	     PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
             GO TO CM20.
       CM85.
	     MOVE W90-BAL	 TO DEB-OUT.
             MOVE W90-CUR        TO DEB-MTD.
             MOVE W90-30         TO DEB-ONE.
             MOVE W90-60         TO DEB-TWO.
             MOVE W90-90         TO DEB-THREE.
             MOVE W90-120        TO DEB-FOUR.
             MOVE W90-INT        TO DEB-INTEREST.
	     PERFORM AY70 THRU AY999.
036380 CM999.
036390       EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 8.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
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
	       GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
      *
      *    ****    S E T   U P   A G E   D A T E S
      *
             MOVE W12-T-YMD      TO W22-DTE2.
      *	   IF W22-DD2 > 27
      *	       ADD 1		 TO W22-MM2.
       ZA47.
           IF W22-MM2 > 01
               SUBTRACT 1        FROM W22-MM2
           ELSE
               MOVE 12           TO W22-MM2
	       SUBTRACT 1	 FROM W22-CY2.
	   IF W22-MM2 = 02
               MOVE 28           TO W22-DD2
           ELSE
           IF W22-MM2 = 04 OR 06 OR 09 OR 11
               MOVE 30           TO W22-DD2
           ELSE
               MOVE 31           TO W22-DD2.
       ZA48.
           IF W12-MM = W22-MM2
               IF W12-DD < W22-DD2
                   MOVE W12-DD   TO W22-DD2.
	      MOVE W22-DTE2	  TO W30-CUR.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-30.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-60.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-90.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-120.
044780	     GO TO ZA999-EXIT.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
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
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = WS-SYS-ID
	       MOVE "Supervisor─"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 8.
      
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
       I-O-ERROR-EXIT.
             EXIT.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041760	   ELSE
041770	   IF WS-F-ERROR = 3
041780	       MOVE W02-CREDIT TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041640	   ELSE
041730	   IF WS-F-ERROR = 4
041740	       MOVE W02-CRTRAN TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870	       MOVE WS-PARKEY TO WS-KEY.
041880	   IF WS-STATUS = "10"
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
