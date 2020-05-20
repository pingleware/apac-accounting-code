      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******    ******   **    **	  ****	    ****     ******    *
      *   **	**  **	  **  **    **	 **  **	   **  **   **	  **   *
      *   **	    **	      **    **	**    **  **	**	  **   *
      *   **	     ******   ********	**    **  **	**     ****    *
      *   **		  **  **    **	**    **  **	**	  **   *
      *   **	**  **	  **  **    **	 **  **	   **  **   **	  **   *
      *    ******    ******   **    **	  ****	    ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     ( A P A C )   P E T T Y   C A S H	P R O G R A M	       *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CSH003.
000030 AUTHOR.         J W LEMMON  (APAC).
000040 DATE-WRITTEN.   FEBRUARY 1993.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1993 - 2010
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

       COPY CONTROL.SL.

       COPY LEDTRF.SL.

000120 COPY PARAM.SL.

000240 COPY RECOVER.SL.

       COPY SHARED.SL.

000280	   SELECT CSHDRW  ASSIGN W02-CSHDRW
000290                    ORGANIZATION SEQUENTIAL
000300                    ACCESS SEQUENTIAL.
000290     SELECT CSHSLE  ASSIGN DISK
000300                    ORGANIZATION LINE SEQUENTIAL
000310                    ACCESS SEQUENTIAL.
      /
000320 DATA DIVISION.
000330 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY LEDTRF.FDE.

000350 COPY PARAM.FDE.

000470 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000530 FD  CSHDRW    LABEL RECORD OMITTED.
000550 01  CSH-REC.
000560     03  CSH-OPEN        PIC  X(06).

000530 FD  CSHSLE    LABEL RECORD STANDARD
000540               VALUE OF FILE-ID W02-PRINTER.
000550 01  CSH-L.
000560     03  CSH-DATA.
	       05  CSH-LINE PIC  X(40).
	       05  FILLER   PIC  X(40).
      /
000570 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000580 77  WS-DISC	    PIC  9(02)V99 COMP-3.
000590 77  WS-TDISC         PIC  9(02)V99 COMP-3.
000600 77  WS-SUB           PIC  9(04)    COMP-5.
000600 77  WS-SUPER         PIC  9(02)    COMP-5.
000600 77  WS-S1            PIC  9(04)    COMP-5.
000610 77  WS-S2            PIC  9(04)    COMP-5.
000620 77  WS-S3            PIC  9(04)    COMP-5.
000630 77  WS-S4            PIC  9(04)    COMP-5.
000640 77  WS-S5            PIC  9(04)    COMP-5.
000650 77  WS-S6            PIC  9(04)    COMP-5.
000660 77  WS-S7            PIC  9(04)    COMP-5.
000670 77  WS-S8            PIC  9(04)    COMP-5.
000680 77  WS-S9            PIC  9(04)    COMP-5.
001790 77  WS-IXS           PIC  9(04)    COMP-5.
001800 77  WS-IXS1          PIC  9(04)    COMP-5.
001810 77  WS-IXS2          PIC  9(04)    COMP-5.
001820 77  WS-IXS3          PIC  9(04)    COMP-5.
001830 77  WS-IXS4          PIC  9(04)    COMP-5.
000700 77  WS-PARKEY	    PIC  9(04)	  COMP-5.
000720 77  WS-NETKEY	    PIC  9(04)	  COMP-5.
000730 77  WS-AUDKEY        PIC  9(04)    COMP-5.
000740 77  WS-SALSTR        PIC  9(04)    COMP-5.
000750 77  WS-SALKEY        PIC  9(04)    COMP-5.
000760 77  WS-RECKEY        PIC  9(04)    COMP-5.
000770 77  WS-RECOVER       PIC  9(04)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
002440 77  WS-TRANS	   PIC	9(06)	 COMP-5 VALUE 1.
000810 77  WS-KEY1	    PIC  9(04)	  COMP-5.
000860 77  WS-TME2	    PIC  9(08).
002520 77  WS-PORT	   PIC	9(05) VALUE 259.
      * 1004.
       77  WS-DRAW1	   PIC	9(05) VALUE 260.
000600 77  WS-OPEN	    PIC  X(01) VALUE X"01".
000600 77  WS-CLOSE         PIC  X(01) VALUE X"00".
000870 77  WS-PAR           PIC  9(01) VALUE 0.
000880 77  WS-INO           PIC  9(02) VALUE 0.
000890 77  WS-LINES         PIC  9(02) VALUE 0.
000930 77  WS-OPTION	    PIC  X(01).
002610 77  WS-SKIP	   PIC	X(01) VALUE " ".
002610 77  WS-LOOK         PIC  X(01) VALUE "S".
002620 77  WS-ETYPE        PIC  X(01) VALUE "I".
002040 77  WS-WS            PIC X(01).
000950 77  WS-NUM           PIC  Z9.
000960 77  WS-ERROR         PIC  9(01).
001020 77  WS-UPDATE	    PIC  9(01) VALUE ZERO.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
000070 77  WS-DRAW	   PIC	9(02).
      /
002910 01  WS-DB-LINE.
002920     03  WS-TOP-LINE.
002930         05  WS-TCHAR PIC X(01) OCCURS 80.
002940     03  WS-TOP-LNE2.
002950         05  WS-TCR  PIC X(01) OCCURS 80.
002960     03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
002970         05  FILLER      PIC  X(01).
002980         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003000	   03  WS-MID-LINE.
003010         05  WS-MCHAR PIC X(01) OCCURS 80.
	   03  WS-MINV-LINE REDEFINES WS-MID-LINE.
	       05  WS-MIV1     PIC  X(01).
	       05  WS-MIV78    PIC  X(78).
	       05  WS-MIV80    PIC  X(01).
003020	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003040	   03  WS-BOT-LINE.
003050         05  WS-BCHAR PIC X(01) OCCURS 80.
003060     03  WS-BOT-LNE2.
003070         05  WS-BCR  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
003080     03  WS-TOP-LNE.
003090         05  WS-TCHR PIC X(01) OCCURS 80.
003100     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003110         05  FILLER  PIC X(01).
003120         05  WS-H-LINE
003130                     PIC X(78).
003140         05  FILLER  PIC X(01).
003150     03  WS-MID-LNE.
003160         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003170	   03  WS-BOT-LNE.
003180         05  WS-BCHR PIC X(01) OCCURS 80.
003190     03  WS-TP-LNE.
003200         05  WS-TCH  PIC X(01) OCCURS 80.
003210     03  WS-MD-LNE.
003220         05  WS-MCH  PIC X(01) OCCURS 80.
003230     03  WS-BT-LNE.
003240         05  WS-BCH  PIC X(01) OCCURS 80.
           03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".

001260 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "CSH".
	   03  WS-PROG	       PIC  X(03) VALUE "003".

       COPY LEDTRF.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

001280 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

000470 COPY RECOVER.ID.

       COPY SHARED.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER     PIC X(12) VALUE "LPT1".
           03  W02-CSHDRW     PIC X(04) VALUE "COM1".

       COPY W10.SAL.

       COPY W12.WS.

001820 01  W15-CASH-SLIP.
001830     03  W15-L1.
001860	       05  W15-EXPP1   PIC  X(01).
	       05  FILLER      PIC  X(04) VALUE SPACES.
001870         05  W15-COMP    PIC  X(32) VALUE SPACES.
001880         05  W15-ECAN1   PIC  X(01).
001890     03  W15-L1A.
001920	       05  W15-EXPP4   PIC  X(01).
001930	       05  W15-DOCTYP  PIC  X(19) VALUE SPACES.
001940         05  W15-ECAN4   PIC  X(01).
002020	   03  W15-L4.
002030	       05  FILLER      PIC  X(19) VALUE "Date/Datum       :".
002040	       05  W15-DATE    PIC  Z9/99/9999.
002020	   03  W15-L4A.
002030	       05  FILLER      PIC  X(19) VALUE "Time/Tyd         :".
002040	       05  W15-TIME    PIC  X(08).
002050     03  W15-L5.
002060	       05  FILLER      PIC  X(19) VALUE "Salesperson      :".
002070	       05  W15-SMAN    PIC  X(21).
002140     03  W15-L7A.
002150	       05  FILLER      PIC  X(19) VALUE "Petty Cash Vouch :".
002160         05  W15-CRNO    PIC  X(08).
002460	   03  W15-L13.
002470	       05  W15-REMK    PIC X(31).
002480	       05  W15-VAL     PIC Z(04)9.99-.

       COPY W20.WS.

002580 01  W25-CALCS.
002590     03  W25-RESULT      PIC  9(02)V99.
002600     03  W25-RESULT1 REDEFINES W25-RESULT.
002610         05  W25-WHOLE   PIC  9(02).
002620         05  W25-DECIMAL PIC  9(02).
002630     03  W25-TOTAL       PIC  9(07)V99 COMP-3.
002640     03  W25-VALUE       PIC S9(07)V99 COMP-3.
002650     03  W25-INSTAL      PIC S9(07)V99 COMP-3.
002660     03  W25-CREDIT      PIC S9(07)V99 COMP-3.
002670     03  W25-CUR         PIC S9(07)V99 COMP-3.
002680     03  W25-INSTL.
002690         05  W25-MTHS OCCURS 6
002700                         PIC S9(07)V99 COMP-3.
002710     03  W25-INSTALS REDEFINES W25-INSTL.
002720         05  W25-MTH1    PIC S9(07)V99 COMP-3.
002730         05  W25-MTH2    PIC S9(07)V99 COMP-3.
002740         05  W25-MTH3    PIC S9(07)V99 COMP-3.
002750         05  W25-MTH4    PIC S9(07)V99 COMP-3.
002760         05  W25-MTH5    PIC S9(07)V99 COMP-3.
002770         05  W25-MTH6    PIC S9(07)V99 COMP-3.
001660	   03  W25-TIME        PIC 9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001690         05  W25-HH      PIC 9(02).
001690         05  W25-MM      PIC 9(02).
001690         05  W25-SS      PIC 9(02).
001680         05  FILLER      PIC 9(02).

       01  W40-COMPANY.
004900	   03  W40-NAME.
004910	       05  W40-NCH     PIC  X(01) OCCURS 32.

       COPY W40.WS.

       COPY FUNCTION.WS.

002870 COPY W50.WS.

002890 01  W60-ITEM.
002900     03  W60-ITM.
002910         05  W60-ICHAR OCCURS 14
002920                         PIC  X(01).
002930 01  W65-ENTRY.
002940     03  W65-FOUR.
002950         05  W65-CHAR OCCURS 2
002960                         PIC  X(01).
       COPY W70DEBT.IWS.

       COPY W85.WS.

003160 01  W85-PASS.
003180     03  W85-SUPER OCCURS 9
003190                         PIC  X(06).
003200     03  W85-ENTRY       PIC  9(02)    COMP-3.
003210     03  W85-MARG        PIC S9(02)V99.

003230 01  W90-TRAN.
003990	   03  W90-DATE        PIC  9(08)    COMP-3.
004000	   03  W90-DTE	       PIC  99/99/9999.
003410	   03  W90-CASH        PIC S9(07)V99 COMP-3.
003450	   03  W90-PMT	       PIC S9(07)V99 COMP-3.
004030	   03  W90-REF	       PIC  X(08).

003630 01  W95-STM.
003640     03  W95-COMP        PIC  X(40).
003650     03  W95-ADD1        PIC  X(30).
003660     03  W95-ADD2        PIC  X(30).
003670     03  W95-ADD3        PIC  X(30).
003680     03  W95-ADD4        PIC  X(30).
003690     03  W95-POST        PIC  9(04).
003700     03  W95-TEL         PIC  X(08).
	   03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.

003710 01  W100-EDIT.
003720     03  W100-BAL        PIC  Z(06)9.99-.
003730     03  W100-CUR        PIC  Z(06)9.99-.
003740     03  W100-30         PIC  Z(06)9.99-.
003750     03  W100-60         PIC  Z(06)9.99-.
003760     03  W100-90         PIC  Z(06)9.99-.
003770     03  W100-INT        PIC  Z(06)9.99-.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
003780 SCREEN SECTION.

	COPY BLANK.CRT.

003930 01  S07.
003940	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 15
				 VALUE "PETTY CASH VOUCHER".
003950	   03  LINE  4 COLUMN 24 VALUE "Salesperson".
	   03	       COLUMN 36 BACKGROUND-COLOR 5
				 PIC  X(02) FROM WS-SPACE.
003960	   03  LINE  5 COLUMN 24 VALUE "Ref. Number".
	   03	       COLUMN 36 BACKGROUND-COLOR 5
				 PIC  X(08) FROM WS-SPACE.
003980	   03  LINE  6 COLUMN 24 VALUE "Date       ".
	   03	       COLUMN 36 BACKGROUND-COLOR 5
				 PIC  X(08) FROM WS-SPACE.
	   03  LINE  7 COLUMN 24 VALUE "Narrative  ".
	   03	       COLUMN 36 BACKGROUND-COLOR 5
				 PIC  X(24) FROM WS-SPACE.
004060	   03  LINE  9 COLUMN 24 VALUE "Amount     ".
	   03	       COLUMN 36 BACKGROUND-COLOR 5
				 PIC  X(10) FROM WS-SPACE.

004490 01  S26-CASH.
004500	   03  LINE  9 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
004510				 PIC Z(06)9.99- USING W90-CASH AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004790 AA000		  SECTION.
004800 AA000-INIT.
003240	   IF LS0-SALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
004810	     PERFORM ZA000-INIT.
	     PERFORM JA000.
	     CLOSE CSHSLE
		   RECOVER.
       AA49.
             EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
004930 AC000-READ              SECTION.

011170 COPY CONTROL.RD.

       COPY LEDTRF.RD.

011290 COPY PARAM.RD.

       COPY SHARED.RD.

      /
006420 AD000-WRITE             SECTION.

011170 COPY CONTROL.WR.

       COPY LEDTRF.WR.

011290 COPY PARAM.WR.

       COPY SHARED.WR.

       COPY CASHDRAW.OPN.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
011670 AY000           SECTION.
011680 AY01.
011690       MOVE 01             TO REC-FILE.
011700       MOVE WS-PARKEY      TO REC-KEY.
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
011720       MOVE PAR-RECORD1    TO REC-PARAM.
011730       GO TO AY50.

012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.

012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.
012200 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.
       AY49.
012210       MOVE 99             TO REC-FILE.
012220       MOVE ZERO           TO REC-KEY.
012230       MOVE SPACES         TO REC-DETAIL.
012240       PERFORM AY50.
012250       ADD 1               TO WS-TRANS.
012260     IF WS-RECOVER > 95
012270         CLOSE RECOVER
012280         OPEN OUTPUT RECOVER
012290         CLOSE RECOVER
012300         OPEN I-O RECOVER
012310         MOVE ZERO         TO WS-RECOVER.
012320       GO TO AY59.
012330 AY50.
012340       ADD 1               TO WS-RECOVER.
012350       MOVE WS-RECOVER     TO WS-RECKEY.
012360       MOVE WS-TRANS       TO REC-TRAN.
012370       WRITE REC-RECORD.
012380     IF WS-STATUS NOT = "00"
012390         DISPLAY "Write error Recovery file - Status " AT 2212
012400                  WITH FOREGROUND-COLOR 14
012410                  WS-STATUS WITH FOREGROUND-COLOR 15
012420         STOP RUN.
012430       CLOSE RECOVER.
012440       OPEN I-O RECOVER.
012450 AY59.
012460       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
	     MOVE SHR-STOCK	 TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "DB"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012670	     MOVE 1		 TO WS-NETKEY.
012680       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012700       MOVE NET-DEBTOR     TO W70-DEBT.
	   IF WS-STOCK = "Y"
012710	       MOVE 3		 TO WS-SHARED
012720	       PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
	       PERFORM AY37 THRU AY59
	       MOVE SHR-RECORD	 TO NET-RECORD
	   ELSE
012710	       MOVE 3		 TO WS-NETKEY
012720	       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
012730	       PERFORM AY39 THRU AY59.
012740       GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
	       IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
      *
		   IF WS-WAIT = ZERO
		       GO TO AY63
		   ELSE
		       GO TO AY62-WAIT
		   END-IF
	       ELSE
		   GO TO AY62-WAIT
	       END-IF
	   END-IF.
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012990       MOVE 1              TO WS-NETKEY.
013000       MOVE W70-DEBT       TO NET-DEBTOR.
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020       PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES)
	       MOVE PAR-PROG(WS-SUB)
				 TO PAR-PROG(WS-USUB)
	       MOVE PAR-USR(WS-SUB)
				 TO PAR-USR(WS-USUB)
	       ADD 1		 TO WS-USUB
	       GO TO AY75.

       AY80.
013070	     SUBTRACT 1 FROM WS-USUB
				 GIVING PAR-USERS.
013080	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	   IF LS0-POS NOT = 2
012430         CLOSE RECOVER
012440         OPEN I-O RECOVER.
013090       COMMIT.

013080 AY999.
013090       EXIT.

       COPY LEDTRF.UPD.

      /
009550 JA000		       SECTION 50.
009560 JA00.
018730       DISPLAY CLR-SCREEN.
018740	     DISPLAY "PROCESS PETTY CASH VOUCHER" AT 0229
018750                WITH FOREGROUND-COLOR 15.
018760	     MOVE SPACES	 TO W40-CSHPASS.
	     PERFORM SAVE-SCREEN.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 30		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1128
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
042540	     DISPLAY "³ " AT 1228 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				       BACKGROUND-COLOR 7
		     "Enter Password        "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1328 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
042550	     ACCEPT W40-CSHPASS AT 1245
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 3 HIGHLIGHT AUTO NO-ECHO.
018800	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018860	     MOVE 1		 TO WS-LENGTH.
	     PERFORM RESTORE-SCREEN.
018870       MOVE 5              TO WS-PARKEY.
018880       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
018890       MOVE PAR-PW         TO W85-PASS.
018900     IF NOT (W40-CSHPASS = W85-SUPER (1) OR
018940                           W85-SUPER (5) OR
018990                           "!@#$%^")
048290	       MOVE "Invalid password"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019030	       GO TO JA999.
009600       ADD 150 WS-DRAW     GIVING WS-PARKEY.
009610       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
009620       MOVE PAR-RECORD151  TO W85-RECORD151.

009630 JA05.
009640       DISPLAY CLR-SCREEN.
009650       DISPLAY S07.
009680	     MOVE W85-CSHDTE	 TO W90-DTE W15-DATE.
009690	     DISPLAY W90-DTE AT 0636
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
009700	     MOVE ZERO		 TO W10-SMAN.
009740       MOVE SPACES         TO CSH-DATA.
009750 JA10.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to EXIT"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
009760	     ACCEPT W10-SMAN AT 0436
009770		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO JA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO JA10
		 WHEN OTHER
		     CALL X"E5"
		     GO TO JA10
	       END-EVALUATE.
	   IF W10-SMAN = 99
	       GO TO JA999.
009780     IF W10-SMAN > 50
009790	       GO TO JA10.
	     DISPLAY CLEAR-L25.
009800     IF W10-SMAN = ZERO
	       MOVE SPACES	 TO W15-SMAN
009810	       GO TO JA15.
009820       ADD 100 W10-SMAN    GIVING WS-PARKEY.
009830       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
009840     IF PAR-SALESMAN = SPACES
009850	       GO TO JA10.
009860	     DISPLAY PAR-SALESMAN AT 0439
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
009970	     MOVE PAR-SALESMAN TO W15-SMAN.
009870 JA15.
010310	     MOVE SPACES	 TO W10-DESC.
010330 JA20.
010510	     ACCEPT W10-DESC AT 0736
010520		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF W10-DESC = SPACES
	       GO TO JA20.
012750 JA25.
      *
      *    ****    C A S H   V A L U E
      *
021100	     DISPLAY CLEAR-L25.
012760       ACCEPT S26-CASH.
012770	     MOVE W90-CASH	 TO W90-PMT.
      *
      *    ****    A R E   P A Y M E N T   D E T A I L S   C O R R E C T
      *
012950 JA40.
024810	     MOVE "'C'orrect or 'R'ekey refund"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
013020	   IF NOT (WS-OPTION = "C" OR "R")
013030	       GO TO JA40.
013050     IF WS-OPTION = "R"
013060	       GO TO JA25.
	   IF W90-CASH = ZERO
	       GO TO JA999.
      *
      *    ****    P R O C E S S   P E T T Y   C A S H	 V O U C H E R
      *
013250	     DISPLAY "Processing Petty Cash" AT 2520 WITH
013260		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
013280       PERFORM AY60 THRU AY999.
013330	     MOVE W70-INVOICE	 TO W90-REF W15-CRNO.
013290     IF W70-INVOICE = 99999999
013300	       MOVE 1		 TO W70-INVOICE
013310     ELSE
013320	       ADD 1		 TO W70-INVOICE.
013340	     DISPLAY  W90-REF AT 0536
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     MOVE W85-SI	 TO CSH-DATA.
	     WRITE CSH-L.
	   IF W90-CASH < ZERO
	       MOVE "PETTY CASH REFUND"
				 TO W15-DOCTYP
	   ELSE
013350	       MOVE "PETTY CASH VOUCHER"
				 TO W15-DOCTYP.
013360	     MOVE W15-L1A	 TO CSH-DATA.
013370       WRITE CSH-L.
013380	     MOVE W15-L1	 TO CSH-DATA.
013390       WRITE CSH-L.
013440	     MOVE W15-L4	 TO CSH-DATA.
013450       WRITE CSH-L.
012150	     ACCEPT W25-TIME FROM TIME.
             MOVE W25-HH         TO W95-HH.
             MOVE W25-MM         TO W95-MM.
             MOVE W25-SS         TO W95-SS.
	     MOVE W95-TIME	 TO W15-TIME.
013440	     MOVE W15-L4A	 TO CSH-DATA.
013450       WRITE CSH-L.
013460	   IF W10-SMAN NOT = ZERO
013470	       MOVE W15-L5	 TO CSH-DATA
013480         WRITE CSH-L.
013490	     MOVE W15-L7A	 TO CSH-DATA.
013500       WRITE CSH-L.
013510	     MOVE ALL "-"	 TO CSH-LINE.
013520       WRITE CSH-L.
013680	     MOVE SPACES	 TO CSH-DATA.
013690       WRITE CSH-L.
013600	     MOVE W10-DESC	 TO W15-REMK.
013620	     MOVE W90-CASH	 TO W15-VAL W25-VALUE.
013680	     MOVE W15-L13	 TO CSH-DATA.
013690       WRITE CSH-L.
013680	     MOVE SPACES	 TO CSH-DATA.
013690       WRITE CSH-L.
013510	     MOVE ALL "-"	 TO CSH-LINE.
013520       WRITE CSH-L.
014850	     MOVE SPACES	 TO CSH-DATA.
014860	     MOVE 1		 TO WS-S9.
014870 JA45.
014880       WRITE CSH-L.
014890	   IF WS-S9 < 4
014900	       ADD 1		 TO WS-S9
014910	       GO TO JA45.
014800	     PERFORM OPEN-CASH-DRAWER THRU OPEN-CASH-DRAWER-EXIT.
	     MOVE "Authorised________________________"
				 TO CSH-LINE.
	     WRITE CSH-L.
	     MOVE SPACES	 TO CSH-DATA.
014790	     WRITE CSH-L.
014850       MOVE SPACES TO CSH-DATA.
014860	     MOVE 1		 TO WS-S9.
014870 JA50.
014880       WRITE CSH-L.
014890     IF WS-S9 < W85-ADVANCE
014900         ADD 1 TO WS-S9
014910	       GO TO JA50.
	     MOVE W85-CSI	 TO CSH-DATA.
014880       WRITE CSH-L.
014850       MOVE SPACES         TO CSH-DATA.
	     CLOSE CSHSLE.
	     OPEN OUTPUT CSHSLE.
	     CALL X"91" USING X91-RES X91-FUN CSHSLE.
015360	     ADD 150 WS-DRAW	 GIVING WS-PARKEY.
015380       PERFORM AY01 THRU AY59.
015390	     SUBTRACT W90-CASH	 FROM PAR-PETTY.
      * 			      PAR-CASH.
015470       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
015370	     MOVE PAR-RECORD151  TO W85-RECORD151.
015480	     MOVE 7		 TO WS-PARKEY.
015500       PERFORM AY01 THRU AY59.
      *
      *    ****    U P D A T E	 P E T T Y   C A S H
      *
	   IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
	       MOVE "  PC"	 TO WX-LEDG
	       MOVE WS-DRAW	 TO WX-TRN
	       MOVE "Petty Cash Vouchers"
                                 TO WX-NAR
	       COMPUTE WX-VALUE = W90-CASH * -1
               MOVE PAR-CASH-GL  TO WX-AC
	       MOVE 37		 TO WX-TYPE
	       PERFORM GL-TRF-UPDATE
	       MOVE W90-CASH	 TO WX-VALUE
	       MOVE WX-PETGL	 TO WX-AC
	       MOVE 40		 TO WX-TYPE
	       PERFORM GL-TRF-UPDATE.
015530       PERFORM AY70 THRU AY999.
015540	     CLOSE PARAM NETWORK.
015550	     OPEN I-O PARAM NETWORK.
015570	     DISPLAY CLEAR-L25.
029700	     MOVE "ENTER to continue"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.

017900 JA999.
017910       EXIT.
      /
020720 ZA000-INIT	       SECTION 91.
020730 ZA000-OPEN.
             PERFORM ZA58 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
	     MOVE LS-DRAW	 TO WS-DRAW.
049200	     DISPLAY CLR-SCREEN.
020750	     DISPLAY "CASH DRAWER PROGRAM" AT 0231
020760                WITH FOREGROUND-COLOR 15.
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
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
020770	     MOVE ZERO		 TO WS-LINES.
           IF WS-DRAW = ZERO
	       MOVE 40		 TO WS-DRAW.
021020	     OPEN OUTPUT RECOVER.
021030       CLOSE RECOVER.
021040       OPEN I-O RECOVER.
021400	     MOVE 1		 TO WS-PARKEY.
021410       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021590     IF PAR-CSHEAD = SPACES
021600	       MOVE "Setup cash slip headings"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
021620	       GO TO ZA205.
021430	     MOVE PAR-DMY	 TO W12-TODAY.
021440	     MOVE PAR-YMD	 TO W12-T-YMD.
             MOVE PAR-CSHEAD     TO W15-COMP W40-NAME.
	     MOVE 6		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-WHOLESALE  TO WS-WS.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"          TO WX-INT
           ELSE
               MOVE "Y"          TO WX-INT.
             MOVE PAR-DEBGL      TO WX-DEBGL.
             MOVE PAR-CREGL      TO WX-CREGL.
             MOVE PAR-INTGL      TO WX-INTGL.
             MOVE PAR-BNKGL      TO WX-BNKGL.
             MOVE PAR-UNPROF     TO WX-UNPROF.
             MOVE PAR-REDGL      TO WX-REDGL.
             MOVE PAR-ADJGL      TO WX-ADJGL.
             MOVE PAR-RLGL       TO WX-RLGL.
             MOVE PAR-DSGL       TO WX-DSGL.
	     MOVE PAR-PETGL	 TO WX-PETGL.
021490	     ADD 150 WS-DRAW	 GIVING WS-PARKEY.
021500       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021510	     MOVE W12-TODAY	 TO PAR-CSHDTE
021520	     MOVE PAR-RECORD151  TO W85-RECORD151.
021530	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
021540	     MOVE 5		 TO WS-PARKEY.
021550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021560	     MOVE PAR-PW	 TO W85-PASS.
021680	     MOVE 32		 TO WS-S1.
020780       DISPLAY "Waiting for printer response" AT 1431
020790                WITH FOREGROUND-COLOR 14 BLINK.
021690 ZA05.
021700     IF W40-NCH (WS-S1) = SPACE
021710	       SUBTRACT 1	 FROM WS-S1
021720         GO TO ZA05.
021730     IF WS-S1 > 16
021740	       MOVE W85-NUL	 TO W15-EXPP1 W15-ECAN1
021750     ELSE
021760	       MOVE W85-SO	 TO W15-EXPP1
021770	       MOVE W85-CSO	 TO W15-ECAN1.
021800	     MOVE W85-SO	 TO W15-EXPP4.
021810	     MOVE W85-CSO	 TO W15-ECAN4.
           IF W85-CPRN = 1
	       MOVE "LPT1"	 TO W02-PRINTER
           ELSE
           IF W85-CPRN = 2
	       MOVE "LPT2"	 TO W02-PRINTER
           ELSE
           IF W85-CPRN = 3
	       MOVE "LPT3"	 TO W02-PRINTER
           ELSE
           IF W85-CPRN = 4
	       MOVE "COM1"	 TO W02-PRINTER
           ELSE
           IF W85-CPRN = 5
	       MOVE "COM2"	 TO W02-PRINTER
           ELSE
           IF W85-CPRN = 9
	       MOVE "CSH003.PRN" TO W02-PRINTER.
             OPEN OUTPUT CSHSLE.
	     CALL X"91" USING X91-RES X91-FUN CSHSLE.
      *
      *     O L D   S T Y L E   C A S H   D R A W E R
      *
           IF W85-DRAWER  = "F"
               IF W85-PORT = 2
                   MOVE "COM2"   TO W02-CSHDRW
               ELSE
               IF W85-PORT = 3
                   MOVE "COM3"   TO W02-CSHDRW
               ELSE
               IF W85-PORT = 4
                   MOVE "COM4"   TO W02-CSHDRW.
           IF W85-DRAWER = "F"
               OPEN OUTPUT CSHDRW
	       MOVE ALL "A"	 TO CSH-OPEN.
             GO TO ZA999.
055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA58.
051090       MOVE 1 TO WS-S1.
051100	     MOVE SPACES TO WS-MID-LINE WS-MID-LNE.
051110 ZA60.
051120       MOVE WS-G1          TO WS-TCHAR (WS-S1) WS-BCHAR (WS-S1).
051130       MOVE WS-G1          TO WS-TCHR (WS-S1)  WS-BCHR (WS-S1).
051140       MOVE WS-G1          TO WS-TCH (WS-S1)   WS-BCH (WS-S1).
051150       MOVE WS-G8          TO WS-TCR (WS-S1)   WS-BCR (WS-S1).
051160     IF WS-S1 < 80
051170         ADD 1 TO WS-S1
051180         GO TO ZA60.
051190       MOVE WS-G9          TO WS-TCR   (1).
051200       MOVE WS-G10         TO WS-TCR   (80).
051210       MOVE WS-G11         TO WS-BCR   (1).
051220       MOVE WS-G12         TO WS-BCR   (80).
051230       MOVE WS-G14         TO WS-TCHAR (1)  WS-BCHAR (1)
051240                              WS-TCHR  (1)  WS-BCHR  (1)
051250                              WS-TCH   (1)  WS-BCH   (1).
051260       MOVE WS-G13         TO WS-TCHAR (80) WS-BCHAR (80)
051270                              WS-TCHR  (80) WS-BCHR  (80)
051280                              WS-TCH   (80) WS-BCH   (80).
051290       MOVE WS-G2          TO WS-TCHAR (4)  WS-TCHAR (19)
051300                              WS-TCHAR (50) WS-TCHAR (59) 
051310                              WS-TCHAR (69)
051320                              WS-TCHR  (18) WS-TCHR  (49)
051330                              WS-TCHR  (60) WS-TCHR  (70)
051340                              WS-TCH   (7)  WS-TCH   (48)
051350                              WS-TCH   (60) WS-TCH   (72).
051360       MOVE WS-G3          TO WS-MCHAR (4)  WS-MCHAR (19) 
051370                              WS-MCHAR (50) WS-MCHAR (59) 
051380                              WS-MCHAR (69) WS-MCHAR (80)
                                    WS-MCHAR (1)
051390                              WS-MCH   (7)  WS-MCH   (48)
051400				    WS-MCH   (60) WS-MCH   (72)
051420                              WS-MCHR  (18) WS-MCHR  (49)
051430                              WS-MCHR  (60) WS-MCHR  (70)
051440				    WS-MCHR  (1)  WS-MCHR  (80).
051460       MOVE WS-G4          TO WS-BCHAR (4)  WS-BCHAR (19) 
051470                              WS-BCHAR (50) WS-BCHAR (59) 
051480                              WS-BCHAR (69)
051490                              WS-BCHR  (18) WS-BCHR  (49)
051500                              WS-BCHR  (60) WS-BCHR  (70)
051510                              WS-BCH   (7)  WS-BCH   (48)
051520                              WS-BCH   (60) WS-BCH   (72).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
022010 ZA200.
022020       DISPLAY "Files locked - Try later" AT 2312
022030                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
022040       DISPLAY "Press any key" AT 2512 
022050		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
022060	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
022090 ZA999.
022100       EXIT.
      /
022110 ZB000-ERROR	     SECTION 92.

051650 OPEN-ERROR.
051660       DISPLAY CLR-SCREEN.
051670       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
051680       PERFORM DISPLAY-FILE-NAME.
051690
051700 READ-ERROR.
051710       DISPLAY CLR-SCREEN.
051720       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
051730       PERFORM DISPLAY-FILE-NAME.
051740
051750 WRITE-ERROR.
051760       DISPLAY CLR-SCREEN.
051770       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
051730       PERFORM DISPLAY-FILE-NAME.
051780

051790 DISPLAY-FILE-NAME.
051840	   IF WS-F-ERROR = 2
051850	       MOVE W02-NETWORK  TO WS-FILE
051860	       MOVE WS-NETKEY	 TO WS-KEY
052070	   ELSE
052080     IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100         MOVE WS-PARKEY    TO WS-KEY
052110     ELSE
052120     IF WS-F-ERROR = 18
052130         MOVE W02-RECOVER  TO WS-FILE
052140	       MOVE WS-RECKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX.
052310     IF WS-STATUS = "10"
052320         MOVE "End of FILE" TO WS-STAT-MESSAGE
052330     ELSE
052340     IF WS-STATUS = "22"
052350         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
052360     ELSE
052370     IF WS-STATUS = "23"
052380         MOVE "Invalid record number" TO WS-STAT-MESSAGE
052390     ELSE
052400     IF WS-STATUS = "24"
052410         MOVE "DISK full" TO WS-STAT-MESSAGE
052420     ELSE
052430     IF WS-STATUS = "30"
052440         MOVE "DISK error" TO WS-STAT-MESSAGE
052450     ELSE
052460     IF WS-STATUS = "94"
052470         MOVE "FILE locked" TO WS-STAT-MESSAGE.
052480       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
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
052580       STOP RUN.
