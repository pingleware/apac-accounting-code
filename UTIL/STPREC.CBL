      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******   *******   *******   ******     *
      *   **    **     **     **    **  **    **  **       **    **    *
      *   **           **     **    **  **   **   **       **          *
      *    ******      **     *******   ******    *****    **          *
      *         **     **     **        **   **   **       **          *
      *   **    **     **     **        **    **  **       **    **    *
      *    ******      **     **        **    **  *******   ******     *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *       S T O C K   R E C O V E R Y   P R O G R A M              *
      *                                                                *
      *     VERSION 8.13.08 - August 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPREC.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   DECEMBER 1986.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1986 - 2008
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

000110	   SELECT NETWORK ASSIGN DISK
                          LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000130                    ACCESS RANDOM
000140                    ORGANIZATION RELATIVE
000150                    RELATIVE WS-NETKEY.

000110	   SELECT DEPART  ASSIGN DISK
                          LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000190			  ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD DPT-CODE
			  ALTERNATE RECORD DPT-NKEY DUPLICATES.

000110     SELECT PARAM   ASSIGN DISK
                          LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000130                    ACCESS RANDOM
000140                    ORGANIZATION RELATIVE
000150                    RELATIVE WS-PARKEY.

      $SET KEYCOMPRESS "7"
000210     SELECT LEDTRF ASSIGN DISK
                         LOCK EXCLUSIVE
000220                   STATUS WS-STATUS
000230                   ACCESS DYNAMIC
000240                   ORGANIZATION INDEXED
000250                   RECORD XFR-KEY.

000170	   SELECT STKALT  ASSIGN DISK
			  LOCK EXCLUSIVE
000180                    STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD STKA-KEY
			  ALTERNATE RECORD STKA-DKEY DUPLICATES.

000010	   SELECT STKDEX  ASSIGN DISK
000020			  LOCK EXCLUSIVE
000040			  STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
000070			  RECORD STX-KEY
			  ALTERNATE RECORD STX-DSKEY =
				    STX-DTE STX-EXT-CODE DUPLICATES
			  ALTERNATE RECORD STX-REFKEY =
				    STX-GRN STX-DTE STX-TIME
				    DUPLICATES
			  ALTERNATE RECORD STX-DTSKEY =
				    STX-DTE STX-TIME STX-EXT-CODE
				    DUPLICATES.

000110     SELECT STOCK   ASSIGN DISK
                          LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000130                    ACCESS DYNAMIC
000140                    ORGANIZATION INDEXED
000070			  RECORD STK-CODE
000080                    ALTERNATE RECORD STK-ACODE DUPLICATES
000090                    ALTERNATE RECORD STK-BSEQ = 
000100				    STK-BIN STK-CODE
000110                    ALTERNATE RECORD STK-LSEQ =
000120				    STK-LDG STK-CODE
000130                    ALTERNATE RECORD STK-DKEY DUPLICATES
000130			  ALTERNATE RECORD STK-DKEY2 DUPLICATES
000140                    ALTERNATE RECORD STK-XREF DUPLICATES.

000010	   SELECT SHARED  ASSIGN DISK
                          LOCK EXCLUSIVE
000040			  STATUS WS-STATUS
000050                    ACCESS RANDOM
000060                    ORGANIZATION RELATIVE
000070			  RELATIVE WS-SHARED.

	   SELECT SPARTS  ASSIGN DISK
                          LOCK EXCLUSIVE
                          STATUS WS-STATUS
                          ACCESS DYNAMIC
                          ORGANIZATION INDEXED
			  RECORD PRT-EXT-ITEM
			  ALTERNATE RECORD PRT-TYPE DUPLICATES.

000170	   SELECT WARHSE  ASSIGN DISK
			  LOCK EXCLUSIVE
000180                    STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD WAR-CODE
			  ALTERNATE RECORD WAR-NKEY DUPLICATES.

000170	   SELECT WSTOCK  ASSIGN DISK
			  LOCK EXCLUSIVE
000180                    STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD WST-KEY
			  ALTERNATE RECORD WST-IKEY =
				    WST-CODE WST-EXT-CODE WST-WAR.
      $SET NOKEYCOMPRESS

000240 COPY RECOVER.SL.

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.IFD.

       COPY DEPART.GFD.

       COPY LEDTRF.FD.

000290 COPY PARAM.IFD.

       COPY SHARED.FD.

000140 COPY STKALT.GFD.

       COPY STKDEX.FD.

000370 COPY STOCK.GFD.

000370 COPY SPARTS.FD.

       COPY WARHSE.FD.

       COPY WSTOCK.FD.

000410 COPY RECOVER.GFD.

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
000450 77  WS-PARKEY        PIC 9(04)     COMP-5.
000480 77  WS-NETKEY        PIC 9(04)     COMP-5.
000510 77  WS-RECKEY        PIC 9(04)     COMP-5.
000520 77  WS-RECOVER       PIC 9(04)     COMP-5.
000520 77  WS-SHARED	    PIC 9(04)	  COMP-5.
000530 77  WS-TRANS         PIC 9(04)     COMP-5 VALUE 1.
000540 77  WS-EKEY          PIC Z(04)9.
000560 77  WS-IND           PIC 9(01)     VALUE ZERO.
000560 77  WS-LINK          PIC 9(01)     VALUE ZERO.
000560 77  WS-ERROR         PIC 9(01)     VALUE ZERO.
000570 77  WS-PROG          PIC X(02).
       77  WS-OPTION        PIC X(01).
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

000610 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

000370 COPY SHARED.ID.

000310 COPY STKALT.ID.

       COPY STKDEX.ID.

000370 COPY STOCK.ID.

000370 COPY SPARTS.ID.

000410 COPY RECOVER.ID.

       COPY WARHSE.ID.

       COPY WSTOCK.ID.

       COPY W12.WS.

       COPY W40.WS.

       COPY FUNCTION.WS.

       COPY W70STOCK.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
000850 SCREEN SECTION.

       COPY BLANK.CRT.

000860 01  S00.
000910     03  LINE  2 COLUMN 29 FOREGROUND-COLOR 15
000920                 VALUE "STOCK RECOVERY PROGRAM".
000930     03  LINE  4 COLUMN  2 FOREGROUND-COLOR 3
000940                 VALUE "Checking Recovery file".
000950     03  LINE  6 COLUMN 12 FOREGROUND-COLOR 3
000960                 VALUE "RECOVER :".

000970 01  S01.
000980     03  LINE  6 COLUMN 22 FOREGROUND-COLOR 15
000990                 PIC Z(04)9 USING WS-RECKEY.

001000 01  S02.
001010     03  LINE  8 COLUMN 12 FOREGROUND-COLOR 3
001020                 VALUE "Busy recovering files".
001030     03  LINE 10 COLUMN 12 FOREGROUND-COLOR 3
001040                 VALUE "RECOVER :".

001050 01  S03.
001060     03  LINE 10 COLUMN 22 FOREGROUND-COLOR 15
001070                 PIC Z(04)9 USING WS-RECKEY.

001080 01  S04.
001090     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001100                 VALUE "Recovery complete".
001110     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001120                 VALUE "You may now restart the system".

001130 01  S05.
001140	   03  LINE 12 COLUMN 08 FOREGROUND-COLOR 11
001150                 VALUE "Recovery unsuccessfull !".
001160	   03  LINE 14 COLUMN 08 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190	   03  LINE 16 COLUMN 08 FOREGROUND-COLOR 11
001200                 VALUE "since creating the last backup disk/s".
	   03  LINE 18 COLUMN 08 FOREGROUND-COLOR 6 HIGHLIGHT
		       PIC  X(12) FROM WS-FILE.
	   03	       COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
		       PIC  X(24) FROM WS-KEYX.
	   03	       COLUMN 50 FOREGROUND-COLOR 3 HIGHLIGHT
		       PIC  X(02) FROM WS-STATUS.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001220 AA000	       SECTION.
001230 AA00.
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 AA02.
041250	     MOVE WS-G1 	 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
	     MOVE WS-G8 	 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO AA02.
	     MOVE WS-G9 	 TO WS-TCH  (1).
	     MOVE WS-G10	 TO WS-TCH  (80).
	     MOVE WS-G11	 TO WS-BCH  (1).
	     MOVE WS-G12	 TO WS-BCH  (80).
	     MOVE WS-G14	 TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE WS-G13	 TO WS-TCHR (80) WS-BCHR (80).
021930	     MOVE WS-G2 	 TO WS-TCHR (16) WS-TCHR (41)
021940				    WS-TCHR (64) WS-TCHR (71).
021950	     MOVE WS-G3 	 TO WS-MCHR (16) WS-MCHR (41)
021960				    WS-MCHR (64) WS-MCHR (71)
				    WS-MCHR (1)  WS-MCHR (80).
021970	     MOVE WS-G4 	 TO WS-BCHR (16) WS-BCHR (41)
021980				    WS-BCHR (64) WS-BCHR (71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	     MOVE LS-SYS-ID	 TO WS-SYS-ID.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER	 TO WS-WRKST.
001250	     DISPLAY CLR-SCREEN.
001270       PERFORM ZA000.
001260       DISPLAY S00.
001280     IF WS-ERROR = 9
001290         GO TO AA50.
      *
      *    ****    R E C O V E R Y   F I L E
      *
001320	     OPEN INPUT RECOVER.
001330	     MOVE 1		 TO WS-RECKEY.

001340 AA05.
001350       READ RECOVER WAIT.
001360     IF WS-STATUS NOT = "00"
001370         GO TO AA10.
001380       DISPLAY S01.
001390     IF REC-FILE = 99
001400	       MOVE ZERO	 TO WS-TRANS
001410     ELSE
001420	       MOVE REC-TRAN	 TO WS-TRANS.
001430	     ADD 1		 TO WS-RECKEY.
001440       GO TO AA05.

001450 AA10.
001460     IF WS-TRANS = ZERO
001470         GO TO AA20.
001480       DISPLAY S02.

001490 AA15.
001500	     SUBTRACT 1 	 FROM WS-RECKEY.
001510     IF WS-RECKEY = ZERO
001520         GO TO AA20.
001530       READ RECOVER WAIT.
001540     IF WS-STATUS NOT = "00"
001550         GO TO AA50.
001560     IF REC-TRAN NOT = WS-TRANS
001570         GO TO AA20.
001580       DISPLAY S03.
001590     IF REC-FILE = 01
001600         PERFORM AY01 THRU AY999
001610     ELSE
001710     IF REC-FILE = 05
001720         PERFORM AY05 THRU AY999
001730     ELSE
001740     IF REC-FILE = 20
001750         PERFORM AY20 THRU AY999
001730     ELSE
001740     IF REC-FILE = 38
001750         PERFORM AY38 THRU AY999
001730     ELSE
001740     IF REC-FILE = 39
001750	       PERFORM AY20 THRU AY999
001730     ELSE
001740     IF REC-FILE = 40
001750	       PERFORM AY40 THRU AY999
001730	   ELSE
001740	   IF REC-FILE = 46
001750	       PERFORM AY46 THRU AY999
001730	   ELSE
001740	   IF REC-FILE = 49
001750	       PERFORM AY49 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 51
001750	       PERFORM AY051 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 52
001750	       PERFORM AY052 THRU AY999.
001760       GO TO AA15.

001770 AA20.
001780	     MOVE 4		 TO WS-PARKEY.
001790       READ PARAM WAIT.
001800	     SUBTRACT 1 	 FROM PAR-USERS.
001810	     MOVE SPACES	 TO PAR-USR(WS-SUB) PAR-PROG(WS-SUB).
001820       REWRITE PAR-RECORD1.
001830       CLOSE PARAM
001840             STOCK
001850             SPARTS
001860             DEPART
                   LEDTRF
001860             NETWORK
001870		   RECOVER
		   SHARED
		   STKALT
		   STKDEX
		   WARHSE
		   WSTOCK.
001910       DISPLAY S04.
001920       STOP RUN.

001930 AA50.
	     MOVE SPACES	 TO WS-FILE WS-KEYX.
	   IF NOT (WS-ERROR = 9)
	       IF REC-FILE = 01
		   MOVE "Parameter"
				 TO WS-FILE
		   MOVE REC-KEY	 TO WS-KEYX
	       ELSE
	       IF REC-FILE = 05
		   MOVE "Stock"  TO WS-FILE
		   MOVE STK-CODE TO WS-KEYX
	       ELSE
	       IF REC-FILE = 20
		   MOVE "Parts"  TO WS-FILE
		   MOVE PRT-EXT-ITEM
				 TO WS-KEYX
	       ELSE
	       IF REC-FILE = 37
		   MOVE "Shared" TO WS-FILE
		   MOVE REC-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 38
		   MOVE "Department"
				 TO WS-FILE
		   MOVE DPT-CODE TO WS-KEYX
	       ELSE
	       IF REC-FILE = 39
		   MOVE "Control"
				 TO WS-FILE
		   MOVE REC-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 40
		   MOVE "Integration"
				 TO WS-FILE
		   MOVE XFR-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 46
		   MOVE "Adjustments"
				 TO WS-FILE
		   MOVE STX-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 51
		   MOVE "WareHouse"
				 TO WS-FILE
		   MOVE WAR-CODE TO WS-KEYX
	       ELSE
	       IF REC-FILE = 52
		   MOVE "W/H Stock"
				 TO WS-FILE
		   MOVE WST-KEY  TO WS-KEYX.

001940	     DISPLAY S05.
001950       STOP RUN.
      /
      *    THIS ROUTINE REWRITES THE BEFORE IMAGES OF THE FILES.
      *
001960 AY000           SECTION.
001970 AY01.
001980	     MOVE REC-KEY	 TO WS-PARKEY.
001990	     MOVE REC-PARAM	 TO PAR-RECORD1.
002000       REWRITE PAR-RECORD1.
002010     IF WS-STATUS NOT = "00"
002020         PERFORM AA50.
002030       GO TO AY999.

002280 AY05.
002300	     MOVE REC-STOCKF	 TO STK-RECORD1.
           IF REC-TYPE = 0 OR 9
002310         REWRITE STK-RECORD1
           ELSE
           IF REC-TYPE = 1
               DELETE STOCK RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE STK-RECORD1.
002350	     GO TO AY999.

002360 AY20.
002380	     MOVE REC-SPARTS	 TO PRT-REC1.
           IF REC-TYPE = 0
002310         REWRITE PRT-REC1
           ELSE
           IF REC-TYPE = 1
               DELETE SPARTS RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE PRT-REC1.
002350       GO TO AY999.
      *
      *    ****    S H A R E D	 F I L E
      *
       AY37.
002370	     MOVE REC-KEY	 TO WS-SHARED.
002380	     MOVE REC-NETWORK	 TO SHR-RECORD.
002390	     REWRITE SHR-RECORD.
002400     IF WS-STATUS NOT = "00"
002420         PERFORM AA50.
002430       GO TO AY999.
      *
      *    ****   D E P A R T M E N T	/   S A L E S	L E D G E R
      * 	  F I L E
      *
       AY38.
002380	     MOVE REC-DEPART	 TO DPT-RECORD.
	   IF REC-TYPE = 0
002390	       REWRITE DPT-RECORD
           ELSE
           IF REC-TYPE = 1
	       DELETE DEPART RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE DPT-RECORD.
002400	   IF WS-STATUS NOT = "00"
002420         PERFORM AA50.
002430       GO TO AY999.
      *
      *    ****    C O N T R O L   F I L E
      *
       AY39.
002370	     MOVE REC-KEY	 TO WS-NETKEY.
002380	     MOVE REC-NETWORK	 TO NET-RECORD.
002390       REWRITE NET-RECORD.
002400     IF WS-STATUS NOT = "00"
002420         PERFORM AA50.
002430	     GO TO AY999.

       AY40.
             MOVE REC-LEDTRF     TO XFR-REC.
           IF REC-TYPE = 0
002310         REWRITE XFR-REC
           ELSE
           IF REC-TYPE = 1
               DELETE LEDTRF RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE XFR-REC.
002350       GO TO AY999.

011690 AY46.
	     MOVE REC-STKDEX	 TO STX-REC1.
	   IF REC-TYPE = 0
002310	       REWRITE STX-REC1
           ELSE
           IF REC-TYPE = 1
	       DELETE STKDEX RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE STX-REC1.
011110	     GO TO AY999.

011690 AY49.
	     MOVE REC-STKALT	 TO STKA-RECORD1.
	   IF REC-TYPE = 0
002310	       REWRITE STKA-RECORD1
           ELSE
           IF REC-TYPE = 1
	       DELETE STKALT RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE STKA-RECORD1.
011110	     GO TO AY999.

002280 AY051.
002300	     MOVE REC-WARHSE	 TO WAR-RECORD1.
	   IF REC-TYPE = 0
002310	       REWRITE WAR-RECORD1
           ELSE
           IF REC-TYPE = 1
	       DELETE WARHSE RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE WAR-RECORD1.
002350       GO TO AY999.

002280 AY052.
002300	     MOVE REC-WSTOCK	 TO WST-RECORD1.
	   IF REC-TYPE = 0
002310	       REWRITE WST-RECORD1
           ELSE
           IF REC-TYPE = 1
	       DELETE WSTOCK RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE WST-RECORD1.
002440 AY999.
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
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 	  I N T E G R A T I O N	  F I L E	    ****
      *    *************************************************************
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	    R E C O V E R Y   F I L E		    ****
      *    *************************************************************
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
      *	   *************************************************************
      *    *** S T O C K   ( I V E N T O R Y )	 P A R T S   F I L E ***
      *    *************************************************************
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
      *    *************************************************************
      *    ****	S T O C K   A L T E R N A T E	I N D E X   F I L E ****
      *    *************************************************************
	     MOVE "STKALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKALT.
      *    *************************************************************
      *    ****  A D J U S T M E N T   -   H I S T O R Y   F I L E  ****
      *    *************************************************************
	     MOVE "STKDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKDEX.
      *    *************************************************************
      *    ****      S T O C K	 ( I V E N T O R Y )   F I L E	    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
      *    *************************************************************
      *    **** 	    W A R E H O U S E	F I L E		    ****
      *    *************************************************************
	     MOVE "WARHSE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WARHSE.
      *    *************************************************************
      *    W A R E H O U S E  -  S T O C K  ( I V E N T O R Y )  F I L E
      *    *************************************************************
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
002480	     OPEN I-O PARAM.
042120     IF WS-STAT1 = "9"
042130         GO TO ZA200.
002490     IF WS-STATUS NOT = "00"
002500	       MOVE 15		 TO WS-F-ERROR
002510         GO TO ZA999-EXIT.
002520	     MOVE 1		 TO WS-PARKEY.
             READ PARAM.
002540     IF WS-STATUS NOT = "00"
002550	       MOVE 15		 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
	     MOVE PAR-DMY	 TO W12-TODAY.
      *
      *    ****    C O N T R O L   F I L E
      *
042140	     OPEN I-O NETWORK.
042150     IF WS-STAT1 = "9"
042160         GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 2		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *
      *    ****    S H A R E D	 F I L E
      *
002670	     OPEN I-O SHARED
042120     IF WS-STAT1 = "9"
042130	       GO TO ZA200.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 33		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    D E P A R T M E N T	 /   S A L E S	 L E D G E R
      * 	   F I L E
      *
       ZA01-DEPT.
042140	     OPEN I-O DEPART.
042150     IF WS-STAT1 = "9"
	       IF FLE-LOCKED
042160		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\DPTREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\DPTREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA01-DEPT.
003050	   IF WS-STATUS NOT = "00"
003060	       MOVE 7		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *
      *    ****    S T O C K   D E S C R I P T I O N   INDEX   FILE
      *
       ZA01-DESC.
002670	     OPEN I-O STKALT.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\STAREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\STAREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA01-DESC.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 49		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    S T O C K   A D J U S T M E N T S   F I L E
      *
       ZA001.
053830	     OPEN I-O STKDEX.
042120	   IF WS-STAT1 = "9"
053850	       IF FLE-LOCKED
053860		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\CARREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\CARREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA001.
053900	   IF WS-STATUS NOT = "00"
053910	       MOVE 46		 TO WS-F-ERROR
044350	       PERFORM OPEN-ERROR.
      *
      *    ****    S T O C K   F I L E
      *
       ZA01.
003040       OPEN I-O STOCK.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\STKREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\STKREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA01.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 22		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *
      *    ****    S T O C K   M E M O	 F I L E
      *
       ZA02.
003040       OPEN I-O SPARTS.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\PROREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\PROREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA02.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 21		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *
      *    ****    I N T E G R A T I O N   F I L E   ( G / L )
      *
       ZA04A.
002670       OPEN I-O LEDTRF.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\LEDREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\LEDREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA04A.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 40		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    W A R E H O U S E   F I L E
      *
       ZA04B.
003040	     OPEN I-O WARHSE
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\WARREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\WARREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA04B.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 51		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *
      *    ****    W A R E H O U S E   -   S T O C K   F I L E
      *
       ZA04C.
003040	     OPEN I-O WSTOCK
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\WSTREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\WSTREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA04C.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 52		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
002520	     MOVE 4		 TO WS-PARKEY.
002530       READ PARAM.
002540     IF WS-STATUS NOT = "00"
002550	       MOVE 15		 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
002840     IF PAR-USERS = ZERO
002850	       MOVE 9		 TO WS-ERROR
002860         GO TO ZA999-EXIT.
002870	     MOVE 1		 TO WS-SUB.

002880 ZA05.
002890	   IF PAR-USR(WS-SUB) = LS-USER
002900	       MOVE PAR-PROG(WS-SUB)
				 TO WS-PROG
002910         GO TO ZA999-EXIT.
002920     IF WS-SUB < 24
002930	       ADD 1		 TO WS-SUB
002940         GO TO ZA05.
002950	     MOVE 9		 TO WS-ERROR.
002960       GO TO ZA999-EXIT.

020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2212
020130                WITH FOREGROUND-COLOR 14.
020140       DISPLAY "Press any key" AT 2312 
020150                WITH FOREGROUND-COLOR 14.
020160       ACCEPT WS-OPTION AT 2326 WITH FOREGROUND-COLOR 15 AUTO.
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
041800     ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 21
041860	       MOVE W02-SPARTS	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE PRT-EXT-ITEM TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 22
041860	       MOVE W02-STOCKF	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 40
041860	       MOVE W02-LEDTRF	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 46
041860	       MOVE W02-STKDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 49
041860	       MOVE W02-STKALT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STKA-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 51
041860	       MOVE W02-WARHSE	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE WAR-CODE	 TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 52
041860	       MOVE W02-WSTOCK	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.
041880     IF WS-STATUS = "10"
041890	       MOVE "End of FILE"
				 TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920	       MOVE "Duplicate record number"
				 TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950	       MOVE "Invalid record number"
				 TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980	       MOVE "DISK full"  TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040	       MOVE "FILE locked"
				 TO WS-STAT-MESSAGE.
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
