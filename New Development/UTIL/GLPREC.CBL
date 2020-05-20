      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *    ******   **        *******   *******   *******   ******     *
      *   **    **  **        **    **  **    **  **       **    **    *
      *   **        **        **    **  **   **   **       **          *
      *   **        **        *******   ******    *****    **          *
      *   **   ***  **        **        **   **   **       **          *
      *   **    **  **        **        **    **  **       **    **    *
      *    ******   ********  **        **    **  *******   ******     *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *       G E N E R A L   L E D G E R   R E C O V E R Y   P R O G  *
      *                                                                *
      *     VERSION 9.00.00 - October 2012                             *
      *                                                                *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     GLPREC.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   DECEMBER 1986.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1986 - 2013
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
000080                CONSOLE IS CRT.

000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY "APACFIDS.SL".

000110	   SELECT NETWORK ASSIGN DISK
                          LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000130                    ACCESS RANDOM
000140                    ORGANIZATION RELATIVE
000150                    RELATIVE WS-NETKEY.

000110     SELECT DEPART  ASSIGN DISK
                          LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000130                    ACCESS RANDOM
000140                    ORGANIZATION RELATIVE
000150                    RELATIVE WS-DEPKEY.

000110     SELECT PARAM  ASSIGN DISK
                         LOCK EXCLUSIVE
000120                   STATUS WS-STATUS
000130                   ACCESS RANDOM
000140                   ORGANIZATION RELATIVE
000150                   RELATIVE WS-PARKEY.

      $SET KEYCOMPRESS "7"
000230     SELECT CREDIT ASSIGN DISK
                         LOCK EXCLUSIVE
000240                   STATUS WS-STATUS
000250                   ACCESS DYNAMIC
000260                   ORGANIZATION INDEXED
000270                   RECORD P-NUMBER
                         ALTERNATE RECORD P-NKEY DUPLICATES.
      $SET NOKEYCOMPRESS

000180     SELECT CTRANS ASSIGN DISK
                         LOCK EXCLUSIVE
000190                   STATUS WS-STATUS
000200                   ACCESS RANDOM
000210                   ORGANIZATION RELATIVE
000220                   RELATIVE WS-CTRKEY.

      $SET KEYCOMPRESS "7"
000110     SELECT GACCNT ASSIGN DISK
                         LOCK EXCLUSIVE
000170			 STATUS WS-STATUS
000180			 ACCESS DYNAMIC
000260			 ORGANIZATION INDEXED
000270			 RECORD G-AC
			 ALTERNATE RECORD G-FSKEY =
			     G-TYPE G-AC.

000210	   SELECT GLTRAN ASSIGN DISK
			 LOCK EXCLUSIVE
000220                   STATUS WS-STATUS
000230                   ACCESS DYNAMIC
000240                   ORGANIZATION INDEXED
000250                   RECORD T-KEY
			 ALTERNATE RECORD T-AKEY DUPLICATES
			 ALTERNATE RECORD T-VKEY =
			     T-AC T-VAL T-DATE DUPLICATES.

000210	   SELECT GPDCHQ ASSIGN DISK
			 LOCK EXCLUSIVE
000220                   STATUS WS-STATUS
000230                   ACCESS DYNAMIC
000240                   ORGANIZATION INDEXED
000250			 RECORD GPD-KEY
			 ALTERNATE RECORD GPD-BNK =
				   GPD-BANK GPD-REFER DUPLICATES.

000210	   SELECT LEDTRF ASSIGN DISK
                         LOCK EXCLUSIVE
000220                   STATUS WS-STATUS
000230                   ACCESS DYNAMIC
000240                   ORGANIZATION INDEXED
000250                   RECORD XFR-KEY.

000210	   SELECT PDCHEQ ASSIGN DISK
                         LOCK EXCLUSIVE
000220			 STATUS WS-STATUS
000230                   ACCESS DYNAMIC
000240                   ORGANIZATION INDEXED
000250			 RECORD PDT-KEY
			 ALTERNATE RECORD PDT-AC DUPLICATES
			 ALTERNATE RECORD PDT-REFER DUPLICATES.
      $SET NOKEYCOMPRESS

000240 COPY "RECOVER.SL".

      $SET KEYCOMPRESS "7"
000230     SELECT TXTRAN  ASSIGN DISK
                          LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270                    RECORD TAX-KEY.
      $SET NOKEYCOMPRESS

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY "APACFIDS.FDE".

000290 COPY "CONTROL.IFD".

000290 COPY "DEPART.GFD".

000290 COPY "PARAM.IFD".

000140 COPY "CREDIT.GFD".

000160 COPY "CRTRAN.FD".

000200 COPY "GACCNT.FD".

000220 COPY "GLTRAN.FD".

000220 COPY "GPDCHQ.FD".

       COPY "LEDTRF.FD".

000220 COPY "PDCHEQ.FD".

000410 COPY "RECOVER.GFD".

       COPY "TXTRAN.IFD".

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC  X(18)     VALUE "aeWlimemnomLalismJ".
000440 77  WS-S1	    PIC 9(04)	  COMP-5.
001890 77  WS-S2	   PIC	9(04)	 COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
000440 77  WS-SUB	    PIC 9(04)	  COMP-5.
003290 77  WS-GLKEY         PIC 9(04)     COMP-5 VALUE 1.
003290 77  WS-NETKEY        PIC 9(04)     COMP-5 VALUE 1.
003290 77  WS-DEPKEY        PIC 9(04)     COMP-5 VALUE 1.
003300 77  WS-CTRKEY        PIC 9(04)     COMP-5 VALUE 1.
003330 77  WS-GTRKEY        PIC 9(04)     COMP-5 VALUE 1.
003340 77  WS-PARKEY        PIC 9(04)     COMP-5 VALUE 1.
003350 77  WS-OTRKEY        PIC 9(04)     COMP-5 VALUE 1.
003360 77  WS-RECKEY        PIC 9(04)     COMP-5.
000520 77  WS-RECOVER       PIC 9(05)     COMP-0.
000530 77  WS-TRANS         PIC 9(05)     COMP-0 VALUE 1.
000540 77  WS-EKEY          PIC Z(04)9.
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

000590 COPY "WS.WS".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

004000 01  W02-FID.

       COPY "APACFIDS.ID".

000290 COPY "CONTROL.ID".

000290 COPY "DEPART.ID".

000140 COPY "CREDIT.ID".

000160 COPY "CRTRAN.ID".

000200 COPY "GACCNT.ID".

000220 COPY "GLTRAN.ID".

       COPY "LEDTRF.ID".

       COPY "PARAM.ID".

000410 COPY "RECOVER.ID".

       COPY "TXTRAN.ID".

       COPY "W12.WS".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       LINKAGE SECTION.

       COPY "CHAIN.LS".

      /
000850 SCREEN SECTION.

       COPY "BLANK.CRT".

000860 01  S00.
000910     03  LINE  2 COLUMN 29 FOREGROUND-COLOR 3
000920                 VALUE "General Ledger Recovery Program".
000930     03  LINE  4 COLUMN  1 FOREGROUND-COLOR 3
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
001140     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001150                 VALUE "Recovery unsuccessfull !".
001160     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001200                 VALUE "since creating the last backup disk/s".

       COPY "ERROR.CRT".

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
001350       READ RECOVER.
001360     IF WS-STATUS NOT = "00"
001370         GO TO AA10.
001380       DISPLAY S01.
      *
      *    ****    E N D   O F	 T R A N S A C T I O N	 ?
      *
001390	   IF REC-FILE = 99
001400	       MOVE ZERO	 TO WS-TRANS
001410     ELSE
      *
      *    ****    S T O R E   T R A N S A C T I O N   N U M B E R
      *
001420	       MOVE REC-TRAN	 TO WS-TRANS.
001430	     ADD 1		 TO WS-RECKEY.
001440       GO TO AA05.
001450 AA10.
      *
      *    ****    A N Y T H I N G   T O   R E C O V E R   ?
      *
001460	   IF WS-TRANS = ZERO
001470         GO TO AA20.
      *
      *    ****    R E C O V E R Y   R E Q U I R E D
      *
001480	     DISPLAY S02.
001490 AA15.
      *
      *    ****    R E A D   R E C O V E R Y   F I L E
      * 	   F R O M   T H E   L A S T   R E C O R D
      * 	   A N D   R E W R I T E   R E C O R D S   I N
      * 	   R E V E R S E   S E Q U E N C E
      *
001500	     SUBTRACT 1 	 FROM WS-RECKEY.
001510     IF WS-RECKEY = ZERO
001520         GO TO AA20.
001530       READ RECOVER.
001540     IF WS-STATUS NOT = "00"
               DISPLAY "AA15" AT 2002
001550         GO TO AA50.
      *
      *    ****    R E C O V E R Y   C O M P L E T E   ?
      * 	   ( T R A N S A C T I O N   N U M B E R
      * 	     C H A N G E D )
      *
001560	   IF REC-TRAN NOT = WS-TRANS
001570         GO TO AA20.
001580       DISPLAY S03.
      *
      *    ****    D E T E R M I N E   W H I C H   F I L E
      *
001590	   IF REC-FILE = 01
001600         PERFORM AY01 THRU AY999
001610     ELSE
001650     IF REC-FILE = 07
001660         PERFORM AY07 THRU AY999
001670     ELSE
001680     IF REC-FILE = 08
001690         PERFORM AY08 THRU AY999
001700     ELSE
001620     IF REC-FILE = 12
001630         PERFORM AY12 THRU AY999
001640     ELSE
001710     IF REC-FILE = 13
001720         PERFORM AY13 THRU AY999
001730     ELSE
001740     IF REC-FILE = 19
001750         PERFORM AY19 THRU AY999
001730     ELSE
001740     IF REC-FILE = 38
001750         PERFORM AY38 THRU AY999
001730     ELSE
001740     IF REC-FILE = 39
001750	       PERFORM AY39 THRU AY999
001730     ELSE
001740     IF REC-FILE = 40
001750	       PERFORM AY40 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 44
001750	       PERFORM AY44 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 45
001750	       PERFORM AY45 THRU AY999.
001760       GO TO AA15.
      *
      *    ****   R E C O V E R Y   I S   C O M P L E T E
      *
001770 AA20.
      *
      *    ****    R E S E T   R E C O V E R Y	 S T A T U S   F O R
      * 	   T H I S   W O R K   S T A T I O N
      *
001780	     MOVE 4		 TO WS-PARKEY.
001790       READ PARAM.
001800	     SUBTRACT 1 	 FROM PAR-USERS.
001810	     MOVE SPACES	 TO PAR-USR (WS-SUB) PAR-PROG (WS-SUB).
001820       REWRITE PAR-RECORD1.
	   IF LS0-CRP = 2
               CLOSE CREDIT
                     CTRANS.
001830       CLOSE PARAM
                   GACCNT
001860		   GLTRAN
		   GPDCHQ
		   LEDTRF
		   PDCHEQ
001870             RECOVER.
001910       DISPLAY S04.
001920       STOP RUN.

001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.
      /
      *    THIS ROUTINE REWRITES THE BEFORE IMAGES OF THE FILES.
      *
001960 AY000           SECTION.
      *
      *    ****    P A R A M E T E R   F I L E
      *
001970 AY01.
001980	     MOVE REC-KEY	 TO WS-PARKEY.
001990	     MOVE REC-PARAM	 TO PAR-RECORD1.
002000       REWRITE PAR-RECORD1.
002010     IF WS-STATUS NOT = "00"
002020         PERFORM AA50.
002030       GO TO AY999.
      *
      *    ****    C R E D I T O R   A C C O U N T   F I L E
      *
011840 AY07.
011870	     MOVE REC-CREDIT	 TO P-REC.
           IF REC-TYPE = 0
002310         REWRITE P-REC
           ELSE
           IF REC-TYPE = 1
               DELETE CREDIT RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE P-REC.
011880       GO TO AY999.
      *
      *    ****    C R E D I T O R   T R A N S A C T I O N   F I L E
      *
011890 AY08.
011910	     MOVE REC-KEY	 TO WS-CTRKEY.
011920	     MOVE REC-CRTRAN	 TO CT-REC.
002310       REWRITE CT-REC.
002320     IF WS-STATUS = "00"
               IF CT-REC = LOW-VALUES
                   DELETE CTRANS RECORD.
011930       GO TO AY999.
      *
      *    ****    G / L E D G E R   A C C O U N T   F I L E
      *
011940 AY12.
011970	     MOVE REC-GACCNT	 TO G-REC.
           IF REC-TYPE = 0
002310	       REWRITE G-REC
	   ELSE
           IF REC-TYPE = 1
	       DELETE GACCNT RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE G-REC.
011980       GO TO AY999.
      *
      *    ****    G / L E D G E R   T R A N S A C T I O N   F I L E
      *
011990 AY13.
012020	     MOVE REC-GLTRAN	 TO T-REC.
           IF REC-TYPE = 0
002310         REWRITE T-REC
           ELSE
           IF REC-TYPE = 1
               DELETE GLTRAN RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE T-REC.
012030       GO TO AY999.
      *
      *    ****   V A T   F I L E   -	C U R R E N T	R A T E
      *
       AY19.
002300	     MOVE REC-TXTRAN	 TO TAX-RECORD1.
           IF REC-TYPE = 0
002310         REWRITE TAX-RECORD1
           ELSE
           IF REC-TYPE = 1
               DELETE TXTRAN RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE TAX-RECORD1.
002350       GO TO AY999.
      *
      *    ****   D E P A R T M E N T	/   S A L E S	L E D G E R
      * 	  F I L E
      *
       AY38.
002370	     MOVE REC-KEY	 TO WS-DEPKEY.
002380	     MOVE REC-DEPART	 TO DPT-RECORD.
002390       REWRITE DPT-RECORD.
002400     IF WS-STATUS NOT = "00"
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
002430       GO TO AY999.
      *
      *    ****   I N T E G R A T I O N   F I L E   ( G / L )
      *
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
      *
      *    ****   C R E D I T O R   P D   C H E Q U E	F I L E
      *
       AY44.
	     MOVE REC-PDCHEQ	 TO PDT-REC.
           IF REC-TYPE = 0
002310	       REWRITE PDT-REC
           ELSE
           IF REC-TYPE = 1
	       DELETE PDCHEQ RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE PDT-REC.
002350       GO TO AY999.
      *
      *    ****   G / L   P D	C H E Q U E   F I L E
      *
       AY45.
	     MOVE REC-GPDCHQ	 TO GPD-REC.
           IF REC-TYPE = 0
002310	       REWRITE GPD-REC
           ELSE
           IF REC-TYPE = 1
	       DELETE GPDCHQ RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE GPD-REC.
002350       GO TO AY999.

002440 AY999.
002450       EXIT.

       COPY "FUNCTION.SCR".

       COPY "ERROR.SCR".

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
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
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
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
002480	     OPEN I-O PARAM.
002490     IF WS-STATUS NOT = "00"
002500	       MOVE 15		 TO WS-F-ERROR
002510         PERFORM OPEN-ERROR.
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
      *    ****    D E P A R T M E N T	 /   S A L E S	 L E D G E R
      * 	   F I L E
      *
042140	     OPEN I-O DEPART.
042150     IF WS-STAT1 = "9"
042160         GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 7		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *
      *    ****    G / L E D G E R    A C C O U N T   F I L E
      *
042170	     OPEN I-O GACCNT.
042180     IF WS-STAT1 = "9"
	       IF FLE-LOCKED
042190		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\GACREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\GACREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA02.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 8		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *
      *    ****    G / L E D G E R   T R A N S A C T I O N   F I L E
      *
       ZA02.
003040       OPEN I-O GLTRAN.
042120     IF WS-STAT1 = "9"
	       IF FLE-LOCKED
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\GTRREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\GTRREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA02.
003050	   IF WS-STATUS NOT = "00"
003060	       MOVE 11		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *
      *    ****   V A T   F I L E   ( C U R R E N T   R A T E )
      *
       ZA03.
002670       OPEN I-O TXTRAN.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\TAXREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\TAXREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA03.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 32		 TO WS-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    I N T E G R A T I O N   F I L E   ( G / L )
      *
       ZA04.
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
	       GO TO ZA04.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 40		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
	   IF LS0-CRP NOT = 2
               GO TO ZA04B.
      *
      *    ****    C R E D I T O R   T R A N S A C T I O N   F I L E
      *
042210	     OPEN I-O CTRANS.
042220     IF WS-STAT1 = "9"
042230         GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 4		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *
      *    ****    C R E D I T O R    A C C O U N T   F I L E
      *
       ZA04A.
042240       OPEN I-O CREDIT.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\CRDREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\CRDREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA04A.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 3		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.

       ZA04B.
019900	     OPEN I-O GPDCHQ.
	   IF WS-STATUS = "05"
	       CLOSE GPDCHQ
	       OPEN OUTPUT GPDCHQ
	       CLOSE GPDCHQ
	       OPEN I-O GPDCHQ.
042250	   IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\GPDREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\GPDREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA04B.
050190	   IF WS-STATUS NOT = "00"
050200	       MOVE 45		  TO WS-F-ERROR
050210	       PERFORM OPEN-ERROR.

       ZA04C.
019900	     OPEN I-O PDCHEQ.
	   IF WS-STATUS = "05"
	       CLOSE PDCHEQ
	       OPEN OUTPUT PDCHEQ
	       CLOSE PDCHEQ
	       OPEN I-O PDCHEQ.
042250	   IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\PDCREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\PDCREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA04B.
050190	   IF WS-STATUS NOT = "00"
050200	       MOVE 44		  TO WS-F-ERROR
050210	       PERFORM OPEN-ERROR.
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
002890	   IF PAR-USR (WS-SUB) = LS-USER
002900	       MOVE PAR-PROG (WS-SUB)
				 TO WS-PROG
002910         GO TO ZA999-EXIT.
002920     IF WS-SUB < 24
002930	       ADD 1		 TO WS-SUB
002940         GO TO ZA05.
002950	     MOVE 9		 TO WS-ERROR.
002960       GO TO ZA999-EXIT.

020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2412
020130                WITH FOREGROUND-COLOR 14.
020140       DISPLAY "Press any key" AT 2512
020150		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
020160	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
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
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT TO WS-FILE
041670         MOVE ZERO      TO WS-KEY
               MOVE P-NUMBER     TO WS-KEYX
041680     ELSE
041690     IF WS-F-ERROR = 4
041700         MOVE W02-CRTRAN TO WS-FILE
041710         MOVE WS-CTRKEY TO WS-KEY
041800     ELSE
041810     IF WS-F-ERROR = 7
041820         MOVE W02-DEPART TO WS-FILE
041830         MOVE WS-DEPKEY TO WS-KEY
041840     ELSE
041810     IF WS-F-ERROR = 8
041820         MOVE W02-GACCNT TO WS-FILE
041830         MOVE WS-GLKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 11
041860         MOVE W02-GLTRAN TO WS-FILE
041870         MOVE WS-GTRKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER TO WS-FILE
041870         MOVE WS-RECKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN TO WS-FILE
041870         MOVE ZERO       TO WS-KEY
               MOVE TAX-KEY      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF TO WS-FILE
041870         MOVE ZERO       TO WS-KEY
               MOVE XFR-KEY    TO WS-KEYX.
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
