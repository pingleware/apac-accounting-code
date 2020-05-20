      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   ********   ******   *******   *******   *******   ******     *
      *      **     **	  **  **    **  **    **  **	     **    **    *
      *      **     **	      **    **  **   **	  **	     **          *
      *      **     **	      *******   ******	  *****    **          *
      *      **     **	      **	    **   **	  **	     **          *
      *   ** **     **	  **  **	    **    **  **	     **    **    *
      *    ***	   ******   **	    **    **  *******   ******     *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *     J O B   C O S T I N G   R E C O V E R Y   P R O G R A M    *
      *                                                                *
      *     VERSION 9.00.00 - October 2012                             *
      *                                                                *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JCPREC.
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
000080                 CONSOLE IS CRT.

000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY "APACFIDS.SL".

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

      $SET KEYCOMPRESS "7"
000010	   SELECT ITEMS   ASSIGN DISK
                          LOCK EXCLUSIVE
000040			  STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
000070			  RECORD ITM-MODEL-NO
000130			  ALTERNATE RECORD ITM-DKEY DUPLICATES.

000010	   SELECT JCPHST  ASSIGN DISK
			  LOCK EXCLUSIVE
000040			  STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
000070			  RECORD HST-KEY
000130			  ALTERNATE RECORD HST-NKEY DUPLICATES
			  ALTERNATE RECORD HST-AKEY =
				    HST-AC HST-DATE DUPLICATES
			  ALTERNATE RECORD HST-DKEY =
				    HST-DATE HST-REF
			  ALTERNATE RECORD HST-RKEY =
				    HST-KEY HST-DATE
000130			  ALTERNATE RECORD HST-REF DUPLICATES.

000230	   SELECT JOBCST  ASSIGN DISK
			  LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270			  RECORD JOB-KEY
			  ALTERNATE RECORD JOB-DETAILS =
				 JOB-TYPE JOB-EXT-ITEM JOB-REF
				 DUPLICATES.

000230	   SELECT JOBHDR  ASSIGN DISK
			  LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270			  RECORD JHD-KEY
			  ALTERNATE RECORD JHD-KEY2 DUPLICATES
			  ALTERNATE RECORD JHD-KEY3 DUPLICATES
			  ALTERNATE RECORD JHD-KEY4 DUPLICATES
			  ALTERNATE RECORD JHD-KEY5 DUPLICATES
			  ALTERNATE RECORD JHD-KEY6 DUPLICATES.

000010	   SELECT JOBSTAT ASSIGN DISK
			  LOCK EXCLUSIVE
000040			  STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
000070			  RECORD JST-KEY.

000230	   SELECT JOBTECH ASSIGN DISK
			  LOCK EXCLUSIVE
000240			  STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270			  RECORD TIM-KEY
			  ALTERNATE RECORD TIM-JOB-KEY =
			    TIM-JOB TIM-TECH
			    DUPLICATES.
      $SET NOKEYCOMPRESS

000240 COPY "RECOVER.SL".

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY "APACFIDS.FDE".

       COPY "CONTROL.IFD".

       COPY "ITEMS.FD".

       COPY "JCPHST.FD".

       COPY "JOBCST.FD".

       COPY "JOBHDR.FD".

       COPY "JOBSTAT.FD".

       COPY "JOBTECH.FD".

000290 COPY "PARAM.IFD".

000410 COPY "RECOVER.GFD".

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC  X(18)     VALUE "aeWlimemnomLalismJ".
000440 77  WS-S1	    PIC 9(04)	  COMP-5.
001890 77  WS-S2	   PIC	9(04)	 COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
000440 77  WS-SUB	    PIC 9(04)	  COMP-5.
000450 77  WS-PARKEY        PIC 9(04)     COMP-5.
000450 77  WS-NETKEY        PIC 9(04)     COMP-5.
000510 77  WS-RECKEY        PIC 9(04)     COMP-5.
000520 77  WS-RECOVER       PIC 9(04)     COMP-5.
       77  WS-NXTKEY        PIC  9(04)    COMP-5.
       77  WS-INVOIC        PIC  9(04)    COMP-5.
000530 77  WS-TRANS         PIC 9(04)     COMP-5 VALUE 1.
000540 77  WS-EKEY          PIC Z(04)9.
000560 77  WS-ERROR         PIC 9(01)     VALUE ZERO.
000570 77  WS-PROG          PIC X(02).
       77  WS-OPTION        PIC X(01).
       77  WS-LINK          PIC 9(01)     VALUE ZERO.
004360 77  WS-INDS          PIC 9(01)     VALUE ZERO.
004360 77  WS-INDD          PIC 9(01)     VALUE ZERO.
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

       COPY "WS.WS".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

000610 01  W02-FID.

       COPY "APACFIDS.ID".

       COPY "CONTROL.ID".

       COPY "ITEMS.ID".

       COPY "JCPHST.ID".

       COPY "JOBCST.ID".

       COPY "JOBHDR.ID".

       COPY "JOBSTAT.ID".

       COPY "JOBTECH.ID".

       COPY "PARAM.ID".

000410 COPY "RECOVER.ID".

       COPY "W12.WS".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W70.WS".

       COPY "W70DEBT.IWS".

       LINKAGE SECTION.

       COPY "CHAIN.LS".

      /
000850 SCREEN SECTION.

       COPY "BLANK.CRT".

000860 01  S00.
000910     03  LINE  2 COLUMN 29 FOREGROUND-COLOR 15
000920                 VALUE "DEBTOR RECOVERY PROGRAM".
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
001090     03  LINE 19 COLUMN 12 FOREGROUND-COLOR 11
001100                 VALUE "Recovery complete".
001110     03  LINE 21 COLUMN 12 FOREGROUND-COLOR 11
001120                 VALUE "You may now restart the system".

001130 01  S05.
001140     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001150                 VALUE "Recovery unsuccessfull !".
001160     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001200                 VALUE "since creating the last backup disk/s".

      *** PATCH FILE POINTERS - DISPLAY
005940 01  S08.
005950     03  LINE  2 COLUMN 31 FOREGROUND-COLOR 15
                                 VALUE "FIX FILE POINTERS".
           03  LINE  4 COLUMN 12 VALUE "Reading Transaction File".
           03  LINE  6 COLUMN 12 VALUE "Reading Audit File".

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
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
001270	     PERFORM ZA000.
001260       DISPLAY S00.
001280     IF WS-ERROR = 9
001290	       GO TO AA50.
      *
      *    ****    R E C O V E R Y   F I L E
      *
001320       OPEN INPUT RECOVER.
001330	     MOVE 1		 TO WS-RECKEY.

001340 AA05.
001350       READ RECOVER.
001360     IF WS-STATUS NOT = "00"
001370         GO TO AA10.
001380       DISPLAY S01.
      *
      *    ****    E N D   O F	 T R A N S A C T I O N	 ?
      *
001390     IF REC-FILE = 99
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
001460     IF WS-TRANS = ZERO
001470         GO TO AA20.
      *
      *    ****    R E C O V E R Y   R E Q U I R E D
      *
001480       DISPLAY S02.
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
001550         GO TO AA50.
      *
      *    ****    R E C O V E R Y   C O M P L E T E   ?
      * 	   ( T R A N S A C T I O N   N U M B E R
      * 	     C H A N G E D )
      *
001560     IF REC-TRAN NOT = WS-TRANS
001570         GO TO AA20.
001580       DISPLAY S03.
      *
      *    ****    D E T E R M I N E   W H I C H   F I L E
      *
001590     IF REC-FILE = 01
001600         PERFORM AY01 THRU AY999
001730	   ELSE
001740	   IF REC-FILE = 21
001750		PERFORM AY21 THRU AY999
001610	   ELSE
001740	   IF REC-FILE = 25
001750	       PERFORM AY25 THRU AY999
001730	   ELSE
001740	   IF REC-FILE = 39
001750		PERFORM AY39 THRU AY999
001730	   ELSE
001740	   IF REC-FILE = 57
001750	       PERFORM AY057 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 58
001750	       PERFORM AY058 THRU AY999
001730*    ELSE
001740*    IF REC-FILE = 59
001750*        PERFORM AY059 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 60
001750	       PERFORM AY060 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 61
001750	       PERFORM AY061 THRU AY999.
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
001830	     CLOSE ITEMS
		   JCPHST
		   JOBCST
		   JOBHDR
		   JOBSTAT
		   JOBTECH
		   PARAM
                   NETWORK
001870		   RECOVER.
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
      *    ****    J O B   C O S T I N G   -   H E A D E R   F I L E
      *
       AY21.
002300	     MOVE REC-JOBHDR	 TO JHD-REC1.
           IF REC-TYPE = 0
002310	       REWRITE JHD-REC1
           ELSE
           IF REC-TYPE = 1
	       DELETE JOBHDR RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE JHD-REC1.
002350       GO TO AY999.
      *
      *    ****    J O B   C O S T I N G   -   L I N E	 F I L E
      *
       AY25.
002300	     MOVE REC-JOBCST	 TO JOB-REC1.
           IF REC-TYPE = 0
002310         REWRITE JOB-REC1
           ELSE
           IF REC-TYPE = 1
               DELETE JOBCST RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE JOB-REC1.
002350       GO TO AY999.
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
      *
      *    ****    I T E M   /	 M O D E L   F I L E
      *
       AY057.
002300	     MOVE REC-ITEMS	 TO ITM-RECORD1.
           IF REC-TYPE = 0
002310	       REWRITE ITM-RECORD1
           ELSE
           IF REC-TYPE = 1
	       DELETE ITEMS RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE ITM-RECORD1.
002430	     GO TO AY999.
      *
      *    ****    J O B   H I S T O R Y   F I L E
      *
       AY058.
002300	     MOVE REC-JCPHST	 TO HST-RECORD1.
           IF REC-TYPE = 0
002310	       REWRITE HST-RECORD1
           ELSE
           IF REC-TYPE = 1
	       DELETE JCPHST RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE HST-RECORD1.
002430	     GO TO AY999.
      *
      *    ****    J O B   S T A T U S	 F I L E
      *
       AY060.
002300	     MOVE REC-JOBSTAT	 TO JST-REC1.
           IF REC-TYPE = 0
002310	       REWRITE JST-REC1
           ELSE
           IF REC-TYPE = 1
	       DELETE JOBSTAT RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE JST-REC1.
002430	     GO TO AY999.
      *
      *    ****    T I M E   R E C O R D   -   T E C H N I C I A N
      *
       AY061.
002300	     MOVE REC-JOBTECH	 TO TIM-REC1.
           IF REC-TYPE = 0
002310	       REWRITE TIM-REC1
           ELSE
           IF REC-TYPE = 1
	       DELETE JOBTECH RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE TIM-REC1.

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
001250       DISPLAY CLR-SCREEN.
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
	     MOVE "ITEMS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-ITEMS.
	     MOVE "JCHIST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JCPHST.
	     MOVE "JOBCST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBCST.
	     MOVE "JOBHDR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBHDR.
	     MOVE "JOBSTAT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBSTAT.
	     MOVE "JOBTECH"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBTECH.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
002480	     OPEN I-O PARAM.
042120     IF WS-STAT1 = "9"
	       MOVE WS-PARID	 TO WS-FILE
042130         GO TO ZA200.
002490     IF WS-STATUS NOT = "00"
002500	       MOVE 15		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
002520	     MOVE 1		 TO WS-PARKEY.
             READ PARAM.
002540     IF WS-STATUS NOT = "00"
002550	       MOVE 15		 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
	     MOVE PAR-DMY	 TO W12-TODAY.
      *
      *    ****    C O N T R O L   F I L E
      *
002670       OPEN I-O NETWORK
042120     IF WS-STAT1 = "9"
	       MOVE W02-NETWORK	 TO WS-FILE
042130         GO TO ZA200.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 2		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    S Y S T E M	 S T A T U S   R E C O R D
      *
002520	     MOVE 4		 TO WS-PARKEY.
002530       READ PARAM.
002540     IF WS-STATUS NOT = "00"
002550	       MOVE 15		 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
002840     IF PAR-USERS = ZERO
002850	       MOVE 9		 TO WS-ERROR
002860         GO TO ZA999-EXIT.
002870	     MOVE 1		 TO WS-SUB.

002880 ZA01.
002890	   IF PAR-USR (WS-SUB) = LS-USER
002900	       MOVE PAR-PROG (WS-SUB)
				 TO WS-PROG
002910	       GO TO ZA02.
002920     IF WS-SUB < 24
002930	       ADD 1		 TO WS-SUB
002940	       GO TO ZA01.
002950	     MOVE 9		 TO WS-ERROR.
002960       GO TO ZA999-EXIT.
      *
      *    ****    J O B   L I N E   F I L E
      *
       ZA02.
002670	     OPEN I-O JOBCST.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-JOBCST
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\JOBREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\JOBREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA02.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 13		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    J O B   H E A D E R	 F I L E
      *
       ZA03.
038970	     OPEN I-O JOBHDR.
042120     IF WS-STAT1 = "9"
038990         IF FLE-LOCKED
		   MOVE W02-JOBHDR
				 TO WS-FILE
039000		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\JOBREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\JOBREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA03.
039080	   IF WS-STATUS NOT = "00"
039090	       MOVE 14		 TO WS-F-ERROR
039100         PERFORM OPEN-ERROR.
      *
      *    ****    I T E M   /	 M O D E L   F I L E
      *
       ZA04.
038970	     OPEN I-O ITEMS.
042120     IF WS-STAT1 = "9"
038990         IF FLE-LOCKED
		   MOVE W02-ITEMS
				 TO WS-FILE
039000		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\JITREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\JITREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA04.
039080	   IF WS-STATUS NOT = "00"
039090	       MOVE 57		 TO WS-F-ERROR
039100         PERFORM OPEN-ERROR.
      *
      *    ****    J O B   H I S T O R Y  F I L E
      *
       ZA05.
038970	     OPEN I-O JCPHST.
042120     IF WS-STAT1 = "9"
038990         IF FLE-LOCKED
		   MOVE W02-JCPHST
				 TO WS-FILE
039000		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\JHSREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\JHSREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA05.
039080	   IF WS-STATUS NOT = "00"
039090	       MOVE 58		 TO WS-F-ERROR
039100         PERFORM OPEN-ERROR.
      *
      *    ****    J O B   S T A T U S	 F I L E
      *
       ZA06.
038970	     OPEN I-O JOBSTAT.
042120     IF WS-STAT1 = "9"
038990         IF FLE-LOCKED
		   MOVE W02-JOBSTAT
				 TO WS-FILE
039000		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\JSTREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\JSTREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA06.
039080	   IF WS-STATUS NOT = "00"
039090	       MOVE 60		 TO WS-F-ERROR
039100         PERFORM OPEN-ERROR.
      *
      *    ****    T E C H N I C I A N	 T I M E   F I L E
      *
       ZA07.
038970	     OPEN I-O JOBTECH.
042120     IF WS-STAT1 = "9"
038990         IF FLE-LOCKED
		   MOVE W02-JOBTECH
				 TO WS-FILE
039000		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\JTCREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\JTCREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA07.
039080	   IF WS-STATUS NOT = "00"
039090	       MOVE 61		 TO WS-F-ERROR
039100         PERFORM OPEN-ERROR.
002960	     GO TO ZA999-EXIT.

020110 ZA200.
	     DISPLAY "File : " AT 2112 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		      WS-FILE WITH FOREGROUND-COLOR 7 HIGHLIGHT.
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
041490	     DISPLAY CLR-SCREEN.
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
041610	   IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041230	   ELSE
041240     IF WS-F-ERROR = 13
041250         MOVE W02-JOBCST   TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
               MOVE JOB-KEY      TO WS-KEYX
041110	   ELSE
041240	   IF WS-F-ERROR = 14
041250	       MOVE W02-JOBHDR	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE JHD-KEY2	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 57
041860	       MOVE W02-ITEMS	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE ITM-MODEL-NO TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 58
041860	       MOVE W02-JCPHST	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE HST-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 60
041860	       MOVE W02-JOBSTAT	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE JST-KEY	 TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 61
041860	       MOVE W02-JOBTECH	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE TIM-KEY	 TO WS-KEYX.
041880	   IF WS-STATUS = "10"
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
