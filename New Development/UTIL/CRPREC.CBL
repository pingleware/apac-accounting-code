      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******   *******   *******   ******     *
      *   **    **  **    **  **    **  **    **  **       **    **    *
      *   **        **   **   **    **  **   **   **       **          *
      *   **        ******    *******   ******    *****    **          *
      *   **        **   **   **        **   **   **       **          *
      *   **    **  **    **  **        **    **  **       **    **    *
      *    ******   **    **  **        **    **  *******   ******     *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *       C R E D I T O R   R E C O V E R Y   P R O G R A M        *
      *                                                                *
      *     VERSION 9.00.00 - October 2012                             *
      * 							       *
      ******************************************************************
      * 							       *
      *  Aug 2011 - Include the Creditor Memo File in the recovery     *
      * 	    procedures. See CRP005 - Memo maintenance program. *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CRPREC.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   DECEMBER 1986.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1986 - 2012
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

      $SET KEYCOMPRESS "7"
000010	   SELECT DEPART  ASSIGN DISK
000020			  LOCK EXCLUSIVE
000040                    STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD DPT-CODE
			  ALTERNATE RECORD DPT-NKEY DUPLICATES.
      $SET NOKEYCOMPRESS

000110     SELECT PARAM   ASSIGN DISK
                          LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000130                    ACCESS RANDOM
000140                    ORGANIZATION RELATIVE
000150                    RELATIVE WS-PARKEY.

      $SET KEYCOMPRESS "7"
000230	   SELECT CRDMEM  ASSIGN DISK
                          LOCK MANUAL
                          WITH LOCK ON MULTIPLE RECORDS
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270			  RECORD CME-KEY.

000230     SELECT CREDIT ASSIGN DISK
                         LOCK EXCLUSIVE
000240                   STATUS WS-STATUS
000250                   ACCESS DYNAMIC
000260                   ORGANIZATION INDEXED
000270                   RECORD P-NUMBER
                         ALTERNATE RECORD P-NKEY DUPLICATES.

000180     SELECT CTRANS ASSIGN DISK
                         LOCK EXCLUSIVE
000190			 STATUS WS-STATUS
000040			 ACCESS DYNAMIC
000050			 ORGANIZATION INDEXED
000060			 RECORD CT-KEY
			 ALTERNATE RECORD CT-TRSEQ =
			   CT-CODE CT-SDTE CT-SINV DUPLICATES
			 ALTERNATE RECORD CT-PJKEY =
			   CT-PJ CT-SDTE CT-CRED CT-CODE DUPLICATES.

000210	   SELECT LEDTRF ASSIGN DISK
                         LOCK EXCLUSIVE
000220                   STATUS WS-STATUS
000230                   ACCESS DYNAMIC
000240                   ORGANIZATION INDEXED
000250                   RECORD XFR-KEY.

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

000330	   SELECT PORDER  ASSIGN DISK
                          LOCK EXCLUSIVE
000340                    STATUS WS-STATUS
000350                    ACCESS DYNAMIC
000360                    ORGANIZATION INDEXED
000390			  RECORD ORD-KEY
000400			  ALTERNATE RECORD ORD-EXT-ITEM
                                           DUPLICATES
			  ALTERNATE RECORD ORD-CKEY =
					   ORD-CRED ORD-EXT-ITEM
000440					   DUPLICATES
			  ALTERNATE RECORD ORD-DKEY =
					   ORD-CRED ORD-DATE
					   ORD-EXT-ITEM
					   DUPLICATES.

000010	   SELECT PURDEX  ASSIGN DISK
                          LOCK EXCLUSIVE
000040			  STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
000070			  RECORD PRD-KEY
			  ALTERNATE RECORD PRD-SKEY =
				    PRD-EXT-CODE PRD-SUPP DUPLICATES
			  ALTERNATE RECORD PRD-MKEY =
				    PRD-EXT-CODE PRD-DTE DUPLICATES
			  ALTERNATE RECORD PRD-DKEY =
				    PRD-DTE PRD-SUPP DUPLICATES
			  ALTERNATE RECORD PRD-CDKEY =
				    PRD-SUPP PRD-DTE DUPLICATES
			  ALTERNATE RECORD PRD-DSKEY =
				    PRD-DTE PRD-EXT-CODE DUPLICATES
			  ALTERNATE RECORD PRD-RSKEY =
				    PRD-SUPP PRD-REF
				    PRD-DTE  PRD-SEQ DUPLICATES.

000010	   SELECT SHARED  ASSIGN DISK
                          LOCK EXCLUSIVE
000040			  STATUS WS-STATUS
000050                    ACCESS RANDOM
000060                    ORGANIZATION RELATIVE
000070			  RELATIVE WS-SHARED.

000230	   SELECT TXTRAN  ASSIGN DISK
                          LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270                    RECORD TAX-KEY.
      $SET NOKEYCOMPRESS

000240 COPY "RECOVER.SL".

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY "APACFIDS.FDE".

000290 COPY "CONTROL.IFD".

000290 COPY "DEPART.GFD".

000290 COPY "PARAM.IFD".

000140 COPY "CRDMEM.FD".

000140 COPY "CREDIT.GFD".

000160 COPY "CRTRAN.FD".

       COPY "LEDTRF.FD".

       COPY "SHARED.FD".

000200 COPY "STOCK.GFD".

000220 COPY "PORDER.GFD".

000300 COPY "PURDEX.FD".

000410 COPY "RECOVER.GFD".

       COPY "TXTRAN.IFD".

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC  X(18)     VALUE "aeWlimemnomLalismJ".
000440 77  WS-S1	   PIC	9(04)	 COMP-5.
000440 77  WS-S2	   PIC	9(04)	 COMP-5.
001890 77  WS-S3	   PIC	9(04)	 COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
000440 77  WS-SUB	   PIC	9(04)	 COMP-5.
003290 77  WS-NETKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003290 77  WS-STRKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003340 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003360 77  WS-RECKEY	   PIC	9(04)	 COMP-5.
000520 77  WS-RECOVER	   PIC	9(05)	 COMP-0.
000520 77  WS-SHARED	   PIC	9(04)	 COMP-5.
000530 77  WS-TRANS	   PIC	9(05)	 COMP-0 VALUE 1.
000540 77  WS-EKEY	   PIC	Z(04)9.
000560 77  WS-ERROR	   PIC	9(01)	 VALUE ZERO.
000570 77  WS-PROG	   PIC	X(02).
       77  WS-OPTION	   PIC	X(01).
       77  TODAY-DDMMYY	   PIC	9(08)	 COMP-5.
       77  WS-USUB	   PIC	9(04)	 COMP-5.

002420 01  WS-DB-LINE.
003080	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003200	   03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

000590 COPY "WS.WS".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

004000 01  W02-FID.

       COPY "APACFIDS.ID".

000290 COPY "CONTROL.ID".

000290 COPY "DEPART.ID".

000140 COPY "CRDMEM.ID".

000140 COPY "CREDIT.ID".

000160 COPY "CRTRAN.ID".

       COPY "LEDTRF.ID".

       COPY "PARAM.ID".

000200 COPY "STOCK.ID".

000220 COPY "PORDER.ID".

       COPY "PURDEX.ID".

000410 COPY "RECOVER.ID".

000370 COPY "SHARED.ID".

       COPY "TXTRAN.ID".

       COPY "W12.WS".

004520 01  W25-VALUES.
004530	   03  W25-CR	       PIC S9(09)V99 COMP-3.
004530	   03  W25-DT	       PIC S9(09)V99 COMP-3.
004540	   03  W25-V1	       PIC S9(09)V99 COMP-3.
004550	   03  W25-V2	       PIC S9(09)V99 COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       LINKAGE SECTION.

       COPY "CHAIN.LS".

      /
000850 SCREEN SECTION.

       COPY "BLANK.CRT".

000860 01  S00.
000910     03  LINE  2 COLUMN 29 FOREGROUND-COLOR 3
000920                 VALUE "Creditor Recovery Program".
000930     03  LINE  4 COLUMN  2 FOREGROUND-COLOR 3
000940                 VALUE "Checking Recovery file".
000950     03  LINE  6 COLUMN 12 FOREGROUND-COLOR 3
000960                 VALUE "RECOVER :".

000860 01  S00V.
000910	   03  LINE  2 COLUMN 24 FOREGROUND-COLOR 15
000920		       VALUE "VERIFY CREDITOR ACCOUNTS AND LINKS".
000930	   03  LINE  4 COLUMN  2 VALUE "Account :".
	   03  LINE  6 COLUMN  2 VALUE "Transaction :".

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
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	     MOVE LS-SYS-ID	 TO WS-SYS-ID.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
001250	     DISPLAY CLR-SCREEN.
001270	     PERFORM ZA000.
	   IF WS-ERROR = 9
	       STOP RUN.
001260       DISPLAY S00.
      *    *************************************************************
      *    ****    R E C O V E R Y   F I L E
      *    *************************************************************
001320	     OPEN INPUT RECOVER.
001330	     MOVE 1		 TO WS-RECKEY.
001340 AA05.
001350       READ RECOVER.
001360     IF WS-STATUS NOT = "00"
001370         GO TO AA10.
001380       DISPLAY S01.
      *    *************************************************************
      *    ****    E N D   O F	 T R A N S A C T I O N	 ?
      *    *************************************************************
001390	   IF REC-FILE = 99
001400	       MOVE ZERO	 TO WS-TRANS
001410     ELSE
      *    *************************************************************
      *    ****    S T O R E   T R A N S A C T I O N   N U M B E R
      *    *************************************************************
001420	       MOVE REC-TRAN	 TO WS-TRANS.
001430	     ADD 1		 TO WS-RECKEY.
001440       GO TO AA05.
001450 AA10.
      *    *************************************************************
      *    ****    A N Y T H I N G   T O   R E C O V E R   ?
      *    *************************************************************
001460	   IF WS-TRANS = ZERO
001470         GO TO AA20.
      *    *************************************************************
      *    ****    R E C O V E R Y   R E Q U I R E D
      *    *************************************************************
001480	     DISPLAY S02.

001490 AA15.
      *    *************************************************************
      *    ****    R E A D   R E C O V E R Y   F I L E
      * 	   F R O M   T H E   L A S T   R E C O R D
      * 	   A N D   R E W R I T E   R E C O R D S   I N
      * 	   R E V E R S E   S E Q U E N C E
      *    *************************************************************
001500	     SUBTRACT 1 	 FROM WS-RECKEY.
001510     IF WS-RECKEY = ZERO
001520         GO TO AA20.
001530       READ RECOVER.
001540     IF WS-STATUS NOT = "00"
001550         GO TO AA50.
      *    *************************************************************
      *    ****    R E C O V E R Y   C O M P L E T E   ?
      * 	   ( T R A N S A C T I O N   N U M B E R
      * 	     C H A N G E D )
      *    *************************************************************
001560	   IF REC-TRAN NOT = WS-TRANS
001570         GO TO AA20.
001580       DISPLAY S03.
      *    *************************************************************
      *    ****    D E T E R M I N E   W H I C H   F I L E
      *    *************************************************************
001590	   IF REC-FILE = 01
001600	       PERFORM AY01 THRU AY999
001610     ELSE
001620     IF REC-FILE = 05
001630	       PERFORM AY05 THRU AY999
001610     ELSE
001620	   IF REC-FILE = 06
001630	       PERFORM AY06 THRU AY999
001640     ELSE
001650     IF REC-FILE = 07
001660         PERFORM AY07 THRU AY999
001670     ELSE
001680     IF REC-FILE = 08
001690         PERFORM AY08 THRU AY999
001700     ELSE
001710     IF REC-FILE = 10
001720         PERFORM AY10 THRU AY999
001730	   ELSE
001740     IF REC-FILE = 19
001750         PERFORM AY19 THRU AY999
001730     ELSE
	   IF REC-FILE = 37
	       PERFORM AY37 THRU AY999
	   ELSE
001740	   IF REC-FILE = 38
001750         PERFORM AY38 THRU AY999
001730     ELSE
001740     IF REC-FILE = 39
001750	       PERFORM AY39 THRU AY999
001730     ELSE
001740     IF REC-FILE = 40
001750	       PERFORM AY40 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 56
001750	       PERFORM AY056 THRU AY999.
001760       GO TO AA15.
      *    *************************************************************
      *    ****    R E S E T   R E C O V E R Y	 S T A T U S   F O R
      * 	   T H I S   W O R K   S T A T I O N
      *    *************************************************************
001770 AA20.
001780	     MOVE 4		 TO WS-PARKEY.
001790       READ PARAM.
001800	     SUBTRACT 1 	 FROM PAR-USERS.
001810	     MOVE SPACES	 TO PAR-USR(WS-SUB) PAR-PROG(WS-SUB).
001820       REWRITE PAR-RECORD1.
	   IF LS0-STP = 2
001900         CLOSE STOCK.
001830       CLOSE PARAM
                   NETWORK
                   DEPART
001860             TXTRAN
		   CRDMEM
                   CREDIT
                   CTRANS
                   LEDTRF
                   PORDER
		   PURDEX
001870		   RECOVER
		   SHARED.
001910       DISPLAY S04.
001920       STOP RUN.

001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.

      /
      *    THIS ROUTINE REWRITES THE BEFORE IMAGES OF THE FILES.
      *
001960 AY000           SECTION.
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E
      *    *************************************************************
001970 AY01.
001980	     MOVE REC-KEY	 TO WS-PARKEY.
001990	     MOVE REC-PARAM	 TO PAR-RECORD1.
002000       REWRITE PAR-RECORD1.
002010     IF WS-STATUS NOT = "00"
002020         PERFORM AA50.
002030       GO TO AY999.
      *    *************************************************************
      *    ****    S T O C K   F I L E
      *    *************************************************************
002280 AY05.
002300	     MOVE REC-STOCKF	 TO STK-RECORD1.
           IF REC-TYPE = 0
002310         REWRITE STK-RECORD1
           ELSE
           IF REC-TYPE = 1
               DELETE STOCK RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE STK-RECORD1.
002350	     GO TO AY999.
      *    *************************************************************
      *    ****    C R E D I T O R   M E M O   F I L E
      *    *************************************************************
011840 AY06.
011870	     MOVE REC-CRDMEM	 TO CME-RECORD1.
           IF REC-TYPE = 0
002310	       REWRITE CME-RECORD1
           ELSE
           IF REC-TYPE = 1
	       DELETE CRDMEM RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE CME-RECORD1.
011880	     GO TO AY999.
      *    *************************************************************
      *    ****    C R E D I T O R   A C C O U N T   F I L E
      *    *************************************************************
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
011880	     GO TO AY999.
      *    *************************************************************
      *    ****    C R E D I T O R   T R A N S A C T I O N   F I L E
      *    *************************************************************
011890 AY08.
011920	     MOVE REC-CRTRAN	 TO CT-REC.
           IF REC-TYPE = 0
002310	       REWRITE CT-REC
           ELSE
           IF REC-TYPE = 1
	       DELETE CTRANS RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE CT-REC.
011930       GO TO AY999.
      *    *************************************************************
      *    ****    P U R C H A S E   O R D E R	 F I L E
      *    *************************************************************
011940 AY10.
011970	     MOVE REC-PORDER	 TO ORD-RECORD1.
           IF REC-TYPE = 0
002310         REWRITE ORD-RECORD1
           ELSE
           IF REC-TYPE = 1
               DELETE PORDER RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE ORD-RECORD1.
011980	     GO TO AY999.
      *    *************************************************************
      *    ****   V A T   F I L E
      *    *************************************************************
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
002350	     GO TO AY999.
      *    *************************************************************
      *    ****    S H A R E D	 F I L E
      *    *************************************************************
       AY37.
002370	     MOVE REC-KEY	 TO WS-SHARED.
002380	     MOVE REC-NETWORK	 TO SHR-RECORD.
002390	     REWRITE SHR-RECORD.
002400     IF WS-STATUS NOT = "00"
002420         PERFORM AA50.
002430       GO TO AY999.
      *    *************************************************************
      *    ****   D E P A R T M E N T	/   S A L E S	L E D G E R
      * 	  F I L E
      *    *************************************************************
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
002430       GO TO AY999.
      *    *************************************************************
      *    ****    C O N T R O L   F I L E
      *    *************************************************************
       AY39.
002370	     MOVE REC-KEY	 TO WS-NETKEY.
002380	     MOVE REC-NETWORK	 TO NET-RECORD.
002390       REWRITE NET-RECORD.
002400     IF WS-STATUS NOT = "00"
002420         PERFORM AA50.
002430       GO TO AY999.
      *    *************************************************************
      *    ****   I N T E G R A T I O N   F I L E   ( G / L )
      *    *************************************************************
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
002350	     GO TO AY999.
      *
      *    ****   C A R D E X	F I L E   ( P U R C H A S E S )
      *
011070 AY056.
011100	     MOVE REC-PURDEX	 TO PRD-REC1.
	   IF REC-TYPE = 0
002310	       REWRITE PRD-REC1
           ELSE
           IF REC-TYPE = 1
	       DELETE PURDEX RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE PRD-REC1.
002350	     GO TO AY999.

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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E
      *    *************************************************************
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
      *    ***	  C R E D I T O R   M E M O   ( A / P )   F I L E    ***
      *    *************************************************************
	     MOVE "CRDMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRDMEM.
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
      *    *************************************************************
      *    ( A / P )   C R E D I T O R	 T R A N S A C T I O N	 F I L E
      *    *************************************************************
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
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
      *    ****    P U R C H A S E S   -   C A R D E X	 F I L E    ****
      *    *************************************************************
	     MOVE "PURDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PURDEX.
      *    *************************************************************
      *    ****        P U R C H A S E	 O R D E R   F I L E	    ****
      *    *************************************************************
	     MOVE "PORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PORDER.
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
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
      *    *************************************************************
      *    ****      V A T   T R A N S A C T I O N S   F I L E	    ****
      *    *************************************************************
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
002480	     OPEN I-O PARAM.
042120     IF WS-STAT1 = "9"
	       MOVE "PARAM"	 TO WS-FILE
042130	       GO TO ZA200.
002490     IF WS-STATUS NOT = "00"
002500	       MOVE 15		 TO WS-F-ERROR
002510         PERFORM OPEN-ERROR.
002520	     MOVE 1		 TO WS-PARKEY.
             READ PARAM.
002540     IF WS-STATUS NOT = "00"
002550	       MOVE 15		 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
	     MOVE PAR-DMY	 TO W12-TODAY.
002520	     MOVE 3		 TO WS-PARKEY.
002530       READ PARAM.
002540     IF WS-STATUS NOT = "00"
002550	       MOVE 15		 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
042140	     OPEN I-O NETWORK.
042150	   IF WS-STAT1 = "9"
	       MOVE "CONTROL"	 TO WS-FILE
042160         GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 2		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
002670	     OPEN I-O SHARED
042120     IF WS-STAT1 = "9"
	       MOVE "SHARED"	 TO WS-FILE
042130         GO TO ZA200.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 33		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
042140	     OPEN I-O DEPART.
042150     IF WS-STAT1 = "9"
	       MOVE "DEPART"	 TO WS-FILE
042160         GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 7		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *    *************************************************************
      *    ****        P U R C H A S E	 O R D E R   F I L E	    ****
      *    *************************************************************
       ZA01.
042170       OPEN I-O PORDER.
042180     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE "PORDER" TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\PURREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\PURREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA01.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 16		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *    *************************************************************
      *    ( A / P )   C R E D I T O R	 T R A N S A C T I O N	 F I L E
      *    *************************************************************
042210	     OPEN I-O CTRANS.
042220     IF WS-STAT1 = "9"
	       MOVE "CTRANS"	 TO WS-FILE
042230         GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 4		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
       ZA01A.
042240	     OPEN I-O CRDMEM.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE "CR-MEMO"
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\CRMREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\CRMREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA01A.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 6		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
       ZA02.
042240       OPEN I-O CREDIT.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE "CR-ACNT"
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\CRDREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\CRDREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA02.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 3		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *    *************************************************************
      *    ****      V A T   T R A N S A C T I O N S   F I L E	    ****
      *    *************************************************************
       ZA03A.
002670	     OPEN I-O TXTRAN.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE "VAT-FILE"
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\TAXREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\TAXREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA03A.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 32		 TO WS-ERROR
002700         PERFORM OPEN-ERROR.
	   IF LS0-STP NOT = 2
               GO TO ZA04A.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
       ZA04.
003040       OPEN I-O STOCK.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE "STOCK"  TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\STKREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\STKREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA04.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 22		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *    *************************************************************
      *    ****    P U R C H A S E S   -   C A R D E X	 F I L E    ****
      *    *************************************************************
       ZA04-A.
003040	     OPEN I-O PURDEX.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE "CARDEX" TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\PRDREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\PRDREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA04-A.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 56		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *    *************************************************************
      *    **** 	  I N T E G R A T I O N	  F I L E	    ****
      *    *************************************************************
       ZA04A.
002670       OPEN I-O LEDTRF.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE "LEDTRF" TO WS-FILE
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

       COPY "LOCKED.PRO".

002950	     MOVE 9		 TO WS-ERROR.

003080 ZA999-EXIT.
003090       EXIT.
      /
003100 ZB000-ERROR           SECTION.

       COPY "ERRORS.PRO".

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 3
041660	       MOVE W02-CREDIT	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE P-NUMBER	 TO WS-KEYX
041680     ELSE
041690     IF WS-F-ERROR = 4
041700	       MOVE W02-CRTRAN	 TO WS-FILE
041710	       MOVE ZERO	 TO WS-KEY
	       MOVE CT-KEY	 TO WS-KEYX
041680     ELSE
041690	   IF WS-F-ERROR = 6
041700	       MOVE W02-CRDMEM	 TO WS-FILE
041710	       MOVE ZERO	 TO WS-KEY
	       MOVE CME-KEY	 TO WS-KEYX
041800	   ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 16
041860	       MOVE W02-PORDER	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
               MOVE ORD-ORD      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 22
041860	       MOVE W02-STOCKF	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 32
041860	       MOVE W02-TXTRAN	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 40
041860	       MOVE W02-LEDTRF	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 56
041860	       MOVE W02-PURDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE PRD-KEY	 TO WS-KEYX.

       COPY "DISPERR.PRO".
