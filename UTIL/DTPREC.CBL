      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******   *******   *******   ******     *
      *   **    **     **     **    **  **    **  **       **    **    *
      *   **    **     **     **    **  **   **   **       **          *
      *   **    **     **     *******   ******    *****    **          *
      *   **    **     **     **        **   **   **       **          *
      *   **    **     **     **        **    **  **       **    **    *
      *   *******      **     **        **    **  *******   ******     *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *       D E B T O R   R E C O V E R Y   P R O G R A M            *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPREC.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   DECEMBER 1986.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1986 - 2011
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

      $SET KEYCOMPRESS "7"
000010	   SELECT CARDEX  ASSIGN DISK
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

	   SELECT CHEQUE  ASSIGN DISK
000270                    LOCK EXCLUSIVE
000290                    STATUS WS-STATUS
000300                    ACCESS DYNAMIC
000310                    ORGANIZATION INDEXED
                          RECORD CHQ-TR-NUMBER
000420                    ALTERNATE RECORD CHQ-DKEY =
                            CHQ-YMD CHQ-TR-NUMBER.
      $SET NOKEYCOMPRESS

000110     SELECT DEPART  ASSIGN DISK
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
000170	   SELECT DEBALT  ASSIGN DISK
			  LOCK EXCLUSIVE
000180                    STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD DAL-KEY
			  ALTERNATE RECORD DAL-NKEY DUPLICATES.

000170     SELECT DEBTOR  ASSIGN DISK
			  LOCK EXCLUSIVE
000180                    STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210                    RECORD DEB-ACNO
                          ALTERNATE RECORD DEB-NKEY DUPLICATES.

000010	   SELECT DBFOTO  ASSIGN DISK
			  LOCK EXCLUSIVE
000030			  STATUS WS-STATUS
000040                    ACCESS DYNAMIC
000050                    ORGANIZATION INDEXED
000060			  RECORD DBF-KEY.

000010     SELECT DBTRAN  ASSIGN DISK
			  LOCK EXCLUSIVE
000030			  STATUS WS-STATUS
000040                    ACCESS DYNAMIC
000050                    ORGANIZATION INDEXED
000060                    RECORD TRA-KEY
                          ALTERNATE RECORD TRA-TRSEQ =
                            TRA-CODE TRA-DATE DUPLICATES.

000110	   SELECT GARTEE  ASSIGN DISK
			  LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000130                    ACCESS DYNAMIC
000140                    ORGANIZATION INDEXED
                          RECORD GUA-KEY
000150			  ALTERNATE RECORD GUA-EXT-CODE
                                           DUPLICATES
000150                    ALTERNATE RECORD GUA-NKEY
                                           DUPLICATES
000400                    ALTERNATE RECORD GUA-SERIAL 
000440                                     DUPLICATES.
000230	   SELECT JOBCST  ASSIGN DISK
			  LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270			  RECORD JOB-KEY
			  ALTERNATE RECORD JOB-DETAILS =
				 JOB-TYPE JOB-EXT-ITEM JOB-REF
				 DUPLICATES
			  ALTERNATE RECORD JOB-CODE =
				 JOB-EXT-ITEM JOB-REF
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

000210	   SELECT LEDTRF ASSIGN DISK
                         LOCK EXCLUSIVE
000220                   STATUS WS-STATUS
000230                   ACCESS DYNAMIC
000240                   ORGANIZATION INDEXED
000250                   RECORD XFR-KEY.

000010	   SELECT SLIP	  ASSIGN DISK
000020			  LOCK EXCLUSIVE
000030                    STATUS WS-STATUS
000190			  ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD SLP-KEY
			  ALTERNATE RECORD SLP-TKEY =
			     SLP-TYPE SLP-KEY
			  ALTERNATE RECORD SLP-DKEY =
			     SLP-DRAW SLP-KEY.

000010	   SELECT SHARED  ASSIGN DISK
                          LOCK EXCLUSIVE
000040			  STATUS WS-STATUS
000050                    ACCESS RANDOM
000060                    ORGANIZATION RELATIVE
000070			  RELATIVE WS-SHARED.

000230	   SELECT SORDER  ASSIGN DISK
                          LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270                    RECORD SOR-KEY
                          ALTERNATE RECORD SOR-AKEY = 
                            SOR-AC SOR-KEY DUPLICATES.

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
      $SET NOKEYCOMPRESS

000370     SELECT AUDIT   ASSIGN DISK
                          LOCK EXCLUSIVE
000380                    STATUS WS-STATUS
000390                    ACCESS RANDOM
000400                    ORGANIZATION RELATIVE
000410                    RELATIVE WS-AUDKEY.

      $SET KEYCOMPRESS "7"
000230     SELECT INVOICE ASSIGN DISK
                          LOCK EXCLUSIVE
000040			  STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
000070			  RECORD DOC-RMKEY =
			    DOC-AC DOC-SEVEN DOC-ONE DOC-SEQN
                          ALTERNATE RECORD DOC-AKEY = 
			    DOC-AC DOC-TYPE DOC-SEVEN DOC-ONE DUPLICATES
			  ALTERNATE RECORD DOC-DKEY =
			    DOC-DTE DOC-SEVEN DOC-ONE DOC-SQN DUPLICATES
			  ALTERNATE RECORD DOC-RKEY =
			    DOC-SEVEN DOC-ONE DOC-SQN DUPLICATES.

000230	   SELECT TXTRAN  ASSIGN DISK
                          LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270                    RECORD TAX-KEY.

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

000390 COPY AUDIT.IFD.

       COPY CARDEX.FD.

       COPY CONTROL.IFD.

       COPY CHEQUE.FD.

       COPY DBFOTO.FD.

000330 COPY DBTRAN.GFD.

000310 COPY DEBALT.GFD.

000310 COPY DEBTOR.GFD.

       COPY DEPART.GFD.

       COPY GARTEE.FD.

       COPY INVOIC.FD.

       COPY JOBCST.FD.

       COPY JOBHDR.FD.

       COPY LEDTRF.FD.

000290 COPY PARAM.IFD.

       COPY SHARED.FD.

       COPY SLIP.FD.

       COPY SORDER.FD.

000370 COPY STOCK.GFD.

000410 COPY RECOVER.GFD.

       COPY TXTRAN.IFD.

       COPY WSTOCK.FD.

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000440 77  WS-S1	    PIC  9(04)	  COMP-5.
001890 77  WS-S2	   PIC	9(04)	 COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
000440 77  WS-SUB	    PIC  9(04)	  COMP-5.
000450 77  WS-PARKEY	    PIC  9(04)	  COMP-5.
000450 77  WS-NETKEY	    PIC  9(04)	  COMP-5.
000490 77  WS-AUDKEY	    PIC  9(06)	  COMP-5.
000510 77  WS-RECKEY	    PIC  9(04)	  COMP-5.
000520 77  WS-RECOVER	    PIC  9(04)	  COMP-5.
000520 77  WS-SHARED	    PIC  9(04)	  COMP-5.
       77  WS-NXTKEY        PIC  9(04)    COMP-5.
       77  WS-INVOIC        PIC  9(04)    COMP-5.
000530 77  WS-TRANS	    PIC  9(04)	   COMP-5 VALUE 1.
000540 77  WS-EKEY	    PIC  Z(04)9.
000560 77  WS-ERROR	    PIC  9(01)	   VALUE ZERO.
000570 77  WS-PROG	    PIC  X(02).
       77  WS-OPTION	    PIC  X(01).
       77  WS-LINK	    PIC  9(01)	   VALUE ZERO.
004360 77  WS-INDS	    PIC  9(01)	   VALUE ZERO.
004360 77  WS-INDD	    PIC  9(01)	   VALUE ZERO.
       77  TODAY-DDMMYY	    PIC	 9(08) COMP-5.
       77  WS-USUB	    PIC	 9(04) COMP-5.

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

       COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

000610 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

       COPY CONTROL.ID.

       COPY CHEQUE.ID.

000310 COPY DEPART.ID.

000310 COPY DEBALT.ID.

000310 COPY DEBTOR.ID.

000330 COPY DBFOTO.ID.

000330 COPY DBTRAN.ID.

       COPY GARTEE.ID.

       COPY JOBCST.ID.

       COPY JOBHDR.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

000370 COPY SHARED.ID.

       COPY SLIP.ID.

       COPY SORDER.ID.

000370 COPY STOCK.ID.

000390 COPY AUDIT.ID.

       COPY INVOIC.ID.

000410 COPY RECOVER.ID.

       COPY TXTRAN.ID.

       COPY WSTOCK.ID.

       COPY W12.WS.

       COPY W40.WS.

       COPY FUNCTION.WS.

       COPY W70.WS.

       COPY W70DEBT.IWS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
000850 SCREEN SECTION.

       COPY BLANK.CRT.

000860 01  S00.
000910	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
000920                 VALUE "DEBTOR RECOVERY PROGRAM".
000930     03  LINE  4 COLUMN  2 FOREGROUND-COLOR 3
000940                 VALUE "Checking Recovery file".
000950     03  LINE  6 COLUMN 12 FOREGROUND-COLOR 3
000960                 VALUE "RECOVER :".

000970 01  S01.
000980	   03  LINE  6 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
000990                 PIC Z(04)9 USING WS-RECKEY.

001000 01  S02.
001010     03  LINE  8 COLUMN 12 FOREGROUND-COLOR 3
001020                 VALUE "Busy recovering files".
001030     03  LINE 10 COLUMN 12 FOREGROUND-COLOR 3
001040                 VALUE "RECOVER :".

001050 01  S03.
001060	   03  LINE 10 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
001070                 PIC Z(04)9 USING WS-RECKEY.

001080 01  S04.
001090	   03  LINE 19 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
001100                 VALUE "Recovery complete".
001110	   03  LINE 21 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
001120                 VALUE "You may now restart the system".

001130 01  S05.
001140	   03  LINE 12 COLUMN 08 FOREGROUND-COLOR 3 HIGHLIGHT
001150                 VALUE "Recovery unsuccessfull !".
001160	   03  LINE 14 COLUMN 08 FOREGROUND-COLOR 3 HIGHLIGHT
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190	   03  LINE 16 COLUMN 08 FOREGROUND-COLOR 3 HIGHLIGHT
001200                 VALUE "since creating the last backup disk/s".
	   03  LINE 18 COLUMN 08 FOREGROUND-COLOR 6 HIGHLIGHT
		       PIC  X(12) FROM WS-FILE.
	   03	       COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
		       PIC  X(24) FROM WS-KEYX.
	   03	       COLUMN 50 FOREGROUND-COLOR 3 HIGHLIGHT
		       PIC  X(02) FROM WS-STATUS.

      *** PATCH FILE POINTERS - DISPLAY
005940 01  S08.
005950	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "FIX FILE POINTERS".
           03  LINE  4 COLUMN 12 VALUE "Reading Transaction File".
           03  LINE  6 COLUMN 12 VALUE "Reading Audit File".

006150 01  S09C.
006160	   03  FOREGROUND-COLOR 7 HIGHLIGHT PIC ZZZZ9 USING WS-AUDKEY.

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
	       MOVE 9		 TO WS-ERROR
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
001610     ELSE
001620     IF REC-FILE = 02
001630         PERFORM AY02 THRU AY999
001670     ELSE
001680     IF REC-FILE = 04
001690         PERFORM AY04 THRU AY999
001700     ELSE
001710     IF REC-FILE = 05
001720         PERFORM AY05 THRU AY999
001700     ELSE
001710     IF REC-FILE = 09
001720         PERFORM AY09 THRU AY999
001730     ELSE
001740     IF REC-FILE = 13
001750         PERFORM AY13 THRU AY999
001730     ELSE
001740     IF REC-FILE = 16
001750         PERFORM AY16 THRU AY999
001730     ELSE
001740     IF REC-FILE = 17
001750         PERFORM AY17 THRU AY999
001730	   ELSE
001740     IF REC-FILE = 19
001750         PERFORM AY19 THRU AY999
001730	   ELSE
001740	   IF REC-FILE = 21
001750	       PERFORM AY21 THRU AY999
001730	   ELSE
001740     IF REC-FILE = 22
001750         PERFORM AY22 THRU AY999
001730	   ELSE
001740	   IF REC-FILE = 35
001750	       PERFORM AY35 THRU AY999
001730	   ELSE
001740     IF REC-FILE = 36
001750         PERFORM AY36 THRU AY999
001730	   ELSE
	   IF REC-FILE = 37
	       PERFORM AY37 THRU AY999
	   ELSE
001740     IF REC-FILE = 38
001750         PERFORM AY38 THRU AY999
001730     ELSE
001740     IF REC-FILE = 39
001750	       PERFORM AY39 THRU AY999
001730     ELSE
001740     IF REC-FILE = 40
001750	       PERFORM AY40 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 42
001750	       PERFORM AY42 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 43
001750	       PERFORM AY43 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 47
001750	       PERFORM AY47 THRU AY999
001730     ELSE
001740	   IF REC-FILE = 52
001750	       PERFORM AY052 THRU AY999.
001760       GO TO AA15.
      *
      *    ****   R E C O V E R Y   I S   C O M P L E T E
      *
001770 AA20.
           IF WS-LINK = 1
               PERFORM HA000.
      *
      *    ****    R E S E T   R E C O V E R Y	 S T A T U S   F O R
      * 	   T H I S   W O R K   S T A T I O N
      *
001780	     MOVE 4		 TO WS-PARKEY.
001790       READ PARAM.
001800	     SUBTRACT 1 	 FROM PAR-USERS.
001810	     MOVE SPACES	 TO PAR-USR (WS-SUB) PAR-PROG (WS-SUB).
001820       REWRITE PAR-RECORD1.
001830       CLOSE PARAM
		   CARDEX
                   NETWORK
                   DEPART
                   INVOICE
                   CHEQUE
001840		   DEBALT
001840             DEBTOR
001850		   DBFOTO
001850             DBTRAN
                   LEDTRF
001870             RECOVER
		   SHARED
                   SORDER
		   TXTRAN
001890		   AUDIT.
           IF WS-PROG = "DS"
001900         CLOSE STOCK
		     SLIP
		     GARTEE
		     WSTOCK.
	   IF LS0-JCP = 2
               CLOSE JOBCST.
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
	       IF REC-FILE = 02
		   MOVE "Debtor" TO WS-FILE
		   MOVE DEB-ACNO TO WS-KEYX
	       ELSE
	       IF REC-FILE = 03
		   MOVE "Debtor Cont"
				 TO WS-FILE
		   MOVE DBF-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 04
		   MOVE "Debtor Tran"
				 TO WS-FILE
		   MOVE TRA-AC	 TO WS-KEYX
	       ELSE
	       IF REC-FILE = 05
		   MOVE "Stock"  TO WS-FILE
		   MOVE STK-CODE TO WS-KEYX
	       ELSE
	       IF REC-FILE = 09
		   MOVE "Guarantee"
				 TO WS-FILE
		   MOVE GUA-EXT-CODE
				 TO WS-KEYX
	       ELSE
	       IF REC-FILE = 16
		   MOVE "Audit"  TO WS-FILE
		   MOVE REC-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 17
		   MOVE "Sales Order"
				 TO WS-FILE
		   MOVE SOR-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 19
		   MOVE "VAT (Txtran)"
				 TO WS-FILE
		   MOVE TAX-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 21
		   MOVE "Job Header"
				 TO WS-FILE
		   MOVE JHD-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 22
		   MOVE "Invoice"
				 TO WS-FILE
		   MOVE DOC-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 25
		   MOVE "Job Entry"
				 TO WS-FILE
		   MOVE JOB-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 35
		   MOVE "Slip"	 TO WS-FILE
		   MOVE SLP-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 36
		   MOVE "Cheque" TO WS-FILE
		   MOVE CHQ-TR-NUMBER
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
	       IF REC-FILE = 43
		   MOVE "Cardex" TO WS-FILE
		   MOVE CRD-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 47
		   MOVE "Alt Index" TO WS-FILE
		   MOVE DAL-KEY  TO WS-KEYX
	       ELSE
	       IF REC-FILE = 52
		   MOVE "W/H Stock"
				 TO WS-FILE
		   MOVE WST-KEY  TO WS-KEYX.
001940       DISPLAY S05.
001950       STOP RUN.
      /
010300 AC000-READ              SECTION.
010670 AC51.
010680       READ AUDIT.
010690     IF WS-STATUS NOT = "00"
010700         IF WS-STATUS = "23"
010710		   MOVE 1	 TO WS-INDD
010720         ELSE
010730         DISPLAY (24, 12) "Invalid key on AUDIT - Status "
010740                          WS-STATUS.  
010750 AC55.
010760       EXIT.
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
      *    ****    D E B T O R	 A C C O U N T	 F I L E
      *
002040 AY02.
002060	     MOVE REC-DEBTOR	 TO DEB-RECORD1.
           IF REC-TYPE = 0
002070         REWRITE DEB-RECORD1
           ELSE
           IF REC-TYPE = 1
002070         DELETE DEBTOR RECORD
           ELSE
           IF REC-TYPE = 2
002070         WRITE DEB-RECORD1.
002110       GO TO AY999.
      *
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E
      *
002200 AY04.
002220	     MOVE REC-DBTRAN	 TO TRA-RECORD1.
           IF REC-TYPE = 0
002230         REWRITE TRA-RECORD1
           ELSE
           IF REC-TYPE = 1
002070         DELETE DBTRAN RECORD
           ELSE
           IF REC-TYPE = 2
002070         WRITE TRA-RECORD1.
002270       GO TO AY999.
      *
      *    ****    S T O C K   F I L E
      *
002280 AY05.
002300	     MOVE REC-STOCKF	TO STK-RECORD1.
           IF REC-TYPE = 0
002310         REWRITE STK-RECORD1
           ELSE
           IF REC-TYPE = 1
               DELETE STOCK RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE STK-RECORD1.
002350       GO TO AY999.
      *
      *    ****   G U A R A N T E E   F I L E
      *
002280 AY09.
002300	     MOVE REC-GARTEE	 TO GUA-RECORD1.
           IF REC-TYPE = 0
002310         REWRITE GUA-RECORD1
           ELSE
           IF REC-TYPE = 1
               DELETE GARTEE RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE GUA-RECORD1.
002350       GO TO AY999.
      *
      *    ****    J O B   C O S T I N G   -   L I N E	 F I L E
      *
       AY13.
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
      *    ****    A U D I T   F I L E
      *
002360 AY16.
002370	     MOVE REC-KEY	 TO WS-AUDKEY.
002380	     MOVE REC-AUDITF	 TO AUD-REC1.
           IF AUD-REC1 = LOW-VALUES
	       MOVE 1		 TO WS-LINK.
002390       REWRITE AUD-REC1.
           IF WS-STATUS = "23"
               MOVE 1            TO WS-LINK
               GO TO AY999.
002400     IF WS-STATUS NOT = "00"
002410         IF AUD-REC1 NOT = LOW-VALUES
002420             PERFORM AA50.
002320     IF WS-STATUS = "00"
               IF AUD-REC1 = LOW-VALUES
                   DELETE AUDIT RECORD.
002430       GO TO AY999.
      *
      *    ****    S A L E S   O R D E R   F I L E
      *
       AY17.
002300       MOVE REC-SORDER TO SOR-REC1.
           IF REC-TYPE = 0
002310         REWRITE SOR-REC1
           ELSE
           IF REC-TYPE = 1
               DELETE SORDER RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE SOR-REC1.
002350       GO TO AY999.
      *
      *    ****   V A T   F I L E
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
      *    ****    I N V O I C E   F I L E
      *
       AY22.
	     MOVE REC-INVOIC	 TO DOC-REC1.
           IF REC-TYPE = 0
002310         REWRITE DOC-REC1
           ELSE
           IF REC-TYPE = 1
               DELETE INVOICE RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE DOC-REC1.
002350       GO TO AY999.
      *
      *    ****    S L I P   F I L E   ( P O I N T - O F - S A L E )
      *
       AY35.
	     MOVE REC-SLIP	 TO SLP-RECORD1.
           IF REC-TYPE = 0
002310	       REWRITE SLP-RECORD1
           ELSE
           IF REC-TYPE = 1
	       DELETE SLIP RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE SLP-RECORD1.
002350       GO TO AY999.
      *
      *    ****   C H E Q U E	F I L E   ( B A N K I N G )
      *
       AY36.
             MOVE REC-CHEQUE     TO CHQ-RECORD1.
           IF REC-TYPE = 0
002310         REWRITE CHQ-RECORD1
           ELSE
           IF REC-TYPE = 1
               DELETE CHEQUE RECORD
           ELSE
           IF REC-TYPE = 2
               WRITE CHQ-RECORD1.
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
      *    ****   D E B T O R	C O N T R A C T   F I L E
      *
       AY42.
	     MOVE REC-DBFOTO	 TO DBF-RECORD1.
           IF REC-TYPE = 0
002310	       REWRITE DBF-RECORD1
           ELSE
           IF REC-TYPE = 1
	       DELETE DBFOTO RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE DBF-RECORD1.
002350       GO TO AY999.
      *
      *    ****   C A R D E X	F I L E   ( S A L E S )
      *
       AY43.
	     MOVE REC-CARDEX	 TO CRD-REC1.
           IF REC-TYPE = 0
002310	       REWRITE CRD-REC1
           ELSE
           IF REC-TYPE = 1
	       DELETE CARDEX RECORD
           ELSE
           IF REC-TYPE = 2
	       WRITE CRD-REC1.
002350       GO TO AY999.
      *
      *    ****    D E B T O R	 A L T E R N A T E   I N D E X	 F I L E
      *
002040 AY47.
002060	     MOVE REC-DEBALT	 TO DAL-RECORD1.
           IF REC-TYPE = 0
002070	       REWRITE DAL-RECORD1
           ELSE
           IF REC-TYPE = 1
002070	       DELETE DEBALT RECORD
           ELSE
           IF REC-TYPE = 2
002070	       WRITE DAL-RECORD1.
002110       GO TO AY999.
      *
      *    ****    W A R E H O U S E   S T O C K   F I L E
      *
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
      /   ***************************************************	*
      *** P A T C H   D E B T O R   F I L E   P O I N T E R S ***
      *   ***************************************************	*
013790 HA000           SECTION.
013800 HA00.
013810       DISPLAY CLR-SCREEN.
013820       DISPLAY S08.
	     MOVE 1		 TO WS-NETKEY.
             READ NETWORK.

       HA15.
	     MOVE ZERO		 TO WS-AUDKEY.
	     MOVE ZERO		 TO WS-INDD.

       HA45.
	     ADD 1		 TO WS-AUDKEY.
             PERFORM AC51 THRU AC55.
             DISPLAY S09C AT 0637.
           IF WS-INDD = 0
               GO TO HA45.
	     MOVE ZERO		 TO WS-INDD.
	     MOVE WS-AUDKEY	 TO DEB-AUDIT.
             REWRITE NET-RECORD.
	     MOVE ZERO		 TO WS-INDD.

013960 HA999.
013970       EXIT.

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
	     MOVE "AUDITF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-AUDITF.
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
	     MOVE "CHEQUE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CHEQUE.
	     MOVE "DBFOTO"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBFOTO.
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
	     MOVE "DEBALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBALT.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
	     MOVE "JOBCST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBCST.
	     MOVE "JOBHDR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBHDR.
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "PSLIPS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SLIP.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
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
      *    ****    S H A R E D	 F I L E
      *
002670	     OPEN I-O SHARED
042120     IF WS-STAT1 = "9"
	       MOVE W02-SHARED	 TO WS-FILE
042130         GO TO ZA200.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 33		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    D E P A R T M E N T	 /   S A L E S	 L E D G E R
      * 	   F I L E
      *
002670	     OPEN I-O DEPART
042120     IF WS-STAT1 = "9"
	       MOVE W02-DEPART	 TO WS-FILE
042130         GO TO ZA200.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 7		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    D E B T O R	 A C C O U N T	 F I L E
      *
       ZA01.
002670       OPEN I-O DEBTOR.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-DEBTOR
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\DEBREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\DEBREB"
	       OPEN I-O PARAM
			NETWORK
	       GO TO ZA01.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 6		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E
      *
       ZA01-A.
002710       OPEN I-O DBTRAN.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-DBTRAN
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\DBTREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\DBTREB"
	       OPEN I-O PARAM
			NETWORK
	       GO TO ZA01-A.
002720     IF WS-STATUS NOT = "00"
002730	       MOVE 5		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    C H E Q U E	 F I L E   ( B A N K I N G )
      *
       ZA01A.
002670       OPEN I-O CHEQUE.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-CHEQUE
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\CHQREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\CHQREB"
	       OPEN I-O PARAM
			NETWORK
	       GO TO ZA01A.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 36		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    D E B T O R	 A L T E R N A T E   I N D   F I L E
      *
       ZA01-NME.
002670	     OPEN I-O DEBALT.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-DEBALT
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE DEBTOR
		     PARAM
	       CALL "UTP\DALREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\DALREB"
	       OPEN I-O DEBTOR
			PARAM
			NETWORK
	       GO TO ZA01-NME.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 47		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    I N V O I C E   F I L E   ( H I S T O R Y )
      *
       ZA02.
002670       OPEN I-O INVOICE.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-INVOIC
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\INVREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\INVREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA02.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 12		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    C A R D E X	 F I L E   ( S A L E S )
      *
       ZA02A.
002670	     OPEN I-O CARDEX.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-CARDEX
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\CARREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\CARREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA02A.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 43		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****   V A T   F I L E   ( C U R R E N T   R A T E )
      *
       ZA03A.
002670	     OPEN I-O TXTRAN.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-TXTRAN
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
002690	       MOVE 32		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    I N T E G R A T I O N   F I L E   ( G / L )
      *
       ZA04A.
002670       OPEN I-O LEDTRF.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-LEDTRF
				 TO WS-FILE
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
002910	       GO TO ZA08.
002920     IF WS-SUB < 24
002930	       ADD 1		 TO WS-SUB
002940         GO TO ZA05.
002950	     MOVE 9		 TO WS-ERROR.
002960       GO TO ZA999-EXIT.
      *
      *    ****    I S	 J O B	 C O S T I N G	 U S E D
      *
       ZA08.
	   IF LS0-JCP NOT = 2
               GO TO ZA09.
      *
      *    ****    J O B   L I N E   F I L E
      *
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
	       GO TO ZA08.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 13		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    J O B   H E A D E R	 F I L E
      *
       ZA08A.
038970	     OPEN I-O JOBHDR.
042120     IF WS-STAT1 = "9"
038990         IF FLE-LOCKED
		   MOVE W02-JOBHDR
				 TO WS-FILE
039000		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM JOBCST
	       CALL "UTP\JOBREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\JOBREB"
	       OPEN I-O PARAM NETWORK JOBCST
	       GO TO ZA08A.
039080	   IF WS-STATUS NOT = "00"
039090	       MOVE 14		 TO WS-F-ERROR
039100         PERFORM OPEN-ERROR.
      *
      *    ****    S A L E S   O R D E R   F I L E
      *
       ZA09.
002670       OPEN I-O SORDER.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-SORDER
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\SALREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\SALREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA09.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 20		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    A U D I T   F I L E
      *
002970 ZA10.
003000       OPEN I-O AUDIT.
042120     IF WS-STAT1 = "9"
	       MOVE W02-AUDITF	 TO WS-FILE
042130	       GO TO ZA200.
003010     IF WS-STATUS NOT = "00"
003020	       MOVE 1		 TO WS-F-ERROR
003030         PERFORM OPEN-ERROR.
      *
      *    ****    D E B T O R	 C O N T R A C T   F I L E
      *
044300	     OPEN I-O DBFOTO.
           IF WS-STATUS = "05"
	       CLOSE DBFOTO
	       OPEN OUTPUT DBFOTO
	       CLOSE DBFOTO
	       OPEN I-O DBFOTO.
044310	   IF WS-STAT1 = "9"
	       IF FLE-LOCKED
		   MOVE W02-DBFOTO
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\DBFREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\DBFREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA09.
      *
      *    ****   W A S   S T O C K   B E I N G   U P D A T E D
      *
002980	   IF NOT (WS-PROG = "DS")
002990         GO TO ZA999-EXIT.
      *
      *    ****    G U A R A N T E E   F I L E
      *
       ZA12.
002670       OPEN I-O GARTEE.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-GARTEE
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\GARREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\GARREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA12.
002680     IF WS-STATUS NOT = "00"
002690	       MOVE 9		 TO WS-F-ERROR
002700         PERFORM OPEN-ERROR.
      *
      *    ****    S T O C K   F I L E
      *
       ZA15.
003040       OPEN I-O STOCK.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-STOCKF
				 TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\STKREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\STKREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA15.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 22		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *
      *    ****    W A R E H O U S E   -   S T O C K   F I L E
      *
       ZA16.
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
	       GO TO ZA16.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 52		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.

       ZA17.
003040	     OPEN I-O SLIP.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
		   MOVE W02-SLIP TO WS-FILE
042130		   GO TO ZA200.
042120	   IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR
	      (WS-STATUS = "39")
	       CLOSE PARAM
	       CALL "UTP\SLPREB"
		     USING LS-PARID LS-USER-ID LS0-PROGRAMS
			   LS0-SECURITY
	       CANCEL "UTP\SLPREB"
	       OPEN I-O PARAM NETWORK
	       GO TO ZA17.
003050	   IF NOT (WS-STATUS = "00" OR "05")
003060	       MOVE 38		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
002960	     GO TO ZA999-EXIT.

020110 ZA200.

       COPY LOCKED.PRO.

003080 ZA999-EXIT.
003090       EXIT.
      /
003100 ZB000-ERROR           SECTION.

       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041570     IF WS-F-ERROR = 1
041580	       MOVE W02-AUDITF	 TO WS-FILE
041590	       MOVE WS-AUDKEY	 TO WS-KEY
041600     ELSE
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041720     ELSE
041730     IF WS-F-ERROR = 5
041740	       MOVE W02-DBTRAN	 TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
041760     ELSE
041770     IF WS-F-ERROR = 6
041780	       MOVE W02-DEBTOR	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041800     ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041800	   ELSE
041810     IF WS-F-ERROR = 9
041820	       MOVE W02-GARTEE	 TO WS-FILE
041830	       MOVE ZERO	 TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 12
041860	       MOVE W02-INVOIC	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
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
041850     IF WS-F-ERROR = 22
041860	       MOVE W02-STOCKF	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 32
041860	       MOVE W02-TXTRAN	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 36
041860	       MOVE W02-CHEQUE	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
               MOVE CHQ-TR-NUMBER
				 TO WS-KEYX
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
041850	   IF WS-F-ERROR = 42
041860	       MOVE W02-DBFOTO	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE DBF-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 52
041860	       MOVE W02-WSTOCK	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.

       COPY DISPERR.PRO.
