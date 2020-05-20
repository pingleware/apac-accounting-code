      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	*******   **	**  *******    *
      *   **	**  **	  **  **    **	**    **  **	**  **	  **   *
      *   **	    **	  **  **    **	**    **  **	**  **	  **   *
      *   **	    ******    *******	*******   **	**  ******     *
      *   **	    **	 **   **	**	  **	**  **	 **    *
      *   **	**  **	  **  **	**	  **	**  **	  **   *
      *    ******   **	  **  **	**	   ******   **	  **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   U P D A T E   C A R D E X 	       *
      *                                                                *
      *     VERSION 8.00 - August 2000				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       CRPPUR.
000030 AUTHOR.		       J.W.LEMMON (CSS).
000040 DATE-WRITTEN.	       JUNE, 1983.
000050 SECURITY.           PROGRAM MATERIAL IS THE SOLE PROPERTY OF 
000060			   JWL SOFTWARE.
000070 ENVIRONMENT DIVISION.
000080 CONFIGURATION SECTION.
000090 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000110 INPUT-OUTPUT SECTION.
000120 FILE-CONTROL.

       COPY APACFIDS.SL.

000140 COPY PARAM.SL.

000300 COPY PURDEX.SL.

000240 COPY RECOVER.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000290 COPY PARAM.FDE.

000300 COPY PURDEX.FDE.

000390 COPY RECOVER.GFD.

      /
003280 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001800 77  WS-IXS1	   PIC	9(04)	 COMP-5.
003340 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
003360 77  WS-RECKEY       PIC  9(04)    COMP-5.
003410 77  WS-TRANS	   PIC	9(04)	 COMP-5 VALUE 1.
003420 77  WS-ERROR        PIC  9(01)    VALUE 0.
003470 77  WS-S            PIC  9(04)    COMP-5.
003480 77  WS-S1           PIC  9(04)    COMP-5.
003490 77  WS-S2           PIC  9(04)    COMP-5.
003500 77  WS-S3           PIC  9(04)    COMP-5.
003510 77  WS-S4           PIC  9(04)    COMP-5.
003510 77  WS-S9           PIC  9(04)    COMP-5.
003530 77  WS-OPEN	   PIC	X(01) VALUE "N".
003540 77  WS-OPTION       PIC  X(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430	   03  WS-TOP-LNE2.
002440	       05  WS-TCH  PIC	X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	      03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE2.
002480	       05  WS-BCH  PIC	X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
	       05  FILLER  PIC	X(01) VALUE "³".
	       05  WS-SPC  PIC	X(60) VALUE SPACES.
	       05  FILLER  PIC	X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".
002450	   03  WS-MD-LNE.
002460	       05  WS-MCR  PIC X(80) VALUE "³      ³
      - 	 "                     ³               ³             ³".
	   03  WS-MDCR-LNE REDEFINES WS-MD-LNE.
	       05  WS-MCR1     PIC  X(01).
	       05  WS-MCR78    PIC  X(78).
	       05  WS-MCR80    PIC  X(01).

003980 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

004000 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

       COPY PURDEX.ID.

000280 COPY RECOVER.ID.

       COPY W05.WS.

       01  W10-STOCK.
002850	   03  W10-SELL        PIC S9(07)V999  COMP-3.
002860	   03  W10-CASH        PIC S9(07)V999  COMP-3.
002860	   03  W10-WSALE       PIC S9(07)V999  COMP-3.
002850	   03  W10-VSELL       PIC S9(09)V99   COMP-3.
002860	   03  W10-VCASH       PIC S9(09)V99   COMP-3.
002860	   03  W10-VWSALE      PIC S9(09)V99   COMP-3.
	   03  W10-CHECK       PIC S9(09)V99   COMP-3.
	   03  W10-STCK        PIC  X(01).
	   03  W10-SLNGTH      PIC  X(01)      COMP-X.
004470 01  W15-INPUT-AREA.
004480     03  W15-DATE        PIC 9(06).
004490     03  W15-DATE1       REDEFINES W15-DATE.
004500         05  W15-DD      PIC 9(02).
004510         05  W15-MM      PIC 9(02).
004520         05  W15-YY      PIC 9(02).
004530     03  W15-ACNO        PIC X(05).
004540     03  W15-SNO         REDEFINES W15-ACNO.
004550         05  W15-APREF   PIC X(01).
004560         05  W15-SERIAL  PIC X(04).
004570     03  W10-ACODE       PIC X(14).
004570     03  W10-CODE2       PIC X(14).
           03  W10-ITEM.
004570         05  W15-ITEM    PIC X(14).
	   03  W10-ITEM14 REDEFINES W10-ITEM.
	       05  W10-I14    PIC  9(14).
	   03  W10-ITEM13 REDEFINES W10-ITEM.
	       05  W10-I13    PIC  9(13).
	       05  FILLER     PIC  X(01).
	   03  W10-ITEM12 REDEFINES W10-ITEM.
	       05  W10-I12    PIC  9(12).
	       05  FILLER     PIC  X(02).
	   03  W10-ITEM11 REDEFINES W10-ITEM.
	       05  W10-I11    PIC  9(11).
	       05  FILLER     PIC  X(03).
	   03  W10-ITEM10 REDEFINES W10-ITEM.
	       05  W10-I10    PIC  9(10).
	       05  FILLER     PIC  X(04).
	   03  W10-ITEM9 REDEFINES W10-ITEM.
	       05  W10-I9     PIC  9(09).
	       05  FILLER     PIC  X(05).
	   03  W10-ITEM8 REDEFINES W10-ITEM.
	       05  W10-I8     PIC  9(08).
	       05  FILLER     PIC  X(06).
	   03  W10-ITEM7 REDEFINES W10-ITEM.
	       05  W10-I7     PIC  9(07).
	       05  FILLER     PIC  X(07).
	   03  W10-ITEM6 REDEFINES W10-ITEM.
	       05  W10-I6     PIC  9(06).
	       05  FILLER     PIC  X(08).
	   03  W10-ITEM5 REDEFINES W10-ITEM.
	       05  W10-I5     PIC  9(05).
	       05  FILLER     PIC  X(09).
	   03  W10-ITEM4 REDEFINES W10-ITEM.
	       05  W10-I4     PIC  9(04).
	       05  FILLER     PIC  X(10).
	   03  W10-ITEM3 REDEFINES W10-ITEM.
	       05  W10-I3     PIC  9(03).
	       05  FILLER     PIC  X(11).
	   03  W10-CODE1.
002570         05  W15-CODE1   PIC X(14).
002580     03  W15-COD1 REDEFINES W10-CODE1.
002590         05  W15-CD1     PIC X(02).
002600         05  W15-CDE1    PIC X(12).
004580     03  W15-NAME.
001490         05  W15-NCHAR   PIC X(01) OCCURS 40.
001500     03  W15-N           REDEFINES W15-NAME.
001510         05  W15-NIX.
001520             07  W15-ENTRY PIC X(08).
001530             07  FILLER  PIC X(16).
001540         05  FILLER      PIC X(16).
004590     03  W15-UN          PIC 9(05)V99.
004600	   03  W15-UNIT REDEFINES W15-UN.
004610         05  FILLER      PIC 9(01).
004620         05  W15-UNT     PIC 9(04).
               05  W15-UDEC    PIC 9(02).
004630     03  W15-ORD         PIC 9(10).
004640     03  W15-ONO         REDEFINES W15-ORD.
004650         05  W15-OPREF   PIC 9(08).
004660         05  W15-OSUB    PIC 9(02).
	   03  W10-DESC.
             04  W10-SDSC.
004670         05  W15-DESCR.
004680           07  W15-DESC  PIC X(18).
004690           07  FILLER    PIC X(12).

       COPY W40.WS.

       COPY FUNCTION.WS.

005450 COPY W50.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

       COPY W30.WS.

      /
005700 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION USING LS-PARID
				W30-PURDEX.
004910 AA000-START-UP  SECTION.
004920 AA00.
	   IF W30-STATUS = "O"
	       PERFORM
	   ELSE
	   IF W30-STATUS = "C"
	       CLOSE PURDEX
	   ELSE
	       PERFORM CA000-UPDATE.

       AA05.
	     EXIT PROGRAM.

       BA000-INITIALIZE SECTION 50.
       BA00.
	     MOVE LS-EXT15	 TO W02-PTH56.
	     OPEN I-O PURDEX.
	   IF WS-STATUS = "05"
	       CLOSE PURDEX
	       OPEN OUTPUT PURDEX
	       CLOSE PURDEX
	       OPEN I-O PURDEX.






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
