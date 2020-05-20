      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *  ********   ******   *******	**	    ****    ********   *
      *     **	   **	 **  **    **	**	   **  **   **	   **  *
      *     **	   **	     **    **	**	  **	**  **	   **  *
      *     **	   **	     *******	**	  ********  ********   *
      *     **	   **	     ** 	**	  **	**  **	   **  *
      *  ** **	   **	 **  ** 	**	  **	**  **	   **  *
      *   ***	    ******   ** 	********  **	**  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      P R I N T	 J O B	 L A B E L S   P R O G R A M	       *
      *                                                                *
      *     VERSION 8.00 - August 2000				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JCPLAB.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1991.
000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY PARAM.SL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000290 COPY PARAM.FDE.

      /
002090 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002100 77  WS-DISC	   PIC	99V99	 COMP-3.
002110 77  WS-TDISC        PIC  99V99    COMP-3.
002120 77  WS-CASH-AC      PIC  X(06).
002130 77  WS-SUB          PIC  9(04)    COMP-5.
002140 77  WS-S1           PIC  9(04)    COMP-5.
002150 77  WS-S2           PIC  9(04)    COMP-5.
002160 77  WS-S3           PIC  9(04)    COMP-5.
002170 77  WS-S4           PIC  9(04)    COMP-5.
002180 77  WS-S5           PIC  9(04)    COMP-5.
002190 77  WS-S6           PIC  9(04)    COMP-5.
002200 77  WS-S7           PIC  9(04)    COMP-5.
002210 77  WS-S8           PIC  9(04)    COMP-5.
002220 77  WS-S9           PIC  9(04)    COMP-5.
002230 77  WS-IXS          PIC  9(04)    COMP-5.
002240 77  WS-IXS1         PIC  9(04)    COMP-5.
002250 77  WS-IXS2         PIC  9(04)    COMP-5.
002260 77  WS-IXS3         PIC  9(04)    COMP-5.
002270 77  WS-IXS4         PIC  9(04)    COMP-5.
002280 77  WS-PARKEY       PIC  9(04)    COMP-5.
002400 77  WS-PRN1	   PIC	9(01) VALUE 0.
002410 77  WS-PRN2         PIC  9(01) VALUE 0.  
002430 77  WS-START	   PIC	9(08).
002440 77  WS-END          PIC  9(08).
002460 77  WS-TME2	   PIC	9(08).
002470 77  WS-PORT         PIC  9(05) VALUE 1004.
002480 77  WS-OPEN         PIC  X(01) VALUE X"01".
002490 77  WS-CLOSE        PIC  X(01) VALUE X"00".
002500 77  WS-PAR          PIC  9(01) VALUE 0.
002510 77  WS-INO          PIC  9(02) VALUE 0.
002520 77  WS-LINES        PIC  9(02) VALUE 0.
002530 77  WS-FIRST        PIC  9(02) VALUE 0.
002540 77  WS-LAST         PIC  9(02) VALUE 0.
002550 77  WS-LASTINV      PIC  9(04)    COMP-5.
002560 77  WS-LINE         PIC  9(04)    COMP-5.
002590 77  WS-OPTION	   PIC	X(01).
002640 77  WS-DEM	   PIC	9(01).
002650 77  WS-ERROR        PIC  9(01).
002690 77  WS-COMP	   PIC	9(01).
002710 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
002720 77  WS-CONREC       PIC  9(01) VALUE ZERO.
002730 77  WS-HEAD         PIC  X(01).
002740 77  WS-TYPE         PIC  X(01).
002750 77  WS-AGE          PIC  X(01).
002760 77  WS-PRC          PIC  X(01).
002830 77  WS-BATCH	   PIC	9(05).
002860 77  WS-A 	   PIC	X(19) VALUE "Any key to continue".
002870 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
002880 77  WS-PER	   PIC	Z(02)9.99.
002890 77  WS-CR           PIC  X(01).
002900 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
001350 01  WS-CRT-LINES.
001380	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
001400     03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
001410         05  FILLER      PIC  X(01).
001420         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001440	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "³              ³   ³
      -        "                       ³        ³         ³          ³".
	   03  WS-MJOB REDEFINES WS-MID-LINE.
	       05  FILLER      PIC  X(01).
	       05  WS-JOB78    PIC  X(78).
	       05  FILLER      PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
001500	   03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
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
               05  FILLER  PIC  X(60) VALUE SPACES.
	       05  FILLER  PIC	X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".
001590	   03  WS-MID-LNE.
001600	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
001650	   03  WS-MD-LNE.
001660	       05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
001710	   03  WS-MD-LINE.
001720	       05  WS-MCA   PIC  X(80) VALUE "³        ³      ³
      -        "                       ³        ³        ³           ³".
	   03  WS-JB78-L REDEFINES WS-MD-LINE.
	       05  WS-MJB1     PIC  X(01).
	       05  WS-MJB78    PIC  X(78).
	       05  WS-MJB80    PIC  X(01).
001710	   03  WS-MD-LIN.
001720	       05  WS-MCB   PIC  X(80) VALUE "³  ³        ³            ³
      -        "                                ³         ³          ³".
	   03  WS-RQ78-L REDEFINES WS-MD-LIN.
	       05  WS-RQ1      PIC  X(01).
	       05  WS-RQ78     PIC  X(78).
	       05  WS-RQ80     PIC  X(01).

003340 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "JCP".
	   03  WS-PROG	       PIC  X(03) VALUE "PRN".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

       01  W02-PRINTER-DETAILS.
	   03  W02-PRINTER     PIC  X(12).
002570	   03  W02-PGE-LENGTH  PIC  9(02).
001870	   03  W02-PRN-LENGTH  PIC  9(02).
	   03  W02-LINAGE      PIC  9(02).
	   03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      * 	   C  =  Condensed print
      * 	   E  =  Expanded print
      * 	   H  =  Header line
      * 	   X  =  Cancel expanded print
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  W02-PRN-TYPE    PIC  X(01).
	   03  W02-PRN-LINE    PIC  X(136).

	   03  R-SL1 REDEFINES W02-PRN-LINE.
	       05  R-SLA       PIC  X(78).
	       05  FILLER      PIC  X(58).

	   03  R-SL2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(58).
	       05  R-SLB       PIC  X(78).

	   03  REP-L1 REDEFINES W02-PRN-LINE.
	       05  REP-DETAIL1	   PIC X(132).

	   03  REP-L2 REDEFINES W02-PRN-LINE.
	       05  REP-DETAIL2	   PIC X(96).

	   03  REP-L3 REDEFINES W02-PRN-LINE.
	       05  REP-AC	   PIC X(10).
	       05  REP-ACNO	   PIC X(06).
	       05  FILLER	   PIC X(01).
	       05  REP-NAME	   PIC X(40).
	       05  FILLER	   PIC X(08).
	       05  REP-DATE	   PIC X(06).
	       05  REP-TIME.
		   07  REP-DTE	   PIC Z9/99/99.

	   03  REP-L3A REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(10).
	       05  REP-TEL	   PIC X(14).
	       05  REP-CNTCT	   PIC X(10).
	       05  REP-CONTACT	   PIC X(30).
	       05  FILLER	   PIC X(32).

	   03  REP-L4 REDEFINES W02-PRN-LINE.
	       05  REP-H1	   PIC X(29).
	       05  REP-REF	   PIC 9(08).
	       05  FILLER	   PIC X(42).

	   03  REP-L5 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(02).
	       05  REP-H2	   PIC X(16).
	       05  REP-H3	   PIC X(33).
	       05  REP-H4	   PIC X(10).
	       05  REP-H5	   PIC X(15).
	       05  REP-H6	   PIC X(13).
	       05  REP-H7	   PIC X(05).
	       05  FILLER	   PIC X(02).

	   03  REP-L6 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(02).
	       05  REP-ITM1	   PIC X(14).
	       05  FILLER	   PIC X(02).
	       05  REP-DESC	   PIC X(30).
	       05  FILLER	   PIC X(02).
	       05  REP-QNT	   PIC Z(05)9.99.
	       05  FILLER	   PIC X(02).
	       05  REP-NC.
		   07  REP-PRICE   PIC Z(05)9.99.
	       05  FILLER	   PIC X(02).
	       05  REP-VAT	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(02).
	       05  REP-VAL	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(02).

	   03  REP-L7 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(11).
	       05  REP-SMAN	   PIC X(30).
	       05  FILLER	   PIC X(01).
	       05  REP-H8	   PIC X(10).
	       05  REP-TERMS	   PIC X(28).

	   03  REP-L8 REDEFINES W02-PRN-LINE.
	       05  FILLER.
		   07  REP-H9	   PIC	X(10).
		   07  FILLER	   PIC	X(01).
	       05  REP-MODEL.
		   07  REP-ORD	   PIC	X(14).
		   07  FILLER	   PIC	X(10).
	       05  FILLER	   PIC	X(01).
	       05  REP-ITMDESC	   PIC	X(42).

	   03  REP-L9 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(08).
	       05  REP-REMK	   PIC X(40).
	       05  FILLER	   PIC X(48).

	   03  LB5-L1 REDEFINES W02-PRN-LINE.
	       05  LB5-DET OCCURS 4.
		   07  LB5-CODE    PIC	X(08).
		   07  FILLER	   PIC	X(09).

	   03  LB5-L2 REDEFINES W02-PRN-LINE.
	       05  LB5-DATA2 OCCURS 4.
		   07  LB5-DESC    PIC	X(30).
		   07  FILLER	   PIC	X(03).

	   03  LB5-L3 REDEFINES W02-PRN-LINE.
	       05  LB5-DATA3 OCCURS 4.
		   07  LB5-NAME    PIC	X(30).
		   07  FILLER	   PIC	X(03).

	   03  LB5-L4 REDEFINES W02-PRN-LINE.
	       05  LB5-DATA4 OCCURS 4.
		   07  LB5-DATE    PIC	Z9/99/9999.
		   07  FILLER	   PIC	X(07).

	   03  LB5-L5 REDEFINES W02-PRN-LINE.
	       05  LB5-DATA5 OCCURS 4.
		   07  LB5-ACNO    PIC	X(06).
		   07  FILLER	   PIC	X(11).

       COPY W10.JCP.

       COPY W12.WS.

004520 01  W25-CALCS.
004730     03  W25-RESULT      PIC 9(05)V99.
004740     03  W25-RESULT1 REDEFINES W25-RESULT.
004750         05  W25-WHOLE   PIC 9(03).
004760         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
004570     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
004580     03  W25-VALUE       PIC S9(07)V99 COMP-3.
004590     03  W25-CREDIT      PIC S9(07)V99 COMP-3.
001660     03  W25-TIME        PIC 9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001690         05  W25-HH      PIC 9(02).
001690         05  W25-MM      PIC 9(02).
001690         05  W25-SS      PIC 9(02).
001680         05  FILLER      PIC 9(02).

       COPY W40.WS.

       COPY FUNCTION.WS.

005370 COPY W50.WS.

005620 COPY W70DEBT.IWS.

005690 01  W75-TITLES.
005700     03  W75-DESCRIP.
005710         05  FILLER      PIC X(28)      VALUE
005720                     "Mr     Mnr    Mrs    Mev    ".
005730         05  FILLER      PIC X(28)      VALUE
005740                     "Miss   Mej    Ms     Ms     ".
005750         05  FILLER      PIC X(28)      VALUE
005760                     "Dr     Dr     Rev    Ds     ".
005770         05  FILLER      PIC X(28)      VALUE
005780                     "Prof   Prof   The HonSy Edel".
005790         05  FILLER      PIC X(28)      VALUE
005800                     "                            ".
005810     03  W75-CODES REDEFINES W75-DESCRIP.
005820         05  W75-TITLE  OCCURS 10.
005830             07  W75-ETITLE PIC X(07).
005840             07  W75-ATITLE PIC X(07).

005860 01  W80-ACCOUNT.
005870     03  W80-ACNO        PIC X(06).
005880     03  W80-CRED REDEFINES W80-ACNO.
005890         05  W80-CNO     PIC 9(05).
005900         05  W80-CAC REDEFINES W80-CNO.
005910             07  W80-CTYPE PIC 9(01).
005920             07  W80-SER   PIC 9(04).
005930         05  FILLER        PIC 9(01).
005940     03  W80-TYPE        PIC 9(01).

       COPY W90.JCP.

006590 01  W95-STM.
006600     03  W95-COMP        PIC X(40).
006610	   03  W95-ADDRESS.
	       05  W95-CHAR    PIC  X(01) OCCURS 120.
	   03  W95-ADD-RED REDEFINES W95-ADDRESS.
	       05  W95-ADDPRN  PIC  X(70).
	       05  FILLER      PIC  X(50).
	   03  W95-ADD1.
	       05  W95-CHR1    PIC  X(01) OCCURS 30.
006620	   03  W95-ADD2.
	       05  W95-CHR2    PIC  X(01) OCCURS 30.
006630	   03  W95-ADD3.
	       05  W95-CHR3    PIC  X(01) OCCURS 30.
006640	   03  W95-ADD4.
	       05  W95-CHR4    PIC  X(01) OCCURS 30.
006650     03  W95-PC.
006660         05  W95-POST    PIC 9(04).
006670     03  W95-TEL         PIC X(14).
006680     03  W95-V1.
006690         05  W95-T       PIC Z(04)9.99.
006700     03  W95-V2.
006710         05  W95-B       PIC Z(06)9.99-.
006720     03  W95-D.
006730         05  W95-DTE     PIC Z9/99/99.
           03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.
006740 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99.
006750     03  W100-BAL        PIC Z(08)9.99-.
006760     03  W100-CUR        PIC Z(08)9.99-.
006770     03  W100-30         PIC Z(08)9.99-.
006780     03  W100-60         PIC Z(08)9.99-.
006790     03  W100-90         PIC Z(08)9.99-.
006800     03  W100-120        PIC Z(08)9.99-.
006800     03  W100-INT        PIC Z(08)9.99-.
006810     03  W100-CRL        PIC Z(06)9.
006820     03  W100-V2.
006830         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.

       LINKAGE SECTION.

000290 01  LS-PARID.
000320	   03  LS-SYS-ID       PIC  X(03).

000010 01  LS-USER-ID.
000020	   03  LS-USER.
	       05  LS-USE      PIC  9(03).
000040	   03  LS-COMPANY      PIC  X(40).
	   03  LS-INSTR        PIC  X(01).
	   03  LS-STOCK-CODE.
	       05  LS-NEXT-PRG PIC  X(12).
	       05  LS-ACNO     PIC  X(06).
	   03  LS-DAYS	       PIC  9(02) COMP-5.
	   03  LS-DEM	       PIC  9(02) COMP-5.
000060	   03  LS-USUB	       PIC  9(04) COMP-5.
000070	   03  LS-DRAW	       PIC  9(02).
000080	   03  LS-CSS	       PIC  X(01).
000090	   03  LS-OK	       PIC  X(01).
000100	   03  LS-RET	       PIC  X(01).
	   03  LS-TODAY-DDMMYY PIC  9(08) COMP-5.
	   03  LS-CUR-YR       PIC  X(01) COMP-X.
	   03  LS-CUR-CEN      PIC  X(01) COMP-X.
	   03  LS-PRV-CEN      PIC  X(01) COMP-X.
	   03  LS-NXT-CEN      PIC  X(01) COMP-X.
	   03  LS-SLS-ORDNO.
	     05  LS-SLS-JOBNO  PIC  X(08) VALUE SPACES.
	   03  LS-CARDEX       PIC  X(01).
	   03  LS-CRDMTH       PIC  X(01).
	   03  APAC-SYSTEM     PIC  X(12).
	   03  APAC-VERSION    PIC  X(04).

       01  L92-LABELS.
	   03  L92-JOB	       PIC  X(08).
	   03  L92-ITEM        PIC  X(40).
	   03  L92-NAME        PIC  X(40).
	   03  L92-ACNO        PIC  X(06).
	   03  L92-DATE        PIC  9(08).
	   03  L92-NO	       PIC  9(02).

      /
006840 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION USING LS-PARID LS-USER-ID L92-LABELS.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
010270	     PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM BD000.
	     GO TO AZ000-EOJ.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY OPTION.CRT.

010950 COPY LOCKED.RC2.

010970 COPY CLEAR.CRT.

       COPY PRINTUTL.AP5.

      /
      *    ****    READ FILES ROUTINES
      *
011130 AC000              SECTION.

011290 COPY PARAM.RD.

      /
011370 AD000-WRITE             SECTION.

011410 COPY PARAM.WR.

       COPY APAC.HLP.

      /
013100 AZ000-END               SECTION.
013110 AZ000-EOJ.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AZ100.
013260       EXIT PROGRAM.
      /
      *    ****    J O B - L A B E L S
      *
018500 BD000	     SECTION 5.
       BD00.
      *
      *    ****    1 2	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-L1.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-L1.
	     MOVE 1		 TO WS-S1.

018570 BD05.
	     MOVE L92-JOB	 TO LB5-CODE(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BD05.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-L1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-L1.
	     MOVE 1		 TO WS-S1.

       BD10.
	     MOVE L92-ITEM	 TO LB5-DESC(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BD10.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-L1.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-L1.
	     MOVE 1		 TO WS-S1.

       BD15.
	     MOVE L92-ACNO	 TO LB5-ACNO(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BD15.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-L1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-L1.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE 1		 TO WS-S1.

       BD20.
	     MOVE L92-NAME	 TO LB5-NAME(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BD20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-L1.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-L1.
	     MOVE 1		 TO WS-S1.

       BD25.
	     MOVE L92-DATE	 TO LB5-DATE(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BD25.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-L1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-L1.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****    1 0	 C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.

       BD599.
             EXIT.
      /
049180 ZA000-INIT	       SECTION 90.
049190 ZA000-OPEN.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
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
050580	     MOVE 1		 TO WS-PARKEY.
050590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050600       MOVE PAR-DMY        TO W12-TODAY.
050610       MOVE PAR-YMD        TO W12-T-YMD.
050620       MOVE PAR-COMPANY    TO W95-COMP.
050640       MOVE PAR-TELEPHONE  TO W95-TEL.

051610 ZA999.
051620       EXIT.

051640 I-O-ERRORS      SECTION	91.
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
052080	   IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100	       MOVE WS-PARKEY	 TO WS-KEY.
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
