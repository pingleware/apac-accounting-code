      $SET LINKCOUNT"640"
      ******************************************************************
      *                                                                *
      *   ********  *******   *******	   ****      ****    *******   *
      *      **     **	  **  **    **	  **  **    **	**   **        *
      *      **     **	  **  **    **	 **    **  **	 **  **        *
      *      **     *******   *******	 **    **  **	 **  *******   *
      *      **     **	  **  **   **	 **    **  **	 **	   **  *
      *   ** **     **	  **  **    **	  **  **    **	**   **    **  *
      *    ***	    *******   **    **	   ****	     ****     ******   *
      *                                                                *
      *     ENGLISH						       *
      *                                                                *
      *     S P A R E S   R E Q U I S I T I O N   P R I N T   S L I P  *
      *                                                                *
      *     VERSION 8.00 - August 2000				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JBR005.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1992.
000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000130 COPY CONTROL.SL.

000210 COPY DEPART.SL.

       COPY JOBREQ.SL.

000210 COPY PARAM.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

000500 COPY DEPART.FDE.

       COPY JOBREQ.FDE.

000440 COPY PARAM.FDE.

      /
000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000570 77  WS-DISC	   PIC	99V99	 COMP-3.
000580 77  WS-TDISC        PIC  99V99    COMP-3.
000590 77  WS-CASH-AC      PIC  X(06).
000600 77  WS-SUB          PIC  9(04)    COMP-5.
000610 77  WS-S1           PIC  9(04)    COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
000650 77  WS-S5           PIC  9(04)    COMP-5.
000660 77  WS-S6           PIC  9(04)    COMP-5.
000670 77  WS-S7           PIC  9(04)    COMP-5.
000680 77  WS-S8           PIC  9(04)    COMP-5.
000690 77  WS-S9           PIC  9(04)    COMP-5.
000700 77  WS-IXS          PIC  9(04)    COMP-5.
000710 77  WS-IXS1         PIC  9(04)    COMP-5.
000720 77  WS-IXS2         PIC  9(04)    COMP-5.
000730 77  WS-IXS3         PIC  9(04)    COMP-5.
000740 77  WS-IXS4         PIC  9(04)    COMP-5.
000750 77  WS-PARKEY       PIC  9(04)    COMP-5.
000770 77  WS-DEPKEY       PIC  9(04)    COMP-5.
000780 77  WS-KEYSTR       PIC  9(06)    COMP-5.
000800 77  WS-SALSTR       PIC  9(04)    COMP-5.
000810 77  WS-SALKEY       PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
000820 77  WS-NETKEY       PIC  9(04)    COMP-5.
000840 77  WS-RECKEY       PIC  9(06)    COMP-5.
000850 77  WS-RECOVER      PIC  9(06)    COMP-5.
000860 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000870 77  WS-PRN1         PIC  9(01) VALUE 0.  
000880 77  WS-PRN2         PIC  9(01) VALUE 0.  
000900 77  WS-START	   PIC	9(08).
000910 77  WS-END          PIC  9(08).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
000930 77  WS-TME2	   PIC	9(08).
000940 77  WS-PORT         PIC  9(05) VALUE 1004.
000950 77  WS-OPEN         PIC  X(01) VALUE X"01".
000960 77  WS-CLOSE        PIC  X(01) VALUE X"00".
000970 77  WS-PAR          PIC  9(01) VALUE 0.
000980 77  WS-INO          PIC  9(02) VALUE 0.
000990 77  WS-LINES        PIC  9(02) VALUE 0.
001000 77  WS-FIRST	   PIC	9(03) VALUE 0.
001010 77  WS-LAST	   PIC	9(03) VALUE 0.
001020 77  WS-LASTINV      PIC  9(04)    COMP-5.
001030 77  WS-LINE         PIC  9(04)    COMP-5.
001040 77  WS-PAGE         PIC  9(02)    COMP-5.
001050 77  WS-L1           PIC  9(04)    COMP-5.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
001060 77  WS-OPTION	   PIC	X(01).
001060 77  WS-IN-OPT       PIC  X(01).
001070 77  WS-MAINT        PIC  X(01).
001070 77  WS-TME          PIC  X(01).
001070 77  WS-SKIP         PIC  X(01) VALUE " ".
001080 77  WS-LOOK         PIC  X(01) VALUE "S".
001090 77  WS-ETYPE        PIC  X(01) VALUE "I".
001120 77  WS-ERROR        PIC  9(01).
001140 77  WS-CHAR1        PIC  X(01).
001150 77  WS-CHAR2        PIC  X(01).
001160 77  WS-COMP         PIC  9(01).
001170 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
001180 77  WS-CONREC       PIC  9(01) VALUE ZERO.
001190 77  WS-HEAD         PIC  X(01).
001200 77  WS-TYPE         PIC  X(01).
001210 77  WS-AGE          PIC  X(01).
001220 77  WS-PRC          PIC  X(01).
001250 77  WS-BATCH	   PIC	9(05).
001280 77  WS-A 	   PIC	X(19) VALUE "Any key to continue".
001290 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
001310 77  WS-ERR2         PIC  X(09) VALUE "No Record".
001320 77  WS-PER          PIC  Z(02)9.99.
001330 77  WS-CR           PIC  X(01).
001340 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
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
001450	       05  WS-MCHAR PIC  X(80) VALUE "³   ³              ³
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
001720	       05  WS-MCB   PIC  X(80) VALUE "³   ³        ³
      -        "  ³                             ³         ³          ³".
	   03  WS-RQ78-L REDEFINES WS-MD-LIN.
	       05  WS-RQ1      PIC  X(01).
	       05  WS-RQ78     PIC  X(78).
	       05  WS-RQ80     PIC  X(01).

001840 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "JBR".
	   03  WS-PROG	       PIC  X(03) VALUE "005".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

002060 COPY CONTROL.ID.

003760 COPY DEPART.ID.

002140 COPY RECOVER.ID.

002180 COPY JOBREQ.ID.

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
	       05  FILLER	   PIC	X(56).
	       05  REP-DATE	   PIC	X(06).
	       05  REP-DTE	   PIC Z9/99/9999.
	       05  FILLER	   PIC	X(02).
	       05  REP-TME	   PIC	X(06).
	       05  REP-TIME	   PIC	X(08).

	   03  REP-L3A REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(10).
	       05  REP-TEL	   PIC X(14).
	       05  REP-CNTCT	   PIC X(10).
	       05  REP-CONTACT	   PIC X(30).
	       05  FILLER	   PIC X(32).

	   03  REP-L4 REDEFINES W02-PRN-LINE.
	       05  REP-H1	   PIC X(28).
	       05  REP-REF	   PIC 9(08)/99.
	       05  FILLER	   PIC X(41).

	   03  REP-L5 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(02).
	       05  REP-H2	   PIC X(10).
	       05  REP-H3	   PIC X(16).
	       05  REP-H4	   PIC X(43).
	       05  REP-H5	   PIC X(10).
	       05  REP-H6	   PIC X(09).

	   03  REP-L6 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(02).
	       05  REP-JOB	   PIC Z(07)9.
	       05  FILLER	   PIC X(01).
	       05  REP-EXT-ITEM.
		   07  REP-ITM1	   PIC	X(14).
		   07  REP-SL	   PIC	X(01).
		   07  REP-ITM-EXT PIC	X(03).
	       05  FILLER	   PIC X(01).
	       05  REP-DESC	   PIC X(40).
	       05  FILLER	   PIC X(01).
	       05  REP-QNT	   PIC Z(05)9.99.
	       05  FILLER	   PIC X(01).
	       05  REP-NC.
		   07  REP-PRICE   PIC	Z(05)9.99.
	       05  FILLER	   PIC	X(02).
	       05  REP-REMKS	   PIC	X(08).

	   03  REP-L7 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(11).
	       05  REP-SMAN	   PIC X(30).
	       05  FILLER	   PIC X(01).
	       05  REP-H8	   PIC X(10).
	       05  REP-TERMS	   PIC X(28).

	   03  REP-L8 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(11).
	       05  REP-ORD	   PIC X(14).
	       05  FILLER	   PIC X(03).
	       05  REP-H9	   PIC X(10).
	       05  REP-CONTACT2    PIC X(42).

       COPY W05.VAT.

002380 01  W05-INV.
002390     03  W05-ENTRY       PIC  ZZ9.
002400     03  FILLER          PIC  X(01) VALUE SPACES.
002410     03  W05-DATE        PIC  Z9/99/99.
002420     03  FILLER          PIC  X(01) VALUE SPACES.
002430     03  W05-TYPE        PIC  X(03).
002440     03  FILLER          PIC  X(03) VALUE SPACES.
002450     03  W05-REF         PIC  9(07).
002460     03  FILLER          PIC  X(01) VALUE SPACES.
002470     03  W05-VALUE       PIC  Z(06)9.99-.

       COPY W10.JCP.

003220	   03  W10-JREQ       PIC  9(08).

       COPY W12.WS.

003390 01  W14-JOB.
003400	   03  W14-REF	      PIC  Z(08).
003410	   03  W14-AC	      PIC  X(06).
003420     03  W14-NAME.
003430	       05  W14-NME    PIC  X(32).
003440	       05  FILLER     PIC  X(08).
003450	   03  W14-DATE       PIC  Z9/99/9999.
003470	   03  W14-DOC	      PIC  X(01).
003480	   03  W14-DDOC.
003490	       05  W14-INV    PIC  Z(08).
003500	   03  W14-VALUE      PIC  Z(07)9.99.
	   03  W14-STATUS     PIC  X(01).

003510 01  W14-DELETE.
003520     03  W14-SUB         PIC 9(03)     COMP-3.
003530     03  W14-SQN         PIC 9(05)     COMP-3 OCCURS 200.

003540 01  W15-DISCOUNT.
003550     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       COPY W20.WS.

003650 01  W25-CALCS.
003660	   03  W25-RESULT      PIC 9(05)V99.
003670     03  W25-RESULT1 REDEFINES W25-RESULT.
003680	       05  W25-WHOLE   PIC 9(05).
003690         05  W25-DECIMAL PIC 9(02).
	   03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003700	   03  W25-TOTAL       PIC 9(07)V99  COMP-3.
003710     03  W25-VALUE       PIC S9(07)V99 COMP-3.
003720     03  W25-CREDIT      PIC S9(07)V99 COMP-3.
001660	   03  W25-TIME        PIC 9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001690         05  W25-HH      PIC 9(02).
001690         05  W25-MM      PIC 9(02).
001690         05  W25-SS      PIC 9(02).
001680         05  FILLER      PIC 9(02).
       01  W35-DETKEY.
           03  W35-KEY.
000050       05  W35-REF       PIC  X(08).
             05  W35-SEQN.
                 07  W35-SEQ   PIC  9(03).
                 07  W35-SUB   PIC  9(02).
             05  W35-SQN REDEFINES W35-SEQN
                               PIC  9(05).
003860 01  W40-COMPANY.
003870     03  W40-NAME1.
003880         05  W40-NAME.
                   07  W40-NCASH.
004720                 09  W40-ENTRY 
                               PIC X(07).
                       09  FILLER
                               PIC X(02).
004730             07  FILLER  PIC X(31).
003910         05  FILLER      PIC X(32).
003920     03  W40-NAME2 REDEFINES W40-NAME1.
003930         05  W40-SURNAME PIC X(24).
003940         05  W40-FNAME   PIC X(24).
003950         05  W40-SNAME   PIC X(24).
003960     03  W40-NAME3 REDEFINES W40-NAME1.
003970         05  W40-NCHAR OCCURS 72 PIC X(01).
003830	   03  W40-NME.
003840         05  W40-NCH     PIC  X(01) OCCURS 32.
           03  W40-CONTACT     PIC  X(30).
004210	   03  W40-TEL	       PIC X(14).
004210     03  W40-FAX         PIC X(14).
004220     03  W40-HTEL        PIC X(14).

       COPY W40.WS.

       COPY FUNCTION.WS.

004540 COPY W50.WS.


004740 COPY W70DEBT.IWS.

004760 01  W75-KEYS.
004770     03  W75-S          PIC 9(02) COMP-5.
004780     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
004790     03  W75-DNO        PIC X(06) OCCURS 18.
	   03  W75-REQ.
	       05  W75-REQ-KEY OCCURS 18.
		   07  W75-JREQ	   PIC	9(08).
		   07  W75-REQ-SQN PIC	9(05).
	   03  W75-JOBS REDEFINES W75-REQ.
004810	       05  W75-JCST   PIC  9(08) OCCURS 18.
	       05  FILLER     PIC  X(90).
	   03  W75-TYPES REDEFINES W75-REQ.
	       05  W75-JREM   PIC  X(06) OCCURS 18.
	       05  FILLER     PIC  X(126).
004820 01  W75-TITLES.
004830     03  W75-DESCRIP.
004840         05  FILLER      PIC X(28)      VALUE
004850                     "Mr     Mnr    Mrs    Mev    ".
004860         05  FILLER      PIC X(28)      VALUE
004870                     "Miss   Mej    Ms     Ms     ".
004880         05  FILLER      PIC X(28)      VALUE
004890                     "Dr     Dr     Rev    Ds     ".
004900         05  FILLER      PIC X(28)      VALUE
004910                     "Prof   Prof   The HonSy Edel".
004920         05  FILLER      PIC X(28)      VALUE
004930                     "                            ".
004940     03  W75-CODES REDEFINES W75-DESCRIP.
004950         05  W75-TITLE  OCCURS 10.
004960             07  W75-ETITLE PIC X(07).
004970             07  W75-ATITLE PIC X(07).
004980 01  W80-ACCOUNT.
004990     03  W80-ACNO        PIC X(06).
005000     03  W80-CRED REDEFINES W80-ACNO.
005010         05  W80-CNO     PIC 9(05).
005020         05  W80-CAC REDEFINES W80-CNO.
005030             07  W80-CTYPE PIC 9(01).
005040             07  W80-SER   PIC 9(04).
005050         05  FILLER        PIC 9(01).
005060     03  W80-TYPE        PIC 9(01).
005070 01  W85-PASS.
005080     03  W85-SUPER       PIC  X(06)    OCCURS 9.
005090     03  W85-ENTRY       PIC  9(02)    COMP-3.
005100     03  W85-MARG        PIC S9(02)V99.
005110 01  W85-BATCHES.
005120         05  W85-BATCH   PIC  9(05).

       COPY W90.JCP.

005680 01  W95-STM.
005690     03  W95-COMP        PIC X(40).
005770     03  W95-V1.
005780         05  W95-T       PIC Z(04)9.99.
005790     03  W95-V2.
005800         05  W95-B       PIC Z(06)9.99-.
005810     03  W95-D.
005820	       05  W95-DTE     PIC Z9/99/9999.
	   03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.
005830 01  W100-EDIT.
005840     03  W100-LBAL       PIC Z(06)9.99-.
005850     03  W100-LARR       PIC Z(06)9.99.
005860     03  W100-BAL        PIC Z(08)9.99-.
005870     03  W100-CUR        PIC Z(08)9.99-.
005880     03  W100-30         PIC Z(08)9.99-.
005890     03  W100-60         PIC Z(08)9.99-.
005900     03  W100-90         PIC Z(08)9.99-.
005910     03  W100-120        PIC Z(08)9.99-.
005920     03  W100-INT        PIC Z(08)9.99-.
005930     03  W100-CRL        PIC Z(06)9.
005940     03  W100-V2.
005950         05  W100-S6V2   PIC Z(05)9.99-.
005960     03  W100-V3 REDEFINES W100-V2.
005970         05  W100-PRICE  PIC X(09).
005980         05  FILLER      PIC X(01).
005990     03  W100-V4 REDEFINES W100-V2.
006000         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(08)9-.
	   03  W100-V6 REDEFINES W100-V2.
	       05  W100-QNTY   PIC Z(04)9.99.
	       05  FILLER      PIC X(02).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
006010 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY JRQLUP.CRT.

010020 01  S42.
010030	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 15
010040				 VALUE "PRINT SPARES REQUISITIONS".
	   03  LINE  4 COLUMN  2 VALUE "Requisition No :".

       01  S43.
	   03  S43A.
	       05  LINE  4 COLUMN 19 BACKGROUND-COLOR 0
				     FOREGROUND-COLOR 7 HIGHLIGHT
				     PIC Z(08) USING W10-JREQ AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.

010580 AA000-MAIN      SECTION.
010590 AA000-INIT.
003240	   IF LS0-JBLEV < 1
	       DISPLAY CLEAR-L25
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010600	     PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM EA000.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

013140 AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010910 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
011180	     PERFORM JOBREQ-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
011360 AA99.
011370       EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

       COPY PRINTUTL.AS1.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

011670 COPY CONTROL.RD.

011210 COPY DEPART.RD.

011750 COPY PARAM.RD.

011790 COPY JOBREQ.RD.

      /
011850 AD000-WRITE             SECTION.

011930 COPY JOBREQ.WR.

       COPY JOBREQ.LUP.

       COPY APAC.HLP.

      /
017690 BB050               SECTION.
017700 BB50.
	     MOVE "H"		 TO W02-PRN-TYPE.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
017710	     MOVE SPACES	 TO REP-DETAIL1.
017750	     MOVE "REQUISITION - REFERENCE NO:"
                                 TO REP-H1.
             MOVE W90-PS         TO REP-REF.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF W90-PICK > 01
	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE "REPLACE PREVIOUS REQUISITION"
                                 TO REP-H1
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.

       BB60.
	     MOVE "WORKSHOP/SERVICE DEPARTMENT"
				 TO REP-H1.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017710	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017710	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE "Date:"        TO REP-DATE.
             MOVE W12-TODAY      TO REP-DTE.
	     MOVE "Time:"	 TO REP-TME.
012150       ACCEPT W25-TIME FROM TIME.
             MOVE W25-HH         TO W95-HH.
             MOVE W25-MM         TO W95-MM.
             MOVE W25-SS         TO W95-SS.
             MOVE W95-TIME       TO REP-TIME.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017790	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE ALL "="        TO REP-DETAIL1.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017790	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE " JOB NO."	 TO REP-H2.
017800	     MOVE "ITEM CODE"	 TO REP-H3.
017820	     MOVE "DESCRIPTION"  TO REP-H4.
017840	     MOVE "QUANTITY"	 TO REP-H5.
	     MOVE "UNT-PRICE"	 TO REP-H6.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

017950 BB65.
017960       EXIT.
      /
      *  ****  P R I N T   S P A R E S	 R E Q U I S I T I O N S
      *
036990 EA000		       SECTION 5.
037000 EA00.
037010       DISPLAY CLR-SCREEN.
	     DISPLAY S42.
025810	     MOVE ZERO		 TO W10-JREQ.

       EA05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011600		     "=Requisition Lookup"
		      WITH FOREGROUND-COLOR 1
			   BACKGROUND-COLOR 3.

       EA06.
	     MOVE 1		 TO WS-SKIP.
	     ACCEPT S43A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W10-JREQ
		     GO TO EA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO EA06
		 WHEN F2-KEY
		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO EA06
	       END-EVALUATE
016570	       DISPLAY S43A
016580	       IF W10-JREQ = ZERO
016590		   GO TO EA06
	       ELSE
		   GO TO EA15.
	   IF W10-JREQ = ZERO
	       GO TO EA999.

       EA15.
	     INITIALIZE REQ-REC1.
	     MOVE W10-JREQ	 TO REQ-REF W90-REF.
	     MOVE 100		 TO REQ-SQN.
	     PERFORM READ-JOBREQ THRU READ-JOBREQ-EXIT.
	   IF WS-F-ERROR = 42
032780	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO EA06.
	     DISPLAY CLEAR-L25.
	     DISPLAY "Checking for outstanding Items" AT 2312
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "Y"		 TO WS-SKIP.

       EA20.
	     PERFORM READ-JOBREQ-NEXT THRU READ-JOBREQ-EXIT.
	   IF WS-F-ERROR = 42
	       GO TO EA25.
	   IF REQ-REF NOT = W10-JREQ
	       GO TO EA25.
	   IF REQ-TYPE = 9
	       GO TO EA25.
	   IF REQ-TYPE NOT = 1
	       GO TO EA20.
	   IF NOT (REQ-STATUS = "R")
	       GO TO EA20.
	     MOVE "N"		 TO WS-SKIP.

       EA25.
	     DISPLAY CLEAR-L25.
	   IF WS-SKIP = "N"
	       GO TO EA30.
	     MOVE "No outstanding items - Press ANY key"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	     GO TO EA00.

       EA30.
023010	     DISPLAY "Printing picking slip" AT 2312
023020		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
023030	     MOVE W90-REF	 TO LS-SLS-JOBNO.
      *
      *    ****    R E A D   J O B R E Q   F I L E   A N D   P R I N T
      *
026330	     MOVE W90-REF	 TO REQ-REF.
	     MOVE ZERO		 TO REQ-SQN.
	     PERFORM START-AT-REQ-CODE THRU READ-JOBREQ-EXIT.
	   IF WS-F-ERROR = 42
	       MOVE "Database corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO EA999.

       EA140.
	     PERFORM READ-JOBREQ-NEXT THRU READ-JOBREQ-EXIT.
	   IF WS-F-ERROR = 42
	       GO TO EA165.
	   IF REQ-REF NOT = W90-REF
	       GO TO EA165.
	   IF REQ-TYPE = 0
	       MOVE REQ-REF	 TO W90-RF
	       MOVE REQ-PICNO	 TO W90-PICK
               ADD 1             TO W90-PICK
               PERFORM BB050
	       GO TO EA140.
	   IF REQ-TYPE = 9
	       GO TO EA165.
	   IF REQ-TYPE NOT = 1
	       IF REQ-TYPE = 2
		   GO TO EA140
               ELSE
		   MOVE REQ-DESC TO REP-DESC
		   PERFORM EA145
		   GO TO EA140.
	   IF NOT (REQ-STATUS = "R")
	       GO TO EA140.
	     MOVE REQ-JOB	 TO REP-JOB.
	   IF WS-USE-ITM = "Y"
	       MOVE REQ-ITEM	 TO REP-ITM1
	       MOVE "/"		 TO REP-SL
	       MOVE REQ-ITM	 TO REP-ITM-EXT
	   ELSE
	       MOVE REQ-EXT-ITEM TO REP-EXT-ITEM.
	     MOVE REQ-DESC	 TO REP-DESC.
	     MOVE REQ-QNT	 TO REP-QNT.
	   IF REQ-SELL = ZERO
               MOVE "  N/C"      TO REP-NC
           ELSE
	       MOVE REQ-SELL	 TO REP-PRICE.

       EA145.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "REQUISITION - CONTINUED ..."
                                 TO REP-H1
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM BB60 THRU BB65.
	     MOVE SPACES	 TO REP-DETAIL1.
       EA165.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE ALL "="	 TO REP-DETAIL1.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017710	     MOVE SPACES	 TO REP-DETAIL1.
026330	     MOVE W90-REF	 TO REQ-REF.
	     MOVE 100		 TO REQ-SQN.
	     PERFORM READ-JOBREQ-LOCK THRU READ-JOBREQ-EXIT.
	     ADD 1		 TO REQ-PICNO.
	     PERFORM REWRITE-JOBREQ-UNLOCK THRU WRITE-JOBREQ-EXIT.
	     MOVE SPACES	 TO REP-DETAIL1.
	     GO TO EA00.

       EA999.
	     EXIT.
      /
038140 ZA000-INIT	       SECTION 9.
038150 ZA000-OPEN.
038160	     PERFORM ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
      *	     MOVE 44		 TO USER-NUMBER.
      *	     MOVE 1		 TO USER-SETTING.
      *	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *    PRE-DISPLAY ACCEPT FIELDS BEFORE DATA-ENTRY
      *
      *      MOVE 76		 TO USER-NUMBER.
      *      MOVE 3		 TO USER-SETTING.
      *      MOVE 1		 TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "JOBREQ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBREQ.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
038300	     MOVE 3		 TO WS-PARKEY.
038310       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.

       ZA01B.
038430       MOVE PAR-VAT-REG    TO W05-VATNO.
039390	     MOVE 5		 TO WS-PARKEY.
039400       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039430	     MOVE PAR-DISCOUNT	 TO W15-DISCOUNT.
	     MOVE 1		 TO W05-V-RATE.

       ZA05-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA05-VAT.
039440	     MOVE 1		 TO WS-PARKEY.
039450       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039460       MOVE PAR-DMY        TO W12-TODAY.
039470       MOVE PAR-YMD        TO W12-T-YMD.
039480       MOVE PAR-COMPANY    TO W95-COMP.
039740	     MOVE 7		 TO WS-PARKEY.
039750       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039760       MOVE PAR-CASH-AC    TO WS-CASH-AC.
039770       GO TO ZA999.

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
040090 ZA60.
040650	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = WS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
040660 ZA200.
040670       DISPLAY "Files locked - Try later" AT 2312
040680                WITH FOREGROUND-COLOR 14
040690               " " WS-STATUS WITH FOREGROUND-COLOR 15.
040700       DISPLAY "Press any key" AT 2512 
040710		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040730 ZA205.
040740       EXIT PROGRAM.
040750 ZA999.
040760       EXIT.

040780 I-O-ERRORS      SECTION	9.
040790 OPEN-ERROR.
040800       DISPLAY CLR-SCREEN.
040810       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
040820       PERFORM DISPLAY-FILE-NAME.
040830
040840 READ-ERROR.
040850       DISPLAY CLR-SCREEN.
040860       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
040870       PERFORM DISPLAY-FILE-NAME.
040880
040890 WRITE-ERROR.
040900       DISPLAY CLR-SCREEN.
040910       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
040920       PERFORM DISPLAY-FILE-NAME.
040930

040950 DISPLAY-FILE-NAME.
040960     IF WS-F-ERROR = 2
040970	       MOVE W02-NETWORK  TO WS-FILE
040980	       MOVE WS-NETKEY	 TO WS-KEY
041230	   ELSE
056430	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041110	   ELSE
041120     IF WS-F-ERROR = 15
041130         MOVE WS-PARID     TO WS-FILE
041140         MOVE WS-PARKEY    TO WS-KEY
041150	   ELSE
041240	   IF WS-F-ERROR =  42
041250	       MOVE W02-JOBREQ	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE REQ-KEY	 TO WS-KEYX.
041350	   IF WS-STATUS = "10"
041360         MOVE "End of FILE" TO WS-STAT-MESSAGE
041370     ELSE
041380     IF WS-STATUS = "22"
041390         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041400     ELSE
041410     IF WS-STATUS = "23"
041420         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041430     ELSE
041440     IF WS-STATUS = "24"
041450         MOVE "DISK full" TO WS-STAT-MESSAGE
041460     ELSE
041470     IF WS-STATUS = "30"
041480         MOVE "DISK error" TO WS-STAT-MESSAGE
041490     ELSE
041500     IF WS-STATUS = "94"
041510         MOVE "FILE locked" TO WS-STAT-MESSAGE.
041520       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
041530       DISPLAY "Status " AT 1212 
041540                WS-STATUS WITH FOREGROUND-COLOR 11
041550               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
041560     IF WS-STATUS NOT = "94" 
041570         DISPLAY "Key    " AT 1412 
041580                  WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
041590         DISPLAY "Reverse backup or contact program Support"
041600                  AT 1612.
041610         DISPLAY "Please make a note of these details" AT 1812.
041620       STOP RUN.
