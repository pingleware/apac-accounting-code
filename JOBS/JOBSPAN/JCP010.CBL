      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   ********   ******   *******	   ****       **      ****     *
      *      **     **	  **  **    **	  **  **     ***     **  **    *
      *      **     **	      **    **	 **    **     **    **	  **   *
      *      **     **	      *******	 **    **     **    **	  **   *
      *      **     **	      **	 **    **     **    **	  **   *
      *   ** **     **	  **  **	  **  **      **     **  **    *
      *    ***	     ******   **	   ****     ******    ****     *
      *                                                                *
      *     ENGLISH						       *
      *                                                                *
      *     T E C H N I C I A N   S T A R T   J O B		       *
      *                                                                *
      *     VERSION 8.00 - August 2000				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JCP010.
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

000170 COPY DEBTOR.ISL.

000270 COPY JOBTECH.SL.

000270 COPY JOBHDRN.SL.

000210 COPY PARAM.SL.

000230 COPY RECOVER.SL.

       COPY SHARED.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

000400 COPY DEBTOR.FDE.

000500 COPY JOBTECH.FDE.

000500 COPY JOBHDRN.FDE.

000440 COPY PARAM.FDE.

000460 COPY RECOVER.GFD.

       COPY SHARED.FDE.

      /
000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000570 77  WS-DISC	   PIC	99V99	 COMP-3.
000580 77  WS-TDISC        PIC  99V99    COMP-3.
000590 77  WS-CASH-AC      PIC  X(06).
000600 77  WS-SUB          PIC  9(04)    COMP-5.
002170 77  WS-SUPER        PIC  9(02)    COMP-5.
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
000780 77  WS-KEYSTR       PIC  9(06)    COMP-5.
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
000820 77  WS-NETKEY       PIC  9(04)    COMP-5.
000840 77  WS-RECKEY       PIC  9(06)    COMP-5.
000850 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000870 77  WS-PRN1	   PIC	9(01) VALUE 0.
000880 77  WS-PRN2         PIC  9(01) VALUE 0.  
000900 77  WS-JOB	   PIC	9(08).
000910 77  WS-TECH	   PIC	9(02).
       77  WS-TECHNICIAN   PIC	X(30).
000970 77  WS-PAR          PIC  9(01) VALUE 0.
000980 77  WS-INO          PIC  9(02) VALUE 0.
000990 77  WS-LINES        PIC  9(02) VALUE 0.
001000 77  WS-FIRST        PIC  9(02) VALUE 0.
001010 77  WS-LAST         PIC  9(02) VALUE 0.
001020 77  WS-LASTINV      PIC  9(04)    COMP-5.
001030 77  WS-LINE         PIC  9(04)    COMP-5.
001040 77  WS-PAGE         PIC  9(02)    COMP-5.
001050 77  WS-L1           PIC  9(04)    COMP-5.
001060 77  WS-OPTION       PIC  X(01).
001060 77  WS-IN-OPT       PIC  X(01).
001070 77  WS-MAINT        PIC  X(01).
001070 77  WS-TME          PIC  X(01).
001070 77  WS-SKIP         PIC  X(01) VALUE " ".
001080 77  WS-LOOK         PIC  X(01) VALUE "S".
001090 77  WS-ETYPE        PIC  X(01) VALUE "I".
001120 77  WS-ERROR        PIC  9(01).
001160 77  WS-COMP	   PIC	9(01).
001170 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
001180 77  WS-CONREC       PIC  9(01) VALUE ZERO.
001190 77  WS-HEAD         PIC  X(01).
001200 77  WS-TYPE         PIC  X(01).
001210 77  WS-AGE          PIC  X(01).
001220 77  WS-PRC          PIC  X(01).
001330 77  WS-CR	   PIC	X(01).
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
001450	       05  WS-MCHAR PIC  X(80) VALUE "³  ³              ³
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

001840 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "JCP".
	   03  WS-PROG	       PIC  X(03) VALUE "010".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

002060 COPY CONTROL.ID.

002100 COPY DEBTOR.ID.

002180 COPY JOBTECH.ID.

002180 COPY JOBHDR.ID.

       COPY PARAM.ID.

002140 COPY RECOVER.ID.

       COPY SHARED.ID.

       COPY W10.JCP.

       01  W10-JOB-TYPE.
           03  W10-KEY.
	       05  W10-JREM   PIC  X(06).
	   03  W10-REMK1      PIC  X(40).
	   03  W10-REMK2      PIC  X(40).
	   03  W10-REMK3      PIC  X(40).
	   03  W10-REMK4      PIC  X(40).

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

       COPY W20.WS.

003650 01  W25-CALCS.
003660     03  W25-RESULT      PIC 9(02)V99.
003670     03  W25-RESULT1 REDEFINES W25-RESULT.
003680         05  W25-WHOLE   PIC 9(02).
003690         05  W25-DECIMAL PIC 9(02).
003700     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
003710     03  W25-VALUE       PIC S9(07)V99 COMP-3.
003720     03  W25-CREDIT      PIC S9(07)V99 COMP-3.
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001690	       05  W25-HH      PIC  9(02).
001690	       05  W25-MM      PIC  9(02).
001690	       05  W25-SS      PIC  9(02).
001680	       05  FILLER      PIC  9(02).
	   03  W25-TRED2 REDEFINES W25-TIME.
	       05  W25-HMS     PIC  9(06).
	       05  FILLER      PIC  9(02).

       01  W30-PAYMENT-CODES.
	   03  W30-P-CODES.
	       05  FILLER      PIC  X(22) VALUE "RPREPAIRS".
	       05  FILLER      PIC  X(22) VALUE "FSFIELD SUPPORT".
	       05  FILLER      PIC  X(22) VALUE
					 "MCON SITE MAINT. CONT.".
	       05  FILLER      PIC  X(22) VALUE "W WARRANTY".
	       05  FILLER      PIC  X(22) VALUE "PMPREVENTATIVE MAINT.".
	       05  FILLER      PIC  X(22) VALUE "SRSPECIAL RATES".
	       05  FILLER      PIC  X(22) VALUE "RNREPEAT NO CHARGE".
	       05  FILLER      PIC  X(22) VALUE "CBCALL BACK NO CHARGE".
	       05  FILLER      PIC  X(22) VALUE "I INSTALLATION".
	       05  FILLER      PIC  X(22) VALUE "C CONTRACT".
	       05  FILLER      PIC  X(22) VALUE
					 "  ERROR - INVALID CODE".
	   03  W30-PAYMENTS REDEFINES W30-P-CODES.
	       05  W30-PMETH   OCCURS 11 TIMES.
		   07  W30-CDE PIC  X(02).
		   07  W30-DSC PIC  X(20).

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
003980	   03  W40-ANAME       PIC X(40).
           03  W40-CONTACT     PIC  X(30).
003990     03  W40-DEBTREC.
004000	       05  FILLER	 PIC X(580).
004010     03  W40-ADD REDEFINES W40-DEBTREC.
004020       05  W40-ADDRESS1.
004030         07  W40-ADD1    PIC X(30).
004040         07  W40-ADD2    PIC X(30).
004050         07  W40-ADD3    PIC X(30).
004060         07  W40-ADD4    PIC X(30).
004070       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
004080         07  W40-ACHAR OCCURS 120 PIC X(01).
004090       05  W40-PC1       PIC X(04).
004100	     05  W40-PADDRESS.
004110	       07  W40-PADD1   PIC  X(30).
004120	       07  W40-PADD2   PIC  X(30).
004130	       07  W40-PADD3   PIC  X(30).
004140	       07  W40-PADD4   PIC  X(30).
004150	     05  W40-PADDR2   REDEFINES W40-PADDRESS.
004160         07  W40-PCHAR OCCURS 120 PIC X(01).
004170       05  W40-PC2       PIC X(04).
             05  W40-TERMS     PIC X(30).
             05  W40-SPACES    PIC X(30).
             05  W40-TOP.
               07  W40-T1      PIC  X(01).
               07  W40-T2-27   PIC  X(26).
               07  W40-T28     PIC  X(01).
             05  W40-MID.
               07  W40-M1      PIC  X(01).
               07  W40-M2-27   PIC  X(26).
               07  W40-M28     PIC  X(01).
             05  W40-BOT.
               07  W40-B1      PIC  X(01).
               07  W40-B2-27   PIC  X(26).
               07  W40-B28     PIC  X(01).
003220       05  FILLER        PIC  X(77).
004190     03  W40-POST        PIC 9(04).
004200     03  W40-PPOST       PIC 9(04).
004210     03  W40-TEL         PIC X(14).
004210     03  W40-FAX         PIC X(14).
004220     03  W40-HTEL        PIC X(14).
004230     03  W40-ID.
004240         05  W40-IDNO    PIC X(14).
004250     03  W40-TAX REDEFINES W40-ID.
004260         05  W40-TAXNO   PIC X(14).
004270     03  W40-TITLE       PIC 9(02).
004280     03  W40-LANG        PIC X(01).
004290     03  W40-SMAN        PIC 9(02). 
           03  W40-LOC         PIC  X(03).
           03  W40-STATUS      PIC  X(01).
           03  W40-CUS-DET.
               05  W40-NM1     PIC  X(40).
               05  W40-AD1     PIC  X(30).
               05  W40-AD2     PIC  X(30).
               05  W40-AD3     PIC  X(30).
               05  W40-AD4     PIC  X(30).
               05  W40-PC      PIC  X(04).
	   03  W40-DTE	       PIC  9(08) COMP-3.
004100	   03  W40-REMARKS.
	       05  W40-REMK1   PIC  X(40).
	       05  W40-REMK2   PIC  X(40).
	       05  W40-REMK3   PIC  X(40).
	       05  W40-REMK4   PIC  X(40).

       COPY W40.WS.

       COPY FUNCTION.WS.

004540 COPY W50.WS.

004560 01  W60-NME-ADD.
004570     03  W60-NAME.
004580         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

004740 COPY W70DEBT.IWS.

004760 01  W75-KEYS.
004770     03  W75-S          PIC 9(02) COMP-5.
004780     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
004790     03  W75-DNO        PIC X(06) OCCURS 18.
	   03  W75-JOBS.
	       05  W75-JCOST OCCURS 18.
004810		   07  W75-JCST	 PIC  9(08).
		   07  W75-IDESC PIC  X(30).
           03  W75-TYPES REDEFINES W75-JOBS.
	       05  W75-JREM   PIC  X(06) OCCURS 18.
	       05  W75-TECH   PIC  9(02) COMP-5 OCCURS 18.
	       05  FILLER     PIC  X(558).

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
005700     03  W95-ADD1        PIC X(30).
005710     03  W95-ADD2        PIC X(30).
005720     03  W95-ADD3        PIC X(30).
005730     03  W95-ADD4        PIC X(30).
005740     03  W95-PC.
005750         05  W95-POST    PIC 9(04).
005760     03  W95-TEL         PIC X(14).
005810	   03  W95-D.
005820	       05  W95-DATE    PIC  Z9/99/9999.
	   03  W95-T.
	       05  W95-HH      PIC  9(02).
	       05  FILLER      PIC  X(01) VALUE ":".
	       05  W95-MM      PIC  9(02).
	       05  FILLER      PIC  X(01) VALUE ":".
	       05  W95-SS      PIC  9(02).

       01  W96-S-TIME.
	   03  W96-TIME        PIC  9(06).
	   03  W96-R-TIME REDEFINES W96-TIME.
	       05  W96-HH      PIC  9(02).
	       05  W96-MM      PIC  9(02).
	       05  W96-SS      PIC  9(02).

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

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
006010 SCREEN SECTION.

       COPY BLANK.CRT.

006620 01  S05.
006630	   03  LINE 12 COLUMN 12 VALUE "Enter Password".

006640 01  S06.
006650	   03  LINE 12 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC	X(06)
006660                           USING W40-CSHPASS AUTO SECURE.

       01  S10.
	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "LOG-ON TO A JOB".
	   03  LINE  4 COLUMN 17 VALUE "Technician No.".

       01  S10A.
	   03  LINE  4 COLUMN 32 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC Z9 USING WS-TECH AUTO.
       01  S10D.
	   03  LINE  4 COLUMN 35 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(30) FROM PAR-TECHNICIAN.

       COPY JOBLUP.CRT.

       COPY TECHNIC.CRT.

010240 01  S30A.
010250	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 0 PIC Z(08) USING W10-JCST AUTO.

       01  S33.
	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "LOG-ON TO A JOB".
010050	   03  LINE  3 COLUMN  3 VALUE "Job Number :".
010090	   03  LINE  4 COLUMN  3 VALUE "Ac Number  :".
010060	   03  LINE  5 COLUMN  3 VALUE "Name       :".
010070	   03  LINE  6 COLUMN  3 VALUE "Ord Number :".
010080	   03  LINE  7 COLUMN  3 VALUE "Technician :".
	   03  LINE  8 COLUMN  3 VALUE "Telephone  :".
	   03  LINE  9 COLUMN  3 VALUE "Contact    :".
010100	   03  LINE 10 COLUMN  3 VALUE "Date       :".
	   03  LINE 11 COLUMN  3 VALUE "Inv Type   :".
	   03  LINE 12 COLUMN  3 VALUE "Warr. Debt.:".
	   03  LINE 13 COLUMN  3 VALUE "Status     :".
	   03  LINE 14 COLUMN  3 VALUE "Cur. Total :".
	   03  LINE 15 COLUMN  3 VALUE "Comments   :".

010400 01  S36.
010410	   03  FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC X(06) USING W80-ACNO AUTO.

       COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010580 AA000-MAIN      SECTION.
010590 AA000-INIT.
003240	   IF LS0-JBLEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010600	     PERFORM ZA000-INIT.
	     PERFORM BA000.
	   IF NOT (WS-ERROR = 9)
	       PERFORM BG000.
	     CLOSE RECOVER
		   JOBTECH.
       AA49.
	     EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010910 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
011210	     PERFORM JOBHDR-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
011360 AA99.
011370       EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
011570 AA400              SECTION.
011580 AA401.
011590	     DISPLAY "F1" AT 2502 WITH FOREGROUND-COLOR 6 HIGHLIGHT
				       BACKGROUND-COLOR 3
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011600		     "=Job no," WITH FOREGROUND-COLOR 1
				     BACKGROUND-COLOR 3
011610		     "F3" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
011620		     "=Acnt no," WITH FOREGROUND-COLOR 1
				      BACKGROUND-COLOR 3
		     "F4" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Item desc," WITH FOREGROUND-COLOR 1
					BACKGROUND-COLOR 3
		     "F5" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Client name," WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "F6" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Contact," WITH FOREGROUND-COLOR 1
				      BACKGROUND-COLOR 3
		     "F7" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Date Lookup" WITH FOREGROUND-COLOR 1
					 BACKGROUND-COLOR 3.

011630 AA405.
011640       EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

011670 COPY CONTROL.RD.

011710 COPY DEBTOR.RD.

011750 COPY PARAM.RD.

011790 COPY JOBTECH.RD.

011790 COPY JOBHDRN.RD.

       COPY SHARED.RD.

      /
011850 AD000-WRITE             SECTION.

011870 COPY CONTROL.WR.

011890 COPY PARAM.WR.

011930 COPY JOBTECH.WR.

011790 COPY JOBHDR.WR.

011990 COPY JOBCST.LUP.

       COPY TECHNIC.LUP.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
012010 AY000           SECTION.
012020 AY21.
012030	     MOVE 21		 TO REC-FILE.
012040       MOVE ZERO           TO REC-KEY.
012050       MOVE WS-ACTION      TO REC-TYPE.
012060	     MOVE JHD-REC1	 TO REC-JOBHDR.
012070       GO TO AY50.

012080 AY39.
012090       MOVE 39             TO REC-FILE.
012100       MOVE WS-NETKEY      TO REC-KEY.
012110       MOVE NET-RECORD     TO REC-NETWORK.
012120       GO TO AY50.

012020 AY061.
012030	     MOVE 61		 TO REC-FILE.
012040       MOVE ZERO           TO REC-KEY.
012050       MOVE WS-ACTION      TO REC-TYPE.
012060	     MOVE TIM-REC1	 TO REC-JOBTECH.
012070       GO TO AY50.

012130 AY40.
012140       MOVE 99             TO REC-FILE.
012150       MOVE ZERO           TO REC-KEY.
012160       MOVE SPACES         TO REC-DETAIL.
012170       PERFORM AY50.
012180       ADD 1               TO WS-TRANS.
012190     IF WS-RECOVER > 95
012200         CLOSE RECOVER
012210         OPEN OUTPUT RECOVER
012220         CLOSE RECOVER
012230         OPEN I-O RECOVER
012240         MOVE ZERO         TO WS-RECOVER.
012250       GO TO AY59.

012260 AY50.
012270       ADD 1               TO WS-RECOVER.
012280       MOVE WS-RECOVER     TO WS-RECKEY.
012290       MOVE WS-TRANS       TO REC-TRAN.
012300       WRITE REC-RECORD.
012310     IF WS-STATUS NOT = "00"
012320         DISPLAY "Write error Recovery file - Status " AT 2212
012330                  WITH FOREGROUND-COLOR 14
012340                  WS-STATUS WITH FOREGROUND-COLOR 15
012350         STOP RUN.
012380 AY59.
012390       EXIT.

012400 AY60.
012480	   IF WS-SKIP = "Y" OR "A"
012490         GO TO AY999.
      *
      *    ****    Start processing transaction
      *
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
012690*      MOVE 5		 TO WS-NETKEY.
012700*      PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012430	     MOVE 4		 TO WS-PARKEY.
012440       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012450     IF PAR-USERS < 24
012460         MOVE 1            TO WS-SUB
012470         GO TO AY62.
012480	     DISPLAY "WAITING" AT 2551 WITH FOREGROUND-COLOR 12
					    BACKGROUND-COLOR 3.
012490       COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 130		 TO WS-WAIT.
012510	     PERFORM LOCK-REC-LOOP.
012520	     DISPLAY SPACE AT 2551 WITH BACKGROUND-COLOR 3
					FOREGROUND-COLOR 1.
012530       GO TO AY60.

012540 AY61.
012550	     MOVE "JC"		 TO PAR-PROG (WS-USUB).
012560	     MOVE LS-USER	 TO PAR-USR (WS-USUB).
012570       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
012650	     GO TO AY999.

012660 AY62.
012670	   IF PAR-PROG (WS-SUB) = "JC"
      *
      *    ****    Test if this is a program error first
      *
	       IF LS-USER = PAR-USR (WS-USUB)
                   SUBTRACT 1    FROM PAR-USERS
                   MOVE SPACES   TO PAR-PROG (WS-USUB)
                                    PAR-USR (WS-USUB)
               ELSE
                   UNLOCK PARAM
                   DISPLAY "Waiting" AT 2572 
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 14 BLINK
		   ACCEPT WS-STIME FROM TIME
		   MOVE 130	 TO WS-WAIT
		   PERFORM LOCK-REC-LOOP
                   DISPLAY "Waiting" AT 2572 
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 14 BLINK
                   GO TO AY60.
012760     IF WS-SUB < 24
012770         ADD 1             TO WS-SUB
012780         GO TO AY62.
012790	     DISPLAY SPACE AT 2572 WITH BACKGROUND-COLOR 3
					FOREGROUND-COLOR 1.
012800       MOVE 1              TO WS-SUB.

012810 AY63.
012820     IF PAR-USR (WS-SUB) = SPACES
012830         MOVE WS-SUB       TO WS-USUB
012840         ADD 1             TO PAR-USERS
012850         GO TO AY61.
012860       ADD 1               TO WS-SUB. 
012870       GO TO AY63.
      *
      *    ****    Write links back and unlock PARAM record 4.
      *
012880 AY70.
012920	     PERFORM AY40 THRU AY59.
012930       MOVE 4              TO WS-PARKEY.
012940       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
012950       MOVE SPACES         TO PAR-PROG (WS-USUB)
012960                              PAR-USR (WS-USUB).
012970       SUBTRACT 1          FROM PAR-USERS.
012980       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
012990       COMMIT.

013000 AY999.
013010       EXIT.
      *
      *    ****   G E T   J O B C S T	R E C O R D   U S I N G   JOB-KEY.
      *
      *           WS-ERROR:
      *
      *                    1 = NO RECORD
      *                    2 = NOT JOB RECORD (INVOICE)
      *                    3 = JOB DETAILS INCOMPLETE
      *                    4 = INVALID ACCOUNT NUMBER
      *
014550 GET-JOBCST	    SECTION 2.

014570 GET-JCST-JOBNO.
             INITIALIZE W14-DELETE.
             MOVE 1              TO W14-SUB.
             MOVE SPACES         TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-PC.
014580	     MOVE W10-JCST	 TO JHD-REF.
014600       MOVE ZERO           TO WS-ERROR.
014610	     PERFORM READ-JOBHDR THRU READ-JOBHDR-EXIT.
014620	   IF WS-F-ERROR = 14
014630         MOVE 1            TO WS-ERROR
014640         GO TO GET-JOBCST-EXIT.
014650	   IF JHD-DOC NOT = "J"
014660         MOVE 2            TO WS-ERROR
014670         GO TO GET-JOBCST-EXIT.
014680	     MOVE 1		 TO WS-LAST.
014790	     MOVE JHD-AC	 TO W80-ACNO DEB-ACNO.
014800	     PERFORM CA155-GET-DEBTOR.
014810	   IF WS-ERROR NOT = ZERO
014820	       MOVE 4		 TO WS-ERROR
014830	       GO TO GET-JOBCST-EXIT.
	     MOVE JHD-DTE	 TO W40-DTE.
	     MOVE JHD-ITEM	 TO W10-IDESC.
014860	     MOVE JHD-NAME	 TO W40-NAME.
	     MOVE JHD-AD1	 TO W40-AD1.
	     MOVE JHD-AD2	 TO W40-AD2.
	     MOVE JHD-AD3	 TO W40-AD3.
	     MOVE JHD-AD4	 TO W40-AD4.
	     MOVE JHD-PC	 TO W40-PC.
	     MOVE JHD-TEL	 TO W40-TEL.
	     MOVE JHD-HTEL	 TO W40-HTEL.
	     MOVE JHD-CONTACT	 TO W40-CONTACT.
	     MOVE JHD-ID	 TO W40-ID.
014870	     MOVE JHD-ORD	 TO W90-ORD.
014850	     MOVE JHD-OREF	 TO W90-REG.
014890	     MOVE JHD-EXP	 TO W90-EXP.
014910	     MOVE JHD-PRC	 TO WS-PRC.
014880	     MOVE JHD-SMAN	 TO W10-SMAN.
	     MOVE JHD-STATUS	 TO W90-STATUS.
	     MOVE JHD-COMMENT	 TO W10-COMMENT.
	     MOVE JHD-COMMENT2	 TO W10-COMMENT2.
	     MOVE JHD-START	 TO W90-START.
015230	     MOVE JHD-SUBTOT	 TO W90-DB.
015240	     MULTIPLY -1 BY JHD-DSCNT
015250				 GIVING W90-TDISC.
015260	     MOVE W90-TDISC	 TO W90-LDISC.
015270	     MOVE JHD-NON-TXBL	 TO W90-NONTAX.
015280	     MOVE JHD-TAXABLE	 TO W90-EXCTOT.
015290	     MOVE JHD-TAX	 TO W90-TTAX.
015300	     MOVE JHD-TOT	 TO W90-TOTAL.
015310	     MOVE JHD-BAL	 TO W90-BAL.
	     MOVE JHD-WCOMP	 TO W90-WCOMP.
	     MOVE JHD-PMETH	 TO W90-PMETH.
	     MOVE JHD-LOCATION	 TO W90-LOCATION.
      *
015410 GET-JOBCST-EXIT.
015420       EXIT.
      *
      *    ****    Read debtor record - using account number.
      *
013610 CA155-GET-DEBTOR  SECTION 2.
013620 CA155.
013630       MOVE W80-ACNO       TO DEB-ACNO.
013640       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
013650     IF WS-F-ERROR = 6
013660         MOVE 1            TO WS-ERROR
013670     ELSE
013680     IF DEB-ACNO NOT = W80-ACNO
013690         MOVE 2            TO WS-ERROR
013700     ELSE
013710         MOVE 0            TO WS-ERROR
013720         MOVE DEB-TYPE     TO W80-TYPE.
013730 CA160-EXIT.
013740       EXIT.
      /
      *    ****    T E C H N I C I A N	 D E T A I L S
      *
       BA000	     SECTION 50.
       BA00.
022600       DISPLAY CLR-SCREEN.
	     DISPLAY S10.
	     MOVE ZERO		 TO WS-TECH.
      *
      *    ****    T E C H N I C I A N	 C O D E   /   N U M B E R
      *
	     DISPLAY "F2" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "=Technician Code Lookup"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BA05.
	     ACCEPT S10A.
016640	     DISPLAY CLEAR-L25.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO BA999
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM TECHNICIAN-LOOKUP
011320		     PERFORM RESTORE-SCREEN
		     MOVE WS-S7  TO WS-TECH
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA05
	       END-EVALUATE
016570	       DISPLAY S10A
016580	       IF WS-TECH = ZERO
		   GO TO BA05.
	   IF WS-TECH > 50
	       MOVE "Technician must be in the range 00 to 50"
				 TO WS-ERR-MES
003260	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
	       GO TO BA05.
	   IF WS-TECH = ZERO
	       MOVE 9		 TO WS-ERROR
	       GO TO BA999.
	     ADD 200 WS-TECH	 GIVING WS-PARKEY.
	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF PAR-TECHNICIAN = SPACES
		MOVE "Invalid Technician Number"
				 TO WS-ERR-STRING
003260		PERFORM ERROR-MESSAGE
		GO TO BA05.
	     DISPLAY S10D.
	     MOVE PAR-TECHNICIAN TO WS-TECHNICIAN.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA00.

       BA999.
	     EXIT.
      /
      *    ****    L O G - O N	 T O   A   J O B
      *
033340 BG000	     SECTION 51.
033350 BG00.
022600       DISPLAY CLR-SCREEN.
             DISPLAY S33.
033470       MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC 
033480				    W90-BAL W90-SALES
033490				    W90-CUR W90-EXCTOT.
033500       MOVE 1              TO WS-FIRST WS-LAST.
033510       MOVE ZERO           TO W10-JCST.

033520 BG05.
033530       PERFORM AA400.
033540	     ACCEPT S30A AT 0316.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BG999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "J"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F5-KEY
		     MOVE "N"	 TO WS-ETYPE
		 WHEN F6-KEY
		     MOVE "C"	 TO WS-ETYPE
		 WHEN F7-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BG05
	       END-EVALUATE
016560	       PERFORM AA50
016570	       DISPLAY S30A AT 0316
016580	       IF W10-JCST = ZERO
016590		   GO TO BG05.
033600	     DISPLAY CLEAR-L25.
033610     IF W10-JCST = ZERO
033620         GO TO BG999.
033630       MOVE W10-JCST       TO W90-REF.

033640 BG10.
033650       PERFORM GET-JOBCST.
033660	   IF WS-ERROR = 1
033670	       MOVE "No Record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033680         GO TO BG05.
033690     IF WS-ERROR = 2
033700	       MOVE "Not a job"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033710         GO TO BG05.
033720     IF WS-ERROR = 3
033730	       MOVE "Job corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033750         GO TO BG05.
033760     IF WS-ERROR = 4
033770	       MOVE "Invalid a/c no"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033780         GO TO BG05.
033790	     DISPLAY S36 AT 0416.
033800	     DISPLAY W40-NAME AT 0516
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033810	     DISPLAY W90-ORD AT 0616
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033820	     DISPLAY W10-SMAN AT 0716
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
	     MOVE SPACES	 TO W10-DEPT.
033830     IF W10-SMAN NOT = ZERO
033840         ADD 200 W10-SMAN    GIVING WS-PARKEY
033850         PERFORM READ-PARAM THRU READ-PARAM-EXIT
	       MOVE PAR-TECHNICIAN
				 TO W10-DEPT
033860	       DISPLAY PAR-TECHNICIAN AT 0719
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0.
	     DISPLAY W40-TEL AT 0816
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
	     DISPLAY W40-CONTACT AT 0916
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
	     MOVE W40-DTE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO W90-DTE.
	     DISPLAY W90-DTE AT 1016
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
	     MOVE 1		  TO WS-S7.
      *
      *    ****    G E T   P A Y M E N T   T Y P E
      * 	   D E S C R I P T I O N
      *
       BG15.
	   IF WS-S7 > 10
	       GO TO BG20.
	   IF NOT (W90-PMETH = W30-CDE (WS-S7))
	       ADD 1		  TO WS-S7
	       GO TO BG15.

       BG20.
	     MOVE W30-CDE (WS-S7) TO W90-PMETH.
	     DISPLAY W30-DSC (WS-S7) AT 1116
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *
      *    ****    T E S T   I F   W A R R A N T Y   D E B T O R
      * 	   A N D   D I S P L A Y   D E T A I L S
      *
	     MOVE SPACES	 TO W40-ANAME.
	   IF NOT (W90-WCOMP = SPACES)
	       DISPLAY W90-WCOMP AT 1216
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0
	       MOVE DEB-RECORD1  TO W40-DEBTREC
	       MOVE W90-WCOMP	 TO W80-ACNO
	       PERFORM CA155-GET-DEBTOR
      *
      *    ****    I S	 W A R R A N T Y   A C C O U N T   V A L I D   ?
      *
	       IF WS-ERROR = ZERO
		   MOVE DEB-NAME TO W40-ANAME
		   IF W80-TYPE = 1
		       INSPECT W40-ANAME REPLACING ALL "*" BY " "
		       INSPECT W40-ANAME REPLACING FIRST "!" BY " "
		   END-IF
		   DISPLAY W40-ANAME AT 1223
			   WITH FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 0.
           IF W90-STATUS = " "
               MOVE "U"          TO W90-STATUS.
           IF W90-STATUS = "U"
	       DISPLAY "UNASSIGNED" AT 1316
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
           ELSE
           IF W90-STATUS = "A"
	       DISPLAY "ASSIGNED" AT 1316
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
           ELSE
           IF W90-STATUS = "W"
	       DISPLAY "WAITING" AT 1316
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 0
           ELSE
           IF W90-STATUS = "C"
	       DISPLAY "JOB COMPLETED" AT 1316
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0.
             MOVE W90-TOTAL      TO W100-BAL.
	     DISPLAY W100-BAL AT 1416
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
	     DISPLAY W10-COMMENT AT 1516
		    WITH FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 0.
	     DISPLAY W10-COMMENT2 AT 1616
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 0.
	     MOVE 17		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
	   IF WS-OPTION = "N"
	       GO TO BG00.
	   IF NOT (JHD-S-DTE = ZERO) AND (JHD-E-DTE = ZERO)
	       PERFORM SAVE-SCREEN-3
	       MOVE JHD-S-DTE	 TO W22-DTE2
	       MOVE W22-CC2	 TO W22-CC3
	       MOVE W22-YY2	 TO W22-YY3
	       MOVE W22-MM2	 TO W22-MM3
	       MOVE W22-DD2	 TO W22-DD3
	       MOVE W22-DTE3	 TO W95-DATE
	       MOVE JHD-S-TIME	 TO W96-TIME
	       MOVE W96-HH	 TO W95-HH
	       MOVE W96-MM	 TO W95-MM
	       MOVE W96-SS	 TO W95-SS
	       MOVE 19		 TO SHADE-ROW
	       MOVE 18		 TO SHADE-COL
	       MOVE 47		 TO SHADE-WIDTH
	       MOVE 4		 TO SHADE-LINES
	       DISPLAY
		  "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
		   AT 1816 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				BACKGROUND-COLOR 4
		  "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
	       DISPLAY "³ " AT 1916 WITH FOREGROUND-COLOR 4 HIGHLIGHT
					 BACKGROUND-COLOR 4
		       "Technician: " WITH FOREGROUND-COLOR 6 HIGHLIGHT
					   BACKGROUND-COLOR 4
			W10-SMAN WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 4
		       " " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 4
			W10-DEPT WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 4
		       " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
	       DISPLAY "³ " AT 2016 WITH FOREGROUND-COLOR 4 HIGHLIGHT
					 BACKGROUND-COLOR 4
		       "Log on	  : " WITH FOREGROUND-COLOR 6 HIGHLIGHT
					   BACKGROUND-COLOR 4
			W95-D WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 4
		       " at " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 4
			W95-T WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 4
		       " and must   " WITH FOREGROUND-COLOR 7 HIGHLIGHT
					  BACKGROUND-COLOR 4
		       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
	       DISPLAY "³              " AT 2116
			WITH FOREGROUND-COLOR 4 HIGHLIGHT
			     BACKGROUND-COLOR 4
		       "first log off.    Press any key. "
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 4
		       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
	       DISPLAY "À" AT 2216 WITH FOREGROUND-COLOR 4 HIGHLIGHT
					BACKGROUND-COLOR 4
		      "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
	       PERFORM SCREEN-SHADOW
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
	       PERFORM RESTORE-SCREEN-3
	       GO TO BG999.
012150	     ACCEPT W25-TIME FROM TIME.
      *
      *    ****    C H E C K   I F   T E C H N I C I A N   B U S Y
      * 	   W I T H   A N O T H E R   J O B
      *
       BG25.
	     MOVE WS-TECH	 TO TIM-TECH.
	     MOVE ZERO		 TO TIM-DATE TIM-TIME.
	     PERFORM START-AT-TIM-KEY THRU READ-JOBTECH-EXIT.
	   IF WS-F-ERROR = 61
	       GO TO BG40.

       BG30.
	     PERFORM READ-JOBTECH-NEXT THRU READ-JOBTECH-EXIT.
	   IF WS-F-ERROR = 61
	       GO TO BG40.
	   IF NOT (TIM-TECH = WS-TECH)
	       GO TO BG40.
	   IF NOT (TIM-E-DATE = ZERO)
	       GO TO BG30.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 19		 TO SHADE-ROW.
	     MOVE 18		 TO SHADE-COL.
	     MOVE 47		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
		      AT 1816 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				   BACKGROUND-COLOR 4
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     DISPLAY "³ " AT 1916 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				       BACKGROUND-COLOR 4
		     "Technician: " WITH FOREGROUND-COLOR 6 HIGHLIGHT
					 BACKGROUND-COLOR 4
		      WS-TECH WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 4
		     " " WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 4
		      WS-TECHNICIAN WITH FOREGROUND-COLOR 7 HIGHLIGHT
				    BACKGROUND-COLOR 4
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
	     DISPLAY "³ " AT 2016 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				       BACKGROUND-COLOR 4
		     "Busy with job " WITH FOREGROUND-COLOR 6 HIGHLIGHT
					   BACKGROUND-COLOR 4
		      TIM-JOB WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 4
		     ". Continue to LOG-ON and"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     DISPLAY "³ " AT 2116
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "LOG-OFF from current job "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 4
		     "/" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 4
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 4
		     " [ ]              "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     DISPLAY "À" AT 2216 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				      BACKGROUND-COLOR 4
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     PERFORM SCREEN-SHADOW.
	     MOVE "N"		 TO WS-OPTION.

       BG35.
	     ACCEPT WS-OPTION AT 2148
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO BG35.
	     PERFORM RESTORE-SCREEN-3.
	   IF WS-OPTION = "N"
	       GO TO BG999.
      *
      *    ****    S T A R T   R E C O V E R Y	 L O G	 ( L O G   O F F )
      *
021660	     PERFORM AY60 THRU AY999.
	     MOVE ZERO		 TO WS-ACTION.
      *
      *    ****    T E C H N I C I A N	 T I M E   R E C O R D
      *
	     PERFORM AY061 THRU AY59.
	     MOVE W12-T-YMD	 TO TIM-E-DATE.
	     MOVE W25-HMS	 TO TIM-E-TIME.
      *
      *    ****    J O B   H E A D E R	 R E C O R D  ( L O G	O F F )
      *
	     MOVE TIM-JOB	 TO JHD-REF.
014610	     PERFORM READ-JOBHDR-LOCK THRU READ-JOBHDR-EXIT.
	     PERFORM AY21 THRU AY59.
	     MOVE W22-DTE2	 TO JHD-E-DTE.
	     MOVE W25-HMS	 TO JHD-E-TIME.
	     PERFORM REWRITE-JOBTECH-UNLOCK THRU WRITE-JOBTECH-EXIT.
	     PERFORM REWRITE-JOBHDR-UNLOCK THRU WRITE-JOBHDR-EXIT.
      *
      *    ****    C L O S E   F I L E S   T O	 F O R C E   W R I T E
      * 	   T H E   R E C O R D S
      *
	     CLOSE JOBHDR
		   JOBTECH.
	     OPEN I-O JOBHDR
		      JOBTECH.
      *
      *    ****    E N D   R E C O V E R Y   -	 L O G	 O F F
      *
022770	     PERFORM AY70 THRU AY999.
      *
      *    ****    S T A R T   R E C O V E R Y	 L O G	 ( L O G   O F F )
      *
       BG40.
021660	     PERFORM AY60 THRU AY999.
	     MOVE ZERO		 TO WS-ACTION.
      *
      *    ****    R E A D   T H E   J O B   H E A D E R   R E C O R D
      * 	   O F	 T H E	 J O B	 T O   L O G   O N
      *
      *    This is required as the JOB HEADER record must be locked
      *    before updating.
      *
	     MOVE W10-JCST	 TO JHD-REF.
014610	     PERFORM READ-JOBHDR-LOCK THRU READ-JOBHDR-EXIT.
	     PERFORM AY21 THRU AY59.
	     MOVE W22-DTE2	 TO JHD-S-DTE.
	     MOVE W25-HMS	 TO JHD-S-TIME.
	     MOVE WS-TECH	 TO JHD-SMAN.
      *
      *    ****    C R E A T E	 T I M E   R E C O R D	 F O R	 T H E
      * 	   T E C H N I C I A N
      *
	     MOVE WS-TECH	 TO TIM-TECH.
	     MOVE W22-DTE2	 TO TIM-DATE.
	     MOVE W25-HMS	 TO TIM-TIME.
	     MOVE JHD-REF	 TO TIM-JOB.
	     MOVE ZERO		 TO TIM-E-DATE
				    TIM-E-TIME.
	     MOVE JHD-ITEM	 TO TIM-ITEM.
	     MOVE JHD-PMETH	 TO TIM-PMETH.
	     MOVE JHD-AC	 TO TIM-AC.
	     MOVE JHD-NAME	 TO TIM-NAME.
	     MOVE JHD-WCOMP	 TO TIM-WCOMP.
	     MOVE W40-ANAME	 TO TIM-WNAME.
	     PERFORM WRITE-JOBTECH THRU WRITE-JOBTECH-EXIT.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY061 THRU AY59.
	     PERFORM REWRITE-JOBHDR-UNLOCK THRU WRITE-JOBHDR-EXIT.
      *
      *    ****    C L O S E   F I L E S   T O	 F O R C E   W R I T E
      * 	   T H E   R E C O R D S
      *
	     CLOSE JOBHDR
		   JOBTECH.
	     OPEN I-O JOBHDR
		      JOBTECH.
      *
      *    ****    E N D   R E C O V E R Y   -	 L O G	 O N
      *
022770	     PERFORM AY70 THRU AY999.
	     MOVE "Request has been processed - Press any key"
				 TO WS-ERR-MES.
	     MOVE 18		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.

       BG999.
             EXIT.
      /
038140 ZA000-INIT      SECTION 90.
038150 ZA000-OPEN.
038160	     PERFORM ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
038170	     DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.
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
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
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
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "JOBHDR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBHDR.
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
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
038300	     MOVE 3		 TO WS-PARKEY.
038310       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
038440	     OPEN OUTPUT RECOVER.
038450       CLOSE RECOVER.
038460       OPEN I-O RECOVER.
	     OPEN I-O JOBTECH.
	   IF WS-STATUS = "05"
	       CLOSE JOBTECH
	       OPEN OUTPUT JOBTECH
	       CLOSE JOBTECH
	       OPEN I-O JOBTECH.
038980	   IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000             GO TO ZA200
039010         ELSE
039020         IF FLE-LIMIT
039030             GO TO ZA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060		   MOVE 61	 TO WS-F-ERROR
039070             GO TO ZA50.
039080     IF WS-STATUS NOT = "00"
039090	       MOVE 61		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039100		   PERFORM OPEN-ERROR.
039440	     MOVE 1		 TO WS-PARKEY.
039450       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039460       MOVE PAR-DMY        TO W12-TODAY.
039470       MOVE PAR-YMD        TO W12-T-YMD.
039480       MOVE PAR-COMPANY    TO W95-COMP.
039500       MOVE PAR-TELEPHONE  TO W95-TEL.
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

039850 ZA50.
039860	   IF WS-F-ERROR = 61
039870	       MOVE "TIME LOG file"
039880				 TO WS-FILE.
039980	     DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.
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

040780 I-O-ERRORS      SECTION	91.
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
040970         MOVE W02-NETWORK TO WS-FILE
040980         MOVE WS-NETKEY TO WS-KEY
041110	   ELSE
041240	   IF WS-F-ERROR = 14
041250	       MOVE W02-JOBHDR	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE JHD-KEY2	 TO WS-KEYX
041110	   ELSE
041120     IF WS-F-ERROR = 15
041130         MOVE WS-PARID     TO WS-FILE
041140         MOVE WS-PARKEY    TO WS-KEY
041150     ELSE
041160     IF WS-F-ERROR = 18
041170         MOVE W02-RECOVER  TO WS-FILE
041180	       MOVE WS-RECKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 33
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041190     ELSE
041200	   IF WS-F-ERROR = 61
041210	       MOVE W02-JOBTECH  TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE TIM-KEY	 TO WS-KEYX.
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
