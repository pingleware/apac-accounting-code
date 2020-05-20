      $SET LINKCOUNT"512"
      ******************************************************************
      * 							       *
      *   ********   ******   *******	   ****      ****	 ***   *
      *      **     **    **  **    **    **  **    **  **      * **   *
      *      **     **        **    **   **    **  **    **    *  **   *
      *      **     **        *******    **    **  **    **   *   **   *
      *      **     **        **         **    **  **    **  ********  *
      *   ** **     **    **  **          **  **    **  **        **   *
      *    ***       ******   **           ****      ****        ****  *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *     J O B   C O S T I N G   C A N C E L   J O B		       *
      *                                                                *
      *     VERSION 8.00 - August 2000				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JCP004.
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

000130 COPY CONTROL.SL.

000170 COPY DEBTOR.ISL.

000190 COPY DEPART.SL.

000270 COPY JOBCST.SL.

000270 COPY JOBHDRN.SL.

000210 COPY PARAM.SL.

000230 COPY RECOVER.SL.

000250 COPY SALE.SL.

       COPY SHARED.SL.

000310 COPY STOCK.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

000400 COPY DEBTOR.FDE.

000420 COPY DEPART.FDE.

000500 COPY JOBCSTN.FDE.

000500 COPY JOBHDRN.FDE.

000440 COPY PARAM.FDE.

000460 COPY RECOVER.GFD.

000480 COPY SALE.JFD.

       COPY SHARED.FDE.

000540 COPY STOCK.FDE.

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
000760 77  WS-DEBKEY       PIC  9(06)    COMP-5.
000770 77  WS-DEPKEY       PIC  9(04)    COMP-5.
000780 77  WS-KEYSTR       PIC  9(06)    COMP-5.
000800 77  WS-SALSTR	   PIC	9(04)	 COMP-5.
000810 77  WS-SALKEY       PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
000820 77  WS-NETKEY       PIC  9(04)    COMP-5.
000830 77  WS-AUDKEY       PIC  9(06)    COMP-5.
000840 77  WS-RECKEY       PIC  9(06)    COMP-5.
000850 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000870 77  WS-PRN1	   PIC	9(01) VALUE 0.
000880 77  WS-PRN2         PIC  9(01) VALUE 0.  
000890 77  WS-ADV-RATE     PIC  9(03)V99 COMP-3.
000900 77  WS-START        PIC  9(08).
000910 77  WS-END          PIC  9(08).
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
001250 77  WS-BATCH	   PIC	9(05).
001280 77  WS-A 	   PIC	X(19) VALUE "Any key to continue".
001290 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
001300 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
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
001450	       05  WS-MCHAR PIC  X(80) VALUE "³              ³   ³
      -        "                       ³        ³         ³          ³".
	   03  WS-MJOB REDEFINES WS-MID-LINE.
	       05  FILLER      PIC  X(01).
	       05  WS-JOB78    PIC  X(78).
	       05  FILLER      PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE SPACES.
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
      -        "³                               ³         ³          ³".
	   03  WS-RQ78-L REDEFINES WS-MD-LIN.
	       05  WS-RQ1      PIC  X(01).
	       05  WS-RQ78     PIC  X(78).
	       05  WS-RQ80     PIC  X(01).

001840 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "JCP".
	   03  WS-PROG	       PIC  X(03) VALUE "004".

001900 01  W00-TRAN-CODES.
001910     03  W00-T-VAT   PIC S9(09)V99 COMP-3.
001920     03  W00-E-DESC  PIC  X(12).
001930     03  W00-A-DESC  PIC  X(12).
001940     03  W00-ACTION  PIC  9(01).
001950     03  W00-T-CODE  PIC  9(02)    COMP-5.
001960     03  W00-T-GL    PIC  9(06)    COMP-3.
001970     03  W00-T-DAY   PIC  9(06)    COMP-5.
001980     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
001990     03  W00-T-MTD   PIC  9(06)    COMP-5.
002000     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
002010     03  W00-T-YTD   PIC  9(06)    COMP-5.
002020     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

002060 COPY CONTROL.ID.

002100 COPY DEBTOR.ID.

002120 COPY DEPART.ID.

002180 COPY JOBCST.ID.

002180 COPY JOBHDR.ID.

       COPY PARAM.ID.

002140 COPY RECOVER.ID.

002160 COPY SALE.ID.

       COPY SHARED.ID.

002220 COPY STOCK.ID.

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

003540 01  W15-DISCOUNT.
003550     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
             05  W15-TRAN OCCURS 15.
               07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
               07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
           03  W15-DISPLAY REDEFINES W15-DETAIL.
               05  W15-BAL1    PIC  X(11).
               05  W15-ALL1    PIC  X(11).
               05  W15-BAL2    PIC  X(11).
               05  W15-ALL2    PIC  X(11).
               05  W15-BAL3    PIC  X(11).
               05  W15-ALL3    PIC  X(11).
               05  W15-BAL4    PIC  X(11).
               05  W15-ALL4    PIC  X(11).
               05  W15-BAL5    PIC  X(11).
               05  W15-ALL5    PIC  X(11).
               05  W15-BAL6    PIC  X(11).
               05  W15-ALL6    PIC  X(11).
               05  W15-BAL7    PIC  X(11).
               05  W15-ALL7    PIC  X(11).
               05  W15-BAL8    PIC  X(11).
               05  W15-ALL8    PIC  X(11).
               05  W15-BAL9    PIC  X(11).
               05  W15-ALL9    PIC  X(11).
               05  W15-BAL10   PIC  X(11).
               05  W15-ALL10   PIC  X(11).
               05  W15-BAL11   PIC  X(11).
               05  W15-ALL11   PIC  X(11).
               05  W15-BAL12   PIC  X(11).
               05  W15-ALL12   PIC  X(11).
               05  W15-BAL13   PIC  X(11).
               05  W15-ALL13   PIC  X(11).
               05  W15-BAL14   PIC  X(11).
               05  W15-ALL14   PIC  X(11).
               05  W15-BAL15   PIC  X(11).
               05  W15-ALL15   PIC  X(11).

       COPY W20.WS.

003650 01  W25-CALCS.
003660     03  W25-RESULT      PIC 9(02)V99.
003670     03  W25-RESULT1 REDEFINES W25-RESULT.
003680         05  W25-WHOLE   PIC 9(02).
003690         05  W25-DECIMAL PIC 9(02).
003700     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
003710     03  W25-VALUE       PIC S9(07)V99 COMP-3.
003720     03  W25-CREDIT      PIC S9(07)V99 COMP-3.

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
	       05  FILLER     PIC  X(576).

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
005770     03  W95-V1.
005780         05  W95-T       PIC Z(04)9.99.
005790     03  W95-V2.
005800         05  W95-B       PIC Z(06)9.99-.
005810     03  W95-D.
005820	       05  W95-DTE     PIC Z9/99/9999.

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

       01  W200-MEMO.
           03  W200-MEM-FLDS. 
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
006010 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY STKLUP.CRT.

       COPY JOBLUP.CRT.

010020 01  S30.
010030     03  LINE  2 COLUMN 36 FOREGROUND-COLOR 15 
010040                           VALUE "JOB CARD".
010050     03  LINE  3 COLUMN  2 VALUE "Ac No".
010060     03          COLUMN 15 VALUE "Name".
010070	   03	       COLUMN 61 VALUE "Ord No°°°°°°°°°°°°°".
010080     03  LINE  4 COLUMN  2 VALUE "Tech.".
           03          COLUMN 61 VALUE "Tel.".
010090     03  LINE  5 COLUMN  2 VALUE "Job No".
           03          COLUMN 20 VALUE "Contact".
010100     03          COLUMN 67 VALUE "Date".
009370	   03  LINE  6 COLUMN  2 REVERSE-VIDEO VALUE "Item Code      I".
009380	   03	       COLUMN 18 REVERSE-VIDEO VALUE "TM Description  ".
009390     03          COLUMN 33 REVERSE-VIDEO VALUE "                ".
009400     03          COLUMN 49 REVERSE-VIDEO VALUE "  Quantity ".
009410     03          COLUMN 60 REVERSE-VIDEO VALUE "Unit Price  ".
009420     03          COLUMN 72 REVERSE-VIDEO VALUE "Extended".
009160	   03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
010190	   03  LINE 20 COLUMN  3 VALUE "Sub Total ".
010200	   03  LINE 21 COLUMN  3 VALUE "Discount  ".
010210     03  LINE 22 COLUMN  3 VALUE "T A X -VAT".
010230	   03  LINE 23 COLUMN  3 VALUE "AMOUNT DUE".

010240 01  S30A.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC Z(08) USING W10-JCST AUTO.

010290 01  S31A.
010300     03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15 
010310                           VALUE "CANCEL JOB".

010400 01  S36.
010410	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC X(06) USING W80-ACNO AUTO.

       COPY ERROR.CRT.

       COPY OPT.CRT.

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
015620	     MOVE "J"		 TO W90-ORG.
015630	     PERFORM BE000.
013040	     CLOSE RECOVER
		   SALE.
013140 AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010840 AA50J.
010850       MOVE "J"            TO WS-LOOK.
010910 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
011210	     PERFORM JOBHDR-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
011350       MOVE "S" TO WS-LOOK.
011360 AA99.
011370       EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

011390 COPY LOCKED.RC2.

       COPY OPTION.CRT.

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

011730 COPY DEPART.RD.

011750 COPY PARAM.RD.

011790 COPY JOBCST.RD.

011790 COPY JOBHDR.RD.

011770 COPY SALE.RD.

       COPY SHARED.RD.

011830 COPY STOCK.RD.

      /
011850 AD000-WRITE             SECTION.

011870 COPY CONTROL.WR.

011890 COPY PARAM.WR.

011930 COPY JOBCST.WR.

011930 COPY JOBHDR.WR.

011910 COPY SALE.WR.

011910 COPY STOCK.WR.

011990 COPY JOBCST.LUP.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
012010 AY000           SECTION.
011790 AY05.
011860       MOVE 05             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
011890         PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
011900       MOVE STK-RECORD1    TO REC-STOCKF.
011830       GO TO AY50.
012020 AY21.
012030	     MOVE 21		 TO REC-FILE.
012040       MOVE ZERO           TO REC-KEY.
012050       MOVE WS-ACTION      TO REC-TYPE.
012060	     MOVE JHD-REC1	 TO REC-JOBHDR.
012070       GO TO AY50.
012020 AY25.
012030	     MOVE 25		 TO REC-FILE.
012040       MOVE ZERO           TO REC-KEY.
012050       MOVE WS-ACTION      TO REC-TYPE.
012060       MOVE JOB-REC1       TO REC-JOBCST.
012070       GO TO AY50.
012080 AY39.
012090       MOVE 39             TO REC-FILE.
012100       MOVE WS-NETKEY      TO REC-KEY.
012110       MOVE NET-RECORD     TO REC-NETWORK.
012120       GO TO AY50.
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
012550       MOVE "DS"           TO PAR-PROG (WS-USUB).
012560	     MOVE LS-USER	 TO PAR-USR (WS-USUB).
012570       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012600	     MOVE 1		 TO WS-NETKEY.
012610       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012620       PERFORM AY39 THRU AY59.
012630       MOVE NET-DEBTOR     TO W70-DEBT.
012640       PERFORM AY39 THRU AY59.
012650       GO TO AY999.
012660 AY62.
012670     IF PAR-PROG (WS-SUB) = "DB" OR "DS"
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
012880 AY70.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
012890       MOVE 1              TO WS-NETKEY.
012900       MOVE W70-DEBT       TO NET-DEBTOR.
012910       PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
012920       PERFORM AY40 THRU AY59.
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
014680       MOVE 1              TO WS-LAST WS-SALKEY.
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
014580	     MOVE W10-JCST	 TO JOB-REF.
022140       MOVE 100            TO JOB-SQN.
014610	     PERFORM READ-JOBCST THRU READ-JOBCST-EXIT.
014680	     MOVE 1		 TO WS-LAST WS-SALKEY.
014620     IF WS-F-ERROR = 13
014640         GO TO GET-JOBCST-EXIT.
014690       GO TO GET-JCST-TEST-TYPE.

014710 GET-JCST-NEXT.
014720       PERFORM READ-JOBCST-NEXT THRU READ-JOBCST-EXIT.
014730     IF WS-F-ERROR = 13
014750         GO TO GET-JOBCST-EXIT.
	   IF JOB-REF NOT = W10-JCST
	       MOVE W10-JCST   TO JOB-REF
	       GO TO GET-JOBCST-TEST-EXIT.

014770 GET-JCST-TEST-TYPE.
      * 		     I T E M   D E T A I L S
014930     IF JOB-TYPE = 1
               PERFORM GET-CHECK-STOCK-FILE
               IF WS-F-ERROR = 0
014940             INITIALIZE SAL-RECORD1
		   MOVE "S"	 TO SAL-TYPE
014950		   MOVE JOB-EXT-ITEM
				 TO SAL-EXT-ITEM
014960		   MOVE JOB-DESC TO SAL-DESC
014970             MOVE JOB-QNT  TO SAL-QNT
014980             MOVE JOB-COST TO SAL-COST
014990             MOVE JOB-SELL TO SAL-SELL
015000             MOVE JOB-VAT  TO SAL-VAT
015010             MOVE JOB-VAL  TO SAL-VAL
015020             MOVE JOB-INCTAX
                                 TO SAL-INCTAX
015030             MOVE JOB-NONTAX
                                 TO SAL-NONTAX
015040             MOVE JOB-TAXVAL   
                                 TO SAL-TAXVAL
015050             MOVE JOB-DSC  TO SAL-DSC
015060             MOVE JOB-DISC TO SAL-DISC
015070             MOVE JOB-SQN  TO SAL-SQN
                   MOVE JOB-TECH TO SAL-TECH
		   MOVE JOB-VAT-SUB
				 TO SAL-VAT-SUB
		   IF SAL-VAT-SUB > ZERO
		       ADD JOB-TAXVAL
				 TO W90-TAXVAL(SAL-VAT-SUB)
023060		       MOVE W05-VAT(SAL-VAT-SUB)
023060				 TO W05-RATE
023080		       COMPUTE W90-VAT(SAL-VAT-SUB) ROUNDED =
			      (W90-TAXVAL(SAL-VAT-SUB) * W05-RTE)
		   END-IF
015080		   PERFORM GET-JCST-WRITE-SALE
               ELSE
                   PERFORM GET-CHANGE-TO-REMKS
015090     ELSE
      *                      P A R T S   D E T A I L S
015100     IF JOB-TYPE = 2
015110         INITIALIZE SAL-RECORD1
	       MOVE "T" 	 TO SAL-TYPE
014950	       MOVE JOB-EXT-ITEM TO SAL-EXT-ITEM
014960	       MOVE JOB-DESC	 TO SAL-DESC
014970         MOVE JOB-QNT      TO SAL-QNT
015140         MOVE JOB-SQN      TO SAL-SQN
015150*        PERFORM GET-JCST-WRITE-SALE
015160     ELSE
      *                      R E M A R K S
015170     IF JOB-TYPE = 3
015180         INITIALIZE SAL-RECORD1
	       MOVE "R" 	 TO SAL-TYPE
015190	       MOVE JOB-RMKS	 TO SAL-DESC
015200         MOVE JOB-SQN      TO SAL-SQN
015210         PERFORM GET-JCST-WRITE-SALE
           ELSE
      *                      I N T E R N A L   N O T E S
           IF JOB-TYPE = 4 
               INITIALIZE SAL-RECORD1
	       MOVE "I" 	 TO SAL-TYPE
	       MOVE JOB-RMKS	 TO SAL-DESC
               MOVE JOB-SQN      TO SAL-SQN
               MOVE 1            TO SAL-SUPER
	       PERFORM GET-JCST-WRITE-SALE.
015340	     GO TO GET-JCST-NEXT.

015360 GET-JCST-WRITE-SALE.
015370       MOVE WS-LAST        TO SAL-CODE.
015380       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
015390       ADD 1               TO WS-LAST WS-SALKEY.
015400 
       GET-CHECK-STOCK-FILE.
	     MOVE JOB-EXT-ITEM	 TO STK-CODE.
013560	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       GET-CHANGE-TO-REMKS.
             INITIALIZE SAL-RECORD1.
	     STRING JOB-EXT-ITEM DELIMITED BY SIZE
		    JOB-DESC DELIMITED BY SIZE
                                 INTO SAL-DESC.
             MOVE JOB-SQN        TO SAL-SQN W14-SQN (W14-SUB).
             ADD 1               TO W14-SUB.
             PERFORM GET-JCST-WRITE-SALE.

       GET-CH-TO-RMK-EXIT.
	     GO TO GET-JCST-NEXT.

       GET-JOBCST-TEST-EXIT.
	       IF NOT (W14-SUB > 1)
015330             GO TO GET-JOBCST-EXIT.

       GET-ADJUST-TOTALS.
             SUBTRACT 1 FROM W14-SUB
                                 GIVING WS-SUB.
             MOVE 1              TO W14-SUB.

       GET-ADJUST-TOTALS-LOOP.
             MOVE W14-SQN (W14-SUB)
                                 TO JOB-SQN.
             PERFORM READ-JOBCST-LOCK THRU READ-JOBCST-EXIT.
025000       SUBTRACT JOB-VAL    FROM W90-DB.
025010       SUBTRACT JOB-DISC   FROM W90-LDISC.
025020       SUBTRACT JOB-NONTAX FROM W90-NONTAX.
025030	     SUBTRACT JOB-TAXVAL FROM W90-EXCTOT.
025050       COMPUTE W90-BAL = W90-BAL - (JOB-COST * JOB-QNT).
             MOVE 3              TO JOB-TYPE.
	     STRING JOB-EXT-ITEM DELIMITED BY SIZE
		    JOB-DESC DELIMITED BY SIZE
                                 INTO JOB-DESC.
	     MOVE SPACES	 TO JOB-EXT-ITEM JOB-SERNO.
             MOVE ZERO           TO JOB-QNT JOB-COST JOB-INCTAX
                                    JOB-VAT JOB-SELL JOB-NONTAX
                                    JOB-VAL JOB-DISC JOB-TAXVAL
                                    JOB-DSC.
             PERFORM REWRITE-JOBCST-UNLOCK THRU WRITE-JOBCST-EXIT.
           IF W14-SUB < WS-SUB
               ADD 1             TO W14-SUB
               GO TO GET-ADJUST-TOTALS-LOOP.

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
      *    ****    C A N C E L   J O B
      *
033340 BE000	     SECTION 5.
033350 BE00.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
033360       MOVE 5              TO WS-PARKEY.
033370       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
033380       MOVE PAR-PW         TO W85-PASS. 
033390 BE05.
      *
      *    ****    J O B   C O S T I N G   S C R E E N
      *
033400       DISPLAY CLR-SCREEN.
033410       DISPLAY S30.
      *            C H A N G E   S C R E E N   H E A D I N G
033420       DISPLAY S31A.
033430       MOVE 8              TO LIN.
033440       PERFORM BE115 UNTIL LIN > 18.
033450       MOVE W12-TODAY      TO W90-DTE.
033460	     DISPLAY W90-DTE AT 0572
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033470       MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC 
033480				    W90-BAL W90-SALES
033490				    W90-CUR W90-EXCTOT.
033500       MOVE 1              TO WS-FIRST WS-LAST.
033510       MOVE ZERO           TO W10-JCST.
	     MOVE 1		 TO WS-S9.

       BE05A.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BE05A.

033520 BE10.
033530       PERFORM AA400.
033540       ACCEPT S30A AT 0510.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W10-JCST
		     GO TO BE599
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
      *
      *    ****    J O B   N U M B E R
      *
		 WHEN F2-KEY
		     MOVE "J"	 TO WS-ETYPE
      *
      *    ****    A C C O U N T   N U M B E R
      *
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
      *
      *    ****    I T E M   D E S C R I P T I O N
      *
		 WHEN F4-KEY
		     MOVE "I"	 TO WS-ETYPE
      *
      *    ****    C L I E N T	 N A M E
      *
		 WHEN F5-KEY
		     MOVE "N"	 TO WS-ETYPE
      *
      *    ****    C O N T A C T
      *
		 WHEN F6-KEY
		     MOVE "C"	 TO WS-ETYPE
      *
      *    ****    D A T E
      *
		 WHEN F7-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BE10
	       END-EVALUATE
016560	       PERFORM AA50J THRU AA50
016570	       DISPLAY S30A AT 0510
016580	       IF W10-JCST = ZERO
016590		   GO TO BE10.
033600	     DISPLAY CLEAR-L25.
033610     IF W10-JCST = ZERO
033620         GO TO BE599.
033630       MOVE W10-JCST       TO W90-REF.
      *
      *    ****    G E T   T H E   S E L E C T E D   J O B
      *
033640 BE12.
033650       PERFORM GET-JOBCST.
      *
      *    ****    C H E C K   F O R   E R R O R S
      *
033660	   IF WS-ERROR = 1
033670	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033680	       GO TO BE10.
033690     IF WS-ERROR = 2
033700	       MOVE "Not a job"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033710	       GO TO BE10.
033720     IF WS-ERROR = 3
033730	       MOVE "Job corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033750	       GO TO BE10.
033760     IF WS-ERROR = 4
033770	       MOVE "Invalid a/c no"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033780	       GO TO BE10.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
      *
      *    ****    D I S P L A Y   H E A D E R	 D E T A I L S
      *
033790	     DISPLAY S36 AT 0308
033800	     DISPLAY W40-NAME AT 0320
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033810	     DISPLAY W90-ORD AT 0368
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033820	     DISPLAY W10-SMAN AT 0411
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033830     IF W10-SMAN NOT = ZERO
033840         ADD 200 W10-SMAN    GIVING WS-PARKEY
033850         PERFORM READ-PARAM THRU READ-PARAM-EXIT
033860	       DISPLAY PAR-TECHNICIAN AT 0420
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0.
	     DISPLAY W40-TEL AT 0466
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
	     DISPLAY W40-CONTACT AT 0528
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033870	     MOVE 1		 TO WS-SALKEY.
033880       SUBTRACT 1          FROM WS-LAST.
      *
      *    ****    I T E M   L O C A T I O N
      *
	     DISPLAY "LOCATION:" AT 2039
		      W90-LOCATION AT 2049
		      WITH BACKGROUND-COLOR 5
			   FOREGROUND-COLOR 3 HIGHLIGHT.
      *
      *    ****    H A S   A N O T H E R   A C C O U N T   N U M B E R
      * 	   B E E N   R E C O R D E D   O N   T H I S   J O B
      *
	   IF W90-WCOMP = SPACES
	       GO TO BE15.
	     MOVE W90-WCOMP	 TO DEB-ACNO.
013640	     PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
	   IF NOT (WS-STAT1 = "0")
	       MOVE "INVALID ACCOUNT NUMBER"
				 TO W40-ANAME
	   ELSE
	       MOVE DEB-NAME	 TO W40-ANAME
016750	       IF DEB-TYPE = 1
016760		   INSPECT W40-ANAME REPLACING ALL "*" BY " "
016770		   INSPECT W40-ANAME REPLACING FIRST "!" BY " ".
      *
      *    ****    I S	 T H I S   A   W A R R A N T Y ,   R E P E A T
      * 	   O R	 G R O U P   A C C O U N T
      *
	   IF W90-PMETH = "W"
	       DISPLAY "W A R R A N T Y   A C C O U N T" AT 2139
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
	   ELSE
	   IF W90-PMETH = "RP" OR "CB"
	       DISPLAY "E X P E N S E   A C C O U N T" AT 2139
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
	   ELSE
	       DISPLAY "G R O U P   A C C O U N T" AT 2139
			WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     DISPLAY W40-ANAME AT 2239
			WITH FOREGROUND-COLOR 7 HIGHLIGHT.

033890 BE15.
033900       MOVE W90-DB         TO W90-TOTAL.
033910       SUBTRACT W90-LDISC  FROM W90-TOTAL.
033920       MOVE W90-DB         TO W100-BAL.
033930	     DISPLAY W100-BAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
033940       MULTIPLY -1 BY W90-LDISC 
033950                           GIVING W100-BAL.
033960	     DISPLAY W100-BAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
034000	   IF (W90-TAX = SPACES) OR
	      (W90-EXP = "C" OR "X")
034010	       GO TO BE20.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       BE16.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO BE16.
034060	     MOVE W25-TOTAL	 TO W100-BAL.
034080	     ADD W25-TOTAL	 TO W90-TOTAL.
034070	     DISPLAY W100-BAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.

034090 BE20.
034140	     MOVE W90-TOTAL	 TO W100-BAL.
034150	     DISPLAY W100-BAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
           IF W90-TOTAL NOT = ZERO
	       MOVE "May not delete this job"
				 TO WS-ERR-MES
	       MOVE SPACE	 TO WS-OPTION
	       PERFORM OPT-MESSAGE
               GO TO BE05.
      *
      *    ****    D I S P L A Y   S A L E S   D E T A I L S
      *
034160 BE25.
034170       MOVE 8              TO LIN.
034180	     PERFORM BE115 UNTIL LIN > 18.
034190       MOVE 8              TO LIN.
034200       MOVE 1              TO WS-LINES WS-FIRST.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
034210 BE30.
034480	     MOVE "'C'ancel job - 'E'xit  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
034950	   IF NOT (WS-OPTION = "C" OR "E")
034960	       GO TO BE30.
034970     IF WS-OPTION = "E"
034980         GO TO BE05.
      *
      *    ****    D E L E T E   R E C O R D S   F R O M   O R D E R
      *            F I L E
      *
035190 BE65.
035200	     DISPLAY "Deleting Job" AT 2520 WITH
035210		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
035220       MOVE ZERO           TO WS-SALKEY.
035230       PERFORM AY60 THRU AY999.
      *
      *    ****    U P D A T E	 J O B C S T   F I L E
      *
035240 BE70.
035250	     MOVE W90-REF	 TO JOB-REF.
035260       MOVE ZERO           TO JOB-SQN.
035270       PERFORM START-AT-JOB-NO THRU READ-JOBCST-EXIT.
035280     IF WS-F-ERROR = 13
035290         GO TO BE80.
      *
      *    ****    L O O P   A N D   D E L E T E   A L L   L I N E
      * 	   I T E M S   F O R   T H I S	 J O B
      *
035300 BE75.
035310       PERFORM READ-JOBCST-NEXT-LOCK THRU READ-JOBCST-EXIT.
035320     IF WS-F-ERROR = 13
	       UNLOCK JOBCST
035330         GO TO BE80.
035340     IF JOB-REF NOT = W90-REF
	       UNLOCK JOBCST
035350         GO TO BE80.
      *
      *    ****    I F	 T H I S   I S	 A   S T O C K	 I T E M
      * 	   U P D A T E	 T H E	 S T O C K   R E C O R D
      *
	   IF JOB-TYPE = 1
	       MOVE JOB-EXT-ITEM TO STK-CODE
	       MOVE 0		 TO WS-ACTION
               PERFORM AY05 THRU AY59
019830	       IF (STK-IND < 2) OR (STK-IND = 3)
		   SUBTRACT JOB-QNT
				 FROM STK-QJOB
		   ADD JOB-QNT	 TO STK-QUANT
	       END-IF
               PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
035360       MOVE 2              TO WS-ACTION.
035370	     PERFORM AY25 THRU AY59.
035380       PERFORM DELETE-JOBCST-REC-UNLOCK THRU WRITE-JOBCST-EXIT.
035390       GO TO BE75.
      *
      *    ****    D E L E T E	 T H E	 J O B	 H E A D E R
      * 	   R E C O R D
      *
035400 BE80.
035360	     MOVE 2		 TO WS-ACTION.
035370	     PERFORM AY21 THRU AY59.
035380	     PERFORM DELETE-JOBHDR-REC THRU WRITE-JOBHDR-EXIT.
035410       PERFORM AY70 THRU AY999.
035420	     CLOSE SALE.
035450       OPEN OUTPUT SALE.
035460       CLOSE SALE.
035470       OPEN I-O SALE.
035480       GO TO BE05.

035490 BE115.
028770	     DISPLAY (LIN, 1) "³" WITH BACKGROUND-COLOR 0
				       FOREGROUND-COLOR 3
			       WS-JOB78 WITH FOREGROUND-COLOR 3
			      "³" WITH BACKGROUND-COLOR 0
				       FOREGROUND-COLOR 3.
035510       ADD 1               TO LIN.

035520 BE599.
035530       EXIT.
      /
038140 ZA000-INIT	       SECTION 9.
038150 ZA000-OPEN.
038160	     PERFORM ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
038170	     DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.
038180       DISPLAY "JOB MAINTENANCE PROGRAM" AT 1229
038190                WITH FOREGROUND-COLOR 15.
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
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "JOBCST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBCST.
	     MOVE "JOBHDR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBHDR.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SALEJ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
038300	     MOVE 3		 TO WS-PARKEY.
038310       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
038430	     MOVE PAR-VAT-REG	 TO W05-VATNO.
038440       OPEN OUTPUT RECOVER.
038450       CLOSE RECOVER.
038460       OPEN I-O RECOVER.
038940	     OPEN OUTPUT SALE.
038950       CLOSE SALE.
038960       OPEN I-O SALE.
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
039500       MOVE PAR-TELEPHONE  TO W95-TEL.
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
040970         MOVE W02-NETWORK TO WS-FILE
040980         MOVE WS-NETKEY TO WS-KEY
041030	   ELSE
041040     IF WS-F-ERROR = 6
041050         MOVE W02-DEBTOR   TO WS-FILE
041060         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041070	   ELSE
041080     IF WS-F-ERROR = 7
041090         MOVE W02-DEPART   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
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
041110	   ELSE
041120     IF WS-F-ERROR = 15
041130         MOVE WS-PARID     TO WS-FILE
041140         MOVE WS-PARKEY    TO WS-KEY
041150     ELSE
041160     IF WS-F-ERROR = 18
041170         MOVE W02-RECOVER  TO WS-FILE
041180         MOVE WS-RECKEY    TO WS-KEY
041190     ELSE
041200     IF WS-F-ERROR = 19
041210         MOVE W02-SALE TO WS-FILE
041220         MOVE WS-SALKEY TO WS-KEY
041310	   ELSE
041320     IF WS-F-ERROR = 22
041330         MOVE W02-STOCKF   TO WS-FILE
041340         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 33
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.
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
