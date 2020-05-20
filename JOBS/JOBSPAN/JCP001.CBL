      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   ********   ******   *******      ****      ****       **     *
      *      **     **    **  **    **    **  **    **  **     ***     *
      *      **     **        **    **   **    **  **    **     **     *
      *      **     **        *******    **    **  **    **     **     *
      *      **     **        **         **    **  **    **     **     *
      *   ** **     **    **  **          **  **    **  **      **     *
      *    ***	     ******   **	   ****      ****     ******   *
      *                                                                *
      *     ENGLISH		    A D V A N C E D   V E R S I O N    *
      *                                                                *
      *     C R E A T E   N E W   J O B 			       *
      *                                                                *
      *     VERSION 8.00 - August 2000				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JCP001.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1992.

	   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 to 1999
					   by James William Lemmon.
	   All rights reserved.

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

000140 COPY DEBMEM.SL.

       COPY ITEMS.SL.

       COPY JCPHST.SL.

000270 COPY JOBCST.SL.

000270 COPY JOBHDRN.SL.

000270 COPY JOBRMK.SL.

000210 COPY PARAM.SL.

000230 COPY RECOVER.SL.

       COPY SHARED.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

000400 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

       COPY ITEMS.FDE.

       COPY JCPHST.FDE.

000500 COPY JOBCSTN.FDE.

000270 COPY JOBHDRN.FDE.

000500 COPY JOBRMKN.FDE.

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
000800 77  WS-SALSTR       PIC  9(04)    COMP-5.
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
000900 77  WS-START	   PIC	9(08).
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
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
001200 77  WS-TYPE	   PIC	X(01).
001210 77  WS-AGE          PIC  X(01).
001220 77  WS-PRC          PIC  X(01).
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
      -        "³                               ³         ³          ³".
	   03  WS-RQ78-L REDEFINES WS-MD-LIN.
	       05  WS-RQ1      PIC  X(01).
	       05  WS-RQ78     PIC  X(78).
	       05  WS-RQ80     PIC  X(01).

001840 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "JCP".
	   03  WS-PROG	       PIC  X(03) VALUE "001".

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

000140 COPY DEBMEM.ID.

       COPY ITEMS.ID.

       COPY JCPHST.ID.

002180 COPY JOBCST.ID.

000270 COPY JOBHDR.ID.

002180 COPY JOBRMK.ID.

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
003520	   03  W14-SUB	      PIC  9(03)     COMP-3.
003530	   03  W14-SQN	      PIC  9(05)     COMP-3 OCCURS 200.

003540 01  W15-DISCOUNT.
003550	   03  W15-DISC       PIC S9(03)V99 COMP-3 OCCURS 10.

       01  W15-PRICE-NAMES.
	   03  W15-PNAME.
	       05  W15-CHECK   PIC  X(01).
	       05  W15-REST    PIC  X(09).

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
             05  W15-TRAN OCCURS 15.
	       07  W15-VALUE  PIC  Z(06)9.99- BLANK WHEN ZERO.
	       07  W15-ALLOC  PIC  Z(06)9.99- BLANK WHEN ZERO.
           03  W15-DISPLAY REDEFINES W15-DETAIL.
	       05  W15-BAL1   PIC  X(11).
	       05  W15-ALL1   PIC  X(11).
	       05  W15-BAL2   PIC  X(11).
	       05  W15-ALL2   PIC  X(11).
	       05  W15-BAL3   PIC  X(11).
	       05  W15-ALL3   PIC  X(11).
	       05  W15-BAL4   PIC  X(11).
	       05  W15-ALL4   PIC  X(11).
	       05  W15-BAL5   PIC  X(11).
	       05  W15-ALL5   PIC  X(11).
	       05  W15-BAL6   PIC  X(11).
	       05  W15-ALL6   PIC  X(11).
	       05  W15-BAL7   PIC  X(11).
	       05  W15-ALL7   PIC  X(11).
	       05  W15-BAL8   PIC  X(11).
	       05  W15-ALL8   PIC  X(11).
	       05  W15-BAL9   PIC  X(11).
	       05  W15-ALL9   PIC  X(11).
	       05  W15-BAL10  PIC  X(11).
	       05  W15-ALL10  PIC  X(11).
	       05  W15-BAL11  PIC  X(11).
	       05  W15-ALL11  PIC  X(11).
	       05  W15-BAL12  PIC  X(11).
	       05  W15-ALL12  PIC  X(11).
	       05  W15-BAL13  PIC  X(11).
	       05  W15-ALL13  PIC  X(11).
	       05  W15-BAL14  PIC  X(11).
	       05  W15-ALL14  PIC  X(11).
	       05  W15-BAL15  PIC  X(11).
	       05  W15-ALL15  PIC  X(11).

       COPY W20.WS.

003650 01  W25-CALCS.
003660     03  W25-RESULT      PIC 9(02)V99.
003670     03  W25-RESULT1 REDEFINES W25-RESULT.
003680         05  W25-WHOLE   PIC 9(02).
003690         05  W25-DECIMAL PIC 9(02).
003700     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
003710     03  W25-VALUE       PIC S9(07)V99 COMP-3.
003720     03  W25-CREDIT      PIC S9(07)V99 COMP-3.

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
	   03  W30-PAYMENTS REDEFINES W30-P-CODES.
	       05  W30-PMETH   OCCURS 10 TIMES.
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
			       PIC  X(07).
                       09  FILLER
			       PIC  X(02).
004730		   07  FILLER  PIC  X(31).
003910	       05  FILLER      PIC  X(32).
003920     03  W40-NAME2 REDEFINES W40-NAME1.
003930	       05  W40-SURNAME PIC  X(24).
003940	       05  W40-FNAME   PIC  X(24).
003950	       05  W40-SNAME   PIC  X(24).
003960     03  W40-NAME3 REDEFINES W40-NAME1.
003970         05  W40-NCHAR OCCURS 72 PIC X(01).
003830	   03  W40-NME.
003840	       05  W40-NCH     PIC  X(01) OCCURS 40.
003980	   03  W40-ANAME       PIC  X(40).
           03  W40-CONTACT     PIC  X(30).
003990     03  W40-DEBTREC.
004000	       05  FILLER      PIC  X(580).
004010     03  W40-ADD REDEFINES W40-DEBTREC.
004020       05  W40-ADDRESS1.
004030	       07  W40-ADD1    PIC  X(30).
004040	       07  W40-ADD2    PIC  X(30).
004050	       07  W40-ADD3    PIC  X(30).
004060	       07  W40-ADD4    PIC  X(30).
004070       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
004080         07  W40-ACHAR OCCURS 120 PIC X(01).
004090	     05  W40-PC1       PIC  X(04).
004100       05  W40-PADDRESS.
004110	       07  W40-PADD1   PIC  X(30).
004120	       07  W40-PADD2   PIC  X(30).
004130	       07  W40-PADD3   PIC  X(30).
004140	       07  W40-PADD4   PIC  X(30).
004150       05  W40-PADDR2   REDEFINES W40-PADDRESS.
004160         07  W40-PCHAR OCCURS 120 PIC X(01).
004170	     05  W40-PC2       PIC  X(04).
	     05  W40-TERMS     PIC  X(30).
	     05  W40-SPACES    PIC  X(30).
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
004190	   03  W40-POST        PIC  9(04).
004200	   03  W40-PPOST       PIC  9(04).
004210	   03  W40-TEL	       PIC  X(14).
004210	   03  W40-FAX	       PIC  X(14).
004220	   03  W40-HTEL        PIC  X(14).
004230     03  W40-ID.
004240	       05  W40-IDNO    PIC  X(14).
004250     03  W40-TAX REDEFINES W40-ID.
004260	       05  W40-TAXNO   PIC  X(14).
004270	   03  W40-TITLE       PIC  9(02).
004280	   03  W40-LANG        PIC  X(01).
004290	   03  W40-SMAN        PIC  9(02).
	   03  W40-LOC	       PIC  X(05).
           03  W40-STATUS      PIC  X(01).
           03  W40-CUS-DET.
               05  W40-NM1     PIC  X(40).
               05  W40-AD1     PIC  X(30).
               05  W40-AD2     PIC  X(30).
               05  W40-AD3     PIC  X(30).
               05  W40-AD4     PIC  X(30).
               05  W40-PC      PIC  X(04).
           03  W40-DTE         PIC  9(06) COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

004540 COPY W50.WS.

004560 01  W60-NME-ADD.
004570     03  W60-NAME.
004580         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
	       05  W60-ADD     PIC  X(60).
	       05  FILLER      PIC  X(10).
003600	   03  W60-ADDRESS1.
	       05  W60-PAD     PIC  X(60).
	       05  FILLER      PIC  X(10).

004740 COPY W70DEBT.IWS.

004760 01  W75-KEYS.
004770	   03  W75-S	      PIC  9(02) COMP-5.
004780	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
004790	   03  W75-DNO	 OCCURS 18.
	       05  W75-TECH   PIC  9(02) COMP-5.
	       05  FILLER     PIC  X(05).
	   03  W75-JOBS.
004810	       05  W75-JCST   PIC  9(08) OCCURS 18.
           03  W75-TYPES REDEFINES W75-JOBS.
	       05  W75-JREM   PIC  X(06) OCCURS 18.
               05  FILLER     PIC  X(36).
	   03  W75-ITEM       PIC  X(24) OCCURS 12.

004820 01  W75-TITLES.
004830     03  W75-DESCRIP.
004840	       05  FILLER     PIC  X(28)      VALUE
004850                     "Mr     Mnr    Mrs    Mev    ".
004860	       05  FILLER     PIC  X(28)      VALUE
004870                     "Miss   Mej    Ms     Ms     ".
004880	       05  FILLER     PIC  X(28)      VALUE
004890                     "Dr     Dr     Rev    Ds     ".
004900	       05  FILLER     PIC  X(28)      VALUE
004910                     "Prof   Prof   The HonSy Edel".
004920	       05  FILLER     PIC  X(28)      VALUE
004930                     "                            ".
004940     03  W75-CODES REDEFINES W75-DESCRIP.
004950         05  W75-TITLE  OCCURS 10.
004960		   07  W75-ETITLE PIC  X(07).
004970		   07  W75-ATITLE PIC  X(07).

004980 01  W80-ACCOUNT.
004990	   03  W80-ACNO 	 PIC  X(06).
005000     03  W80-CRED REDEFINES W80-ACNO.
005010	       05  W80-CNO	 PIC  9(05).
005020         05  W80-CAC REDEFINES W80-CNO.
005030		   07  W80-CTYPE PIC  9(01).
005040		   07  W80-SER	 PIC  9(04).
005050	       05  FILLER	 PIC  9(01).
005060	   03  W80-TYPE 	 PIC  9(01).

005070 01  W85-PASS.
005080     03  W85-SUPER       PIC  X(06)    OCCURS 9.
005090     03  W85-ENTRY       PIC  9(02)    COMP-3.
005100	   03  W85-MARG        PIC S9(02)V99.

       01  W85-JCP-PASS.
	   03  W85-JPW	       PIC  X(06).
	   03  W85-JPWC REDEFINES W85-JPW
			       PIC  X(01) OCCURS 6.
	   03  W85-JPW2        PIC  X(06).
	   03  W85-JPWC2 REDEFINES W85-JPW2
			       PIC  X(01) OCCURS 6.
	   03  W85-ENCRYPT REDEFINES W85-JPW2.
	       05  W85-ENC1    PIC  X(01) COMP-X.
	       05  W85-ENC2    PIC  X(01) COMP-X.
	       05  W85-ENC3    PIC  X(01) COMP-X.
	       05  W85-ENC4    PIC  X(01) COMP-X.
	       05  W85-ENC5    PIC  X(01) COMP-X.
	       05  W85-ENC6    PIC  X(01) COMP-X.

005110 01  W85-BATCHES.
005120         05  W85-BATCH   PIC  9(05).

       COPY W90.JCP.

       01  W92-LABELS.
	   03  W92-JOB	       PIC  X(08).
	   03  W92-ITEM        PIC  X(40).
	   03  W92-NAME        PIC  X(40).
	   03  W92-ACNO        PIC  X(06).
	   03  W92-DATE        PIC  9(08).
	   03  W92-NO	       PIC  9(02).

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
005820         05  W95-DTE     PIC Z9/99/99.

005830 01  W100-EDIT.
005840	   03  W100-LBAL       PIC  Z(06)9.99-.
005850	   03  W100-LARR       PIC  Z(06)9.99.
005860	   03  W100-BAL        PIC  Z(08)9.99-.
005870	   03  W100-CUR        PIC  Z(08)9.99-.
005880	   03  W100-30	       PIC  Z(08)9.99-.
005890	   03  W100-60	       PIC  Z(08)9.99-.
005900	   03  W100-90	       PIC  Z(08)9.99-.
005910	   03  W100-120        PIC  Z(08)9.99-.
005920	   03  W100-INT        PIC  Z(08)9.99-.
005930	   03  W100-CRL        PIC  Z(06)9.
005940     03  W100-V2.
005950	       05  W100-S6V2   PIC  Z(05)9.99-.
005960     03  W100-V3 REDEFINES W100-V2.
005970	       05  W100-PRICE  PIC  X(09).
005980	       05  FILLER      PIC  X(01).
005990     03  W100-V4 REDEFINES W100-V2.
006000	       05  W100-EXT    PIC  Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
	       05  W100-QNT    PIC  Z(08)9-.
	   03  W100-DTE REDEFINES W100-V2.
	       05  W100-DATE   PIC  ZZ99/99/99.

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

006620 01  S05.
006630	   03  LINE 12 COLUMN 12 VALUE "Enter Password".

006640 01  S06.
006650	   03  LINE 12 COLUMN 28 FOREGROUND-COLOR 15
				 BACKGROUND-COLOR 0 PIC	X(06)
006660                           USING W40-CSHPASS AUTO SECURE.

       01  S07.
	   03  LINE 12 COLUMN 52 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC Z9
				 USING WS-INO AUTO.

       COPY JRMLUP.CRT.

       COPY DEBLUP.CRT.

       COPY ITMLUP.CRT.

       COPY TECHNIC.CRT.

008990 01  S25.
	   03  LINE  7 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
009000	   03  LINE  8 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				 "³Addresses:                 ".
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Balance  ".
009010	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-BAL.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009030	   03  LINE  9 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ 1 ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  MTD      ".
009040	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING W100-CUR.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009060	   03  LINE 10 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Current  ".
009070	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-CUR.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009090	   03  LINE 11 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD3.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   30 Days ".
009100	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-30.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009120	   03  LINE 12 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD4.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   60 Days ".
009130	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-60.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009150	   03  LINE 13 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PC1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   90 Days ".
009160	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-90.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009180	   03  LINE 14 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ 2 ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  120 Days ".
009190	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-120.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009210	   03  LINE 15 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Interest ".
009220	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-INT.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009240	   03  LINE 16 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD3.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Cr Limit ".
009250	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9 USING DEB-CRL.
009090	   03	       COLUMN 69 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "    ³".
	   03  LINE 17 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD4.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Last Pmt ".
	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC 9999/99/99 USING DEB-PMT.
009090	   03	       COLUMN 70 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "     ³".
	   03  LINE 18 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PC2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "       Amt ".
	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-LAST-PMT.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
	   03  LINE 19 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S29.
           03  LINE  2 COLUMN 31 FOREGROUND-COLOR 15
                                 VALUE "CREATE A JOB RECORD".
	   03  LINE  4 COLUMN  3 VALUE "Date:".
	   03  LINE  5 COLUMN  3 VALUE "Account No".
	   03  LINE  6 COLUMN  3 VALUE "Name      ".
	   03  LINE  7 COLUMN  3 VALUE "Telephone ".
	   03	       COLUMN 31 VALUE "Contact".
	   03  LINE  8 COLUMN  3 VALUE "Cust Ref  ".
	   03  LINE  9 COLUMN  3 VALUE "Item/Model".
	   03  LINE 10 COLUMN  3 VALUE "Serial No.".
	   03  LINE 11 COLUMN  3 VALUE "Remarks 1 ".
	   03  LINE 12 COLUMN  3 VALUE "        2 ".
	   03  LINE 13 COLUMN  3 VALUE "        3 ".
	   03  LINE 14 COLUMN  3 VALUE "        4 ".
	   03  LINE 15 COLUMN  3 VALUE "Inv. Type ".
	   03  LINE 16 COLUMN  3 VALUE "Warr.Debt.".
	   03  LINE 17 COLUMN  3 VALUE "Technician".
	   03  LINE 18 COLUMN  3 VALUE "Use Price ".
	   03	       COLUMN 17 VALUE "(".
	   03	       COLUMN 18 FOREGROUND-COLOR  6 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 19 FOREGROUND-COLOR  7 VALUE "=".
	   03	       COLUMN 20 FOREGROUND-COLOR  3
				 PIC  X(10) FROM WS-RT-HD.
	   03	       COLUMN 30 FOREGROUND-COLOR 7 VALUE ",".
	   03	       COLUMN 31 FOREGROUND-COLOR  6 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 FOREGROUND-COLOR  7 VALUE "=".
	   03	       COLUMN 33 FOREGROUND-COLOR  3
				 PIC  X(10) FROM WS-CS-HD.
	   03	       COLUMN 43 FOREGROUND-COLOR 7 VALUE ",".
	   03	       COLUMN 44 FOREGROUND-COLOR  6 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 45 FOREGROUND-COLOR  7 VALUE "=".
	   03	       COLUMN 46 FOREGROUND-COLOR  3
				 PIC  X(10) FROM WS-WS-HD.
	   03	       COLUMN 56 VALUE ")".
	   03  LINE 19 COLUMN  3 VALUE "V.A.T.    ".
	   03	       COLUMN 17 VALUE "(".
           03          COLUMN 18 FOREGROUND-COLOR 14 VALUE "I".
	   03	       COLUMN 19 FOREGROUND-COLOR  7
                                 VALUE "nclusive or ".
           03          COLUMN 31 FOREGROUND-COLOR 14 VALUE "E".
	   03	       COLUMN 32 FOREGROUND-COLOR  7 VALUE "xclusive".
	   03	       COLUMN 40 VALUE ")".
	   03  LINE 20 COLUMN  3 VALUE "Location".
	   03  LINE 21 COLUMN  3 VALUE "Priority".
	   03  LINE 22 COLUMN  3 VALUE "JOB NO:".

       01  S29A.
           02  S29-COD.
	   03  LINE  6 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
                                 PIC X(40) USING W40-NAME AUTO.

	   03  S29-TEL.
	   05  LINE  7 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
                                 PIC X(14) USING W40-HTEL AUTO.

           03  S29-CONTACT.
	   05  LINE  7 COLUMN 39 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
                                 PIC  X(30) USING W40-CONTACT AUTO.

           02  S29-DEBT.
	   03  LINE  8 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
                                 PIC X(12) USING W90-ORD AUTO.

	   02  S29-MODEL.
	   03  LINE  9 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC  X(24) USING W90-MODEL AUTO.

	   02  S29-ITEM.
	   03  LINE  9 COLUMN 39 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC  X(40) USING W90-IDESC AUTO.

	   02  S29-SERIAL.
	   03  LINE 10 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC  X(15) USING W90-SERIAL AUTO.

	   02  S29-REMARKS.
	   03  LINE 11 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(40) USING W90-REMK1 AUTO.
	   03  LINE 12 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(40) USING W90-REMK2 AUTO.
	   03  LINE 13 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(40) USING W90-REMK3 AUTO.
	   03  LINE 14 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(40) USING W90-REMK4 AUTO.

	   02  S29-PAYMENT.
	   03  LINE 15 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(02) USING W90-PMETH AUTO.

	   02  S29-WDEBT.
	   03  LINE 16 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC  X(06) USING W90-WAR-AC AUTO.

	   02  S29-TECH.
	   03  LINE 17 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC Z9 USING W90-TECH AUTO.

	   02  S29-PRICE.
	   03  LINE 18 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
                                 PIC X(01) USING WS-PRC AUTO.

	   02  S29-VAT.
	   03  LINE 19 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
                                 PIC X(01) USING W90-EXP AUTO.

	   02  S29-LOCATION.
	   03  LINE 20 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC  X(05) USING W40-LOC AUTO.

	   02  S29-PRIORITY.
	   03  LINE 21 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC Z9 USING W90-PRIORITY AUTO.

010400 01  S36.
010410	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC X(06) USING W80-ACNO AUTO.

       01  S40.
	   03  LINE  8 COLUMN 15 FOREGROUND-COLOR 14 VALUE "Ú".
           03          COLUMN 16 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "¿".
007450	   03  LINE  9 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 34 FOREGROUND-COLOR 6
                                 VALUE "CLIENT DETAILS".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 10 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 16 FOREGROUND-COLOR 6
                                 VALUE "Name    >".
           03          COLUMN 65 FOREGROUND-COLOR 6
                                 VALUE "<".     
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 11 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 16 FOREGROUND-COLOR 6
                                 VALUE "Address >".
           03          COLUMN 55 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 12 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 24 FOREGROUND-COLOR 6
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 13 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 24 FOREGROUND-COLOR 6
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 14 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 24 FOREGROUND-COLOR 6
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 15 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 24 FOREGROUND-COLOR 6
                                 VALUE ">".
           03          COLUMN 29 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 16 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 16 FOREGROUND-COLOR 6
                                 VALUE "Tel.    >".
           03          COLUMN 39 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 17 COLUMN 15 FOREGROUND-COLOR 14 VALUE "³".
           03          COLUMN 16 FOREGROUND-COLOR 6
                                 VALUE "Id.No.  >".
           03          COLUMN 39 FOREGROUND-COLOR 6
                                 VALUE "<".
007460	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
	   03  LINE 18 COLUMN 15 FOREGROUND-COLOR 14 VALUE "À".
           03          COLUMN 16 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 FOREGROUND-COLOR 14 VALUE "Ù".

       01  S41.
           03  LINE 10 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(40) USING W40-NM1 AUTO.
           03  LINE 11 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD1 AUTO.
           03  LINE 12 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD2 AUTO.
           03  LINE 13 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD3 AUTO.
           03  LINE 14 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD4 AUTO.
           03  LINE 15 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(04) USING W40-PC AUTO.
           03  LINE 16 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(14) USING W40-HTEL AUTO.
           03  LINE 17 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(14) USING W40-ID AUTO.

       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ú".
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "¿".
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / S P E C I F I C A T I O N S    
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Æ".
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "µ".
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "À".
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "Ù".

       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

       COPY S99.CRT.

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
015630	     PERFORM BC000.
013040	     CLOSE ITEMS
		   RECOVER.

013140 AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010810 AA50D.
010820       MOVE "D"            TO WS-LOOK.
010830       GO TO AA50.

       AA50I.
	     MOVE "I"		 TO WS-LOOK.
010830       GO TO AA50.

       AA50R.
	     MOVE "R"		 TO WS-LOOK.
010830       GO TO AA50.

010910 AA50.
011050	     PERFORM SAVE-SCREEN.
	   IF NOT (WS-LOOK = "I")
011080	       MOVE 4		 TO W44-FUNCTION
011090	       PERFORM SCREEN-CONTENTS.
011170     IF WS-LOOK = "D"
011180         PERFORM DEBTOR-LOOKUP
	   ELSE
	   IF WS-LOOK = "I"
	       PERFORM ITEMS-LOOKUP
	   ELSE
	       PERFORM JOBRMK-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
	     MOVE SPACE 	 TO WS-LOOK.

011360 AA99.
011370       EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

011670 COPY CONTROL.RD.

011710 COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

       COPY ITEMS.RD.

       COPY JCPHST.RD.

011790 COPY JOBCST.RD.

000270 COPY JOBHDR.RD.

011790 COPY JOBRMK.RD.

011750 COPY PARAM.RD.

       COPY SHARED.RD.

      /
011850 AD000-WRITE             SECTION.

011870 COPY CONTROL.WR.

011890 COPY PARAM.WR.

       COPY ITEMS.WR.

011930 COPY JCPHST.WR.

011930 COPY JOBCST.WR.

000270 COPY JOBHDR.WR.

011970 COPY DEBTOR.ILU.

       COPY ITEMS.LUP.

011990 COPY JOBRMK.LUP.

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

011070 AY058.
011080	     MOVE 58		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE HST-RECORD1	 TO REC-JCPHST.
011110       GO TO AY50.

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
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012600	     MOVE 1		 TO WS-NETKEY.
012610       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012620       PERFORM AY39 THRU AY59.
012630       MOVE NET-DEBTOR     TO W70-DEBT.
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

012880 AY70.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
012890	     MOVE 1		 TO WS-NETKEY.
012900       MOVE W70-DEBT       TO NET-DEBTOR.
012910       PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
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
      *
013750 CA165-NAME        SECTION 2.
013760 CA165.
013770       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
013780                           INTO W40-SURNAME 
013790                                W40-FNAME 
013800                                W40-SNAME
013810       OVERFLOW
013820           MOVE 1          TO WS-ERROR.
013830 CA170-EXIT.
013840       EXIT.
      *
014530 CA175-ADDRESS	 SECTION 2.
014540 CA175.
014550       UNSTRING W60-ADDRESS DELIMITED BY "*"
014560				 INTO W40-AD1
				      W40-AD2
				      W40-AD3
				      W40-AD4
014570       OVERFLOW
014580           MOVE 1          TO WS-ERROR.
014590 CA175-EXIT.
014600       EXIT.
      *
014150 CA185-ADDRESS	 SECTION 2.
014160 CA185.
014170       UNSTRING W60-ADDRESS1 DELIMITED BY "*"
014180                           INTO W40-PADD1 
014190                                W40-PADD2 
014200                                W40-PADD3 
014210                                W40-PADD4
014220       OVERFLOW
014230           MOVE 1          TO WS-ERROR.
014240 CA185-EXIT.
014250       EXIT.

      /
       BB000		   SECTION 3.
       BB00.
           IF DEB-NCASH = "CASH SALE" OR "KONTANT V"
               GO TO BB06.
       BB05.
	     MOVE "Enter client details Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO BB05.
           IF WS-OPTION = "N"
               GO TO BB20.
       BB06.
	     DISPLAY CLEAR-L25.
	     PERFORM SAVE-SCREEN.
             MOVE SPACES         TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-HTEL
                                    W40-ID  W40-PC.
             MOVE 0815           TO CPOS.
       BB10.
             DISPLAY 
                 "                                                    "
                 AT CPOS.
           IF CLIN < 18
               ADD 1             TO CLIN
               GO TO BB10.
             DISPLAY S40. 
       BB15.
             ACCEPT S41.
           IF DEB-NCASH = "CASH SALE" OR "KONTANT V"
               IF (W40-NM1 = SPACES) OR
                  (W40-AD1 = SPACES) OR
                  (W40-HTEL = SPACES)
		   MOVE "Require Name, Address & Telephone"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
                   GO TO BB15.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
               GO TO BB15.
           IF DEB-NCASH = "CASH SALE" OR "KONTANT V"
               MOVE W40-NM1      TO W40-NAME
               MOVE W40-AD1      TO W40-ADD1
               MOVE W40-AD2      TO W40-ADD2
               MOVE W40-AD3      TO W40-ADD3
               MOVE W40-AD4      TO W40-ADD4
               MOVE W40-PC       TO W40-PC1.

       BB20.
	     DISPLAY CLEAR-L25.

       BB29.
             EXIT.
      /
015800 BB100		   SECTION 3.
016070 BB106.
016080	     DISPLAY "Enter password" AT 2551
016090		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
016100	     ACCEPT W40-CSHPASS AT 2567
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
016110			 AUTO NO-ECHO.
016120       MOVE 6 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016180	     MOVE 1 TO WS-LENGTH.
       BB107.
           IF W40-CSHPASS = "!@$^*)"
               MOVE 10           TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (1)
               MOVE 1            TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (9)
               MOVE 9            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
           GO TO BB110.
       BB108.
           IF W40-CSHPASS = "!#%&()"
               MOVE 10           TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (1)
               MOVE 1            TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (6)
               MOVE 6            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
018480 BB110.
018490       EXIT.
      /
      *    ****    N E W   J O B S
      *
016320 BC000	     SECTION 50.
016330 BC00.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
016340       MOVE 5              TO WS-PARKEY.
016350       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016360       MOVE PAR-PW         TO W85-PASS. 
016370 BC05.
      *
      *    ****    N E W   J O B   S C R E E N
      *
016380       DISPLAY CLR-SCREEN.
016390       DISPLAY S29.
016420       MOVE W12-TODAY      TO W90-DTE.
016430	     DISPLAY W90-DTE AT 0414
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
016440       MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC 
016450				    W90-BAL W90-SALES
016460				    W90-CUR W90-EXCTOT.
016470       MOVE 1              TO WS-FIRST WS-LAST.
016480	     MOVE SPACES	 TO W90-TAX  W40-PADDRESS
				    W90-ORD  W90-IDESC
				    W10-JREM W90-REMARKS
				    W90-WAR-AC.
      *
      *    ****    D E B T O R   A C C O U N T   N U M B E R
      *
016490 BC10.
016500	     MOVE SPACES	 TO W80-ACNO.

016510 BC11.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Debtor Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12
		     "=Create,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
016540	     ACCEPT S36 AT 0514.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO BC599
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50D THRU AA50
		 WHEN F8-KEY
003240		     IF LS0-DBLEV > 0
			 PERFORM GA000
			 MOVE LS0-DBT
				 TO W80-ACNO
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC11
	       END-EVALUATE
016570	       DISPLAY S36 AT 0514
016580	       IF W80-ACNO = SPACES
016590		   GO TO BC11.
016610	   IF W80-ACNO = SPACES
016620         GO TO BC599.

016630 BC12.
016640	     DISPLAY CLEAR-L25.
016650	     PERFORM CA155-GET-DEBTOR.
016660     IF WS-ERROR = 1
016670	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
016680         GO TO BC11.
016690     IF WS-ERROR = 2
016700	       MOVE "Invalid account"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
016720         GO TO BC11.

016730 BC15.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
016740       MOVE DEB-NAME       TO W40-NAME.
	   IF W80-TYPE = 1
	       MOVE DEB-H-TEL	 TO W40-HTEL
	   ELSE
	       MOVE DEB-B-TEL	 TO W40-HTEL.
	     MOVE DEB-CONTACT	 TO W40-CONTACT.
	     MOVE DEB-PRICE	 TO WS-PRC.
	     MOVE DEB-INV-TYPE	 TO W90-EXP.
016750     IF W80-TYPE = 1
016760         INSPECT W40-NAME REPLACING ALL "*" BY " "
016770         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
016780       DISPLAY S29-COD.
025980	     MOVE SPACES	 TO W40-PC W40-PC2.
025990       MOVE DEB-ADDRESS    TO W60-ADDRESS.
026000       PERFORM CA175-ADDRESS.
	     MOVE W40-AD1	 TO W40-ADD1.
	     MOVE W40-AD2	 TO W40-ADD2.
	     MOVE W40-AD3	 TO W40-ADD3.
	     MOVE W40-AD4	 TO W40-ADD4.
	     MOVE W40-PC	 TO W40-PC1.
026010     IF NOT (DEB-POST = ZERO OR 9999)
026020         MOVE DEB-POST     TO W40-PC1.
026030     IF W40-ADD4 = SPACES
026040         MOVE W40-PC1      TO W40-ADD4
026050         MOVE SPACES       TO W40-PC1.
026060     IF W40-ADD3 = SPACES
026070         MOVE W40-ADD4     TO W40-ADD3
026080         MOVE SPACES       TO W40-ADD4.
026090     IF W40-ADD2 = SPACES
026100         MOVE W40-ADD3     TO W40-ADD2
026110         MOVE SPACES       TO W40-ADD3.
026120     IF W40-ADD1 = SPACES
026130         MOVE W40-ADD2     TO W40-ADD1
026140         MOVE SPACES       TO W40-ADD2.
026150       MOVE DEB-PADDR      TO W60-ADDRESS1.
026160       PERFORM CA185-ADDRESS.
026170     IF NOT (DEB-PPOST = ZERO OR 9999)
026180         MOVE DEB-PPOST    TO W40-PC2.
026190     IF W40-PADD4 = SPACES
026200         MOVE W40-PC2      TO W40-PADD4
026210         MOVE SPACES       TO W40-PC2.
026220     IF W40-PADD3 = SPACES
026230         MOVE W40-PADD4    TO W40-PADD3
026240         MOVE SPACES       TO W40-PADD4.
026250     IF W40-PADD2 = SPACES
026260         MOVE W40-PADD3    TO W40-PADD2
026270         MOVE SPACES       TO W40-PADD3.
026280     IF W40-PADD1 = SPACES
026290         MOVE W40-PADD2    TO W40-PADD1
026300         MOVE SPACES       TO W40-PADD2.
      *
      *    ****    I S   T H I S   T H E   C O R R E C T   A C C O U N T
      *
	     PERFORM BC18-MEMO THRU BC18E.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 23		 TO SHADE-COL.
	     MOVE 51		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
019090	     DISPLAY S25.
	     PERFORM SCREEN-SHADOW.
021280	     DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
021290	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
011320	     PERFORM RESTORE-SCREEN-2.
      *
      *    ****   C A T E G O R Y   8 0   -  S U S P E N D E D   A / C
      *
           IF DEB-CAT = 80
	       MOVE "This account has been suspended"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC05.
032920	     MOVE "'C'ontinue, 'R'ekey, 'E'xit  [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.

016890 BC18.
	     PERFORM OPT-MESSAGE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC00
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BC18.
016970	   IF NOT (WS-OPTION = "C" OR "E" OR "R")
016980         GO TO BC18.
	     GO TO BC20.

       BC18-MEMO.
019250	     DISPLAY CLEAR-L25.
             MOVE W80-ACNO       TO DME-AC.
             MOVE 1              TO DME-SEQ.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO BC18E.
             INITIALIZE W200-MEM-FLDS.

       BC18A.
             MOVE DME-SEQ        TO WS-S1.
             MOVE DME-DATA       TO W200-MEM (WS-S1).
             PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
                   GO TO BC18A.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     DISPLAY S-DEB-MEM.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY SA-MEM.
021280       DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
021290	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     PERFORM RESTORE-SCREEN-3.

       BC18E.
             EXIT.

017010 BC20.
017020	     DISPLAY CLEAR-L25.
017030     IF WS-OPTION = "R"
017040         GO TO BC05.
017050     IF WS-OPTION = "E"
017060         GO TO BC599.
017070       MOVE "Y"            TO WS-OPTION.
      *
      *    ****    C H E C K   A R R E A R S   &   C R E D I T   L I M I T
      *
017080     IF (DEB-60 > ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO)
019360	       MOVE "Arrears > 30 days - Override Y/N"
				 TO WS-ERR-MES
	       MOVE SPACE	 TO WS-OPTION
	       PERFORM OPT-MESSAGE
017180	       IF WS-OPTION = "Y"
017190             MOVE ZERO     TO WS-ERROR
017200             PERFORM BB106
017200             PERFORM BB108
017210             IF WS-ERROR = 1
017220                 MOVE "N"  TO WS-OPTION.                       
017240	   IF NOT (WS-OPTION = "Y")
017250         GO TO BC05.
017260     IF DEB-CRL > ZERO
017270         IF DEB-BAL > DEB-CRL
019550		   MOVE "Credit limit exceded - Override Y/N"
				 TO WS-ERR-MES
		   MOVE SPACE	 TO WS-OPTION
		   PERFORM OPT-MESSAGE
017370		   IF WS-OPTION = "Y"
017380		       MOVE ZERO TO WS-ERROR
017200		       PERFORM BB106
017390		       PERFORM BB108
017400		       IF WS-ERROR = 1
017410			   MOVE "N"
				 TO WS-OPTION.
019700	   IF NOT (WS-OPTION = "Y")
017440	       GO TO BC05.
	   IF (DEB-TERMS = 3) AND
	      (DEB-NCASH = "CASH SALE" OR "KONTANT V")
               MOVE SPACES       TO W40-NAME W40-HTEL.
      *
      *    ****    I F	 C O D	 A C C O U N T	 -   G E T   C L I E N T
      * 	   D E T A I L S
      *
       BC21.
           IF DEB-TERMS = 3
	       ACCEPT S29-COD
	       IF USER-FUNC
		   EVALUATE KEY-CODE-1
		     WHEN ESC-KEY
			 MOVE SPACES TO W80-ACNO
			 GO TO BC00
		   END-EVALUATE.

       BC21A.
	     ACCEPT S29-TEL.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC11
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC21A
	       END-EVALUATE.

       BC21AB.
	     ACCEPT S29-CONTACT.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC21A
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC21AB
	       END-EVALUATE.

       BC21B.
	     ACCEPT S29-DEBT.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC21AB
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC21B
	       END-EVALUATE.

       BC21C.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     " Model No/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011040		     " Description Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      *
      *    ****    I T E M   M O D E L	 N U M B E R   /   C O D E
      *
       BC21D.
	     ACCEPT S29-MODEL.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC21B
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BC21D
		 WHEN F2-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC21D
	       END-EVALUATE
	       PERFORM AA50I THRU AA50
	       MOVE W10-MODEL	 TO W90-MODEL
	       IF W90-MODEL = SPACES
		   GO TO BC21D
	       ELSE
		   DISPLAY S29-MODEL
		   MOVE ITM-DESC TO W90-IDESC
		   DISPLAY S29-ITEM
		   DISPLAY CLEAR-L25
		   GO TO BC21F.
	   IF W90-MODEL = SPACES
	       GO TO BC21D.
016640	     DISPLAY CLEAR-L25.
	     MOVE 24		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W90-MODEL
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W90-MODEL	 TO ITM-MODEL-NO.
	     PERFORM READ-ITEMS THRU READ-ITEMS-EXIT.
	   IF WS-F-ERROR = 57
	       PERFORM BE000
	       IF WS-OPTION = "R"
		   GO TO BC21C
	       ELSE
	       IF WS-OPTION = "U"
		   GO TO BC21E.
	     MOVE ITM-DESC	 TO W90-IDESC.
	     DISPLAY S29-ITEM.
	     GO TO BC21F.
      *
      *    ****    I T E M   D E S C R I P T I O N
      *
       BC21E.
	     ACCEPT S29-ITEM.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC21C
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC21E
	       END-EVALUATE.
	   IF W90-IDESC = SPACES
	       MOVE "Item description required"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC21E.
      *
      *    ****    I T E M   S E R I A L   N U M B E R
      *
       BC21F.
	     ACCEPT S29-SERIAL.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC21E
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC21F
	       END-EVALUATE.

       BC22.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Job Code Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      *
      *    ****    T H E   J O B   D E S C R I P T I O N
      *
       BC23.
	     ACCEPT S29-REMARKS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC21C
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BC22
		 WHEN F2-KEY
      *
      *    ****    G E T   S T A N D A R D I S E D   J O B
      * 	   D E S C R I P T I O N
      *
016560		     PERFORM AA50R THRU AA50
016580		     IF W10-JREM = SPACES
016590			 GO TO BC22
		     END-IF
		     MOVE JRM-REMK1	TO W90-REMK1
		     MOVE JRM-REMK2	TO W90-REMK2
		     MOVE JRM-REMK3	TO W90-REMK3
		     MOVE JRM-REMK4	TO W90-REMK4
		     DISPLAY S29-REMARKS
		     GO TO BC23
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC22
	       END-EVALUATE.
016580	   IF W90-REMK1 = SPACES
016590	       GO TO BC22.
	     DISPLAY CLEAR-L25.
      * 			      \
      *      RP = REPAIRS	       ³
      *      FS = FIELD SUPPORT	       ³
      *      MC = ON SITE MAINT. CONT. ³
      *      W	= WARRANTY	       ³
      *      PM = PREVENTATIVE MAINT.	> See  W30-PAYMENT-CODES.
      *      SR = SPECIAL RATES	       ³
      *      RN = REPEAT NO CHARGE     ³
      *	     CB = CALL BACK NO CHARGE  ³
      *	     I	= INSTALLATION	       ³
      *	     C	= CONTRACT	      /
      *
       BC23-PAYMENT.
	     MOVE SPACES	 TO W90-PMETH.
001020	     DISPLAY " " AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     " to view codes"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1.
      *
      *    ****    G E T   S T A N D A R D I S E D   I N V O I C E
      * 	   T Y P E   D E S C R I P T I O N
      *
       BC23A.
	     ACCEPT S29-PAYMENT.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF WS-S7 > 1
			 SUBTRACT 1 FROM WS-S7
		     ELSE
			 MOVE 10 TO WS-S7
		     END-IF
		     PERFORM BC23AB
		 WHEN DOWN-KEY
		     IF WS-S7 < 10
			 ADD 1	 TO WS-S7
		     ELSE
			 MOVE 1  TO WS-S7
		     END-IF
		     PERFORM BC23AB
		 WHEN OTHER
		     CALL X"E5"
	       END-EVALUATE
	       GO TO BC23A
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO BC23
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC23A
	       END-EVALUATE.
	     MOVE 2		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W90-PMETH
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF NOT (W90-PMETH = "RP" OR "FS" OR "MC" OR
			       "W"  OR "PM" OR "SR" OR
			       "RN" OR "CB" OR "I"  OR "C")
	       MOVE "Invalid Invoice Type"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC23A.
	     DISPLAY CLEAR-L25.
	     MOVE 1		  TO WS-S7.

       BC23A-LOOP.
	   IF NOT (W90-PMETH = W30-CDE (WS-S7))
      *
      *    NO LIMIT CHECK PERFORMED AS THE CODE HAS BEEN VALIDATED BEFORE
      *    REACHING THIS POINT AND WILL BE IN THE TABLE.
      *
	       ADD 1		  TO WS-S7
	       GO TO BC23A-LOOP.

       BC23AB.
	     MOVE W30-CDE (WS-S7) TO W90-PMETH.
	     DISPLAY S29-PAYMENT.
	     DISPLAY W30-DSC (WS-S7) AT 1517
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      *
      *    ****    I S	 T H I S   A   R E P E A T
      *
       BC23AC.
	   IF W90-PMETH = "RN" OR "CB"
	       PERFORM SAVE-SCREEN-3
	       PERFORM BD000
	       PERFORM RESTORE-SCREEN-3
	       IF WS-ERROR = "9"
		   GO TO BC23-PAYMENT.
      *
      *    ****    I S	 T H I S   A   W A R R A N T Y
      *
       BC23B.
	   IF NOT (W90-PMETH = "W" OR "RN" OR "CB")
	       GO TO BC23C.
	   IF W90-PMETH = "RN" OR "CB"
	       DISPLAY "Charge A/C" AT 1603.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Debtor Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      *
      *    ****    S T O R E   T H E   D E B T O R   A C C O U N T
      * 	   D E T A I L S
      *
	     MOVE DEB-RECORD1	 TO W40-DEBTREC.
	     MOVE W40-NAME	 TO W40-NME.
      *
      *    ****    G E T   T H E   W A R R A N T Y   A C C O U N T
      * 	   N U M B E R
      *
       BC23BA.
	     ACCEPT S29-WDEBT.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
      *
      *    ****    R E S T O R E   D E B T O R	 A C C O U N T
      * 	   D E T A I L S
      *
		     MOVE W40-DEBTREC  TO DEB-RECORD1
		     MOVE W40-NME      TO W40-NAME
		     GO TO BC23A
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50D THRU AA50
      *
      *    ****   S T O R E   W A R R A N T Y	A C C O U N T
      * 	  N U M B E R
      *
		     MOVE W80-ACNO     TO W90-WAR-AC
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC23BA
	       END-EVALUATE
016570	       DISPLAY S29-WDEBT
016580	       IF W80-ACNO = SPACES
016590		   GO TO BC23BA.
016650       PERFORM CA155-GET-DEBTOR.
016660     IF WS-ERROR = 1
016670	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
016680	       GO TO BC23BA.
016690     IF WS-ERROR = 2
016700	       MOVE "Invalid account"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
016720	       GO TO BC23BA.
016740	     MOVE DEB-NAME	 TO W40-ANAME.
      *
      *    ****    R E M O V E	 N A M E   C O N T R O L
      * 	   C H A R A C T E R S	 I F   I N D I V I D U A L
      *
016750	   IF W80-TYPE = 1
016760	       INSPECT W40-ANAME REPLACING ALL "*" BY " "
016770	       INSPECT W40-ANAME REPLACING FIRST "!" BY " ".
	     DISPLAY W40-ANAME AT 1621
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
      *
      *    ****    C L E A R   I N C O R R E C T   A C C O U N T
      * 	   N A M E   F R O M   T H R   S C R E E N
      *
	       MOVE ALL "°"	 TO W40-ANAME
	       DISPLAY W40-ANAME AT 1621
		       WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
	       MOVE SPACES	 TO W40-ANAME
	       GO TO BC23BA.
      *
      *    ****    W A R R A N T Y   D E B T O R   P R I C E   C O D E
      *
	     MOVE DEB-PRICE	 TO WS-PRC.
      *
      *    ****    R E S T O R E   D E B T O R	 A C C O U N T
      * 	   D E T A I L S
      *
	     MOVE W40-DEBTREC	 TO DEB-RECORD1.
	     MOVE W40-NME	 TO W40-NAME.
	     MOVE DEB-ACNO	 TO W80-ACNO.
016640	     DISPLAY CLEAR-L25.
      *
      *    ****    T E C H N I C I A N	 C O D E   /   N U M B E R
      *
       BC23C.
	     DISPLAY "F6" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "=Technician Code Lookup"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     ACCEPT S29-TECH.
016640	     DISPLAY CLEAR-L25.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC23B
		 WHEN F6-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM TECHNICIAN-LOOKUP
011320		     PERFORM RESTORE-SCREEN
		     MOVE WS-S7  TO W90-TECH
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC23C
	       END-EVALUATE
016570	       DISPLAY S29-TECH
016580	       IF W90-TECH = ZERO
		   GO TO BC23C.
	   IF W90-TECH > 50
	       MOVE "Technician must be in the range 00 to 50"
				 TO WS-ERR-MES
003260	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
	       GO TO BC23C.
	   IF NOT (W90-TECH = ZERO)
	       ADD 200 W90-TECH GIVING WS-PARKEY
	       PERFORM READ-PARAM THRU READ-PARAM-EXIT
	       IF PAR-TECHNICIAN = SPACES
		   MOVE "Invalid Technician Number"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO BC23C
	       END-IF
	       MOVE PAR-TECHNICIAN
				 TO W90-TECHNICIAN
	       DISPLAY W90-TECHNICIAN AT 1717
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 1
	       MOVE "A"		 TO W90-STATUS
           ELSE
               MOVE "U"          TO W90-STATUS.

       BC23D.
	     ACCEPT S29-PRICE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC23C
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC23D
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-PRC
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
017600	   IF NOT (WS-PRC = "1" OR "2" OR "3")
	       MOVE "Price code must be '1','2' or '3'"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC23D.

       BC23E.
	     ACCEPT S29-VAT.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC23D
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC23E
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING W90-EXP
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W90-EXP = "E" OR "I")
	       MOVE "VAT code must be 'E' or 'I'"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC23E.

       BC23F.
	     ACCEPT S29-LOCATION.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC23E
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC23F
	       END-EVALUATE.
	   IF W40-LOC = SPACES
	       MOVE "Location field may not be left blank"
				 TO WS-ERR-MES
003260	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
	       GO TO BC23F.
	     MOVE 5		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-LOC
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

       BC23G.
	     ACCEPT S29-PRIORITY.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BC23F
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC23G
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
017920	   IF WS-OPTION = "N"
017930         GO TO BC21.
	   IF W40-NAME = SPACES
	       MOVE "Client name required"
				 TO WS-ERR-MES
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC21.
	     MOVE 30		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W90-IDESC
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
      *
      *    ****    P R O C E S S   T H E   J O B C A R D   D E T A I L S
      *
017620 BC25.
021040	     DISPLAY CLEAR-L25.
021050	     DISPLAY "P" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
021060		     "rocess Job, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
021070		     "A" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
021080		     "mend details or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
021090		     "C" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
021100		     "ancel" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

021130 BC130.
021140	     ACCEPT WS-OPTION AT 2539
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
021200	   IF NOT (WS-OPTION = "P" OR "A" OR "C")
021210         GO TO BC130.
021220	     DISPLAY CLEAR-L25.
021260     IF WS-OPTION = "A"
021280         GO TO BC21.
021290     IF WS-OPTION = "C"
021300         GO TO BC599.
      *
      *    ****    P R O C E S S   T H E   J O B
      *
021620 BC188.
021630	    DISPLAY "Creating the Job" AT 2520
		     WITH BACKGROUND-COLOR 3
			  FOREGROUND-COLOR 6 HIGHLIGHT.
021650       MOVE ZERO           TO WS-SALKEY.
021660       PERFORM AY60 THRU AY999.
021670       MOVE W70-JOBNO      TO W90-REF.
021680	     DISPLAY W90-REF AT 2214
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 0.
021690     IF W70-JOBNO = 99999999
021700         MOVE 1            TO W70-JOBNO
021710     ELSE
021720         ADD 1             TO W70-JOBNO.
      *
      *    ****    T E S T   F O R   I N D I V I D U A L
      *
021730     IF W80-TYPE NOT = 1
021740         GO TO BC220.
021750       MOVE DEB-NAME       TO W60-NAME.
021760       PERFORM CA165-NAME.
      *
      *    ****    T I T L E
      *
021770     IF DEB-LANG = "A"
021780         MOVE W75-ATITLE (DEB-TITLE) 
021790                           TO W60-NAME
021800     ELSE
021810         MOVE W75-ETITLE (DEB-TITLE) 
021820                           TO W60-NAME.
021830       MOVE 7              TO WS-S6.

021840 BC190.
021850     IF W60-NCHAR (WS-S6) NOT = SPACE
021860         GO TO BC195.
021870       SUBTRACT 1          FROM WS-S6.
021880     IF WS-S6 > 0
021890         GO TO BC190.
      *
      *    ****    F I R S T   I N I T I A L
      *
021900 BC195.
021910	     MOVE 1		 TO WS-S5.
021920       ADD 2               TO WS-S6.
021930       MOVE W40-NCHAR (25) TO W60-NCHAR (WS-S6).
021940       ADD 1               TO WS-S6.
021950       MOVE "."            TO W60-NCHAR (WS-S6).
021960     IF W40-NCHAR (49) = SPACE
021970         GO TO BC200.
      *
      *    ****    S E C O N D   I N I T I A L
      *
021980       ADD 1               TO WS-S6.
021990       MOVE W40-NCHAR (49) TO W60-NCHAR (WS-S6).
022000       ADD 1               TO WS-S6.
022010       MOVE "."            TO W60-NCHAR (WS-S6).

022020 BC200.
022030       ADD 2               TO WS-S6.
      *
      *    ****    S U R N A M E
      *
022040 BC205.
022050	     MOVE W40-NCHAR (WS-S5)
022060                           TO W60-NCHAR (WS-S6).
022070     IF WS-S5 < 24
022080         ADD 1             TO WS-S5
022090         ADD 1             TO WS-S6
022100         GO TO BC205.
022110       MOVE W60-NAME       TO W40-NAME.
           IF DEB-TERMS = "3"
               PERFORM BB000
021630	       DISPLAY "Creating the Job" AT 2520
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT
	   ELSE
	       MOVE DEB-ADDRESS  TO W60-ADDRESS
	       PERFORM CA175-ADDRESS
	       MOVE DEB-POST	 TO W40-PC.
      *
      *    ****    S T A R T   J O B   L O G G I N G 
      *
022120 BC220.
      *
      * 	   J O B H D R
      *
	     INITIALIZE JHD-REC1.
022180	     MOVE W12-T-YMD	 TO JHD-DTE.
	     MOVE ZERO		 TO JHD-START JHD-STIME.
022130	     MOVE W90-REF	 TO JHD-REF.
022150	     MOVE W90-IDESC	 TO JHD-ITEM.
022160	     MOVE W80-ACNO	 TO JHD-AC.
022170	     MOVE W40-NAME	 TO JHD-NAME.
	     MOVE W40-AD1	 TO JHD-AD1.
	     MOVE W40-AD2	 TO JHD-AD2.
	     MOVE W40-AD3	 TO JHD-AD3.
	     MOVE W40-AD4	 TO JHD-AD4.
	     MOVE W40-PC	 TO JHD-PC.
	     MOVE W40-HTEL	 TO JHD-TEL.
	     MOVE W40-CONTACT	 TO JHD-CONTACT.
022200	     MOVE W90-ORD	 TO JHD-ORD.
022210	     MOVE W90-TECH	 TO JHD-SMAN.
022220	     MOVE "J"		 TO JHD-DOC.
022240	     MOVE SPACES	 TO JHD-OREF.
022250	     MOVE W90-EXP	 TO JHD-EXP.
022270	     MOVE WS-PRC	 TO JHD-PRC.
	     MOVE W40-LOC	 TO JHD-LOCATION.
	     MOVE W90-PRIORITY	 TO JHD-PR-CUST.
	     MOVE W90-SERIAL	 TO JHD-SERIAL.
	     MOVE W90-MODEL	 TO JHD-MODEL.
	     MOVE W90-PMETH	 TO JHD-PMETH.
	     MOVE W90-WAR-AC	 TO JHD-WCOMP.
	     MOVE W90-STATUS	 TO JHD-STATUS.
	     MOVE ZERO		 TO JHD-S-DTE JHD-S-TIME
				    JHD-E-DTE JHD-E-TIME.
022290	     PERFORM WRITE-JOBHDR THRU WRITE-JOBHDR-EXIT.
022300	     MOVE 1		 TO WS-ACTION.
022310	     PERFORM AY21 THRU AY59.
      *
      * 	   J O B C S T
      *
022130	     MOVE W90-REF	 TO JOB-REF.
022140	     MOVE ZERO		 TO JOB-SQN.
	     GO TO BC235.
      *
      *    ****    L O G   R E C O R D   T O   O R D E R   F I L E
      *
022280 BC225.
022290       PERFORM WRITE-JOBCST THRU WRITE-JOBCST-EXIT.
022300       MOVE 1              TO WS-ACTION.
022310	     PERFORM AY25 THRU AY59.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   O R D E R   F I L E
      *
022320 BC235.
022370	   IF W90-REMK1 NOT = SPACES
	       MOVE W90-REMK1	 TO W10-DESC
               PERFORM BC247.
022370	   IF W90-REMK2 NOT = SPACES
	       MOVE W90-REMK2	 TO W10-DESC
               PERFORM BC247.
022370	   IF W90-REMK3 NOT = SPACES
	       MOVE W90-REMK3	 TO W10-DESC
               PERFORM BC247.
022370	   IF W90-REMK4 NOT = SPACES
	       MOVE W90-REMK4	 TO W10-DESC
               PERFORM BC247.
	     INITIALIZE HST-RECORD1.
	     MOVE W90-REF	 TO HST-REF.
	     MOVE W90-SERIAL	 TO HST-SERIAL.
	     MOVE W90-MODEL	 TO HST-MODEL.
	   IF W90-PMETH = "W "
	       MOVE 2		 TO HST-TYPE
	   ELSE
	   IF W90-PMETH = "RN" OR "CB"
	       MOVE 3		 TO HST-TYPE
	   ELSE
	       MOVE 1		 TO HST-TYPE.
	     MOVE W90-REMK1	 TO HST-REMKS1.
	     MOVE W90-REMK2	 TO HST-REMKS2.
	     MOVE W90-REMK3	 TO HST-REMKS3.
	     MOVE W90-REMK4	 TO HST-REMKS4.
	     MOVE W90-WAR-AC	 TO HST-WCOMP.
	     MOVE JHD-DTE	 TO HST-DATE.
022150	     MOVE W90-IDESC	 TO HST-DESC.
022160	     MOVE W80-ACNO	 TO HST-AC.
022170	     MOVE W40-NAME	 TO HST-NAME.
	     PERFORM WRITE-JCPHST THRU WRITE-JCPHST-EXIT
022300	     MOVE 1		 TO WS-ACTION.
022310	     PERFORM AY058 THRU AY59.
022560       GO TO BC255.

022570 BC247.
022580       MOVE 3              TO JOB-TYPE.
022590	     INITIALIZE JOB-DETAIL.
022600	     MOVE W10-DESC	 TO JOB-RMKS.
022610       ADD 1               TO JOB-SEQ.
             MOVE ZERO           TO JOB-SUB.
022620       PERFORM BC225.
      *
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *
022630 BC255.
022770	     PERFORM AY70 THRU AY999.
030660	     DISPLAY "CREATE JOB" AT 0235 WITH FOREGROUND-COLOR 15.
023210	     DISPLAY CLEAR-L25.
030670	     DISPLAY "Printing job details" AT 2512
030680		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
030690	     MOVE W90-REF	 TO LS-SLS-JOBNO.
	     MOVE LS-RET	 TO WS-OPTION.
	     MOVE "N"		 TO LS-RET.
030750	     CALL "JCP\JCPPRN"
		   USING LS-PARID LS-USER-ID LS0-PROGRAMS
			 LS0-SECURITY.
030770	     CANCEL "JCP\JCPPRN".
	     MOVE WS-OPTION	 TO LS-RET.
023210	     DISPLAY CLEAR-L25.

       BC260.
	     MOVE "Print labels Y/N?  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO BC260.
	   IF WS-OPTION = "Y"
	       PERFORM BF000.

       BC265.
	     MOVE "Press 'ENTER' to continue - 'E' to exit  [ ]"
				 TO WS-ERR-MES.
	     MOVE " "		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = " " OR "E")
	       GO TO BC265.
023320	   IF NOT (WS-OPTION = "E")
023340	       GO TO BC05.

025620 BC599.
025630       EXIT.
      *
      *    ****    V E R I F Y	 R E P E A T
      *
       BD000	       SECTION 51.
       BD00.
	     MOVE ZERO		 TO WS-ERROR.
	     MOVE "N"		 TO WS-IN-OPT.
	     MOVE W90-MODEL	 TO HST-MODEL.
	     MOVE W90-SERIAL	 TO HST-SERIAL.
	     MOVE W12-T-YMD	 TO W20-DTE.
	   IF W20-MONTH < 3
	       ADD 12		 TO W20-MONTH
	       SUBTRACT 1	 FROM W20-CNYR.
	     SUBTRACT 2 	 FROM W20-MONTH.
	     MOVE W20-DTE	 TO HST-DATE.
	     PERFORM START-AT-JCPHST-RKEY THRU READ-JCPHST-EXIT.
	   IF WS-F-ERROR = 58
	       GO TO BD15.

       BD05.
	     PERFORM RESTORE-SCREEN-3.
	     PERFORM READ-JCPHST-NEXT THRU READ-JCPHST-EXIT.
	   IF WS-F-ERROR = 58
	       GO TO BD15.
	   IF (W90-MODEL NOT = HST-MODEL) OR
	      (W90-SERIAL NOT = HST-SERIAL)
	       GO TO BD15.
	     MOVE "Y"		 TO WS-IN-OPT.
	     MOVE HST-DATE	 TO W100-DATE.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 21		 TO SHADE-COL.
	     MOVE 40		 TO SHADE-WIDTH.
	     MOVE 6		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
		      AT 0919
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "¿" AT
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 1019
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		      W100-DATE WITH FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 5
		     "                              ³"
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 1119
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		      HST-REMKS1 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 1219
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		      HST-REMKS2 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 1319
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		      HST-REMKS3 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 1419
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		      HST-REMKS4 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 1519
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
		      WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 1619
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "	  " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
		     "C" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 5
		     "onfirm repeat/call back, "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "N" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 5
		     "ext      "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "À" AT 1719
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     PERFORM SCREEN-SHADOW.
	     MOVE SPACE 	 TO WS-OPTION.

       BD10.
	     ACCEPT WS-OPTION AT 1655
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BD25
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BD10
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "C" OR "N")
	       CALL X"E5"
	       GO TO BD10.
	   IF WS-OPTION = "N"
	       GO TO BD05.
	     GO TO BD999.

       BD15.
	   IF WS-IN-OPT = "N"
	       MOVE "No repair during last 2 months"
				 TO WS-ERR-STRING
	   ELSE
	       MOVE "No other repairs on file"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     MOVE "N"		 TO WS-OPTION.

       BD20.
	     MOVE "Process as repeat/call back Y/N  [N]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO BD20.
           IF WS-OPTION = "N"
	       GO TO BD25.
	     MOVE SPACES	 TO W85-JPW2.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 30		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1128
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
042540	     DISPLAY "³ " AT 1228 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				       BACKGROUND-COLOR 7
		     "Enter Password        "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1328 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
042550	     ACCEPT W85-JPW2 AT 1245
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 3 HIGHLIGHT AUTO NO-ECHO.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W85-JPW2
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     PERFORM RESTORE-SCREEN.
042560	   IF W85-JPW2 = "+_)(*&" OR
042570	      W85-JPW2 = W85-JPW
042580	       GO TO BD999.
042600	     MOVE "Invalid password"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.

       BD25.
	     MOVE 9		 TO WS-ERROR.

       BD999.
	     EXIT.
      *
      *    ****    N O	 I T E M / M O D E L   O N   F I L E
      *
       BE000	       SECTION 52.
       BE00.
	     MOVE "'A'dd,'R'e-key,'U'se - do not add  [ ]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "A" OR "R" OR "U")
	       GO TO BE00.
	   IF WS-OPTION = "U"
	       GO TO BE999.
	   IF WS-OPTION = "R"
	       GO TO BE999.

       BE05.
	     ACCEPT S29-ITEM.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BE00
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BE05
	       END-EVALUATE.
	   IF W90-IDESC = SPACES
	       MOVE "Item/model description required"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BE05.
	     MOVE 40		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W90-IDESC
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W90-MODEL	 TO ITM-MODEL-NO.
	     MOVE W90-IDESC	 TO ITM-DESC.
	     PERFORM WRITE-ITEMS THRU WRITE-ITEMS-EXIT.

       BE999.
	     EXIT.
      *
      *    ****    P R I N T   L A B E L S
      *
       BF000	       SECTION 53.
       BF00.
	     MOVE W90-REF	 TO W92-JOB.
	     MOVE W40-NAME	 TO W92-NAME.
	     MOVE W90-IDESC	 TO W92-ITEM.
	     MOVE W80-ACNO	 TO W92-ACNO.
	     MOVE TODAY-DDMMYY	 TO W92-DATE.
      *
      *    ***** LABEL FORMAT IS 4 UP *****
      *
	     MOVE 4		 TO W92-NO.
	     GO TO BF10.

       BF02.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1124
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³ Number of labels required    " AT 1224
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1324 WITH FOREGROUND-COLOR 0
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
	     MOVE ZERO		 TO WS-INO.

       BF05.
	     ACCEPT S07.
	     MOVE 16		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
020250	   IF WS-OPTION = "N"
	       GO TO BF05.
	     PERFORM RESTORE-SCREEN-2.

       BF10.
030750	     CALL "JCP\JCPLAB"
		   USING LS-PARID LS-USER-ID W92-LABELS.
030770	     CANCEL "JCP\JCPLAB".

       BF999.
	     EXIT.
      *
      *    ****    C A L L   D E B T O R   C R E A T I O N
      *
       GA000	       SECTION 54.
       GA00.
	     PERFORM SAVE-SCREEN.
	     CLOSE RECOVER.
004200	     CALL "DTP\DTP001"
		   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
	     CANCEL "DTP\DTP001".
038440	     OPEN OUTPUT RECOVER.
038450       CLOSE RECOVER.
038460       OPEN I-O RECOVER.
	     PERFORM RESTORE-SCREEN.
       GA99.
             EXIT.
      /
038140 ZA000-INIT	       SECTION 90.
038150 ZA000-OPEN.
038160	     PERFORM ZA60.
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
      *    UP AND DOWN ARROW KEYS
      *
	     MOVE 2		 TO USER-ACTION.
	     MOVE 3		 TO USER-SETTING.
	     MOVE 5		 TO USER-NUMBER.
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
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
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
	     MOVE "JOBRMK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBRMK.
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
      *
      *    ****    O V E R R I D E   P A S S W O R D
      *
	     MOVE PAR-JCP	 TO W85-JPW2.
	     SUBTRACT 31	 FROM W85-ENC1.
	     SUBTRACT 13	 FROM W85-ENC2.
	     SUBTRACT 32	 FROM W85-ENC3.
	     SUBTRACT 23	 FROM W85-ENC4.
	     SUBTRACT 34	 FROM W85-ENC5.
	     SUBTRACT 43	 FROM W85-ENC6.
	     MOVE W85-JPWC2 (1)  TO W85-JPWC (6).
	     MOVE W85-JPWC2 (2)  TO W85-JPWC (5).
	     MOVE W85-JPWC2 (3)  TO W85-JPWC (4).
	     MOVE W85-JPWC2 (4)  TO W85-JPWC (3).
	     MOVE W85-JPWC2 (5)  TO W85-JPWC (2).
	     MOVE W85-JPWC2 (6)  TO W85-JPWC (1).
038440	     OPEN OUTPUT RECOVER.
038450       CLOSE RECOVER.
038460       OPEN I-O RECOVER.
039390	     MOVE 5		 TO WS-PARKEY.
039400       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039430	     MOVE PAR-DISCOUNT	 TO W15-DISCOUNT.
039440       MOVE 1              TO WS-PARKEY.
039450       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039460       MOVE PAR-DMY        TO W12-TODAY.
039470       MOVE PAR-YMD        TO W12-T-YMD.
039480       MOVE PAR-COMPANY    TO W95-COMP.
039500       MOVE PAR-TELEPHONE  TO W95-TEL.
039740	     MOVE 7		 TO WS-PARKEY.
039750       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039760       MOVE PAR-CASH-AC    TO WS-CASH-AC.
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT (PAR-RT-HD = X"00000000000000000000")
	       MOVE PAR-RT-HD	 TO WS-RT-HD.
	     INSPECT WS-CS-HD REPLACING LEADING "-" BY " ".
	     INSPECT WS-WS-HD REPLACING LEADING "-" BY " ".
	     INSPECT WS-RT-HD REPLACING LEADING "-" BY " ".
	   IF WS-CS-HD = SPACES
	       MOVE "Cash"	 TO WS-CS-HD.
	   IF WS-WS-HD = SPACES
	       MOVE "Wholesale"	 TO WS-WS-HD.
	   IF WS-RT-HD = SPACES
	       MOVE "Retail"	 TO WS-RT-HD.
	     MOVE WS-CS-HD	 TO W15-PNAME.

       ZA05-NAME-LOOP.
	   IF W15-CHECK = SPACE
	       MOVE W15-REST	 TO W15-PNAME
	       GO TO ZA05-NAME-LOOP.

       ZA05-EXIT.
	     MOVE W15-PNAME	 TO WS-CS-HD.
	     MOVE WS-WS-HD	 TO W15-PNAME.
	     PERFORM ZA05-NAME-LOOP.
	     MOVE W15-PNAME	 TO WS-WS-HD.
	     MOVE WS-RT-HD	 TO W15-PNAME.
	     PERFORM ZA05-NAME-LOOP.
	     MOVE W15-PNAME	 TO WS-RT-HD.
038670	     OPEN I-O ITEMS.
           IF WS-STATUS = "05"
	       CLOSE ITEMS
	       OPEN OUTPUT ITEMS
	       CLOSE ITEMS
	       OPEN I-O ITEMS.
038680	   IF RUNTIME-ERROR
038690         IF FLE-LOCKED
038700             GO TO ZA200
038710         ELSE
038720         IF FLE-LIMIT
038730             GO TO ZA49
038740         ELSE
038750	       IF IDX-CORRUPT
038760		   MOVE 57	 TO WS-F-ERROR
038770             GO TO ZA50.
038780     IF WS-STATUS NOT = "00"
038790	       MOVE 57		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
038800		   PERFORM OPEN-ERROR.
039770	     GO TO ZA999.

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.

039850 ZA50.
041850	   IF WS-F-ERROR = 57
041860	       MOVE "MODEL/ITEM file"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
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
041030	   ELSE
041040     IF WS-F-ERROR = 6
041050         MOVE W02-DEBTOR   TO WS-FILE
041060         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041230	   ELSE
041240	   IF WS-F-ERROR = 13
041250         MOVE W02-JOBCST   TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
               MOVE JOB-KEY      TO WS-KEYX
041110     ELSE
041240	   IF WS-F-ERROR = 14
041250	       MOVE W02-JOBHDR	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE JHD-KEY2	 TO WS-KEYX
041110     ELSE
041120     IF WS-F-ERROR = 15
041130         MOVE WS-PARID     TO WS-FILE
041140         MOVE WS-PARKEY    TO WS-KEY
041150     ELSE
041160     IF WS-F-ERROR = 18
041170         MOVE W02-RECOVER  TO WS-FILE
041180         MOVE WS-RECKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 33
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041150     ELSE
041240     IF WS-F-ERROR =  41
041250	       MOVE W02-JOBRMK	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE JRM-KEY	 TO WS-KEYX
041150     ELSE
041240	   IF WS-F-ERROR =  57
041250	       MOVE W02-ITEMS	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE ITM-MODEL-NO TO WS-KEYX
041150     ELSE
041240	   IF WS-F-ERROR =  58
041250	       MOVE W02-JCPHST	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE HST-KEY	 TO WS-KEYX.
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
