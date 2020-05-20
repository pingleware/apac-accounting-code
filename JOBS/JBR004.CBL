      $SET LINKCOUNT"640"
      ******************************************************************
      *                                                                *
      *   ********  *******   *******	  ****	    ****	**     *
      *      **     **	  **  **    **	 **  **    **  **      ***     *
      *      **     **	  **  **    **	**    **  **	**    * **     *
      *      **     *******   *******	**    **  **	**   *	**     *
      *      **     **	  **  **   **	**    **  **	**  *******    *
      *   ** **     **	  **  **    **	 **  **    **  **	**     *
      *    ***	    *******   **    **	  ****	    ****       ****    *
      *                                                                *
      *     ENGLISH						       *
      *                                                                *
      *     A L L O C A T E   S P A R E S   T O   A   J O B	       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  September 2005 - Details of JOBS introduced. New sort key     *
      * 		  JOB-DETAILS = JOB-TYPE JOB-EXT-ITEM JOB-REF  *
      * 							       *
      *  May 2006	- New sort key (Lookup from Stock Item)        *
      * 		  JOB-CODE = JOB-EXT-ITEM JOB-REF	       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JBR004.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1992.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2008
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
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000130 COPY CONTROL.SL.

       COPY DEBTOR.ISL.

000210 COPY DEPART.SL.

       COPY GARTEE.SL.

000270 COPY JOBCST.SL.

000270 COPY JOBHDR.SL.

       COPY JOBREQ.SL.

       COPY JOBWAR.SL.

000210 COPY PARAM.SL.

000230 COPY RECOVER.SL.

       COPY SHARED.SL.

000290 COPY SPARTS.SL.

000140 COPY STKMEM.SL.

000310 COPY STOCK.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

000500 COPY GARTEE.FDE.

       COPY DEBTOR.FDE.

000500 COPY DEPART.FDE.

000500 COPY JOBCST.FDE.

000270 COPY JOBHDR.FDE.

       COPY JOBREQ.FDE.

       COPY JOBWAR.FDE.

000440 COPY PARAM.FDE.

000460 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000520 COPY SPARTS.FDE.

000140 COPY STKMEM.FDE.

000540 COPY STOCK.FDE.

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
000780 77  WS-KEYSTR	   PIC	9(06)	 COMP-5.
000800 77  WS-SALSTR       PIC  9(04)    COMP-5.
000810 77  WS-SALKEY       PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
000820 77  WS-NETKEY       PIC  9(04)    COMP-5.
000840 77  WS-RECKEY       PIC  9(06)    COMP-5.
000850 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000870 77  WS-PRN1	   PIC	9(01) VALUE 0.
000880 77  WS-PRN2         PIC  9(01) VALUE 0.  
000900 77  WS-START        PIC  9(08).
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
003650 77  WS-COND	   PIC	X(06).
003660 77  WS-NORM	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
001060 77  WS-OPTION	   PIC	X(01).
001060 77  WS-IN-OPT       PIC  X(01).
001070 77  WS-MAINT        PIC  X(01).
001070 77  WS-TME          PIC  X(01).
001070 77  WS-SKIP         PIC  X(01) VALUE " ".
001080 77  WS-LOOK	   PIC	X(01) VALUE "R".
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
      -        "                     ³          ³        ³           ³".
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
	   03  WS-PROG	       PIC  X(03) VALUE "004".

001880 01  WS-SLS-JOB.
001890     03  WS-SLS-JOBNO    PIC  X(08) VALUE SPACES.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

002060 COPY CONTROL.ID.

002100 COPY DEBTOR.ID.

003760 COPY DEPART.ID.

002180 COPY GARTEE.ID.

       COPY PARAM.ID.

002140 COPY RECOVER.ID.

002180 COPY JOBCST.ID.

000270 COPY JOBHDR.ID.

       COPY JOBREQ.ID.

       COPY JOBWAR.ID.

       COPY SHARED.ID.

       COPY SPARTS.ID.

       COPY STOCK.ID.

       COPY STKMEM.ID.

003770 01  W02-FILE-IDS.
003910     03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "JBR004.".
               05  W02-USER   PIC X(05) VALUE SPACES.

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
	   03  W10-NEW-SER    PIC  X(16).
	   03  W10-OLD-SER    PIC  X(16).
	   03  W10-OLD-ITEM   PIC  X(18).

       COPY W12.WS.

003390 01  W14-JOB.
003400     03  W14-REF         PIC Z(08).
003410     03  W14-AC          PIC X(06).
003420     03  W14-NAME.
003430	       05  W14-NME     PIC X(30).
003440	       05  FILLER      PIC X(10).
003450	   03  W14-DATE        PIC Z9/99/9999.
003470     03  W14-DOC         PIC X(01).
003480     03  W14-DDOC.
003490         05  W14-INV     PIC Z(08).
003500     03  W14-VALUE       PIC Z(07)9.99.
           03  W14-STATUS      PIC X(01).
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
004230	   03  W40-ID.
004240         05  W40-IDNO    PIC X(14).
004210	   03  W40-TEL	       PIC X(14).
004210     03  W40-FAX         PIC X(14).
004220     03  W40-HTEL        PIC X(14).
	   03  W40-JOBREQ      PIC  X(160).
	   03  W40-CUS-DET.
               05  W40-NM1     PIC  X(40).
               05  W40-AD1     PIC  X(30).
               05  W40-AD2     PIC  X(30).
               05  W40-AD3     PIC  X(30).
               05  W40-AD4     PIC  X(30).
               05  W40-PC      PIC  X(04).
	   03  W40-DTE	       PIC  9(08) COMP-3.

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

       COPY JOBLUP.CRT.

       01  S-CUST.
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "³               J O B   D E T A I L S              ³".
	   03  LINE 10 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 11 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "³Account :                                         ³".
	   03	       COLUMN 37 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 PIC X(06)
				 FROM W80-ACNO.
	   03  LINE 12 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "³Name    :                                         ³".
	   03	       COLUMN 37 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 PIC X(40)
				 FROM JHD-NAME.
	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "³Contact :                                         ³".
	   03	       COLUMN 37 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 PIC X(19)
				 FROM JHD-CONTACT.
	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "³Tel no. :                                         ³".
	   03	       COLUMN 37 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 PIC X(14)
				 FROM JHD-HTEL.
	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "³Item    :                                         ³".
	   03	       COLUMN 37 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 PIC X(30)
				 FROM JHD-ITEM.
	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

010020 01  S42.
010030	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 15
010040				 VALUE "SPARES ALLOCATION".
	   03  LINE  4 COLUMN  2 VALUE "Requisition No :".
010110	   03  LINE  6 COLUMN  2 VALUE "Job No         :".
	   03  LINE  7 COLUMN  2 VALUE "Item Code      :".
	   03  LINE  8 COLUMN  2 VALUE "Description    :".
	   03  LINE  9 COLUMN  2 VALUE "Quantity       :".
	   03  LINE 10 COLUMN  2 VALUE "Unit Price     :".
	   03  LINE 11 COLUMN  2 VALUE "Extended Price :".

       01  S42-WAR.
	   03  LINE  8 COLUMN 14 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
				"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  9 COLUMN 14 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
				"³Serial No :                   ³".
	   03  LINE 10 COLUMN 14 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
				"³DETAILS OF ITEM REPLACED      ³".
	   03  LINE 11 COLUMN 14 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
				"³Item code :  		       ³".
	   03  LINE 12 COLUMN 14 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
				"³Serial No :                   ³".
	   03  LINE 13 COLUMN 14 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
				"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S43-WAR.
	   03  LINE 11 COLUMN 27 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(18) USING REQ-ORG-ITEM AUTO.
	   03  LINE 12 COLUMN 27 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(18) USING REQ-ORG-SERNO AUTO.

       01  S43.
	   03  S43A.
	       05  LINE  4 COLUMN 19 BACKGROUND-COLOR 0
				     FOREGROUND-COLOR 7 HIGHLIGHT
				     PIC Z(08) USING W10-JREQ AUTO.
	   03  S43B.
	       04  S43-JOBNO.
	       05  LINE  6 COLUMN 19 BACKGROUND-COLOR 0
				     FOREGROUND-COLOR 3 HIGHLIGHT
				     PIC Z(08) USING W10-JCST AUTO.
	       04  S43-ADDITIONAL.
	       05  LINE  7 COLUMN 19 BACKGROUND-COLOR 0
				     FOREGROUND-COLOR 3 HIGHLIGHT
				     PIC X(14) USING W10-ITEM AUTO.
	       05  LINE  8 COLUMN 19 BACKGROUND-COLOR 0
				     FOREGROUND-COLOR 3 HIGHLIGHT
				     PIC X(40) USING W10-DESC AUTO.
	       05  LINE  9 COLUMN 21 BACKGROUND-COLOR 0
				     FOREGROUND-COLOR 3 HIGHLIGHT
				     PIC Z(04)9.99 USING W10-QUANT AUTO.
	       05  LINE 10 COLUMN 20 BACKGROUND-COLOR 0
				     FOREGROUND-COLOR 3 HIGHLIGHT
				     PIC Z(05)9.99 USING W10-SELL AUTO.
	       05  LINE 11 COLUMN 19 BACKGROUND-COLOR 0
				     FOREGROUND-COLOR 3 HIGHLIGHT
				     PIC Z(06)9.99 USING W25-VALUE AUTO.

       01  S43-ITM.
005540	   03  LINE  7 COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(14) FROM W10-ITEM AUTO.
	   03	       COLUMN 33 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "/".
	   03	       COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(03) FROM W10-ITM AUTO.

005530 01  S43-EXT.
005540	   03  LINE  7 COLUMN 19 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(18) FROM W10-EXT-ITEM AUTO.

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

	     PERFORM EA000.

	     CLOSE RECOVER.
013140 AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010910 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM JOBREQ-LOOKUP.
	     MOVE "R"		 TO WS-LOOK.
011320	     PERFORM RESTORE-SCREEN.
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

011210 COPY DEPART.RD.

       COPY GARTEE.RD.

011750 COPY PARAM.RD.

011790 COPY JOBCST.RD.

011790 COPY JOBHDR.RD.

011790 COPY JOBREQ.RD.

011790 COPY JOBWAR.RD.

       COPY SHARED.RD.

011810 COPY SPARTS.RD.

       COPY STOCK.RD.

       COPY STKMEM.RD.

      /
011850 AD000-WRITE             SECTION.

011870 COPY CONTROL.WR.

011890 COPY PARAM.WR.

011930 COPY JOBCST.WR.

011930 COPY JOBHDR.WR.

011930 COPY JOBREQ.WR.

011930 COPY JOBWAR.WR.

       COPY JOBREQ.LUP.

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
012020 AY26.
012030	     MOVE 26		 TO REC-FILE.
012040       MOVE ZERO           TO REC-KEY.
012050       MOVE WS-ACTION      TO REC-TYPE.
012060	     MOVE REQ-REC1	 TO REC-JOBREQ.
012070       GO TO AY50.
012020 AY28.
012030	     MOVE 28		 TO REC-FILE.
012040       MOVE ZERO           TO REC-KEY.
012050       MOVE WS-ACTION      TO REC-TYPE.
012060	     MOVE JBW-REC1	 TO REC-JOBWAR.
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
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012480	   IF WS-SKIP = "Y" OR "A"
012490         GO TO AY999.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
	     MOVE SHR-STOCK	 TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "DS"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012670	     MOVE 1		 TO WS-NETKEY.
012680       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012700       MOVE NET-DEBTOR     TO W70-DEBT.
012740	     GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
	       IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
      *
		   IF WS-WAIT = ZERO
		       GO TO AY63
		   ELSE
		       GO TO AY62-WAIT
		   END-IF
	       ELSE
		   GO TO AY62-WAIT
	       END-IF
	   END-IF.
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
012990	     MOVE 1		 TO WS-NETKEY.
013000       MOVE W70-DEBT       TO NET-DEBTOR.
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY40 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES)
	       MOVE PAR-PROG(WS-SUB)
				 TO PAR-PROG(WS-USUB)
	       MOVE PAR-USR(WS-SUB)
				 TO PAR-USR(WS-USUB)
	       ADD 1		 TO WS-USUB
	       GO TO AY75.

       AY80.
013070	     SUBTRACT 1 FROM WS-USUB
				 GIVING PAR-USERS.
013080	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	   IF LS0-POS NOT = 2
012430         CLOSE RECOVER
012440         OPEN I-O RECOVER.
013090       COMMIT.

013000 AY999.
013010       EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

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

       COPY CA200.STK.

       COPY CA500.STK.

      /
      *  ****  S P A R E S   A L L O C A T E D
      *
036990 EA000		       SECTION 5.
037000 EA00.
037010       DISPLAY CLR-SCREEN.
	     DISPLAY S42.
025810	     MOVE ZERO		 TO W10-JREQ.
	     MOVE 1		 TO WS-S9.

       EA05A.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO EA05A.

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
		     MOVE "S"	 TO WS-LOOK
		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO EA06
	       END-EVALUATE
016570	       DISPLAY S43A
016580	       IF W10-JREQ = ZERO
016590		   GO TO EA06.
	   IF W10-JREQ = ZERO
	       GO TO EA999.
	     INITIALIZE REQ-REC1.
	     MOVE W10-JREQ	 TO REQ-REF.
	     MOVE 100		 TO REQ-SQN.
	     PERFORM READ-JOBREQ THRU READ-JOBREQ-EXIT.
	   IF WS-F-ERROR = 42
032780	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO EA06.
	     DISPLAY CLEAR-L25.

       EA10.
	     PERFORM READ-JOBREQ-NEXT THRU READ-JOBREQ-EXIT.
	   IF REQ-TYPE = 9
	       GO TO EA00.
	   IF NOT (REQ-TYPE = 1)
	       GO TO EA10.
	   IF REQ-STATUS NOT = "S"
	       GO TO EA10.
	     MOVE SPACES	 TO W10-OLD-SER
				    W10-OLD-ITEM.

       EA15.
	     MOVE REQ-REF	 TO W10-JREQ.
	     MOVE REQ-JOB	 TO W10-JCST.
	     MOVE REQ-EXT-ITEM	 TO W10-EXT-ITEM STK-CODE.
	     MOVE REQ-DESC	 TO W10-DESC.
	     MOVE REQ-QNT	 TO W10-QUANT.
	     MOVE REQ-COST	 TO W10-COST.
	     MOVE REQ-SELL	 TO W10-SELL.
	     MOVE REQ-INSTRUCT	 TO W90-INSTR.
019430	     COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
	     DISPLAY S43B.
	   IF WS-EXT-STK = "Y"
	       DISPLAY S43-EXT
	   ELSE
	   IF WS-USE-ITM = "Y"
	       DISPLAY S43-ITM.
	     MOVE W10-JCST	 TO JHD-REF.
	     PERFORM READ-JOBHDR THRU READ-JOBHDR-EXIT.
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
	     MOVE JHD-INSTRUCT	 TO W90-INSTRUCT.
	     MOVE JHD-PMETH	 TO W90-PMETH.
	   IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
	     PERFORM CA000.
	   IF WS-F-ERROR = 0
	       MOVE STK-TAX	 TO WS-VAT-SUB
023060	       MOVE W05-VAT(WS-VAT-SUB)
023060				 TO W05-RATE
	       GO TO EA25.

       EA20.
	     MOVE "No Stock Record - 'C'ontinue or 'E'xit  [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "C" OR "E")
	       GO TO EA20.
	   IF WS-OPTION = "C"
	       GO TO EA10.
	     GO TO EA00.

       EA25.
	   IF W90-INSTR = "W" OR "S"
	       IF W90-INSTR = "W"
		   MOVE "Warranty"
				 TO W90-INSTRUCT
	       ELSE
		   MOVE "Swop out"
				 TO W90-INSTRUCT
	       END-IF
	       DISPLAY "    N/C    " AT 1119
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			W90-INSTRUCT
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
	       IF REQ-SERNO NOT = SPACES
		   PERFORM EC000.

       EA26.
	     DISPLAY "Allocate item/s to job no (" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "Y" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 6 HIGHLIGHT
		     "/" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 1
		     "N" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 6 HIGHLIGHT
		     ")? > < - " WITH BACKGROUND-COLOR 3
				      FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     " for Job details" WITH BACKGROUND-COLOR 3
						FOREGROUND-COLOR 1.
	     MOVE "Y"		 TO WS-OPTION.

       EA30.
025850	     ACCEPT WS-OPTION AT 2536
		    WITH BACKGROUND-COLOR 3
			 FOREGROUND-COLOR 6 HIGHLIGHT
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN F2-KEY
		     PERFORM EB000
		     IF WS-OPTION = "R"
			 GO TO EA00
		     ELSE
			 GO TO EA26
		 WHEN OTHER
		     CALL X"E5"
		     GO TO EA30
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO EA30.
	     DISPLAY CLEAR-L25.
	   IF WS-OPTION = "N"
	       GO TO EA10.
      *
      *    ****    R E A D   J O B   D E T A I L S
      *
	     MOVE W10-JCST	 TO JOB-REF.
	     MOVE 99900 	 TO JOB-SEQN.
	     PERFORM START-AT-JOB-NO THRU READ-JOBCST-EXIT.
	   IF WS-F-ERROR = ZERO
	       PERFORM READ-JOBCST-PREV THRU READ-JOBCST-EXIT.
	   IF (WS-F-ERROR = 13) OR
	      (JOB-REF NOT = W10-JCST)
	       MOVE W10-JCST	 TO JOB-REF.
	       MOVE ZERO	 TO JOB-SEQ.
	     ADD 1		 TO JOB-SEQ.
	     MOVE ZERO		 TO JOB-SUB.
	   IF NOT (REQ-INSTRUCT = "R")
	       MOVE ZERO	 TO W25-VALUE.
      *
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *
019830     IF (STK-IND < 2) OR (STK-IND = 3)
019840         COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
	     MOVE W25-VALUE	 TO W90-VALUE.
019880	   IF (STK-TAX = 0) OR (W90-EXP = "X" OR "C")
019890         ADD W90-VALUE     TO W90-NONTAX
019900	       MOVE W90-VALUE	 TO REQ-NONTAX
	       MOVE ZERO	 TO REQ-VAT-SUB
019910     ELSE
	       IF W90-EXP = "I"
		   COMPUTE W90-TAXVAL(WS-VAT-SUB) =
			   W90-TAXVAL(WS-VAT-SUB) + (W90-VALUE -
			 ((W90-VALUE / (100.0000 + W05-RATE)) *
			   W05-RATE))
		   COMPUTE REQ-TAXVAL = (W90-VALUE -
			 ((W90-VALUE / (100.0000 + W05-RATE)) *
			   W05-RATE))
		   ADD REQ-TAXVAL    TO W90-EXCTOT
	       ELSE
		   MOVE W90-VALUE
				 TO REQ-TAXVAL
022890		   ADD W90-VALUE TO W90-TAXVAL(WS-VAT-SUB)
		   ADD REQ-TAXVAL    TO W90-EXCTOT
	       END-IF
	       MOVE WS-VAT-SUB	 TO REQ-VAT-SUB.
019940	   IF (W90-TAX NOT = SPACES) AND
	      (W90-EXP NOT = "C" AND "X")
019950	       COMPUTE REQ-VAT ROUNDED = REQ-TAXVAL * W05-RTE.
	   IF (W90-EXP = "I") AND (STK-TAX NOT = 0)
	       COMPUTE REQ-VAT ROUNDED = (REQ-SELL /
018040		      (100.0000 + W05-RATE)) * W05-RATE.
	   IF REQ-VAT-SUB > 0
	       ADD REQ-VAT	 TO W90-VAT(REQ-VAT-SUB).
      *
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *
019970	     ADD W25-VALUE	 TO W90-DB.
019990       MOVE W90-DB         TO W90-TOTAL.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
023040	   IF (W90-TAX = SPACES) OR
	      (W90-EXP = "C" OR "X")
020100	       GO TO EA35.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       EA32.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO EA32.
020170	     ADD W25-TOTAL	 TO W90-TTAX.
	     ADD W90-TTAX	 TO W90-TOTAL.

020180 EA35.
	     MOVE 1		 TO JOB-TYPE.
	     MOVE W10-EXT-ITEM	 TO JOB-EXT-ITEM.
	     MOVE W10-DESC	 TO JOB-DESC.
029770	     MOVE W10-QUANT	 TO JOB-QNT.
029780	     MOVE W10-COST	 TO JOB-COST.
	   IF NOT (REQ-INSTRUCT = "R")
	       MOVE ZERO	 TO JOB-SELL
				    JOB-VAT
	   ELSE
029790	       MOVE REQ-VAT	 TO JOB-VAT
029800	       MOVE W10-SELL	 TO JOB-SELL.
029810	     MOVE W25-VALUE	 TO JOB-VAL.
029820*	     MOVE ZERO		 TO JOB-DISC.
029830*	     MOVE ZERO		 TO JOB-DSC.
	     MOVE ZERO		 TO JOB-TECH.
029840	     MOVE REQ-INCTAX	 TO JOB-INCTAX.
029850	     MOVE REQ-NONTAX	 TO JOB-NONTAX.
029860	     MOVE REQ-TAXVAL	 TO JOB-TAXVAL.
	     MOVE REQ-SERNO	 TO JOB-SERNO.
	     MOVE REQ-VAT-SUB	 TO JOB-VAT-SUB.
	     MOVE REQ-DSC	 TO JOB-DSC.
	     MOVE REQ-DISC	 TO JOB-DISC.
      *
      *   ****	  S T A R T   R E C O V E R Y	L O G G I N G
      *
	     PERFORM AY60 THRU AY999.
	     MOVE 0		 TO WS-ACTION.
	     MOVE REQ-REC1	 TO W40-JOBREQ.
	     PERFORM READ-JOBREQ-LOCK THRU READ-JOBREQ-EXIT.
	     PERFORM AY26 THRU AY59.
	     MOVE W40-JOBREQ	 TO REQ-REC1.
	     PERFORM READ-JOBHDR-LOCK THRU READ-JOBHDR-EXIT.
	     PERFORM AY21 THRU AY59.
      *
      *   ****	  U P D A T E	J O B R E Q
      *
	     MOVE "A"		 TO REQ-STATUS.
	     MOVE W10-OLD-SER	 TO REQ-ORG-SERNO.
	     MOVE W10-OLD-ITEM	 TO REQ-ORG-ITEM.
	     PERFORM REWRITE-JOBREQ-UNLOCK THRU WRITE-JOBREQ-EXIT.
	     PERFORM WRITE-JOBCST THRU WRITE-JOBCST-EXIT.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY25 THRU AY59.
	   IF W10-OLD-SER NOT = SPACES
	       MOVE W10-OLD-SER  TO JBW-OLD-SER
	       MOVE W10-OLD-ITEM TO JBW-OLD-ITEM
	       MOVE REQ-SERNO	 TO JBW-NEW-SER
	       MOVE REQ-EXT-ITEM TO JBW-NEW-ITEM
	       MOVE REQ-JOB	 TO JBW-JOB
	       MOVE ZERO	 TO JBW-SEQ
	       PERFORM WRITE-JOBWAR THRU WRITE-JOBWAR-EXIT
	       MOVE 1		 TO WS-ACTION
	       PERFORM AY28 THRU AY59.
030320	     MOVE W90-DB	 TO JHD-SUBTOT.
030330       MULTIPLY -1 BY W90-TDISC 
030340				 GIVING JHD-DSCNT
030350	     MOVE W90-EXCTOT	 TO JHD-TAXABLE.
030360	     MOVE W90-NONTAX	 TO JHD-NON-TXBL.
030370	     MOVE W90-TTAX	 TO JHD-TAX.
030380	     MOVE W90-TOTAL	 TO JHD-TOT.
030390	     MOVE W90-BAL	 TO JHD-BAL.
030410	     PERFORM REWRITE-JOBHDR-UNLOCK THRU WRITE-JOBHDR-EXIT.
      *
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *
022770       PERFORM AY70 THRU AY999.

       EA40.
	     MOVE "'C'ontinue or 'E'nd  [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "C" OR "E")
	       GO TO EA40.
	   IF WS-OPTION = "C"
	       GO TO EA10.

       EA999.
	     EXIT.
      /
      *    ****    C L I E N T	 D E T A I L S
      *
       EB000	       SECTION 5.
       EB00.
	     DISPLAY CLEAR-L25.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 50		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     PERFORM SAVE-SCREEN-3.
	     DISPLAY S-CUST.
	     PERFORM SCREEN-SHADOW.

       EB05.
	     MOVE "'C'ontinue or 'R'ekey  [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     MOVE 18		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "C" OR "R")
	       GO TO EB05.
	     PERFORM RESTORE-SCREEN-3.
       EB999.
	     EXIT.
      /
      *    ****    W A R R A N T Y   D E T A I L S
      *
       EC000	       SECTION 5.
       EC00.
	     MOVE "Repair under warranty (Y/N)  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.

       EC05.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO EC00.
	   IF WS-OPTION = "N"
	       MOVE "R" 	 TO REQ-INSTRUCT
	       GO TO EC900.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 16		 TO SHADE-COL.
	     MOVE 28		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     PERFORM SAVE-SCREEN-3.
	     DISPLAY S42-WAR.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY REQ-SERNO AT 0927 WITH BACKGROUND-COLOR 7
					    FOREGROUND-COLOR 6.
       EC10.
	     ACCEPT S43-WAR.
	     DISPLAY CLEAR-L25.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-OLD-SER
				    W10-OLD-ITEM
		     GO TO EC30
		 WHEN OTHER
		     CALL X"E5"
		     GO TO EC10
	       END-EVALUATE.
	   IF W10-OLD-ITEM = SPACES AND
	      W10-OLD-SER = SPACES
	       GO TO EC30.
	   IF W10-OLD-ITEM = SPACES OR
	      W10-OLD-SER = SPACES
	       MOVE "Both fields must contain data"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO EC10.
       EC15.
	     MOVE "'C'ontinue or 'R'ekey   [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "C" OR "R")
	       GO TO EC15.
	   IF WS-OPTION = "R"
	       GO TO EC10.
	     MOVE W10-OLD-ITEM	 TO GUA-EXT-CODE.
	     MOVE W10-OLD-SER	 TO GUA-SERIAL.
	     PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
	   IF WS-F-ERROR = 9
032780	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO EC10.
	     GO TO EC900.

       EC30.
	     MOVE "Must item be replaced as NO CHARGE (Y/N)  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"	       TO WS-OPTION.

       EC35.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO EC30.
	   IF WS-OPTION = "N"
	       MOVE "R"		 TO REQ-INSTRUCT.

       EC900.
	     PERFORM RESTORE-SCREEN-3.
       EC999.
	     EXIT.
      /
038140 ZA000-INIT	       SECTION 9.
038150 ZA000-OPEN.
038160	     PERFORM ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
038170	     DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.
038180	     DISPLAY "CREATE A NEW JOB" AT 1229
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
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
	     MOVE "JOBHDR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBHDR.
	     MOVE "JOBREQ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBREQ.
	     MOVE "JOBWAR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBWAR.
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
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
038300	     MOVE 3		 TO WS-PARKEY.
038310       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	     MOVE 110		 TO WS-USUB.

       ZA01A.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF TRM-PICP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
           IF TRM-PICP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
           IF TRM-PICP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
           IF TRM-PICP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-PICP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-PICP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN1 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH.
       ZA01B.
038430       MOVE PAR-VAT-REG    TO W05-VATNO.
049880*       OPEN OUTPUT PRNREP.
      *	     CALL X"91" USING X91-RES X91-FUN PRNREP.
038440	     OPEN OUTPUT RECOVER.
038450       CLOSE RECOVER.
038460       OPEN I-O RECOVER.
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
	   IF LS-USER = LS-SYS-ID
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
041230	   ELSE
056430	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041230	   ELSE
041850	   IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
041230	   ELSE
041240     IF WS-F-ERROR = 13
041250         MOVE W02-JOBCST   TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
               MOVE JOB-KEY      TO WS-KEYX
041110     ELSE
041120     IF WS-F-ERROR = 15
041130         MOVE WS-PARID     TO WS-FILE
041140         MOVE WS-PARKEY    TO WS-KEY
041150     ELSE
041160     IF WS-F-ERROR = 18
041170         MOVE W02-RECOVER  TO WS-FILE
041180         MOVE WS-RECKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE PRT-EXT-ITEM TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 22
052210         MOVE W02-STOCKF   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 28
041860	       MOVE W02-JOBWAR	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE JBW-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
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
