      $SET LINKCOUNT"704"
      ******************************************************************
      *                                                                *
      *   ********  *******   *******	   ****      ****	**     *
      *      **     **    **  **    **    **  **    **  **     ***     *
      *      **     **	  **  **    **	 **    **  **	 **	**     *
      *      **     *******   *******	 **    **  **	 **	**     *
      *      **     **	  **  **   **	 **    **  **	 **	**     *
      *   ** **     **	  **  **    **	  **  **    **	**	**     *
      *    ***	    *******   **    **	   ****	     ****     ******   *
      *                                                                *
      *     ENGLISH						       *
      *                                                                *
      *     S P A R E S   R E Q U I S I T I O N 		       *
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
000020 PROGRAM-ID.     JBR001.
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

000270 COPY JOBCST.SL.

000270 COPY JOBHDR.SL.

       COPY JOBREQ.SL.

000210 COPY PARAM.SL.

000230 COPY RECOVER.SL.

       COPY SALE.SL.

       COPY SHARED.SL.

000290 COPY SPARTS.SL.

000140 COPY STKMEM.SL.

000310 COPY STOCK.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

       COPY DEBTOR.FDE.

000500 COPY DEPART.FDE.

000500 COPY JOBCST.FDE.

000270 COPY JOBHDR.FDE.

       COPY JOBREQ.FDE.

000440 COPY PARAM.FDE.

000460 COPY RECOVER.GFD.

000480 COPY SALE.JFD.

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
002170 77  WS-SUPER        PIC  9(02)    COMP-5.
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
000900 77  WS-START	   PIC	9(08).
000910 77  WS-END          PIC  9(08).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
000930 77  WS-TME2	   PIC	9(08).
000980 77  WS-INO          PIC  9(02) VALUE 0.
000990 77  WS-LINES        PIC  9(02) VALUE 0.
001000 77  WS-FIRST	   PIC	9(03) VALUE 0.
001010 77  WS-LAST	   PIC	9(03) VALUE 0.
001020 77  WS-LASTINV      PIC  9(04)    COMP-5.
001030 77  WS-LINE         PIC  9(04)    COMP-5.
001040 77  WS-PAGE         PIC  9(02)    COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
001050 77  WS-L1	   PIC	9(04)	 COMP-5.
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
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
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
      -        "      ³                         ³         ³          ³".
	   03  WS-RQ78-L REDEFINES WS-MD-LIN.
	       05  WS-RQ1      PIC  X(01).
	       05  WS-RQ78     PIC  X(78).
	       05  WS-RQ80     PIC  X(01).

001840 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "JBR".
	   03  WS-PROG	       PIC  X(03) VALUE "001".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

002060 COPY CONTROL.ID.

002100 COPY DEBTOR.ID.

003760 COPY DEPART.ID.

       COPY PARAM.ID.

002140 COPY RECOVER.ID.

       COPY SALE.ID.

002180 COPY JOBCST.ID.

000270 COPY JOBHDR.ID.

002180 COPY JOBREQ.ID.

       COPY SHARED.ID.

       COPY SPARTS.ID.

       COPY STOCK.ID.

       COPY STKMEM.ID.

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
	       05  FILLER	   PIC	X(54).
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
	       05  REP-H2	   PIC X(09).
	       05  REP-H3	   PIC X(19).
	       05  REP-H4	   PIC X(42).
	       05  REP-H5	   PIC X(09).
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
004210	   03  W40-TEL	       PIC X(14).
004210     03  W40-FAX         PIC X(14).
004220     03  W40-HTEL        PIC X(14).
           03  W40-DTE         PIC  9(06) COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

004540 COPY W50.WS.


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

       COPY JOBLUP.CRT.

       01  S-CUST.
	   03  LINE  8 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  9 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "³           C L I E N T   D E T A I L S            ³".
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
				 FROM W40-NAME.
	   03  LINE 13 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "³Contact :                                         ³".
	   03	       COLUMN 37 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 PIC X(19)
				 FROM W40-CONTACT.
	   03  LINE 14 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "³Tel no. :                                         ³".
	   03	       COLUMN 37 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 PIC X(14)
				 FROM W40-HTEL.
	   03  LINE 15 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "³Order   :                                         ³".
	   03	       COLUMN 37 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 PIC X(12)
				 FROM W90-ORD.
	   03  LINE 16 COLUMN 26 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	       "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

009660 01  S26-DSC.
009670	   03	       COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009680                           PIC Z9.99 USING WS-DISC AUTO.

010020 01  S42.
010030	   03  LINE  2 COLUMN 32 FOREGROUND-COLOR 15
010040				 VALUE "SPARES REQUISITIONS".
	   03  LINE  3 COLUMN  2 VALUE "Requisition No :".
010110	   03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
	     " No Job no   Item code    Description
      -      " Quantity Unit Price".
009160	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 21 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".

010240 01  SA-JOBNO.
010250	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 0 PIC Z(08) USING W10-JCST AUTO.

007910 01  SA-ITEM.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(14) USING W10-ITEM AUTO.

       01  SA-ITM.
005540	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(14) USING W10-ITEM AUTO.
	   03	       COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(03) USING W10-ITM AUTO.

005530 01  SA-EXT.
005540	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(18) USING W10-EXT-ITEM AUTO.

005530 01  SA-ITEM-3.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  SA-ITEM-4.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  SA-ITEM-5.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  SA-ITEM-6.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  SA-ITEM-7.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  SA-ITEM-8.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  SA-ITEM-9.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  SA-ITEM-10.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  SA-ITEM-11.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  SA-ITEM-12.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  SA-ITEM-13.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  SA-ITEM-14.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(14) USING W10-I14 AUTO.

009840 01  SA-QNT.
009850	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 0 PIC Z(04)9.99 USING W10-QUANT AUTO.

009870 01  SA-PRC.
009880	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 0 PIC Z(05)9.99 USING W10-SELL AUTO.

009960 01  SA-AVGE.
009970	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 0 PIC Z(04)9.999 USING STK-AVGE AUTO.

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
013040	     CLOSE SALE
		   RECOVER.

013140 AA49.
013150       EXIT PROGRAM.

       COPY AA50JS.LUP.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

       COPY AA300.STK.

      *
      *    ****   D I S P L A Y   J O B   L O O K U P	O P T I O N S
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

011210 COPY DEPART.RD.

011750 COPY PARAM.RD.

011790 COPY JOBCST.RD.

011790 COPY JOBHDR.RD.

011790 COPY JOBREQ.RD.

011770 COPY SALE.RD.

       COPY SHARED.RD.

011810 COPY SPARTS.RD.

       COPY STOCK.RD.

       COPY STKMEM.RD.

      /
011850 AD000-WRITE             SECTION.

011870 COPY CONTROL.WR.

011890 COPY PARAM.WR.

011930 COPY JOBREQ.WR.

011910 COPY SALE.WR.

       COPY APAC.HLP.

011990 COPY JOBCST.LUP.

       COPY PRINTUTL.AS1.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
012010 AY000           SECTION.
012020 AY26.
012030	     MOVE 26		 TO REC-FILE.
012040       MOVE ZERO           TO REC-KEY.
012050       MOVE WS-ACTION      TO REC-TYPE.
012060	     MOVE REQ-REC1	 TO REC-JOBREQ.
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
      /
017690 BB050               SECTION.
017700 BB50.
	     MOVE "H"		 TO W02-PRN-TYPE.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
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
	     MOVE "H"		 TO W02-PRN-TYPE.
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
015800 BB100               SECTION.
015810 BB100-MARG.
015820	     MOVE ZERO		 TO WS-ERROR.
	     MOVE W10-SPAPRC	 TO W10-ORDER.
018010	   IF (W90-TAX = SPACES) AND
	      (W90-EXP NOT = "C" AND "X")
015840	       IF STK-TAX NOT = ZERO
015850		   COMPUTE W10-ORDER = (W10-SPAPRC /
015860                                  (100.0000 + W05-RATE)) * 100. 
	   IF WS-PRC = "3"
018050	       IF STK-WSALE NOT > W10-ORDER
018060             GO TO BB110
               END-IF
           ELSE
	   IF WS-PRC = "2"
018050	       IF STK-CASH NOT > W10-ORDER
018060             GO TO BB110
               END-IF
           ELSE
	   IF STK-SELL NOT > W10-ORDER
               GO TO BB110.
	   IF NOT (W10-ORDER = ZERO)
	       GO TO BB104.

	BB103.
	     MOVE "Is this a no-charge item Y/N"
				 TO WS-ERR-MES.
	     MOVE SPACE		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
016020	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO BB103.
	     GO TO BB110.

       BB104.
015890	     COMPUTE W10-MARG = ((W10-ORDER - W10-COST) / W10-ORDER)
015900                          * 100.00.
015910     IF W10-MARG NOT < W85-MARG
015920         GO TO BB110.

015950 BB105.
018110	     MOVE "Margin less than allowed - Override Y/N?"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
016020	   IF NOT (WS-OPTION = "N" OR "Y")
016030         GO TO BB105.
016040     IF WS-OPTION = "N"
016050	       MOVE 1		 TO WS-ERROR
016060         GO TO BB110.

016070 BB106.
016080	     DISPLAY "Enter password" AT 2551
016090		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
016100	     ACCEPT W40-CSHPASS AT 2567
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
016110			 AUTO NO-ECHO.
016120	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016180	     MOVE 1		 TO WS-LENGTH.

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
	   IF DEB-INV-TYPE = "I"
	       MOVE SPACES	 TO W90-TAX
	   ELSE
	       MOVE W05-VATNO	 TO W90-TAX.

013730 CA160-EXIT.
013740       EXIT.

       COPY CA200.STK.

       COPY CA500.STK.

      /
      *  ****  S P A R E S   R E Q U I S I T I O N S
      *
036990 EA000		       SECTION 5.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
       EA0.
018520	     MOVE 5		 TO WS-PARKEY.
018530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
018540       MOVE PAR-PW         TO W85-PASS. 

037000 EA00.
037010       DISPLAY CLR-SCREEN.
	     DISPLAY S42.
033430	     MOVE 6		 TO LIN.
033440	     PERFORM EA05 UNTIL LIN > 20.
016470	     MOVE 1		 TO WS-FIRST WS-LAST
014680				    WS-SALKEY.
025810       MOVE ZERO           TO W10-JCST.
	     GO TO EA10.

       EA05.
028770	     DISPLAY (LIN, 1) "³" WITH BACKGROUND-COLOR 0
				       FOREGROUND-COLOR 3
			       WS-RQ78 WITH FOREGROUND-COLOR 3
			      "³" WITH BACKGROUND-COLOR 0
				       FOREGROUND-COLOR 3.
028780       ADD 1               TO LIN.
       EA10.
	     MOVE 6		 TO LIN.

       EA12.
018550	     MOVE SPACES	 TO W10-DESC W10-EXT-ITEM.
	   IF W10-STCK = "N"
	       IF W10-SLNGTH = 3
		   MOVE ZERO	 TO W10-I3
	       ELSE
	       IF W10-SLNGTH = 4
		   MOVE ZERO	 TO W10-I4
	       ELSE
	       IF W10-SLNGTH = 5
		   MOVE ZERO	 TO W10-I5
	       ELSE
	       IF W10-SLNGTH = 6
		   MOVE ZERO	 TO W10-I6
	       ELSE
	       IF W10-SLNGTH = 7
		   MOVE ZERO	 TO W10-I7
	       ELSE
	       IF W10-SLNGTH = 8
		   MOVE ZERO	 TO W10-I8
	       ELSE
	       IF W10-SLNGTH = 9
		   MOVE ZERO	 TO W10-I9
	       ELSE
	       IF W10-SLNGTH = 10
		   MOVE ZERO	 TO W10-I10
	       ELSE
	       IF W10-SLNGTH = 11
		   MOVE ZERO	 TO W10-I11
	       ELSE
	       IF W10-SLNGTH = 12
		   MOVE ZERO	 TO W10-I12
	       ELSE
	       IF W10-SLNGTH = 13
		   MOVE ZERO	 TO W10-I13
	       ELSE
		   MOVE ZERO	 TO W10-I14.
018560	     MOVE ZERO		 TO W10-SELL.
018560       MOVE 1              TO W10-QUANT.

       EA15.
028630       DISPLAY (LIN, 2) WS-LAST WITH FOREGROUND-COLOR 11.
025840	     PERFORM AA400.

       EA20.
025850	     ACCEPT SA-JOBNO AT LINE LIN COLUMN 6.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W10-JCST
		     IF WS-LAST = 1
			 GO TO EA999
		     ELSE
			 GO TO EA22
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
		     GO TO EA20
	       END-EVALUATE
016560	       PERFORM AA50J THRU AA59
016570	       DISPLAY SA-JOBNO AT LINE LIN COLUMN 6
016580	       IF W10-JCST = ZERO
016590		   GO TO EA20.

       EA22.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
025910	     DISPLAY CLEAR-L25.
025920     IF W10-JCST = ZERO
025930	       GO TO EA65.
025940       MOVE W10-JCST       TO W90-REF.

025950 EA25.
014580	     MOVE W10-JCST	 TO JHD-REF.
014610	     PERFORM READ-JOBHDR THRU READ-JOBHDR-EXIT.
014620	   IF WS-F-ERROR = 14
025980	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
025990	       GO TO EA15.
014650	   IF JHD-DOC NOT = "J"
	       MOVE "Not a job"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
025990	       GO TO EA15.
	     MOVE JHD-PRC	 TO WS-PRC.
	     MOVE JHD-AC	 TO W80-ACNO DEB-ACNO.
	     MOVE JHD-ORD	 TO W90-ORD.
	     MOVE JHD-NAME	 TO W40-NAME.
	     MOVE JHD-CONTACT	 TO W40-CONTACT.
	     MOVE JHD-TEL	 TO W40-TEL.
014800	     PERFORM CA155-GET-DEBTOR.
026070	   IF WS-ERROR NOT = ZERO
026080	       MOVE "Invalid a/c no"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
026090	       GO TO EA15.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 50		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     PERFORM SAVE-SCREEN-3.
	     DISPLAY S-CUST.
	     PERFORM SCREEN-SHADOW.

       EA30.
	     MOVE "'C'ontinue or 'R'ekey  [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     MOVE 18		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "C" OR "R")
	       GO TO EA30.
	     PERFORM RESTORE-SCREEN-3.
	   IF WS-OPTION = "R"
	       GO TO EA15.
	     MOVE SPACES	 TO W10-EXT-ITEM.

       EA35.
	     PERFORM AA300.

       EA40.
	   IF WS-EXT-STK = "Y"
	       ACCEPT SA-EXT AT LINE LIN
	   ELSE
      *
      *        ***** ALPHA-NUMERIC CODE *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   ACCEPT SA-ITM AT LINE LIN
	       ELSE
021070		   ACCEPT SA-ITEM AT LINE LIN
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT SA-ITEM-3 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT SA-ITEM-4 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT SA-ITEM-5 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH =15
	       ACCEPT SA-ITEM-6 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT SA-ITEM-7 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT SA-ITEM-8 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT SA-ITEM-9 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT SA-ITEM-10 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT SA-ITEM-11 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT SA-ITEM-12 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT SA-ITEM-13 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT SA-ITEM-14 AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO EA20
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO EA40
	       END-EVALUATE
016560	       PERFORM AA50 THRU AA59
018630	       DISPLAY SA-ITEM AT LINE LIN
016580	       IF W10-EXT-ITEM = SPACES
016590		   GO TO EA40.
	     MOVE ZERO		 TO COL-AND-LINE.
	     DISPLAY CLEAR-L25.
018660	   IF W10-EXT-ITEM = SPACES
018680	       GO TO EA20.

       EA45.
018690	     PERFORM CA000.
018700     IF WS-F-ERROR = 22
	       MOVE "Z"		 TO W10-ETYPE
018620	       PERFORM AA50 THRU AA59
018630	       DISPLAY SA-ITEM AT LINE LIN
018640	       IF W10-EXT-ITEM = SPACES
018720		   GO TO EA35
               ELSE
		   GO TO EA45.
	   IF STK-IND = 2
	       MOVE "Labour code - may not requisition"
				 TO WS-ERR-MES
	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
	       GO TO EA35.
018730	     MOVE WS-LAST	 TO WS-SALKEY.
018740       INITIALIZE SAL-RECORD1.
018750       MOVE WS-LAST        TO SAL-CODE.
	     MOVE W10-JCST	 TO SAL-JOBNO.
018780	     DISPLAY CLEAR-L25.
      *
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *
018790 EA50.
018800     IF STK-IND = 3
018810         MOVE ZERO         TO STK-AVGE
               MOVE SPACES       TO W10-DESC
018820	       ACCEPT (LIN, 34) W10-SDSC
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0 UPDATE AUTO
018840         DISPLAY "Index Unit cost" AT 2512
018850			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018860	       ACCEPT SA-AVGE AT 2528
018870     ELSE
018880         MOVE STK-DESC     TO W10-DESC
018890	       DISPLAY (LIN, 34) W10-SDSC25 WITH FOREGROUND-COLOR 11.
018900     IF STK-IND = 3
018910         IF STK-AVGE = ZERO
018920		   MOVE "Unit cost is zero"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
018940		   GO TO EA50.
018950	     DISPLAY CLEAR-L25.
      *
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *
018960	     MOVE W10-EXT-ITEM	 TO SAL-EXT-ITEM.
018970       MOVE W10-DESC       TO SAL-DESC.
018980       MOVE STK-AVGE       TO SAL-COST W10-COST.
      *
      *    ****    G E T   Q U A N T I T Y
      *
018990	     ACCEPT SA-QNT AT LINE LIN COLUMN 61.
019000     IF W10-QUANT = ZERO
018880	       MOVE SPACES	 TO W10-DESC
018890	       DISPLAY (LIN, 34) W10-SDSC25 WITH FOREGROUND-COLOR 11
019020	       GO TO EA35.
019030	     DISPLAY CLEAR-L25.
	     MOVE W10-QUANT	 TO SAL-QNT.
      *
      *    ****    I N S T R U C T I O N S
      *
       EA52.
	     MOVE "'R'epair,'S'wop out or 'W'arranty claim  [R]"
				 TO WS-ERR-MES.
	     MOVE "R"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO W90-INSTR.
	   IF NOT (W90-INSTR = "R" OR "S" OR "W")
	       GO TO EA52.
	     MOVE W90-INSTR	 TO SAL-INSTRUCT.
	     DISPLAY CLEAR-L25.
      *
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *
       EA55.
019040     IF STK-IND < 3
019050         IF W90-TAX NOT = SPACES
019060		   IF WS-PRC = "1"
019070                 MOVE STK-SELL 
019080                           TO W10-SELL
019090             ELSE
019100		   IF WS-PRC = "2"
019110                 MOVE STK-CASH 
019120                           TO W10-SELL
019130             ELSE
019140                 MOVE STK-WSALE 
019150                           TO W10-SELL
019160         ELSE
019170		   IF WS-PRC = "1"
019180                 MOVE W10-VSELL 
019190                           TO W10-SELL
019200             ELSE
019210		   IF WS-PRC = "2"
019220                 MOVE W10-VCASH 
019230                           TO W10-SELL
019240             ELSE
019250                 MOVE W10-VWSALE 
019260                           TO W10-SELL.
             MOVE W10-SELL       TO W10-SPAPRC.

       EA58.
019280	     ACCEPT SA-PRC AT LINE LIN COLUMN 70.
           IF W10-SELL < W10-SPAPRC
               COMPUTE WS-DISC ROUNDED = ((W10-SELL - W10-SPAPRC) / 
                                           W10-SELL) * 100.00000
022110	       DISPLAY "Discount %" AT 2518
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
022190         DISPLAY S26-DSC AT LINE 25
	       DISPLAY "R" AT 2541
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "ekey or "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 5
		       "ENTER"
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       " to continue"
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
	       ACCEPT WS-OPTION AT 2568
		      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
022440	       DISPLAY CLEAR-L25
               IF WS-OPTION = "R"
		   GO TO EA58.
019290       MOVE W10-SELL       TO W10-SPAPRC.
019310       PERFORM BB100.
             MOVE ZERO           TO WS-DISC.
019320     IF WS-ERROR = 1
019330         MOVE ZERO         TO WS-ERROR
019340	       GO TO EA55.
019350	     DISPLAY CLEAR-L25.
019370	     MOVE W10-SELL	 TO SAL-SELL.

       EA60.
	     MOVE "'A'ccept or 'C'ancel  [A]"
				 TO WS-ERR-MES.
	     MOVE "A"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "A" OR "C")
	       GO TO EA60.
020420	   IF WS-OPTION = "C"
020430	       DISPLAY (LIN, 2) WS-RQ78
020460	       GO TO EA15.
020480	     PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
020490       ADD 1               TO WS-LAST.
      *
      *    ****    T E S T   I F   S C R E E N   F U L L
      *
020500	   IF LIN < 20
020510         ADD 1             TO LIN
020520	       GO TO EA65.
020530	     MOVE 6		 TO LIN.
033440	     PERFORM EA05 UNTIL LIN > 20.
020550	     GO TO EA10.

020560 EA65.
016400	     MOVE "'N'ext or 'E'nd  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.

       EA70.
	     PERFORM OPT-MESSAGE.
020420	   IF WS-OPTION = "N"
	       GO TO EA12.
      *
      *    ****    W A S   T H E R E   A N   E N T R Y
      *
	     SUBTRACT 1 	 FROM WS-LAST.
021010     IF WS-LAST = ZERO
021020	       GO TO EA999.
      *
      *    ****    P R O C E S S   R E Q U I S I T I O N S
      *
021030 EA75.
016600	     MOVE SPACE		 TO WS-OPTION.
021050	     MOVE "'P'rocess,'A'mend,'C'ancel or 'M'ore items [ ]"
				 TO WS-ERR-MES.
033910	     PERFORM OPT-MESSAGE.

021130 EA80.
021200	   IF NOT (WS-OPTION = "P" OR "A" OR "C" OR "M")
020720	       GO TO EA75.
021230	   IF WS-OPTION = "M"
021240         ADD 1             TO WS-LAST
021250	       GO TO EA12.
021260     IF WS-OPTION = "A"
021270	       PERFORM EA170 THRU EA235
021280	       GO TO EA75.
021290     IF WS-OPTION = "C"
021300	       GO TO EA999.
021610	     DISPLAY CLEAR-L25.
      *
      *    ****    P R O C E S S   T H E   O R D E R
      *
021620 EA85.
021630	    DISPLAY "Processing Requisition" AT 2520 WITH
021640		     BACKGROUND-COLOR 3 FOREGROUND-COLOR 6
		     HIGHLIGHT BLINK.
021650       MOVE ZERO           TO WS-SALKEY.
      *
      *    ****    S T A R T   R E C O V E R Y   L O G G I N G
      *
       EA90.
021660	     PERFORM AY60 THRU AY999.
021670	     MOVE W70-PRTREQ	 TO W90-REF.
021680	     DISPLAY W90-REF AT 0318 WITH FOREGROUND-COLOR 11.
021690     IF W70-ORDER = 99999999
021700	       MOVE 1		 TO W70-PRTREQ
021710     ELSE
021720	       ADD 1		 TO W70-PRTREQ.
      *
      *    ****    S T A R T   R E Q U I S I T I O N   L O G G I N G
      *
022120 EA95.
022130	     MOVE W90-REF	 TO REQ-REF.
022140	     MOVE 1		 TO REQ-SEQ.
022150	     MOVE ZERO		 TO REQ-TYPE REQ-SUB.
      *
      *    ****    L O G   R E C O R D   T O   O R D E R   F I L E
      *
022280 EA100.
022290	     PERFORM WRITE-JOBREQ THRU WRITE-JOBREQ-EXIT.
022300       MOVE 1              TO WS-ACTION.
022310	     PERFORM AY26 THRU AY59.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   O R D E R   F I L E
      *
022320 EA105.
022330	     ADD 1		 TO WS-SALKEY.
022340     IF WS-SALKEY > WS-LAST
022350	       GO TO EA130.
022360       PERFORM READ-SALE THRU READ-SALE-EXIT.
022410	     MOVE 1		 TO REQ-TYPE.
	     MOVE SAL-JOBNO	 TO REQ-JOB.
022420	     MOVE SAL-EXT-ITEM	 TO REQ-EXT-ITEM.
022430	     MOVE SAL-DESC	 TO REQ-DESC.
022440	     MOVE SAL-QNT	 TO REQ-QNT.
	     MOVE SAL-COST	 TO REQ-COST.
	     MOVE SAL-SELL	 TO REQ-SELL.
	     MOVE SAL-INSTRUCT	 TO REQ-INSTRUCT.
	     MOVE "R"		 TO REQ-STATUS.
022550	     ADD 1		 TO REQ-SEQ.
	     PERFORM EA100.
	     PERFORM EA110 THRU EA125.
022560	     GO TO EA105.

       EA110.
	     MOVE REQ-EXT-ITEM	 TO PRT-EXT-ITEM STK-CODE.
020530       PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510     IF WS-F-ERROR = 21
	       GO TO EA125.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
021370       MOVE 1              TO WS-S6.

021420 EA115.
021430	   IF PRT-EXT-CODE(WS-S6) = SPACES
021440         ADD 1             TO WS-S6
021450	       GO TO EA120.
021470	     MOVE PRT-EXT-CODE(WS-S6)
				 TO W10-EXT-ITEM.
021490       PERFORM CA000.
021500     IF WS-F-ERROR = 22
021510         ADD 1             TO WS-S6
021520	       GO TO EA120.
022580	     MOVE 2		 TO REQ-TYPE.
022590	     INITIALIZE REQ-DETAIL.
	     MOVE STK-CODE	 TO REQ-EXT-ITEM.
021540	     MOVE STK-DESC	 TO REQ-DESC.
             MULTIPLY SAL-QNT BY PRT-QUANT (WS-S6)
				 GIVING REQ-QNT.
022610	     ADD 1		 TO REQ-SEQ.
	     PERFORM EA100.
021600       ADD 1 TO WS-S6.

021660 EA120.
021670     IF WS-S6 < 31
021680	       GO TO EA115.

       EA125.
             EXIT.
      *
      *    ****    R E Q U I S I T I O N   T R A I L E R
      *
       EA130.
022640	     INITIALIZE REQ-DETAIL.
022650	     MOVE 9		 TO REQ-TYPE.
022750	     MOVE 99900 	 TO REQ-SQN.
022760	     PERFORM EA100.
      *
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *
022880 EA135.
022770       PERFORM AY70 THRU AY999.
022810	     MOVE "Print requisition slip (Y/N)?  [Y]"
				 TO WS-ERR-MES.
022870	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
022960	   IF NOT (WS-OPTION = "Y" OR "N")
022970	       GO TO EA135.
022980	   IF WS-OPTION = "N"
	       GO TO EA00.
023010	     DISPLAY "Printing picking slip" AT 2312
023020		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 6
			   HIGHLIGHT BLINK.
023030	     MOVE W90-REF	 TO LS-SLS-JOBNO.

      *
      *    ****    R E A D   J O B R E Q   F I L E   A N D   P R I N T
      *
026330	     MOVE W90-REF	 TO REQ-REF.
	     MOVE ZERO		 TO REQ-SQN.
	     PERFORM START-AT-REQ-CODE THRU READ-JOBREQ-EXIT.
	   IF WS-F-ERROR = 42
               DISPLAY "Database corrupted - Press ANY KEY" AT 2502 
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
	       ACCEPT WS-OPTION AT 2537
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO
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
	     MOVE REQ-JOB	 TO REP-JOB.
	     MOVE REQ-EXT-ITEM	 TO PRT-EXT-ITEM.
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
	   IF REQ-INSTRUCT = "S"
	       MOVE "SWAP OUT"	 TO REP-REMKS
	   ELSE
	   IF REQ-INSTRUCT = "W"
	       MOVE "WARRANTY"	 TO REP-REMKS.
020530       PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510     IF WS-F-ERROR = 21
	       MOVE REQ-QNT	 TO REP-QNT
	       PERFORM EA145
021310	       GO TO EA140.

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
	       MOVE "H"		 TO W02-PRN-TYPE
	       MOVE "REQUISITION - CONTINUED ..."
                                 TO REP-H1
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM BB60 THRU BB65.
	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
       EA150.
021370       MOVE 1              TO WS-S6.

021420 EA155.
021430	   IF PRT-EXT-CODE (WS-S6) = SPACES
021440         ADD 1             TO WS-S6
021450	       GO TO EA160.
	     MOVE PRT-EXT-CODE(WS-S6)
				 TO W10-EXT-ITEM.
	   IF WS-USE-ITM = "Y"
	       MOVE PRT-CODE(WS-S6)
				 TO REP-ITM1
	       MOVE "/"		 TO REP-SL
	       MOVE PRT-PITM(WS-S6)
				 TO REP-ITM-EXT
	   ELSE
	       MOVE PRT-EXT-CODE(WS-S6)
				 TO REP-EXT-ITEM.
021480	     MULTIPLY PRT-QUANT(WS-S6) BY REQ-QNT
                                 GIVING REP-QNT.
021490       PERFORM CA000.
021500     IF WS-F-ERROR = 22
021510         ADD 1             TO WS-S6
021520	       GO TO EA160.
021540       MOVE STK-DESC       TO REP-DESC.
	     PERFORM EA145
021600       ADD 1 TO WS-S6.

021660 EA160.
021670     IF WS-S6 < 31
021680	       GO TO EA155.
	     GO TO EA140.
      *
      *    ****    U P D A T E   O R D E R   F I L E 
      *
       EA165.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE ALL "="	 TO REP-DETAIL1.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
026330	     MOVE W90-REF	 TO REQ-REF.
	     MOVE 100		 TO REQ-SQN.
	     PERFORM READ-JOBREQ-LOCK THRU READ-JOBREQ-EXIT.
	     ADD 1		 TO REQ-PICNO.
	     PERFORM REWRITE-JOBREQ-UNLOCK THRU WRITE-JOBREQ-EXIT.
             PERFORM AY70 THRU AY999.
	     MOVE SPACES	 TO REP-DETAIL1.
	     GO TO EA00.
      *
      *    ****    A M E N D   R E Q U I S I T I O N   D E T A I L S
      *
031000 EA170.
031010	     MOVE 6		 TO LIN.
031020	     PERFORM EA05 UNTIL LIN > 20.
031030	     MOVE 6		 TO LIN.
031040       MOVE 1              TO WS-LINES WS-FIRST.

031050 EA175.
031060       MOVE WS-FIRST TO WS-SALKEY.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
031070 EA180.
031080	   IF WS-SALKEY > WS-LAST
031090	       GO TO EA183.
031100       PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
031110       DISPLAY (LIN, 2) SAL-CODE WITH FOREGROUND-COLOR 11.
	     MOVE SAL-JOBNO	 TO W10-JCST.
	     DISPLAY SA-JOBNO AT LINE LIN COLUMN 6.
	   IF WS-USE-ITM = "Y"
	       DISPLAY ( , 15) SAL-ITEM
			       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			       "/" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       SAL-ITM
			       WITH FOREGROUND-COLOR 3 HIGHLIGHT
	   ELSE
031150	       DISPLAY ( , 15) SAL-EXT-ITEM
			       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
031160	     DISPLAY ( , 34) SAL-SDSC WITH FOREGROUND-COLOR 11.
031170       MOVE SAL-QNT        TO W10-QUANT.
031180	     DISPLAY SA-QNT AT LINE LIN COLUMN 61.
031190       MOVE SAL-SELL       TO W10-SELL.
031200	     DISPLAY SA-PRC AT LINE LIN COLUMN 70.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
031230 EA182.
031240	   IF LIN < 20
031250         ADD 1             TO LIN WS-SALKEY WS-LINES
031260	       GO TO EA180.

031270 EA183.
031280       EXIT.

031290 EA185.
031300       MOVE WS-LINES       TO WS-LINE.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
031310	   IF WS-LINES < 15
031320         IF WS-FIRST = 1
029420		   MOVE "'A'mend/'D'elete an Item - 'E'xit"
				 TO WS-ERR-MES
029440	       ELSE
029450		   MOVE "'A'mend/'D'elete an Item,'P'revious - 'E'xit"
				 TO WS-ERR-MES
029480		   MOVE 2	 TO WS-LINES
031490	   ELSE
029500	   IF WS-SALKEY < WS-LAST
029510         IF WS-FIRST = 1
029450		   MOVE "'A'mend/'D'elete an Item,'N'ext - 'E'xit"
				 TO WS-ERR-MES
029550		   MOVE 1	 TO WS-LINES
029560         ELSE
029450		   MOVE "'A'mend/'D'elete,'P'revious,'N'ext - 'E'xit"
				 TO WS-ERR-MES
029600		   MOVE 3	 TO WS-LINES
029610     ELSE
029620         MOVE 2            TO WS-LINES
029450	       MOVE "'A'mend/'D'elete an Item,'P'revious - 'E'xit"
				 TO WS-ERR-MES.

031830 EA190.
	     PERFORM OPT-MESSAGE.
031900	   IF NOT (WS-OPTION = "A" OR "D" OR "E" OR "N" OR "P")
031910	       GO TO EA190.
031920     IF WS-OPTION = "E"
031930	       GO TO EA235.
031940     IF WS-OPTION = "A" OR "D"
031950	       GO TO EA200.
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
031960     IF WS-LINES = 2 OR 3
031970         IF WS-OPTION = "P"
031980		   PERFORM EA195
032000		   SUBTRACT 15	 FROM WS-FIRST
032010		   GO TO EA175.
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
032020     IF WS-LINES = 1 OR 3
032030         IF WS-OPTION = "N"
032040		   PERFORM EA195
032060		   ADD 15	 TO WS-FIRST
032070		   GO TO EA175.
032080	     GO TO EA190.
      *
      *    ****    C L E A R   S C R E E N
      *
032090 EA195.
032100	     MOVE 6		 TO LIN.
032110	     PERFORM EA05 UNTIL LIN > 20.
032120	     MOVE 6		 TO LIN.
032130       MOVE 1              TO WS-LINES.
      *
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *
032140 EA200.
032150	   IF WS-OPTION = "A"
032160	       GO TO EA210.
      *
      *    ****    M A K E   S U R E   T H A T   A N   I T E M 
      *            M U S T   B E   D E L E T E D
      *
032200 EA205.
043780	     MOVE "Are you sure Y/N?  [ ]"
				 TO WS-ERR-MES.
030880	     MOVE SPACE		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
032280	   IF NOT (WS-OPTION = "N" OR "Y")
032290	       GO TO EA205.
032310     IF WS-OPTION = "N"
032320         MOVE WS-LINE      TO WS-LINES
032330	       GO TO EA185.
032340       MOVE "D"            TO WS-OPTION.

032350 EA210.
032360	     DISPLAY CLEAR-L25.
032370       DISPLAY "Enter item No to Amend/Delete >  <" AT 2501
032380		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
032390       MOVE ZERO           TO WS-INO.
      *
      *    ****    G E T   T H E   N U M B E R   O F   T H E   I T E M
      *            T H A T   M U S T   B E   A M E N D E D   O R
      *            D E L E T E D
      *
032400 EA215.
032410	     ACCEPT WS-INO AT 2532
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
032420			 UPDATE AUTO.
032430     IF WS-INO = ZERO
032440	       DISPLAY CLEAR-L25
032450         MOVE WS-LINE      TO WS-LINES
032460	       GO TO EA185.
032470     IF WS-INO > WS-SALKEY
032480	       GO TO EA215.
032490     IF WS-INO < WS-FIRST
032500	       GO TO EA215.
      *
      *    ****    C A L C U L A T E   T H E   L I N E   N U M B E R
      *            O N   W H I C H   T H E   I T E M   I S
      *            D I S P L A Y E D
      *
032510	   IF WS-INO < 16
032520	       ADD 5 WS-INO	 GIVING LIN
032530     ELSE
032540	       COMPUTE LIN = WS-INO - WS-FIRST + 6.
      *
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *
032550       MOVE WS-INO         TO WS-SALKEY.
032560       PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    I S   I T   A N   A M E N D M E N T
      *
032730 EA216.
032740     IF WS-OPTION = "A"
032750	       GO TO EA225.
      *
      *    ****    D E L E T E   I T E M   F R O M   O R D E R   F I L E 
      *
032780       ADD 1               TO W14-SUB.
032790       MOVE SAL-SQN        TO W14-SQN (W14-SUB).
      *
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *
032800 EA220.
032810	     MOVE WS-SALKEY	 TO WS-SALSTR.
032820       ADD 1               TO WS-SALKEY.
032830     IF WS-SALKEY > WS-LAST
032840         SUBTRACT 1        FROM WS-LAST
032850	       GO TO EA170.
032860       PERFORM READ-SALE THRU READ-SALE-EXIT.
032870       SUBTRACT 1          FROM SAL-CODE.
032880       MOVE WS-SALSTR      TO WS-SALKEY.
032890       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
032900       ADD 1               TO WS-SALKEY.
032910	     GO TO EA220.
      *
      *    ****    A M E N D   Q U A N T I T Y
      *
032920 EA225.
032970	     ACCEPT SA-QNT AT LINE LIN COLUMN 61.
032980	     DISPLAY CLEAR-L25.
032990     IF W10-QUANT = ZERO
044520	       MOVE "Quantity MAY NOT be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033020	       MOVE SAL-QNT	 TO W10-QUANT
033030	       GO TO EA225.
	     PERFORM CHECK-CORRECT.
033160	   IF WS-OPTION = "N"
033170	       GO TO EA225.
      *
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *
033180       MOVE W10-QUANT      TO SAL-QNT.

033260 EA232.
033280       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
033290	     GO TO EA170.

033300 EA235.
033310       EXIT.

       EA999.
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
042000	     DISPLAY CLR-SCREEN.
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
	     MOVE "JOBREQ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBREQ.
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
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
038300	     MOVE 3		 TO WS-PARKEY.
038310       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.

       ZA01B.
038430       MOVE PAR-VAT-REG    TO W05-VATNO.
038440	     OPEN OUTPUT RECOVER.
038450       CLOSE RECOVER.
038460       OPEN I-O RECOVER.
038440	     OPEN OUTPUT SALE.
038450	     CLOSE SALE.
038460	     OPEN I-O SALE.
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
	   ELSE
041240	   IF WS-F-ERROR = 13
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
041850     IF WS-F-ERROR = 19
041860         MOVE W02-SALE     TO WS-FILE
041870         MOVE WS-SALKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE PRT-EXT-ITEM TO WS-KEYX
041840     ELSE
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
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041150     ELSE
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
