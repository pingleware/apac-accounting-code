      $SET LINKCOUNT"380"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******	  ****	    ****     ******    *
      *   **	**  **	  **  **    **	 **  **    **  **   **	  **   *
      *   **	**  **	  **  **    **	**    **  **	**	  **   *
      *   *******   ******    *******	**    **  **	**     ****    *
      *   **	    **	 **   **	**    **  **	**	  **   *
      *   **	    **	  **  **	 **  **    **  **   **	  **   *
      *   **	    **	  **  **	  ****	    ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   S A L E S   L E D G E R		       *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Jan 2010 - Allow for two discounts for a Debtor on the same   *
      * 	    Sales Ledger code. Included two additional Debtor  *
      * 	    discount codes (11 and 12) each with a field to    *
      * 	    define which debtor discount code uses this as an  *
      * 	    alternate discount. The relevant Stock Item will   *
      * 	    have an indicator field to instruct the system to  *
      * 	    use the alternate discount. The alternate discount *
      * 	    is only applicable if the discount code for which  *
      * 	    this alternate discount is applicable is the code  *
      * 	    on the Debtor account record.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRP003.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2010
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
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY CONTROL.SL.

       COPY DEPART.SL.

000160 COPY GACCNT.SL.

000140 COPY PARAM.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

       COPY DEPART.FDE.

000320 COPY GACCNT.FDE.

000220 COPY PARAM.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
000940 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000950 77  WS-S1	   PIC 9(04)	 COMP-5.
000960 77  WS-S2	   PIC 9(04)	 COMP-5.
000970 77  WS-S3	   PIC 9(04)	 COMP-5.
000980 77  WS-S4	   PIC 9(04)	 COMP-5.
000990 77  WS-S5	   PIC 9(04)	 COMP-5.
001000 77  WS-S6	   PIC 9(04)	 COMP-5.
001000 77  WS-S7	   PIC 9(04)	 COMP-5.
001000 77  WS-S8	   PIC 9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC 9(04)	 COMP-5.
001010 77  WS-PARKEY	   PIC 9(04)	 COMP-5.
001010 77  WS-PRNKEY	   PIC 9(04)	 COMP-5.
001010 77  WS-NETKEY	   PIC 9(04)	 COMP-5.
001020 77  WS-SECKEY	   PIC 9(04)	 COMP-5.
001070 77  WS-NUM	   PIC Z9.
001070 77  WS-SNO	   PIC 9(02).
       77  WS-ADV-RATE	   PIC S9(03)V99 COMP-3.
001090 77  WS-PAGE	   PIC 9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
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
001900 77  WS-OPEN	   PIC	9(02) VALUE ZERO.
001100 77  WS-OPTION	   PIC	X(01).
001120 77  WS-ERROR	   PIC	9(01).
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
001160 77  WS-COMP	   PIC	9(01).
001170 77  WS-IND	   PIC	9(01) VALUE 0.
002190 77  WS-ER6	   PIC	X(09) VALUE "No Record".
002200 77  WS-ER7          PIC  X(13) VALUE "Group Heading".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MDGL-LNE REDEFINES WS-MID-LNE.
	       05  WS-MGL1     PIC X(01).
	       05  WS-MGL78    PIC X(78).
	       05  WS-MGL80    PIC X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.

001260 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "PRP".
	   03  WS-PROG	       PIC  X(03) VALUE "003".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

000320 COPY GACCNT.ID.

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
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  W02-PRN-TYPE    PIC  X(01).
	   03  W02-PRN-LINE    PIC  X(136).
000260	   03  REP-LINE1 REDEFINES W02-PRN-LINE.
000270	       05  REP-DETAIL1 PIC  X(136).
000280	   03  REP-LINE2 REDEFINES W02-PRN-LINE.
000290	       05  REP-DETAIL2 PIC  X(80).
000300	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
000310	       05  REP-PAGE    PIC  X(06).
000320	       05  REP-P-NO    PIC  Z(03)9.
000330	       05  FILLER      PIC  X(09).
000340	       05  REP-COMPANY PIC  X(40).
000350	       05  FILLER      PIC  X(05).
000360	       05  REP-DATE    PIC  X(06).
000370	       05  REP-DTE     PIC  Z9/99/9999.
000380	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
000390	       05  FILLER      PIC  X(11).
000400	       05  REP-HEAD1   PIC  X(04).
000410	       05  FILLER      PIC  X(06).
000420	       05  REP-HEAD2   PIC  X(15).
000430	       05  FILLER      PIC  X(06).
000440	       05  REP-HEAD3   PIC  X(15).
000450	       05  FILLER      PIC  X(06).
000460	       05  REP-HEAD4   PIC  X(06).
000470	       05  FILLER      PIC  X(11).
000480	   03  REP-LINE5 REDEFINES W02-PRN-LINE.
000490	       05  FILLER      PIC  X(11).
000500	       05  REP-TRAN    PIC  Z99.
000510	       05  FILLER      PIC  X(09).
000520	       05  REP-ENG     PIC  X(12).
000530	       05  FILLER      PIC  X(09).
000540	       05  REP-AFR     PIC  X(12).
000550	       05  FILLER      PIC  X(10).
000560	       05  REP-ACT     PIC  X(01).
000570	       05  FILLER      PIC  X(13).
000580	   03  REP-LINE6 REDEFINES W02-PRN-LINE.
000590	       05  FILLER      PIC  X(06).
000600	       05  REP-HEAD5   PIC  X(07).
000610	       05  REP-BATCH   PIC  Z(04)9.
000620	       05  FILLER      PIC  X(04).
000630	       05  REP-HEAD6   PIC  X(20).
000640	       05  FILLER      PIC  X(38).
000650	   03  REP-LINE7 REDEFINES W02-PRN-LINE.
000660	       05  FILLER      PIC  X(02).
000670	       05  REP-HEAD7   PIC  X(12).
000680	       05  REP-HEAD8   PIC  X(09).
000690	       05  REP-HEAD9   PIC  X(07).
000700	       05  REP-HEAD10  PIC  X(16).
000710	       05  REP-HEAD11  PIC  X(10).
000720	       05  REP-HEAD12  PIC  X(13).
000730	       05  REP-HEAD13  PIC  X(11).
000740	   03  REP-LINE8 REDEFINES W02-PRN-LINE.
000750	       05  FILLER      PIC  X(03).
000760	       05  REP-TAC     PIC  9(05).
000770	       05  FILLER      PIC  X(04).
000780	       05  REP-TDTE    PIC  X(08).
000790	       05  FILLER      PIC  X(04).
000800	       05  REP-TCDE    PIC  9(02).
000810	       05  FILLER      PIC  X(04).
000820	       05  REP-TDESC   PIC  X(12).
000830	       05  FILLER      PIC  X(03).
000840	       05  REP-TREF    PIC  X(06).
000850	       05  FILLER      PIC  X(03).
000860	       05  REP-TDB     PIC  Z(06)9.99.
000870	       05  FILLER      PIC  X(03).
000880	       05  REP-TCR     PIC  Z(06)9.99.
000890	       05  FILLER      PIC  X(03).
000900	   03  REP-LINE9 REDEFINES W02-PRN-LINE.
000910	       05  FILLER      PIC  X(54).
000920	       05  REP-BTOT    PIC  Z(06)9.99-.
000930	       05  FILLER      PIC  X(15).

001340 01  W05-PRINT.
001350     03  W05-ESC        PIC 9(02) COMP-X.
001360     03  W05-ESCR REDEFINES W05-ESC.
001370         05  W05-E      PIC X(01).
001380     03  W05-DBL        PIC 9(02) COMP-X.
001390     03  W05-DBLR REDEFINES W05-DBL.
001400         05  W05-D      PIC X(01).
001410     03  W05-CDBL       PIC 9(02) COMP-X.
001420     03  W05-CDBLR REDEFINES W05-CDBL.
001430         05  W05-CD     PIC X(01).
001440     03  W05-SI         PIC 9(02) COMP-X.
001450     03  W05-SIR REDEFINES W05-SI.
001460         05  W05-I      PIC X(01).
001470     03  W05-CSI        PIC 9(02) COMP-X.
001480     03  W05-CSIR REDEFINES W05-CSI.
001490         05  W05-CI     PIC X(01).
001500     03  W05-SO         PIC 9(02) COMP-X.
001510     03  W05-SOR REDEFINES W05-SO.
001520         05  W05-O      PIC X(01).
001530     03  W05-CSO        PIC 9(02) COMP-X.
001540     03  W05-CSOR REDEFINES W05-CSO.
001550         05  W05-CO     PIC X(01).
001560     03  W05-6LPI       PIC 9(02) COMP-X.
001570     03  W05-6LPIR REDEFINES W05-6LPI.
001580         05  W05-6      PIC X(01).
001590     03  W05-8LPI       PIC 9(02) COMP-X.
001600     03  W05-8LPIR REDEFINES W05-8LPI.
001610         05  W05-8      PIC X(01).
001590     03  W05-NUL        PIC 9(02) COMP-X.
001600     03  W05-NULR REDEFINES W05-NUL.
001610         05  W05-N      PIC X(01).
001620     03  W05-CSHDRW     PIC 9(02).
001230 01  W06-COND.
001240     03  W06-DEC         PIC 9(03) OCCURS 4.
001250 01  W07-NORM.
001260     03  W07-DEC         PIC 9(03) OCCURS 4.
001270 01  W08-EXPD.
001280     03  W08-DEC         PIC 9(03) OCCURS 4.
001290 01  W09-ECAN.
001300     03  W09-DEC         PIC 9(03) OCCURS 4.
001310 01  W10-PGEL48.
001320     03  W10-DEC         PIC 9(03) OCCURS 4.
001330 01  W11-PGEL66.
001340     03  W11-DEC         PIC 9(03) OCCURS 4.
001330 01  W12-10CPI.
001340     03  W12-DEC         PIC 9(03) OCCURS 4.
001330 01  W13-CPI12.
001340     03  W13-DEC         PIC 9(03) OCCURS 4.
001330 01  W14-CPI17.
001340     03  W14-DEC         PIC 9(03) OCCURS 4.
001330 01  W15-DBLP.
001340     03  W15-DEC         PIC 9(03) OCCURS 4.
001330 01  W16-DCAN.
001340     03  W16-DEC         PIC 9(03) OCCURS 4.

001660 01  W10-INPUT.
002890	   03  W10-LEDG.
	       05  W10-LEDG-NO PIC  9(04).
	   03  W10-LEDG-RED REDEFINES W10-LEDG.
	       05  W10-GROUP   PIC  X(03).
	       05  W10-D-NO    PIC  9(01).
001680	   03  W10-DATE        PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       COPY W12.WS.

       01  W15-CONTROL.
           03  W15-DEBINV     PIC  9(08)    COMP-3.
           03  W15-DEBREC     PIC  9(08)    COMP-3.
           03  W15-DEBORD     PIC  9(08)    COMP-3.
           03  W15-DEBJOB     PIC  9(08)    COMP-3.
           03  W15-CRDORD     PIC  9(08)    COMP-3.
003540	   03  W15-LEDG       PIC  9(03)    COMP.

001490 01  W15-HEX.
001500     03  W15-ASCII       PIC X(01) OCCURS 256.

002660 01  W15-INPUT.
002410	   03  W15-ACNT    PIC	9(06).
002670	   03  W15-ACCOUNT PIC	9(06).
002680     03  W15-ACNO1   REDEFINES W15-ACCOUNT.
002690         05  W15-ACNO
002700                     PIC 9(04).
002710         05  W15-SER PIC  9(02).
002720     03  W15-ACNO2   REDEFINES W15-ACCOUNT.
002730         05  W15-GR  PIC 9(02).
002740         05  W15-SGRP
002750                     PIC 9(02).
002760         05  W15-ASER
002770                     PIC 9(02).
002780     03  W15-ACNO3   REDEFINES W15-ACCOUNT.
002790         05  W15-GROUP
002800                     PIC 9(02).
002810         05  W15-REST
002820                     PIC 9(04).
002830     03  W15-ACNO4   REDEFINES W15-ACCOUNT.
002840         05  W15-GRP PIC  9(01).
002850         05  FILLER  PIC  9(05).
003050     03  W15-ACNME   PIC  X(30).

       COPY W20.WS.

001890 01  W25-CALCS.
001900     03  W25-RESULT     PIC 9(07)V99.
001910     03  W25-RESULT1 REDEFINES W25-RESULT.
001920         05  FILLER      PIC 9(01).
001930         05  W25-WHOLE  PIC 9(06).
001940         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(06).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
001950     03  W25-PASS.
001960         05  W25-PASS1  PIC 9(03).
001970         05  W25-PASS2  PIC 9(03).
001980     03  W25-TIME       PIC 9(08).
001990     03  W25-TRED REDEFINES W25-TIME.
002000         05  FILLER      PIC 9(02).
002010         05  W25-FACT   PIC 9(06).
002020     03  W25-TOTAL      PIC S9(07)V99  COMP-3.
002030     03  W25-VALUE      PIC S9(07)V99  COMP-3.
002040 01  W30-MONTHS.
002050     03  W30-MONTH-NAMES.
002060         05  W30-ENG    PIC X(36)     VALUE
002070                         "JanFebMarAprMayJunJulAugSepOctNovDec".
002080     03  W30-MONTH REDEFINES W30-MONTH-NAMES
002090                         PIC X(03)     OCCURS 12.
002100 01  W35-PASSWORD.
002100     02  W35-PASS.
002110     03  W35-PASS1      PIC 9(03).
002120     03  W35-PASS2      PIC 9(03).
002130     02  W35-PASSW REDEFINES W35-PASS.
002140     03  W35-PNUM       PIC 9(06).
002150 01  W40-COMPANY.
002160     02  W40-NAME1.
002170         03  W40-NAME   PIC X(40).
002180         03  FILLER      PIC X(32).
002190     02  W40-NAME2 REDEFINES W40-NAME1.
002200         03  W40-SURNAME PIC X(24).
002210         03  W40-FNAME  PIC X(24).
002220         03  W40-SNAME  PIC X(24).
002230     02  W40-NAME3 REDEFINES W40-NAME1.
002240         03  W40-NCHAR OCCURS 72 PIC X(01).
002250     02  W40-ADDRESS1.
002260         03  W40-ADD1   PIC X(30).
002270         03  W40-ADD2   PIC X(30).
002280         03  W40-ADD3   PIC X(30).
002290         03  W40-ADD4   PIC X(30).
002300     02  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
002310         03  W40-ACHAR OCCURS 120 PIC X(01).
002320     02  W40-POST       PIC 9(04).
002330     02  W40-TEL        PIC X(14).
002340     02  W40-COST-CODE  PIC X(10).
           02  W40-VATNO      PIC X(14).

       COPY W40.WS.

002370 01  W45-TRAN.
002380     03  W45-CODE       PIC  9(02).
002390     03  W45-ENG        PIC  X(12).
002400     03  W45-AFR        PIC  X(12).
002410     03  W45-ACT        PIC  X(01).
      *    03  W45-E-ABRV     PIC  X(03).
      *    03  W45-A-ABRV     PIC  X(03).
           03  W45-T-GL       PIC  9(06).
002370 01  W46-TRAN.
002380     03  W46-CODE       PIC  9(02).
002390     03  W46-DESC       PIC  X(15).
           03  W46-ABRV       PIC  X(03).
002410     03  W46-ACT        PIC  X(01).
           03  W46-T-GL       PIC  9(06).

       COPY FUNCTION.WS.

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
	   03  W75-SLEDG      PIC  9(03)    COMP-3 OCCURS 10.
	   03  W75-GNO	      PIC 9(06) COMP-5
                              OCCURS 18.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
      *   ******    ******   *******   ********  ********  **     ** 
      *  **    **  **    **  **    **  **        **        ***    ** 
      *  **        **        **    **  **        **        ** *   ** 
      *   ******   **        ******    *****     *****     **  *  ** 
      *        **  **        **   **   **        **        **   * ** 
      *  **    **  **    **  **    **  **        **        **    *** 
      *   ******    ******   **    **  ********  ********  **     ** 
      *                                                              
003150 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S10.
003530	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
003540				 VALUE "SALES LEDGER OPTIONS".
	   03  LINE  4 COLUMN 12 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "M".
	   03	       COLUMN 13 VALUE "aintain, ".
	   03	       COLUMN 22 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "P".
	   03	       COLUMN 23 VALUE "rint or ".
	   03	       COLUMN 31 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "E".
	   03	       COLUMN 32 VALUE "xit ? > <".

003990 01  S12.
004000     03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15
004010                           VALUE "SALES LEDGER".
004020	   03  LINE  4 COLUMN  6 VALUE "Ledger No....".
004030	   03	       COLUMN 30 VALUE "Description...".
004040     03  LINE  6 COLUMN  6 VALUE "General Ledger Numbers:".
	   03  LINE  7 COLUMN  8 VALUE "Income G/L.".
004050	   03	       COLUMN 30 VALUE "Discount G/L..".
004070	   03  LINE  8 COLUMN  8 VALUE "Stock G/L..".
004060	   03	       COLUMN 30 VALUE "Cost-Of-Sales.".
           03  LINE 10 COLUMN  6 VALUE "Markup Percentages:".
004080	   03  LINE 11 COLUMN  8 VALUE "Retail.....".
004080	   03	       COLUMN 30 VALUE "Cash..........".
004080	   03	       COLUMN 55 VALUE "Wholesale.....".
           03  LINE 13 COLUMN  6 VALUE "Discount Rates (Debtors):".
	   03	       COLUMN 55 VALUE "Alternate Rates".
	   03  LINE 14 COLUMN  8 VALUE "Rate  1....".
	   03	       COLUMN 30 VALUE "Rate  2.......".
	   03	       COLUMN 55 VALUE "For Rate.".
	   03  LINE 15 COLUMN  8 VALUE "Rate  3....".
	   03	       COLUMN 30 VALUE "Rate  4.......".
	   03	       COLUMN 55 VALUE "Alt Rate.".
	   03  LINE 16 COLUMN  8 VALUE "Rate  5....".
	   03	       COLUMN 30 VALUE "Rate  6.......".
	   03	       COLUMN 55 VALUE "For Rate.".
	   03  LINE 17 COLUMN  8 VALUE "Rate  7....".
	   03	       COLUMN 30 VALUE "Rate  8.......".
	   03	       COLUMN 55 VALUE "Alt Rate.".
	   03  LINE 18 COLUMN  8 VALUE "Rate  9....".
	   03	       COLUMN 30 VALUE "Rate 10.......".
           03  LINE 20 COLUMN  8 FOREGROUND-COLOR 14 VALUE 
	   "Ledger Codes XXXX, YYYY & ZZZZ are reserved for system use".
	   03  LINE 21 COLUMN  8 FOREGROUND-COLOR 14 VALUE
	   "Codes starting with VAT are reserved for Value Added Tax".

       01  S12A.
004000	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 15
004010				 VALUE "VALUE ADDED TAX".
	   03  LINE  7 COLUMN  8 VALUE "Output VAT.".
004050	   03	       COLUMN 30 VALUE "Not Used......".
004070	   03  LINE  8 COLUMN  8 VALUE "Input VAT..".
004060	   03	       COLUMN 30 VALUE "Not Used......".

005040 01  S25.
005050     03  S25A.
005060	       05  LINE  4 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC X(04) USING W10-LEDG AUTO.
005080     03  S25B.
005090	       05  LINE  4 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC X(30) USING DPT-DESC AUTO.
005080	   03  S25C.
005110	       05  LINE  7 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC Z(05)9 USING DPT-SALE-GL AUTO.
005080	   03  S25D.
005130	       05  LINE  7 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC Z(05)9 USING DPT-DISC-GL AUTO.
005080	   03  S25E.
005160	       05  LINE  8 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC Z(05)9 USING DPT-PUR-GL AUTO.
005080	   03  S25F.
005180	       05  LINE  8 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC Z(05)9 USING DPT-COST-GL AUTO.
	   03  S25G.
005200	       05  LINE 11 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-MARKUP AUTO.
005200	       05	   COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-CMRKUP AUTO.
005200	       05	   COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-WMRKUP AUTO.
	       05  LINE 14 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-DD1 AUTO.
	       05	   COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-DD2 AUTO.
	       05  LINE 15 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-DD3 AUTO.
	       05	   COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-DD4 AUTO.
	       05  LINE 16 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-DD5 AUTO.
	       05	   COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-DD6 AUTO.
	       05  LINE 17 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-DD7 AUTO.
	       05	   COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-DD8 AUTO.
	       05  LINE 18 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-DD9 AUTO.
	       05	   COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
				     PIC ZZ9.99 USING DPT-DD10 AUTO.
	   03  S25H.
	       05  LINE 14 COLUMN 65 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC 99 USING DPT-ALT-DD1 AUTO.
	       05  LINE 15 COLUMN 65 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-DD11 AUTO.
	       05  LINE 16 COLUMN 65 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC 99 USING DPT-ALT-DD2 AUTO.
	       05  LINE 17 COLUMN 65 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 4
003470				     PIC ZZ9.99 USING DPT-DD12 AUTO.

       COPY DEPART.CRT.

       COPY S99.CRT.

       COPY LEDLUP.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
      *        *******    *******     ******     ****** 
      *        **    **   **    **   **    **   **    **
      *        **    **   **   **    **    **   **
      *        *******    ******     **    **   **
      *        **         **   **    **    **   **
      *        **         **    **   **    **   **    **
      *        **         **    **    ******     ****** 
      *
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006220 AA000-MAIN	       SECTION.
006230 AA000-INIT.
003240	   IF LS0-PALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.
	     PERFORM BA000.
	   IF LS0-LEV > 0
	       CLOSE GACCNT.

       AA49.
             EXIT PROGRAM.

       COPY APAC.HLP.

       AA350-LOOKUP    SECTION.
       AA350.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM GACCNT-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
       AA399.
             EXIT.

       COPY LOCKED.RC2.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

      /
      *    ****    READ FILES ROUTINES
      *
006630 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY GACCNT.RD.

       COPY PARAM.RD.

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
006940 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY PARAM.WR.

       COPY DEPART.LUP.

       COPY GACCNT.LUP.

       BA000	       SECTION.
       BA00.
007330	     DISPLAY CLR-SCREEN.
007340       DISPLAY S10.
	     MOVE "M"		 TO WS-OPTION.

       BA05.
	     ACCEPT WS-OPTION AT 0439
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA05
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "E" OR "M" OR "P")
	       GO TO BA05.
	   IF WS-OPTION = "M"
	       PERFORM YA000
	   ELSE
	   IF WS-OPTION = "P"
	       PERFORM YB000
	   ELSE
	       GO TO BA999.
	     GO TO BA00.
       BA999.
	     EXIT.

       YA000	       SECTION 5.
      *
      *  ***    *   *     *****  ***    *     ***** ****   ***  ***** **** 
      * *   *  * *  *     *     *   *   *     *     *   * *   * *     *   *
      * *     *   * *     *     *       *     *     *   * *     *     *   *
      *  ***  ***** *     ***    ***    *     ***   *   * *     ***   **** 
      *     * *   * *     *         *   *     *     *   * *  ** *     *   *
      * *   * *   * *     *     *   *   *     *     *   * *   * *     *   *
      *  ***  *   * ***** *****  ***    ***** ***** ****   ***  ***** *   *
      *
009110 YA75-SALES.

       YA80.
009120       DISPLAY CLR-SCREEN.
009130	     DISPLAY S12.
	     MOVE ZERO		 TO WS-IND.
009140	     MOVE SPACES	 TO W10-LEDG.
	     INITIALIZE DPT-RECORD.
	     DISPLAY S25.
011010	     DISPLAY "F2" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     "=Sales Ledger Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

009150 YA85.
009160       ACCEPT S25A.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA999
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER
                     CALL X"E5"
               END-EVALUATE
015990	       GO TO YA85.
019250	     DISPLAY CLEAR-L25.
009170	   IF W10-LEDG = SPACES
009180	       GO TO YA999.
	   IF W10-GROUP = "VAT"
               DISPLAY S12A.
009240	     MOVE W10-LEDG	 TO DPT-CODE.
009250       PERFORM READ-DEPART-LOCK THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE 0		 TO WS-F-ERROR
	       MOVE 1		 TO WS-IND.
	     DISPLAY S25.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Delete - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       YA90.
009260	     ACCEPT S25B.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F3-KEY
		     GO TO YA130
		 WHEN OTHER
		     CALL X"E5"
		     GO TO YA90
	       END-EVALUATE.
	   IF (LS0-LEV = 0) AND
	      (WS-IND = 1)
	       MOVE ZERO	 TO DPT-SALE-GL
				    DPT-DISC-GL
				    DPT-COST-GL
				    DPT-PUR-GL
	       GO TO YA120.

       YA95.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       YA100.
	     PERFORM YA95.
009260	     ACCEPT S25C.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE DPT-SALE-GL
				 TO W15-ACCOUNT
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO YA100
	       END-EVALUATE
	       MOVE W15-ACCOUNT  TO DPT-SALE-GL
	       DISPLAY S25C
               IF W15-ACCOUNT = ZERO
015990		   GO TO YA100
	       END-IF
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO YA90
		 WHEN DOWN-KEY
		     GO TO YA105
	       END-EVALUATE.
019780	   IF DPT-SALE-GL = ZERO
019790	       GO TO YA105.
008970	     MOVE DPT-SALE-GL	 TO W15-ACCOUNT	G-AC.
013830	   IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013850	       GO TO YA100.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
019820	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019830	       GO TO YA100.

       YA105.
	     PERFORM YA95.
009260	     ACCEPT S25D.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE DPT-DISC-GL
				 TO W15-ACCOUNT
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO YA105
	       END-EVALUATE
	       MOVE W15-ACCOUNT  TO DPT-DISC-GL
	       DISPLAY S25D
               IF W15-ACCOUNT = ZERO
015990		   GO TO YA105
	       END-IF
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO YA100
		 WHEN DOWN-KEY
		     GO TO YA110
	       END-EVALUATE.
019780	   IF DPT-DISC-GL = ZERO
019790	       GO TO YA110.
008970	     MOVE DPT-DISC-GL	 TO W15-ACCOUNT	G-AC.
013830	   IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013850	       GO TO YA105.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
019820	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019830	       GO TO YA105.

       YA110.
	     PERFORM YA95.
009260	     ACCEPT S25E.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE DPT-PUR-GL
				 TO W15-ACCOUNT
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO YA110
	       END-EVALUATE
	       MOVE W15-ACCOUNT  TO DPT-PUR-GL
	       DISPLAY S25E
               IF W15-ACCOUNT = ZERO
015990		   GO TO YA110
	       END-IF
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO YA105
		 WHEN DOWN-KEY
		     GO TO YA115
	       END-EVALUATE.
019780	   IF DPT-PUR-GL = ZERO
019790	       GO TO YA115.
008970	     MOVE DPT-PUR-GL	 TO W15-ACCOUNT	G-AC.
013830	   IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013850	       GO TO YA110.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
019820	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019830	       GO TO YA110.

       YA115.
	     PERFORM YA95.
009260	     ACCEPT S25F.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE DPT-COST-GL
				 TO W15-ACCOUNT
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO YA115
	       END-EVALUATE
	       MOVE W15-ACCOUNT  TO DPT-COST-GL
	       DISPLAY S25F
               IF W15-ACCOUNT = ZERO
015990		   GO TO YA115
	       END-IF
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO YA110
		 WHEN DOWN-KEY
		     GO TO YA120
	       END-EVALUATE.
019780	   IF DPT-COST-GL = ZERO
019790	       GO TO YA120.
008970	     MOVE DPT-COST-GL	 TO W15-ACCOUNT	G-AC.
013830	   IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013850	       GO TO YA115.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
019820	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019830	       GO TO YA115.

       YA120.
	     DISPLAY CLEAR-L25.
	     ACCEPT S25G.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO YA120
	       END-EVALUATE
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF LS0-LEV > 0
			 GO TO YA115
		     ELSE
			 GO TO YA90
		     END-IF
	       END-EVALUATE.

       YA125.
	     DISPLAY CLEAR-L25.
	     ACCEPT S25H.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO YA125
	       END-EVALUATE
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF LS0-LEV > 0
			 GO TO YA120
		     ELSE
			 GO TO YA90
		     END-IF
	       END-EVALUATE.
	   IF DPT-ALT-DD1 > 10 OR
	      DPT-ALT-DD2 > 10
	       MOVE "Alternate Discount Rate code > 10"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019830	       GO TO YA125.
009270	     MOVE W10-LEDG	 TO DPT-SALES-NO.
	   IF WS-IND = 1
	       PERFORM WRITE-DEPART THRU WRITE-DEPART-EXIT
	   ELSE
009280	       PERFORM REWRITE-DEPART-UNLOCK THRU WRITE-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       IF WS-IND = 1
		   MOVE "Error while adding record"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
	       ELSE
		   MOVE "Error while updating record"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
	       END-IF
	       GO TO YA999.
009350	     GO TO YA80.

       YA130.
	     MOVE "Delete department (Y/N)?  [N]"
				 TO WS-ERR-MES.
022870	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
029730	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO YA125.
	   IF WS-OPTION = "N"
	       GO TO YA80.
000180	     DELETE DEPART.
000190     IF WS-STAT1 NOT = "0"
000200	       MOVE 7		 TO WS-F-ERROR
000210         PERFORM WRITE-ERROR.
	     GO TO YA80.

       YA999.
	     EXIT.

       YB000	       SECTION 5.
       YB00.
	     DISPLAY "Busy Printing" AT 1212
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.

015000 YB025-SLD.
	     MOVE "H"		 TO W02-PRN-TYPE.
015010       MOVE SPACES         TO REP-DETAIL1.
015020       MOVE "PAGE:"        TO REP-PAGE.
             ADD  1              TO WS-PAGE.
015030       MOVE WS-PAGE        TO REP-P-NO.
015040       MOVE "DATE:"        TO REP-DATE.
015050       MOVE W12-TODAY      TO REP-DTE.
015060       MOVE "SALES LEDGER CODES" TO REP-COMPANY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015080	     MOVE SPACES	 TO REP-DETAIL2.
015090       MOVE "CODE"         TO REP-HEAD1.
015100       MOVE "DESCRIPTION"  TO REP-COMPANY.
	     PERFORM CALL-PRINTUTL.
015120	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "D"		 TO W02-PRN-TYPE.

       YB28.
015130	     MOVE "   !"	 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO YB35.

015140 YB030.
009250	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO YB35.
015170	     MOVE DPT-CODE	 TO REP-HEAD1.
015180       MOVE DPT-DESC       TO REP-COMPANY.
	   IF W02-LINAGE < (WS-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM YB025-SLD.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL2
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO YB999.
015220	     GO TO YB030.

       YB35.
015230	     MOVE "**** END REPORT ****" TO REP-DETAIL2.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       YB999.
	     EXIT.

      /
015920 ZA000-INIT	       SECTION 8.
015930 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
	     DISPLAY CLR-SCREEN.
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
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
016210	     MOVE 1		 TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016230       MOVE PAR-DMY        TO W12-TODAY.
016240       MOVE PAR-YMD        TO W12-T-YMD.
	     MOVE W12-MONTH	 TO WS-OPEN.
	   IF LS0-LEV > 0
	       OPEN INPUT GACCNT.
044780	     GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (16) WS-TCHR (41)
021940                              WS-TCHR (64) WS-TCHR (71).
021950	     MOVE WS-G3 	 TO WS-MCHR (11) WS-MCHR (45)
021960				    WS-MCHR (63)
				    WS-MCHR (1)	 WS-MCHR (80).
021970	     MOVE WS-G4 	 TO WS-BCHR (16) WS-BCHR (41)
021980                              WS-BCHR (64) WS-BCHR (71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2312
020130                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
020140       DISPLAY "Press any key" AT 2512 
020150                WITH FOREGROUND-COLOR 14.
020160       ACCEPT WS-OPTION AT 2526 WITH FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.

016260 ZA999-EXIT.
016270       EXIT.
      /
016280 ZB000-ERROR	     SECTION 8.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
      
041560 DISPLAY-FILE-NAME.
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041640	   ELSE
041810     IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE G-AC	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY.
041880     IF WS-STATUS = "10"
041890         MOVE "End of FILE" TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980         MOVE "DISK full" TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040         MOVE "FILE locked" TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
045390       DISPLAY "Status " AT 1212 
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
                        WS-KEY WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
