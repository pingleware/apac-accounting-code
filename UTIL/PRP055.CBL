      $SET LINKCOUNT"320"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******	  ****	    ****     ******    *
      *   **	**  **	  **  **    **	 **  **    **  **   **	  **   *
      *   **	**  **	  **  **    **	**    **  **	**	 **    *
      *   *******   ******    *******	**    **  **	**     **      *
      *   **	    **	 **   **	**    **  **	**   **        *
      *   **	    **	  **  **	 **  **    **  **   **	       *
      *   **	    **	  **  **	  ****	    ****    ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   P R I N T   T I T L E   C O D E S       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRP055.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   FEBRUARY 1993.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1993 - 2008
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

000120 COPY SYSTEM.SL.

000140 COPY PARAM.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000200 COPY SYSTEM.FD.

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
000950 77  WS-S1	       PIC 9(04)     COMP-5.
000960 77  WS-S2               PIC 9(04)     COMP-5.
000970 77  WS-S3               PIC 9(04)     COMP-5.
000980 77  WS-S4               PIC 9(04)     COMP-5.
000990 77  WS-S5               PIC 9(04)     COMP-5.
001000 77  WS-S6               PIC 9(04)     COMP-5.
001000 77  WS-S7	       PIC 9(04)     COMP-5.
001000 77  WS-S8	       PIC 9(04)     COMP-5.
001010 77  WS-PARKEY           PIC 9(04)     COMP-5.
001010 77  WS-PRNKEY           PIC 9(04)     COMP-5.
001010 77  WS-DEPKEY           PIC 9(04)     COMP-5.
001010 77  WS-NETKEY           PIC 9(04)     COMP-5.
001020 77  WS-SECKEY           PIC 9(04)     COMP-5.
001070 77  WS-NUM              PIC Z9.
001070 77  WS-SNO              PIC 9(02).
       77  WS-ADV-RATE         PIC S9(03)V99 COMP-3.
001090 77  WS-PAGE             PIC 9(04)     COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
001100 77  WS-OPTION	       PIC X(01).
001100 77  WS-VAT-GST          PIC X(01).
001120 77  WS-ERROR            PIC 9(01).
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
001140 77  WS-CHAR1	       PIC X(01).
001150 77  WS-CHAR2            PIC X(01).
001160 77  WS-COMP             PIC 9(01).
001170 77  WS-TYPE             PIC X(01).
001200 77  WS-INDEX-STORE      PIC X(04).
001210 77  WS-INDEX-NAME       PIC X(04).
001220 77  WS-BATCH            PIC 9(05).
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
002450     03  WS-MID-LNE2.
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

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

       COPY SYSTEM.ID.

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
001670     03  W10-LEDG       PIC 9(03).
001680	   03  W10-DATE       PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       COPY W12.WS.

       01  W15-CONTROL.
           03  W15-DEBINV     PIC  9(08)    COMP-3.
           03  W15-DEBREC     PIC  9(08)    COMP-3.
           03  W15-DEBORD     PIC  9(08)    COMP-3.
           03  W15-DEBJOB     PIC  9(08)    COMP-3.
           03  W15-CRDORD     PIC  9(08)    COMP-3.
001490 01  W15-HEX.
001500     03  W15-ASCII       PIC X(01) OCCURS 256.

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
       01  W50-PRINTERS.
           03  W50-TRM-PRINTER.
000470         05  W50-PICP    PIC  9(01).
               05  W50-PRN1    PIC  9(01).
000480         05  W50-INVP    PIC  9(01).
               05  W50-PRN2    PIC  9(01).
000490         05  W50-STDP    PIC  9(01).
               05  W50-PRN3    PIC  9(01).
000500         05  W50-STMP    PIC  9(01).
               05  W50-PRN4    PIC  9(01).
000510         05  W50-CSHP    PIC  9(01).
               05  W50-PRN5    PIC  9(01).
000520         05  W50-POS     PIC  9(01).
               05  W50-PRN6    PIC  9(01).
           03  W50-TRM-PRN-CONT.
      *                                     CONDENSED PRINT
000230         05  W50-CONP.
                 07  W50-CHAR1  PIC  X(01) OCCURS 4.
      *                                     CANCEL CONDENSED PRINT
000240         05  W50-NORP.
                 07  W50-CHAR2  PIC  X(01) OCCURS 4.
      *                                     EXPANDED PRINT
000250         05  W50-EXPP.
                 07  W50-CHAR3  PIC  X(01) OCCURS 4.
      *                                     CANCEL EXPANDED PRINT
000260         05  W50-ECAN.
                 07  W50-CHAR4  PIC  X(01) OCCURS 4.
      *                                     SET PAGE LENGTH TO 48
000270         05  W50-8LPI.
                 07  W50-CHAR5  PIC  X(01) OCCURS 4.
      *                                     SET PAGE LENGTH TO 66
000280         05  W50-6LPI.
                 07  W50-CHAR6  PIC  X(01) OCCURS 4.
      *                                     PRINT 10 CHARS PER INCH
000290         05  W50-10CPI.
                 07  W50-CHAR7  PIC  X(01) OCCURS 4.
      *                                     PRINT 12 CHARS PER INCH
000300         05  W50-12CPI.
                 07  W50-CHAR8  PIC  X(01) OCCURS 4.
      *                                     PRINT 17 CHARS PER INCH
000310         05  W50-17CPI.
                 07  W50-CHAR9  PIC  X(01) OCCURS 4.
      *                                     SET DOUBLE PRINT
000320         05  W50-DBLP.
                 07  W50-CHAR10 PIC  X(01) OCCURS 4.
      *                                     RELEASE DOUBLE PRINT
000330         05  W50-DCAN.
                 07  W50-CHAR11 PIC  X(01) OCCURS 4.
           03  W50-PRINTER     PIC  9(01).
           03  W50-TERMINAL    PIC  9(03).

       COPY FUNCTION.WS.

002420 01  W60-NME-ADD.
002430     03  W60-CSHEAD     PIC  X(32).
002440 01  W65-ENTRY.
002450     03  W65-FOUR.
002460         05  W65-CHAR OCCURS 4 PIC X(01).
002670 01  W75-TITLES.
002680     03  W75-DESCRIP.
002690        05  FILLER       PIC X(28)       VALUE
002700                     "Mr     Mnr    Mrs    Mev    ".
002710         05  FILLER      PIC X(28)       VALUE
002720                     "Miss   Mej    Ms     Ms     ".
002730         05  FILLER      PIC X(28)       VALUE
002740                     "Dr     Dr     Rev    Ds     ".
002750         05  FILLER      PIC X(28)       VALUE
002760                     "Prof   Prof   The HonSy Edel".
002770         05  FILLER      PIC X(28)       VALUE
002780                     "                            ".
002790     03  W75-CODES REDEFINES W75-DESCRIP.
002800         05  W75-TITLE  OCCURS 10.
002810             07  W75-ETITLE PIC X(07).
002820             07  W75-ATITLE PIC X(07).
002830 01  W80-ACCOUNT.
002840     03  W80-ACNO       PIC X(06).
002850     03  W80-NUMD REDEFINES W80-ACNO.
002860         05  W80-COMP   PIC 9(01).
002870         05  W80-TYPE   PIC 9(01).
002880         05  W80-SERIAL PIC 9(04).
           03  W80-CASH-GL    PIC 9(06).
       01  W90-CALC-KEYS.
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.
004020     03  W90-CATKEY      PIC  9(04)    COMP-5.
004020     03  W90-CS          PIC  9(02)    COMP-5.

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

       COPY ERROR.CRT.

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
003240	   IF LS0-PALEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.
	     PERFORM YB000.

       AA49.
             EXIT PROGRAM.

       COPY LOCKED.RC2.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

       AA200		  SECTION.
       AA201.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     WS-BLNK78 WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
019260	     ADD 1		 TO CLIN.

       AA205.
             EXIT.

      /
      *    ****    READ FILES ROUTINES
      *
006630 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

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

015620 YB150.
	     MOVE "H"		 TO W02-PRN-TYPE.
015630       MOVE 1              TO WS-S1.
015640       MOVE SPACES         TO REP-DETAIL1.
015650       MOVE "PAGE:"        TO REP-PAGE.
015660       MOVE 1              TO REP-P-NO.
015670       MOVE "DATE:"        TO REP-DATE.
015680       MOVE W12-TODAY      TO REP-DTE.
015690       MOVE "      TITLE CODES" 
                                 TO REP-COMPANY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015710	     MOVE SPACES	 TO REP-DETAIL1.
015720       MOVE "CODE"         TO REP-HEAD1.
015730       MOVE "ENG TITLE"    TO REP-HEAD2.
015740       MOVE "AFR TITLE"    TO REP-HEAD3.
	     PERFORM CALL-PRINTUTL.
015760	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "D"		 TO W02-PRN-TYPE.

015770 YB155.
015780     IF W75-ETITLE (WS-S1) = SPACES
015790         GO TO YB160.
015800       MOVE WS-S1          TO REP-TRAN.
015810       MOVE W75-ETITLE (WS-S1) 
                                 TO REP-ENG.
015820       MOVE W75-ATITLE (WS-S1) 
                                 TO REP-AFR.
	     PERFORM CALL-PRINTUTL.
015840	     MOVE SPACES	 TO REP-DETAIL2.

015850 YB160.
015860     IF WS-S1 < 10
015870         ADD 1              TO WS-S1
015880         GO TO YB155.
015590	     MOVE "**** END REPORT ****"
                                 TO REP-COMPANY.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       YB999.
	     EXIT.
      /
015920 ZA000-INIT	       SECTION 8 .
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
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SYSTEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SYSTEM.
	     MOVE 9		 TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-GST    TO WS-VAT-GST.

       ZA27.
016210       MOVE 1              TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016230       MOVE PAR-DMY        TO W12-TODAY.
016240       MOVE PAR-YMD        TO W12-T-YMD.
008740	     OPEN INPUT SECUR.
007570	     MOVE 1		 TO WS-SECKEY.

       ZA28.
007580       READ SECUR.
007590     IF RUNTIME-ERROR
007600         IF RECORD-LOCKED
007610             GO TO ZA28.
007620     IF WS-STATUS NOT = "00"
007630         DISPLAY "Disk error - ZA30" AT 2312
                        WITH FOREGROUND-COLOR 14
007640         STOP RUN.
007650	     MOVE 1		 TO WS-S1.

007660 ZA30.
007670       MOVE SEC-ASCII (WS-S1) TO W15-ASCII (WS-S1).
007680     IF WS-S1 < 256
007690         ADD 1 TO WS-S1
007700         GO TO ZA30.
             CLOSE SECUR.
044780       GO TO ZA999-EXIT.
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
021950       MOVE WS-G3          TO WS-MCHR (16) WS-MCHR (41)
021960                              WS-MCHR (64) WS-MCHR (71)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (16) WS-BCHR (41)
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
