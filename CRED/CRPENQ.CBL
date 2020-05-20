      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	********  **	**   ******    *
      *   **	**  **	  **  **    **	**	  ***	**  **	  **   *
      *   **	    **	  **  **    **	**	  ****	**  **	  **   *
      *   **	    ******    *******	****	  ** ** **  **	  **   *
      *   **	    **	 **   **	**	  **  ****  **	 ***   *
      *   **	**  **	  **  **	**	  **   ***   ******    *
      *    ******   **	  **  **	********  **	**	  **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   M A I N T E N A N C E   P R O G R A M    *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.		       CRPENQ.
000030 AUTHOR.                         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.                   JUNE, 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2011
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

000070 ENVIRONMENT DIVISION.
000080 CONFIGURATION SECTION.
000090 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000110 INPUT-OUTPUT SECTION.
000120 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000160 COPY CREDIT.SL.

000140 COPY CRDMEM.SL.

000200 COPY CRTRAN.SLM.

000140 COPY PARAM.SL.

       COPY PDCHEQ.SL.

000220 COPY PORDER.SL.

000140 COPY STOCK.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000310 COPY CREDIT.FDE.

000140 COPY CRDMEM.FDE.

000330 COPY CRTRAN.FDE.

000290 COPY PARAM.FDE.

       COPY PDCHEQ.FDE.

000370 COPY PORDER.FDE.

000540 COPY STOCK.FDE.

000410 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000420 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
000430 77  WS-CTRKEY       PIC  9(04)    COMP-5 VALUE 1.
000440 77  WS-CREKEY       PIC  9(04)    COMP-5 VALUE 1.
000450 77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
000490 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
000500 77  WS-ERROR        PIC  9(01) VALUE 0.
000530 77  WS-FC           PIC  9(01) VALUE 0.
       77  WS-SUB          PIC  9(04)    COMP-5.
000550 77  WS-S            PIC  9(04)    COMP-5.
000560 77  WS-S1           PIC  9(04)    COMP-5.
000570 77  WS-S2           PIC  9(04)    COMP-5.
000580 77  WS-S3           PIC  9(04)    COMP-5.
000590 77  WS-S4           PIC  9(04)    COMP-5.
000600 77  WS-S5           PIC  9(04)    COMP-5.
000610 77  WS-S6           PIC  9(04)    COMP-5.
000620 77  WS-S7           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
002860 77  WS-IXS2         PIC  9(04)    COMP-5.
002870 77  WS-IXS3         PIC  9(04)    COMP-5.
002880 77  WS-IXS4         PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
000630 77  WS-ENT	   PIC	X(08).
000640 77  WS-ENT1         PIC  X(08).
000650 77  WS-KEYSTR       PIC  9(04)    COMP-5.
000660 77  WS-OPTION       PIC  X(01).
       77  WS-NUM          PIC  Z9.
000690 77  WS-KEY1         PIC  9(04)    COMP-5.
000750 77  WS-DTE	   PIC	X(01).
000750 77  WS-SEQ	   PIC	X(01).
000750 77  WS-CHAR1        PIC  X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
000790 77  WS-SEASON	   PIC	X(01).
000930 77  WS-ER8          PIC  X(09) VALUE "No record".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  TODAY-YYMMDD	   PIC	9(08) COMP-5.
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
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
002430	   03  WS-TP-LNE.
002440         05  WS-TCR  PIC X(01) OCCURS 80.
	   03  WS-TPCR-LNE REDEFINES WS-TP-LNE.
	       05  WS-TCR1     PIC  X(01).
	       05  WS-TCR78    PIC  X(78).
	       05  WS-TCR80    PIC  X(01).
002450	   03  WS-MD-LNE.
002460         05  WS-MCR  PIC X(01) OCCURS 80.
	   03  WS-MDCR-LNE REDEFINES WS-MD-LNE.
	       05  WS-MCR1     PIC  X(01).
	       05  WS-MCR78    PIC  X(78).
	       05  WS-MCR80    PIC  X(01).
002470	   03  WS-BT-LNE.
002480         05  WS-BCR  PIC X(01) OCCURS 80.
	   03  WS-BTCR-LNE REDEFINES WS-BT-LNE.
	       05  WS-BCR1     PIC  X(01).
	       05  WS-BCR78    PIC  X(78).
	       05  WS-BCR80    PIC  X(01).
	   03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

000980 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "CRP".
	   03  WS-PROG	       PIC  X(03) VALUE "ENQ".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

001000 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY CREDIT.ID.

000140 COPY CRDMEM.ID.

       COPY CRTRAN.ID.

       COPY PARAM.ID.

       COPY PDCHEQ.ID.

       COPY PORDER.ID.

       COPY STOCK.ID.

002250 01  W00-TRAN-CODES.
           03  W00-C-ABR   PIC  X(03).
001410     03  W00-C-DESC  PIC  X(15).
001430     03  W00-C-ACT   PIC  9(01).
001440     03  W00-C-CODE  PIC  9(02)    COMP-5.
001450     03  W00-C-GL    PIC  9(06)    COMP-3.
001460     03  W00-C-DAY   PIC  9(06)    COMP-5.
001470     03  W00-C-VAL   PIC S9(09)V99 COMP-3.
001480     03  W00-C-MTD   PIC  9(06)    COMP-5.
001490     03  W00-C-MTDV  PIC S9(09)V99 COMP-3.
001500     03  W00-C-YTD   PIC  9(06)    COMP-5.
001510     03  W00-C-YTDV  PIC S9(09)V99 COMP-3.

       COPY W05.WS.

001310 01  W10-MONTHS.
001320     03  W10-MONTH-NAMES
                             PIC  X(36) VALUE
001330                        "JanFebMarAprMayJunJulAugSepOctNovDec".
001340     03  W10-MONTH     REDEFINES W10-MONTH-NAMES
001350                       PIC  X(03) OCCURS 12.
001360 01  W15-INPUT-AREA.
001370     03  W15-DATE      PIC  X(08).
001380     03  W15-DATE-CHAR REDEFINES W15-DATE
001390                       PIC  X(01)     OCCURS 8.
003410	   03  W15-DTE	       PIC 9(08).
003420	   03  W15-RDATE       REDEFINES W15-DTE.
003430         05  W15-DD      PIC 9(02).
003440         05  W15-MM      PIC 9(02).
003450	       05  W15-CC      PIC 9(02).
003450         05  W15-YY      PIC 9(02).
001400	   03  W15-ACNO      PIC  X(06).
001410     03  W15-SNO       REDEFINES W15-ACNO.
001420         05  W15-APREF PIC  X(01).
001430         05  W15-SERIAL
			     PIC  X(05).
001440     03  W15-ITEM      PIC  9(05).
001450     03  W15-I-CODE    REDEFINES W15-ITEM.
001460         05  W15-PREFIX
                             PIC  9(03).
001470         05  W15-TYPE  PIC  9(02).
001480     03  W15-NAME.
001490         05  W15-NCHAR PIC  X(01) OCCURS 40.
001500     03  W15-N         REDEFINES W15-NAME.
001510         05  W15-DESC.
001520             07  W15-ENTRY
                             PIC  X(08).
001530             07  FILLER
                             PIC  X(16).
001540         05  FILLER    PIC  X(16).
001550     03  W15-UN        PIC  9(05).
001560     03  W15-VALUE     PIC  9(07)V99.
001570     03  W15-VAL2      PIC  9(07)V99.
004630     03  W15-ORD         PIC 9(10).
004640     03  W15-ONO         REDEFINES W15-ORD.
004650         05  W15-OPREF   PIC 9(08).
004660         05  W15-OSUB    PIC 9(02).
004820	   03  W15-DEL	       PIC 9(08).
           03  W15-DEL-DMY REDEFINES W15-DEL.
               05  W15-DEL-DD  PIC 9(02).
               05  W15-DEL-MM  PIC 9(02).
	       05  W15-DEL-CC  PIC 9(02).
               05  W15-DEL-YY  PIC 9(02).
001620     03  W15-STYLE     PIC  X(08).
001630     03  W15-COST      PIC S9(07)V99 COMP-3.
001640     03  W15-SELL      PIC S9(07)V99 COMP-3.
001650     03  W15-SETT      PIC  9(06).
001660     03  W15-SET       REDEFINES W15-SETT.
001670         05  W15-30    PIC  9(01).
001680         05  W15-60    PIC  9(01).
001690         05  W15-90    PIC  9(01).
001700         05  W15-120   PIC  9(01).
001710         05  W15-150   PIC  9(01).
001720         05  W15-180   PIC  9(01).
001730     03  W15-PAY       REDEFINES W15-SETT.
001740         05  W15-MONTH PIC  9(01)     OCCURS 6.
001760     03  W15-ADDRESS   PIC  X(120).               
001770     03  W15-ADD  REDEFINES W15-ADDRESS.              
001780         05  W15-ADDR1 PIC  X(30).          
001790         05  W15-ADDR2 PIC  X(30).             
001800         05  W15-ADDR3 PIC  X(30).          
001810         05  W15-ADDR4 PIC  X(30).          
001820     03  W15-ADD2 REDEFINES W15-ADDRESS.                     
001830         05  W15-ACHAR PIC  X(01) OCCURS 120.         
001840     03  W15-PCODE     PIC  9(04).
           03  W15-CONTACT   PIC  X(30).
           03  W15-TEL       PIC  X(14).
           03  W15-FAX       PIC  X(14).
           03  W15-TAX       PIC  X(01).
           03  W15-TERMS     PIC  X(01).
           03  W15-PMNT      PIC  X(12).

001850 01  W20-CALCS.
001860     03  W20-RESULT    PIC  9(06)V99.
001870     03  W20-RESULT-1  REDEFINES W20-RESULT.
001880         05  W20-WHOLE PIC  9(06).
001890         05  W20-DECIMAL
                             PIC  9(02).
001900     03  W20-ORD       PIC  9(07)    COMP-3.
	   03  W21-DATE.
	       05  W21-CC      PIC 99.
	       05  W21-YY      PIC 99.
	       05  W21-MM      PIC 99.
	       05  W21-DD      PIC 99.
	   03  W21-DTE REDEFINES W21-DATE
			       PIC 9(08).
	   03  W22-DTE	       PIC 9(08).
	   03  W22-TODAY REDEFINES W22-DTE.
	       05  W22-DD      PIC 99.
	       05  W22-MM      PIC 99.
	       05  W22-CC      PIC 99.
	       05  W22-YY      PIC 99.

001910 01  W25-VALUES.
001920     03  W25-CFLOW OCCURS 6 
                             PIC S9(07)V99 COMP-3.
001930     03  W25-V1        PIC S9(07)V99 COMP-3.
001940     03  W25-V2        PIC S9(07)V99 COMP-3.
001950 01  W25-EDIT.
001960     03  W25-5.
001970	       05  W25-1     PIC  ZZZZ9.
001980     03  W25-3V2.
001990         05  W25-2     PIC  ZZ9.99.
002000     03  W25-7V2.
002010         05  W25-3     PIC  Z(06)9.99.
002020     03  W25-S7V2.
002030         05  W25-VAL   PIC  Z(07)9.99-.
002040     03  W25-3N.
002050         05  W25-4     PIC  ZZ9.
002060	   03  W25-5V2.
002070	       05  W25-7     PIC  Z(05)9.99.
002060	   03  W25-S5V2.
002070         05  W25-CVAL  PIC  Z(06)9.99-.
002080     03  W25-DTE.
002090	       05  W25-DATE  PIC  Z9/99/9999.
002100     03  W25-DEL.
002110         05  W25-2N    PIC  9(02).
002120     03  W25-6.
002130         05  W25-NO    PIC  Z(07)9.

       01  W30-KEY.
000150	   03  W30-CRED	     PIC  X(06).
000050	   03  W30-SDTE	     PIC  9(08)	 COMP.
000040	   03  W30-CODE	     PIC  9(02).
000060	   03  W30-SINV.
	       05  W30-SINN  PIC  9(08).
	   03  W30-SEQ	     PIC  9(02)	 COMP-X.

       01  W35-DATES.
	   03  W35-DOC-DATE  PIC  9(08)  COMP-0 OCCURS 10.
	   03  W35-TRN-DATE  PIC  9(08)  COMP-0 OCCURS 10.

002180 01  W40-NADD.
002190     03  W40-ADD.
002200         05  W40-ACHAR PIC  X(01) OCCURS 70.
           03  W40-ADDR REDEFINES W40-ADD.
               05  W40-ADDRS PIC  X(60).
               05  FILLER    PIC  X(10).
002210     03  W40-NAME.
002220         05  W40-NCHAR PIC  X(01) OCCURS 40.

       COPY W40.WS.

       COPY FUNCTION.WS.

002240 COPY W50.WS.

       01  W75-KEYS.
           03  W75-S         PIC  9(02) COMP-5.
           03  W75-S1        PIC  9(02) COMP-5.
	   03  W75-CNO	     PIC  X(06) OCCURS 18.
	   03  W75-POR	     PIC  9(10) OCCURS 18.

003980 01  W90-TRAN.
004020     03  W90-CODE        PIC  9(02).
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.

002440 01  W95-COMPANY.
002450     03  W95-COMP      PIC  X(40).

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
002460 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S09.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W15-ACNO AUTO.

003510 01  S13.
003520	   03  LINE  3 COLUMN 32 FOREGROUND-COLOR 7 HIGHLIGHT
003530                           VALUE "CREDITOR ENQUIRIES".
010450	   03  LINE  6 COLUMN 12 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "A".
010460	   03	       COLUMN 13 VALUE "ccount or ".
010470	   03	       COLUMN 23 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "O".
010480	   03	       COLUMN 24 VALUE "rder Enquiry - ".
010490	   03	       COLUMN 39 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "E".
010560	   03	       COLUMN 40 VALUE "xit or ".
	   03	       COLUMN 47 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Esc".
	   03	       COLUMN 50 VALUE "ape".

003640 01  S14.
003650	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
003660                           VALUE "CREDITOR ENQUIRY".
003670     03  LINE  3 COLUMN  2 VALUE "Ac-No".
003680	   03	       COLUMN 15 VALUE "Name".
	   03  LINE  4 COLUMN 15 VALUE "Contact".
	   03	       COLUMN 55 VALUE "Telephone".
003690     03  LINE  5 COLUMN  2 VALUE "Addr".
003700	   03	       COLUMN 39 VALUE "³".
003710     03          COLUMN 41 VALUE "Op-Bal".
003720	   03	       COLUMN 62 VALUE "³".
003730     03          COLUMN 64 VALUE "30:".
003740	   03  LINE  6 COLUMN 39 VALUE "³".
003750     03          COLUMN 41 VALUE "Debits".
003760	   03	       COLUMN 62 VALUE "³".
003770     03          COLUMN 64 VALUE "60:".
003780	   03  LINE  7 COLUMN 39 VALUE "³".
003790     03          COLUMN 41 VALUE "Credits".
003800	   03	       COLUMN 62 VALUE "³".
003810     03          COLUMN 64 VALUE "90:".
003820	   03  LINE  8 COLUMN 39 VALUE "³".
003830     03          COLUMN 41 VALUE "Cl-Bal".
003840	   03	       COLUMN 62 VALUE "³".
003850     03          COLUMN 63 VALUE "120:".
003860     03  LINE  9 COLUMN  2 VALUE "P/C".
003870	   03	       COLUMN 39 VALUE "³".
003890	   03	       COLUMN 62 VALUE "³".
003900     03          COLUMN 63 VALUE "150:".
003910	   03  LINE 10 COLUMN 39 VALUE "³".
003880     03          COLUMN 45 VALUE "Due".
003920	   03	       COLUMN 62 VALUE "³".
003930     03          COLUMN 63 VALUE "180:".
003940     03  LINE 11 COLUMN  2 VALUE 
003950                 "Terms - :".
003960     03  LINE 12 COLUMN  2 VALUE "Transactions:". 
	   03  LINE 13 COLUMN  2 BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
003970				 VALUE " Trn Date   Inv/Ref        Type
      -		       "        Cost/Value       Selling   Age 30-180 ".

       01  S14A.
	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(40) USING P-NAME.
	   03  LINE  4 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING P-CONTACT.
	   03	       COLUMN 65 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(14) USING P-TEL.
	   03  LINE  5 COLUMN  8 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W15-ADDR1.
	   03	       COLUMN 48 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-PBAL.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-30D.
	   03  LINE  6 COLUMN  8 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W15-ADDR2.
	   03	       COLUMN 48 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-DT.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-60D.
	   03  LINE  7 COLUMN  8 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W15-ADDR3.
	   03	       COLUMN 48 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-CR.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-90D.
	   03  LINE  8 COLUMN  8 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W15-ADDR4.
	   03	       COLUMN 48 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-BAL.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-120D.
	   03  LINE  9 COLUMN  8 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC 9(04) USING P-PC.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-150D.
	   03  LINE 10 COLUMN 48 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-OUT.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-180D.
	   03  LINE 11 COLUMN 14 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(12) USING W15-PMNT.

004020 01  S15.
004030	   03  LINE  2 COLUMN 32 FOREGROUND-COLOR 7 HIGHLIGHT
004040                           VALUE "OUTSTANDING ORDERS".
004050     03  LINE  4 COLUMN  2 VALUE "Account No:".
004060     03          COLUMN 26 VALUE "Name:".
004070	   03  LINE  6 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1 VALUE
004080		       "   Date    Quant Unit-Cost Total-Cost Item code
004090- 	       "         Del   Qty   MTD M/AVG".

       01  S16.
	   03  LINE 13 COLUMN 21 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	   "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE 14 COLUMN 21 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	   "³               POST DATED CHEQUES ISSUED                ³".
	   03  LINE 15 COLUMN 21 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	   "³   Date    Narrative                             Value  ³".
	   03  LINE 16 COLUMN 21 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	   "³                                                        ³".
	   03  LINE 17 COLUMN 21 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	   "³                                                        ³".
	   03  LINE 18 COLUMN 21 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	   "³                                                        ³".
	   03  LINE 19 COLUMN 21 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	   "³                                                        ³".
	   03  LINE 20 COLUMN 21 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	   "³                                                        ³".
	   03  LINE 21 COLUMN 21 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	   "³                                                        ³".
	   03  LINE 22 COLUMN 21 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	   "³                                                        ³".
	   03  LINE 23 COLUMN 21 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 0 VALUE
	   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S16A.
	   03  COLUMN 22 BACKGROUND-COLOR 7 FOREGROUND-COLOR 1
			 PIC Z9/99/9999 FROM W15-DEL.
	   03  COLUMN 33 BACKGROUND-COLOR 7 FOREGROUND-COLOR 1
			 PIC  X(30) FROM PDT-NAR.
	   03  COLUMN 64 BACKGROUND-COLOR 7 FOREGROUND-COLOR 1
			 PIC  Z(08)9.99DB FROM PDT-VAL.

       01  S16B.
	   03  BACKGROUND-COLOR 5 FOREGROUND-COLOR 3 HIGHLIGHT
				  PIC  X(08) FROM CT-REF.

       01  S16C.
	   03  BACKGROUND-COLOR 5 FOREGROUND-COLOR 3 HIGHLIGHT
				  PIC  X(08) FROM CT-SINV.

       COPY CRDLUP.CRT.

       01  S-POR.
	   03  LINE  5 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  6 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³ Order Number ³".
	   03  LINE  7 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  8 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE  9 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 10 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 11 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 12 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 13 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 14 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 15 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 16 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 17 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 18 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 19 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 20 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 21 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 22 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³              ³".
	   03  LINE 23 COLUMN 60 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  SD-POR.
	   03  COLUMN 63 FOREGROUND-COLOR 5
			 BACKGROUND-COLOR 7 PIC Z(07)9 FROM W15-OPREF.
	   03  COLUMN 71 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
			 VALUE "/".
	   03  COLUMN 72 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
			 PIC 99 FROM W15-OSUB.

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
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
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.
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

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004910 AA000-START-UP  SECTION.
004920 AA000.
004930       PERFORM ZA000-INIT.
004940	     PERFORM GA000.

       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
011050	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM CREDITOR-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			  OPT-MESSAGE			       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'. 			       ³
      *    ³							       ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³							       ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE.				       ³
      *    ³							       ³
      *    ³ This would be displayed as:			       ³
      *    ³	ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³	³	   Print transactions (Y/N) [ ] 	 ³°°   ³
      *    ³	ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³	  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 13) move the line number to be used to SLIN and   ³
      *    ³ then PEFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      *
      *    G E T   A   C R E D I T O R   T R A N S A C T I O N
      *    R E C O R D
      *
      *    PERFORM THIS ROUTINE WITH THE TRANSACTION CODE IN W90-CODE
      *
      *    ON EXITING THE TRANSACTION CODE DETAILS ARE IN W00-
      *    WHILE W90-TRNKEY CONTAINS THE KEY TO THE PARAMETER FILE
      *    WHERE THIS TRANSACTION CODE IS FOUND AND W90-TS IS THE
      *    TRANSACTION CODE SUBSCRIPT IN THE RECORD.
      *
       GET-CREDITOR-TRAN-RECORD SECTION.

       GET-CREDITOR-TRAN.
             ADD 381 W90-CODE    GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2
                                 GIVING W90-TRNKEY
                                 REMAINDER W90-TS.
             ADD 1               TO W90-TS.

       GET-CREDITOR-TRAN-CODE.
             MOVE W90-TRNKEY     TO WS-PARKEY.

       STORE-CREDITOR-TRAN-DESC.
             MOVE PAR-C-ABR (W90-TS)
                                 TO W00-C-ABR.
             MOVE PAR-C-DESC (W90-TS)
                                 TO W00-C-DESC.
             MOVE PAR-C-ACT (W90-TS)
                                 TO W00-C-ACT.
             MOVE PAR-C-CODE (W90-TS)
                                 TO W00-C-CODE.
             MOVE PAR-C-GL   (W90-TS)
                                 TO W00-C-GL.

       STORE-CREDITOR-TRAN-STATS.
             MOVE PAR-C-DAY  (W90-TS)
                                 TO W00-C-DAY.
             MOVE PAR-C-VAL  (W90-TS)
                                 TO W00-C-VAL.
             MOVE PAR-C-MTD  (W90-TS)
                                 TO W00-C-MTD.
             MOVE PAR-C-MTDV (W90-TS)
                                 TO W00-C-MTDV.
             MOVE PAR-C-YTD  (W90-TS)
                                 TO W00-C-YTD.
             MOVE PAR-C-YTDV (W90-TS)
                                 TO W00-C-YTDV.

       GET-CREDITOR-TRAN-EXIT.
             EXIT.

       GET-CREDITOR-TRAN-DESC SECTION.

       GET-CRED-TRAN-DESC.
             PERFORM GET-CREDITOR-TRAN.
             MOVE W90-TRNKEY     TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM STORE-CREDITOR-TRAN-DESC.

       GET-CRED-TRAN-DESC-EXIT.
             EXIT.
      
      /
      *    ****    READ FILES ROUTINES
      *
005630 AB00-READ      SECTION.

       COPY CONTROL.RD.

       COPY CREDIT.RD.

000140 COPY CRDMEM.RD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Special start for displaying transactions descending Seq. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
000020 START-AT-CTRN-KEY-DESC.
000030	     START CTRANS
	       KEY < CT-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
           IF WS-STATUS = "23"
	       MOVE 4		 TO WS-F-ERROR
000050	       GO TO READ-CRTRAN-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 4		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-CRTRAN-EXIT.

       COPY CRTRAN.RD.

       COPY PARAM.RD.

       COPY PDCHEQ.RD.

       COPY PORDER.RD.

       COPY STOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
007140 AC00-WRITE    SECTION.

       COPY CONTROL.WR.

       COPY PORDER.WR.

       COPY STOCK.WR.

       COPY CREDIT.LUP.

       COPY APAC.HLP.

      /
011010 CA155-GET-CREDITOR      SECTION.
011020 CA155-ACCOUNT.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
011060	     MOVE W15-ACNO	 TO P-NUMBER.
011130       PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
011140     IF WS-F-ERROR = 3 
011150         MOVE 1 TO WS-ERROR
011160     ELSE
011170     IF P-NUMBER NOT = W15-ACNO
011180         MOVE 2 TO WS-ERROR
011190     ELSE
011200         MOVE 0 TO WS-ERROR.
011210 CA160-EXIT.
011220       EXIT.
      /
      *    ****   C R E D I T O R   E N Q U I R I E S
      *
017480 GA000	     SECTION 5.
017490 GA00.
017500       DISPLAY CLR-SCREEN.
017510       DISPLAY S13.
017520 GA005.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
015600		     GO TO GA999
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO GA005
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	   ELSE
	       CALL X"E5"
	       GO TO GA005.
      * 			   E X I T   P R O G R A M
015590	   IF WS-OPTION = "E"
015600	       GO TO GA999.
      * 			   A C C O U N T   E N Q U I R Y
015610	   IF WS-OPTION = "A"
015630	       PERFORM GB000
015640	       GO TO GA00.
      * 			   O R D E R   E N Q U I R Y
015650	   IF WS-OPTION = "O"
015670	       PERFORM GC000
015680	       GO TO GA00.
017620	     GO TO GA005.
017630 GA999.
017640       EXIT.
      /
      *    ****    A C C O U N T   E N Q U I R Y
      *
017650 GB000	     SECTION 51.
017660 GB00.
017670       DISPLAY CLR-SCREEN.
017680       DISPLAY S14.
             MOVE SPACES         TO W15-ACNO.

017690 GB05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     " to do Creditor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     " to exit "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
017700	     ACCEPT S09 AT 0308.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO GB99
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB05
	       END-EVALUATE
	       DISPLAY S09 AT 0308
	       IF W15-ACNO = SPACES
015990             GO TO GB05.
	     DISPLAY CLEAR-L25.
017710     IF W15-ACNO = SPACES
017720	       GO TO GB99.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
017740	     PERFORM CA155-GET-CREDITOR.
017750     IF WS-ERROR NOT = ZERO
017760	       MOVE WS-ER8	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017770         GO TO GB05.
017790       MOVE P-ADDR         TO W40-ADD.
015150	     UNSTRING W40-ADD DELIMITED BY "*" INTO
015160         W15-ADDR1 W15-ADDR2 W15-ADDR3 W15-ADDR4
015170	       OVERFLOW MOVE 1	 TO WS-ERROR.
	     MOVE P-PMNT	 TO W15-PMNT.
             DISPLAY S14A.
           IF P-TAX = "I"
	       DISPLAY "(Inclusive)" AT 1162
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 5
           ELSE
           IF P-TAX = "E"
	       DISPLAY "(Exclusive)" AT 1162
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 5
           ELSE
	       DISPLAY "(No VAT)" AT 1162
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 5.
	     MOVE ZERO		 TO WS-ENQEND WS-ENQPOS.

018180 GB10.
	     MOVE "Ascending/Descending Tran Date seq (A/D)  [A]"
				 TO WS-ERR-MES.
	     MOVE "A"		 TO WS-OPTION.
	     MOVE "T"		 TO WS-DTE.
	     PERFORM OPT-MESSAGE.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     GO TO GB00
               END-EVALUATE.
	   IF NOT(WS-OPTION = "A" OR "D")
	       GO TO GB10.
	     MOVE WS-OPTION	 TO WS-SEQ.
	     MOVE 1401		 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
018200       MOVE 14             TO LIN.
	     MOVE P-NUMBER	 TO CT-CRED.
	   IF WS-SEQ = "A"
	       MOVE ZERO	 TO CT-SDTE
				    CT-CODE
				    CT-SEQ
	       MOVE SPACES	 TO CT-SINV
	       PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT
	   ELSE
	       MOVE TODAY-YYMMDD TO CT-SDTE
	       MOVE 99		 TO CT-CODE
				    CT-SEQ
	       MOVE "ZZZZZZZZ"	 TO CT-SINV
	       PERFORM START-AT-CTRN-KEY-DESC THRU READ-CRTRAN-EXIT.
	   IF WS-F-ERROR = 4
	       GO TO GB25.
	     PERFORM GB65 VARYING WS-S7 FROM 1 BY 1 UNTIL WS-S7 = 10.
	     MOVE 0		 TO WS-S7.

018210 GB15.
	   IF WS-SEQ = "A"
	       PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT
	   ELSE
	       PERFORM READ-CRTRAN-PREV THRU READ-CRTRAN-EXIT.
	   IF (WS-F-ERROR = 4) OR
	      (CT-CRED NOT = P-NUMBER)
               MOVE WS-ENQPOS    TO WS-ENQEND
	       MOVE W30-KEY	 TO CT-KEY
	       PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT
	       GO TO GB25.
	     MOVE CT-KEY	 TO W30-KEY.
	     COMMIT.
	     ADD 1		 TO WS-ENQPOS WS-S7.
      *
      *    ****    S W I T C H	 D A T E   T O	 D D M M C C Y Y
      *
       GB18.
	     MOVE CT-SDTE	 TO W21-DTE.
003430	     MOVE W21-DD	 TO W15-DD.
003440	     MOVE W21-MM	 TO W15-MM.
003450	     MOVE W21-CC	 TO W15-CC.
003450	     MOVE W21-YY	 TO W15-YY.
	     MOVE W15-DTE	 TO W35-TRN-DATE(WS-S7).
018230	     MOVE CT-DTE	 TO W21-DTE.
003430	     MOVE W21-DD	 TO W15-DD.
003440	     MOVE W21-MM	 TO W15-MM.
003450	     MOVE W21-CC	 TO W15-CC.
003450	     MOVE W21-YY	 TO W15-YY.
	     MOVE W15-DTE	 TO W35-DOC-DATE(WS-S7).
	   IF WS-DTE = "T"
	       MOVE W35-TRN-DATE(WS-S7)
				 TO W25-DATE
018240	       DISPLAY W25-DTE AT LINE LIN COLUMN 2
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	   ELSE
	       MOVE W35-DOC-DATE(WS-S7)
				 TO W25-DATE
018240	       DISPLAY W25-DTE AT LINE LIN COLUMN 2
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	   IF CT-CODE = 01 OR 02
018270	       DISPLAY S16B AT LINE LIN COLUMN 14
	   ELSE
018490	       DISPLAY S16C AT LINE LIN COLUMN 14.
             MOVE CT-CODE        TO W90-CODE.
	     PERFORM GET-CREDITOR-TRAN-DESC.
	     DISPLAY W00-C-DESC AT LINE LIN COLUMN 24
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018530	     MOVE CT-COST	 TO W25-VAL.
018540	     DISPLAY W25-S7V2 AT LINE LIN COLUMN 41
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018550     IF CT-SELL NOT = ZERO
018560         MOVE CT-SELL      TO W25-VAL
018570	       DISPLAY W25-S7V2 AT LINE LIN COLUMN 55
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
018580     IF CT-AGE = ZERO
018590         GO TO GB20.
018600       MOVE ZERO           TO W15-SETT.
018610       MOVE CT-AGE         TO WS-S4.
018620     IF WS-S4 > 31
018630         MOVE 1            TO W15-180
018640         SUBTRACT 32       FROM WS-S4.
018650     IF WS-S4 > 15
018660         MOVE 1            TO W15-150
018670         SUBTRACT 16       FROM WS-S4.
018680     IF WS-S4 > 7
018690         MOVE 1            TO W15-120
018700         SUBTRACT 8        FROM WS-S4.
018710     IF WS-S4 > 3
018720         MOVE 1            TO W15-90
018730         SUBTRACT 4        FROM WS-S4.
018740     IF WS-S4 > 1
018750         MOVE 1            TO W15-60
018760         SUBTRACT 2        FROM WS-S4.
018770     IF WS-S4 > 0
018780         MOVE 1            TO W15-30.
018790	     DISPLAY W15-30 AT LINE LIN COLUMN 69
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018800	     DISPLAY W15-60 AT LINE LIN COLUMN 71
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018810	     DISPLAY W15-90 AT LINE LIN COLUMN 73
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018820	     DISPLAY W15-120 AT LINE LIN COLUMN 75
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018830	     DISPLAY W15-150 AT LINE LIN COLUMN 77
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018840	     DISPLAY W15-180 AT LINE LIN COLUMN 79
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018850 GB20.
018890	   IF LIN < 23
018940         ADD 1             TO LIN
018950         GO TO GB15.

       GB25.
023540	     DISPLAY "PgDn" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "," WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "PgUp" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "," WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to end," WITH BACKGROUND-COLOR 3
					FOREGROUND-COLOR 1
		     "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " Trn Dates,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F7" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " PD Cheques,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " Doc Dates,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F9" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " Memo/Rmks"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       GB30.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB00
		 WHEN F6-KEY
		     PERFORM GB80 THRU GB85
		     GO TO GB25
		 WHEN F7-KEY
		     PERFORM GB100
		     GO TO GB25
		 WHEN F8-KEY
		     PERFORM GB70 THRU GB75
		     GO TO GB25
		 WHEN F9-KEY
		     PERFORM GB35 THRU GB40-E
		     GO TO GB25
		 WHEN PAGE-UP
		     IF WS-ENQPOS = 0 OR 1 OR 10
			 GO TO GB25
		     ELSE
			 GO TO GB50
		     END-IF
		 WHEN PAGE-DOWN  GO TO GB45
		 WHEN OTHER	 CALL X"E5" CALL X"E5"
	       END-EVALUATE
	       GO TO GB30.
	     CALL X"E5".
	     GO TO GB30.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      M E M O	D E T A I L S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       GB35.
019250	     DISPLAY CLEAR-L25.
	     MOVE W15-ACNO	 TO CME-AC.
	     MOVE 1		 TO CME-SEQ.
	     PERFORM READ-CRDMEM THRU READ-CRDMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
	       GO TO GB40-E.
             INITIALIZE W200-MEM-FLDS.

       GB40.
	     MOVE CME-SEQ	 TO WS-S1.
	     MOVE CME-DATA	 TO W200-MEM (WS-S1).
	     PERFORM READ-CRDMEM-NEXT THRU READ-CRDMEM-EXIT.
           IF WS-F-ERROR = ZERO
	       IF CME-AC = W15-ACNO
		   GO TO GB40.
	     PERFORM SAVE-SCREEN.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.
             DISPLAY SA-MEM.
019250	     DISPLAY CLEAR-L25.
021280       DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM RESTORE-SCREEN.

       GB40-E.
	     EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			 P A G E - D O W N		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       GB45.
           IF WS-ENQPOS = WS-ENQEND
	       GO TO GB30.
	     MOVE 1401		 TO CPOS.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
018200       MOVE 14             TO LIN.
	     PERFORM GB65 VARYING WS-S7 FROM 1 BY 1 UNTIL WS-S7 = 10.
	     MOVE 0		 TO WS-S7.
023590	     GO TO GB15.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			   P A G E - U P		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
023600 GB50.
             MOVE 1              TO WS-IXS1.

       GB55.
	   IF WS-SEQ = "A"
	       PERFORM READ-CRTRAN-PREV THRU READ-CRTRAN-EXIT
	   ELSE
	       PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
	   IF WS-F-ERROR = 4
	       GO TO GB60.
	   IF CT-CRED NOT = P-NUMBER
	       IF WS-SEQ = "A"
023390		   PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT
	       ELSE
023390		   PERFORM READ-CRTRAN-PREV THRU READ-CRTRAN-EXIT
	       END-IF
	       GO TO GB60.
	     SUBTRACT 1 	 FROM WS-ENQPOS.
	     MOVE CT-KEY	 TO W30-KEY.
             ADD 1               TO WS-IXS1.
	   IF WS-IXS1 < 20
               IF WS-ENQPOS > 0
		   GO TO GB55.

       GB60.
	     MOVE 1401		 TO CPOS.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
018200       MOVE 14             TO LIN.
	     ADD 1		 TO WS-ENQPOS.
	     PERFORM GB65 VARYING WS-S7 FROM 1 BY 1 UNTIL WS-S7 = 10.
	     MOVE 1		 TO WS-S7.
	     GO TO GB18.

       GB65.
	     MOVE ZERO		 TO W35-DOC-DATE(WS-S7).

       GB70.
	     MOVE 1		 TO WS-S6.
	     MOVE 14		 TO LIN.
	     MOVE "D"		 TO WS-DTE.
	     DISPLAY "Doc" AT 1303
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT.

       GB75.
	   IF NOT(WS-S6 > WS-S7)
	       MOVE W35-DOC-DATE(WS-S6)
				 TO W25-DATE
	       DISPLAY W25-DTE AT LINE LIN COLUMN 2
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       ADD 1		 TO WS-S6 LIN
	       GO TO GB75.

       GB80.
	     MOVE 1		 TO WS-S6.
	     MOVE 14		 TO LIN.
	     MOVE "T"		 TO WS-DTE.
	     DISPLAY "Trn" AT 1303
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       GB85.
	   IF NOT(WS-S6 > WS-S7)
	       MOVE W35-TRN-DATE(WS-S6)
				 TO W25-DATE
	       DISPLAY W25-DTE AT LINE LIN COLUMN 2
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       ADD 1		 TO WS-S6 LIN
	       GO TO GB85.

       GB99.
	     EXIT.
      *
      *    ****    P D	 C H E Q U E S
      *
       GB100	       SECTION 51.
       GB105.
	     PERFORM SAVE-SCREEN.
	     MOVE 14		 TO SHADE-ROW.
	     MOVE 23		 TO SHADE-COL.
	     MOVE 56		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY S16.
	     PERFORM SCREEN-SHADOW.
019250	     DISPLAY CLEAR-L25.
	     INITIALIZE PDT-REC.
	     MOVE W15-ACNO	 TO PDT-AC.
	     PERFORM START-AT-PDCHEQ-AC THRU READ-PDCHEQ-EXIT.
	   IF WS-F-ERROR = 44
      *
      *    ****    N O	 P D   C H E Q U E S   F O R   C R E D I T O R
      *
	       GO TO GB125.

       GB110.
	     MOVE 16		 TO CLIN.

       GB115.
	     PERFORM READ-PDCHEQ-NEXT THRU READ-PDCHEQ-EXIT.
	   IF (WS-F-ERROR = 44) OR
	      (PDT-AC NOT = W15-ACNO)
      *
      *    ****    N O	 M O R E   P D	C H E Q U E S	F O R
      * 	   C R E D I T O R
      *
	       GO TO GB125.
      *
      *    ****    S W I T C H	 D A T E   T O	 D D M M C C Y Y
      *
	     MOVE PDT-DATE	 TO W21-DTE.
	     MOVE W21-CC	 TO W15-CC.
	     MOVE W21-YY	 TO W15-YY.
	     MOVE W21-MM	 TO W15-MM.
	     MOVE W21-DD	 TO W15-DD.
	     MOVE W15-DTE	 TO W15-DEL.
	     DISPLAY S16A AT LINE CLIN.
	   IF CLIN < 22
	       ADD 1		 TO CLIN
	       GO TO GB115.
	     DISPLAY "C" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "ontinue or "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 1.
       GB120.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO GB130
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB120
	       END-EVALUATE
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF WS-OPTION NOT = "C"
	       GO TO GB120.
	     DISPLAY S16.
	     GO TO GB110.
       GB125.
	     DISPLAY "***** END *****" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     " - Press " WITH BACKGROUND-COLOR 3
				      FOREGROUND-COLOR 1
		     "ANY" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     " key to continue"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       GB130.
011320	     PERFORM RESTORE-SCREEN.

019010 GB999.
019020       EXIT.
      /
      *    ****    O U T S T A N D I N G   O R D E R S
      *
019030 GC000		       SECTION 52.
019040 GC00.
019050       DISPLAY CLR-SCREEN.
019060       DISPLAY S15.
019070       MOVE SPACES         TO W15-ACNO.
019080       MOVE 8              TO LIN.

019090 GC05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Creditor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
019100       ACCEPT S09 AT 0414.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO GC999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GC05
	       END-EVALUATE
	       DISPLAY S09 AT 0414
	       IF W15-ACNO = SPACES
015990		   GO TO GC05.
	     DISPLAY CLEAR-L25.
019120     IF W15-ACNO = SPACES
019130         GO TO GC999.
019140       PERFORM CA155-GET-CREDITOR.
019150     IF WS-ERROR NOT = ZERO
019160	       MOVE WS-ER8	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019170         GO TO GC05.
019180	     DISPLAY P-NAME AT 0432
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       GC07.
000360	     MOVE 1		 TO W75-S W75-S1.

       GC08.
	     MOVE ZEROES	 TO W75-POR (W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
	       GO TO GC08.
             MOVE ZERO           TO W75-S1.

       GC09.
             MOVE P-NUMBER       TO ORD-CRED.
	     MOVE SPACES	 TO ORD-EXT-ITEM.
	     MOVE ZERO		 TO ORD-DATE.
	     MOVE "'D'ate or 'I'tem code sequence  [I]"
				 TO WS-ERR-MES.
	     MOVE "I"		 TO WS-OPTION.

       GC09A.
033910	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION NOT = "I" OR "D")
	       GO TO GC09A.
	     MOVE WS-OPTION	 TO WS-SEQ.
	   IF WS-OPTION = "D"
	       PERFORM START-AT-ORD-C-DATE THRU READ-PORDER-EXIT
	   ELSE
	       PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
019200	       MOVE "No outstanding orders"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019220         GO TO GC05.

019250 GC10.
	   IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO GC12.
019260	     PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
           IF (WS-F-ERROR = 16) OR (ORD-CRED NOT = P-NUMBER)
	       SUBTRACT 1	 FROM W75-S
               GO TO GC15.

       GC12.
	     MOVE ORD-KEY	 TO W75-POR (W75-S).
019270       MOVE ORD-COST       TO W25-V1.
019280	     MOVE W25-V1	 TO W25-3.
019290	     DIVIDE ORD-COST BY ORD-QUANT GIVING W25-7.
019300	     MOVE ORD-QUANT	 TO W25-1.
      *
      *    ****    S W I T C H	 D A T E   T O	 D D M M C C Y Y
      *
019310	     MOVE ORD-DATE	 TO W21-DTE.
003430	     MOVE W21-DD	 TO W15-DD.
003440	     MOVE W21-MM	 TO W15-MM.
003450	     MOVE W21-CC	 TO W15-CC.
003450	     MOVE W21-YY	 TO W15-YY.
	     MOVE W15-DTE	 TO W25-DATE.
019320       MOVE ORD-DELIV      TO W15-DEL.
019330	     DISPLAY W25-DTE AT LINE LIN COLUMN 2
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
019340	     DISPLAY W25-5 AT LINE LIN COLUMN 13
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
019350	     DISPLAY W25-5V2 AT LINE LIN COLUMN 19
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
019360	     DISPLAY W25-7V2 AT LINE LIN COLUMN 29
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE ORD-ITEM	 TO STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-EXT-STK = "Y"
019370	       DISPLAY ORD-EXT-ITEM AT LINE LIN COLUMN 40
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	   ELSE
	   IF WS-USE-ITM = "Y"
	       DISPLAY ORD-ITEM AT LINE LIN COLUMN 40
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
		       "/" WITH FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 5
		       ORD-ITM
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	   ELSE
	       DISPLAY ORD-ITEM AT LINE LIN COLUMN 40
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	   IF W15-DEL-MM = ZERO
019380	       DISPLAY W10-MONTH (W05-MM) AT LINE LIN COLUMN 59
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	   ELSE
019380	       DISPLAY W10-MONTH (W15-DEL-MM) AT LINE LIN COLUMN 59
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
000990	     MOVE STK-QUANT	 TO W25-1.
001000	     DISPLAY W25-5 AT LINE LIN COLUMN 63
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
001010	     MOVE STK-MTD	 TO W25-1.
001020	     DISPLAY W25-5 AT LINE LIN COLUMN 69
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
001010	     MOVE STK-MAVG	 TO W25-1.
001020	     DISPLAY W25-5 AT LINE LIN COLUMN 75
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
019480	   IF LIN < 23
019490	       ADD 1		 TO LIN W75-S
019500         GO TO GC10.

       GC15.
001020	     DISPLAY " " AT 2503
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
001030		     " highlight - " WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "PgUp" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1
			      BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     " - " WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape, " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F8" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Order Numbers,"
019520		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F3" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Delete"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       GC16.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 0		 TO W75-S1.
	     MOVE 0801		 TO CPOS.

       GC17.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.

       GC20.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO GC100
		 WHEN DOWN-KEY
001340		     GO TO GC105
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO GC20
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      GO TO GC00
		 WHEN F3-KEY
		      GO TO GC55
		 WHEN F8-KEY
		      GO TO GC45
		 WHEN PAGE-UP
		      GO TO GC35
		 WHEN PAGE-DOWN
		      GO TO GC30
		 WHEN OTHER
		      CALL X"E5"
	       END-EVALUATE.
	     CALL X"E5".
	     GO TO GC20.

       GC25.
	     DISPLAY CLEAR-L25.
             MOVE 0801           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
019550       MOVE 8              TO LIN.
	     PERFORM GC07 THRU GC08.
019560       GO TO GC10.

019570 GC30.
	   IF W75-S < 16
	       GO TO GC20.
019610	     GO TO GC25.

       GC35.
	     MOVE 1		 TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       GC40.
	     PERFORM READ-PORDER-PREV THRU READ-PORDER-EXIT.
	   IF WS-F-ERROR = 16
	       MOVE SPACE	 TO WS-OPTION
	       GO TO GC25.
	   IF WS-IXS1 < 32
               ADD 1             TO WS-IXS1
	       GO TO GC40.
	     GO TO GC25.

       GC45.
011050	     PERFORM SAVE-SCREEN.
	     DISPLAY S-POR.
	     MOVE 6		 TO SHADE-ROW.
	     MOVE 62		 TO SHADE-COL.
	     MOVE 14		 TO SHADE-WIDTH.
	     MOVE 18		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
000340	     DISPLAY CLEAR-L25.
000350	     MOVE 8		 TO CLIN.
000360	     MOVE 1		 TO W75-S1.

       GC50.
	     MOVE W75-POR (W75-S1)
				 TO W15-ORD.
	     DISPLAY SD-POR AT LINE CLIN.
	   IF W75-S1 < W75-S
	       ADD 1		 TO CLIN W75-S1
	       GO TO GC50.
	     DISPLAY "Press " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "ANY" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     " key to continue"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
011320	     PERFORM RESTORE-SCREEN.
	     GO TO GC20.

       GC55.
	     DISPLAY CLEAR-L25.
	     DISPLAY "Delete highlighted order " AT 2503
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3.
	     MOVE SPACE 	 TO WS-OPTION.
	     ACCEPT WS-OPTION AT 2532
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 3 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y")
	       PERFORM GC15
	       GO TO GC20.
	     MOVE W75-POR (W75-S1)
				 TO W15-ORD.
	     MOVE W15-ONO	 TO ORD-KEY.
	     PERFORM READ-PORDER-LOCK THRU READ-PORDER-EXIT.
	     MOVE ORD-ITEM	 TO STK-CODE.
	     PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
	     SUBTRACT ORD-QUANT  FROM STK-ORD.
	   IF NOT (STK-ORD > ZERO)
	       MOVE ZERO	 TO STK-ORD STK-DTE.
	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
	     PERFORM DELETE-PORDER-REC THRU WRITE-PORDER-EXIT.
	   IF W75-S = 1
	       GO TO GC00.
	   IF W75-S1 = 1
	       MOVE 2		 TO W75-S1
	   ELSE
	       MOVE 1		 TO W75-S1.
	     MOVE W75-POR (W75-S1)
				 TO W15-ORD.
	     MOVE W15-ONO	 TO ORD-KEY.
	     PERFORM READ-PORDER THRU READ-PORDER-EXIT.
	   IF WS-SEQ = "D"
	       PERFORM START-AT-ORD-C-DATE THRU READ-PORDER-EXIT
	   ELSE
	       PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
	     GO TO GC25.

       GC100.
           IF W75-S1 > 1
               SUBTRACT 1 FROM W75-S1 CLIN.
	     GO TO GC110.

       GC105.
	   IF W75-S1 = ZERO
	       MOVE 1		 TO W75-S1
	       GO TO GC110.
           IF W75-S1 < W75-S
	       ADD 1		 TO W75-S1 CLIN.

       GC110.
	     PERFORM GC17.
	     DISPLAY W43-DET (CLIN) AT CPOS
		     WITH BACKGROUND-COLOR 2
			  FOREGROUND-COLOR 7 HIGHLIGHT.
	     GO TO GC20.

019620 GC999.
019630       EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
019640 ZA000-INIT    SECTION 90.
019650 ZA000-START.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W22-DTE.
	     MOVE W22-DD	 TO W21-DD.
	     MOVE W22-MM	 TO W21-MM.
	     MOVE W22-CC	 TO W21-CC.
	     MOVE W22-YY	 TO W21-YY.
	     MOVE W21-DTE	 TO TODAY-YYMMDD.
	     MOVE LS-USUB	 TO WS-USUB.
019660	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
	     MOVE "CRDMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRDMEM.
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "PDCHEQ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PDCHEQ.
	     MOVE "PORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PORDER.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
019700	     MOVE 3		 TO WS-PARKEY.
019710       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
019960	     MOVE 1		 TO WS-PARKEY.
019970       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019980       MOVE PAR-COMPANY    TO W95-COMP.
019990       MOVE PAR-DMY        TO W05-DATE.
020000       MOVE PAR-YMD        TO W05-DATE-YMD.
020020	     MOVE 2		 TO WS-NETKEY.
020030       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
020100       GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MD-LNE WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCR  (WS-S1) WS-BCR  (WS-S1).
             MOVE WS-G8          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1)
                                    WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
021930       MOVE WS-G2          TO WS-TCR  (8)  WS-TCR (50)
021940                              WS-TCR  (66).
021950       MOVE WS-G3          TO WS-MCR  (8)  WS-MCR (50)
021960                              WS-MCR  (66) WS-MCR (1)
				    WS-MCR  (80).
021970       MOVE WS-G4          TO WS-BCR  (8)  WS-BCR (50)
021980                              WS-BCR  (66).
051190       MOVE WS-G9          TO WS-TCH  (1).
051200       MOVE WS-G10         TO WS-TCH  (80).
051210       MOVE WS-G11         TO WS-BCH  (1).
051220       MOVE WS-G12         TO WS-BCH  (80).
051230       MOVE WS-G14         TO WS-TCR  (1)  WS-BCR (1).
051260       MOVE WS-G13         TO WS-TCR (80)  WS-BCR (80).
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
020150		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
020160	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
020190 ZA999-EXIT.
020200       EXIT.
      /
020210 ZB000-I-O-ERROR SECTION 91.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE P-NUMBER	 TO WS-KEYX
041680	   ELSE
041690     IF WS-F-ERROR = 4
041700	       MOVE W02-CRTRAN	 TO WS-FILE
041710	       MOVE ZERO	 TO WS-KEY
	       MOVE CT-KEY	 TO WS-KEYX
	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 16
041860         MOVE W02-PORDER   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE ORD-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 24
041860	       MOVE W02-CRDMEM	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 44
041860	       MOVE W02-PDCHEQ	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE PDT-KEY	 TO WS-KEYX.
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
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
