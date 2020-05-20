      $ set Ans85 Mf NoOsvs NoVsc2 NoQual NoAlter DefaultByte"00" Case
      special-names.
      call-convention 74 is staticW32Api.
      working-storage section.
       78 programName value 'RDPrnt2'.
	1 dwCount		pic xxxx    comp-5.
	1 hPrinter		pic xxxx    comp-5.
	1 DocInfo1.
	  2 pDocName			    pointer.
	  2 pOutputFile			    pointer.
	  2 pDatatype			    pointer.
	1 dwJob			pic xxxx    comp-5.
	1 dwBytesWritten	pic xxxx    comp-5.
	1 sMyDocument		pic x(12)   value z'My Document'.
	1 sDatatype		pic xxxx    value z'RAW'.
	1 OpenPrinterReturn	pic xxxx    comp-5.
	1 StartPagePrinterReturn
				pic xxxx    comp-5.
	1 WritePrinterReturn	pic xxxx    comp-5.
	1 EndPagePrinterReturn	pic xxxx    comp-5.
	1 EndDocPrinterReturn	pic xxxx    comp-5.
	1 ClosePrinterReturn	pic xxxx    comp-5.
	1 lineSelection		pic xx	    comp-5.
      * These are the escape sequences.
	1 hpCourierMedium	pic x(32)   value x'1B' & '(11U' &
				x'1B' &
				 '(s0p12.00h10.0v0s0b3T' &
				x'1B' & '&l0O'.
	1 hpBoldOn		pic xxxxx   value x'1B' & '(s3B'.
	1 hpBoldOff		pic xxxxx   value x'1B' & '(s0B'.
	1 hpUnderlineOn		pic xxxxx   value x'1B' & '&d0D'.
	1 hpUnderlineOff	pic xxxx    value x'1B' & '&d@'.
	1 hpReset		pic xx	    value x'1B' & 'E'.
	1 hpEscape		pic x	    value x'1B'.
      * This is the line that would contain your report output.
	1 hpTestLine		pic x(61) value
	      'The Quick Brown Fox jumped over the Lazy Dog''s Back'.
      * This is the work area where the print line with embedded
      * escape sequences is assembled.
	1 HPEscapeLine		pic x(256)	    value spaces.
	1 CRLF			pic xx		    value x'0D0A'.
      * Bob's "form" printing problem.
	1 boxTopLine pic x(22) value '          ' & 'ÚÄÄÄÄÄÄÄÄÄÄ¿'.
	1 boxMidLine pic x(22) value '          ' & '³          ³'.
	1 boxBotLine pic x(22) value '          ' & 'ÀÄÄÄÄÄÄÄÄÄÄÙ'.
       linkage section.
	1 szPrinterName pic x(256).
       procedure division using by reference szPrinterName.
	     display programName ': Starting.'
	     call staticW32Api 'OpenPrinterA'
		  using by reference szPrinterName
			by reference hPrinter
			by value	0 size 4
		  returning	OpenPrinterReturn
	   if OpenPrinterReturn <> 0
	       set pDocName to address of sMyDocument
	       set pOutputFile to null
	       set pDatatype to address of sDatatype
	       call staticW32Api 'StartDocPrinterA'
		    using by value hPrinter
			  by value 1 size 4
			  by reference DocInfo1
		    returning dwJob
	   if dwJob <> 0
	       call staticW32Api 'StartPagePrinter'
		    using by value hPrinter
		    returning	StartPagePrinterReturn
	   if StartPagePrinterReturn <> 0
	       perform with test after
	       varying lineSelection from 1 by 1
	       until lineSelection = 10 or WritePrinterReturn = 0
	       evaluate lineSelection
		 when 1
      * 	 This selects the font and prints the first line.
		   move spaces		to hpEscapeLine
		   move hpCourierMedium	to hpEscapeLine(11:32)
		   move hpTestLine	to hpEscapeLine(43:51)
		   move CRLF		to hpEscapeLine(94:2)
		   move 95		to dwCount
		 when 2
      * 	 This prints with an underline. If you don't turn it off,
      * 	 it will continue on the next line.
		   move spaces		to hpEscapeLine
		   move hpUnderlineOn	to hpEscapeLine(11:5)
		   move hpTestLine	to hpEscapeLine(16:51)
		   move hpUnderLineOff	to hpEscapeLine(67:4)
		   move CRLF		to hpEscapeLine(71:2)
		   move 72		to dwCount
		 when 3
      * 	 This just confirms that everything is back to normal.
		   move spaces		to hpEscapeLine
		   move hpTestLine	to hpEscapeLine(11:51)
		   move CRLF		to hpEscapeLine(62:2)
		   move 63		to dwCount
		 when 4
      * 	 This uses underline and bold.
		   move spaces		to hpEscapeLine
		   move 'Page One'	to hpEscapeLine(11: 8)
		   move hpBoldOn	to hpEscapeLine(19: 5)
		   move hpUnderlineOn	to hpEscapeLine(27: 5)
		   move hpTestLine	to hpEscapeLine(32:51)
		   move hpUnderlineOff	to hpEscapeLine(83: 4)
		   move hpBoldOff		to hpEscapeLine(87: 5)
		   move 'Contents'	to hpEscapeLine(95: 8)
		   move CRLF		to hpEscapeLine(103:2)
		   move 104		to dwCount
		 when 5
      * 	 This uses bold.
		   move spaces		to hpEscapeLine
		   move hpBoldOn	to hpEscapeLine(11:5)
		   move hpTestLine	to hpEscapeLine(16:51)
		   move CRLF		to hpEscapeLine(67:2)
		   move 68		to dwCount
		 when 6
      * 	 This is another plain line.
		   move spaces		to hpEscapeLine
		   move hpBoldOff	to hpEscapeLine(11:5)
		   move hpTestLine	to hpEscapeLine(16:51)
		   move CRLF		to hpEscapeLine(67:2)
		   move 68		to dwCount
		 when 7
		   move spaces		to hpEscapeLine
		   move boxTopLine	to hpEscapeLine
		   move CRLF		to hpEscapeLine(23:2)
		   move 24 to dwCount
		 when 8
		   move spaces		to hpEscapeLine
		   move boxMidLine	to hpEscapeLine
		   move CRLF		to hpEscapeLine(23:2)
		   move 24		to dwCount
		 when 9
		   move spaces		to hpEscapeLine
		   move boxBotLine	to hpEscapeLine
		   move CRLF		to hpEscapeLine(23:2)
		   move 24		to dwCount
		 when 10
      * 	 This resets the printer to clear the remainder of the
      * 	 report and leave the printer in it's default condition
      * 	 for the next report. It is probably a good idea to do
      * 	 this at the beginning of the report as well.
		   move spaces		to hpEscapeLine
		   move hpReset		to hpEscapeLine(1:2)
		   move 2		to dwCount
	       end-evaluate
	       call staticW32Api 'WritePrinter'
		    using by value     hPrinter
			  by reference hpEscapeLine
			  by value     dwCount
			  by reference dwBytesWritten
		    returning	     WritePrinterReturn
	       end-perform
	       if WritePrinterReturn <> 0
		   call staticW32Api 'EndPagePrinter'
			using by value hPrinter
			returning EndPagePrinterReturn
	       if EndPagePrinterReturn <> 0
		   call staticW32Api 'EndDocPrinter'
			using by value hPrinter
			returning EndDocPrinterReturn
	       if EndDocPrinterReturn <> 0
		   call staticW32Api 'ClosePrinter'
			using by value hPrinter
			returning ClosePrinterReturn
	       else
		   display 'Unable to EndDocPrinter'
		   call staticW32Api 'ClosePrinter'
		       using by value hPrinter
		       returning ClosePrinterReturn
		end-if
	      else
		display 'Unable to EndPagePrinter'
		call staticW32Api 'EndDocPrinter'
		    using by value hPrinter
		    returning EndDocPrinterReturn
		call staticW32Api 'ClosePrinter'
		    using by value hPrinter
		    returning ClosePrinterReturn
		move 1 to RETURN-CODE
	      end-if
	    else
	      display 'Unable to WritePrinter'
	      call staticW32Api 'EndPagePrinter'
		   using by value hPrinter
		   returning EndPagePrinterReturn
	      call staticW32Api 'EndDocPrinter'
		   using by value hPrinter
		   returning EndDocPrinterReturn
	      call staticW32Api 'ClosePrinter'
		   using by value hPrinter
		   returning ClosePrinterReturn
	      move 1 to RETURN-CODE
	    end-if
	    else
	      display 'Unable to StartPagePrinter'
	      call staticW32Api 'EndDocPrinter'
		   using by value hPrinter
		    returning EndDocPrinterReturn
	      call staticW32Api 'ClosePrinter'
		   using by value hPrinter
		   returning ClosePrinterReturn
	    end-if
	    else
	      call staticW32Api 'ClosePrinter'
		   using by value hPrinter
		   returning ClosePrinterReturn
	      display 'Unable to StartDocPrinter'
	    end-if
	    else
	      display 'Unable to OpenPrinter, ' szPrinterName
	    end-if
	      display programName ': Ending.'
	      exit program
	      stop run.
