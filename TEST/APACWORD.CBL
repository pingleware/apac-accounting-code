      $set ooctrl(+P)   mfoo
       Identification Division.
       Program-ID. MSword.

       Environment Division.
       Class-Control.
           WordClass   is class "$OLE$word.application"
           ExcelClass  is class "$OLE$excel.application"
           .

       Working-Storage Section.

       copy "mfole.cpy".


       01 WordServer                   Object Reference.
       01 TheWorkbooks                 Object Reference.
       01 TheWorkbook                  Object Reference.
       01 TheWorksheet                 Object Reference.
       01 TheRange                     Object Reference.
       01 TheApplication               Object Reference.
       01 aSelection                   Object Reference.
       01 aDocument                    Object Reference.
       01 WordDocuments                Object Reference.
       01 ExcelServer                  Object Reference.
       01 TheCharts                    Object Reference.
       01 aChart                       Object Reference.
       01 aChartArea                   Object Reference.
       01 theBorders                   Object Reference.
       01 aBorder                      Object Reference.
       01 theFonts                     Object Reference.
       01 lnksize                      PIC S9(9) COMP-5.
       01 bordertype                   PIC S9(9) COMP-5 value -3.
       01 myRange                      pic x(10).


       local-storage section.


       Procedure Division.
           Call "cob32api"

           Invoke ExcelClass   "new"  Returning ExcelServer
	   invoke excelServer  "setVisible"    using by value 1
           invoke excelServer  "getWorkbooks"  returning theWorkbooks

           invoke theWorkbooks "Add"
           Invoke ExcelServer  "getapplication" Returning TheApplication
           Invoke Theapplication "getActiveworkbook"
                                               Returning TheWorkbook
           Invoke Theapplication "getActivesheet"
                                                Returning TheWorksheet

           move "a1" to myRange
           Invoke TheWorksheet  "getRange"  using myRange
                                  Returning TheRange
           Invoke TheRange  "setValue" using "1999" & x"00"
           Invoke TheWorksheet  "getRange"  using "b1"
                                  Returning TheRange
           Invoke TheRange  "setValue" using "100" & x"00"
           Invoke TheWorksheet  "getRange"  using "c1"
                                  Returning TheRange
           Invoke TheRange  "setValue" using "200" & x"00"
           Invoke TheWorksheet  "getRange"  using "d1"
                                  Returning TheRange
           Invoke TheRange  "setValue" using "300" & x"00"
           Invoke TheWorksheet  "getRange"  using "e1"
                                  Returning TheRange
           Invoke TheRange  "setValue" using "500" & x"00"

           Invoke TheWorksheet  "getRange"  using "a1:e1"
                                  Returning TheRange
           invoke theRange "select"
           invoke excelServer "getCharts" returning theCharts
           invoke theCharts "Add"
           invoke excelServer "getActiveChart" returning aChart
           invoke aChart "getChartArea" returning aChartArea
           invoke aChartArea "select"
           invoke aChartArea "copy"

           Invoke wordClass    "new"         Returning WordServer
           Invoke wordServer   "setVisible" using by value 1
           invoke wordServer    "getDocuments" returning WordDocuments
           invoke wordDocuments "add"
           invoke wordServer "getActiveDocument" returning aDocument
           invoke wordServer "getSelection" returning aSelection
           invoke aSelection "Paste"
           invoke aSelection "TypeParagraph"
           invoke aSelection "TypeParagraph"
           invoke aSelection "TypeParagraph"
           invoke aSelection "moveUp" using by value 5 size 4, 1

           invoke aSelection "insertAfter" using "Created with :   "
           invoke aSelection "insertAfter" using "Net Express"
           invoke aSelection "moveEnd"
           invoke aSelection "movedown" using by value 5 size 4, 1
           invoke aSelection "moveup" using by value 5 size 4, 1
           invoke aSelection "moveEnd"
           invoke aSelection "moveleft" using by value 1 ,12, 1

           invoke aSelection "getfont" returning theFonts
           invoke theFonts "getBorders" returning theBorders
           invoke theBorders "getCount" returning lnksize
           move 1 to bordertype
           invoke theBorders "item" using bordertype
                                returning aBorder

           invoke aBorder "setLineStyle" using by value 1 size 4
           invoke aBorder "setLineWidth" using by value 4 size 4
           invoke aSelection "movedown" using by value 5 size 4, 1

           call "cbl_delete_file" using "c:\text1.xls" & x"00"
           invoke theWorksheet "saveAs" using "c:\text1.xls" & x"00"
           invoke excelServer "quit"
           invoke excelServer "finalize" returning excelServer

           Invoke wordServer   "setVisible" using by value 1
           call "cbl_delete_file" using "c:\text1.doc" & x"00"
           invoke aDocument "saveAs" using "c:\text1.doc" & x"00"
           invoke wordServer "quit"
           invoke wordServer "finalize" returning wordServer

           GoBack.
