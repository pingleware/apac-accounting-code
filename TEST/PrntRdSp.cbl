      $ set Ans85 Mf NoOsvs NoVsc2 NoQual NoAlter DefaultByte"00"
       special-names.
           call-convention 8 is staticCobol.
       working-storage section.
           78 programName value 'PrntRdSp'.
           1  szPrinterName pic x(256).
       procedure division.
           display programName ': Starting.'
           call staticCobol 'PrintI2'
                using by reference szPrinterName
           if RETURN-CODE = 0
               call staticCobol 'RDPrnt2'
                    using by reference szPrinterName
               if RETURN-CODE = 0
                   continue
               else
                   display programName ': Printing failed.'
               end-if
           else
               display programName ': Printer name not available.'
           end-if
           display programName ': Ending.'
           exit program
           stop run.
