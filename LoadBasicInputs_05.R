# ReadIn

InputsWB             <-          loadWorkbook(file = 'R-Inputs.xlsx')

sheets               <-          getSheets(InputsWB)

BasicInputs          <-          readColumns(sheets$'BasicInputs',
                                             startColumn = 1,
                                             endColumn = 6,
                                             startRow = 1,
                                             endRow = 2,
                                             as.data.frame = TRUE,
                                             header = TRUE,
                                             colClasses = rep('numeric', 6))

# Read in two columns : salary to NQ / to Q
pre.salary           <-          readColumns(sheets$'PreRetirement',
                                             startColumn = 1,
                                             endColumn = 2,
                                             startRow = 1,
                                             endRow = 101,
                                             as.data.frame = TRUE,
                                             header = TRUE,
                                             colClasses = 'numeric')

Allocations.nq       <-          readColumns(sheets$'Allocs',
                                             startColumn = 2,
                                             endColumn = 11,
                                             startRow = 2,
                                             endRow = 102,
                                             as.data.frame = TRUE,
                                             header = TRUE,
                                             colClasses = 'numeric')
Allocations.q        <-          readColumns(sheets$'Allocs',
                                             startColumn = 13,
                                             endColumn = 22,
                                             startRow = 2,
                                             endRow = 102,
                                             as.data.frame = TRUE,
                                             header = TRUE,
                                             colClasses = 'numeric')

InflationTaper       <-          readColumns(sheets$'InflationTaper',
                                             startColumn = 1,
                                             endColumn = 1,
                                             startRow = 1,
                                             endRow = 101,
                                             as.data.frame = TRUE,
                                             header = TRUE,
                                             colClasses = rep('numeric', 1))

AnnuitiesAfterTax    <-          readColumns(sheets$'AnnuitiesAfterTax',
                                             startColumn = 1,
                                             endColumn = 2,
                                             startRow = 1,
                                             endRow = 101,
                                             as.data.frame = TRUE,
                                             header = TRUE,
                                             colClasses = rep('numeric', 2))

Pension              <-          readColumns(sheets$'Pension',
                                             startColumn = 1,
                                             endColumn = 1,
                                             startRow = 1,
                                             endRow = 101,
                                             as.data.frame = TRUE,
                                             header = TRUE,
                                             colClasses = rep('numeric', 1))

SocSec               <-          readColumns(sheets$'SocSec',
                                             startColumn = 1,
                                             endColumn = 1,
                                             startRow = 1,
                                             endRow = 101,
                                             as.data.frame = TRUE,
                                             header = TRUE,
                                             colClasses = rep('numeric', 1))

InfusionOfCapital    <-          readColumns(sheets$'InfusionOfCapital',
                                             startColumn = 1,
                                             endColumn = 1,
                                             startRow = 1,
                                             endRow = 101,
                                             as.data.frame = TRUE,
                                             header = TRUE,
                                             colClasses = rep('numeric', 1))


# ++++ Input basic information of pre-retirement stage ++++++++++++++++++++++



MaxERIN_05()

