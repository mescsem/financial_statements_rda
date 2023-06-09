# Robotic Desktop Automation (RDA) of the Financial Statements from the SMV

## Description

The code leverages the RSelenium package in R for Web Scraping and Robotic Desktop Automation. It extracts financial data from the `Superintendencia del Mercado de Valores (SMV)` [website](https://www.smv.gob.pe/SIMV/Frm_InformacionFinanciera?data=A70181B60967D74090DCD93C4920AA1D769614EC12), specifically focusing on the *balance sheets* and *income statements* for a specified period. The extracted data is then parsed and stored in an Oracle SQL database.

## Process

1. Clear the current environment.
2. Load necessary libraries for web scraping, data manipulation, and database connectivity.
3. Define custom functions for data conversion and window switching.
4. Set configurations such as working directory, browser type, webpage URL, year, trimester, starting row, and database connection details.
5. Start an RSelenium session and navigate to the specified webpage.
6. Select options on the webpage based on the chosen year and trimester, and perform a search.
7. Extract a table from the webpage, filter it to keep the most recent presentation dates for each company.
8. Iterate over each company, click on links to access their financial information, and extract data from balance sheet and income statement tables.
9. Consolidate the extracted data into two data frames: balance sheet data and income statement data.
10. Close the RSelenium session and terminate the Java process used by RSelenium.
11. Save the extracted data to an Oracle SQL database using the sqlSave function.
12. Close the database connection.  