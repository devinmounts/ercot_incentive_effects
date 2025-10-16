######################################################
# ERCOT Scarcity Pricing Analysis
######################################################

## Dependencies

* R 4.4.2 (required)
* R scripts must be executed using the Command Prompt (PC) or Terminal(Mac) for the scripts to locate directories correctly -- See how to use Command Prompt below
* R scripts require LongPathsEnabled in your regedit if using Windows 10 or later - See how to Edit Long Paths below
* Stata SE or greater (for execution from console with RStata)
* You may need to add an exception in your virus protection software for your default ado folder and your ercot_incentive_effects repository folder. 
* RScript added to system path

## To Run

### Main Results:

Use the Command Prompt (PC) or Terminal (Mac) to run the R script (instructions at the bottom of this document if needed): 

1. Run ./R\_files/run\_ercot\_program.R

   * Creates necessary relative folders
   * Processes source data
   * Creates main body tables 1,3,4; figures 1 and 2; select stats in main body

2. Run ./Stata/underbidding\_data\_summary.do
  a. Adjust absolute file path in ./Stata/cd\_edata.ado to {absolute\_reference}/Data/ERCOT Compiled Data"
  b. Adjust absolute file path in ./Stata/cd\_estats.ado to {absolute\_reference}/Tables/Summary Stats"
  c. Adjust absolute file path in ./Stata/cd\_underbid_robustness.ado to {absolute\_reference}/Tables/Regressions/underbidding/robustness"
  d. Adjust absolute file path in ./Stata/cd\_underbid.ado to {absolute\_reference}/Tables/Regressions/underbidding"
  e. Copy the Stata files to your C:/ado folder, or your machine's default Stata ado directory.
  f. Open each ado file and run - CNTRL+D
  g. Open and run underbidding\_data\_summary.do
  
   * Creates main body table 2

6. Run ./Stata/underbidding\_matching.do  -- Requires 0.5-4.0 hours

   * Create main body table 6

7. Run ./R\_files/run\_rls\_underbidding\_timeseries\_model.R

   * Creates main body table 5

### Appendix:

1. Adjsut absolute file path in ./Stata/cd\_underbid\_robustness.ado to {absolute\_reference}/Tables/Regressions/underbidding/robustness"
2. Run ./Stata/underbidding\_matching\_robustness.do
3. Run ./R\_files/ercot\_appendix\_robustness.R
 
 
######################################################
### Command Prompt - setup and execution instructions
######################################################

To run `.R` files from the Windows Terminal using the `Rscript` command, follow these steps:

## Add `Rscript` to Windows PATH

---

### 1. Locate the `Rscript.exe` Executable

- Open **File Explorer**.
- Navigate to where R is installed. It's usually in:

  `C:\Program Files\R\R-4.4.2\bin\x64`  -If a 64-bit computer, otherwise, omit "\x64""

- Inside the `bin` folder, locate `Rscript.exe`.
- Copy the full path to the clipboard, for example:

  `C:\Program Files\R\R-4.4.2\bin\x64`

---

### 2. Open Environment Variables

- Press `Windows + S` and search for:

  **Edit the system environment variables**

- Click to open it.
- In the **System Properties** window, click the **"Environment Variables"** button near the bottom.

---

### 3. Edit the System `Path` Variable

- In the **Environment Variables** window:
  - Under **System variables**, scroll to find and select the variable named `Path`.
  - Click **"Edit"**.

---

### 4. Add the Rscript Path

- In the **Edit Environment Variable** window:
  - Click **"New"**.
  - Paste the path you copied earlier, for example:

    `C:\Program Files\R\R-4.4.2\bin\x64`

- Click **OK** to save and close all windows.

### 5. Verify Installation

- Open a **new** Command Prompt or PowerShell window (restart it if it was already open).
- Type:

  ```sh
    Rscript --version
  ```
  - You should see something like:
  ```sh
    R scripting front-end version 4.4.2
  ```

- You should now be able to run .R programs with:
  ```sh 
    Rscript path\to\your_script.R
  ```
- Alternatively use change directory 

    cd path\to\your_script.R
  
  You can then execute
  
    Rscript your_script.R
    

######################################################
### Enable Long Path Support in Windows (Windows 10, version 1607 and later):
######################################################
    - Registry Editor:
      -  Open the Run dialog (Windows key + R), type regedit, and press Enter.
      -  Navigate to HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\FileSystem.
      -  Locate LongPathsEnabled, double-click it, and change its Value data to 1.
      -  Restart your computer.
    
