######################################################
# ERCOT Scarcity Pricing Analysis
######################################################

## Dependencies

* R 4.4.2 (required)
* R scripts must be executed using the Command Prompt (PC) or Terminal(Mac) for the scripts to locate directories correctly -- See how to use Command Prompt below
* R scripts require LongPathsEnabled in your regedit if using Windows 10 or later - See how to Edit Long Paths below
* Stata SE or greater (for execution from console with RStata)
* Stata packages from SSC: run `ssc install estout` and `ssc install hettreatreg` once before the Stata steps
* You may need to add an exception in your virus protection software for your default ado folder and your ercot_incentive_effects repository folder. 
* RScript added to system path

## To Run

### One command (recommended)

From the **repository root**:

```sh
Rscript master.R            # full pipeline in dependency order (steps 01-06)
Rscript master.R 02 04      # run only selected steps by label
```

`master.R` runs each R driver (working directory `./R_files`) and each Stata do-file (from the
repository root) as its own clean process. Set `STATA_EXE` if Stata is not at the default
`C:/Program Files/Stata17/StataMP-64.exe`. The full step map is in the header of `master.R`. Stata
sub-do-files are numbered (`02_`, `04_`, `05_`) by their position in the pipeline.

### Main Results (manual, equivalent):

Use the Command Prompt (PC) or Terminal (Mac) to run the R script (instructions at the bottom of this document if needed): 

1. Run ./R\_files/run\_ercot\_program.R

   * Creates necessary relative folders
   * Processes source data
   * Creates main body tables 1,3,4; figures 1 and 2; select stats in main body

2. Run ./Stata/02\_underbidding\_data\_summary.do
  a. From the repository root, launch Stata and run: `do Stata/02_underbidding_data_summary.do`
     (path locals are derived from the working directory at launch; no ado-folder setup required)

   * Creates main body table 2

4. Run ./Stata/04\_underbidding\_matching.do  -- Requires ~1.8 hours on Stata/MP

   * Create main body table 6

7. Run ./R\_files/run\_rls\_underbidding\_timeseries\_model.R

   * Creates main body table 5

### Appendix:

5. Run ./Stata/05\_underbidding\_matching\_robustness.do from the repository root -- full-sample matching (test\_matching = 0) requires ~40 minutes on Stata MP; set test\_matching = 1 for a quick 5,000-observation test run (results will not match the paper)
2. Run ./R\_files/ercot\_appendix\_robustness.R
 
 
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
    
