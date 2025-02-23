# Banking COBOL Project

[![Language](https://img.shields.io/badge/language-COBOL-blue)](https://shields.io/) 
[![Lines of Code](https://tokei.rs/b1/github/George2Times/bank)](https://github.com/George2Times/bank)
[![License](https://img.shields.io/badge/license-MIT-green)](LICENSE)
[![Status](https://img.shields.io/badge/status-in%20development-yellow)](https://github.com/George2Times/bank)
[![Build](https://img.shields.io/badge/build-passing-brightgreen)](https://github.com/George2Times/bank/actions)
[![Contributions](https://img.shields.io/badge/contributions-welcome-brightgreen)](https://github.com/George2Times/bank)
[![Issues](https://img.shields.io/github/issues/George2Times/bank)](https://github.com/George2Times/bank/issues)
[![Last Commit](https://img.shields.io/github/last-commit/George2Times/bank)](https://github.com/George2Times/bank)

A simple banking system built using COBOL to demonstrate core functionalities such as account management, deposit, withdrawal, and balance inquiries. The project uses COBOL data files to simulate customer and transaction data storage.

---

## Project Structure

### File Structure:
```
.
├── banking.cob                 # Main COBOL program source code
├── banking.cob.bak             # Backup of the COBOL source code
├── banking.exe                 # Compiled COBOL executable (output file)
├── customer_data.dat           # Data file containing customer records
├── transaction_logs.dat        # Placeholder for logging transactions (not implemented yet)
├── README.md                   # Documentation file
├── tests/                      # Test scripts and generated output
│   ├── test.sh                 # Script to automate COBOL program tests
│   ├── deposit_test.*          # Files for deposit operation tests
│   ├── withdraw_test.*         # Files for withdrawal operation tests
│   ├── view_balance_test.*     # Files for view balance operation tests
│   ├── customer_data_test_*.dat # Generated test output data files
├── .gitignore                  # Ignored files for Git
```

---

## COBOL File Record Comparison

### Customer Records (`customer_data.dat`):
- **Structure** (59 chars):
  - `CustomerID`: 5 digits
  - `CustomerName`: 30 Alphanumeric
  - `Balance`: 7 Numeric + 2 decimal places
  - `PhoneNumber`: 15 Alphanumeric
- **Example Record**:
  ```
  10001John Smith    005001500555-1234
  ```
- **Linkage**:
  - `banking.cob` reads and updates records in `customer_data.dat` based on `CustomerID`.
  - Records are accessed for deposit/withdrawal operations, balance display, etc.

### Transaction Records (`transaction_logs.dat` - to be implemented):
- **Structure**:
  - `TransactionID`, `CustomerID`, `TransactionType`, `Amount`, `Timestamp`
- **Purpose**:
  - Tracks each operation performed on customer accounts.
  - Enables transaction auditing and history.

---

## Planned Features

### Transaction Functionality:
- Add support for `transaction_logs.dat` to record and log each operation.
- Implement options for auditing and account activity history.

---

## Setup Instructions

### Installing COBOL on Windows/Linux
1. **Follow the video tutorial**: [COBOL Installation Guide by Jay Summet](https://www.youtube.com/watch?v=st8rjU0h0JM&ab_channel=JaySummet).
2. **Install OpenCOBOL/GnuCOBOL**:
   - **Linux**:
     ```bash
     sudo apt-get update
     sudo apt-get install open-cobol
     ```
   - **Windows**:
     - Use the `Cygwin` environment. Install the `open-cobol` package using the installer.

---

## Compiling and Running the Program

1. **Compile the COBOL program**:
   ```bash
   cobc -x banking.cob -o banking.exe
   ```
2. **Run the executable**:
   ```bash
   ./banking.exe
   ```

---

## Testing the Program

### Manual Testing:
1. Run `banking.exe` and follow the on-screen prompts:
   - View balance.
   - Deposit money.
   - Withdraw money.
2. Input a valid `CustomerID` from `customer_data.dat`.
3. Validate updated balances for deposits/withdrawals.

### Automated Tests:
1. Use `test.sh` to automate tests for core functionalities:
   ```bash
   ./tests/test.sh
   ```
   - Backs up `customer_data.dat` and restores it after the tests.
   - Saves test results in `tests/` as `.out` and `.diff` files.
   - Outputs customer data to `tests/customer_data_test_output*.dat`.

2. Compare test results using `diff`:
   ```bash
   diff tests/deposit_test.out tests/deposit_test.expected
   ```

---

## TODO

### Features to Implement:
- [ ] **Transaction Logging**: Implement `transaction_logs.dat` to record every transaction.
- [ ] **Transaction History**: Allow customers to view transaction history by `CustomerID`.
- [ ] **Localization Support**: Enable support for multiple languages in the user interface.

### Testing Improvements:
- [ ] **Edge Case Testing**:
  - Invalid `CustomerID`.
  - Insufficient balance during withdrawals.
  - Corrupted or missing data files.
- [ ] **Dynamic Test Data**:
  - Automate generation of random customer data for stress testing.

### Codebase Enhancements:
- [ ] **Error Handling**:
  - Display meaningful error messages for missing/corrupted files.
  - Prevent invalid operations (e.g., negative deposits).
- [ ] **Code Refactoring**:
  - Modularize COBOL code for better readability and maintainability.
  - Use standardized naming conventions for variables and sections.
- [ ] **Interactive Menu**:
  - Add options for creating/deleting customer records.
  - Improve menu navigation for better usability.

---

Feel free to fork this repository, submit issues, or contribute to its development!