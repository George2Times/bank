### README.md for Banking COBOL Project

---

# Banking COBOL Project

A simple banking system built using COBOL to demonstrate core functionalities such as account management, deposit, withdrawal, and balance inquiries. The project uses COBOL data files to simulate customer and transaction data storage.

---

## Project Structure

### File Structure:
```
.
├── banking.cob       # Main COBOL program source code
├── banking.cob.bak   # Backup of the COBOL source code
├── banking.exe       # Compiled COBOL executable (output file)
├── customer_data.dat # Data file containing customer records
├── transaction_logs.dat # Placeholder for logging transactions (not implemented yet)
├── README.md         # Documentation file
├── .gitignore        # Ignored files for Git
```

### COBOL File Record Comparison
- **Customer Records** (`customer_data.dat`):
  - **Structure**:
    ```
    CustomerID: 5 digits
    CustomerName: Alphanumeric
    Balance: Numeric with 2 decimal places
    PhoneNumber: Alphanumeric
    ```
  - Example Record:
    ```
    10001John Smith    005001500555-1234
    ```
  - **Linkage**: 
    - `banking.cob` reads and updates records in `customer_data.dat` based on `CustomerID`.
    - Records are accessed for deposit/withdrawal operations, balance display, etc.

- **Transaction Records** (`transaction_logs.dat` - to be implemented):
  - **Structure**: 
    ```
    TransactionID, CustomerID, TransactionType, Amount, Timestamp
    ```
    - Tracks each operation performed on customer accounts.
  - **Linkage**: To be integrated with banking operations to log deposits and withdrawals.

---

## Planned Features
- **Transaction Functionality**:
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

1. **Test the program manually**:
   - Run `banking.exe` and follow the on-screen prompts to:
     - View balance.
     - Deposit money.
     - Withdraw money.

2. **Expected Input and Output**:
   - Input a valid `CustomerID` from `customer_data.dat`.
   - Validate updated balances for deposits/withdrawals.

---

## Automating Tests

### Automating COBOL Program Tests:
1. Use a shell script to automate input for the COBOL program:
   ```bash
   echo "1\n10001\n" | ./banking.exe > output.txt
   ```
   - Simulates entering option `1` (View Balance) and `CustomerID: 10001`.

2. **Compare Outputs**:
   - Use a tool like `diff` to validate results:
     ```bash
     diff output.txt expected_output.txt
     ```

---

## TODO

- [ ] Implement transaction logging in `transaction_logs.dat`.
- [ ] Add support for viewing transaction history for each customer.
- [ ] Improve error handling for invalid input and corrupted data files.
- [ ] Create automated tests for edge cases (e.g., invalid `CustomerID`, insufficient balance).
- [ ] Enhance the `README.md` with visuals such as screenshots or examples.
- [ ] Refactor code for modularity and better readability.
- [ ] Add the ability to delete customer records.
- [ ] Add localization support for better usability in non-English environments.

---

Feel free to fork this repository, submit issues, or contribute to its development!