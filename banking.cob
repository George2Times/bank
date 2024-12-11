       IDENTIFICATION DIVISION.
       PROGRAM-ID. BankingSystem.  *> Program name

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CustomerFile ASSIGN TO "customer_data.dat"
               ORGANIZATION IS LINE SEQUENTIAL.  *> File for customer records
           SELECT TransactionLogFile ASSIGN TO "transaction_logs.dat"
               ORGANIZATION IS LINE SEQUENTIAL.  *> File for transaction logs

       DATA DIVISION.
       FILE SECTION.
       FD CustomerFile.
       01 CustomerRecord.
           05 CustomerID      PIC X(5).    *> Customer ID field
           05 CustomerName    PIC X(30).   *> Customer name field
           05 Balance         PIC 9(7)V99. *> Customer balance (7 digits + 2 decimals)
           05 PhoneNumber     PIC X(15).   *> Customer phone number field

       FD TransactionLogFile.
       01 TransactionRecord.
           05 TransactionID   PIC 9(6).    *> Transaction ID field
           05 TransCustomerID PIC X(5).    *> Customer ID associated with the transaction
           05 TransType       PIC X(8).    *> Transaction type (e.g., Deposit/Withdraw)
           05 TransAmount     PIC 9(7)V99. *> Transaction amount

       WORKING-STORAGE SECTION.
       01 MenuChoice         PIC 9 VALUE 0.   *> User menu choice
       01 InputCustomerID    PIC X(5).        *> Input for customer ID
       01 InputAmount        PIC 9(7)V99.     *> Input for transaction amount
       01 EOF                PIC X VALUE 'N'. *> End of file marker
       01 ERR                PIC X VALUE 'N'. *> Error marker
       01 IsFound            PIC X VALUE "N". *> Flag to indicate if customer is found
       01 TempBalance        PIC 9(7)V99.     *> Temporary storage for balance operations

       PROCEDURE DIVISION.
       MAIN-PARA.
           *> Display menu and process user input in a loop
           PERFORM DISPLAY-MENU
           DISPLAY "Current Menu Choice: " MenuChoice.
           DISPLAY "ERR Status: " ERR.
           PERFORM UNTIL MenuChoice = 9 OR ERR = 'Y'
               EVALUATE MenuChoice
                   WHEN 1
                       PERFORM VIEW-BALANCE
                   WHEN 2
                       PERFORM DEPOSIT-MONEY
                   WHEN 3
                       PERFORM WITHDRAW-MONEY
                   WHEN OTHER
                       DISPLAY "Invalid Choice. Try again."
               END-EVALUATE
               PERFORM DISPLAY-MENU
               DISPLAY "Current Menu Choice: " MenuChoice
               DISPLAY "Error Status: " ERR
           END-PERFORM
           DISPLAY "Thank you for using the Banking System!"
           STOP RUN.

       DISPLAY-MENU.
           *> Display the main menu
           DISPLAY "--------------------------------------".
           DISPLAY "Welcome to Banking System".
           DISPLAY "1. View Balance".
           DISPLAY "2. Deposit Money".
           DISPLAY "3. Withdraw Money".
           DISPLAY "9. Exit".
           DISPLAY "Enter your choice: ".
           ACCEPT MenuChoice
               ON EXCEPTION
                    MOVE 'Y' TO ERR.

       VIEW-BALANCE.
           *> View the balance of a specific customer
           MOVE 'N' TO EOF
           DISPLAY "Enter Customer ID: ".
           ACCEPT InputCustomerID.
           OPEN INPUT CustomerFile
           PERFORM UNTIL EOF = "Y"
               READ CustomerFile INTO CustomerRecord
                   AT END
                        MOVE "Y" TO EOF
                   NOT AT END
                        IF CustomerID = InputCustomerID
                            DISPLAY "Customer Name: #" CustomerName "#"
                            DISPLAY "Customer Balance: #" Balance "#"
                            DISPLAY "Phone Number: #" PhoneNumber "#"
                            MOVE "Y" TO IsFound
                        END-IF
               END-READ
           END-PERFORM.
           CLOSE CustomerFile
           IF IsFound NOT = "Y"
               DISPLAY "Customer not found."
           END-IF.

       DEPOSIT-MONEY.
           *> Reset flags and prompt user for input
           MOVE 'N' TO EOF
           DISPLAY "Enter Customer ID: "
           ACCEPT InputCustomerID
           DISPLAY "Enter Amount to Deposit: "
           ACCEPT InputAmount
           
           *> Open the customer file for input-output operations
           OPEN I-O CustomerFile
           
           *> Search for the customer and update the balance
           PERFORM UNTIL EOF = "Y"
               READ CustomerFile INTO CustomerRecord
                   AT END
                       MOVE "Y" TO EOF
                   NOT AT END
                       IF CustomerID = InputCustomerID
                           ADD InputAmount TO Balance
                           DISPLAY "Before REWRITE: " CustomerRecord
                           REWRITE CustomerRecord
                           DISPLAY "After REWRITE: " CustomerRecord
                           DISPLAY "Deposit Successful!"
                           DISPLAY "New Balance: $" Balance
                           MOVE "Y" TO IsFound
                       END-IF
               END-READ
           END-PERFORM
           
           *> Close the customer file and handle errors
           CLOSE CustomerFile
           IF IsFound NOT = "Y"
               DISPLAY "Customer not found."
           END-IF.

       WITHDRAW-MONEY.
           *> Subtract money from a customer's account
           MOVE 'N' TO EOF
           DISPLAY "Enter Customer ID: ".
           ACCEPT InputCustomerID.
           DISPLAY "Enter Amount to Withdraw: ".
           ACCEPT InputAmount.
           OPEN I-O CustomerFile
           PERFORM UNTIL EOF = 'Y'
               READ CustomerFile INTO CustomerRecord
                    AT END
                        MOVE 'Y' TO EOF
                    NOT AT END
                        IF CustomerID = InputCustomerID
                            IF Balance < InputAmount
                                DISPLAY "Insufficient Balance."
                            ELSE
                                SUBTRACT InputAmount FROM Balance
                                DISPLAY "Before REWRITE: " 
                                CustomerRecord
                                REWRITE CustomerRecord
                                DISPLAY "After REWRITE: " 
                                CustomerRecord
                                DISPLAY "Withdrawal Successful!"
                                DISPLAY "New Balance: $" Balance
                                MOVE 'Y' TO IsFound
                            END-IF
                        END-IF
               END-READ
           END-PERFORM
           CLOSE CustomerFile
           IF IsFound NOT = 'Y'
                   DISPLAY "Customer not found."
           END-IF.
