       IDENTIFICATION DIVISION.
       PROGRAM-ID. BankingSystem.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CustomerFile ASSIGN TO "customer_data.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TransactionLogFile ASSIGN TO "transaction_logs.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CustomerFile.
       01 CustomerRecord.
           05 CustomerID      PIC X(10).
           05 CustomerName    PIC X(30).
           05 Balance         PIC 9(7)V99.
           05 PhoneNumber     PIC X(15).

       FD TransactionLogFile.
       01 TransactionRecord.
           05 TransactionID   PIC 9(6).
           05 TransCustomerID PIC X(10).
           05 TransType       PIC X(8).
           05 TransAmount     PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01 MenuChoice         PIC 9 VALUE 0.
       01 InputCustomerID    PIC X(10).
       01 InputAmount        PIC 9(7)V99.
       01 IsFound            PIC X VALUE "N".
       01 TempBalance        PIC 9(7)V99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM DISPLAY-MENU
           PERFORM UNTIL MenuChoice = 9
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
           END-PERFORM
           DISPLAY "Thank you for using the Banking System!"
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY "--------------------------------------".
           DISPLAY "Welcome to Banking System".
           DISPLAY "1. View Balance".
           DISPLAY "2. Deposit Money".
           DISPLAY "3. Withdraw Money".
           DISPLAY "9. Exit".
           DISPLAY "Enter your choice: ".
           ACCEPT MenuChoice.

       VIEW-BALANCE.
           DISPLAY "Enter Customer ID: ".
           ACCEPT InputCustomerID.
           OPEN INPUT CustomerFile
           PERFORM UNTIL EOF(CustomerFile)
               READ CustomerFile INTO CustomerRecord
                   AT END
                       MOVE "Y" TO IsFound
                   NOT AT END
                       IF CustomerID = InputCustomerID
                           DISPLAY "Customer Name: " CustomerName
                           DISPLAY "Current Balance: $" Balance
                           MOVE "Y" TO IsFound
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CustomerFile
           IF IsFound NOT = "Y"
               DISPLAY "Customer not found."
           END-IF.

       DEPOSIT-MONEY.
           DISPLAY "Enter Customer ID: ".
           ACCEPT InputCustomerID.
           DISPLAY "Enter Amount to Deposit: ".
           ACCEPT InputAmount.
           OPEN I-O CustomerFile
           PERFORM UNTIL EOF(CustomerFile)
               READ CustomerFile INTO CustomerRecord
                   AT END
                       MOVE "Y" TO IsFound
                   NOT AT END
                       IF CustomerID = InputCustomerID
                           ADD InputAmount TO Balance
                           REWRITE CustomerRecord
                           DISPLAY "Deposit Successful!"
                           DISPLAY "New Balance: $" Balance
                           MOVE "Y" TO IsFound
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CustomerFile
           IF IsFound NOT = "Y"
               DISPLAY "Customer not found."
           END-IF.

       WITHDRAW-MONEY.
           DISPLAY "Enter Customer ID: ".
           ACCEPT InputCustomerID.
           DISPLAY "Enter Amount to Withdraw: ".
           ACCEPT InputAmount.
           OPEN I-O CustomerFile
           PERFORM UNTIL EOF(CustomerFile)
               READ CustomerFile INTO CustomerRecord
                   AT END
                       MOVE "Y" TO IsFound
                   NOT AT END
                       IF CustomerID = InputCustomerID
                           IF Balance >= InputAmount
                               SUBTRACT InputAmount FROM Balance
                               REWRITE CustomerRecord
                               DISPLAY "Withdrawal Successful!"
                               DISPLAY "New Balance: $" Balance
                               MOVE "Y" TO IsFound
                           ELSE
                               DISPLAY "Insufficient Balance."
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CustomerFile
           IF IsFound NOT = "Y"
               DISPLAY "Customer not found."
           END-IF.
