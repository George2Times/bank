#!/bin/bash

# Backup original customer data
echo "Backing up original customer_data.dat..."
cp customer_data.dat customer_data_backup.dat

# Set up test data
echo "Setting up test data for customer_data.dat..."
cp tests/test_customer_data.dat customer_data.dat

# Define test output file
TEST_DATA_FILE_OUTPUT1="tests/customer_data_test_output1.dat"
TEST_DATA_FILE_OUTPUT2="tests/customer_data_test_output2.dat"
TEST_DATA_FILE_OUTPUT3="tests/customer_data_test_output3.dat"

# Run Deposit Test // balance = 0050000,00 USD + 15 USD
echo "Running Deposit Test..."
printf "2\n10001\n15\n9\n" | ./banking.exe > tests/deposit_test.out
# Save test output customer_data.dat in TEST_DATA_FILE_OUTPUT
echo "Saving test output in $TEST_DATA_FILE_OUTPUT1..."
mv customer_data.dat $TEST_DATA_FILE_OUTPUT1
diff -Z tests/deposit_test.expected $TEST_DATA_FILE_OUTPUT1 > tests/deposit_test.diff
if [ $? -eq 0 ]; then
    echo "+1 Deposit Test Passed!"
else
    echo "-1 Deposit Test Failed. See ./tests/deposit_test.diff for details."
fi

# Restore original test data before next test
cp tests/test_customer_data.dat customer_data.dat

# Run Withdraw Test // balance = 0050000,00 USD - 10 USD
echo "Running Withdraw Test..."
printf "3\n10001\n10\n9\n" | ./banking.exe > tests/withdraw_test.out
# Save test output customer_data.dat in TEST_DATA_FILE_OUTPUT
echo "Saving test output in $TEST_DATA_FILE_OUTPUT2..."
mv customer_data.dat $TEST_DATA_FILE_OUTPUT2
diff -Z tests/withdraw_test.expected $TEST_DATA_FILE_OUTPUT2 > tests/withdraw_test.diff
if [ $? -eq 0 ]; then
    echo "+1 Withdraw Test Passed!"
else
    echo "-1 Withdraw Test Failed. See ./tests/withdraw_test.diff for details."
fi

# Restore original test data before next test
cp tests/test_customer_data.dat customer_data.dat

# Run View Balance Test  // balance = 0050000,00 USD
echo "Running View Balance Test..."
printf "1\n10001\n9\n" | ./banking.exe > tests/view_balance_test.out
# Save test output customer_data.dat in TEST_DATA_FILE_OUTPUT
echo "Saving test output in $TEST_DATA_FILE_OUTPUT3..."
mv customer_data.dat $TEST_DATA_FILE_OUTPUT3
diff -Z tests/view_balance_test.expected $TEST_DATA_FILE_OUTPUT3 > tests/view_balance_test.diff
if [ $? -eq 0 ]; then
    echo "+1 View Balance Test Passed!"
else
    echo "-1 View Balance Test Failed. See ./tests/view_balance_test.diff for details."
fi

# Restore original customer data
echo "Restoring original customer_data.dat..."
mv customer_data_backup.dat customer_data.dat

echo "All tests completed!"
