##### Loops ######
##### For Loop Flow Diagram
### Example 1:
# R program to demonstrate the use of for loop

# using for loop
for (val in 1: 5)
{
  # statement
  print(val)
}
#### Example 2:
# R program to illustrate
# application of for loop

# assigning strings to the vector
week < - c('Sunday',
           'Monday',
           'Tuesday',
           'Wednesday',
           'Thursday',
           'Friday',
           'Saturday')

# using for loop to iterate
# over each string in the vector
for (day in week)
{
# displaying each string in the vector
  print(day)
}
##### WHIL Loop Flow Diagram
### Example 1:
# R program to demonstrate the use of while loop

val = 1

# using while loop
while (val <= 5)
{
# statements
print(val)
val = val + 1
}
### Example 2
# R program to illustrate
# application of while loop

# assigning value to the variable
# whose factorial will be calculated
n < - 5

# assigning the factorial variable
# and iteration variable to 1
factorial < - 1
i < - 1

# using while loop
while (i <= n)
{
  
# multiplying the factorial variable
# with the iteration variable
  factorial = factorial * i
  
# incrementing the iteration variable
  i = i + 1
}

# displaying the factorial
print(factorial)
##### Repeat Loop Flow Diagram
### Example 1:
program to demonstrate the use of repeat loop

val = 1

# using repeat loop
repeat
{
# statements
print(val)
  val = val + 1
  
# checking stop condition
if(val > 5)
{
# using break statement
# to terminate the loop
break
}
###EXEMPLE 2:
# R program to illustrate
# the application of repeat loop
  
# initializing the iteration variable with 0
i < - 0
  
# using repeat loop
 repeat
{
# statement to be executed multiple times
print("Geeks 4 geeks!")
    
# incrementing the iteration variable
    i = i + 1
    
    # checking the stop condition
    if (i == 5)
  {
# using break statement
# to terminate the loop
break
}
  }
#### Break Statement:
  # R program to illustrate
  # the use of break statement
  
  # using for loop
  # to iterate over a sequence
  for (val in 1: 5)
  {
    # checking condition
    if (val == 3)
    {
      # using break keyword
      break
    }
    
    # displaying items in the sequence
    print(val)
##### Next Statement:
    # R program to illustrate
    # the use of next statement
    
    # using for loop
    # to iterate over the sequence
    for (val in 1: 5)
    {
      # checking condition
      if (val == 3)
      {
        # using next keyword
        next
      }
      
      # displaying items in the sequence
      print(val)
    }
    
    