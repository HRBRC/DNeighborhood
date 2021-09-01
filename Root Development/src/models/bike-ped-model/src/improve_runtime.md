## September 1st Update:

Though I considered using the apply function, I decided to try another approach before that. I tried to tackle the nested for loop in the third call with "expand.grid". I created a tibble with all possible combinations of the census_tract row and the mileage_df, I did a left join for the tibble and the working_df (joining on the common values). And, the rest of the function was pretty much the same. I didn’t make changes to the other three calls- the first two calls didn’t need any changes, and the final call had just one for loop. It was the nested for loop in the third call that took up a lot of time. So replacing the nested for loop has brought down the run time considerably.

## Test Case: 

health_outcome is “diabetes”, number of census_tracts is 3, and total_mileage is 2

Before making changes the run time was 12 minutes. Now, it is 6.5 minutes. The run time is 2.55 minutes when the health outcome is high bp. And it is 2.46 minutes when the health outcome is poor physical health.
The goal is to make it much lower. So, now, I will work on the for loop in the fourth call. I intend to use lapply() there. Once, I think the run time is decent, I intend to translate the code to Python.

Note: The first three calls for both the functions return the same output, there seems to be a discrepancy for one of the columns on running the fourth call. I am still exploring as to why that is happening. I will work on replacing the for loop in the fourth call, and then see how it works!
