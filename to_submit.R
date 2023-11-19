#Week 4: dplyr package

#Install and call the package dplyr
#TASK: Write the functions to install and call dplyr
install.packages("dplyr")
library(dplyr)

#Task: Write the function to get a dataset from Base R: Titanic
#and give the dataframe a new name of your choice
#(hint: you will want your data to be a dataframe. Use the function: as.data.frame(Titanic))
readdata <- function(){
  raw <- as.data.frame(Titanic)
  return(raw)
}
raw = readdata()

#See the top rows of the data
#TASK: Write the function to see the top rows of the data
top_rows <- function(orig){
  # equivalent
  # raw %>% slice(1:6)
  return (head(orig))
}
tops = top_rows(raw)

#Let's name the dataset with just the two columns, Survived and Sex
#TASK: Write the function to save the two columns as one new dataset
#and give it a name
select_sex_survived <- function(orig){
  return (raw %>% select('Sex','Survived'))
}
sex_survived = select_sex_survived(raw)

#Let's just see the Survived and Sex columns
#Task: Write the function to 'select' the Survived and Sex columns 
#(hint: use the 'select' function)
### From the description, I assume the difference from the other function is that 
### instead of creating a new dataset, this one will operate in-place (overwritting the original)
select_sex_survived_in_place <-function(orig){
  modified <- select_sex_survived(orig)
  assign(deparse(substitute(orig)), modified, env=.GlobalEnv)
}
select_sex_survived_in_place(raw)
### restore the data since it's modified by select_sex_survived_in_place()
raw = readdata()


#Let's get rid of the Sex column in the new dataset created above
#TASK: Write the function that deselects the sex column
#(hint: use the 'select' function to not select a -column)
remove_sex <-function(orig){
  survived = orig %>% select(-c("Sex"))
  return(survived)
}
survived = remove_sex(sex_survived)


#Let's rename a column name
#TASK: Write the function that renames 'Sex' to 'Gender'
### again, I assume this second one is in-place renaming:
sex_to_gender_in_place <-function(orig){
  modified <- orig %>% rename("Gender" = "Sex")
  assign(deparse(substitute(orig)), modified, env=.GlobalEnv)
}
# to rename the original:
sex_to_gender_in_place(raw)
#restore the data since it's changed
raw = readdata()

#Let's make a new dataframe with the new column name
#TASK: Write the function that names a new dataset that includes the 'gender' column
### this one does it in normal way: save to a new dataframe
sex_to_gender <-function(orig){
  return (orig %>% rename("Gender" = "Sex"))
}
renamed = sex_to_gender(raw)

#Let's 'filter' just the males from our dataset
#TASK: Write the function that includes only rows that are 'male'
select_male_only <-function(orig){
  return (renamed %>% filter(Gender=='Male'))
}
male_only = select_male_only(renamed)

#Let's 'arrange' our data by gender (not the data you just filtered)
#TASK: Write the function to group the data by gender (hint: arrange())
group_by_gender <-function(orig)
{
  return (arrange( orig, Gender))
}
### again, this one does it in-place
group_by_gender_in_place <-function(orig)
{
  modified <- group_by_gender(orig)
  assign(deparse(substitute(orig)), modified, env=.GlobalEnv)
}
group_by_gender_in_place(renamed)
### now we're working on the rearranged dataset

#Let's see how many people were examined in the dataset (total the frequency in the original dataframe)
#TASK: Sum the Freq column
total_freq = renamed %>% select("Freq")  %>% summarise_all(sum) 
#TASK: After you run it, write the total here:2201

#Since we have a males dataset, let's make a females dataset
#TASK: Write the function that includes only rows that are 'female'
select_female_only <-function(orig){
  return (renamed %>% filter(Gender=='Female'))
}
female_only = select_female_only(renamed)

#And now let's join the males and females
#TASK: Write the function that joins the male and female rows 
#(hint: try using 'union' or 'bind_rows')
join_tabs <- function(o1,o2){
  return (union(o1,o2))
}
male_female = join_tabs(male_only,female_only)

#Optional Task: add any of the other functions 
#you learned about from the dplyr package

mx = renamed %>% select("Freq")  %>% summarise_all(max)
fs = renamed %>% transmute(Freq = Freq/as.numeric(mx))


