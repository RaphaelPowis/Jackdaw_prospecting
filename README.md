To access a specific object:

library(targets)
variable_name <- tar_read(target_name)

Here is a list of useful targets:

visits: dataframe with all the recorded visits
phase_comp_data: data for model 1
day_day_data: data for model 2
indiv_data: data for model 3
bird_failure_data: data for model 4
box_failure_data: data for model 5
parental_box_data: data for model 6
fitness_data: data for model 7
parental_data: data for model 8

phase_comp_model: results of model 1
...

Here is an example:
library(target)
phase_comp_model <- tar_read(phase_comp_model)
summary(phase_comp_model) 

To run the whole pipeline:

library(targets)
tar_make()

The results are printed in result.html

Note that the target packages stores the results in the _target file to avoids running the same code twice. Since I included the _target file to allow for convinient access to all targets, you will need to delete the "_target" file (not  _target.R, the _target file) if you want to rerun the code. Expect about 30 min of running.
