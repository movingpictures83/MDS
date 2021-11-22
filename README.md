# MDS
# Language: R
# Input: TXT (key, value pairs)
# Output: PREFIX
# Tested with: PluMA 1.1, R 4.0.0
# Dependency: Rtsne_0.15

PluMA plugin to run Multi-Dimensional Scaling (MDS) (Cox et al, 2008) to determine, given
two sets of data and the values of multiple observables in each set, how well the
two sets can be separated based on these values.

The plugin accepts as input a TXT file with keyword, value pairs:
csvfile: Dataset
features.txt: Names of the features for which to account in differentiation
catfiles.txt: Names of sample categories
variables: Number of variables to count
k: Number of dimensions

Data will be output to a CSV file, with each sample and its x-y coordinates.
A plot will also be produced in a PDF file using the PREFIX
