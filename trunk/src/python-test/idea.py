#!/usr/bin/env python
"IDEA is ..."

import argparse

def main():
    "All of the magic happens here"
    args = parse_options()
    print args
    # Parse arff file
    # Separate into train, test using stratified
    # Generate a collection of instances as datums, coordinates, classes for each
    # From the train instances, generate quadrants
    # From the quadrants, generate clusters
    # Use the clusters to classify the test instances
    # Output performance statistics however we want
    # Optionally generate a figure Figure(filename, instances, clusters), Figure.draw()
    # Win

def parse_options():
    "Place new options that you would like to be parsed here."
    parser = argparse.ArgumentParser(description='Perform IDEA on given train and test sets.')
    parser.add_argument('--train',
                        dest='train',
                        metavar='FILE',
                        type=str,
                        nargs='+',
                        help='Specify arff file[s] from which to construct the training set.')
    parser.add_argument('--test',
                        dest='test',
                        metavar='FILE',
                        type=str, nargs='+',
                        help='Specify arff files[s] from which to construct a test set.\n\
                              Not specifying this results in a self-test that\'s only useful\n\
                              for quick tests of the software.')
    
    parser.add_argument('--stratified',
                        dest='stratified',
                        default=None,
                        metavar='RATIO',
                        type=int, nargs=2,
                        help='Specify a ratio for a stratified cross-validation scheme.\n\
                              This takes two arguments for a x train to x test ratio.\n\
                              Do not include a test set with this flag.)')
    parser.add_argument('--output',
                        dest='output',
                        type=str,
                        metavar='CONCAT',
                        default='',
                        help='Specify an output dir.')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()
