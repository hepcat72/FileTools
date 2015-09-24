# filetools
Generic tools for handling plain text and tab-delimited files.

I have been using and refining these personal file tools for years.  Most of the scripts have a usage message when run without parameters.  Most also have a --help message.  That should be sufficient to figure out how to use them.

Here's a few quick notes about some of the tools:

**grep.pl** - This tool is of particular note.  This is my personal implementation of grep.  It's slow, but it's designed for multi-line records and had pre-built record recognition for fasta files and sections of blast results.

**carriage2newline.pl** - Fixes files that have weird combinations of carriage returns and newlines.

**columnStitch.pl** - Stitches selected columns of tab-delimited text files together into one file in the order you specify.

**html_cleaner.pl** - Really quick and dirty script - and incomplete.  Use at own risk.

**numrowscols.pl** - Simple script to report the number of rows containing an indicated number of columns.  If every row has the same number of columns, you will only get 1 line of output.  This script is somewhat unrefined, but it can be handy to simply confirm that the number of columns in a file is consistent.

**realSmartSort.pl** - Sorts the lines of a file, treating numbers numerically and letters alphabetically, even if the numbers have a different number of digits.  In other words, the strings abc1def and abc1000abc will be ordered as abc1def, abc1000abc.

**rename.pl** - Easy file renaming script.  Everyone has one.

**replace.pl** - Search & replace script.

**reverseFile.pl** - Just reverses the order of lines of a file.

**transpose.pl** - Transposes the columns & rows or a tab-delimited file.


If you find any of these scripts useful, give me a shout through github.  I'd love to hear from you.
