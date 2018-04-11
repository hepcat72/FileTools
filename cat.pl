#!/usr/bin/perl

#USAGE: Run with no options to get usage or with --help for basic details

#The reason I created this tool was I had 5 sets of files, all with the same
#name in different directories, and I wanted to concatenate each one that had
#the same name in a simple single command, e.g.:
#~/pub/filetools/cat.pl -i "225_CDS_counts/*.txt" -i "275_tRNA_counts/*.txt" -i "325_ncRNA_counts/*.txt" -i "375_rRNA_counts/*.txt" -i "425_gene_counts/*.txt" --outdir combined_counts -o .all.txt --verbose

use warnings;
use strict;
use CommandLineInterface;

setScriptInfo(VERSION => '1.0',
              CREATED => '1/22/2018',
              AUTHOR  => 'Robert William Leach',
              CONTACT => 'rleach@princeton.edu',
              COMPANY => 'Princeton University',
              LICENSE => 'Copyright 2018',
              HELP    => join('',('This script concatenates files in various ',
				  'combinations.  Use the --outfile option ',
				  'with multiple output files or use the ',
				  '--outdir -o option combination to ',
				  'concatenate files of the same name to a ',
				  'connom output file with a different ',
				  'extension.  See the ADVANCED FILE I/O ',
				  'FEATURES section of the `--help ',
				  '--extended` output for details and ',
				  'examples.')));

my $intype = addInfileOption(GETOPTKEY => 'i=s',
			     PRIMARY   => 1,
			     REQUIRED  => 1);

addOutfileTagteamOption(GETOPTKEY_SUFF     => 'o=s',
			GETOPTKEY_FILE     => 'outfile=s',
			FILETYPEID         => $intype,
			PAIR_RELAT         => 'ONETOONEORMANY',
			REQUIRED           => 0,
			PRIMARY            => 1,
			FORMAT_DESC        => 'ASCII text file.',
			SMRY_DESC_SUFF     => 'Output file extension',
			SMRY_DESC_FILE     => 'Output file',
			COLLISIONMODE_SUFF => 'merge',
#			COLLISIONMODE_FILE => 'merge',
			DETAIL_DESC_SUFF   => << 'END_DETAIL'

Output file extension.  To concatenate files with the same name (from different input directories) using this option, you must supply --outdir.
END_DETAIL
		       );

setDefaults(HEADER        => 0,
	    ERRLIMIT      => 3,
	    DEFRUNMODE    => 'usage');

while(nextFileCombo())
  {
    my $inputFile  = getInfile($intype);
    my $outputFile = getOutfile();

    openIn (*IN, $inputFile)  || next;
    openOut(*OUT,$outputFile,1,0,undef,1,1) || next;

    #If we're doing a lightweight dry run, skip reading the input file
    if(isDryRun())
      {
	closeIn(*IN);
	closeOut(*OUT);
	next;
      }

    my $last_char_is_newline = 1;
    while(getLine(*IN))
      {
	#This is all the script does: prints files
	#If the user upplied an outfile, output goes there by default
	print;
	$last_char_is_newline = (/\n$/ ? 1 : 0);
      }

    unless($last_char_is_newline)
      {print("\n")}

    closeIn(*IN);
    closeOut(*OUT);
  }
