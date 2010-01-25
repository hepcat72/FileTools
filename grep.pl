#!/usr/bin/perl -w

#Grep (for perl patterns)
#Generated using perl_script_template.pl 1.8
#Robert W. Leach
#10/8/2004
#Los Alamos National Laboratory
#Copyright 2004

markTime();
my $software_version_number = '1.9';

##
## Start Main
##

use strict;
use Getopt::Long;

#Declare variables
my($verbose,
   $quiet,
   $split,
   $help,
   @input_files,
   $version,
   $DEBUG,
   $outfile_suffix,
   $current_output_file,
   $pattern,
   $mark_matched,
   $match_marker,
   $no_match_marker,
   $count_pattern,
   $fasta_grep,
   $blast_query_grep,
   $blast_subject_grep,
   $blast_aln_grep,
   $everything_else,
   $combine_with_and,
   $enforce_order,
   $show_files,
   $case_insensitive);
my $pattern_files = [];
my $file_patterns = [];
my $before = 0;
my $after  = 0;

#If there are no arguments and no files directed or piped in
if(scalar(@ARGV) == 0 && (-t STDIN || eof(STDIN)))
  {
    usage();
    exit(0);
  }

#Get the input options
GetOptions('p|perl-pattern=s'      => \$pattern,              #REQUIRED
	   'b|lines-before=s'      => \$before,               #OPTIONAL [0]
	   'a|lines-after=s'       => \$after,                #OPTIONAL [0]
	   'i|case-insensitive!'   => \$case_insensitive,     #OPTIONAL [Off]
	   'm|mark-matched-lines!' => \$mark_matched,         #OPTIONAL [Off]
	   'k|match-marker=s'      => \$match_marker,         #OPTIONAL [*]
	   'u|no-match-marker=s'   => \$no_match_marker,      #OPTIONAL [undef]
	   's|show-file-names!'    => \$show_files,           #OPTIONAL [Off]
	   'c|count-pattern|r|record-pattern=s' =>            #OPTIONAL [undef]
	                              \$count_pattern,
	   'f|pattern-files=s'     => sub {push(@$pattern_files, #OPTIONAL
						sglob($_[1]))},  #[undef]
	   'n|combine-with-AND!'   => \$combine_with_and,     #OPTIONAL [Off]
	   'e|enforce-order!'      => \$enforce_order,        #OPTIONAL [Off]
	   'v|everything-else!'    => \$everything_else,      #OPTIONAL [Off]
	   't|fasta-grep!'         => \$fasta_grep,           #OPTIONAL [Off]
	   'bq|blast-query-grep!'  => \$blast_query_grep,     #OPTIONAL [Off]
	   'bs|blast-subject-grep!' => \$blast_subject_grep,  #OPTIONAL [Off]
	   'ba|blast-alnmt-grep!'  => \$blast_aln_grep,       #OPTIONAL [Off]
	   'D|debug!'              => \$DEBUG,                #OPTIONAL [Off]
	   'version!'              => \$version,              #OPTIONAL [Off]
	   'verbose!'              => \$verbose,              #OPTIONAL [Off]
	   'q|quiet!'              => \$quiet,                #OPTIONAL [Off]
	   'h|help!'               => \$help,                 #OPTIONAL [Off]
	   'o|outfile-suffix=s'    => \$outfile_suffix,       #OPTIONAL [undef]
	   'split!'                => \$split,                #OPTIONAL [Off]
	   '<>'                    => sub {push(@input_files, #REQUIRED STDIN
						sglob($_[0]))}, #acceptable
          );

#Print the argument settings if in debug mode
if($DEBUG)
  {debug("Debug mode on.")}

#If the user has asked for help, call the help subroutine
if($help)
  {
    help();
    exit(0);
  }

#If the user has asked for the software version, print it
if($version)
  {
    print($software_version_number,"\n");
    exit(0);
  }

#Check validity of verbosity options
if($verbose && $quiet)
  {
    error("You cannot supply verbose and quiet flags at the same time.");
    exit(2);
  }

#Put standard input into the input_files array if there is standard input
if(!(-t STDIN || eof(STDIN)))
  {
    #Clear out the outfile suffix if there is input on STDIN
    undef($outfile_suffix) if(scalar(@input_files) == 0);
    push(@input_files,'-');
  }

#Make sure there is input
if(scalar(@input_files) == 0)
  {
    error("No input files detected.") unless($quiet);
    usage();
    exit(1);
  }

if(!defined($pattern) &&
   (scalar(@$pattern_files) == 0 ||
    scalar(grep {!(-r $_)} @$pattern_files) ||
    scalar(grep {-B $_} @$pattern_files)))
  {
    error(((scalar(@$pattern_files) &&
	    scalar(grep {!(-r $_)} @$pattern_files)) ?
	   "Pattern files: [" . join(',',grep {!(-r $_)} @$pattern_files) .
	   "] is not readable." :
	   ((scalar(@$pattern_files) &&
	    scalar(grep {-B $_} @$pattern_files)) ?
	    "Pattern files: [" . join(',',grep {-B $_} @$pattern_files) .
	    "] is a binary file." :
	    "No pattern submitted."))) unless($quiet);
    usage();
    exit(1);
  }
$pattern = ($case_insensitive ? '(?i)' : '') . $pattern
  unless(scalar(@$pattern_files));

if((defined($match_marker) && $match_marker ne '') ||
   (defined($no_match_marker) && $no_match_marker ne ''))
  {$mark_matched = 1}

if($mark_matched &&
   (!defined($match_marker) || $match_marker eq '') &&
   (!defined($match_marker) &&
    (!defined($no_match_marker) || $no_match_marker eq '')))
  {$match_marker = '*'}

if($fasta_grep && defined($count_pattern) && $count_pattern ne '')
  {
    error("You can't use the -t and -c options at the same time.  These are ",
	  "conflicting flags.") unless($quiet);
    usage();
    exit(1);
  }

if($fasta_grep)
  {$count_pattern = '^\s*>'}

my $c_ok = 0;

if($blast_query_grep && $fasta_grep)
  {
    error("You can't use the --bq and -t options at the same time.  These ",
	  "are conflicting flags.") unless($quiet);
    usage();
    exit(1);
  }

if($blast_query_grep && defined($count_pattern) && $count_pattern ne '')
  {
    error("You can't use the --bq and -c options at the same time.  These ",
	  "are conflicting flags.") unless($quiet);
    usage();
    exit(1);
  }

if($blast_query_grep)
  {
    $count_pattern = '^(?:\S*BLAST|Query=|\A\s+Database:)';
    $c_ok = 1;
  }

if($blast_subject_grep && $fasta_grep)
  {
    error("You can't use the --bs and -t options at the same time.  These ",
	  "are conflicting flags.") unless($quiet);
    usage();
    exit(1);
  }

if($blast_subject_grep && !$c_ok && defined($count_pattern) &&
   $count_pattern ne '')
  {
    error("You can't use the --bs and -c options at the same time.  These ",
	  "are conflicting flags.") unless($quiet);
    usage();
    exit(1);
  }

if($blast_subject_grep)
  {
    $count_pattern = '^(?:\S*BLAST|Query=|\A\s+Database:|>)';
    $c_ok = 1;
  }

if($blast_aln_grep && $fasta_grep)
  {
    error("You can't use the --ba and -t options at the same time.  These ",
	  "are conflicting flags.") unless($quiet);
    usage();
    exit(1);
  }

if($blast_aln_grep && !$c_ok && defined($count_pattern) &&
   $count_pattern ne '')
  {
    error("You can't use the --ba and -c options at the same time.  These ",
	  "are conflicting flags.") unless($quiet);
    usage();
    exit(1);
  }

if($blast_aln_grep)
  {$count_pattern = '(?:\A\S*BLAST|\AQuery=|\A\s+Database:|\A>|Score =)'}

if(scalar(@$pattern_files) == 0 &&
   ($enforce_order || $combine_with_and) && !$quiet)
  {warning("The -n and -e flags only work when a pattern file (-f) is used.")}

my $file_by_file = (scalar(@$pattern_files) == scalar(@input_files));

if($verbose && scalar(@$pattern_files))
  {print STDERR ("PATTERN FILE MODE: ",
		 ($file_by_file ? 'FILE BY FILE' :
		  'ALL PATTERN FILES COMBINED COMPARED TO EACH INPUT FILE'),
		 "\n")}

#Create a pattern from the pattern file
if(scalar(@$pattern_files))
  {
    if(defined($pattern) && !$quiet)
      {warning("Ignoring supplied pattern: [$pattern] because you ",
	       "supplied pattern files: [",join(',',@$pattern_files),"].\n")}

    #Ignore any pattern provided
    $pattern = ($case_insensitive ? '(?i)' : '');

    #For each input file of the same type
    foreach my $pattern_file (@$pattern_files)
      {
	if($verbose)
	  {print STDERR ("Opening input file: [$pattern_file].\n")}

	#Open the input file
	unless(open(INPUT,$pattern_file))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open input file: [$pattern_file]\n$!")
	      unless($quiet);
	    next;
	  }

	#Keep track of input file's line number
	my $line_num = 1;

	$pattern = ($case_insensitive ? '(?i)' : '')
	  if(scalar(@$pattern_files) == scalar(@input_files));
	$pattern .= '(?=.*' if(!$enforce_order && $combine_with_and);
	$pattern .= join(($enforce_order ? '.*?' :
			  ($combine_with_and ? ')(?=.*?' : '|')),
			 grep {$_ !~ /^(?:\s*\#.*)$/ && /./}
			 map {chomp;$_}
			 getLine(\*INPUT,$quiet));
	$pattern .= ')' if(!$enforce_order && $combine_with_and);

	if($verbose)
	  {print STDERR ("\n[$pattern_file] Done.  Closing File.  Time ",
			 "taken: [",
			 scalar(markTime()),
			 " Seconds]\n")}

	close(INPUT);

	if($file_by_file)
	  {push(@$file_patterns,$pattern)}
      }
  }

#For each input file of the same type
foreach my $input_file (@input_files)
  {
    if($file_by_file)
      {$pattern = shift(@$file_patterns)}

    #Open and select the next output file if an output file name suffix has
    #been defined
    if(defined($outfile_suffix) && !$split)
      {
	#Set the current output file name
	$current_output_file = $input_file . $outfile_suffix;

	if($verbose)
	  {print STDERR ("Opening output file: [$current_output_file].\n")}

	#Open the output file
	unless(open(OUTPUT,">$current_output_file"))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open output file: [$current_output_file].\n$!")
	      unless($quiet);
	    next;
	  }

	#Select the output file handle
	select(OUTPUT);
      }

    if($verbose)
      {print STDERR ("Opening input file: [",
		     ($input_file eq '-' ? 'STDIN' : $input_file),
		     "].\n")}

    if($verbose && $file_by_file)
      {print STDERR ("Using pattern file: [",shift(@$pattern_files),"].\n")}

    if($pattern eq '' && !$quiet)
      {warning("Your pattern: [$pattern] is empty.  Every line will match.")}

    if($verbose)
      {print STDERR ("PATTERN: ",
		     ($everything_else ? 'EVERYTHING EXCEPT (-v activated): ' :
		      ''),
		     "[$pattern]\n")}

    #Open the input file
    unless(open(INPUT,$input_file))
      {
	#Report an error and iterate if there was an error
	error("Unable to open input file: [$input_file]\n$!") unless($quiet);
	next;
      }

    #Keep track of input file's line number
    my $line_num = 1;

    #This is a decrementing counter that determines when to stop printing after
    #a matched line.  It is set every time there's a match to $after.
    my $still = 0;

    #This is the buffer needed for the lines printed before the matched line
    #When a match is encountered, the buffer is printed and emptied
    my @lines = ();

    my $blast_header         = '';
    my $blast_query_header   = '';
    my $blast_subject_header = '';
    my $blast_footer         = '';
    my $num_matches          = 0;
    my $num_nonmatches       = 0;

    #For each line in the current input file
    while(defined($count_pattern) ?
	  getCountedLine($count_pattern,\*INPUT) : getLine(\*INPUT))
      {
	#Record & skip 'header' records in blast mode unless flags provided
	if($blast_query_grep || $blast_subject_grep || $blast_aln_grep)
	  {
	    debug("Recording blast headers");

	    if(/^\S*BLAST/)
	      {debug("Got top blast header");$blast_header = $_}
	    if($blast_subject_grep && /^>/ && $blast_aln_grep)
	      {debug("Got subject blast header");$blast_subject_header = $_}
	    if($blast_query_grep && /^Query=/ &&
	       ($blast_subject_grep || $blast_aln_grep))
	      {debug("Got query blast header");$blast_query_header = $_}
	    if(/^\s+Database:/)
	      {debug("Got blast footer");$blast_footer = $_}

	    next if(/BLAST/);
	    next if(/^Query=/ && ($blast_aln_grep || $blast_subject_grep));
	    next if(/^>/ && $blast_aln_grep);

	    if($blast_footer)
	      {
		print($blast_footer);
		$blast_footer = '';
		next;
	      }

#	    if($blast_query_grep && $blast_subject_grep && $blast_aln_grep)
#	      {$_ = $blast_query_header . $blast_subject_header . $_}
#	    elsif($blast_subject_grep && $blast_aln_grep)
#	      {$_ = $blast_subject_header . $_}
#	    elsif($blast_query_grep &&
#		  ($blast_aln_grep || $blast_subject_grep))
#	      {$_ = $blast_query_header . $_}
	  }

	if($verbose && $outfile_suffix)
	  {print STDERR ("Reading ",
			 (defined($count_pattern) ? 'record' : 'line'),
			 " $line_num\r")}

	if(scalar(@lines) == 0 || $still == 0)
	  {
	    #Push onto the buffer
	    push(@lines,$_);
	  }

	debug("Pattern matching");

	#If we've matched the pattern
	if(($everything_else ? ($_ !~ /$pattern/s) : (/$pattern/s)))
	  {
	    $num_matches++;
	    debug("Matched");

	    #Open and select the next output file if split is true
	    #This overrides the previous output file selected by outfile suffix
	    #alone
	    if($split)
	      {
		#Close the last output file if one has been opened
		if($num_matches > 1)
		  {
		    #Select standard out
		    select(STDOUT);
		    #Close the output file handle
		    close(OUTPUT);
		  }

		#Set the current output file name
		$current_output_file = $input_file . ".$num_matches" .
		  $outfile_suffix;

		if($verbose)
		  {print STDERR ("Opening output file: [$current_output_file]",
				 ".\n")}

		#Open the output file
		unless(open(OUTPUT,">$current_output_file"))
		  {
		    #Report an error and iterate if there was an error
		    error("Unable to open output file: [$current_output_file]",
			  ".\n$!")
		      unless($quiet);
		    next;
		  }

		#Select the output file handle
		select(OUTPUT);
	      }


	    #Take care of blast parsing: print header stuff
	    if($blast_query_grep || $blast_subject_grep || $blast_aln_grep)
	      {
		debug("Checking blast headers");

		if($blast_header &&
		   $blast_query_header &&
		   $blast_subject_header)
		  {
		    debug("Applying all blast headers");
		    $lines[0] = $blast_header . $blast_query_header .
		      $blast_subject_header . $lines[0];
		  }
		elsif($blast_query_header &&
		      $blast_subject_header)
		  {
		    debug("Applying query and subject blast headers");
		    $lines[0] = $blast_query_header . $blast_subject_header .
		      $lines[0];
		  }
		elsif($blast_subject_header)
		  {
		    debug("Applying subject blast headers");
		    $lines[0] = $blast_subject_header . $lines[0];
		  }
		$blast_header         = '';
		$blast_query_header   = '';
		$blast_subject_header = '';
	      }

	    if(scalar(@lines) > 1 && $still == 0)
	      {
		my(@new_unmatched_lines);
		foreach my $unmatched_line (@lines[0..(scalar(@lines)-2)])
		  {
		    push(@new_unmatched_lines,
			 split(/(?<=\n)/,$unmatched_line));
		    pop(@new_unmatched_lines)
		      if($new_unmatched_lines[-1] !~ /\S/);
		  }

		print(($mark_matched ?
		       (!defined($no_match_marker) ?
			' ' x length($match_marker) :
			$no_match_marker) . ' ' :
		       '') .
		      ($show_files ?
		       ($input_file eq '-' ?
			'STDIN' :
			$input_file) . ':' :
		       ''),
		      join('' .
			   ($mark_matched ?
			    (!defined($no_match_marker) ?
			     ' ' x length($match_marker) :
			     $no_match_marker) . ' ' : '') .
			   ($show_files ?
			    ($input_file eq '-' ?
			     'STDIN' :
			     $input_file) . ':' :
			    ''),
			   @new_unmatched_lines));
	      }

	    my(@new_matched_lines);
	    push(@new_matched_lines,split(/(?<=\n)/,$lines[-1]));
#I think this was removing blank lines at the end of records when using -r
#I don't know why it was originally coded this way, so I'm just commenting it
#out in case I find that it starts inserting lines...
#	    pop(@new_matched_lines)
#	      if(scalar(@new_matched_lines) &&
#		 (!defined($new_matched_lines[-1]) ||
#		  $new_matched_lines[-1] !~ /\S/));
	    pop(@new_matched_lines)
	      if(scalar(@new_matched_lines) &&
		 !defined($new_matched_lines[-1]));

	    foreach my $new_matched_line (@new_matched_lines)
	      {
		print(($mark_matched ?
		       (($everything_else ?
			 ($new_matched_line !~ /$pattern/) :
			 ($new_matched_line =~ /$pattern/)) ?
			(!defined($match_marker) || $match_marker eq '' ?
			 ' ' x length($no_match_marker) :
			 $match_marker) :
		        (!defined($no_match_marker) || $no_match_marker eq '' ?
			 ' ' x length($match_marker) :
			 $no_match_marker)) . ' ' :
		       ''),
		      ($show_files ?
		       ($input_file eq '-' ?
			'STDIN' :
			$input_file) . ':' :
		       ''),
		      $new_matched_line);
	      }

	    @lines = ();
	    $still = $after;
	  }
	#Else if we're not counting specific lines and we're still printing
	#lines after a match OR we are counting specific lines and we're still
	#printing lines after a match (including those that aren't counted
	#after a counted line)
	elsif($still > 0)
	  {
	    $num_nonmatches++;
	    debug("Still matching");

	    #Decrement still
	    $still--;

	    #Only print the line if still is greater than 0
	    if($still >= 0)
	      {
		print(($mark_matched ?
		       (!defined($no_match_marker) ?
			' ' x length($match_marker) :
			$no_match_marker) . ' ' : '') .
		      ($show_files ?
		       ($input_file eq '-' ?
			'STDIN' :
			$input_file) . ':' :
		       ''),
		      $_);
		@lines = ();
	      }
	  }
	else
	  {$num_nonmatches++;debug("Didn't match")}

	#Trim the buffer if it's gotten too large
	if(scalar(@lines) == $before + 1)
	  {shift(@lines)}

	$line_num++;
      }

    if($verbose)
      {print STDERR ("\n$input_file Results: $num_matches MATCHES ",
		     "$num_nonmatches NON-MATCHES\n",
		     "\n[",
		     ($input_file eq '-' ? 'STDIN' : $input_file),
		     '] Done.  Closing File.  Time taken: [',
		     scalar(markTime()),
		     " Seconds]\n")}

    close(INPUT);

    #If an output file name suffix is set
    if((defined($outfile_suffix) && !$split) || ($split && $num_matches > 0))
      {
	#Select standard out
	select(STDOUT);
	#Close the output file handle
	close(OUTPUT);
      }
  }

#Report the stop time if verbose mode is on
if($verbose)
  {print STDERR ("Running Time: ",scalar(markTime(0))," Seconds\n")}

##
## End Main
##






























##
## Subroutines
##

sub help
  {
    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Print a description of this program
    print << "end_print";

$script
Copyright 2004
Robert W. Leach
2/4/2005
Bioscience Division
Bioinformatics and Computational Biology Group, B5
Biodefense Informatics Team
MS M888
Los Alamos National Laboratory
Los Alamos, NM 87545
robleach\@lanl.gov

* WHAT IS THIS: This is a perl version of grep.  It doesn\'t claim to be as
                fast as the unix version of grep, but it has a few nice
                features not offered in the unix version.  It was made in order
                to be able to see lines before and after the line that was
                matched.

* INPUT FORMAT: It\'s pretty much the same as the unix version of grep, except
                the pattern needs a flag (-p).  Also, since it\'s a perl
                pattern, characters have different meanings.  be careful to
                escape your \'\$\' and \'\@\' signs!

* OUTPUT FORMAT: Again it\'s pretty much the same as the unix version of grep,
                 except there\'s an option to flag the line with the matching
                 pattern with any desired string.  You must also specify if you
                 want matched lines prepended with the file name they were
                 found in.

end_print

    return(0);
  }



sub usage
  {
    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    print << "end_print";

USAGE1: $script input_files -p perl_regular_expression [-b number] [-a number] [-m] [-k match_marker_string] [-u no_match_marker_string] [-s] [-f "*.pattern_files"] [-n] [-e] [-c perl_regular_expression] [-v] [-t] [-o .suffix] [--split] [--verbose] [-q] [-h] [--version] [-D]
USAGE2: $script -p perl_regular_expression [-b number] [-a number] [-m] [-k match_marker_string] [-u no_match_marker_string] [-s] [-f "*.pattern_files"] [-n] [-e] [-c perl_regular_expression] [-v] [-t] [--split] [--verbose] [-h] [--version] [-q] [-D] < input_file

                          REQUIRED Input file(s).  Standard input via
                                   redirection is acceptable.  There is no flag
                                   required for input files.
     -p|--perl-pattern    REQUIRED Perl regular expression to look for in the
                                   input file(s).
     -b|--lines-before    OPTIONAL [0] Number of lines to display above a
                                   matched line.
     -a|--lines-after     OPTIONAL [0] Number of lines to display below a
                                   matched line.
     -m|--mark-matched-   OPTIONAL [Off] Mark matched lines (to make them
        lines                      easier to identify).
     -k|--match-marker    OPTIONAL [*] This is the marker used by the -m flag
                                   (described above).  Supplying a value here
                                   turns on the -m flag automatically.
     -u|--no-match-marker OPTIONAL [] When the -m flag is on, this is the
                                   string used to mark unmatched lines.
                                   Supplying a value here turns on the -m flag
                                   automatically.
     -s|--show-file-names OPTIONAL [Off] Show file names where the pattern was
                                   matched.
     -c|--count-pattern|  OPTIONAL [] This is basically a record delimiter
     -r|--record-pattern           pattern.  This allows you to output an
                                   entire record if the record matches the
                                   pattern supplied in -p (multiple line
                                   patterns allowed).  Records are split at the
                                   beginning of the line that matches this
                                   pattern.
     -f|--pattern-files   REQUIRED This parameter is OPTIONAL if -p is
                                   provided.  If both -p and -f are provided,
                                   the -p flag is ignored.  Each line of the
                                   pattern file is concatenated into one
                                   pattern.  Each line is joined in the final
                                   pattern with an "OR" matching behavior by
                                   default.
     -n|--combine-with-   OPTIONAL [Off] This parameter only works when -f is
        AND                        used and is otherwise ignored.  This
                                   parameter changes the joining behavior of
                                   the patterns from a file.  A line in the
                                   "grepped" files matches only if every
                                   pattern from the pattern is found on the
                                   line (i.e. not if only one of the patterns
                                   matches as is the default behavior).
     -e|--enforce-order   OPTIONAL [Off] This parameter only works when the -f
                                   flag is used and is otherwise ignored.  This
                                   parameter changes the joining behavior of
                                   the patterns from a file.  A line in the
                                   "grepped" files matches only if every
                                   pattern from the pattern is found on the
                                   line in the order they occur in the pattern
                                   file.  This flag has the same effect as the
                                   -n flag, but with the added order
                                   restriction.
     -v|--everything-else OPTIONAL [Off] This parameter reverses the matching
                                   behavior of the perl pattern.  Lines that
                                   match and lines that don\'t match are
                                   reversed.  In other words, the behavior is
                                   the same as the -v option of the unix
                                   version of grep.
     -t|--fasta-grep      OPTIONAL [Off] This flag causes fasta records to be
                                   counted as one line (but it doesn\'t allow
                                   you to pattern match past hard returns -
                                   it\'s mainly for matching deflines).  This
                                   flag cannot be used with the -c, -bq, -bs,
                                   or -ba options.
     --bq|--blast-query-  OPTIONAL [Off] This flag causes blast records (per
          grep                     query sequence) to be counted as one line
                                   (but it doesn\'t allow you to pattern match
                                   past hard returns).  This flag cannot be
                                   used with the -c or -t options.  If supplied
                                   with the --bs or --ba options, some records
                                   will contain only the query header
                                   information.  The query header information
                                   is not pattern matched (by the -p pattern)
                                   if either or both of the other blast options
                                   is supplied.
     --bs|--blast-        OPTIONAL [Off] This flag causes blast records (per
          subject-grep             subject sequence) to be counted as one line
                                   (but it doesn't allow you to pattern match
                                   past hard returns).  This flag cannot be
                                   used with the -c or -t options.  If supplied
                                   with the --ba option, some records will
                                   contain only the subject header information.
                                   The query and subject header information is
                                   not pattern matched (by the -p pattern) if
                                   the --ba option is supplied.
     --ba|--blast-alnmt-  OPTIONAL [Off] This flag causes blast records (per
          grep                     alignment) to be counted as one line (but it
                                   doesn\'t allow you to pattern match past
                                   hard returns).  This flag cannot be used
                                   with the -c or -t options.  If this flag is
                                   used without the --bq or --bs options, no
                                   records will contain query or subject header
                                   information.  If this option is supplied,
                                   the query and subject header information is
                                   not pattern matched (by the -p pattern).
     -o|--outfile-suffix  OPTIONAL [nothing] This suffix is added to the input
                                   file names to use as output files.
                                   Redirecting a file into this script
                                   inactivates this option and sends output to
                                   standard out.
     --split              OPTIONAL [Off] This will cause each matched line/
                                   record to go into a different output file
                                   with a numbered suffix indicating the
                                   number of the match (e.g. the first match
                                   will go into input_file.1).  If supplied
                                   with -o, the number will appear before the
                                   supplied suffix  (e.g. the first match will
                                   go into input_file.1supplied_suffix).
     --verbose            OPTIONAL [Off] Verbose mode.
     -q|--quiet           OPTIONAL [Off] Quiet mode.  Turns off warnings and
                                   errors.  Cannot be used with the verbose
                                   flag.
     -h|--help            OPTIONAL [Off] Help.  Use this option to see sample
                                   input files and an explanation.
     --version            OPTIONAL [Off] Print software version and quit.
     -D|--debug           OPTIONAL [Off] Debug mode.

end_print

    return(0);
  }


##
## Subroutine that prints errors with a leading program identifier
##
sub error
  {
    #Gather and concatenate the error message and split on hard returns
    my @error_message = split("\n",join('',@_));
    pop(@error_message) if($error_message[-1] !~ /\S/);

    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Assign the values from the calling subroutine
    #but if called from main, assign the values from main
    my($junk1,$junk2,$line_num,$calling_sub);
    (($junk1,$junk2,$line_num,$calling_sub)) = caller(1) ||
      (($junk1,$junk2,$line_num) = caller());

    #Edit the calling subroutine string
    $calling_sub =~ s/^.*?::(.+)$/$1:/ if(defined($calling_sub));

    #Put location information at the beginning of each line of the message
    foreach my $line (@error_message)
      {
	print STDERR ("ERROR:$script:",
		      ($line_num ?
		       "LINE$line_num:" : "REFERENCED_SUBROUTINE:"),
		      (defined($calling_sub) ? $calling_sub : ''),
		      ' ')
	  if($line =~ /\S/);
	print STDERR ($line,"\n");
      }

    #Return success
    return(0);
  }


##
## Subroutine that prints warnings with a leader string
##
sub warning
  {
    #Gather and concatenate the warning message and split on hard returns
    my @warning_message = split("\n",join('',@_));
    pop(@warning_message) if($warning_message[-1] !~ /\S/);

    #Put leader string at the beginning of each line of the message
    foreach my $line (@warning_message)
      {print STDERR (($line =~ /\S/ ? 'WARNING: ' : ''),$line,"\n")}

    #Return success
    return(0);
  }


##
## Subroutine that gets a line of input and accounts for carriage returns that
## many different platforms use instead of hard returns.  Note, it uses a
## global array reference variable ($infile_line_buffer).
##
sub getLineOLD
  {
    my $file_handle = $_[0];

    #Set a global array variable if not already set
    $main::infile_line_buffer = {} if(!defined($main::infile_line_buffer));
    if(!exists($main::infile_line_buffer->{$file_handle}))
      {$main::infile_line_buffer->{$file_handle} = []}

    #If this sub was called in array context
    if(wantarray)
      {
	#Check to see if this file handle has anything remaining in its buffer
	#and if so return it with the rest
	if(scalar(@{$main::infile_line_buffer->{$file_handle}}) > 0)
	  {return(@{$main::infile_line_buffer->{$file_handle}},
		  map {split("\r",$_)} <$file_handle>)}

	#Otherwise return everything else
	return(map {split("\r",$_)} <$file_handle>);
      }

    #If the file handle's buffer is empty, put more on
    if(scalar(@{$main::infile_line_buffer->{$file_handle}}) == 0)
      {@{$main::infile_line_buffer->{$file_handle}} = split("\r",
							    <$file_handle>)}

    #Shift off and return the first thing in the buffer for this file handle
    return($_ = shift(@{$main::infile_line_buffer->{$file_handle}}));
  }



##
## Subroutine that gets a line of input and accounts for carriage returns that
## many different platforms use instead of hard returns.  Note, it uses a
## global array reference variable ($infile_line_buffer) to keep track of
## buffered lines from multiple file handles.
##
sub getLine
  {
    my $file_handle = $_[0];

    #Set a global array variable if not already set
    $main::infile_line_buffer = {} if(!defined($main::infile_line_buffer));
    if(!exists($main::infile_line_buffer->{$file_handle}))
      {$main::infile_line_buffer->{$file_handle}->{FILE} = []}

    #If this sub was called in array context
    if(wantarray)
      {
	#Check to see if this file handle has anything remaining in its buffer
	#and if so return it with the rest
	if(scalar(@{$main::infile_line_buffer->{$file_handle}->{FILE}}) > 0)
	  {
	    return(@{$main::infile_line_buffer->{$file_handle}->{FILE}},
		   map
		   {
		     #If carriage returns were substituted and we haven't
		     #already issued a carriage return warning for this file
		     #handle
		     if(s/\r\n|\n\r|\r/\n/g &&
			!exists($main::infile_line_buffer->{$file_handle}
				->{WARNED}))
		       {
			 $main::infile_line_buffer->{$file_handle}->{WARNED}
			   = 1;
			 warning("Carriage returns were found in your file ",
				 "and replaced with hard returns");
		       }
		     split(/(?<=\n)/,$_);
		   } <$file_handle>);
	  }
	
	#Otherwise return everything else
	return(map
	       {
		 #If carriage returns were substituted and we haven't already
		 #issued a carriage return warning for this file handle
		 if(s/\r\n|\n\r|\r/\n/g &&
		    !exists($main::infile_line_buffer->{$file_handle}
			    ->{WARNED}))
		   {
		     $main::infile_line_buffer->{$file_handle}->{WARNED}
		       = 1;
		     warning("Carriage returns were found in your file ",
			     "and replaced with hard returns");
		   }
		 split(/(?<=\n)/,$_);
	       } <$file_handle>);
      }

    #If the file handle's buffer is empty, put more on
    if(scalar(@{$main::infile_line_buffer->{$file_handle}->{FILE}}) == 0)
      {
	my $line = <$file_handle>;
	if(!eof($file_handle))
	  {
	    if($line =~ s/\r\n|\n\r|\r/\n/g &&
	       !exists($main::infile_line_buffer->{$file_handle}->{WARNED}))
	      {
		$main::infile_line_buffer->{$file_handle}->{WARNED} = 1;
		warning("Carriage returns were found in your file and ",
			"replaced with hard returns");
	      }
	    @{$main::infile_line_buffer->{$file_handle}->{FILE}} =
	      split(/(?<=\n)/,$line);
	  }
	else
	  {@{$main::infile_line_buffer->{$file_handle}->{FILE}} = ($line)}
      }

    #Shift off and return the first thing in the buffer for this file handle
    return($_ = shift(@{$main::infile_line_buffer->{$file_handle}->{FILE}}));
  }



sub debug
  {
    return(0) unless($DEBUG);

    #Gather and concatenate the error message and split on hard returns
    my @debug_message = split(/\n/,join('',@_));

    #Assign the values from the calling subroutine
    #but if called from main, assign the values from main
    my($junk,$line_num,$calling_sub);
    (($junk,$junk,$line_num,$calling_sub) = caller(1)) ||
      (($junk,$junk,$line_num) = caller());
    $line_num = 0 unless(defined($line_num));

    #Edit the calling subroutine string
    $calling_sub =~ s/^.*?::(.+)$/$1:/ if(defined($calling_sub));

    #Put location information at the beginning of each line of the message
    foreach my $line (@debug_message)
      {
	print STDERR ("DEBUG:LINE$line_num:",
		      (defined($calling_sub) ? $calling_sub : ''),
		      ' ');
	print STDERR ($line,"\n");
      }

    #Return success
    return(0);
  }


#This sub marks the time (which it pushes onto an array) and in scalar context
#returns the time since the last mark by default or supplied mark (optional)
#In array context, the time between all marks is always returned regardless of
#a supplied mark index
#A mark is not made if a mark index is supplied
#Uses a global time_marks array reference
sub markTime
  {
    #Record the time
    my $time = time();

    #Set a global array variable if not already set
    $main::time_marks = [] if(!defined($main::time_marks));

    #Read in the time mark index or set the default value
    my $mark_index = (defined($_[0]) ? $_[0] : -1);  #Optional Default: -1

    #Error check the time mark index sent in
    if($mark_index > (scalar(@$main::time_marks) - 1))
      {
	error("Supplied time mark index is larger than the size of the ",
	      "time_marks array.\nThe last mark will be set.");
	$mark_index = -1;
      }

    #Calculate the time since the time recorded at the time mark index
    my $time_since_mark = (scalar(@$main::time_marks) ?
			   ($time - $main::time_marks->[$mark_index]) : 0);

    #Add the current time to the time marks array
    push(@$main::time_marks,$time)
      if(!defined($_[0]) || scalar(@$main::time_marks) == 0);

    #If called in array context, return time between all marks
    if(wantarray)
      {
	if(scalar(@$main::time_marks) > 1)
	  {return(map {$main::time_marks->[$_ - 1] - $main::time_marks->[$_]}
		  (1..(scalar(@$main::time_marks) - 1)))}
	else
	  {return(())}
      }

    #Return the time since the time recorded at the supplied time mark index
    return($time_since_mark);
  }

##
## Subroutine that gets "counted lines" and lines that follow until the next
## counted line is reached.  The first line in a file is always counted.
##
sub getCountedLine
  {
    my $count_pattern = $_[0];
    my $file_handle   = $_[1];

    #Set a global array variable if not already set
    $main::counted_line_buffer = {} if(!defined($main::counted_line_buffer));
    if(!exists($main::counted_line_buffer->{$file_handle}))
      {$main::counted_line_buffer->{$file_handle} = []}

    #If the file handle's buffer is empty, put more on
    if(scalar(@{$main::counted_line_buffer->{$file_handle}}) == 0)
      {$main::counted_line_buffer->{$file_handle}->[0] = getLine($file_handle)}

    #Now, assuming the line we just read in IS counted, keep adding lines until
    #another counted line is encountered
    while(my $counted_line = getLine($file_handle))
      {
	if($counted_line =~ /$count_pattern/)
	  {
	    debug("LINE: $counted_line\nMATCHED PATTERN: /$count_pattern/");
	    push(@{$main::counted_line_buffer->{$file_handle}},$counted_line);
	    last unless(wantarray);
	  }
	else
	  {
	    debug("Appending line: [$counted_line] ARRAY SIZE: ",
		  scalar(@{$main::counted_line_buffer->{$file_handle}}),"\n");
	    $main::counted_line_buffer->{$file_handle}->[-1] .= $counted_line;
	  }
      }

    if(wantarray)
      {
	#Create a temporary array so the buffer can be emptied
	my @tmp = @{$main::counted_line_buffer->{$file_handle}};
	$main::counted_line_buffer->{$file_handle} = [];

	#Return the temporary array
	return(@tmp);
      }

    debug("TEST: RETURNING LINE(S):\n",
	  "$main::counted_line_buffer->{$file_handle}->[0]\n")
      unless(!defined($main::counted_line_buffer->{$file_handle}->[0]));

    #Shift off and return the first thing in the buffer for this file handle
    return($_ = shift(@{$main::counted_line_buffer->{$file_handle}}));
  }

##
## This subroutine checks for files with spaces in the name before doing a glob
## (which breaks up the single file name improperly even if the spaces are
## escaped).  The purpose is to allow the user to enter input files using
## double quotes and un-escaped spaces as is expected to work with many
## programs which accept individual files as opposed to sets of files.  If the
## user wants to enter multiple files, it is assumed that space delimiting will
## prompt the user to realize they need to escape the spaces in the file names.
## Note, this will not work on sets of files containing a mix of spaces and
## glob characters.
##
sub sglob
  {
    my $command_line_string = $_[0];
    unless(defined($command_line_string))
      {
	warning("Undefined command line string encountered.");
	return($command_line_string);
      }
    return(#If matches unescaped spaces
	   $command_line_string =~ /(?!\\)\s+/ &&
	   #And all separated args are files
	   scalar(@{[glob($command_line_string)]}) ==
	   scalar(@{[grep {-e $_} glob($command_line_string)]}) ?
	   #Return the glob array
	   glob($command_line_string) :
	   #If it's a series of all files with escaped spaces
	   (scalar(@{[split(/(?!\\)\s/,$command_line_string)]}) ==
	    scalar(@{[grep {-e $_} split(/(?!\\)\s+/,$command_line_string)]}) ?
	    split(/(?!\\)\s+/,$command_line_string) :
	    #Return the glob if a * is found or the single arg
	    ($command_line_string =~ /\*/ ? glob($command_line_string) :
	     $command_line_string)));
  }

