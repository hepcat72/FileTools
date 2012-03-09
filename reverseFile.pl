#!/usr/bin/perl -w

#Reverse File
#Generated using perl_script_template.pl 1.8
#Robert W. Leach
#10/8/2004
#Los Alamos National Laboratory
#Copyright 2004

my $software_version_number = '1.0';

##
## Start Main
##

use strict;
use Getopt::Long;

#Declare variables
my($verbose,
   $quiet,
   $help,
   @input_files,
   $version,
   $DEBUG,
   $preserve_args,
   $outfile_suffix,
   $current_output_file);

#If there are no arguments and no files directed or piped in
if(scalar(@ARGV) == 0 && (-t STDIN || eof(STDIN)))
  {
    usage();
    exit(0);
  }

#Preserve the agruments so the user can access them later if needed by calling
#getCommand
$preserve_args = [@ARGV];

#Get the input options
GetOptions('D|debug!'           => \$DEBUG,          #OPTIONAL [Off]
	   'version!'           => \$version,        #OPTIONAL [Off]
	   'v|verbose!'         => \$verbose,        #OPTIONAL [Off]
	   'q|quiet!'           => \$quiet,          #OPTIONAL [Off]
	   'h|help!'            => \$help,           #OPTIONAL [Off]
	   'o|outfile-suffix=s' => \$outfile_suffix, #OPTIONAL [undef]
	   '<>'                 => sub {push(@input_files,$_[0])},
          );                                         #REQUIRED STDIN acceptable

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

if($verbose)
  {print STDERR ("Run conditions: ",getCommand(1),"\n")}

#For each input file of the same type
foreach my $input_file (@input_files)
  {
    #Open and select the next output file if an output file name suffix has
    #been defined
    if(defined($outfile_suffix))
      {
	#Set the current output file name
	$current_output_file = $input_file . $outfile_suffix;

	if($verbose)
	  {print STDERR ("Opening output file: [$current_output_file].\n")}

	#Open the output file
	unless(open(OUTPUT,">$current_output_file"))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open output file: [$current_output_file]\n$!")
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

    #Open the input file
    unless(open(INPUT,$input_file))
      {
	#Report an error and iterate if there was an error
	error("Unable to open input file: [$input_file]\n$!") unless($quiet);
	next;
      }

    #Keep track of input file's line number
    my @file_lines = (getLine(\*INPUT,$quiet));

    #Print the last line of the file
    print($file_lines[-1]);

    #If the last line doesn't have a hard return (which is assumed to be at the
    #end), print a hard return
    if($file_lines[-1] !~ /\n/)
      {
	warning("A hard return has been added to the last line of your file.")
	  unless($quiet);
	print("\n");
      }

    #print the rest of the lines in reverse order
    foreach(reverse(@file_lines[0..(scalar(@file_lines) - 2)]))
      {print}

    if($verbose)
      {print STDERR ("\n[",
		     ($input_file eq '-' ? 'STDIN' : $input_file),
		     '] Done.  Closing File.  Time taken: [',
		     scalar(markTime()),
		     " Seconds]\n")}

    close(INPUT);

    #If an output file name suffix is set
    if(defined($outfile_suffix))
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
2/9/2005
Bioscience Division
Bioinformatics and Computational Biology Group, B5
Biodefense Informatics Team
MS M888
Los Alamos National Laboratory
Los Alamos, NM 87545
robleach\@lanl.gov

* WHAT IS THIS: This script reverses the order of the lines of a file so that
                for example, the first line becomes the last and the last, the
                first, etc.

* INPUT FORMAT: Any text file.  Warning for biologists: If you are running a
                sequence file and you have sequences longer than one line, the
                resulting file will not contain valid sequences because
                although the line order will be reversed, the sequence on each
                line won\'t be reversed.

* OUTPUT FORMAT: A text file whose lines are the reverse order of the input
                 file.

end_print

    return(0);
  }



sub usage
  {
    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    print << "end_print";

USAGE1: $script input_files [-o .suffix] [-v] [-q] [-h] [--version] [-D]
USAGE2: $script \[-v] [-h] [--version] [-q] [-D] < input_file

                          REQUIRED Input file(s).  Standard input via
                                   redirection is acceptable.  There is no flag
                                   required for input files.
     -o|--outfile-suffix  OPTIONAL [nothing] This suffix is added to the input
                                   file names to use as output files.
                                   Redirecting a file into this script
                                   inactivates this option and sends output to
                                   standard out.
     -v|--verbose         OPTIONAL [Off] Verbose mode.
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
sub getLine
  {
    my $file_handle = $_[0];
    my $quiet       = $_[1];

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
		     #If carriage returns were substituted, we're not in quiet
		     #mode, and we haven't already issued a carriage return
		     #warning for this file handle
		     if(s/\r\n|\n\r|\r/\n/g &&
			!$quiet &&
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
		 #If carriage returns were substituted, we're not in quiet
		 #mode, and we haven't already issued a carriage return
		 #warning for this file handle
		 if(s/\r\n|\n\r|\r/\n/g &&
		    !$quiet &&
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
	if($line =~ s/\r\n|\n\r|\r/\n/g &&
	   !$quiet &&
	   !exists($main::infile_line_buffer->{$file_handle}->{WARNED}))
	  {
	    $main::infile_line_buffer->{$file_handle}->{WARNED} = 1;
	    warning("Carriage returns were found in your file and replaced ",
		    "with hard returns");
	  }
	@{$main::infile_line_buffer->{$file_handle}->{FILE}} =
	  split(/(?<=\n)/,$line);
      }

    #Shift off and return the first thing in the buffer for this file handle
    return($_ = shift(@{$main::infile_line_buffer->{$file_handle}->{FILE}}));
  }


sub debug
  {
    #Gather and concatenate the error message and split on hard returns
    my @debug_message = split("\n",join('',@_));
    pop(@debug_message) if($debug_message[-1] !~ /\S/);

    #Assign the values from the calling subroutine
    #but if called from main, assign the values from main
    my($junk1,$junk2,$line_num,$calling_sub);
    (($junk1,$junk2,$line_num,$calling_sub)) = caller(1) ||
      (($junk1,$junk2,$line_num) = caller());

    #Edit the calling subroutine string
    $calling_sub =~ s/^.*?::(.+)$/$1:/ if(defined($calling_sub));

    #Put location information at the beginning of each line of the message
    foreach my $line (@debug_message)
      {
	print STDERR ("DEBUG:LINE$line_num:",
		      (defined($calling_sub) ? $calling_sub : ''),
		      ' ')
	  if($line =~ /\S/);
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

    #Set a global array variable if not already set to contain (as the first
    #element) the time the program started (NOTE: "$^T" is a perl variable that
    #contains the start time of the script)
    $main::time_marks = [$^T] if(!defined($main::time_marks));

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
    my $time_since_mark = $time - $main::time_marks->[$mark_index];

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

sub getCommand
  {
    my $perl_path_flag = $_[0];

    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    my $arguments = [@$preserve_args];
    foreach my $arg (@$arguments)
      {if($arg =~ /(?<!\/)\s/)
	 {$arg = '"' . $arg . '"'}}

    my($command);
    if($perl_path_flag)
      {
	$command = `which $^X`;
	chomp($command);
	$command .= ' ';
      }
    $command .= join(' ',($0,@$arguments));

    #Note, this sub doesn't add any redirected files in or out

    return($command);
  }
