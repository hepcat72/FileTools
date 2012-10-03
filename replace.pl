#!/usr/bin/perl -w

#replace.pl - search & replace text in files
#Generated using perl_script_template.pl 1.20
#Robert W. Leach
#robleach@lanl.gov
#4/12/2005
#Los Alamos National Laboratory
#Copyright 2005

my $template_version_number = '1.20';
my $software_version_number = '1.6';

##
## Start Main
##

use strict;
use Getopt::Long;

#Declare variables
my(@input_files,
   $replace,
   $with,
   $current_output_file);
my $extensions = [];
#Initialize flags
my $recurse_flag    = 0;
my $regex_mode_flag = 0;
my $force           = 0;
my $verbose         = 0;
my $quiet           = 0;
my $help            = 0;
my $version         = 0;
my $DEBUG           = 0;
my $outfile_suffix  = '';

#Preserve the agruments for getCommand
my $preserve_args = [@ARGV];

#If there are no arguments and no files directed or piped in
if(scalar(@ARGV) == 0)
  {
    usage();
    exit(0);
  }

#Get the input options
GetOptions('r|replace=s'        => \$replace,                #REQUIRED
	   'w|with=s'           => \$with,                   #REQUIRED
	   'e|extensions=s'     => sub {push(@$extensions,   #OPTIONAL [undef]
					     $_[1])},
	   'p|perl-regex-mode!' => \$regex_mode_flag,        #OPTIONAL [Off]
	   'recurse!'           => \$recurse_flag,           #OPTIONAL [Off]
	   'f|force!'           => \$force,                  #OPTIONAL [Off]
	   'd|debug!'           => \$DEBUG,                  #OPTIONAL [Off]
	   'version!'           => \$version,                #OPTIONAL [Off]
	   'v|verbose!'         => \$verbose,                #OPTIONAL [Off]
	   'q|quiet!'           => \$quiet,                  #OPTIONAL [Off]
	   'h|help!'            => \$help,                   #OPTIONAL [Off]
	   'o|outfile-suffix=s' => \$outfile_suffix,         #OPTIONAL [undef]
	   '<>'                 => sub {if($_[0] eq '.')     #OPTIONAL [.]
					  {$_[0] = "$ENV{PWD}/*"}
					elsif($_[0] eq '..')
					      {$_[0] = "$ENV{PWD}/../*"}
					push(@input_files,$_[0])},
          );

warning("This version overwrites symbolic links with the actual file.");

#Print the debug mode (it checks the value of the DEBUG global variable)
debug("Debug mode on.");

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
    verbose("Generated using perl_script_template.pl\n",
	    "Version $template_version_number\n",
	    "Robert W. Leach\n",
	    "robleach\@lanl.gov\n",
	    "3/24/2005\n",
	    "Los Alamos National Laboratory\n",
	    'Copyright 2005');
    exit(0);
  }

#Check validity of verbosity options
if($verbose && $quiet)
  {
    $quiet = 0;
    error("You cannot supply verbose and quiet flags at the same time.");
    exit(2);
  }

#Make the extensions into patterns
@$extensions = map {quotemeta($_) . "\$"} @$extensions;
debug("Extensions: ['",join("','",@$extensions),"']");

#Grab search string from flagless parameters if it wasn't supplied and the
#first flagless param isn't a file/directory
if((!defined($replace) || $replace eq '') &&
   scalar(@input_files) && !(-e $input_files[0]) && $input_files[0] ne '')
  {$replace = shift(@input_files)}
elsif(!defined($replace) || $replace eq '')
  {
    error("No search string supplied.");
    usage();
    exit(1);
  }

#Grab replace string from flagless parameters if it wasn't supplied and the
#first flagless param isn't a file/directory
if((!defined($with) || $with eq '') &&
   scalar(@input_files) && !(-e $input_files[0]) && $input_files[0] ne '')
  {$with = shift(@input_files)}
elsif(!defined($with))
  {
    warning("No replace string was supplied.  You must explicitly supply an ",
	    "empty string if there's no replacement.");
    usage();
    exit(2);
  }

#Now that all the possible flagless options are out of the input files,
#glob the input files
@input_files = map {glob($_)} @input_files;

my $captures = [];

#Fix the search string to escape metacharacters (if we're not in regex mode)
if(!$regex_mode_flag)
  {$replace = quotemeta($replace)}
#Else see what substitutions might the user be asking for
elsif($replace =~ /\(/ && $with =~ /\$\d/)
  {while($with =~ /(?<!\\)(\$\d+)/g)
     {push(@$captures,$1)}}

#Fix the replace string to escape forward slashes
#$with =~ s/\//\\\//g;

#Make sure there is input
if(scalar(@input_files) == 0)
  {push(@input_files,glob("$ENV{PWD}/*"))}
debug("Input files: [",join(',',@input_files),"]");

#Issue a warning if force is not used and the outfile suffix has been
#explicitly set to an empty string
if(defined($outfile_suffix) && $outfile_suffix eq '' && !$force)
  {print STDERR ("You must use the force flag (-f) if you do not supply an ",
		 "output file suffix.  Note that this cannot be undone.\n")}
debug("outfile_suffix: [$outfile_suffix]");

verbose("Run conditions: ",getCommand(1),"\n");




if(!defined($outfile_suffix))
  {verbose("Overwrite mode (-f|--force) on.")}

#For each input file of the same type
foreach my $input_file (@input_files)
  {
    searchrep($input_file,
	      $replace,
	      $with,
	      $recurse_flag,
	      $force,
	      $extensions,
	      $captures);
  }





#Report the stop time if verbose mode is on
verbose("Total Running Time: ",scalar(markTime(0))," Seconds\n");

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
Copyright 2005
Robert W. Leach
7/12/2006
Bioscience Division
Bioinformatics and Computational Biology Group, B5
Biodefense Team
MS M888
Los Alamos National Laboratory
Los Alamos, NM 87545
robleach\@lanl.gov

* WHAT IS THIS: This script searches for text and replaces it with a replace
                string in input files which have a specified extension.

* INPUT FORMAT: Text files.

* OUTPUT FORMAT: Text files.

end_print

    return(0);
  }



sub usage
  {
    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    print << "end_print";

USAGE1: $script input_files -s search_string -r replace_string [-p] [--recurse] [-e "extension(s)"] [-f] [-o .suffix] [-v] [-q] [-h] [--version] [-d]
USAGE2: $script -s search_string -r replace_string [-p] [--recurse] [-e "extension(s)"] [-f] [-o .suffix] [-v] [-q] [-h] [--version] [-d] < input_file

                          REQUIRED Input file(s).  Standard input via
                                   redirection is acceptable.  There is no flag
                                   required for input files.
     -r|--replace*        REQUIRED The string to search for in the input files.
                                   *The flag may be omitted if this is the
                                   first argument.
     -w|--with*           REQUIRED The string to replace the search string with
                                   in the input files.
                                   *The flag may be omitted if this is the
                                   second argument and no flag was used for the
                                   search string.
     -p|--perl-regex-mode OPTIONAL [Off] Treat the search string as a perl
                                   regular expression.  Also, allows the
                                   replacement to contain numeric references to
                                   matches captured in \$1, \$2, etc..
                                   E.g. `-r 'find (\\d+) matches' -w '$1'` will
                                   replace "Find 10 matches" with "10" when
                                   using the -p option.
     --recurse            OPTIONAL [Off] Search and replace recursively into
                                   all subdirectories.
     -e|--extensions      OPTIONAL [all] The extensions of the files to search.
                                   This is specifically for use with --recurse.
                                   While it will work in the current directory,
                                   it is easier to use astericks.
     -f|--force           OPTIONAL [Off] Overwrite existing files.
     -o|--outfile-suffix  OPTIONAL [undef] This suffix is added to the input
                                   file names to use as output files.
     -v|--verbose         OPTIONAL [Off] Verbose mode.
     -q|--quiet           OPTIONAL [Off] Quiet mode.  Turns off warnings and
                                   errors.  Cannot be used with the verbose
                                   flag.
     -h|--help            OPTIONAL [Off] Help.  Use this option to see sample
                                   input files and an explanation.
     --version            OPTIONAL [Off] Print software version and quit.
     -d|--debug           OPTIONAL [Off] Debug mode.

end_print

    return(0);
  }


##
## Subroutine that prints warnings with a leader string
##
sub verbose
  {
    return(0) unless($verbose);

    #Read in the first argument and determine whether it's part of the message
    #or a value for the overwrite flag
    my $overwrite_flag = $_[0];

    #If a flag was supplied as the first parameter (indicated by a 0 or 1 and
    #more than 1 parameter sent in)
    if(scalar(@_) > 1 && ($overwrite_flag eq '0' || $overwrite_flag eq '1'))
      {shift(@_)}
    else
      {$overwrite_flag = 0}

    #Read in the message
    my $verbose_message = join('',@_);

    $overwrite_flag = 1 if(!$overwrite_flag && $verbose_message =~ /\r/);

    #Initialize globals if not done already
    $main::last_verbose_size  = 0 if(!defined($main::last_verbose_size));
    $main::last_verbose_state = 0 if(!defined($main::last_verbose_state));
    $main::verbose_warning    = 0 if(!defined($main::verbose_warning));

    #Determine the message length
    my($verbose_length);
    if($overwrite_flag)
      {
	$verbose_message =~ s/\r$//;
	if(!$main::verbose_warning && $verbose_message =~ /\n|\t/)
	  {
	    warning("Hard returns and tabs cause overwrite mode to not work ",
		    "properly.");
	    $main::verbose_warning = 1;
	  }
      }
    else
      {chomp($verbose_message)}

    if(!$overwrite_flag)
      {$verbose_length = 0}
    elsif($verbose_message =~ /\n([^\n]+)$/)
      {$verbose_length = length($1)}
    else
      {$verbose_length = length($verbose_message)}

    #Print the message to standard error
    print STDERR ((!$overwrite_flag && $main::last_verbose_state ? "\n" : ''),
		  $verbose_message,
		  ($main::last_verbose_state ?
		   ' ' x ($main::last_verbose_size - $verbose_length) : ''),
		  ($overwrite_flag ? "\r" : "\n"));

    #Record the state
    $main::last_verbose_size  = $verbose_length;
    $main::last_verbose_state = $overwrite_flag;

    #Return success
    return(0);
  }

##
## Subroutine that prints errors with a leading program identifier
##
sub error
  {
    return(0) if($quiet);

    #Gather and concatenate the error message and split on hard returns
    my @error_message = split("\n",join('',@_));
    pop(@error_message) if($error_message[-1] !~ /\S/);

    $main::error_number++;

    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Assign the values from the calling subroutines/main
    my @caller_info = caller(0);
    my $line_num = $caller_info[2];
    my $caller_string = '';
    my $stack_level = 1;
    while(@caller_info = caller($stack_level))
      {
	my $calling_sub = $caller_info[3];
	$calling_sub =~ s/^.*?::(.+)$/$1/ if(defined($calling_sub));
	$calling_sub = (defined($calling_sub) ? $calling_sub : 'MAIN');
	$caller_string .= "$calling_sub(LINE$line_num):"
	  if(defined($line_num));
	$line_num = $caller_info[2];
	$stack_level++;
      }
    $caller_string .= "MAIN(LINE$line_num):";

    my $leader_string = "ERROR$main::error_number:$script:$caller_string ";

    #Figure out the length of the first line of the error
    my $error_length = length(($error_message[0] =~ /\S/ ?
			       $leader_string : '') .
			      $error_message[0]);

    #Put location information at the beginning of each line of the message
    foreach my $line (@error_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $error_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }


##
## Subroutine that prints warnings with a leader string
##
sub warning
  {
    return(0) if($quiet);

    $main::warning_number++;

    #Gather and concatenate the warning message and split on hard returns
    my @warning_message = split("\n",join('',@_));
    pop(@warning_message) if($warning_message[-1] !~ /\S/);

    my $leader_string = "WARNING$main::warning_number: ";

    #Figure out the length of the first line of the error
    my $warning_length = length(($warning_message[0] =~ /\S/ ?
				 $leader_string : '') .
				$warning_message[0]);

    #Put leader string at the beginning of each line of the message
    foreach my $line (@warning_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $warning_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

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
	if(!eof($file_handle))
	  {
	    if($line =~ s/\r\n|\n\r|\r/\n/g &&
	       !$quiet &&
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

    $main::debug_number++;

    #Gather and concatenate the error message and split on hard returns
    my @debug_message = split("\n",join('',@_));
    pop(@debug_message) if($debug_message[-1] !~ /\S/);

    #Assign the values from the calling subroutine
    #but if called from main, assign the values from main
    my($junk1,$junk2,$line_num,$calling_sub);
    (($junk1,$junk2,$line_num,$calling_sub) = caller(1)) ||
      (($junk1,$junk2,$line_num) = caller());

    #Edit the calling subroutine string
    $calling_sub =~ s/^.*?::(.+)$/$1:/ if(defined($calling_sub));

    my $leader_string = "DEBUG$main::debug_number:LINE$line_num:" .
      (defined($calling_sub) ? $calling_sub : '') .
	' ';

    #Figure out the length of the first line of the error
    my $debug_length = length(($debug_message[0] =~ /\S/ ?
			       $leader_string : '') .
			      $debug_message[0]);

    #Put location information at the beginning of each line of the message
    foreach my $line (@debug_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $debug_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
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


sub searchrep
  {
    my $input_file     = $_[0];
    my $replace        = $_[1];
    my $with           = $_[2];
    my $recurse_flag   = $_[3];
    my $force          = $_[4];
    my $extensions     = $_[5];
    my $captures       = $_[6];
    my $not_first_call = $_[7];  #DO NOT SUPPLY.  INTERNAL USE ONLY.

    debug("searchrep called with input file: [$input_file]");

    if(-d $input_file)
      {
	#Return if recurse flag is not true or this is the current or parent
	#directory on a recursive call
	return if(!$recurse_flag ||
		  (!$not_first_call && $input_file =~ /^\.\.?$/));

	verbose(1,"SEARCHING $input_file RECURSIVELY");

	$input_file .= '/' if($input_file !~ /\/$/);

	opendir(DIR,$input_file);
	my @list = readdir(DIR);
	map {$_ = $input_file . $_} @list;
	closedir(DIR);

	foreach(@list)
	  {searchrep($_,
		     $replace,
		     $with,
		     $recurse_flag,
		     $force,
		     $extensions,
		     $captures,
		     1)}
      }
    elsif(scalar(@$extensions) == 0 ||
	  (scalar(grep {$input_file =~ /$_/} @$extensions)))
      {
	return if(-B $input_file);

	verbose(1,"SEARCHING $input_file");

	#Open and select the next output file if an output file name suffix has
	#been defined
	if(defined($outfile_suffix))
	  {
	    debug("outfile suffix defined.");

	    #Set the current output file name
	    $current_output_file = $input_file . $outfile_suffix;

	    #If the output file already exists and the force flag isn't on,
	    #issue an error (unless the suffix has an empty string - in which
	    #case we've already printed a warning) and don't open the output
	    #file
	    if(!$force && -e $current_output_file)
	      {
		error("Output file exists: [$current_output_file].  ",
		      "Use the force flag (-f) to override.");
		return;
	      }
	    elsif($outfile_suffix ne '')
	      {
		#Open the output file
		unless(open(OUTPUT,">$current_output_file.replace-tmp"))
		  {
		    #Report an error and iterate if there was an error
		    error("Unable to open output file: ",
			  "[$current_output_file.replace-tmp]\n$!");
		    return;
		  }

		#Select the output file handle
		select(OUTPUT);
	      }
	  }

	#Open the input file
	if(!open(INPUT,$input_file))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open input file: [$input_file]\n$!");
	    return;
	  }
	else
	  {verbose(1,"[",
		   ($input_file eq '-' ? 'STDIN' : $input_file),
		   "] Opened input file.")}

	#Keep track of input file's line number
	my $line_num = 1;

	my $output = '';
	my $changed = 0;
	my($num_replacements);

	#For each line in the current input file
	while(getLine(\*INPUT,$quiet))
	  {
	    if(defined($outfile_suffix))
	      {verbose(1,
		       "[",
		       ($input_file eq '-' ? 'STDIN' : $input_file),
		       "] Reading line $line_num.")}

	    if(scalar(@$captures))
	      {
		my @replacements = ();
		my $last_pos     = 0;
		while(/$replace/g)
		  {
		    #Save the position we've left off at
		    my $cur_pos = pos();
		    #Save the length of the match so we can adjust pos() later
		    my $len = length($&);
		    #Save what was matched
		    my $matched = quotemeta($&);
		    #Evaluate the values of $1, $2, $3,... etc.
		    my @strs = map {eval($_)} @$captures;
		    #Create a new 'with' string
		    my $tmpwith = $with;
		    #Enter the actual values into the new 'with' string
		    #Assues order of variables is the same as the values
		    foreach my $rstr (@strs)
		      {$tmpwith =~ s/(?<!\\)\$\d+/$rstr/}
		    #Calculate where we will leave off with the substitution
		    my $newpos = $cur_pos + (length($tmpwith) - $len);
		    #Store the substitution
		    push(@replacements,[$matched,$tmpwith,$cur_pos-$len]);
		    #Now record the number of substitutions and report debug
		    #output
		    $changed++;
		    debug("Replacing [$matched] with [$tmpwith] and setting ",
			  "position from [$cur_pos] to [$newpos].");
		  }

		#Make the stored replacements in reverse order so that the
		#positions are still relevant
		foreach my $replacement (reverse(@replacements))
		  {
		    #Set the position
		    pos($_) = $replacement->[2];
		    #Make the replacement
		    s/$replacement->[0]/$replacement->[1]/;
		  }
	      }
	    elsif($num_replacements = s/$replace/$with/g)
	      {
		$changed += $num_replacements;
		debug("Replacing [$&] with [$with]");
	      }

	    if(defined($outfile_suffix) &&
	       !(!$force && -e $current_output_file) && $outfile_suffix ne '')
	      {print}
	    elsif(defined($outfile_suffix) && $outfile_suffix eq '')
	      {$output .= $_}

	    $line_num++;
	  }

	close(INPUT);

	verbose(1,
		"[",
		($input_file eq '-' ? 'STDIN' : $input_file),
		'] Closed input file.  Time taken: [',
		scalar(markTime()),
		" Seconds].");

	#If an output file name suffix is set and an output file was opened
	if(defined($outfile_suffix) && !(!$force && -e $current_output_file) &&
	   $outfile_suffix ne '')
	  {
	    #Select standard out
	    select(STDOUT);
	    #Close the output file handle
	    close(OUTPUT);

	    chmodmatch($input_file,"$current_output_file.replace-tmp");

	    verbose(1,"[$current_output_file] Opened output file.");
	    unless(rename("$current_output_file.replace-tmp",
			  $current_output_file))
	      {error("Unable to rename the temporary file: ",
		     "[$current_output_file.replace-tmp]")}
	    verbose(1,"[$current_output_file] Closed output file.");
	  }
	elsif($outfile_suffix eq '')
	  {
	    #Open a temporary output file so that if the script is killed, the
	    #original isn't lost
	    unless(open(OUTPUT,">$current_output_file.replace-tmp"))
	      {
		#Report an error and iterate if there was an error
		error("Unable to open output file: ",
		      "[$current_output_file.replace-tmp]\n$!");
		return;
	      }
	    print OUTPUT ($output);
	    close(OUTPUT);

	    verbose(1,"[$current_output_file] Opened output file.");
	    if($changed)
	      {
		chmodmatch($input_file,"$current_output_file.replace-tmp");
		unless(rename("$current_output_file.replace-tmp",
			      $current_output_file))
		  {error("Unable to rename the temporary file: ",
			 "[$current_output_file.replace-tmp]")}
	      }
	    else
	      {unlink("$current_output_file.replace-tmp")}
	    verbose(1,"[$current_output_file] Closed output file.");
	  }

	verbose("$input_file: ",
		($changed ? "$changed REPLACEMENT" . ($changed > 1 ? 'S' : '')
		 : 'UNCHANGED'));
      }
    else
      {debug("SKIPPING INPUT FILE: [$input_file]")}
  }


sub searchrepold
  {
    my $direc = $_[0];
    $direc .= '/' if($direc !~ /\/$/);

    opendir(DIR, $direc);
    my @list = readdir(DIR);
    map {$_ = $direc . $_} @list;
    closedir(DIR);

    for(my $i=0; $i<@list; $i++)
      {
	next if($list[$i] =~ /\.\.?$/);
	if(-d $list[$i] && $recurse_flag)
	  {searchrep($list[$i])}
	next if(!(-f $list[$i]));

	my $present = 'no';
	if(@$extensions)
	  {
	    foreach my $suffix (@$extensions)
	      {
		if($list[$i] =~ /$suffix/)
		  {$present = 'yes'}
	      }
	    next if($present eq 'no');
	  }

	my $outfile;
	unless(open(FILE, $list[$i]))
	  {print "Could not open $list[$i] for reading\n$!\n"; next;}

	my $changed = 0;
	while(<FILE>)
	  {
	    if(/$ARGV[0]/)
	      {
		if(s/$ARGV[0]/$ARGV[1]/g)
		  {$changed = 1}
	      }
	    $outfile .= $_;
	  }
	close FILE;


	my $backup = $list[$i] . ".old";
	`mv $list[$i] $backup` if(!$force && $changed);
	if($changed)
	  {
	    unless(open(FILE, ">$list[$i]"))
	      {print "Could not open $list[$i] for reading\n$!\n"}
	    print FILE $outfile;
	    close FILE;
	  }
	print "Changed: $list[$i]\n" if($changed);
      }
  }


sub chmodmatch
  {
    my $source_file = $_[0];
    my $target_file = $_[1];
    my $mode = getOctPermsString($source_file);
    debug("Matching permissions of $source_file and $target_file: [$mode].");
    chmod(oct($mode),$target_file);
    debug("$target_file changed to mode ",getOctPermsString($target_file))
  }


sub getOctPermsString
  {
    my $file = $_[0];
    my $lsout = `ls -l $file`;
    error($!) if($?);
    my($permstring);
    if($lsout =~ /^\S([\-rwx]{9})/i)
      {$permstring = $1}
    else
      {
	error("Unable to get the file permissions for [$file]");
	return('');
      }
    my $octstring = '0';
    while($permstring =~ /(...)/g)
      {
	my $val = 0;
	my $permset = $1;
	if($permset =~ /r/i)
	  {$val += 4}
	if($permset =~ /w/i)
	  {$val += 2}
	if($permset =~ /x/i)
	  {$val += 1}
	$octstring .= $val;
      }
    if($octstring !~ /^0[0-7]{3}$/)
      {
	error("The octal string is improperly formatted.  It's possible the ",
	      "`ls -l` command returned unexpected output.  Unable to ",
	      "determine permissions for file: [$file].");
	return('');
      }
    debug("Octal String of permissions: [$octstring].");
    return($octstring);
  }
