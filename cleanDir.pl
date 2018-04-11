#!/usr/bin/perl -w

cleanDir(map {$_ eq 'undef' ? undef : $_} @ARGV);

#Deletes files that: are older than $del_older_than (default: everything),
#match the delete_pattern (default: everything), don't match $keep_pattern
#(default: nothing), represent a number of files greater than keep_at_least,
#and do not match any pattern in exclude_pats (default: '^\.' - these files are
#not counted in the number kept, thus it affects the keep_at_least behavior),
#Every condition must be met in order to delete a file.  Files are traversed in
#order of ascending age (based on last modified date))
sub cleanDir
  {
    my $dir            = $_[0];
    my $del_older_than = (defined($_[1]) ? $_[1] : -1);
    my $keep_at_least  = (defined($_[2]) ? $_[2] : 0);
    my $keep_size      = (defined($_[3]) ? $_[3] : 0);
    my $dry_run        = (defined($_[4]) ? $_[4] : 0);
    my $delete_pattern = (defined($_[5]) ? $_[5] : '.');
    my $keep_pattern   = (defined($_[6]) ? $_[6] : '__DONOTMATCH__');
    my $exclude_pats   = (defined($_[7]) ? $_[7] : ['^\.']);

    my $secs_per_min  = 60;
    my $secs_per_hour = 3600;
    my $secs_per_day  = 86400;
    my $secs_per_week = 604800;
    my $secs_per_mon  = 2628000; #average (not inc. leap year)
    my $secs_per_year = 31536000;

    #Convert del_older_than to seconds
    my $sec_num = $del_older_than;
    $sec_num =~ s/\D.*$//g;
    if($del_older_than =~ /s/i || $del_older_than !~ /\D/)
      {$del_older_than = $sec_num}
    elsif($del_older_than =~ /m/)
      {$del_older_than = $sec_num * $secs_per_min}
    elsif($del_older_than =~ /h/i)
      {$del_older_than = $sec_num * $secs_per_hour}
    elsif($del_older_than =~ /d/i)
      {$del_older_than = $sec_num * $secs_per_day}
    elsif($del_older_than =~ /w/i)
      {$del_older_than = $sec_num * $secs_per_week}
    elsif($del_older_than =~ /M/)
      {$del_older_than = $sec_num * $secs_per_mon}
    elsif($del_older_than =~ /y/i)
      {$del_older_than = $sec_num * $secs_per_year}

    if($del_older_than !~ /\d/)
      {$del_older_than = 0}

    my $bytes_per_kilo = 1000;
    my $bytes_per_meg  = 1000000;
    my $bytes_per_gig  = 1000000000;
    my $bytes_per_tera = 1000000000000;

    my $sizenum = $keep_size;
    $sizenum =~ s/\D.*//;
    if($sizenum !~ /\d/)
      {$sizenum = 0}

    if($keep_size =~ /\d *b/i || $keep_size !~ /\D/)
      {$keep_size = $sizenum}
    elsif($keep_size =~ /k/i)
      {$keep_size = $sizenum * $bytes_per_kilo}
    elsif($keep_size =~ /m/i)
      {$keep_size = $sizenum * $bytes_per_meg}
    elsif($keep_size =~ /g/i)
      {$keep_size = $sizenum * $bytes_per_gig}
    elsif($keep_size =~ /t/i)
      {$keep_size = $sizenum * $bytes_per_tera}

    unless(defined($dir))
      {
	print("\nUSAGE: cleanDir.pl directory [file_age min_keep max_size ",
	      "dry_run{0,1} delete_pattern keep_pattern ignore_pattern]\n\n",
	      "file_age   delete files whose last modified date is older ",
	      "than this. Integer values only. Format example: 1d ",
	      "(= 1 day).\n\n           s=seconds (default)\n           ",
	      "m=minutes\n           h=hours\n           d=days\n           ",
	      "w=weeks\n           M=months\n           y=years\n\n",
	      "min_keep   Always keep at least this many files (not counting ",
	      "files matching ignore_pattern).\n\nmax_size   Keep the ",
	      "youngest files up to this cumulative size. Format example: ",
	      "10m\n\n           b=bytes\n           k=kilobytes\n           ",
	      "m=megabytes\n           g=gigabytes\n           ",
	      "t=terabytes\n\n           Subdirectories and ignore_pattern ",
	      "files are included in the size calculation. Files without ",
	      "accessible permissions are not included.\n\ndry_run    ",
	      "0=delete files, 1=show files that would be deleted. Default: ",
	      "0.\n\nAll patterns are perl regular expressions.  Defaults ",
	      "will delete all files except dot files.  Sub-directories are ",
	      "never deleted - this script only deletes files.\n\n");
	return;
      }

    $dir .= "/" unless($dir =~ /\/$/);

    unless(opendir(DIR,$dir))
      {
	print "CleanDir:Unable to open the directory: [$dir].\n";
	return;
      }
    my @files = readdir(DIR);
    closedir DIR;

    #For every item that is guaranteed to be kept
    if($dry_run)
      {print("DRY RUN: The following files will be deleted in directory ",
	     "'$dir':\n")}

    foreach my $file (grep {my $f=$_;-d "$dir$f" || $f =~ /$keep_pattern/ ||
			      scalar(grep {$f =~ /$_/} @$exclude_pats)}
		      @files)
      {
	next if($file eq '.' || $file eq '..');
	if(-d "$dir$file")
	  {$cum_size +=
	     `du -d 0 "$dir$file" 2> /dev/null | cut -f 1 2> /dev/null`}
	else
	  {$cum_size += (stat("$dir$file"))[7]}
      }

    my $currtime = time();   #Gives time in seconds since "epoch"
    my $num_kept = 0;
    my $cum_size = 0;

    #For each item in the directory by ascending age (based on lmd)
    foreach my $file (sort {(stat("$dir$b"))[9] <=> (stat("$dir$a"))[9]}
		      grep {!(-d "$dir$_")} @files)
      {
	my @file_stat = stat("$dir$file");
	$file_stat[9] =~ /(\d+)/;
	my $filetimenum = $1;
	my $size = $file_stat[7];
	if(($currtime - $filetimenum) > $del_older_than &&
	   $file !~ /$keep_pattern/ && $file =~ /$delete_pattern/ &&
	   $num_kept >= $keep_at_least && ($cum_size + $size) > $keep_size &&
	   scalar(grep {$file =~ /$_/} @$exclude_pats) == 0)
	  {
	    if($dry_run)
	      {print("$file\n")}
	    else
	      {unlink("$dir$file") || print STDERR "$!\n"}
	  }
	elsif(scalar(grep {$file =~ /$_/} @$exclude_pats) == 0)
	  {$num_kept++}

	#If this is not a guaranteed kept file (because that size was pre-
	#calculated), increment the kept size
	if(scalar(grep {$file =~ /$_/} @$exclude_pats) == 0 &&
	   $file !~ /$keep_pattern/)
	  {$cum_size += $size}
      }
  }
