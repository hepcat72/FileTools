#!/usr/bin/perl

#USAGE: numrowscols.pl file
#Outputs the number of consecutive rows with the same number of columns

use CommandLineInterface;

setScriptInfo(VERSION => '2.0',
              CREATED => '2/5/2014',
              AUTHOR  => 'Robert William Leach',
              CONTACT => 'rleach@princeton.edu',
              COMPANY => 'Princeton University',
              LICENSE => 'Copyright 2018',
              HELP    => ('This script tells you how many consecutive rows ' .
			  'have the same number of columns.'));

setDefaults(HEADER        => 0,
	    ERRLIMIT      => 3,
	    COLLISIONMODE => 'error', #,merge,rename (when outfile conflict)
	    DEFRUNMODE    => 'usage',
	    DEFSDIR       => undef);

my $iid = addInfileOption(GETOPTKEY   => 'i|infile=s',
			  REQUIRED    => 1,
			  PRIMARY     => 1,
			  FLAGLESS    => 1,
			  SMRY_DESC   => 'Tab delimited text file.',
			  FORMAT_DESC => 'Tab delimited input text file.');

my $ttid = addOutfileTagteamOption(GETOPTKEY_SUFF   => 'o|outfile-suffix=s',
				   GETOPTKEY_FILE   => 'outfile=s',
				   FILETYPEID       => $iid,
				   PRIMARY          => 1,
				   DETAIL_DESC_SUFF => ('Extension appended ' .
							'to file submitted ' .
							'via -i.'),
				   DETAIL_DESC_FILE => 'Outfile.');

while(nextFileCombo())
  {
    my $file = getInfile($iid);
    my $ofile = getOutfile($ttid);

    openIn(*FILE,$file)  || next;
    openOut(*OUT,$ofile) || next;

    print("$file\n#Rows\tCols\n");

    $cnt  = 0;
    $num  = 0;
    $pnum = 0;

    while(getLine(*FILE))
      {
	$num = scalar(split(/\t/,$_,-1));

	if($pnum > 0 && $num == $pnum)
	  {$cnt++}
	elsif($pnum == 0)
	  {$cnt=1}
	else
	  {
	    print("$cnt\t$pnum\n");
	    $cnt=1;
	  }

	$pnum=$num
      }

    print("$cnt\t$pnum\n");

    closeIn(*FILE);
    closeOut(*OUT);
  }
