#!/usr/bin/perl

my $warnme          = 1; #Tell me when unclosed tags are encountered
my $numwarn         = 0;
my $nomoltilinetags = 1; #take out newlines between all <> tags before printing

$html = join('',<STDIN>);

if($nomoltilinetags)
  {
    while($html =~ /(<[^><]+)[\n\r]/)
      {$html =~ s/(<[^><]+)[\n\r]+/$1 /g}
  }

$|=1;
@ignored_tags = ('br','hr','\!\-\-','img','input','p','\?xml','meta','link');
@close_optional_tags = ('td','tr');

$indent_amt = 2;
$indent = 0;

foreach $tagsplit (split(/[\s]*(?=<)|\n|\r/,$html))
  {
    chomp($tagsplit);
    if($tagsplit =~ /<(\/?)([^\s>]+)/)
      {
	$end_flag = $1;
	$tag      = $2;
	$tag_pattern = $tag;
	$tag_pattern =~ s/(?=[^A-Za-z0-9_])/\\/g;
      }
    else
      {
	print(' ' x $indent,$tagsplit,"\n");
	next;
      }

    if(scalar(grep {$tag =~ /$_$/i || $tag =~ /\!\-\-/} @ignored_tags) ||
       $tag =~ /^!/ || $tag =~ /\/$/)
      {print(' ' x $indent,$tagsplit,"\n")}
    elsif(!$end_flag)
      {
#print "$tag eq? $flag_stack[-1]\n";
	#If this is a close-optional tag and the last tag was the same...
	if((grep {$_ =~ /^$tag_pattern$/i} @close_optional_tags) &&
	   $flag_stack[-1] =~ /^$tag_pattern$/i)
	  {
#print "YES!\n";
	    pop(@flag_stack);
	    $indent -= $indent_amt if($indent);
	  }

	push(@flag_stack,$tag);
	print(' ' x $indent,$tagsplit,"\n");
	$indent += $indent_amt;
      }
    else
      {
	while(@flag_stack && $flag_stack[-1] !~ /$tag_pattern/i)
	  {
            if($warnme)
              {
		$numwarn++;
		print STDERR ("WARNING: unclosed tag here: [",
			      join(':',@flag_stack),"].\n");
	      }
#	    print("POPPING: ",
	    pop(@flag_stack)
#		  ,"\n")
	      ;
	    $indent -= $indent_amt if($indent);
	  }
#	print("POPPING: ",
	      pop(@flag_stack)
#	      ,"\n")
		;
        $indent -= $indent_amt if($indent);
	print(' ' x $indent,$tagsplit,"\n");
      }
  }

if($indent != 0)
  {$numwarn++;print STDERR ("WARNING: Unclosed tags at the end of the file: [",join(':',@flag_stack),"].\n")}

if($numwarn)
  {print STDERR ("There were $numwarn unclosed tags encountered.\n")}
