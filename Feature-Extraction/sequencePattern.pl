=head
Sequence Feature Extraction

pattern: [FYL][LIM][WYLF][STGP][FYHLW][ILTY][PH][QFPS][V][FYL]

input eg.:
LYTTGHTYU
YTTTGHTYU
TGTTGHTYU

output eg.:
0:1 1:0 2:0 3:1 4:0 5:0 6:0 7:0 8:0 9:0 
0:1 1:0 2:0 3:1 4:0 5:0 6:0 7:0 8:0 9:0 
0:0 1:0 2:0 3:1 4:0 5:0 6:0 7:0 8:0 9:0 


=cut

print "enter file name:";
$filename=<STDIN>;
unless(open(FH1,"$filename"))
{
	print "file not found.";
}

@cont=<FH1>;
close FH1;

print "would you like to see file contains!!(y/n):";
$ans=<STDIN>;
if($ans=~/[yY]/)
{
print "Your File Contains:\n @cont";
}

open(FH,">output.txt");
@ptr=("[FYL]","[LIM]","[WYLF]","[STGP]","[FYHLW]","[ILTY]","[PH]","[QFPS]","[V]","[FYL]");


foreach$seq(@cont){

	unless(length$seq<=11)
	{
		print FH  length$seq,"$seq is longer.";
	}
	@seq=split('',$seq);
	for $i (0..9){
					if($seq[$i]=~/$ptr[$i]/)
						{print FH "$i:1 ";}
					else
						{print FH "$i:0 ";}
}


print FH "\n";
}
print "\ncheck your result in : output.txt";
close FH;
exit;
