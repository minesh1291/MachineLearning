
=head
 code is written by using this article as reference
  Shen, J., ... & Jiang, H. (2007). Predicting protein–protein interactions based only 
  on sequences information. Proceedings of the National Academy of Sciences, 104(11), 4337-4341.
=cut

use strict;

# get sequences 

	# print"enter fasta file name:";
	# $fname=<STDIN>;

my@freqs=();
my@seqs=read_multi_fasta("sample.fa");
@freqs=multi_freq_vec(@seqs);

#this is how to access the generated matrix
print $freqs[1][0];  #$freqs[seq_no][vect_no]

exit;

sub multi_freq_vec{
	my@seqs=@_;
	my$i=0;
	my@freqs=();
	
	foreach my$seq(@seqs){
		$freqs[$i++]=freq_vec($seq);
	}
	return @freqs;
}

sub freq_vec{
		my($protein)=@_;
		my@pink	=	qw(A G V);
		my@red	=	qw(I L F P);
		my@purple	=	qw(Y M T S);
		my@blue	=	qw(H N Q W);
		my@lightgreen		=	qw(R K);
		my@lightyellow	=	qw(D E);
		my@darkyellow		=	qw(C);

		my@classes=(\@pink,\@red,\@purple,\@blue,\@lightgreen,\@lightyellow,\@darkyellow);

		my@V=gen_rep(@classes);

		#print @{$V[$vec_no][$class_no]},"\n";

		my@freq=(0);
		my$vec_no="";



		for(my $i=0; $i < (length($protein) - 2) ; $i ++) {
			$vec_no=match_vector( substr($protein,$i,3) );
			$freq[$vec_no]++;
		}
		
	return \@freq;
		
			print "freqs:\n";
			my$i=0;
			foreach(@freq){
			print $i++,"\t",$_,"\n";
			}
			
			
			
			sub match_vector{
					my($seq)=@_;
					my@AAs=split('',$seq);
					my$match="";
					my$size=scalar(@classes);
					for(my$i=0;$i<scalar(@AAs);$i++){
						for(my$j=0;$j<scalar(@classes);$j++){	
							if( grep(/$AAs[$i]/,@{$classes[$j]})){
								if($i==2){
									$match+=$j;
								}elsif($i==1){
									$match+=$j*$size;
								}elsif($i==0){
									$match+=$j*($size**2);
								}
							}
						}
					}
					#print"$match\n";
					return$match;
			}
}



sub gen_rep{
	my@cont=@_;
	my@result;
	for(my$i=0;$i<scalar(@cont);$i++){
		for(my$j=0;$j<scalar(@cont);$j++){
			for(my$k=0;$k<scalar(@cont);$k++){
				push(@result,[($cont[$i],$cont[$j],$cont[$k])]);
			}	
		}
	}
	return @result;
}


 
sub read_multi_fasta{
		my$fname=shift@_;
		my@seqs=();

		open(FH,$fname);
		local $/ = ">";  # read by FASTA record
		my@cont=<FH>;
		#print $cont[3];
		shift(@cont);
		#print scalar(@cont);
		foreach my $seq (@cont) {

			my@seq_entry=split('\n',$seq);
				if($seq_entry[$#seq_entry]=~/>/){
					pop(@seq_entry);
				}
			my ($head) = shift(@seq_entry); 
			$seq = join('',@seq_entry);
			push (@seqs,$seq);
			  
		}

		#print scalar@seqs;
		return @seqs;
}
