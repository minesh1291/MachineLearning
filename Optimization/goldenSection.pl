# golden section
# formula f(x)=x^2+54/x

#1
$a=2;
$b=25;
$err=0.0005;

$aw=0; $bw=1; $lw=1; $k=0;
print "step aw   bw    w1    w2     x1     x2    f(x1)    f(x2)   fw(w1)    fw(w2)\n";
for($i=1;;$i++)
{
	printf(" %d %0.3f %0.3f ",$i,$aw,$bw);
	$lw = abs($aw-$bw);
	$w1=$aw + (0.618*$lw);
	$w2=$bw - (0.618*$lw);
	printf(" %0.3f %0.3f ",$w1,$w2);
	if(funw($w1)<funw($w2))
	{
		$aw=$w2;
	}
	elsif(funw($w1)>funw($w2))
	{
		$bw=$w1;
	}
	
	#print "New: $aw####$bw\n";
	($aw,$bw)=sort($aw,$bw);
	$x1=$w1*($b-$a)+$a;
	$x2=$w2*($b-$a)+$a;
	printf(" %0.3f  %0.3f ",$x1,$x2);
	printf(" %0.3f %0.3f ",fun($x1),fun($x2));
	printf(" %0.3f %0.3f ",funw($w1),funw($w2));
	#printf("f(x1) %0.3f f(x2) %0.3f ",fun($x1),fun($x2));
	if(abs($x1-$x2)<$err)
	{
		print "\ninterval=> $x1 - $x2";
		last;
	}

	print "\n";
}
exit;

sub fun()
{
my($x)=@_;
$ans=($x**2)+(54/$x);
return $ans;
}
=head
sub fun()
{
my($x)=@_;
$ans=$x**2;
return $ans;
}
=cut
sub funw()
{
my($w)=@_;
$x=$w*($b-$a)+$a;
$ans= &fun($x);
return $ans;
}
