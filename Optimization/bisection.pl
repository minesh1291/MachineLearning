# bisection
# formula y=x^3-20

$a=1;
$b=4;
$err=5e-8;
print "a\tb\tc\tfun(a)\tfun(b)\tfun(c)\n";
while(1)
{
$c=($a+$b)/2;
#print "$a  \t$b  \t$c  \t";# fun($a),"  \t", fun($b),"\t", fun($c)," \n";
printf("%0.4f\t",$a);
printf("%0.4f\t",$b);
printf("%0.4f\t",$c);
printf("%0.4f\t",fun($a));
printf("%0.4f\t",fun($b));
printf("%0.4f\t\n",fun($c));
if(fun($a)<0 and fun($c)<0)
{$a=$c;}
else
{$b=$c;}
if(abs($a-$b)<$err)
{last;}

}

sub fun()
{
my($x)=@_;
$fx=$x**3-20;
return $fx;
}
