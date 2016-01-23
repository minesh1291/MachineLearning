# bracket finding
# formula f(x)=x^2+54/x

#1

$x0=1;
$d=0.5;
$k=0;


for($i=1;;$i++)
{

#2

if(fun($x0-abs($d))>fun($x0) and fun($x0)>fun($x0+abs($d)))
{$d=abs($d);}
elsif(fun($x0-abs($d))<fun($x0) and fun($x0)<fun($x0+abs($d)))
{$d=-abs($d);}

#3

$x1=$x0+(2**$k)*$d;

#4

if(fun($x1)<fun($x0))
{
$x0=$x1;
print $i,"D=>$d X0=>$x0 X1=>$x1 fun(x0)",fun($x0),"\n";
}
else
{
print "interval=> $x0 - $x1";
last;
}

}

exit;

sub fun()
{
my($x)=@_;
return ($x**2)+(54/$x);
}
