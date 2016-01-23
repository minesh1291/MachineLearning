# newton raphson
# for function:  "y=x^3-20"

$err=0.05;	# expected error
$x0=12;		# starting point
for(;;) #while(1)
{
	$fx=$x0*$x0*$x0-20;  #value of function at x0
	$dfx=3*$x0*$x0;      #value of derivative of function at x0

	$x1=$x0-($fx/$dfx);  #calculation for new x
	
	if ((abs($x0-$x1))<$err) 	#if differance bet. x0 n x1 is less than expeted
		{last;}			#come out of loop		
	
	$x0=$x1;	#store new x1 in old x0	
	print $x1,"\n";
}
