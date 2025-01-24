int toto(var int x) ::= 
x := (x + 1);

null main() ::= 
{
   y := 1 - 9 + 2 * 2 / 4 % 42;
   print "premier\n";
   print(y);
   toto(y);
   print "\nsecond\n";
   print(y);
}