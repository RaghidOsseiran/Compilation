null main() ::={
    int t[4];
    float s[5];
    int pos;
    pos := 0;
    print "begin\n";
    while(pos < t.size) {
        t[pos] := 1;
        pos := pos+1;
    }
    print "t set\n";
    pos := 0;
    while(pos < s.size) {
        s[pos] := 1.0;
        pos := pos+1;
    }
    print "s set\n";
    pos := 0;
    while(pos < t.size) {
        print t[pos];
        print "\n";
        pos := pos+1;
    }
    pos := 0;
    while(pos < s.size) {
        print s[pos];
        print "\n";
        pos := pos+1;
    }
}