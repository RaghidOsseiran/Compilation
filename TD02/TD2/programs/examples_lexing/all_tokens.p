// Commentaire d’une ligne

int int_func(int n, var int[] tab) ::= {
    int a;
    a := 0 + n - 1 * ( n/2 % 3);
    if(a = n) then
        tab[0] := 3;
    else
        tab[tab.size-1] := 1;
    return tab[n/2];
}

/* Commentaire
sur
plusieurs
lignes*/

float float_func(int n, var float[] tab) ::= {
    float b;
    b := 0. + tab[0] - .4 * ( tab[1] / 4.2 % 50.54); //Commentaire en fin de ligne
    while(b <= 40.25){
        b:= b + 1.2;
    }
    return b - tab[3];
}

bool bool_func(bool a, bool b, int n, int m) ::= {
    bool c;
    c := a && b;
    c := a || b;
    c := /*Commentaire au milieu du code */ !a;
    c := n < m;
    c := (n >= m) || (n <> m);
    c := n <= m;
    c := (n = m) || (n > m);
    return c;
}

null main() ::= {
    int a;
    a := 0;
    int tab[11];
    print "coucou\n";
    print int_func(a,tab);
    float b;
    float tab2[14];
    b := 4.2;
    float_func(a,tab2);
}